#-------------------------------------------------------------------------------
# estate_allocator.R
#
# Stage-2 estate tax incidence: the rank-matching allocator that passes each
# scenario's on-model estate tax through to heirs (thoughts doc §10d, §12).
# Replaces the upstream Estate-Tax-Distribution liability column while keeping
# its inheritance amounts (p, x): decedent estates and heir inheritances are
# each sorted by size and matched by cumulative DOLLAR mass, so each estate's
# average tax rate lands on the heirs of (rank-)corresponding inheritances.
# Identifying assumption: bigger inheritances come from bigger estates.
#
# The allocator is a pure function of one leg-scenario's detail file plus the
# baseline heir data — called independently for the baseline and reform legs
# inside process_for_distribution(), so it needs no cross-scenario files and
# cannot race in the SLURM Phase 3b array.
#
# x (inheritance) is GROSS of estate tax — supported by the old upstream file
# (max tax/inheritance = 0.392 < 0.40, impossible under a net convention) —
# so inheritance is scenario-invariant and only liability differs across legs.
#
# NOTE: deemed-realization tax (kg_dynamics) deliberately does NOT use this
# allocator. Deemed realization has no exemption threshold — it applies to all
# transfers at death — so the proportional-to-inheritance smear in
# distribution.R is the conceptually correct incidence for it. The rank match
# exists because the estate tax is threshold'd.
#-------------------------------------------------------------------------------


ESTATE_DETAIL_COLS = c('estate_m', 'estate_p_dsue', 'liab_estate_dsue',
                       'liab_estate_nodsue', 'estate_distributable')


allocate_estate_to_heirs = function(leg_detail, heir_px, yr, leg_id) {

  #----------------------------------------------------------------------------
  # Allocates one leg-scenario's expected estate tax to heirs by rank-matching
  # cumulative dollar mass.
  #
  # Decedent side: each record contributes up to two ladder entries — the DSUE
  # and no-DSUE latent states, with death weights d·p and d·(1−p) — because
  # the unified-credit kink makes taxable status branch-specific (an expected
  # blend would dilute rates near the kink and flatten the λ/x profile; same
  # logic as the per-branch indicator blend in get_estate_totals()). Married
  # records have estate_p_dsue = 0, so the branch machinery covers them with
  # no special case. Entries are sorted by distributable estate descending;
  # entry j carries bequest mass b_j = d_j·n_j at average rate T_j/n_j.
  #
  # Heir side: inheritance mass μ_h = w_h·p_h·x_h, sorted by x descending.
  # The walk matches the two cumulative-mass ladders top-down: each heir's
  # blended rate is the bequest-mass-weighted average of the estate rates
  # over [cum_lo_h, cum_hi_h); heirs entirely below the last taxed bequest
  # dollar get λ = 0. λ_h = x_h × blended rate. By construction the aggregate
  # identity Σ w_h·p_h·λ_h = Σ d_j·T_j holds (and equals the year's
  # est_tax_exp in totals/estate.csv).
  #
  # Hard error if taxed bequest mass exceeds total heir inheritance mass:
  # incidence is never fabricated by scaling rates up ("absorb").
  #
  # Parameters:
  #   - leg_detail (df) : this leg's FULL detail-file year extract (no
  #                       dep_status filter — the identity ties to the revenue
  #                       side), with id, weight (already 1/pct_sample-scaled
  #                       at write time), and the five estate columns
  #   - heir_px (df)    : id, p_inheritance, inheritance from the BASELINE
  #                       Estate-Tax-Distribution interface (heir structure is
  #                       baseline-only; ids absent from leg_detail get no
  #                       weight and drop out)
  #   - yr (int)        : calendar (death) year, for the diagnostics row
  #   - leg_id (str)    : scenario ID of this leg, for the diagnostics row
  #
  # Returns: list with
  #   - heirs (df) : id, estate_tax_liability (λ, conditional on inheriting;
  #                  0 for non-heirs and below-cutoff heirs)
  #   - diag (df)  : 1-row diagnostics (masses, identity residual, cutoff,
  #                  taxed-heir counts, rate profile, dropped tax mass)
  #----------------------------------------------------------------------------

  # Decedent ladder: branch entries with positive death weight, tax, and
  # distributable estate. Zero-distributable taxed estates (possible when
  # debts wipe out the estate but the gift add-back alone exceeds the
  # exemption) carry tax mass with no bequest mass to pin it to; they are
  # dropped from the ladder and surfaced in diagnostics
  estates = leg_detail %>%
    select(id, weight, all_of(ESTATE_DETAIL_COLS)) %>%
    mutate(d = weight * estate_m) %>%
    filter(d > 0) %>%
    pivot_longer(
      cols      = c(liab_estate_dsue, liab_estate_nodsue),
      names_to  = 'branch',
      values_to = 'liab'
    ) %>%
    mutate(
      dw = d * if_else(branch == 'liab_estate_dsue',
                       estate_p_dsue, 1 - estate_p_dsue)
    ) %>%
    filter(dw > 0, liab > 0)

  dropped_zero_n_tax = estates %>%
    filter(estate_distributable <= 0) %>%
    summarise(tau = sum(dw * liab)) %>%
    pull(tau)
  if (dropped_zero_n_tax > 0) {
    warning('estate allocator (', leg_id, ', ', yr, '): $',
            round(dropped_zero_n_tax / 1e6, 1), 'M of expected estate tax ',
            'sits on estates with zero distributable value (gift add-back ',
            'only) and cannot be rank-matched to heirs; left unallocated')
  }

  estates %<>%
    filter(estate_distributable > 0) %>%
    arrange(desc(estate_distributable), id, branch) %>%
    mutate(
      b   = dw * estate_distributable,   # bequest mass
      tau = dw * liab                    # tax mass
    )

  # Heir ladder. heir_px ids missing from this year's detail have no weight
  # and zero mass; they receive λ = 0 like any non-heir
  heirs = heir_px %>%
    left_join(leg_detail %>% select(id, weight, dep_status), by = 'id') %>%
    mutate(mu = replace_na(weight, 0) * p_inheritance * inheritance) %>%
    arrange(desc(inheritance), id)

  total_b   = sum(estates$b)
  total_tau = sum(estates$tau)
  total_mu  = sum(heirs$mu)

  # Degenerate case: no taxed estates (e.g. estate tax repealed)
  if (nrow(estates) == 0) {
    return(list(
      heirs = heirs %>% transmute(id, estate_tax_liability = 0),
      diag  = tibble(
        year = yr, leg = leg_id, n_taxed_branches = 0,
        bequest_mass = 0, heir_mass = total_mu, tax_mass = 0,
        allocated_tax = 0, dropped_zero_n_tax = dropped_zero_n_tax,
        identity_resid = 0, cutoff_x = NA_real_, n_taxed_heirs = 0,
        taxed_heir_weight = 0, exp_taxed_estates = 0, heirs_per_estate = NA_real_,
        max_rate = NA_real_, dep_heir_tax_mass = 0
      )
    ))
  }

  # A reform can tax more bequest mass than the heir file has inheritance
  # mass (pathologically low exemptions). Never scale rates up to absorb the
  # excess — fail loudly
  if (total_b > total_mu) {
    stop('estate allocator (', leg_id, ', ', yr, '): taxed bequest mass ($',
         round(total_b / 1e9, 1), 'B) exceeds total heir inheritance mass ($',
         round(total_mu / 1e9, 1), 'B); the heir ladder is exhausted and the ',
         'rank match cannot allocate this scenario\'s estate tax')
  }

  # The walk, vectorized: cumulative tax mass as a function of cumulative
  # bequest mass is piecewise linear with slope T_j/n_j on entry j's interval
  # (knots strictly increasing since b > 0). Each heir's allocated tax mass
  # is the increment over their inheritance-mass interval; rule = 2 clamps
  # mass beyond the last taxed bequest dollar to the total (zero marginal
  # rate). Straddling heirs get the mass-weighted blend automatically
  ctau = approx(
    x    = c(0, cumsum(estates$b)),
    y    = c(0, cumsum(estates$tau)),
    xout = pmin(c(0, cumsum(heirs$mu)), total_b),
    rule = 2
  )$y

  heirs %<>%
    mutate(
      tax_mass             = diff(ctau),
      estate_tax_liability = if_else(mu > 0, tax_mass / (weight * p_inheritance), 0)
    )

  # Aggregate identity (telescopes to total_tau exactly; tolerance covers
  # interpolation arithmetic only)
  allocated = sum(heirs$tax_mass)
  if (abs(allocated - total_tau) > 1e-6 * max(total_tau, 1)) {
    stop('estate allocator (', leg_id, ', ', yr, '): aggregate identity ',
         'violated — allocated $', allocated, ' vs expected tax $', total_tau)
  }

  diag = tibble(
    year               = yr,
    leg                = leg_id,
    n_taxed_branches   = nrow(estates),
    bequest_mass       = total_b,
    heir_mass          = total_mu,
    tax_mass           = total_tau,
    allocated_tax      = allocated,
    dropped_zero_n_tax = dropped_zero_n_tax,
    identity_resid     = allocated - total_tau,
    cutoff_x           = min(heirs$inheritance[heirs$estate_tax_liability > 0]),
    n_taxed_heirs      = sum(heirs$estate_tax_liability > 0),
    taxed_heir_weight  = sum(replace_na(heirs$weight, 0) * heirs$p_inheritance *
                               (heirs$estate_tax_liability > 0)),
    exp_taxed_estates  = sum(estates$dw),
    heirs_per_estate   = NA_real_,  # filled below
    # Max λ/x can exceed the statutory top rate: the gift add-back taxes
    # transfers heirs effectively received earlier (by design, §12)
    max_rate           = max(heirs$estate_tax_liability /
                               pmax(heirs$inheritance, 1e-9)),
    dep_heir_tax_mass  = sum(heirs$tax_mass[replace_na(heirs$dep_status, 0) != 0])
  ) %>%
    mutate(heirs_per_estate = taxed_heir_weight / pmax(exp_taxed_estates, 1e-9))

  list(
    heirs = heirs %>% select(id, estate_tax_liability),
    diag  = diag
  )
}
