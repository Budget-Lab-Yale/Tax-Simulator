#-------------------------------------------------------------------------------
# estate_allocator.R
#
# Contains functions to pass estate tax through to heirs
#-------------------------------------------------------------------------------

# The estate tax is paid by estates but borne by heirs, so it has to be attributed
# to them. Estates and inheritances are each sorted by size and matched by
# cumulative dollars, so that each estate's average tax rate lands on the heirs of
# correspondingly large inheritances. The assumption that identifies this is that
# bigger inheritances come from bigger estates.
#
# This replaces the liability in the upstream heir data while keeping its
# inheritance amounts. Those are gross of estate tax, which the upstream file itself
# supports: its largest ratio of tax to inheritance is below the top rate, which
# would be impossible if inheritances were net. So inheritances do not vary by
# scenario and only the liability does.
#
# The function depends on one scenario's detail file and the baseline heir data, and
# is called separately for each leg, so it needs no file from another scenario and
# cannot race when the legs run in parallel.
#
# Tax on gains deemed realized at death goes through the same match, as a
# second call with its own ladder. The deemed tax detail column is already an
# expectation over the death event, so each record contributes one entry whose
# death weight and conditional liability multiply back to it.
#-------------------------------------------------------------------------------


ESTATE_DETAIL_COLS = c('estate_m', 'estate_p_dsue', 'liab_estate_dsue',
                       'liab_estate_nodsue', 'estate_distributable')


allocate_estate_to_heirs = function(leg_detail, heir_px, yr, leg_id,
                                    tax = c('estate', 'deemed')) {

  #----------------------------------------------------------------------------
  # Allocates one leg's expected tax at death to heirs: the estate tax, or the
  # income tax on gains deemed realized at death.
  #
  # On the estate side, a single filer contributes two entries rather than one:
  # whether an unused spousal exemption is available changes whether the estate is
  # taxable at all, and averaging over that would dilute rates near the exemption and
  # flatten the profile. A married record has no such branch, so the same machinery
  # covers it. Entries are sorted by distributable estate, largest first, each
  # carrying its bequest dollars at its own average rate.
  #
  # On the deemed side, each record contributes one entry: liab_deemed is
  # already the expectation over the death event, so the entry's conditional
  # liability is liab_deemed / estate_m and the product restores the expected
  # tax exactly. Estates owing no deemed tax stay on the ladder, unlike the
  # estate side: an estate without unrealized gains can be of any size, so
  # removing it would shift the taxed bequest dollars onto larger heirs and
  # zero out the smaller ones. Each heir instead bears the average deemed
  # rate of estates near their inheritance's size. Entries with negative
  # conditional liability cannot sit on a monotone ladder; they are dropped
  # and surfaced in diagnostics.
  #
  # On the heir side, inheritances are sorted the same way. Walking the two sorted
  # lists together, each heir's rate is the average of the estate rates over the
  # range of dollars it spans. Heirs below the last taxed dollar of bequest pay
  # nothing. Their tax is their inheritance times that rate, which by construction
  # sums to the tax the revenue side booked.
  #
  # Stops if the taxed bequests exceed the inheritances available, rather than
  # scaling rates up to absorb the difference.
  #
  # Parameters:
  #   - leg_detail (df) : this leg's FULL detail-file year extract (no
  #                       dep_status filter — the identity ties to the revenue
  #                       side), with id, weight (already 1/pct_sample-scaled
  #                       at write time), the five estate columns, and
  #                       liab_deemed when tax = 'deemed'
  #   - heir_px (df)    : id, p_inheritance, inheritance from the BASELINE
  #                       Estate-Tax-Distribution interface (heir structure is
  #                       baseline-only; ids absent from leg_detail get no
  #                       weight and drop out)
  #   - yr (int)        : calendar (death) year, for the diagnostics row
  #   - leg_id (str)    : scenario ID of this leg, for the diagnostics row
  #   - tax (str)       : which tax to allocate, 'estate' or 'deemed'
  #
  # Returns: list with
  #   - heirs (df) : id, estate_tax_liability (λ, conditional on inheriting;
  #                  0 for non-heirs and below-cutoff heirs)
  #   - diag (df)  : 1-row diagnostics (masses, identity residual, cutoff,
  #                  taxed-heir counts, rate profile, dropped tax mass)
  #----------------------------------------------------------------------------

  tax = match.arg(tax)

  # Decedent ladder: branch entries with positive death weight, tax, and
  # distributable estate. Zero-distributable taxed estates (possible when
  # debts wipe out the estate but the gift add-back alone exceeds the
  # exemption) carry tax mass with no bequest mass to pin it to; they are
  # dropped from the ladder and surfaced in diagnostics
  if (tax == 'estate') {
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
  } else {
    estates = leg_detail %>%
      select(id, weight, estate_m, estate_distributable, liab_deemed) %>%
      mutate(d = weight * estate_m) %>%
      filter(d > 0) %>%
      mutate(branch = 'liab_deemed',
             dw     = d,
             liab   = liab_deemed / estate_m)

    dropped_negative_tax = estates %>%
      filter(liab < 0) %>%
      summarise(tau = sum(dw * liab)) %>%
      pull(tau)
    if (dropped_negative_tax < 0) {
      warning('deemed allocator (', leg_id, ', ', yr, '): $',
              round(abs(dropped_negative_tax) / 1e6, 1), 'M of expected ',
              'deemed tax is negative and cannot sit on the rank-match ',
              'ladder; left unallocated')
    }
    estates %<>% filter(liab >= 0)
  }

  dropped_zero_n_tax = estates %>%
    filter(estate_distributable <= 0) %>%
    summarise(tau = sum(dw * liab)) %>%
    pull(tau)
  if (dropped_zero_n_tax > 0) {
    warning(tax, ' allocator (', leg_id, ', ', yr, '): $',
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

  # Degenerate case: no tax to allocate, because no estates survive the
  # filters (estate tax repealed) or the ladder carries only zero-tax
  # entries (the baseline deemed leg)
  if (nrow(estates) == 0 || total_tau == 0) {
    return(list(
      heirs = heirs %>% transmute(id, estate_tax_liability = 0),
      diag  = tibble(
        year = yr, leg = leg_id, tax = tax, n_taxed_branches = 0,
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
    stop(tax, ' allocator (', leg_id, ', ', yr, '): taxed bequest mass ($',
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
    stop(tax, ' allocator (', leg_id, ', ', yr, '): aggregate identity ',
         'violated — allocated $', allocated, ' vs expected tax $', total_tau)
  }

  diag = tibble(
    year               = yr,
    leg                = leg_id,
    tax                = tax,
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
