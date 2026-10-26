#------------------------------------------------------------------------------
# 01_fillable_inventory.R
#
# Step 3a of the ACS state-input test: which rows of the state-tax data-gaps
# registry (research/state_tax/tax_data_gaps.csv, decision S43) could be filled
# from the ACS person and household records that sit under the
# Affordability-Index matched ACS-PUF file (TY2023, implicate B_m1).
#
# Two parts:
#   A. Classify every registry row: acs_fill = filled | partial_proxy |
#      not_in_extract | not_in_acs, with the ACS variable(s), where they live
#      (the matched file, a join to the shared acs_common_v2 extract, or an
#      IPUMS re-pull) and notes. Rule tables below; first match wins.
#   B. Weighted coverage of the two focus families -- homestead (tenure, rent,
#      property tax) and dependent detail (>3 dependents, students) -- from a
#      column subset of the person file and the return-level federal file.
#
# Run from the Tax-Simulator repo root:
#   module load R/4.4.2-gfbf-2024a
#   AFFORDABILITY_ROOT=<path to Affordability-Index clone> R_LIBS_USER= \
#     Rscript research/state_tax/acs_test/01_fillable_inventory.R
# (R_LIBS_USER empty and the .libPaths() line below keep the personal Arrow
# build, which lacks zstd, from shadowing the module's.)
#
# Outputs (research/state_tax/acs_test/):
#   fillable_inventory.csv  one row per registry row, registry columns + acs_*
#   fillable_summary.csv    counts by imputation item and by materiality
#   acs_coverage.csv        weighted coverage statistics (part B)
#------------------------------------------------------------------------------

.libPaths(setdiff(.libPaths(), path.expand('~/r_libs_4.4')))

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})


#---------
# Inputs
#---------

AFFORDABILITY_ROOT = Sys.getenv('AFFORDABILITY_ROOT')
if (AFFORDABILITY_ROOT == '') {
  stop('AFFORDABILITY_ROOT is unset: point it at the Affordability-Index clone ',
       '(the directory holding data/interim/income/).')
}

FRAME_YEAR   = 2023
INCOME_DIR   = file.path(AFFORDABILITY_ROOT, 'data/interim/income', FRAME_YEAR)
PERSONS_PATH = file.path(INCOME_DIR, paste0('tax_units_', FRAME_YEAR, '.parquet'))
RETURNS_PATH = file.path(INCOME_DIR, 'files', paste0('B_m1_', FRAME_YEAR, '_federal.parquet'))
# Claimed-dependent edges: one row per (dependent, claiming unit) AFTER the
# household-coordinated claim assignment. person_role on the person file is the
# pre-assignment membership and disagrees with the returns' n_dep for ~82,000
# units, so dependent detail is read from here
EDGES_PATH   = file.path(INCOME_DIR, paste0('claim_links_', FRAME_YEAR, '_edges.parquet'))

GAPS_PATH = 'research/state_tax/tax_data_gaps.csv'
OUT_DIR   = 'research/state_tax/acs_test'

for (p in c(PERSONS_PATH, RETURNS_PATH, EDGES_PATH, GAPS_PATH)) {
  if (!file.exists(p)) stop('missing input: ', p)
}


#------------
# Constants
#------------

# Tax-Simulator tracks three dependent age slots (dep_age1-3)
N_DEP_AGE_SLOTS = 3

# Student window of the qualifying-child test (IRC 152(c)(3)): under 19, or a
# full-time student under 24. dep_student_n counts the 19-23 group
STUDENT_AGE_MIN = 19
STUDENT_AGE_MAX = 23

# Adult dependents beyond the tracked ages (ME line 13a row)
ADULT_DEP_AGE_MIN = 24

# IPUMS codes
OWNERSHP_OWNER      = 1
OWNERSHP_RENTER     = 2
OWNERSHPD_NO_CASH   = 21
RELATE_HOUSEHOLDER  = 1
SCHOOL_ENROLLED     = 2
GRADEATT_COLLEGE    = c(6, 7)
SCHLTYPE_PRIVATE    = 3
COST_IN_RENT        = 99997   # COSTGAS / COSTELEC / COSTFUEL: included in rent or condo fee
FUELHEAT_GAS        = 2
FUELHEAT_ELECTRIC   = 4
FUELHEAT_OTHER_FUEL = c(3, 5, 6, 7, 9)
QFLAG_ALLOCATED     = 4       # QPROTX99 / QRENT

MONTHS_PER_YEAR = 12

# PROPTX99 top bracket ($100,000+): coded at its lower bound
PROPTX_TOP_CODE_VALUE = 100000


#-----------------------------------------------------------------
# PROPTX99 recode: bracket code -> dollar midpoint (2018+ ACS)
#-----------------------------------------------------------------

proptx99_midpoint = function(code) {
  #----------------------------------------------------------------------------
  # Converts the IPUMS PROPTX99 bracket code (annual real estate taxes) to the
  # bracket midpoint in dollars. Bracket edges from the acs_common_v2 2023 DDI:
  # 1 none; 2 $1-49; 3-21 $50 steps from $50; 22-56 $100 steps from $1,000;
  # 58-62 $100 steps from $4,500 (57 is 1990-only); 63-68 the $5,000-$9,999
  # widening steps; 69-158 $1,000 steps from $10,000; 159 $100,000+. Code 89
  # is labelled '$30000-39999' in the DDI but sits between $29,000-29,999 (88)
  # and $31,000-31,999 (90), so it is read as $30,000-30,999.
  #
  # Parameters:
  #   - code (int[]) : PROPTX99 values; 0 (N/A) must be filtered out first
  #
  # Returns: dbl[] of dollar amounts. Stops on 0, 57 or any unknown code.
  #----------------------------------------------------------------------------
  if (any(is.na(code))) stop('proptx99_midpoint(): NA code')
  bad = setdiff(unique(code), c(1:56, 58:159))
  if (length(bad)) stop('proptx99_midpoint(): unexpected PROPTX99 code(s) ', paste(bad, collapse = ', '))
  case_when(
    code == 1               ~ 0,
    code == 2               ~ 25,
    code <= 21              ~ 50 * (code - 2) + 25,
    code <= 56              ~ 1000 + 100 * (code - 22) + 50,
    code <= 62              ~ 4500 + 100 * (code - 58) + 50,
    code == 63              ~ 5250,
    code == 64              ~ 5750,
    code <= 68              ~ 6000 + 1000 * (code - 65) + 500,
    code <= 158             ~ 10000 + 1000 * (code - 69) + 500,
    code == 159             ~ PROPTX_TOP_CODE_VALUE
  )
}


#==============================================================================
# A. Classify the registry
#==============================================================================

# Where an ACS variable lives
SRC_MATCHED = 'matched_file'          # on tax_units_2023 / B_m1 already
SRC_COMMON  = 'acs_common_v2_join'    # in the shared IPUMS extract; join on SAMPLE+SERIAL+PERNUM
SRC_REPULL  = 'ipums_repull'          # an IPUMS ACS variable in neither
SRC_NONE    = 'none'

# Rule 1: by overlay_variable (the supplemental-inputs name the row is gated on)
overlay_rules = tribble(
  ~overlay_variable,                       ~acs_fill,        ~acs_variables,                          ~acs_source,  ~acs_notes,
  # --- homestead family (item 1)
  'tenure',                                'filled',         'OWNERSHP',                              SRC_MATCHED,  'owner 1 / renter 2 on every person row; GQ already excluded from the frame',
  'rent_paid',                             'filled',         'RENT x 12 (contract rent)',             SRC_MATCHED,  'contract rent, not RENTGRS: the statutory bases are rent for occupancy, net of separately billed utilities; household amount, attributed to the householder tax unit (see README)',
  'property_tax_paid',                     'filled',         'PROPTX99 (bracket midpoint)',           SRC_MATCHED,  'owner-occupier real estate tax, all owners not only itemizers; bracketed code recoded to midpoints',
  # --- dependent detail (item 5)
  'dep_age4plus',                          'filled',         'claim_links edges (dep_age); B_m1 n_dep', SRC_MATCHED, 'every claimed dependent is an edge to a person row with an age; n_dep - 3 on the return row',
  'dep_student_n',                         'partial_proxy',  'claim_links edges (dep_age, dep_student = SCHOOL 2); GRADEATT', SRC_MATCHED,  'SCHOOL = enrolled in the past 3 months; ACS has no full-time flag. GRADEATT 6-7 isolates college',
  'dep_disabled_n',                        'not_in_extract', 'DIFFCARE, DIFFREM, DIFFPHYS, DIFFMOB',  SRC_REPULL,   'ACS disability items exist but are in neither the matched file nor acs_common_v2 (Affordability fetch_acs_1yr.R lists them as pending re-pull)',
  'other:dep_birth_year',                  'partial_proxy',  'AGE == 0, FERTYR',                      SRC_MATCHED,  'age at interview, not birth in the tax year; adoptions unobserved',
  'other:dep_first_year',                  'partial_proxy',  'AGE == 0',                              SRC_MATCHED,  'newborns only; first-year claims of older or adopted children unobserved',
  'other:dep_income',                      'filled',         'INCTOT (dependent person rows)',        SRC_MATCHED,  '',
  'other:dep_medicaid_status',             'partial_proxy',  'HINSCAID, AGE',                         SRC_MATCHED,  'Medicaid coverage only; institutional support unobserved (GQ excluded)',
  'other:aged_relative_in_home',           'partial_proxy',  'RELATE, AGE',                           SRC_MATCHED,  'aged relatives in the household are observed; developmental disability is not',
  # --- filer status (item 6)
  'disabled1',                             'not_in_extract', 'DIFF* (DIFFREM, DIFFPHYS, DIFFCARE, ...)', SRC_REPULL, 'SSI receipt (INCSUPP) or under-65 Social Security (INCSS) is only a weak proxy in the matched file',
  'disabled2',                             'not_in_extract', 'DIFF*',                                 SRC_REPULL,   'as disabled1',
  'veteran1',                              'not_in_extract', 'VETSTAT, VETDISAB',                     SRC_REPULL,   'not in acs_common_v2; IPUMS ACS carries both',
  'military_pay1',                         'partial_proxy',  'EMPSTATD 13-15, INCWAGE',               SRC_MATCHED,  'active-duty armed forces identified; Guard and reserve drill pay (civilian main job) are not',
  'other:national_guard1',                 'not_in_acs',     '',                                      SRC_NONE,     'Guard membership not asked',
  # --- per-person attribution (item 12)
  'pension_person1',                       'filled',         'INCRETIR (person)',                     SRC_MATCHED,  'a per-spouse share of the unit pension total; ACS retirement income also includes IRA/401(k) withdrawals and disability/survivor pensions',
  'pension_person2',                       'filled',         'INCRETIR (person)',                     SRC_MATCHED,  'as pension_person1',
  'ss_person1',                            'filled',         'INCSS (person)',                        SRC_MATCHED,  'per-spouse share of the unit total',
  'other:unearned_income_person_share',    'filled',         'INCINVST, INCRETIR, INCSS (person)',    SRC_MATCHED,  'per-spouse shares',
  'living_apart',                          'partial_proxy',  'MARST 2-3 (harmonized married_apart)',  SRC_MATCHED,  'spouse absent at interview, not absent for the whole year / last six months',
  'spouse_agi_mfs',                        'partial_proxy',  'B_m1 part 1/2 rows (agi)',              SRC_MATCHED,  'observed for couples split into two returns in the same household; an absent spouse\'s income is not',
  'other:spouse_income_mfs',               'partial_proxy',  'B_m1 part 1/2 rows',                    SRC_MATCHED,  'as spouse_agi_mfs',
  'other:spouse_death_year',               'not_in_extract', 'WIDINYR',                               SRC_REPULL,   'ACS asks widowed in the past 12 months; MARST 5 alone does not date it',
  'other:qualifying_widower',              'not_in_extract', 'WIDINYR, MARST',                        SRC_REPULL,   'needs the year of death (two-year window)',
  # --- household resources (item 3)
  'hh_resources_inc',                      'partial_proxy',  'INCSUPP, INCWELFR, INCOTHER, FOODSTMP, bench_snap', SRC_MATCHED, 'SSI and public assistance observed; VA benefits and child support only inside INCOTHER; SNAP a flag (bench_snap is Census SPM model output)',
  'nontax_support_inc',                    'not_in_acs',     '',                                      SRC_NONE,     'gifts, inheritances, scholarships not asked',
  # --- care / health expenses (items 9, 11)
  'care_exp_uncapped',                     'partial_proxy',  'bench_chxpns (SPMCHXPNS)',              SRC_MATCHED,  'Census SPM modelled child care expense, capped at the lower earner\'s earnings; Affordability decision D7 bars bench_* as a pipeline input',
  'health_premiums',                       'partial_proxy',  'bench_premium (SPMPREMIUM), HINSEMP, HCOVPRIV', SRC_MATCHED, 'SPM modelled premiums (not survey-reported in ACS); coverage type observed',
  'other:health_coverage',                 'partial_proxy',  'HCOVANY, HINSEMP, HCOVPRIV, HINSCAID',  SRC_MATCHED,  'coverage at interview, not months covered',
  'other:med_exp_unreimbursed',            'partial_proxy',  'MOOP, bench_medxpns (SPMMEDXPNS)',      SRC_MATCHED,  'SPM modelled medical out-of-pocket, not survey-reported in ACS',
  'care_provider_approved',                'not_in_acs',     '',                                      SRC_NONE,     '',
  'other:care_provider_quality_rating',    'not_in_acs',     '',                                      SRC_NONE,     '',
  'other:adult_care_exp',                  'not_in_acs',     '',                                      SRC_NONE,     '',
  'other:k12_education_exp',               'not_in_acs',     '',                                      SRC_NONE,     'SCHLTYPE gives private K-12 enrollment, not expenses',
  # --- geography / source state (items 14, none)
  'other:source_state',                    'not_in_extract', 'PWSTATE2',                              SRC_COMMON,   'place-of-work state for wages only; other income has no situs in ACS',
  'other:other_state_income',              'not_in_extract', 'PWSTATE2',                              SRC_COMMON,   'wage income only',
  'other:ui_source_state',                 'not_in_extract', 'MIGPLAC1',                              SRC_COMMON,   'state of residence a year ago; a weak proxy for the paying state',
  'other:locality',                        'partial_proxy',  'PUMA, COUNTYFIP',                       SRC_MATCHED,  'county identified for large counties only; cities via a PUMA-place crosswalk',
  # --- public-employee contributions: a flag is there, the amount is not
  'other:public_employee_contrib',         'not_in_extract', 'CLASSWKRD',                             SRC_COMMON,   'government-worker flag only; contribution amount not in ACS',
  'other:public_retirement_contrib',       'not_in_extract', 'CLASSWKRD',                             SRC_COMMON,   'government-worker flag only; contribution amount not in ACS',
  'other:itin_status',                     'partial_proxy',  'CITIZEN',                               SRC_MATCHED,  'weak: non-citizen status, most of whom hold SSNs',
  'other:itin_filer',                      'partial_proxy',  'CITIZEN',                               SRC_MATCHED,  'weak: as other:itin_status'
)

# Rule 2: overrides on required_variable (regex), applied before rule 1 to
# rows whose overlay_variable matches overlay_scope. They refine rows whose
# overlay name is too coarse ('none', or a family variable with an extra twist)
required_rules = tribble(
  ~overlay_scope,         ~required_regex,                                     ~acs_fill,        ~acs_variables,                 ~acs_source,  ~acs_notes,
  '^property_tax_paid$',  'motor_vehicle',                                     'partial_proxy',  'PROPTX99 (bracket midpoint)',  SRC_MATCHED,  'residence property tax only; motor-vehicle tax not in ACS',
  '^property_tax_paid$',  'all property',                                      'partial_proxy',  'PROPTX99 (bracket midpoint)',  SRC_MATCHED,  'principal residence only; other real property (farm, rental, second homes) not in ACS',
  '^property_tax_paid$',  'salt_prop_uncapped',                                'partial_proxy',  'PROPTX99 (bracket midpoint)',  SRC_MATCHED,  'residence only; itemizers already carry salt_prop',
  '^dep_student_n$',      'k12_private',                                       'partial_proxy',  'SCHLTYPE, GRADEATT',           SRC_MATCHED,  'private-school enrollment observed; homeschool not',
  '^none$',               'dep_relationship_support|household members',       'partial_proxy',  'RELATE, RELATED',              SRC_MATCHED,  'relationship roster observed; support share and months in residence not',
  '^none$',               'blind',                                             'not_in_extract', 'DIFFEYE',                      SRC_REPULL,   'vision difficulty, not legal blindness',
  '^none$',               'elderly or disabled',                               'not_in_extract', 'DIFF*',                        SRC_REPULL,   'Schedule R needs disability under 65',
  '^none$',               'exact age at 100',                                  'partial_proxy',  'AGE',                          SRC_MATCHED,  'ACS ages are top-coded within state',
  '^none$',               'half-years|birth date|month/year of birth',         'not_in_extract', 'BIRTHQTR',                     SRC_REPULL,   'quarter of birth only',
  '^none$',               'medical_expenses_gross',                            'partial_proxy',  'MOOP, bench_medxpns',          SRC_MATCHED,  'SPM modelled, not survey-reported',
  '^none$',               'SE health insurance',                               'partial_proxy',  'bench_premium, INCBUS00',      SRC_MATCHED,  'SPM modelled premiums on self-employed units',
  '^none$',               'qualifying care expenses',                          'partial_proxy',  'bench_chxpns',                 SRC_MATCHED,  'SPM modelled child care expense',
  '^none$',               'income_by_spouse|investment income by spouse|per-spouse employee payroll', 'filled', 'INCINVST, INCRETIR, INCSS, INCBUS00, INCWAGE (person)', SRC_MATCHED, 'per-spouse shares of unit totals'
)

# Co-inputs: a second requirement in required_variable that ACS cannot fully
# supply even when the row's main variable is filled. Collected into one note
co_input_rules = tribble(
  ~required_regex,                                ~co_input_gap,
  'disab',                                        'disability: not_in_extract (DIFF*)',
  'veteran',                                      'veteran status: not_in_extract (VETSTAT)',
  'household (gross )?income|household resources|total income incl|all sources', 'household income incl. nontaxable: partial (INCSUPP/INCWELFR/INCOTHER)',
  'assessed value',                               'assessed value: partial (VALUEH is market value)',
  'motor_vehicle',                                'motor-vehicle tax: not_in_acs',
  'county|locality',                              'county: partial (COUNTYFIP)',
  'months of occupancy',                          'months of occupancy: partial (MOVEDIN, acs_common_v2)',
  'surviving-spouse',                             'surviving spouse: partial (MARST 5)',
  'Homestead Exemption participation',            'homestead-exemption participation: not_in_acs'
)

classify_gaps = function(gaps) {
  #----------------------------------------------------------------------------
  # Assigns acs_fill and its companions to every registry row.
  #
  # Parameters:
  #   - gaps (df) : tax_data_gaps.csv read as character
  #
  # Returns: gaps with acs_fill, acs_variables, acs_source, acs_notes,
  #          acs_rule (which table matched) and co_input_gap appended.
  #----------------------------------------------------------------------------
  rule_cols = c('acs_fill', 'acs_variables', 'acs_source', 'acs_notes')

  # Rule 2 first: the first matching override per row
  override = map_dfr(seq_len(nrow(gaps)), function(i) {
    hit = required_rules %>%
      filter(str_detect(gaps$overlay_variable[i], overlay_scope),
             str_detect(gaps$required_variable[i], regex(required_regex, ignore_case = TRUE))) %>%
      slice_head(n = 1)
    if (nrow(hit) == 0) return(tibble(row_id = i))
    hit %>% select(all_of(rule_cols)) %>% mutate(row_id = i, acs_rule = 'required_variable')
  })

  by_overlay = gaps %>%
    mutate(row_id = row_number()) %>%
    select(row_id, overlay_variable) %>%
    left_join(overlay_rules, by = 'overlay_variable') %>%
    mutate(acs_rule = if_else(is.na(acs_fill), NA_character_, 'overlay_variable'))

  # Everything neither table names is outside ACS: tax-return concepts
  # (business detail, interest composition, pension source, 529s, ...)
  classified = by_overlay %>%
    select(row_id) %>%
    left_join(override, by = 'row_id') %>%
    rows_patch(by_overlay %>% select(row_id, all_of(rule_cols), acs_rule), by = 'row_id') %>%
    mutate(
      acs_rule      = coalesce(acs_rule, 'default'),
      acs_fill      = coalesce(acs_fill, 'not_in_acs'),
      acs_variables = coalesce(acs_variables, ''),
      acs_source    = coalesce(acs_source, SRC_NONE),
      acs_notes     = coalesce(acs_notes, '')
    )

  co_gap = map_chr(gaps$required_variable, function(v) {
    hits = co_input_rules %>% filter(str_detect(v, regex(required_regex, ignore_case = TRUE)))
    paste(hits$co_input_gap, collapse = '; ')
  })

  gaps %>%
    mutate(row_id = row_number()) %>%
    left_join(classified, by = 'row_id') %>%
    mutate(co_input_gap = co_gap) %>%
    select(-row_id)
}

gaps = read_csv(GAPS_PATH, col_types = cols(.default = col_character()))
cat('registry rows:', nrow(gaps), '\n')
if (anyNA(gaps$overlay_variable) || anyNA(gaps$required_variable)) {
  stop('registry has NA overlay_variable / required_variable: ',
       sum(is.na(gaps$overlay_variable)), ' / ', sum(is.na(gaps$required_variable)))
}

inventory = classify_gaps(gaps)
stopifnot(nrow(inventory) == nrow(gaps))

# Overlay names that the rule tables never mention fall to the default; list
# them so a reader can see nothing ACS-relevant was missed by omission
defaulted = inventory %>% filter(acs_rule == 'default') %>% count(overlay_variable, sort = TRUE)
cat('\nrows classified not_in_acs by default, by overlay_variable:\n')
print(defaulted, n = Inf)

write_csv(inventory, file.path(OUT_DIR, 'fillable_inventory.csv'), na = '')

ITEM_ORDER        = c(as.character(1:14), 'new', 'none')
MATERIALITY_ORDER = c('high', 'medium', 'low', 'de_minimis', 'unknown')
FILL_ORDER        = c('filled', 'partial_proxy', 'not_in_extract', 'not_in_acs')

summarise_fill = function(df, group_var, level_order) {
  df %>%
    count(dimension_value = .data[[group_var]], acs_fill) %>%
    mutate(acs_fill = factor(acs_fill, FILL_ORDER)) %>%
    complete(dimension_value, acs_fill, fill = list(n = 0L)) %>%
    pivot_wider(names_from = acs_fill, values_from = n) %>%
    mutate(rows = filled + partial_proxy + not_in_extract + not_in_acs,
           dimension = group_var,
           dimension_value = factor(dimension_value, level_order)) %>%
    arrange(dimension_value) %>%
    mutate(dimension_value = as.character(dimension_value)) %>%
    select(dimension, dimension_value, rows, all_of(FILL_ORDER))
}

fill_summary = bind_rows(
  summarise_fill(inventory, 'imputation_item', ITEM_ORDER),
  summarise_fill(inventory, 'materiality', MATERIALITY_ORDER),
  inventory %>% mutate(all = 'all') %>% summarise_fill('all', 'all')
)
cat('\nfill summary:\n')
print(fill_summary, n = Inf)
write_csv(fill_summary, file.path(OUT_DIR, 'fillable_summary.csv'))

cat('\nhigh/medium-materiality rows that ACS fills or proxies, by overlay variable:\n')
inventory %>%
  filter(materiality %in% c('high', 'medium'), acs_fill %in% c('filled', 'partial_proxy')) %>%
  count(overlay_variable, acs_fill, sort = TRUE) %>%
  print(n = Inf)


#==============================================================================
# B. Weighted coverage, TY2023 (focus families)
#==============================================================================

PERSON_COLS = c('hh_id', 'tu_id', 'PERNUM', 'RELATE', 'AGE', 'SCHOOL',
                'GRADEATT', 'SCHLTYPE', 'OWNERSHP', 'OWNERSHPD', 'RENT', 'RENTGRS',
                'PROPTX99', 'QPROTX99', 'QRENT', 'FUELHEAT', 'COSTGAS', 'COSTELEC',
                'COSTFUEL', 'HHWT')
RETURN_COLS = c('hh_id', 'tu_id', 'part', 'hhwt', 'filer', 'is_dependent_return', 'n_dep', 'state')
EDGE_COLS   = c('hh_id', 'dep_pernum', 'dep_age', 'dep_student', 'claimant_tu_id')

persons = open_dataset(PERSONS_PATH) %>% select(all_of(PERSON_COLS)) %>% collect()
returns = open_dataset(RETURNS_PATH) %>% select(all_of(RETURN_COLS)) %>% collect()
edges   = open_dataset(EDGES_PATH) %>% select(all_of(EDGE_COLS)) %>% collect()
if (anyNA(edges)) stop('claim edges have NAs in ', paste(names(edges)[map_lgl(edges, anyNA)], collapse = ', '))
cat('\nperson rows:', nrow(persons), ' return rows:', nrow(returns), '\n')

# Surface NAs in every column used rather than dropping them
na_counts = persons %>% summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = 'column', values_to = 'n_na') %>% filter(n_na > 0)
cat('person-file columns with NAs (all persons):\n'); print(na_counts, n = Inf)
if (anyNA(returns)) stop('return file has NAs in ', paste(names(returns)[map_lgl(returns, anyNA)], collapse = ', '))

#--- household housing record: from the householder's person row
households = persons %>%
  filter(RELATE == RELATE_HOUSEHOLDER) %>%
  transmute(hh_id, householder_tu = tu_id, OWNERSHP, OWNERSHPD, RENT, RENTGRS, PROPTX99,
            QPROTX99, QRENT, FUELHEAT, COSTGAS, COSTELEC, COSTFUEL, HHWT)
if (anyDuplicated(households$hh_id)) stop('more than one householder in a household')
n_hh_no_householder = n_distinct(persons$hh_id) - nrow(households)
cat('households without a householder row:', n_hh_no_householder, '\n')

households = households %>%
  mutate(
    tenure = case_when(OWNERSHP == OWNERSHP_OWNER  ~ 1L,
                       OWNERSHP == OWNERSHP_RENTER ~ 2L,
                       TRUE                        ~ 0L),
    # Heat included in contract rent: the heating fuel's cost is billed in rent
    heat_in_rent = tenure == 2L & (
      (FUELHEAT == FUELHEAT_GAS              & COSTGAS  == COST_IN_RENT) |
      (FUELHEAT == FUELHEAT_ELECTRIC         & COSTELEC == COST_IN_RENT) |
      (FUELHEAT %in% FUELHEAT_OTHER_FUEL     & COSTFUEL == COST_IN_RENT)),
    # No-cash renters (OWNERSHPD 21) carry RENT = NA in the file: they pay no
    # rent, so rent_paid is 0. Every other renter must have a value
    rent_contract_annual = case_when(tenure == 2L & OWNERSHPD == OWNERSHPD_NO_CASH ~ 0,
                                     tenure == 2L ~ RENT * MONTHS_PER_YEAR),
    rent_gross_annual    = case_when(tenure == 2L & OWNERSHPD == OWNERSHPD_NO_CASH ~ 0,
                                     tenure == 2L ~ RENTGRS * MONTHS_PER_YEAR)
  )
n_cash_renter_rent_na = with(households, sum(tenure == 2L & is.na(rent_contract_annual)))
if (n_cash_renter_rent_na > 0) stop(n_cash_renter_rent_na, ' cash-rent households with RENT NA')
cat('no-cash-rent renter households (RENT NA -> 0):', with(households, sum(tenure == 2L & OWNERSHPD == OWNERSHPD_NO_CASH)), '\n')

owner_codes = households %>% filter(tenure == 1L)
cat('\nPROPTX99 among owner households: code 0 (N/A):', sum(owner_codes$PROPTX99 == 0),
    ' NA:', sum(is.na(owner_codes$PROPTX99)), '\n')
households = households %>%
  mutate(property_tax = if_else(tenure == 1L & !is.na(PROPTX99) & PROPTX99 > 0,
                                proptx99_midpoint(if_else(tenure == 1L & !is.na(PROPTX99) & PROPTX99 > 0, PROPTX99, 1L)),
                                NA_real_))

#--- per-claiming-unit dependent detail: edges joined to the dependent's person row
dependents = edges %>%
  left_join(persons %>% select(hh_id, PERNUM, AGE, SCHOOL, GRADEATT, SCHLTYPE),
            by = c('hh_id', 'dep_pernum' = 'PERNUM'))
if (anyNA(dependents$AGE)) stop(sum(is.na(dependents$AGE)), ' claim edges with no person row')
if (any(dependents$AGE != dependents$dep_age)) stop('edge dep_age disagrees with person AGE')
if (any(dependents$dep_student != (dependents$SCHOOL == SCHOOL_ENROLLED))) {
  cat('note: edge dep_student != (SCHOOL == 2) on', sum(dependents$dep_student != (dependents$SCHOOL == SCHOOL_ENROLLED)), 'edges\n')
}
dep_by_tu = dependents %>%
  rename(tu_id = claimant_tu_id) %>%
  group_by(tu_id) %>%
  summarise(
    n_dep_persons      = n(),
    n_dep_19_23        = sum(AGE >= STUDENT_AGE_MIN & AGE <= STUDENT_AGE_MAX),
    n_student_19_23    = sum(AGE >= STUDENT_AGE_MIN & AGE <= STUDENT_AGE_MAX & SCHOOL == SCHOOL_ENROLLED),
    n_college_19_23    = sum(AGE >= STUDENT_AGE_MIN & AGE <= STUDENT_AGE_MAX & GRADEATT %in% GRADEATT_COLLEGE),
    n_dep_adult_24plus = sum(AGE >= ADULT_DEP_AGE_MIN),
    n_private_k12      = sum(SCHLTYPE == SCHLTYPE_PRIVATE & GRADEATT %in% 2:5),
    n_newborn          = sum(AGE == 0),
    .groups = 'drop'
  )

#--- returns: the filed, non-dependent federal returns of B_m1
filed = returns %>% filter(filer == 1, !is_dependent_return)
tu_return = filed %>%
  group_by(tu_id, hh_id) %>%
  summarise(hhwt = first(hhwt), n_parts = n(), n_dep_returns = sum(n_dep),
            dep_age4plus = sum(pmax(n_dep - N_DEP_AGE_SLOTS, 0)),
            n_returns = n(), .groups = 'drop')

# Check: dependents on the return rows equal claimed-dependent edges, all returns
dep_check = returns %>% group_by(tu_id) %>% summarise(n_dep_returns = sum(n_dep), .groups = 'drop') %>%
  full_join(dep_by_tu %>% select(tu_id, n_dep_persons), by = 'tu_id') %>%
  mutate(n_dep_persons = coalesce(n_dep_persons, 0L))   # a unit with no dependent rows has none
n_dep_mismatch = sum(dep_check$n_dep_returns != dep_check$n_dep_persons)
n_tu_dep_no_return = sum(is.na(dep_check$n_dep_returns))
cat('\ntax units where return n_dep != claimed-dependent edges:', n_dep_mismatch,
    ' (units with dependents but no return row:', n_tu_dep_no_return, ')\n')

units = filed %>%
  left_join(households %>% select(hh_id, householder_tu, tenure, heat_in_rent, OWNERSHPD,
                                  rent_contract_annual, rent_gross_annual, property_tax,
                                  QPROTX99, QRENT, PROPTX99),
            by = 'hh_id') %>%
  mutate(is_householder_unit = tu_id == householder_tu)
n_unit_no_household = sum(is.na(units$tenure))
cat('filed returns with no householder record:', n_unit_no_household, '\n')

wsum  = function(w, x) sum(w * x)
wshare = function(w, x, universe) {
  # weighted share of `universe` where x holds; an NA inside the universe is an error
  universe = rep_len(universe, length(w))
  if (anyNA(x[universe])) stop('wshare(): NA in x inside the universe (', sum(is.na(x[universe])), ' rows)')
  sum(w[universe & x]) / sum(w[universe])
}

w = units$hhwt
renter = units$tenure %in% 2L
owner  = units$tenure %in% 1L
hu     = units$is_householder_unit %in% TRUE

tu_dep = tu_return %>% left_join(dep_by_tu, by = 'tu_id') %>%
  mutate(across(c(n_dep_persons, n_dep_19_23, n_student_19_23, n_college_19_23,
                  n_dep_adult_24plus, n_private_k12, n_newborn), ~ coalesce(.x, 0L)))   # no dependent rows: zero

coverage = tribble(
  ~family,      ~statistic,                                                          ~value,
  'frame',      'filed non-dependent returns (weighted, millions)',                  sum(w) / 1e6,
  'frame',      'filed non-dependent returns (unweighted)',                          length(w),
  'tenure',     'share of returns: owner household',                                 wshare(w, owner, TRUE),
  'tenure',     'share of returns: renter household',                                wshare(w, renter, TRUE),
  'tenure',     'share of returns: tenure N/A or no householder record',             wshare(w, !(owner | renter), TRUE),
  'tenure',     'share of returns in the householder\'s own tax unit',               wshare(w, hu, TRUE),
  'rent',       'renter returns: share with cash rent (RENT > 0)',                   wshare(w, units$rent_contract_annual > 0, renter),
  'rent',       'renter returns: share no cash rent (OWNERSHPD 21)',                 wshare(w, units$OWNERSHPD == OWNERSHPD_NO_CASH, renter),
  'rent',       'renter returns: share in the householder unit (rent attributable)', wshare(w, hu, renter),
  'rent',       'renter returns: share with RENT allocated (QRENT)',                 wshare(w, units$QRENT == QFLAG_ALLOCATED, renter),
  'rent',       'renter returns: share with heat included in contract rent',         wshare(w, units$heat_in_rent %in% TRUE, renter),
  'rent',       'renter householder returns: mean annual contract rent ($, no-cash at 0)', wsum(w[renter & hu], units$rent_contract_annual[renter & hu]) / sum(w[renter & hu]),
  'rent',       'renter householder returns: mean annual gross rent ($, no-cash at 0)',    wsum(w[renter & hu], units$rent_gross_annual[renter & hu]) / sum(w[renter & hu]),
  'rent',       'all renter households: aggregate annual contract rent ($bn, HHWT)', with(households, sum(HHWT[tenure == 2L] * rent_contract_annual[tenure == 2L])) / 1e9,
  'property_tax', 'all owner households: aggregate property tax ($bn, HHWT, midpoints)', with(households, sum(HHWT[tenure == 1L] * coalesce(property_tax[tenure == 1L], 0))) / 1e9,
  'property_tax', 'owner returns: share with property tax > 0',                      wshare(w, units$PROPTX99 > 1 & !is.na(units$PROPTX99), owner),
  'property_tax', 'owner returns: share with property tax none (code 1)',            wshare(w, units$PROPTX99 == 1 & !is.na(units$PROPTX99), owner),
  'property_tax', 'owner returns: share with PROPTX99 N/A (code 0) or NA',           wshare(w, units$PROPTX99 == 0 | is.na(units$PROPTX99), owner),
  'property_tax', 'owner returns: share in the householder unit',                    wshare(w, hu, owner),
  'property_tax', 'owner returns: share with PROPTX99 allocated (QPROTX99)',         wshare(w, units$QPROTX99 == QFLAG_ALLOCATED, owner),
  'property_tax', 'owner returns: share at the $100,000+ top code',                  wshare(w, units$PROPTX99 == 159 & !is.na(units$PROPTX99), owner),
  'property_tax', 'owner householder returns: mean property tax ($, midpoints)',     wsum(w[owner & hu], coalesce(units$property_tax[owner & hu], 0)) / sum(w[owner & hu]),
  'dependents', 'tax units with >3 dependents (weighted, thousands)',                sum(tu_dep$hhwt[tu_dep$n_dep_returns > N_DEP_AGE_SLOTS]) / 1e3,
  'dependents', 'filed returns with n_dep > 3 (weighted, thousands)',                 sum(w[units$n_dep > N_DEP_AGE_SLOTS]) / 1e3,
  'dependents', 'share of tax units with >3 dependents',                             wshare(tu_dep$hhwt, tu_dep$n_dep_returns > N_DEP_AGE_SLOTS, TRUE),
  'dependents', 'dependents beyond the third slot (weighted, thousands)',            wsum(tu_dep$hhwt, tu_dep$dep_age4plus) / 1e3,
  'dependents', 'dependents aged 19-23 (weighted, thousands)',                       wsum(tu_dep$hhwt, tu_dep$n_dep_19_23) / 1e3,
  'dependents', 'of which enrolled (SCHOOL = 2) (weighted, thousands)',              wsum(tu_dep$hhwt, tu_dep$n_student_19_23) / 1e3,
  'dependents', 'of which in college (GRADEATT 6-7) (weighted, thousands)',          wsum(tu_dep$hhwt, tu_dep$n_college_19_23) / 1e3,
  'dependents', 'tax units with >=1 enrolled dependent aged 19-23 (share)',          wshare(tu_dep$hhwt, tu_dep$n_student_19_23 > 0, TRUE),
  'dependents', 'dependents aged 24+ (weighted, thousands)',                         wsum(tu_dep$hhwt, tu_dep$n_dep_adult_24plus) / 1e3,
  'dependents', 'private-school K-12 dependents (weighted, thousands)',              wsum(tu_dep$hhwt, tu_dep$n_private_k12) / 1e3,
  'dependents', 'dependents aged 0 (weighted, thousands)',                           wsum(tu_dep$hhwt, tu_dep$n_newborn) / 1e3,
  'checks',     'tax units: return n_dep != claimed-dependent edges (count)',        n_dep_mismatch,
  'checks',     'households without a householder row (count)',                      n_hh_no_householder,
  'checks',     'filed returns with no householder record (count)',                  n_unit_no_household,
  'checks',     'dependents with SCHOOL NA or unknown (count)',                        sum(is.na(dependents$SCHOOL) | dependents$SCHOOL %in% c(8, 9)),
  'checks',     'dependents with AGE NA (count)',                                    sum(is.na(dependents$AGE))
)

cat('\ncoverage (TY2023, B_m1, weight hhwt):\n')
print(coverage %>% mutate(value = signif(value, 4)), n = Inf)
write_csv(coverage, file.path(OUT_DIR, 'acs_coverage.csv'))

# Renter share by state (filed returns), for the per-state homestead read
by_state = units %>%
  group_by(state) %>%
  summarise(returns_m = sum(hhwt) / 1e6,
            renter_share = sum(hhwt[tenure %in% 2L]) / sum(hhwt),
            renter_hu_cash_rent_share = sum(hhwt[tenure %in% 2L & is_householder_unit & rent_contract_annual > 0]) / sum(hhwt[tenure %in% 2L]),
            owner_proptax_pos_share = sum(hhwt[tenure %in% 1L & PROPTX99 > 1 & !is.na(PROPTX99)]) / sum(hhwt[tenure %in% 1L]),
            .groups = 'drop')
write_csv(by_state, file.path(OUT_DIR, 'acs_coverage_by_state.csv'))
cat('\nwrote', file.path(OUT_DIR, c('fillable_inventory.csv', 'fillable_summary.csv',
                                   'acs_coverage.csv', 'acs_coverage_by_state.csv')), sep = '\n  ')
cat('done\n')
