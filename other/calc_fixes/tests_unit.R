#-------------------------------------------------------------------------
# Unit tests for the 2026-07-01 calc-layer bug fixes (review findings
# #3 CDCTC, #4 payroll ee split, #5 1250/collectibles, #6 magi_ss,
# #8 dependent standard deduction). Synthetic records with hand-computed
# expectations. Run from the repo root.
#-------------------------------------------------------------------------

library(tidyverse)
library(magrittr)

return_vars = list()
source('src/calc/utils.R')
source('src/calc/functions/tax/pr.R')
source('src/calc/functions/income/ss.R')
source('src/calc/functions/income/agi.R')
source('src/calc/functions/tax/tax.R')
source('src/calc/functions/credits/cdctc.R')
source('src/calc/functions/deductions/std_ded.R')

n_fail = 0
check = function(name, got, want, tol = 1e-6) {
  ok = abs(got - want) < tol
  cat(sprintf('%-72s %s  (got %10.2f, want %10.2f)\n',
              name, if (ok) 'PASS' else 'FAIL', got, want))
  if (!ok) n_fail <<- n_fail + 1
}

#---------------------------------------------------------------
# Fix #4: liab_pr_ee includes Additional Medicare Tax
#---------------------------------------------------------------

pr_out = calc_pr(
  tibble(
    wages1 = 300000, wages2 = 0, filing_status = 1,
    pr.tips_exempt = 0, pr.ot_exempt = 0,
    pr.seca_taxable_rate = 0.9235, pr.se_thresh = 400,
    pr.oasdi_ee_rates1 = 0.062,  pr.oasdi_ee_brackets1 = 0,
    pr.oasdi_er_rates1 = 0.062,  pr.oasdi_er_brackets1 = 0,
    pr.hi_ee_rates1    = 0.0145, pr.hi_ee_brackets1    = 0,
    pr.hi_er_rates1    = 0.0145, pr.hi_er_brackets1    = 0,
    pr.add_med_rates1  = 0.009,  pr.add_med_brackets1  = 200000
  ),
  fill_missings = TRUE
)
check('#4 liab_add_med triggered above threshold', pr_out$liab_add_med, 900)
check('#4 liab_pr_ee includes Additional Medicare', pr_out$liab_pr_ee, 300000 * (0.062 + 0.0145) + 900)
check('#4 ee/er split reconciles with liab_pr',
      pr_out$liab_pr_ee + pr_out$liab_pr_er, pr_out$liab_pr)

#---------------------------------------------------------------
# Fix #6: magi_ss adds back tax-exempt interest
#---------------------------------------------------------------

agi_law = tibble(
  txbl_int = 30000, gross_ss = 20000,
  ss.magi_ss_rate = 0.5,
  ss.rates1 = 0.5, ss.rates2 = 0.85,
  ss.brackets1 = 25000, ss.brackets2 = 34000
)

# With $20K muni interest: magi_plus_ss = 30K + 20K + 10K = 60K
#   bracket 1: (34K - 25K) * 0.5 = 4,500; bracket 2: min(26K, 20K) * .85 = 17,000
#   sum 21,500 capped at 0.85 * 20K = 17,000
agi_out_muni = calc_agi(agi_law %>% mutate(exempt_int = 20000), fill_missings = TRUE)
check('#6 txbl_ss with tax-exempt interest added back', agi_out_muni$txbl_ss, 17000)
check('#6 exempt interest itself stays out of AGI', agi_out_muni$agi, 30000 + 17000)

# Without muni interest: magi_plus_ss = 40K -> 4,500 + 6K * 0.85 = 9,600
agi_out_nomuni = calc_agi(agi_law %>% mutate(exempt_int = 0), fill_missings = TRUE)
check('#6 no-muni record unchanged by fix', agi_out_nomuni$txbl_ss, 9600)

#---------------------------------------------------------------
# Fix #5: 1250/collectibles gains keep their place in the stack
#---------------------------------------------------------------

tax_law = tibble(
  div_pref = 0,
  ord.rates1 = 0.25, ord.brackets1 = 0,
  pref.rates1 = 0.15, pref.brackets1 = 0,
  pref.unrecapture_rate = 0.25, pref.collectibles_rate = 0.28,
  pref.tax_at_ord = 0
)

# Portfolio-dominated: ordinary 50K, kg_pref 150K of which 20K is 1250 gain.
# Pre-fix this taxed only 180K of the 200K (1250 slice dropped). Correct:
# .25*50K + .15*130K + .25*20K = 12,500 + 19,500 + 5,000 = 37,000
tax_out1 = calc_tax(tax_law %>% mutate(txbl_inc = 200000, kg_pref = 150000,
                                       kg_1250 = 20000, kg_collect = 0),
                    fill_missings = TRUE)
check('#5 1250 gain taxed when preferred income dominates', tax_out1$liab, 37000)
check('#5 liab_1250 component', tax_out1$liab_1250, 5000)

# Same but with a 10K collectibles slice too, under a 35% ordinary rate so
# both special-rate caps bind: .35*50K + .15*120K + .25*20K + .28*10K
# = 17,500 + 18,000 + 5,000 + 2,800 = 43,300
tax_out2 = calc_tax(tax_law %>% mutate(txbl_inc = 200000, kg_pref = 150000,
                                       kg_1250 = 20000, kg_collect = 10000,
                                       ord.rates1 = 0.35),
                    fill_missings = TRUE)
check('#5 collectibles slice also taxed', tax_out2$liab, 43300)

# Wage-dominated case was already correct pre-fix; must be unchanged:
# .25*150K + .15*30K + .25*20K = 37,500 + 4,500 + 5,000 = 47,000
tax_out3 = calc_tax(tax_law %>% mutate(txbl_inc = 200000, kg_pref = 50000,
                                       kg_1250 = 20000, kg_collect = 0),
                    fill_missings = TRUE)
check('#5 wage-dominated case invariant', tax_out3$liab, 47000)

# No special gains: liab = .25*50K + .15*150K = 35,000 (invariant path)
tax_out4 = calc_tax(tax_law %>% mutate(txbl_inc = 200000, kg_pref = 150000,
                                       kg_1250 = 0, kg_collect = 0),
                    fill_missings = TRUE)
check('#5 no-special-gains case invariant', tax_out4$liab, 35000)

#---------------------------------------------------------------
# Fix #3: CDCTC earned-income cap is a single shared pool
#---------------------------------------------------------------

cdctc_law = tibble(
  filing_status = 2, dep_age1 = 3, dep_age2 = 10, dep_age3 = NA_real_,
  care_exp = 10000, agi = 60000, liab_bc = 5000, ftc = 0, r.cdctc_takeup = 0,
  cdctc.exp_limit = 3000, cdctc.n_dep_limit = 3,
  cdctc.young_age_limit = 4, cdctc.old_age_limit = 12,
  cdctc.young_rate1 = 0.2, cdctc.old_rate1 = 0.2,
  cdctc.discrete_step = 1, cdctc.refundable = 0
)

# Low-earning spouse: ei_limit = 2,000. Young takes all of it; old gets 0.
# Credit = 2,000 * 0.2 = 400 (pre-fix: (2,000 + 2,000) * 0.2 = 800)
cdctc_out1 = calc_cdctc(cdctc_law %>% mutate(ei1 = 2000, ei2 = 50000),
                        fill_missings = TRUE)
check('#3 shared earned-income pool binds across buckets', cdctc_out1$cdctc_nonref, 400)

# High earners: cap slack, both buckets get 3,000 -> 6,000 * 0.2 = 1,200 (invariant)
cdctc_out2 = calc_cdctc(cdctc_law %>% mutate(ei1 = 50000, ei2 = 50000),
                        fill_missings = TRUE)
check('#3 slack-cap case invariant', cdctc_out2$cdctc_nonref, 1200)

# Partially binding: ei_limit = 4,000 -> young 3,000, old min(3,000, 1,000) = 1,000
# Credit = 4,000 * 0.2 = 800
cdctc_out3 = calc_cdctc(cdctc_law %>% mutate(ei1 = 4000, ei2 = 50000),
                        fill_missings = TRUE)
check('#3 partially binding cap stacks young first', cdctc_out3$cdctc_nonref, 800)

#---------------------------------------------------------------
# Fix #8: dependent standard deduction keeps age/blind bonus
#---------------------------------------------------------------

std_law = tibble(
  age2 = NA_real_, blind1 = FALSE, blind2 = NA,
  std.value = 15000, std.bonus = 2000, std.dep_floor = 1350,
  std.dep_earned_bonus = 450, std.bonus_other = 0
)

# Elderly claimed dependent, ei = 1,000: max(1,350, 1,450) = 1,450 capped by
# 15,000 -> 1,450, then + 2,000 age bonus = 3,450 (pre-fix: 1,450)
std_out1 = calc_std_ded(std_law %>% mutate(dep_status = TRUE, ei = 1000, age1 = 70),
                        fill_missings = TRUE)
check('#8 elderly dependent keeps age bonus', std_out1$std_ded, 3450)

# Young dependent, no bonus: unchanged at 1,450
std_out2 = calc_std_ded(std_law %>% mutate(dep_status = TRUE, ei = 1000, age1 = 16),
                        fill_missings = TRUE)
check('#8 young dependent invariant', std_out2$std_ded, 1450)

# High-earning elderly dependent: min(15,000, 20,450) + 2,000 = 17,000 (invariant)
std_out3 = calc_std_ded(std_law %>% mutate(dep_status = TRUE, ei = 20000, age1 = 70),
                        fill_missings = TRUE)
check('#8 high-earning elderly dependent invariant', std_out3$std_ded, 17000)

# Nondependent path untouched: 15,000 + 2,000 = 17,000
std_out4 = calc_std_ded(std_law %>% mutate(dep_status = FALSE, ei = 1000, age1 = 70),
                        fill_missings = TRUE)
check('#8 nondependent path invariant', std_out4$std_ded, 17000)

#---------------------------------------------------------------

cat(sprintf('\n%s: %d failure(s)\n', if (n_fail == 0) 'ALL TESTS PASS' else 'TESTS FAILED', n_fail))
if (n_fail > 0) quit(status = 1)
