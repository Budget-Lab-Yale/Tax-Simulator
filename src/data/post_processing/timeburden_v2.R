suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('./requirements.txt'), library, character.only = T)    
  ))
)

#--------------------------------------------------------------------
# >< no git yet so just using naming conventions for version control
#--------------------------------------------------------------------

#----------------------------------------------------------------------------
# timeburden_v2.R
# 
# Post-processing functions to generate time burden tables for a scenario
#----------------------------------------------------------------------------



process_for_time_burden = function(id, year) {
  
  #----------------------------------------------------------------------------
  # Reads and cleans input data for a given scenario, creating tax provision
  # flags at the record level.
  # 
  # Parameters:
  #   - id   (str) : scenario ID
  #   - year (int) : year to calculate time burdens for
  # 
  # Returns: microdata with all record-level variables required to calculate
  #          time burden (df).
  #----------------------------------------------------------------------------

  #----------------------------------------------------------------------------
  # >< keeping my little thoughts in comments marked with "><" ...
  #    1) i replaced the policy parameter with the scenario id... i'm guessing
  #       that the inputs for id should actually match the folder names, so
  #       something like:
  #         - "baseline"            : Current Law
  #         - "partial-simple"      : Simplified Income Tax,
  #         - "partial-mod_simple"  : Modified Simple Income Tax
  #         - "partial-back_future" : Back to the Future,
  #         - "partial-ubi"         : Universal Basic Income
  #    2) still keeping the baseline_id parameter removed
  #    3) since calculating fixed cost is its own function now, i thought that
  #       this function no longer needed to always process current law
  #----------------------------------------------------------------------------
  
    
  #-----------------------
  # Read microdata input
  #-----------------------
  
  # >< i know that these globals are probably in some other script... but idrk
  #    what that looks like... this is an effort to preserve the flow/format...
  #    hope it makes some sense
  globals = list(
    baseline_root = "202406031411",
    output_root   = "202406031411"
  )
  
  baseline_root = file.path(globals$baseline_root, "baseline")
  output_root   = file.path(globals$output_root, id)
  
  # >< since baseline is only for determining was_hoh, it always uses 2025 data
  baseline = file.path(baseline_root, "static/detail", "2025.csv") %>% 
    fread() %>%
    tibble()
  reform   = file.path(output_root, "static/detail", paste0(year, ".csv")) %>% 
    fread() %>%
    tibble() %>%
    mutate(
      policy = !!id
    )
  
  
  #------------------------------------
  # create relevant record-level flags
  #------------------------------------
  
  microdata = reform %>%
    
    # Remove non-filers
    filter(filer != 0) %>%
    
    # create flags for use of IRS forms
    mutate(
      across(.cols = c("sch_e",
                       "farm",
                       "sole_prop",
                       "se",
                       "txbl_int",
                       "eitc",
                       "qbi_ded",
                       "liab_amt"),
             .fns  = list(function(x) as.integer(x != 0)) ) %>%
        rename_with(~c("sch_e_flag",
                       "sch_farm_flag",
                       "sch_c_flag",
                       "sch_se_flag",
                       "int_flag",            # >< sch_b? rich named it int
                       "sch_eic_flag",
                       "qbi_flag",
                       "amt_flag") ),
      sch_d_flag   = as.integer(div_ord != 0 | div_pref !=0 | txbl_kg != 0 |
                                  kg_st != 0 | kg_lt != 0),
      itemize_flag = as.integer(itemizing),   # >< sch_a? rich named it itemize
      ctc_flag     = as.integer(ctc_ref > 0 | ctc_nonref > 0)
    ) %>%
    
    # base filing status
    left_join(baseline %>%
                mutate(
                  base_status = filing_status
                ) %>%
                select(id, base_status),
              by = 'id') %>%
    
    mutate(
      # person weighting
      weight_person = weight * (1 + (filing_status == 2)),
      
      # adjust for number of available credits
      other_credits = number_of_credits - ctc_flag - sch_eic_flag,

      # variables for whether filer filled out preferred rate calculation, is
      # no longer HoH, and has no taxable income under the future policy
      has_pref_inc  = as.integer(liab_pref > 0),
      was_hoh       = as.integer(base_status == 4 & filing_status == 1),
      no_tax        = as.integer(txbl_inc == 0 & agi >= 0)
    ) %>%
    return()
}



calc_fixed_cost = function() {
  
  #----------------------------------------------------------------------------
  # Calculates fixed cost
  # 
  # Parameters:
  #   - 
  # Returns: fixed cost of filing (dbl).
  #----------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------
  # >< my understanding is that computing fixed cost is specific to the 2025
  #    baseline... if that's the case i have a few thoughts
  #    1) it seems like i don't need any parameters... having microdata as a
  #       feels a bit silly since the microdata needs to be a pre-determined
  #       specific secnario
  #    2) instead, i call the processing function here... is that okay like
  #       stylistically?
  #----------------------------------------------------------------------------
  
  
  #--------------------------------
  # define relevant tax provisions
  #--------------------------------
  
  # >< i thought about including the provisions and coefficients as parameters
  #    since it'd be cleaner... but i think that isn't conceptually sensible
  provisions = c("int_flag",
                 "sch_c_flag",
                 "sch_d_flag",
                 "has_pref_inc",
                 "sch_e_flag",
                 "sch_farm_flag",
                 "itemize_flag",
                 "qbi_flag",
                 "sch_se_flag",
                 "amt_flag",
                 "ctc_flag",
                 "sch_eic_flag",
                 "other_credits")
  
  # time cost of each provision (coefficients)
  time_costs = c(86,
                 655,
                 311,
                 59,
                 374,
                 350,
                 556,
                 155,
                 75,
                 87,
                 34,
                 34,
                 34)
  
  # HoH-contingent items
  hoh_item   = c("itemize_flag",
                 "sch_d_flag",
                 "qbi_flag",
                 "amt_flag",
                 "ctc_flag")
  
  
  #--------------------------
  # process 2025 current law
  #--------------------------
  
  microdata = process_for_time_burden("baseline", 2025)
  
  
  #----------------------
  # calculate fixed cost
  #----------------------
  
  # fixed cost is the residual difference between the modeled time burden and
  # the IRS-reported average of 13 hours (2022)
  
  microdata %>%
    
    # marginal time burden
    mutate(
      marginal_burden = (across(.cols = all_of(provisions)) %>%
                           as.matrix() %*% time_costs) %>% as.vector()
    ) %>%
    
    # fixed cost as residual difference
    summarise(
      total_marginal_burden = sum(weight * marginal_burden),
      total_cost            = sum(weight * 780),
      total_fixed_cost      = total_cost - total_marginal_burden,
      total_taxable_filers  = sum(weight * (no_tax == 0))
    ) %>%
    summarise(
      mean_fixed_cost       = total_fixed_cost / total_taxable_filers
    ) %>%
    pull() %>%
    return()
}



calc_time_burden = function(microdata, fixed_cost) {
  
  #----------------------------------------------------------------------------
  # Calculates and adds record-level time burdens to the processed microdata
  # 
  # Parameters:
  #   - microdata  (df)  : tax unit data, ouput by process_for_time_burden()
  #   - fixed_cost (dbl) : fixed cost of filing, output by calc_fixed_cost()
  # 
  # Returns: tibble of record-level time burdens (df).
  #----------------------------------------------------------------------------
  
  #--------------------------------
  # define relevant tax provisions
  #--------------------------------
  
  provisions = c("int_flag",
                 "sch_c_flag",
                 "sch_d_flag",
                 "has_pref_inc",
                 "sch_e_flag",
                 "sch_farm_flag",
                 "itemize_flag",
                 "qbi_flag",
                 "sch_se_flag",
                 "amt_flag",
                 "ctc_flag",
                 "sch_eic_flag",
                 "other_credits")
  
  # time cost of each provision (coefficients)
  time_costs = c(86,
                 655,
                 311,
                 59,
                 374,
                 350,
                 556,
                 155,
                 75,
                 87,
                 34,
                 34,
                 34)
  
  # HoH-contingent items
  hoh_item   = c("itemize_flag",
                 "sch_d_flag",
                 "qbi_flag",
                 "amt_flag",
                 "ctc_flag")
  
  
  #----------------------------------
  # compute record-level time burden
  #----------------------------------
    
  microdata %>%
    mutate(
      burden = (across(.cols = all_of(provisions)) %>%
                  as.matrix %*% time_costs
                  - was_hoh * (rowSums(across(.cols = all_of(hoh_item)))
                                * 11 + 34)
                  + (no_tax == 0) * fixed_cost) %>% as.vector()
    ) %>%
    return()
}



calc_summary_stats = function(microdata) {

  #----------------------------------------------------------------------------
  # Aggregates record-level time burden microdata into summary stats, grouped
  # income within a policy.
  #
  # Parameters:
  #  - microdata (df) : tax unit data, output by calc_time_burden()
  #
  # Returns: tibble of average and total time burdens grouped by income (df).
  #----------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------
  # >< 1) i wanted to include the grouping step in the processing function but
  #       it seemed like processing would too long then? so i have it here
  #    2) the output is meant to mirror rich's... not sure why it's in minutes
  #       rather than hours
  #    3) most importantly tho, because of issues with differences in number
  #       formatting between r and stata the cutoff values are ever so slightly
  #       different which means quite a few observations (~100s-1000s) fall on
  #       the wrong side of the cutoff (this seems to get worse with higher
  #       quantiles)... even when i use the values from stata it still doesn't
  #       perfectly match :((
  #----------------------------------------------------------------------------
  
  # Calculate income thresholds
  # >< rich includes observations with expanded_inc<0 when computing the
  #    quantiles... not sure why that's right but i do the same here
  cutoffs = wtd.quantile(
    x       = microdata %>% pull(expanded_inc),
    probs   = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.99, 0.999),
    weights = microdata %>% pull(weight_person)
  )
  
  # assign income groups
  microdata = microdata %>%
    mutate(
      income_group = cut(
        x              = expanded_inc,
        # >< rich had his intervals as (l,u] except for the bottom quintile
        #    which included expanded_inc == 0... i don't love the way i did
        #    this but I couldn't think of a better work around
        breaks         = c(-Inf, max(expanded_inc[expanded_inc < 0]) / 2,
                           cutoffs, Inf),
        labels         = c("Negative income", "Bottom quintile",
                           "Second quintile", "Middle quintile",
                           "Fourth quintile", "80% - 90%", "90% - 99%",
                           "99% - 99.9%", "Top 0.1%"),
        right          = T,
        include.lowest = T
      )
    )

  # aggregate time burdens by income group
  # >< not sure why these aren't weighted with person weights
  microdata %>%
    group_by(income_group, policy) %>%
    summarise(
      
      # average time burden
      mean_burden  = wtd.mean(x = burden, weights = weight),
      # total time burden
      total_burden = sum(burden * weight),
      
      # counts
      count        = sum(weight),
      simple_filer = sum(simple_filer * weight)
    ) %>% ungroup() %>%
  return()
}



build_table = function(year) {

  #----------------------------------------------------------------------------
  # Generates time burden tables by year
  # 
  # Parameters:
  #   - year (int)          : year to calculate time burdens for
  #
  # Returns: void. 
  #----------------------------------------------------------------------------
  
  
  # >< wasn't exactly sure what the best way to go about this was... currently
  #    i have everything written to a csv since i wasn't sure how a workbook
  #    should be formatted
  policy_ids = c("baseline", "partial-simple", "partial-mod_simple",
               "partial-back_future", "partial-ubi")
  
  # empty data frame
  results = data.frame(
    income_group = c('Negative income', 'Bottom quintile', 'Second quintile',
                     'Middle quintile', 'Fourth quintile', '80% - 90%',
                     '90% - 99%', '99% - 99.9%', 'Top 0.1%', "OVERALL")
  )
  
  for (id in policy_ids){
    
    # read and process data
    microdata  = process_for_time_burden(id, year)
    # calculate time burdens
    fixed_cost = calc_fixed_cost()
    microdata  = calc_time_burden(microdata, fixed_cost)
    output     = calc_summary_stats(microdata)

    # add time burdens for each policy
    results = results %>%
      left_join(output %>%
                  # compute overall time burdens
                  bind_rows(output %>%
                              reframe(tibble(
                                mean_burden = wtd.mean(x       = mean_burden,
                                                       weights = count),
                                policy      = id
                              )) %>%
                              mutate(income_group = "OVERALL")) %>%
                  mutate( burden = mean_burden / 60 ) %>%
                  select(policy, income_group, burden) %>%
                  pivot_wider(names_from = policy, values_from = burden),
                by = c("income_group"))
  }
  
  # compute hour and percent changes from current law
  results = results %>%
    mutate(across(.cols = all_of(policy_ids) & !baseline,
                  .fns  = list(hrs_change = function(x)  x - baseline,
                               pct_change = function(x) (x - baseline)
                                                          / baseline * 100 )
                 )) %>%
    # reorder columns
    relocate(
      starts_with(policy_ids), .after = last_col()
    ) %>%
    # round numbers to nearest tenths
    mutate(across(.cols = where(is.numeric),
                  .fns  = function(x) round(x,1) ))
  
  
  # write csv
  write_csv(results, paste0("timeburden_", year,".csv"))
}




#-------------------------------------------------------
# everything below here is just for me to check my work
#-------------------------------------------------------


build_table(2025)
build_table(2026)

print("all done!")

  # # >< these are the actual values from the stata code... but even then the
  # #    results are only marginally closer :((
  # cutoffs =
  #   # cl, 2025
  #   data.frame(
  #     vals   = c(28461.924,
  #                60860.07,
  #                101541.24,
  #                171680.52,
  #                248626.69,
  #                937241.94,
  #                4834758.5)
  #   ) %>% mutate(
  #     policy = "cl",
  #     year   = 2025
  #   ) %>%
  #   # mod_simple, 2025
  #   bind_rows((data.frame(
  #     vals   = c(20595.91,
  #                46629.109,
  #                87500.477,
  #                154895.94,
  #                231010.38,
  #                869759.06,
  #                4321755)
  #   ) %>% mutate(
  #     policy = "mod_simple",
  #     year   = 2025
  #   ))) %>%
  #   # simple, 2025
  #   bind_rows((data.frame(
  #     vals   = c(20595.91,
  #                46629.109,
  #                87477.758,
  #                154895.94,
  #                231006.53,
  #                869574.25,
  #                4321677)
  #   ) %>% mutate(
  #     policy = "simple",
  #     year   = 2025
  #   ))) %>%
  #   # back_future, 2025
  #   bind_rows((data.frame(
  #     vals   = c(20595.91,
  #                46629.109,
  #                87477.758,
  #                154895.94,
  #                231006.53,
  #                869574.25,
  #                4321677)
  #   ) %>% mutate(
  #     policy = "back_future",
  #     year   = 2025
  #   ))) %>% 
  #   # ubi, 2025
  #   bind_rows((data.frame(
  #     vals   = c(20595.91,
  #                46629.109,
  #                87477.758,
  #                154895.94,
  #                231006.53,
  #                869574.25,
  #                4321677)
  #   ) %>% mutate(
  #     policy = "ubi",
  #     year   = 2025
  #   ))) %>%
  #   # cl, 2026
  #   bind_rows((data.frame(
  #     vals   = c(29243.885,
  #                62636.074,
  #                104347.96,
  #                176338.89,
  #                255178.38,
  #                955624.38,
  #                4890008)
  #   ) %>% mutate(
  #     policy = "cl",
  #     year   = 2026
  #   ))) %>%
  #   # mod_simple, 2026
  #   bind_rows((data.frame(
  #     vals   = c(21243.158,
  #                47975.57,
  #                89842.102,
  #                158067.22,
  #                237213.48,
  #                887523.5,
  #                4374868.5)
  #   ) %>% mutate(
  #     policy = "mod_simple",
  #     year   = 2026
  #   ))) %>%
  #   # simple, 2026
  #   bind_rows((data.frame(
  #     vals   = c(21243.158,
  #                47986.566,
  #                89839.156,
  #                158027.55,
  #                237205.19,
  #                887488.13,
  #                4374703)
  #   ) %>% mutate(
  #     policy = "simple",
  #     year   = 2026
  #   ))) %>%
  #   # back_future, 2026
  #   bind_rows((data.frame(
  #     vals   = c(21717.732,
  #                48734.305,
  #                91344.617,
  #                160878.38,
  #                238288.67,
  #                888622,
  #                4386381)
  #   ) %>% mutate(
  #     policy = "back_future",
  #     year   = 2026
  #   ))) %>% 
  #   # ubi, 2026
  #   bind_rows((data.frame(
  #     vals   = c(21717.732,
  #                48734.305,
  #                91344.617,
  #                160878.38,
  #                238288.67,
  #                888622,
  #                4386381)
  #   ) %>% mutate(
  #     policy = "ubi",
  #     year   = 2026
  #   )))