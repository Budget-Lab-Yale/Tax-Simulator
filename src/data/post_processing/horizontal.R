

get_horizontal_dist = function(tax_units, scen_id, calibrators) {
  p = calibrators$P
  e = calibrators$e

  negative = tax_units %>% filter(expanded_inc <= 0) %>%
    mutate(
      jitter = 0,
      inc_t = expanded_inc,
      inc_eq = (expanded_inc / ((1 + int(!is.na(male2)) + p * n_dep)^e)) * 2^e,
      etr = case_when(
        expanded_inc == 0                   ~ 0,
        expanded_inc < 0 & liab_iit_net < 0 ~ -1 * liab_iit_net / inc_eq,
        T                                   ~ liab_iit_net / inc_eq
      ),
      tile = ifelse(inc_eq == 0, 0, -1)
    ) 
  
  sub_units = tax_units %>%
    filter(expanded_inc > 0) %>%
    mutate(
      jitter = runif(nrow(.), 0, 1) / 1000,
      inc_t = .5 * (expanded_inc + sqrt(expanded_inc^2 + 1)),
      inc_eq = ((inc_t / ((1 + int(!is.na(male2)) + p * n_dep)^e)) * 2^e) + jitter,
      etr = liab_iit_net / inc_eq,
      tile = ntiles.wtd(inc_eq, 1000, weight)
    ) %>%
    stick_figures() %>%
    rbind(negative) %>%
    mutate(
    inc_cat = factor(case_when(
        tile == -1        ~ 'Negative',
        inc_eq == 0       ~ 'No Income',
        tile %in% 1:99    ~ '1-9',
        tile %in% 100:199 ~ "'10-19",
        tile %in% 200:299 ~ '20-29',
        tile %in% 300:399 ~ '30-39',
        tile %in% 400:499 ~ '40-49',
        tile %in% 500:599 ~ '50-59',
        tile %in% 600:699 ~ '60-69',
        tile %in% 700:799 ~ '70-79',
        tile %in% 800:899 ~ '80-89',
        tile %in% 900:999 ~ '90-90',
        T                 ~ 'Top'
      ), levels = c('Negative', 'No Income', '1-9', "'10-19", '20-29', '30-39', '40-49',
                    '50-59', '60-69', '70-79', '80-89', '90-99', 'Top'))
    )   %>%
    mutate(inc_eq = inc_eq - jitter) %>%
    filter(! tile %in% c(1:10))
  
  sub_units %>% group_by(inc_cat) %>%
    summarise(
      `id` = scen_id,
      `P` = p,
      `e` = e,
      `Average Equalized Income` = wtd.mean(inc_eq, weight),
      `Average Tax Rate` = sum(liab_iit_net) / sum(inc_eq),
      `Lowest Tax Rate` = min(etr),
      `Highest Tax Rate` = max(etr),
      `Standard Deviation of Tax Rate` = sqrt(weighted.var(etr, weight, na.rm = T)),
      `Interquartile Range` = IQR(etr)
    ) %>%
    return()
}

stick_figures - function(tax_units) {
  tax_units %>%
    mutate(
      etr_b = liab_iit_net / expanded_inc,
      bucket = round(expanded_inc, -2)
      #Maybe something with sch_e income
    ) %>%
    filter(
      ((bucket %in% c(22500:27500)) & (n_dep_ctc < 2)) |
        ((bucket %in% c(70e3:77500))  ) |
        ((bucket %in% c(185e3:215e3))),
      between(etr_b, -1, 1), filing_status < 2
    ) %>%
    mutate(bucket = case_when(
      (bucket %in% c(22500:27500)) & (n_dep_ctc == 0) ~ "Figure 1.1",
      (bucket %in% c(22500:27500)) & (n_dep_ctc == 1) ~ "Figure 1.2",
      (bucket %in% c(70e3:77500)) & sch_e == 0        ~ "Figure 2.1",
      (bucket %in% c(70e3:77500)) & sch_e == 1        ~ "Figure 2.2",
      bucket %in% c(185e3:215e3) ~ "Figure 3.1",
      T                          ~ 0 # FLAG
    )) %>%
  group_by(bucket) %>%
    summarise(
      inc = sum(expanded_inc * weight),
      liab = sum(liab_iit_net * weight)
    ) %>%
    mutate(etr = liab / inc) %>%
    write_csv(., file = file.path(globals$output_root, id, 'static/totals/stick_figures.csv'))
  
  return(tax_units)
}




