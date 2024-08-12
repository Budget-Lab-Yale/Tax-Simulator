
baseline = '/vast/palmer/scratch/sarin/jmk263/Tax-Simulator/v1/202406250850/baseline/static/detail/2026.csv' %>%
  fread() %>%
  tibble()

tax_law = '/vast/palmer/scratch/sarin/jmk263/Tax-Simulator/v1/202406250850/baseline/static/supplemental/tax_law.csv' %>%
  read_csv()


out = baseline %>%
  mutate(
    expanded_inc_eq = (expanded_inc / ((1 + int(!is.na(male2)) + .7 * n_dep)^.7)) * 2^.7,
    decile_expanded = cut(expanded_inc_eq,
                          wtd.quantile(
                            expanded_inc_eq, weight,
                            probs = c(seq(0,.9,.1),.99,.999, 1)),
                            labels = c('0-9th Income Percentile',
                              paste0(seq(10,90,10),'th Income Percentile'),
                                     '99th Income Percentile',
                                     '99.9th Income Percentile'),
                           include.lowest = T
                          ),
    etr_eq = liab_iit_net / expanded_inc_eq
  ) %>%
  select(
    id, decile_expanded, etr_eq, expanded_inc_eq, weight
  ) 

check = out %>% filter(is.na(decile_expanded))

out %>% write_csv(., 'horizontal_equity_histogram_data.csv')

out %>%
  group_by(decile_expanded) %>%
  reframe(
    count = sum(weight)
  ) %>%
  mutate(pct = count / sum(count))

tcja = c('baseline', 'full_extension', 'partial_extension', 'sarin_clausing')%>%
  map(
    .f = ~ file.path('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403081546',.x,'static/detail/2026.csv') %>%
      fread() %>%
      tibble() %>%
      mutate(
        etr = liab_iit_net / expanded_inc
      ) %>%
      filter(
        (between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & n_dep_ctc < 2 & filing_status ==2) |
          (between(expanded_inc, 5e5 * .9, 5e5 *1.1) & filing_status == 1) |
          (between(expanded_inc, 2e5 * .97, 2e5 *1.03) & filing_status == 1),
        between(etr, -1, 1)
      ) %>%
      mutate(bucket = case_when(
        between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & (n_dep_ctc == 0)                         ~ "Figure 1.1",
        between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & (n_dep_ctc == 1)                         ~ "Figure 1.2",
        between(expanded_inc, 5e5  * .9, 5e5  * 1.1) & ((kg_lt + kg_st + div_pref + div_ord) / expanded_inc < .5)   ~ "Figure 2.1",
        between(expanded_inc, 5e5  * .9, 5e5  * 1.1) & ((kg_lt + kg_st + div_pref + div_ord) / expanded_inc >= .5)  ~ "Figure 2.2",
        between(expanded_inc, 2e5  * .97, 2e5  * 1.03) & (sch_e / expanded_inc <  .75)            ~ "Figure 3.1",
        between(expanded_inc, 2e5  * .97, 2e5  * 1.03) & (sch_e / expanded_inc >= .75)            ~ "Figure 3.2",
        T                                                                                         ~ "err"
      )) %>%
      filter(bucket != "err") %>%
      group_by(bucket) %>%
      summarise(
        inc  = sum(expanded_inc * weight),
        liab = sum(liab_iit_net * weight)
      ) %>%
      mutate(
        etr      = liab / inc,
        scenario = .x,
        figure   = substr(bucket, 1, 8),
        dot      = ifelse(substr(bucket, 10, 10)=='1', 'First', 'Second')
      ) %>%
      select(scenario, figure, dot, etr) %>%
      pivot_wider(names_from = dot, values_from = etr)
    #write_csv(., file = file.path('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403130110',.x,'static/supplemental/horizontal_characters.csv'))
  ) %>%
  bind_rows()

out = tcja %>%
  bind_rows(ctc)

out %>% write_csv(., 'family_comparison_data.csv')

ctc = c('edelberg_kearney', 'fsa', 'fsa_ctc', 'perm_arpa_ctc', 'perm_tcja_ctc') %>%
  map(
    .f = ~ file.path('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403130110',.x,'static/detail/2026.csv') %>%
      fread() %>%
      tibble() %>%
      mutate(
        etr = liab_iit_net / expanded_inc
      ) %>%
      filter(
        (between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & n_dep_ctc < 2 & filing_status ==2) |
          (between(expanded_inc, 5e5 * .9, 5e5 *1.1) & filing_status == 1) |
          (between(expanded_inc, 2e5 * .97, 2e5 *1.03) & filing_status == 1),
        between(etr, -1, 1)
      ) %>%
      mutate(bucket = case_when(
        between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & (n_dep_ctc == 0)                         ~ "Figure 1.1",
        between(expanded_inc, 25e3 * .97, 25e3 * 1.03) & (n_dep_ctc == 1)                         ~ "Figure 1.2",
        between(expanded_inc, 5e5  * .9, 5e5  * 1.1) & ((kg_lt + kg_st + div_pref + div_ord) / expanded_inc < .5)   ~ "Figure 2.1",
        between(expanded_inc, 5e5  * .9, 5e5  * 1.1) & ((kg_lt + kg_st + div_pref + div_ord) / expanded_inc >= .5)  ~ "Figure 2.2",
        between(expanded_inc, 2e5  * .97, 2e5  * 1.03) & (sch_e / expanded_inc <  .75)            ~ "Figure 3.1",
        between(expanded_inc, 2e5  * .97, 2e5  * 1.03) & (sch_e / expanded_inc >= .75)            ~ "Figure 3.2",
        T                                                                                         ~ "err"
      )) %>%
      filter(bucket != "err") %>%
      group_by(bucket) %>%
      summarise(
        inc  = sum(expanded_inc * weight),
        liab = sum(liab_iit_net * weight)
      ) %>%
      mutate(
        etr      = liab / inc,
        scenario = .x,
        figure   = substr(bucket, 1, 8),
        dot      = ifelse(substr(bucket, 10, 10)=='1', 'First', 'Second')
      ) %>%
      select(scenario, figure, dot, etr) %>%
      pivot_wider(names_from = dot, values_from = etr)
      #write_csv(., file = file.path('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/202403130110',.x,'static/supplemental/horizontal_characters.csv'))
  ) %>%
  bind_rows()

baseline = '/vast/palmer/scratch/sarin/jmk263/Tax-Simulator/v1/202406250850/baseline/static/detail/2026.csv' %>%
  fread() %>%
  tibble()

 box_table = baseline %>%
   left_join(
     tax_law %>% 
       filter(year == 2026) %>% 
       select(filing_status, starts_with('ord.brackets')), 
     by = 'filing_status'
   ) %>%
  mutate(
    across(
      .cols  = starts_with('liab_brac'), 
      .fns   = ~ ifelse(. == 0, 0, 1),
      .names = 'has_{col}'
    ),
    expanded_inc_eq = (expanded_inc / ((1 + int(!is.na(male2)) + .7 * n_dep)^.7)) * 2^.7,
    last_bracket = rowSums(across(.cols = starts_with('has_liab_brac'))), 
    last_bracket = if_else(agi <= 0, 1, last_bracket),
    last_bracket = case_when(
      between(expanded_inc_eq, 
              wtd.quantile(expanded_inc_eq, weight, .99),
              wtd.quantile(expanded_inc_eq, weight, .999)) ~ 8,
      expanded_inc_eq > wtd.quantile(expanded_inc_eq, weight, .999) ~ 9,
      T ~ last_bracket
    ),
    etr = liab_iit_net / expanded_inc_eq
    ) %>% 
   group_by(last_bracket) %>%
   reframe(
     quantile = seq(.05, .95, .01),
     val = wtd.quantile(etr, weight, seq(.05, .95, .01))
   ) %>%
   mutate(Scenario = 'Current Law')



 
 
 scenario = '/vast/palmer/scratch/sarin/jmk263/Tax-Simulator/v1/202406250850/child_credits/static/detail/2026.csv' %>%
   fread() %>%
   tibble() 
 
 box_table %<>% bind_rows(
   scenario %>%
   mutate(
     across(
       .cols  = starts_with('liab_brac'), 
       .fns   = ~ ifelse(. == 0, 0, 1),
       .names = 'has_{col}'
     ),
     expanded_inc_eq = (expanded_inc / ((1 + int(!is.na(male2)) + .7 * n_dep)^.7)) * 2^.7,
     last_bracket = rowSums(across(.cols = starts_with('has_liab_brac'))), 
     last_bracket = if_else(agi <= 0, 1, last_bracket),
     last_bracket = case_when(
       between(expanded_inc_eq, 
               wtd.quantile(expanded_inc_eq, weight, .99),
               wtd.quantile(expanded_inc_eq, weight, .999)) ~ 8,
       expanded_inc_eq > wtd.quantile(expanded_inc_eq, weight, .999) ~ 9,
       T ~ last_bracket
     ),
     etr = liab_iit_net / expanded_inc_eq
   ) %>% 
   group_by(last_bracket) %>%
   reframe(
     quantile = seq(.05, .95, .01),
     val = wtd.quantile(etr, weight, seq(.05, .95, .01))
   ) %>%
   mutate(Scenario = 'Eliminating Certain Tax Provisions')
   ) %>%
   mutate(
     last_bracket_g = case_when(
       last_bracket==1 ~ 'Bottom Bracket',
       last_bracket==2 ~ '2nd Bracket',
       last_bracket==3 ~ '3rd Bracket',
       last_bracket==7 ~ 'Top Bracket',
       last_bracket==8 ~ '99th-99.9th Income Percentile',
       last_bracket==9 ~ 'Top .01 Income Percentile',
       T ~ paste0(last_bracket, "th Bracket")
   ))
 
 
 box_table = baseline %>%
   left_join(
     tax_law %>% 
       filter(year == 2026) %>% 
       select(filing_status, starts_with('ord.brackets')), 
     by = 'filing_status'
   ) %>%
   mutate(
     across(
       .cols  = starts_with('liab_brac'), 
       .fns   = ~ ifelse(. == 0, 0, 1),
       .names = 'has_{col}'
     ),
     expanded_inc_eq = (expanded_inc / ((1 + int(!is.na(male2)) + .7 * n_dep)^.7)) * 2^.7,
     last_bracket = rowSums(across(.cols = starts_with('has_liab_brac'))), 
     last_bracket = if_else(agi <= 0, 1, last_bracket),
     last_bracket = case_when(
       between(expanded_inc_eq, 
               wtd.quantile(expanded_inc_eq, weight, .99),
               wtd.quantile(expanded_inc_eq, weight, .999)) ~ 8,
       expanded_inc_eq > wtd.quantile(expanded_inc_eq, weight, .999) ~ 9,
       T ~ last_bracket
     ),
     etr = liab_iit_net / expanded_inc_eq
   ) %>% 
   group_by(last_bracket) %>%
   reframe(
     quantile = seq(.05, .95, .01),
     val = wtd.quantile(etr, weight, seq(.05, .95, .01))
   ) %>%
   mutate(Scenario = 'Current Law')%>%
   mutate(
     last_bracket_g = case_when(
       last_bracket==1 ~ 'Bottom Bracket',
       last_bracket==2 ~ '2nd Bracket',
       last_bracket==3 ~ '3rd Bracket',
       last_bracket==7 ~ 'Top Bracket',
       last_bracket==8 ~ '99th-99.9th Income Percentile',
       last_bracket==9 ~ 'Top .01 Income Percentile',
       T ~ paste0(last_bracket, "th Bracket")
     ))
 
 
 
p = box_table %>%
   filter(quantile %in% c(.1, .5, .9)) %>%
   mutate(line_group_aes = as.integer(last_bracket) + as.integer(Scenario=='Eliminating Certain Tax Provisions') / 4
          ) %>%

   
   ggplot(aes(x=val, y = line_group_aes, color = Scenario)) +
   geom_point(size = 7) +
   geom_line(aes(group = as.factor(line_group_aes)), linewidth = 3) +
   theme_bw() +
   geom_vline(xintercept = 0, color = 'white') +
   labs(x = 'Effective Tax Rate', y = 'Tax Bracket') + 
  scale_y_continuous(breaks = 1:length(unique(box_table$last_bracket_g)), labels = unique(box_table$last_bracket_g)) + 
  ggtitle("10th, 50th, and 90th Percentile of Effective Tax Rates") +
  theme(
    plot.background = element_rect(fill='transparent', color= NA),
    
    panel.background = element_rect(fill='transparent', color = 'white'),
    panel.grid.major = element_line(color = '#DDDDDD', linewidth = 2),
    panel.grid.minor = element_line(color = NA),
    panel.border = element_rect(fill = 'transparent', color = 'white'),
    
    legend.background = element_rect(fill='transparent', color = NA),
    legend.box.background = element_rect(fill='transparent', color = NA),
    legend.key = element_rect(fill='transparent'),
    legend.key.size = unit(1.75, "cm"),
    legend.position = 'top',
    legend.title = element_blank(),
    
    text = element_text(
      face = 'bold',
      color = 'white',
      size = 30
      ),
    axis.text.x = element_text(face = 'bold', color="white"), 
    axis.text.y = element_text(face = 'bold', color="white")
  ) 
  #+ scale_color_manual(values = c('#F8766D'))

ggsave('ETR Percentile Distribution Transparent Black Text shorter bounds Equalized.png', p, bg='transparent',
       width= 20.313, height = 11.875)



# 
# stat_chart = baseline %>%
#   mutate(across(
#     .cols  = starts_with('liab_brac'), 
#     .fns   = ~ ifelse(. == 0, 0, 1),
#     .names = 'has_{col}'
#   ),
#   last_bracket = rowSums(across(.cols = starts_with('has_liab_brac'))), 
#   last_bracket = if_else(agi <= 0, 1, last_bracket),
#   last_bracket = case_when(
#     between(expanded_inc, 
#             wtd.quantile(expanded_inc, weight, .99),
#             wtd.quantile(expanded_inc, weight, .999)) ~ 8,
#     expanded_inc > wtd.quantile(expanded_inc, weight, .999) ~ 9,
#     T ~ last_bracket
#   )
#   ) %>%
#   select(id, last_bracket, weight, liab_iit_net) %>%
#   rename(liab_b = liab_iit_net) %>%
#   left_join(., scenario %>%
#               mutate(across(
#                 .cols  = starts_with('liab_brac'), 
#                 .fns   = ~ ifelse(. == 0, 0, 1),
#                 .names = 'has_{col}'
#               ),
#               last_bracket = rowSums(across(.cols = starts_with('has_liab_brac'))), 
#               last_bracket = if_else(agi <= 0, 1, last_bracket),
#               last_bracket = case_when(
#                 between(expanded_inc, 
#                         wtd.quantile(expanded_inc, weight, .99),
#                         wtd.quantile(expanded_inc, weight, .999)) ~ 8,
#                 expanded_inc > wtd.quantile(expanded_inc, weight, .999) ~ 9,
#                 T ~ last_bracket
#               )
#               ) %>%
#               select(id, last_bracket, weight, liab_iit_net) %>%
#               rename(liab_r = liab_iit_net),
#             by=c("id", "last_bracket", "weight")
#                   ) %>%
#   mutate(stat = case_when(
#     liab_b - liab_r > 0 ~ "Paid Over Statutory",
#     liab_b - liab_r < 0 ~ "Paid Below Statutory",
#     T ~ "Paid Statutory"
#   )
#   ) %>%
#   group_by(last_bracket, stat) %>%
#   summarise(
#     count = sum(weight)
#   ) %>%
#   group_by(last_bracket) %>%
#   mutate(
#     pct = count / sum(count)
#   ) %>%
#   select(-count) %>%
#   pivot_wider(names_from = stat, values_from = pct) %>%
#   write_csv(., 'stat_chart.csv')
  


 
 
 # ggplot(box_table_s, aes(x=income_group, y=etr, color=scen, weight = weight)) +
 #   geom_boxplot() + coord_flip()+
 #   stat_summary(fun.y = median, geom='point')
 

 
 
 
 # tibble(
 #   income_group = c(1:5),
 #   p5  = c(-0.1, 0, 0, 0.1, 0.1), 
 #   p50 = c(0, 0.1, 0.05, 0.12, 0.12), 
 #   p95 = c(0.05, 0.15, 0.2, 0.2, 0.3)
 # ) %>% 
 #   
 #   # Duplicate it and adjust to pretend like there's a reform 
 #   expand_grid(scenario = c('current_law', 'reform')) %>%  
 #   mutate(p5  = if_else(scenario == 'reform', p5 + 0.03, p5), 
 #          p95 = if_else(scenario == 'reform', p5 + 0.02, p95)) %>% 
 #   
 #   # Reshape long
 #   pivot_longer(cols = -c(scenario, income_group), names_to = 'pctile') %>% 
 #   
 #   # Create a unique numeric identifer for income group X scenario
 #   mutate(line_group_aes = income_group + as.integer(scenario == 'reform') / 4) %>% 
 #   
 #   # Trick ggplot into thinking the y axis has numeric meaning
 #   ggplot(aes(x = value, y = line_group_aes, colour = scenario)) + 
 #   geom_point() + 
 #   geom_line(aes(group = as.factor(line_group_aes))) + 
 #   theme_bw() + 
 #   geom_vline(xintercept = 0) + 
 #   labs(x = 'ETR', y = 'Income group')
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
