#-----------------------------------------------------------------
# Estimates IGE based on mobility matrix from Chetty et al (2014)
#-----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(Hmisc)

# Set params
output_root = '/gpfs/gibbs/project/sarin/shared/model_data/Tax-Simulator/v1/2024021914/baseline/static/detail'
first_year  = 2025

#-----------
# Load data
#-----------

# Read intergenerational mobility parameters
mobility_matrix = read_csv('resources/mobility_matrix.csv') %>% 
  pivot_longer(cols            = -child_rank, 
               names_to        = 'parent_rank', 
               names_transform = as.integer,
               values_to       = 'pdf') %>%
  relocate(parent_rank, .before = everything()) %>% 
  arrange(parent_rank, child_rank) 

# Read tax unit data
tax_units = c(first_year, first_year + 15) %>% 
  map(.f = ~ output_root %>% 
        file.path(paste0(.x, '.csv')) %>% 
        fread() %>% 
        tibble() %>% 
        mutate(year   = .x, 
               n_kids = (!is.na(dep_age1) & dep_age1 < 18) +
                        (!is.na(dep_age2) & dep_age2 < 18) + 
                        (!is.na(dep_age3) & dep_age3 < 18)) %>% 
        select(year, id, weight, n_kids, age1, age2, starts_with('dep_age'), agi)
      ) %>% 
  bind_rows() 


#--------------------------------
# Simulate parent-child outcomes
#--------------------------------

# Calculate income by rank in future year
future_agi = tax_units %>% 
  
  # Filter to 30-ish year olds
  filter(year == first_year + 15, age1 %in% 28:32) %>% 
  mutate(child_rank = cut(
      x = agi, 
      breaks = wtd.quantile(
        x       = .$agi, 
        weights = .$weight, 
        probs = 0:100/100
      ) + runif(101, -0.01, 0.01), 
      labels = 1:100
    ) %>% 
    as.integer()
  ) %>% 
  
  # Get average AGI by rank
  group_by(child_rank) %>% 
  summarise(agi = weighted.mean(agi, weight))


# Simulate child outcomes
child_outcomes = tax_units %>% 
  
  # Assign parent income rank
  filter(year == first_year, n_kids > 0) %>% 
  mutate(parent_rank = cut(
      x = agi, 
      breaks = wtd.quantile(
        x       = .$agi, 
        weights = .$weight, 
        probs = 0:100/100
      ), 
      labels = 1:100
    ) %>% 
    as.integer()
  ) %>% 
  
  # Reshape to child basis then filter to teens
  select(id, parent_weight = weight, starts_with('dep_age'), parent_rank, parent_agi = agi) %>% 
  pivot_longer(cols            = starts_with('dep_age'),
               names_prefix    = 'dep_age', 
               names_to        = 'child_index', 
               names_transform = as.integer, 
               values_to       = 'age') %>% 
  filter(!is.na(age), age %in% 13:17) %>% 
  
  # Expand in child outcomes and update weights 
  left_join(mobility_matrix, by = 'parent_rank', relationship = 'many-to-many') %>% 
  mutate(parent_weight = parent_weight * pdf) %>% 
  select(-pdf) %>% 
  
  # Merge income in adulthood
  left_join(future_agi, by = 'child_rank')


#-----------------
# Run regressions
#-----------------

# Build filtering functions for each dataset 
reg_samples = list(
  
  `Baseline (exclude zeros)` = list(
    chetty_coeff = 0.344, 
    filter_fn    = function(df) { 
      df %>% 
        filter(parent_agi > 0, agi > 0)
    }
  ),
  
  `Recode zeros to $1` = list(
    chetty_coeff = 0.618, 
    filter_fn    = function(df) { 
      df %>% 
        mutate(across(.cols = c(parent_agi, agi), 
                      .fns  = ~ pmax(1, .)))
    }
  ), 
  
  `Recode zeros to $1000` = list(
    chetty_coeff = 0.413, 
    filter_fn    = function(df) { 
      df %>% 
        mutate(across(.cols = c(parent_agi, agi), 
                      .fns  = ~ pmax(1e3, .)))
    }
  ), 
  
  `P10-P90 parents only` = list(
    chetty_coeff = 0.452, 
    filter_fn    = function(df) { 
      df %>% 
        filter(parent_rank %in% 10:90, agi > 0)
    }
  )
)

# Get IGE estimates for each subset
ige_estimates = reg_samples %>% 
  map(.f = ~ list(
    chetty_coeff = .x$chetty_coeff, 
    sim_coeff = lm(
      formula = log(agi) ~ log(parent_agi), 
      data    = .x$filter_fn(child_outcomes), 
      weights = .x$filter_fn(child_outcomes)$parent_weight
    )$coeff['log(parent_agi)'] %>% set_names(NULL)
    )
  ) 

# Plot coefficients
tibble(
  subset           = names(ige_estimates), 
  `Chetty et al.`  = round(unlist(map(ige_estimates, ~ .x$chetty_coeff)), 3),
  `Our simulation` = round(unlist(map(ige_estimates, ~ .x$sim_coeff)), 3)
) %>% 
  pivot_longer(cols     = -subset, 
               names_to = 'source') %>% 
  ggplot(aes(x = subset, y = value, fill = source)) + 
  geom_col(position = 'dodge') + 
  geom_text(aes(label = value), 
            position = position_dodge(0.9),
            vjust = 1.75) + 
  theme_bw() +
  scale_fill_manual(values = c('#bfbfbf', '#ed8585')) + 
  scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
  labs(x = 'Regression subset', 
       y = 'IGE', 
       fill = 'Source') +
  ggtitle('Comparing implied IGE in our simulations to that of Chetty et al.')

