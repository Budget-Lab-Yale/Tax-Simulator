#-----------------------------------------------------------
# Function to calculate education credits
#-----------------------------------------


calc_ed_credits = function(tax_unit, fill_missings = F) {
  
  #----------------------------------------------------------------------------
  # Calculates value of the American Opportunity Credit (AOC) and the Lifetime 
  # Learning Credit (LLC).
  # 
  # Parameters:
  #   - tax_unit (df | list) : either a dataframe or list containing required
  #                            variables (listed below)
  #   - fill_missings (bool) : whether to populate any unsupplied variables
  #                            with 0s (used in testing, not in simulation)
  #
  # Returns: dataframe of following variables:
  #   - ed_nonref (dbl) : value of refundable AOC
  #   - ed_ref (dbl)    : value of LLC and nonrefundable AOC 
  #----------------------------------------------------------------------------
  
  req_vars = c(
    
    # Tax unit attributes
    'agi',      # (dbl) Adjusted Gross Income
    'aoc_exp',  # (dbl) AOC-qualifying education expenses 
    'llc_exp',  # (dbl) LLC-qualifying education expenses
    'liab_bc',  # (dbl) income tax liability before credits (including AMT)
    'ftc',      # (dbl) value of foreign tax credit value
    'cdctc',    # (dbl) Child and Dependent Care Tax Credit
    'old_cred', # (dbl) value of Elderly and Disabled Credit
    
    # Tax law attributes
    'ed.aoc_rates[]',      # (dbl[]) AOC credit rate per dollar of qualifying expenses
    'ed.aoc_brackets[]',   # (int[]) expense brackets for AOC credit rates
    'ed.aoc_po_thresh',    # (int)   AGI threshold above which AOC phases out 
    'ed.aoc_po_range',     # (int)   AGI range over which AOC phases out
    'ed.aoc_refund_rate',  # (dbl)   share of AOC value available as refundable credit
    'ed.llc_rates[]',      # (dbl[]) AOC credit rate per dollar of qualifying expenses
    'ed.llc_brackets[]',   # (int[]) expense brackets for LLC credit rates
    'ed.llc_po_thresh',    # (dbl)   AGI threshold above which LLC phases out
    'ed.llc_po_range'      # (int)   AGI range over which LLC phases out
  )
  
  tax_unit %>% 
    
    # Parse tax unit object passed as argument
    parse_calc_fn_input(req_vars, fill_missings) %>% 
    
    # Calculate credit-eligible expenses for both credits 
    bind_cols(
      integrate_rates_brackets(
        df              = (.), 
        n_brackets      = NULL,
        prefix_brackets = 'ed.aoc_brackets',
        prefix_rates    = 'ed.aoc_rates',
        y               = 'aoc_exp',
        output_name     = 'aoc',
        by_bracket      = F
      ),
      integrate_rates_brackets(
        df              = (.), 
        n_brackets      = NULL,
        prefix_brackets = 'ed.llc_brackets',
        prefix_rates    = 'ed.llc_rates',
        y               = 'llc_exp',
        output_name     = 'llc',
        by_bracket      = F
      )
    ) %>% 
    
    mutate(
      
      # Phase out AOC
      po_share_aoc = pmin(1, pmax(0, agi - ed.aoc_po_thresh) / ed.aoc_po_range),
      aoc          = aoc * (1 - po_share_aoc),
      
      # Allocate AOC between refundable and nonrefundable components 
      aoc_ref    = aoc * ed.aoc_refund_rate,
      aoc_nonref = aoc - aoc_ref,
      
      # Phase out LLC 
      po_share_llc = pmin(1, pmax(0, agi - ed.llc_po_thresh) / ed.llc_po_range),
      llc          = llc * (1 - po_share_llc),
      
      # Limit nonrefundable credit to positive liability
      liab      = pmax(0, liab_bc - ftc - cdctc - old_cred),
      ed_nonref = pmin(liab, aoc_nonref + llc)
    
    ) %>% 
    
    # Keep variables to return
    select(ed_nonref, ed_ref = aoc_ref) %>% 
    return()
}
