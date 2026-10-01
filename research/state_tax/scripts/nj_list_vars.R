x = readRDS('research/state_tax/cross_model/cache/fed_calc_2023.rds')$tax_units
n = names(x); cat(length(n), 'columns\n')
cat(grep('adj|ira|sl_|se_|hsa|keogh|sep|educ|alim|loss|kg|gain|sch_e|part|scorp|sole|bus|farm|rent|estate|other|gross', n, value = TRUE), sep = '\n')
