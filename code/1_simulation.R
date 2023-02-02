# title: 1. simulation for Felbermayr, Mahlkow, & Sandkamp (2023): Cutting through the value chain: the long-run effects of decoupling the East from the West
# DOI: 10.1007/s10663-022-09561-w
# date: 2022-10-21
# author: Hendrik

# load packages
pacman::p_load(KITE) # version: 22.03 Master
pacman::p_load(data.table)
pacman::p_load(tidyverse)
pacman::p_load(haven)
pacman::p_load(parallel)
pacman::p_load(foreach)
pacman::p_load(doParallel)


# data
initial_conditions = read_rds("input/GTAP_10/initial_conditions_2014.rds") # note: missing because of copyright law 

elasticities = read_dta("input/GTAP_10/final_tariff_GTAP_2020_10_08.dta") %>% setDT # Fontagné et al. https://sites.google.com/view/product-level-trade-elasticity)
elasticities = elasticities[, .(sector = gtap_sector, value = -1/epsilon_GTAP)] # inverse of epsilon
elasticities = merge(elasticities, initial_conditions$trade_elasticity[, .(sector)], by = "sector", all.y = T)
initial_conditions$trade_elasticity = left_join(elasticities, initial_conditions$trade_elasticity, by = "sector") %>%
  mutate(value = coalesce(value.x, value.y)) %>% 
  select(-value.x, -value.y)

# set scenarios ----
EU27 = c("DEU", "AUT", "BEL", "DNK", "FIN", "FRA", "GRC", "IRL", "ITA", "LUX", "NLD", "PRT", "ESP", "SWE", "MLT", "CYP", "EST", "LTU", "LVA", "CZE", 
         "HUN", "BGR", "ROU", "POL", "SVK", "SVN", "HRV")
US_allies = c(EU27, "USA", "CAN", "ISL", "NOR", "GBR", "TUR", "ALB", "PHL", "AUS", "NZL", "KOR", "JPN", "TWN")
BRIC = c("BRA", "RUS", "IND", "CHN")
ntb_change = copy(initial_conditions$tariff) %>% mutate(value = 1)
scenarios = list()

# unilateral trade war, imponent x target
scenarios[["EU-CHN_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% EU27 & origin == "CHN", 2, 1))
scenarios[["CHN-EU_uni"]] = ntb_change %>% mutate(value = ifelse(destination == "CHN" & origin %in% EU27, 2, 1))
scenarios[["US_all-CHN_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% US_allies & origin == "CHN", 2, 1))
scenarios[["CHN-US_all_uni"]] = ntb_change %>% mutate(value = ifelse(destination == "CHN" & origin %in% US_allies, 2, 1))
scenarios[["US_all-RUS_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% US_allies & origin == "RUS", 2, 1))
scenarios[["RUS-US_all_uni"]] = ntb_change %>% mutate(value = ifelse(destination == "RUS" & origin %in% US_allies, 2, 1))
scenarios[["EU-BRIC_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% EU27 & origin %in% BRIC, 2, 1))
scenarios[["BRIC-EU_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% BRIC & origin %in% EU27, 2, 1))
scenarios[["US_all-BRIC_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% US_allies & origin %in% BRIC, 2, 1))
scenarios[["BRIC-US_all_uni"]] = ntb_change %>% mutate(value = ifelse(destination %in% BRIC & origin %in% US_allies, 2, 1))

# retaliation
scenarios[["EU-CHN_retail"]] = ntb_change %>% 
  mutate(value = ifelse(destination %in% EU27 & origin == "CHN", 2, 
                        ifelse(destination == "CHN" & origin %in% EU27, 2, 1)))
scenarios[["EU-BRIC_retail"]] = ntb_change %>% 
  mutate(value = ifelse(destination %in% EU27 & origin %in% BRIC, 2, 
                        ifelse(destination %in% BRIC & origin %in% EU27, 2, 1)))
scenarios[["US_all-CHN_retail"]] = ntb_change %>% 
  mutate(value = ifelse(destination %in% US_allies & origin == "CHN", 2, 
                        ifelse(destination == "CHN" & origin %in% US_allies, 2, 1)))
scenarios[["US_all-RUS_retail"]] = ntb_change %>% 
  mutate(value = ifelse(destination %in% US_allies & origin == "RUS", 2, 
                        ifelse(destination == "RUS" & origin %in% US_allies, 2, 1)))
scenarios[["US_all-BRIC_retail"]] = ntb_change %>% 
  mutate(value = ifelse(destination %in% US_allies & origin %in% BRIC, 2, 
                        ifelse(destination %in% BRIC & origin %in% US_allies, 2, 1)))

# baseline ----
initial_conditions$trade_balance[, value := 0] # zero trade balance
baseline = update_equilibrium(initial_conditions = initial_conditions,
                                      verbose = 2,
                                      tolerance = 1e-4)

# update initial_conditions
initial_conditions$trade_share = baseline$equilibrium_new$trade_share_new
initial_conditions$value_added$value = initial_conditions$value_added$value * baseline$equilibrium_new$wage_change$value
initial_conditions$expenditure = baseline$equilibrium_new$expenditure_new

# model ----
initial_conditions = compute_initial_equilibrium(initial_conditions)
UseCores = detectCores() - 2 # how many cores to use
cl = parallel::makeCluster(UseCores, setup_strategy = "sequential") # register CoreCluster
registerDoParallel(cl)

foreach(c = names(scenarios),
        .packages = c("KITE", "readr", "data.table", "stringr")) %dopar% {
          output = update_equilibrium(initial_conditions = initial_conditions,
                                      ntb_new = scenarios[[c]],
                                      verbose = 2,
                                      tolerance = 1e-4)
          write_rds(output, path = str_c("output/results/results_", c, ".rds"), compress = "gz")
          rm(output)
        }
stopCluster(cl) # end cluster
gc()


# additional scenarios for revision ----
UseCores = detectCores() - 2 # how many cores to use
cl = parallel::makeCluster(UseCores, setup_strategy = "sequential") # register CoreCluster
registerDoParallel(cl)

# EU-CHN_retail from ntb_change = 5% to 100%
foreach(ntb_new = seq(5,100,5),
        .packages = c("KITE", "data.table", "tidyverse")) %dopar% {
          
  scenario = ntb_change %>% 
    mutate(value = ifelse(destination %in% EU27 & origin == "CHN", 
                          yes = (1 + ntb_new/100), 
                          no = ifelse(destination == "CHN" & origin %in% EU27, 
                                      yes = (1 + ntb_new/100), 
                                      no = 1)))
  
  output = update_equilibrium(initial_conditions = initial_conditions,
                              ntb_new = scenario,
                              verbose = 2,
                              tolerance = 1e-4)
  write_rds(output, path = str_c("output/results/results_EU-CHN_retail_ntb_change_", (1 + ntb_new/100), ".rds"), compress = "gz")
  rm(output, scenario)
}
stopCluster(cl) # end cluster
gc()

