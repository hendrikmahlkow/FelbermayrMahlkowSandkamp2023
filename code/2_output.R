# title: 2. output tables for Felbermayr, Mahlkow, & Sandkamp (2023): Cutting through the value chain: the long-run effects of decoupling the East from the West
# DOI: 10.1007/s10663-022-09561-w
# date: 2022-10-21
# author: Hendrik

# load packages
pacman::p_load(KITE) # version: 22.03 Master
pacman::p_load(data.table)
pacman::p_load(tidyverse)
pacman::p_load(xtable)
pacman::p_load(sf)
pacman::p_load(ggplot2)
pacman::p_load(ggrepel)
pacman::p_load(cowplot)
pacman::p_load(wesanderson)
pacman::p_load(scales)
pacman::p_load(countrycode)
pacman::p_load(kielinstitute)
pacman::p_load(extrafont)

`%notin%` <- Negate(`%in%`)

EU27 = c("DEU", "AUT", "BEL", "DNK", "FIN", "FRA", "GRC", "IRL", "ITA", "LUX", "NLD", "PRT", "ESP", "SWE", "MLT", "CYP", "EST", "LTU", "LVA", "CZE", 
         "HUN", "BGR", "ROU", "POL", "SVK", "SVN", "HRV")
US_allies = c(EU27, "USA", "CAN", "ISL", "NOR", "GBR", "TUR", "ALB", "PHL", "AUS", "NZL", "KOR", "JPN", "TWN")
BRIC = c("BRA", "RUS", "IND", "CHN")

scenarios = data.table(scenario = c("EU-CHN_uni", "CHN-EU_uni", "US_all-CHN_uni", "CHN-US_all_uni", "EU-BRIC_uni", "BRIC-EU_uni", "US_all-BRIC_uni", 
                                    "BRIC-US_all_uni", "US_all-RUS_uni", "RUS-US_all_uni", "EU-CHN_retail", "US_all-CHN_retail", "EU-BRIC_retail",  
                                    "US_all-BRIC_retail", "US_all-RUS_retail"),
                      table_name = c("1A EU", "1B China", "2A US al.", "2B China", "4A EU", "4B BRIC", "5A US al.", "5B BRIC", "3A US al.", "3B Russia", 
                                     "1C Bilateral", "2C Bilateral", "4C Bilateral", "5C Bilateral", "3C Bilateral"))
# load data
data = list()
for (scen in scenarios$scenario) {
  data[[scen]] = read_rds(str_c("output/results/results_", scen, ".rds"))
}
map_world_gtap = read_rds("meta/map_world_gtap.rds")
st_crs(map_world_gtap) = "+proj=longlat +zone=19 +datum=WGS84"


# result tables --------------------------------------------------------------------------------------------------------------------------------------

# scenario 1: EU-CHN  ----
table = data.table()
for (scen in c("EU-CHN_uni", "CHN-EU_uni", "EU-CHN_retail")) {
  data_temp = process_results(data[[scen]])
  table_temp = data.table()
  table_temp[, scenario := scenarios[scenario == scen, table_name]]
  
  # real trade flow new
  trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
        by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
    mutate(value = value.x / value.y) %>% select(-value.x, -value.y)
  
  table_temp[, CHN_export_to_EU_change := (trade_flow_new[origin == "CHN" & destination %in% EU27, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %in% EU27, sum(value)] - 1) * 100]
  
  table_temp[, EU_export_to_CHN_change := (trade_flow_new[origin %in% EU27 & destination == "CHN", sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination == "CHN", sum(value)] - 1) * 100]
  
  table_temp[, CHN_export_to_ROW_change := (trade_flow_new[origin == "CHN" & destination %notin% c(EU27, "CHN"), sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %notin% c(EU27, "CHN"), sum(value)] - 1) * 100]
  
  table_temp[, EU_export_to_ROW_change := (trade_flow_new[origin %in% EU27 & destination %notin% c(EU27, "CHN"), sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination %notin% c(EU27, "CHN"), sum(value)] - 1) * 100]
  
  table_temp[, CHN_import_from_ROW_change := (trade_flow_new[origin %notin% c(EU27, "CHN") & destination == "CHN", sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin %notin% c(EU27, "CHN") & destination == "CHN", sum(value)] - 1) * 100]
  
  table_temp[, EU_import_from_ROW_change := (trade_flow_new[origin %notin% c(EU27, "CHN") & destination %in% EU27, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %notin% c(EU27, "CHN") & destination %in% EU27, sum(value)] - 1) * 100]
  
  table_temp[, CHN_export_change := (trade_flow_new[origin == "CHN" & destination != "CHN", sum(value)] / 
                                       data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination != "CHN", sum(value)] - 1) * 100]
  
  table_temp[, EU_export_change := (trade_flow_new[origin %in% EU27 & destination %notin% EU27, sum(value)] / 
                                      data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination %notin% EU27, sum(value)] - 1) * 100]
  
  table_temp[, CHN_welfare := data_temp$equilibrium_new$welfare_change[country == "CHN", value]]
  
  # welfare change EU
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% EU27, value_added_share := value_added / sum(value_added)]
  table_temp[, EU_welfare_change := welfare_change[country %in% EU27, sum(welfare_change * value_added_share)]]
  
  table = rbind(table, table_temp)
  rm(data_temp, welfare_change)
}
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Decoupling & \\multicolumn{2}{c}{$\\Delta$ Bilateral} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Welfare}\\\\\n",
                           "scenario & \\multicolumn{2}{c}{exports} & \\multicolumn{2}{c}{to RoW} & \\multicolumn{2}{c}{from RoW} & \\multicolumn{2}{c}{exports} & }\\\\\\cmidrule{2-11}\n",
                           "& China & EU & China & EU & China & EU & China & EU & China & EU} \\\\\n",
                           "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10)} \\\\\n" )
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("output/tables/EU-CHN_decoupling_results.tex"))


# scenario 2: US_all-CHN  ----
table = data.table()
for (scen in c("US_all-CHN_uni", "CHN-US_all_uni", "US_all-CHN_retail")) {
  data_temp = process_results(data[[scen]])
  table_temp = data.table()
  table_temp[, scenario := scenarios[scenario == scen, table_name]]
  
  # real trade flow new
  trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                         by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
    mutate(value = value.x / value.y) %>% select(-value.x, -value.y)
  
  table_temp[, CHN_export_to_US_all_change := (trade_flow_new[origin == "CHN" & destination %in% US_allies, sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %in% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, US_all_export_to_CHN_change := (trade_flow_new[origin %in% US_allies & destination == "CHN", sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination == "CHN", sum(value)] - 1) * 100]
  
  table_temp[, CHN_export_to_ROW_change := (trade_flow_new[origin == "CHN" & destination %notin% c(US_allies, "CHN"), sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %notin% c(US_allies, "CHN"), sum(value)] - 1) * 100]
  
  table_temp[, US_all_export_to_ROW_change := (trade_flow_new[origin %in% US_allies & destination %notin% c(US_allies, "CHN"), sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% c(US_allies, "CHN"), sum(value)] - 1) * 100]
  
  table_temp[, CHN_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, "CHN") & destination == "CHN", sum(value)] / 
                                                data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "CHN") & destination == "CHN", sum(value)] - 1) * 100]
  
  table_temp[, US_all_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, "CHN") & destination %in% US_allies, sum(value)] / 
                                                   data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "CHN") & destination %in% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, CHN_export_change := (trade_flow_new[origin == "CHN" & destination != "CHN", sum(value)] / 
                                       data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination != "CHN", sum(value)] - 1) * 100]
  
  table_temp[, US_allies_export_change := (trade_flow_new[origin %in% US_allies & destination %notin% US_allies, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, CHN_welfare := data_temp$equilibrium_new$welfare_change[country == "CHN", value]]
  # welfare change US_all
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% US_allies, value_added_share := value_added / sum(value_added)]
  table_temp[, US_allies_welfare_change := welfare_change[country %in% US_allies, sum(welfare_change * value_added_share)]]
  
  table = rbind(table, table_temp)
}
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Decoupling & \\multicolumn{2}{c}{$\\Delta$ Bilateral} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Welfare}\\\\\n",
                           "scenario & \\multicolumn{2}{c}{exports} & \\multicolumn{2}{c}{to RoW} & \\multicolumn{2}{c}{from RoW} & \\multicolumn{2}{c}{exports} & }\\\\\\cmidrule{2-11}\n",
                           "& China & US al. & China & US al. & China & US al. & China & US al. & China & US al.} \\\\\n",
                           "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10)} \\\\\n")
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("output/tables/US_allies-CHN_decoupling_results.tex"))
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/US_allies-CHN_decoupling_results.tex"))


# scenario 3: US_all-RUS  ----
table = data.table()
for (scen in c("US_all-RUS_uni", "RUS-US_all_uni", "US_all-RUS_retail")) {
  data_temp = process_results(data[[scen]])
  table_temp = data.table()
  table_temp[, scenario := scenarios[scenario == scen, table_name]]
  
  # real trade flow new
  trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                         by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
    mutate(value = value.x / value.y) %>% select(-value.x, -value.y)
  
  table_temp[, RUS_export_to_US_all_change := (trade_flow_new[origin == "RUS" & destination %in% US_allies, sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination %in% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, US_all_export_to_RUS_change := (trade_flow_new[origin %in% US_allies & destination == "RUS", sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination == "RUS", sum(value)] - 1) * 100]
  
  table_temp[, RUS_export_to_ROW_change := (trade_flow_new[origin == "RUS" & destination %notin% c(US_allies, "RUS"), sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination %notin% c(US_allies, "RUS"), sum(value)] - 1) * 100]
  
  table_temp[, US_all_export_to_ROW_change := (trade_flow_new[origin %in% US_allies & destination %notin% c(US_allies, "RUS"), sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% c(US_allies, "RUS"), sum(value)] - 1) * 100]
  
  table_temp[, RUS_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, "RUS") & destination == "RUS", sum(value)] / 
                                                data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS") & destination == "RUS", sum(value)] - 1) * 100]
  
  table_temp[, US_all_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, "RUS") & destination %in% US_allies, sum(value)] / 
                                                   data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS") & destination %in% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, RUS_export_change := (trade_flow_new[origin == "RUS" & destination != "RUS", sum(value)] / 
                                       data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination != "RUS", sum(value)] - 1) * 100]
  
  table_temp[, US_allies_export_change := (trade_flow_new[origin %in% US_allies & destination %notin% US_allies, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, RUS_welfare := data_temp$equilibrium_new$welfare_change[country == "RUS", value]]
  # welfare change US_all
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% US_allies, value_added_share := value_added / sum(value_added)]
  table_temp[, US_allies_welfare_change := welfare_change[country %in% US_allies, sum(welfare_change * value_added_share)]]
  
  table = rbind(table, table_temp)
}
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Decoupling & \\multicolumn{2}{c}{$\\Delta$ Bilateral} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Welfare}\\\\\n",
                           "scenario & \\multicolumn{2}{c}{exports} & \\multicolumn{2}{c}{to RoW} & \\multicolumn{2}{c}{from RoW} & \\multicolumn{2}{c}{exports} & }\\\\\\cmidrule{2-11}\n",
                           "& Russia & US al. & Russia & US al. & Russia & US al. & Russia & US al. & Russia & US al.} \\\\\n",
                           "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10)} \\\\\n")
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("output/tables/US_allies-RUS_decoupling_results.tex"))
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/US_allies-RUS_decoupling_results.tex"))



# scenario 4: EU-BRIC  ----
table = data.table()
for (scen in c("EU-BRIC_uni", "BRIC-EU_uni", "EU-BRIC_retail")) {
  data_temp = process_results(data[[scen]])
  table_temp = data.table()
  table_temp[, scenario := scenarios[scenario == scen, table_name]]
  
  # real trade flow new
  trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                         by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
    mutate(value = value.x / value.y) %>% select(-value.x, -value.y)
  
  table_temp[, BRIC_export_to_EU_change := (trade_flow_new[origin %in% BRIC & destination %in% EU27, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %in% EU27, sum(value)] - 1) * 100]
  
  table_temp[, EU_export_to_BRIC_change := (trade_flow_new[origin %in% EU27 & destination %in% BRIC, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination %in% BRIC, sum(value)] - 1) * 100]
  
  table_temp[, BRIC_export_to_ROW_change := (trade_flow_new[origin %in% BRIC & destination %notin% c(EU27, BRIC), sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %notin% c(EU27, BRIC), sum(value)] - 1) * 100]
  
  table_temp[, EU_export_to_ROW_change := (trade_flow_new[origin %in% EU27 & destination %notin% c(BRIC, EU27), sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination %notin% c(BRIC, EU27), sum(value)] - 1) * 100]
  
  table_temp[, BRIC_import_from_ROW_change := (trade_flow_new[origin %notin% c(EU27, BRIC) & destination %in% BRIC, sum(value)] / 
                                               data_temp$equilibrium_new$trade_flow[origin %notin% c(EU27, BRIC) & destination %in% BRIC, sum(value)] - 1) * 100]
  
  table_temp[, EU_import_from_ROW_change := (trade_flow_new[origin %notin% c(BRIC, EU27) & destination %in% EU27, sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %notin% c(BRIC, EU27) & destination %in% EU27, sum(value)] - 1) * 100]
  
  table_temp[, BRIC_export_change := (trade_flow_new[origin %in% BRIC & destination %notin% BRIC, sum(value)] / 
                                       data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %notin% BRIC, sum(value)] - 1) * 100]
  
  table_temp[, EU_export_change := (trade_flow_new[origin %in% EU27 & destination %notin% EU27, sum(value)] / 
                                      data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination %notin% EU27, sum(value)] - 1) * 100]
  
  # welfare change BRIC
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% BRIC, value_added_share := value_added / sum(value_added)]
  table_temp[, BRIC_welfare_change := welfare_change[country %in% BRIC, sum(welfare_change * value_added_share)]]
  
  # welfare change EU
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% EU27, value_added_share := value_added / sum(value_added)]
  table_temp[, EU_welfare_change := welfare_change[country %in% EU27, sum(welfare_change * value_added_share)]]
  
  table = rbind(table, table_temp)
}
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Decoupling & \\multicolumn{2}{c}{$\\Delta$ Bilateral} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Welfare}\\\\\n",
                           "scenario & \\multicolumn{2}{c}{exports} & \\multicolumn{2}{c}{to RoW} & \\multicolumn{2}{c}{from RoW} & \\multicolumn{2}{c}{exports} & }\\\\\\cmidrule{2-11}\n",
                           "& BRIC & EU & BRIC & EU & BRIC & EU & BRIC & EU & BRIC & EU} \\\\\n", 
                           "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10)} \\\\\n" )
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("output/tables/EU-BRIC_decoupling_results.tex"))
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/EU-BRIC_decoupling_results.tex"))

# scenario 5: US_all-BRIC  ----
table = data.table()
for (scen in c("US_all-BRIC_uni", "BRIC-US_all_uni", "US_all-BRIC_retail")) {
  data_temp = process_results(data[[scen]])
  table_temp = data.table()
  table_temp[, scenario := scenarios[scenario == scen, table_name]]
  
  # real trade flow new
  trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                         by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
    mutate(value = value.x / value.y) %>% select(-value.x, -value.y)
  
  table_temp[, BRIC_export_to_US_allies_change := (trade_flow_new[origin %in% BRIC & destination %in% US_allies, sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %in% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, US_allies_export_to_BRIC_change := (trade_flow_new[origin %in% US_allies & destination %in% BRIC, sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %in% BRIC, sum(value)] - 1) * 100]
  
  table_temp[, BRIC_export_to_ROW_change := (trade_flow_new[origin %in% BRIC & destination %notin% c(US_allies, BRIC), sum(value)] / 
                                               data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %notin% c(US_allies, BRIC), sum(value)] - 1) * 100]
  
  table_temp[, US_allies_export_to_ROW_change := (trade_flow_new[origin %in% US_allies & destination %notin% c(BRIC, US_allies), sum(value)] / 
                                             data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% c(BRIC, US_allies), sum(value)] - 1) * 100]
  
  table_temp[, BRIC_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, BRIC) & destination %in% BRIC, sum(value)] / 
                                               data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, BRIC) & destination %in% BRIC, sum(value)] - 1) * 100]
  
  table_temp[, US_allies_import_from_ROW_change := (trade_flow_new[origin %notin% c(BRIC, US_allies) & destination %in% US_allies, sum(value)] / 
                                                    data_temp$equilibrium_new$trade_flow[origin %notin% c(BRIC, US_allies) & destination %in% US_allies, sum(value)] - 1) * 100]
  
  table_temp[, BRIC_export_change := (trade_flow_new[origin %in% BRIC & destination %notin% BRIC, sum(value)] / 
                                        data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %notin% BRIC, sum(value)] - 1) * 100]
  
  table_temp[, US_allies_export_change := (trade_flow_new[origin %in% US_allies & destination %notin% US_allies, sum(value)] / 
                                      data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% US_allies, sum(value)] - 1) * 100]
  
  # welfare change BRIC
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% BRIC, value_added_share := value_added / sum(value_added)]
  table_temp[, BRIC_welfare_change := welfare_change[country %in% BRIC, sum(welfare_change * value_added_share)]]
  
  # welfare change US_all
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% US_allies, value_added_share := value_added / sum(value_added)]
  table_temp[, US_allies_welfare_change := welfare_change[country %in% US_allies, sum(welfare_change * value_added_share)]]
  
  table = rbind(table, table_temp)
}
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Decoupling & \\multicolumn{2}{c}{$\\Delta$ Bilateral} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Welfare}\\\\\n",
                           "scenario & \\multicolumn{2}{c}{exports} & \\multicolumn{2}{c}{to RoW} & \\multicolumn{2}{c}{from RoW} & \\multicolumn{2}{c}{exports} & }\\\\\\cmidrule{2-11}\n",
                           "& BRIC & US al. & BRIC & US al. & BRIC & US al. & BRIC & US al. & BRIC & US al.} \\\\\n",
                           "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10)} \\\\\n")
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("output/tables/US_allies-BRIC_decoupling_results.tex"))
print.xtable(xtable(table, digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/US_allies-BRIC_decoupling_results.tex"))


# extension scenario ----
data_energy = read_rds(str_c("./policy_brief/results/results_US_all-RUS_energy.rds"))

data_temp = process_results(data_energy)
table_temp = data.table()
table_temp[, scenario := "6 Energy Sectors"]

# real trade flow new
trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                       by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
  mutate(value = value.x / value.y) %>% select(-value.x, -value.y)

table_temp[, RUS_export_to_US_all_change := (trade_flow_new[origin == "RUS" & destination %in% US_allies, sum(value)] / 
                                               data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination %in% US_allies, sum(value)] - 1) * 100]

table_temp[, US_all_export_to_RUS_change := (trade_flow_new[origin %in% US_allies & destination == "RUS", sum(value)] / 
                                               data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination == "RUS", sum(value)] - 1) * 100]

table_temp[, RUS_export_to_ROW_change := (trade_flow_new[origin == "RUS" & destination %notin% c(US_allies, "RUS"), sum(value)] / 
                                            data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination %notin% c(US_allies, "RUS"), sum(value)] - 1) * 100]

table_temp[, US_all_export_to_ROW_change := (trade_flow_new[origin %in% US_allies & destination %notin% c(US_allies, "RUS"), sum(value)] / 
                                               data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% c(US_allies, "RUS"), sum(value)] - 1) * 100]

table_temp[, RUS_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, "RUS") & destination == "RUS", sum(value)] / 
                                              data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS") & destination == "RUS", sum(value)] - 1) * 100]

table_temp[, US_all_import_from_ROW_change := (trade_flow_new[origin %notin% c(US_allies, "RUS") & destination %in% US_allies, sum(value)] / 
                                                 data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS") & destination %in% US_allies, sum(value)] - 1) * 100]

table_temp[, RUS_export_change := (trade_flow_new[origin == "RUS" & destination != "RUS", sum(value)] / 
                                     data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination != "RUS", sum(value)] - 1) * 100]

table_temp[, US_allies_export_change := (trade_flow_new[origin %in% US_allies & destination %notin% US_allies, sum(value)] / 
                                           data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% US_allies, sum(value)] - 1) * 100]

table_temp[, RUS_welfare := data_temp$equilibrium_new$welfare_change[country == "RUS", value]]
# welfare change US_all
welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
  setnames(c("country", "welfare_change", "value_added"))
welfare_change[country %in% US_allies, value_added_share := value_added / sum(value_added)]
table_temp[, US_allies_welfare_change := welfare_change[country %in% US_allies, sum(welfare_change * value_added_share)]]

add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Decoupling & \\multicolumn{2}{c}{$\\Delta$ Bilateral} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Welfare}\\\\\n",
                           "scenario & \\multicolumn{2}{c}{exports} & \\multicolumn{2}{c}{to RoW} & \\multicolumn{2}{c}{from RoW} & \\multicolumn{2}{c}{exports} & }\\\\\\cmidrule{2-11}\n",
                           "& Russia & US al. & Russia & US al. & Russia & US al. & Russia & US al. & Russia & US al.} \\\\\n",
                           "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10)} \\\\\n")
print.xtable(xtable(as.data.frame(table_temp), digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("output/tables/US_allies-RUS_energy_decoupling_results.tex"))
print.xtable(xtable(as.data.frame(table_temp), digits = 2), include.rownames = FALSE, include.colnames = FALSE, add.to.row = add.to.row, floating = FALSE, 
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/US_allies-RUS_energy_decoupling_results.tex"))


# appendix 1C ----
scen = "EU-CHN_retail"
data_temp = process_results(data[[scen]])
table = copy(data_temp$equilibrium_new$welfare_change[country %in% c(EU27, "CHN")]) %>% setnames("value", "welfare_change")

# real trade flow new
trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                       by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
  mutate(value = value.x / value.y) %>% select(-value.x, -value.y)

# total exports change
table = merge(table, 
              trade_flow_new[origin %in% EU27 & destination != origin, 
                             .(exports_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country == "CHN", exports_new_fob := trade_flow_new[origin == "CHN" & destination != "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination != origin, 
                                                   .(exports = sum(value)),
                                                   by = .(country = origin)],
              all.x = T)
table[country == "CHN", exports := data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination != "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change = (exports_new_fob / exports - 1) * 100)]

# total imports change
table = merge(table, 
              trade_flow_new[origin != destination & destination %in% EU27, 
                             .(imports_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country == "CHN", imports_new_fob := trade_flow_new[origin != "CHN" & destination == "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin != destination & destination %in% EU27, 
                                                   .(imports = sum(value)),
                                                   by = .(country = destination)],
              all.x = T)
table[country == "CHN", imports := data_temp$equilibrium_new$trade_flow[origin != "CHN" & destination == "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change = (imports_new_fob / imports - 1) * 100)]

# exorts to China
table = merge(table, 
              trade_flow_new[origin %in% EU27 & destination == "CHN", 
                             .(exports_to_CHN_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination == "CHN",
                                                   .(exports_to_CHN = sum(value)), 
                                                   by = .(country = origin)],
              all.x = T)
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_CHN_change = (exports_to_CHN_new_fob / exports_to_CHN - 1) * 100)]

# imports from China
table = merge(table, 
              trade_flow_new[origin == "CHN" & destination %in% EU27, 
                             .(imports_from_CHN_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %in% EU27 ,
                                                   .(imports_from_CHN = sum(value)), 
                                                   by = .(country = destination)],
              all.x = T)
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_CHN_change, imports_from_CHN_change = (imports_from_CHN_new_fob / imports_from_CHN - 1) * 100)]

# exorts to RoW
table = merge(table, 
              trade_flow_new[origin %in% EU27 & destination %notin% c(EU27, "CHN"), 
                             .(exports_to_ROW_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country == "CHN", exports_to_ROW_new_fob := trade_flow_new[origin == "CHN" & destination %notin% c(EU27, "CHN"), sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination %notin% c(EU27, "CHN"),
                                                   .(exports_to_ROW = sum(value)), 
                                                   by = .(country = origin)],
              all.x = T)
table[country == "CHN", exports_to_ROW := data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %notin% c(EU27, "CHN"), sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_CHN_change, imports_from_CHN_change, exports_to_ROW_change = (exports_to_ROW_new_fob / exports_to_ROW - 1) * 100)]

# imports to RoW
table = merge(table, 
              trade_flow_new[origin %notin% c(EU27, "CHN") & destination %in% EU27, 
                             .(imports_from_ROW_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country == "CHN", imports_from_ROW_new_fob := trade_flow_new[origin %notin% c(EU27, "CHN") & destination == "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %notin% c(EU27, "CHN") & destination %in% EU27,
                                                   .(imports_from_ROW = sum(value)), 
                                                   by = .(country = destination)],
              all.x = T)
table[country == "CHN", imports_from_ROW := data_temp$equilibrium_new$trade_flow[origin %notin% c(EU27, "CHN") & destination == "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_CHN_change, exports_to_ROW_change, imports_from_CHN_change,
                  imports_from_ROW_change = (imports_from_ROW_new_fob / imports_from_ROW - 1) * 100)]
table[,country := countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")]

# export table
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Country & $\\Delta$ Welfare & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} \\\\\n",
                           "&&Exports&Imports&China&RoW&China&RoW}\\\\\\cmidrule{2-8}\n",
                           "&(1)&(2)&(3)&(4)&(5)&(6)&(7)} \\\\\n",
                           "\\endhead\n",
                           "\\hline\n",
                           "{\\footnotesize Continued on next page}\n",
                           "\\endfoot\n",
                           "\\endlastfoot\n")
table_caption = "\\label{tab:scen-1c}Scenario 1C, changes in percent."
print.xtable(xtable(table, digits = 2, caption = table_caption), 
             include.rownames = FALSE, 
             include.colnames = FALSE,
             caption.placement = "top",
             add.to.row = add.to.row, 
             floating = FALSE, 
             tabular.environment = "longtable",
             latex.environments = "center", file = str_c("output/tables/appendix_", scen,".tex"))
print.xtable(xtable(table, digits = 2, caption = table_caption), 
             include.rownames = FALSE, 
             include.colnames = FALSE, 
             caption.placement = "top",
             add.to.row = add.to.row, 
             floating = FALSE, 
             tabular.environment = "longtable",
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/appendix_", scen,".tex"))


# appendix 3C ----
scen = "US_all-RUS_retail"
data_temp = process_results(data[[scen]])
table = copy(data_temp$equilibrium_new$welfare_change[country %in% c(US_allies, "RUS", "CHN")]) %>% setnames("value", "welfare_change")

# real trade flow new
trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                       by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
  mutate(value = value.x / value.y) %>% select(-value.x, -value.y)

# total exports change
table = merge(table, 
              trade_flow_new[origin %in% US_allies & destination != origin, 
                             .(exports_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country == "RUS", exports_new_fob := trade_flow_new[origin == "RUS" & destination != "RUS", sum(value)]]
table[country == "CHN", exports_new_fob := trade_flow_new[origin == "CHN" & destination != "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination != origin, 
                                                   .(exports = sum(value)),
                                                   by = .(country = origin)],
              all.x = T)
table[country == "RUS", exports := data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination != "RUS", sum(value)]]
table[country == "CHN", exports := data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination != "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change = (exports_new_fob / exports - 1) * 100)]

# total imports change
table = merge(table, 
              trade_flow_new[origin != destination & destination %in% US_allies, 
                             .(imports_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country == "RUS", imports_new_fob := trade_flow_new[origin != "RUS" & destination == "RUS", sum(value)]]
table[country == "CHN", imports_new_fob := trade_flow_new[origin != "CHN" & destination == "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin != destination & destination %in% US_allies, 
                                                   .(imports = sum(value)),
                                                   by = .(country = destination)],
              all.x = T)
table[country == "RUS", imports := data_temp$equilibrium_new$trade_flow[origin != "RUS" & destination == "RUS", sum(value)]]
table[country == "CHN", imports := data_temp$equilibrium_new$trade_flow[origin != "CHN" & destination == "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change = (imports_new_fob / imports - 1) * 100)]

# exorts to Russia
table = merge(table, 
              trade_flow_new[origin %in% US_allies & destination == "RUS", 
                             .(exports_to_RUS_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country == "CHN", exports_to_RUS_new_fob := trade_flow_new[origin == "CHN" & destination == "RUS", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination == "RUS",
                                                   .(exports_to_RUS = sum(value)), 
                                                   by = .(country = origin)],
              all.x = T)
table[country == "CHN", exports_to_RUS := data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination == "RUS", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_RUS_change = (exports_to_RUS_new_fob / exports_to_RUS - 1) * 100)]

# imports from Russia
table = merge(table, 
              trade_flow_new[origin == "RUS" & destination %in% US_allies, 
                             .(imports_from_RUS_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country == "CHN", imports_from_RUS_new_fob := trade_flow_new[origin == "RUS" & destination == "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination %in% US_allies ,
                                                   .(imports_from_RUS = sum(value)), 
                                                   by = .(country = destination)],
              all.x = T)
table[country == "CHN", imports_from_RUS := data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination == "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_RUS_change, imports_from_RUS_change = (imports_from_RUS_new_fob / imports_from_RUS - 1) * 100)]

# exorts to RoW
table = merge(table, 
              trade_flow_new[origin %in% US_allies & destination %notin% c(US_allies, "RUS"), 
                             .(exports_to_ROW_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country == "RUS", exports_to_ROW_new_fob := trade_flow_new[origin == "RUS" & destination %notin% c(US_allies, "RUS"), sum(value)]]
table[country == "CHN", exports_to_ROW_new_fob := trade_flow_new[origin == "CHN" & destination %notin% c(US_allies, "RUS", "CHN"), sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% c(US_allies, "RUS"),
                                                   .(exports_to_ROW = sum(value)), 
                                                   by = .(country = origin)],
              all.x = T)
table[country == "RUS", exports_to_ROW := data_temp$equilibrium_new$trade_flow[origin == "RUS" & destination %notin% c(US_allies, "RUS"), sum(value)]]
table[country == "CHN", exports_to_ROW := data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %notin% c(US_allies, "RUS", "CHN"), sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_RUS_change, imports_from_RUS_change, exports_to_ROW_change = (exports_to_ROW_new_fob / exports_to_ROW - 1) * 100)]

# imports to RoW
table = merge(table, 
              trade_flow_new[origin %notin% c(US_allies, "RUS") & destination %in% US_allies, 
                             .(imports_from_ROW_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country == "RUS", imports_from_ROW_new_fob := trade_flow_new[origin %notin% c(US_allies, "RUS") & destination == "RUS", sum(value)]]
table[country == "CHN", imports_from_ROW_new_fob := trade_flow_new[origin %notin% c(US_allies, "RUS", "CHN") & destination == "CHN", sum(value)]]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS") & destination %in% US_allies,
                                                   .(imports_from_ROW = sum(value)), 
                                                   by = .(country = destination)],
              all.x = T)
table[country == "RUS", imports_from_ROW := data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS") & destination == "RUS", sum(value)]]
table[country == "CHN", imports_from_ROW := data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, "RUS", "CHN") & destination == "CHN", sum(value)]]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_RUS_change, exports_to_ROW_change, imports_from_RUS_change,
                  imports_from_ROW_change = (imports_from_ROW_new_fob / imports_from_ROW - 1) * 100)]
table[,country := countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")]

# export table
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Country & $\\Delta$ Welfare & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} \\\\\n",
                           "&&Exports&Imports&Russia&RoW&Russia&RoW}\\\\\\cmidrule{2-8}\n",
                           "&(1)&(2)&(3)&(4)&(5)&(6)&(7)} \\\\\n",
                           "\\endhead\n",
                           "\\hline\n",
                           "{\\footnotesize Continued on next page}\n",
                           "\\endfoot\n",
                           "\\endlastfoot\n")
table_caption = "\\label{tab:scen-3c}Scenario 3C, changes in percent."
print.xtable(xtable(table, digits = 2, caption = table_caption), 
             include.rownames = FALSE, 
             include.colnames = FALSE,
             caption.placement = "top",
             add.to.row = add.to.row, 
             floating = FALSE, 
             tabular.environment = "longtable",
             latex.environments = "center", file = str_c("output/tables/appendix_", scen,".tex"))
print.xtable(xtable(table, digits = 2, caption = table_caption), 
             include.rownames = FALSE, 
             include.colnames = FALSE, 
             caption.placement = "top",
             add.to.row = add.to.row, 
             floating = FALSE, 
             tabular.environment = "longtable",
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/appendix_", scen,".tex"))


# appendix 5C ----
scen = "US_all-BRIC_retail"
data_temp = process_results(data[[scen]])
table = copy(data_temp$equilibrium_new$welfare_change[country %in% c(US_allies, BRIC)]) %>% setnames("value", "welfare_change")

# real trade flow new
trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                       by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
  mutate(value = value.x / value.y) %>% select(-value.x, -value.y)

# total exports change
table = merge(table, 
              trade_flow_new[origin %in% US_allies & destination != origin, 
                             .(exports_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country %in% BRIC, exports_new_fob := trade_flow_new[origin %in% BRIC & destination != origin, sum(value), by = origin]$V1]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination != origin, 
                                                   .(exports = sum(value)),
                                                   by = .(country = origin)],
              all.x = T)
table[country %in% BRIC, exports := data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination != origin, sum(value), by = origin]$V1]
table = table[, .(country, welfare_change, total_exports_change = (exports_new_fob / exports - 1) * 100)]

# total imports change
table = merge(table, 
              trade_flow_new[origin != destination & destination %in% US_allies, 
                             .(imports_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country %in% BRIC, imports_new_fob := trade_flow_new[origin != destination & destination %in% BRIC, sum(value), by = destination]$V1]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin != destination & destination %in% US_allies, 
                                                   .(imports = sum(value)),
                                                   by = .(country = destination)],
              all.x = T)
table[country %in% BRIC, imports := data_temp$equilibrium_new$trade_flow[origin != destination & destination %in% BRIC, sum(value), by = destination]$V1]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change = (imports_new_fob / imports - 1) * 100)]

# exports to BRIC
table = merge(table, 
              trade_flow_new[origin %in% US_allies & destination %in% BRIC, 
                             .(exports_to_BRIC_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %in% BRIC,
                                                   .(exports_to_BRIC = sum(value)), 
                                                   by = .(country = origin)],
              all.x = T)
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_BRIC_change = (exports_to_BRIC_new_fob / exports_to_BRIC - 1) * 100)]

# imports from BRIC
table = merge(table, 
              trade_flow_new[origin %in% BRIC & destination %in% US_allies, 
                             .(imports_from_BRIC_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %in% US_allies ,
                                                   .(imports_from_BRIC = sum(value)), 
                                                   by = .(country = destination)],
              all.x = T)
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_BRIC_change, imports_from_BRIC_change = (imports_from_BRIC_new_fob / imports_from_BRIC - 1) * 100)]

# exorts to RoW
table = merge(table, 
              trade_flow_new[origin %in% US_allies & destination %notin% c(US_allies, BRIC), 
                             .(exports_to_ROW_new_fob = sum(value)), 
                             by = .(country = origin)],
              all.x = T)
table[country %in% BRIC, exports_to_ROW_new_fob := trade_flow_new[origin %in% BRIC & destination %notin% c(US_allies, BRIC), sum(value), by = origin]$V1]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %in% US_allies & destination %notin% c(US_allies, BRIC),
                                                   .(exports_to_ROW = sum(value)), 
                                                   by = .(country = origin)],
              all.x = T)
table[country %in% BRIC, exports_to_ROW := data_temp$equilibrium_new$trade_flow[origin %in% BRIC & destination %notin% c(US_allies, BRIC), sum(value), by = origin]$V1]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_BRIC_change, imports_from_BRIC_change, exports_to_ROW_change = (exports_to_ROW_new_fob / exports_to_ROW - 1) * 100)]

# imports to RoW
table = merge(table, 
              trade_flow_new[origin %notin% c(US_allies, BRIC) & destination %in% US_allies, 
                             .(imports_from_ROW_new_fob = sum(value)), 
                             by = .(country = destination)],
              all.x = T)
table[country %in% BRIC, imports_from_ROW_new_fob := trade_flow_new[origin %notin% c(BRIC, US_allies) & destination %in% BRIC, sum(value), by = destination]$V1]
table = merge(table, 
              data_temp$equilibrium_new$trade_flow[origin %notin% c(US_allies, BRIC) & destination %in% US_allies,
                                                   .(imports_from_ROW = sum(value)), 
                                                   by = .(country = destination)],
              all.x = T)
table[country %in% BRIC, imports_from_ROW := data_temp$equilibrium_new$trade_flow[origin %notin% c(BRIC, US_allies) & destination %in% BRIC, sum(value), by = destination]$V1]
table = table[, .(country, welfare_change, total_exports_change, total_imports_change, 
                  exports_to_BRIC_change, exports_to_ROW_change, imports_from_BRIC_change,
                  imports_from_ROW_change = (imports_from_ROW_new_fob / imports_from_ROW - 1) * 100)]
table[,country := countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")]

# export table
add.to.row = list(pos = list(0), command = NULL)
add.to.row$command = str_c("Country & $\\Delta$ Welfare & \\multicolumn{2}{c}{$\\Delta$ Total} & \\multicolumn{2}{c}{$\\Delta$ Exports} & \\multicolumn{2}{c}{$\\Delta$ Imports} \\\\\n",
                           "&&Exports&Imports&BRIC&RoW&BRIC&RoW}\\\\\\cmidrule{2-8}\n",
                           "&(1)&(2)&(3)&(4)&(5)&(6)&(7)} \\\\\n",
                           "\\endhead\n",
                           "\\hline\n",
                           "{\\footnotesize Continued on next page}\n",
                           "\\endfoot\n",
                           "\\endlastfoot\n")
table_caption = "\\label{tab:scen-5c}Scenario 5C, changes in percent."
print.xtable(xtable(table, digits = 2, caption = table_caption), 
             include.rownames = FALSE, 
             include.colnames = FALSE,
             caption.placement = "top",
             add.to.row = add.to.row, 
             floating = FALSE, 
             tabular.environment = "longtable",
             latex.environments = "center", file = str_c("output/tables/appendix_", scen,".tex"))
print.xtable(xtable(table, digits = 2, caption = table_caption), 
             include.rownames = FALSE, 
             include.colnames = FALSE, 
             caption.placement = "top",
             add.to.row = add.to.row, 
             floating = FALSE, 
             tabular.environment = "longtable",
             latex.environments = "center", file = str_c("../Cutting-through-the-Value-Chain__Overleaf/tables/tex/appendix_", scen,".tex"))


# country / sector list ----
countrylist = countrycode(data[[1]]$initial_conditions$value_added$country, 
                          origin = "iso3c", destination = "country.name", nomatch = NULL)
write_lines(countrylist, "output/tables/GTAP-10---countrylist.txt", sep = "; ")

sectorlist = fread("meta/sectorlist_short.csv")
write_lines(str_sort(sectorlist$description), "output/tables/GTAP-10---sectorlist.txt", sep = "; ")


# p - descriptive: trade flows in GDP --------------------------------------------------------------------------------------------------------------------------------------------
data_trade_flow = read_rds(str_c("output/results/results_EU-CHN_uni.rds")) %>% process_results()
data_plot = data_trade_flow$equilibrium_new$trade_flow[, .(trade_flow = sum(value)), by = .(origin, destination)]

# EU values
exports_EU = data_plot[origin %in% EU27 & destination %notin% EU27, .(origin = "EU", trade_flow = sum(trade_flow)), by = .(destination)] %>% relocate(origin)
imports_EU = data_plot[destination %in% EU27 & origin %notin% EU27, .(destination = "EU", trade_flow = sum(trade_flow)), by = .(origin)] 
welfare_EU = data_trade_flow$initial_conditions$value_added[country %in% EU27, sum(value)]

data_plot = data_plot[origin %notin% EU27 & destination %notin% EU27]
data_plot = rbind(data_plot, exports_EU) %>% rbind(., imports_EU)

value_added = data_trade_flow$initial_conditions$value_added[country %notin% EU27]
value_added = rbind(value_added, value_added[, .(country = "EU", value = welfare_EU)])

data_plot = merge(data_plot,
                  value_added,
                  by.x = "destination",
                  by.y = "country") %>% setnames("value", "value_added")

data_plot[, trade_flow_share := (trade_flow/value_added)*100]
data_plot = data_plot[origin %in% c(setdiff(US_allies, EU27), BRIC, "EU") & destination %in% c(setdiff(US_allies, EU27), BRIC, "EU")]
data_plot[origin == destination, trade_flow_share := NA]
data_plot[, origin := countrycode(origin, origin = "iso3c", destination = "country.name", nomatch = NULL)]
data_plot[, destination := countrycode(destination, origin = "iso3c", destination = "country.name", nomatch = NULL)]

data_plot = rbind(data_plot, data_plot[,.(origin = "EU", destination = "EU", trade_flow = NA, value_added = NA, trade_flow_share = NA)])

# heatmap
margin_spacer <- function(x) {
  # where x is the column in your dataset
  left_length <- nchar(levels(factor(x)))[1]
  if (left_length > 8) {
    return((left_length - 8) * 4)
  }
  else
    return(0)
}
extrafont::font_import()
ggplot(data_plot, aes(origin, destination)) +
  geom_tile(aes(fill = trade_flow_share)) + 
  geom_text(aes(label = round(trade_flow_share, 1))) +
  labs(x = "Exporter", y = "Importer") +
  scale_fill_gradient2(name = "in %",
                       # low = "#bc5090",
                       # mid = "#ff6361",
                       # high = "#ffa600",
                       low = "#58508d",
                       mid = "#ff6361",
                       high = "#ffa600",
                       midpoint = 15,
                       na.value = "white") +
  # scale_fill_gradient(low = "cornsilk1", high = "#ff6361") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 0.99),
        axis.text = element_text(size = 12),
        legend.title = element_text(vjust = 0.85),
        legend.position = "right",
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(l = 0 + margin_spacer(data_plot$origin)))

ggsave(str_c("output/figures/plot_bilateral_import_gdp_share.pdf"), width = 35, height = 20, units = "cm", dpi = 300)
ggsave(str_c("output/figures/plot_bilateral_import_gdp_share.png"), width = 25, height = 15, units = "cm", dpi = 300)

# US allies value
data_plot = data_trade_flow$equilibrium_new$trade_flow[, .(trade_flow = sum(value)), by = .(origin, destination)]
exports_US_allies = data_plot[origin %in% US_allies & destination %notin% US_allies, .(origin = "US allies", trade_flow = sum(trade_flow)), by = .(destination)] %>% relocate(origin)
imports_US_allies = data_plot[destination %in% US_allies & origin %notin% US_allies, .(destination = "US allies", trade_flow = sum(trade_flow)), by = .(origin)] 
welfare_US_allies = data_trade_flow$initial_conditions$value_added[country %in% US_allies, sum(value)]
data_plot = data_plot[origin %notin% US_allies & destination %notin% US_allies]
data_plot = rbind(data_plot, exports_US_allies) %>% rbind(., imports_US_allies)
value_added = data_trade_flow$initial_conditions$value_added[country %notin% US_allies]
value_added = rbind(value_added, value_added[, .(country = "US allies", value = welfare_EU)])
data_plot = merge(data_plot,
                  value_added,
                  by.x = "destination",
                  by.y = "country") %>% setnames("value", "value_added")
data_plot[, trade_flow_share := (trade_flow/value_added)*100]
data_plot[origin == "RUS" & destination == "US allies"]


# p - map welfare change ----------------------------------------------------------------------------------------------------------------------------------------
for (scen in c("EU-CHN_retail", "US_all-BRIC_retail", "US_all-RUS_retail")) {
  plot_data = process_results(data[[scen]])
  plot_data = base::merge(map_world_gtap, plot_data$equilibrium_new$welfare_change, by.x = "gtap_code", by.y = "country")
  plot_data = st_transform(plot_data, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # plot
  bbox = st_bbox(plot_data[plot_data$gtap_code != "XTW",])
  ggplot(data = plot_data[plot_data$gtap_code != "XTW",]) +
    theme_map() +
    geom_sf(aes(fill = value),
            size = 0.1, # size of the border
            color = alpha("black", 0.1)) + # color of the border
    coord_sf(xlim = c(bbox[[1]], bbox[[3]]), ylim = c(bbox[[2]], bbox[[4]]), expand = FALSE) +
    scale_fill_gradient2(name = "in %",
                         low = "#c4100a",
                         mid = "#f1f1f1",
                         high = "#1f5c99",
                         midpoint = 0) +
    theme(legend.title = element_text(vjust = 0.75),
          legend.position = c(0.05,0.1),
          legend.direction = "vertical")
  ggsave(str_c("output/figures/map_welfare_", scen ,".pdf"), width = 34, height = 17, units = "cm", dpi = 300)
}

 # p - map EU focus  ------------------------------------------------------------------------------------------------------------------------------------------
for (scen in c("EU-CHN_retail", "US_all-RUS_retail")) {
  plot_data = process_results(data[[scen]])
  plot_data = base::merge(map_world_gtap, plot_data$equilibrium_new$welfare_change, by.x = "gtap_code", by.y = "country")
  plot_data = st_transform(plot_data, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs")
  
  bbox = st_bbox(plot_data[plot_data$gtap_code %in% c("ESP", "FIN", "BGR", "GRE", "ROU"),])
  ggplot(data = plot_data) +
    theme_map() +
    geom_sf(aes(fill = value),
            size = 0.05, # size of the border
            color = alpha("black", 0.1)) + # color of the border
    coord_sf(xlim = c(bbox[[1]]-200000, bbox[[3]]+200000), ylim = c(bbox[[2]]+700000, bbox[[4]]+100000), expand = FALSE) +
    scale_fill_gradient2(name = "in %",
                         low = "#c4100a",
                         mid = "#f1f1f1",
                         high = "#1f5c99",
                         midpoint = 0) +
    theme(legend.title = element_text(vjust = 0.75),
          legend.position = c(0,0.3),
          legend.direction = "vertical")
  ggsave(str_c("output/figures/map_welfare_", scen, "_EU_focus.pdf"), width = 30, height = 24, units = "cm", dpi = 300)
}


# plot for revision ----
library(extrafont)
extrafont::font_import()

# load data
data = list()
for (ntb_new in seq(5,100,5)) {
  data[[ntb_new]] = read_rds(str_c("output/results/results_EU-CHN_retail_ntb_change_", (1 + ntb_new/100), ".rds"))
}

data_plot = data.table()
for (ntb_new in seq(5,100,5)) {
  data_temp = process_results(data[[ntb_new]])
  
  table_temp = data.table()
  table_temp[, ntb_change := ntb_new]
  
  # real trade flow new
  trade_flow_new = merge(data_temp$equilibrium_new$trade_flow_new, data_temp$equilibrium_new$price_change, 
                         by.x = c("destination", "sector"), by.y = c("country", "sector")) %>% 
    mutate(value = value.x / value.y) %>% select(-value.x, -value.y)
  
  table_temp[, trade_change := ((trade_flow_new[origin == "CHN" & destination %in% EU27, sum(value)] + 
                                   trade_flow_new[origin %in% EU27 & destination == "CHN", sum(value)]) /
                                  (data_temp$equilibrium_new$trade_flow[origin == "CHN" & destination %in% EU27, sum(value)] + 
                                     data_temp$equilibrium_new$trade_flow[origin %in% EU27 & destination == "CHN", sum(value)]) - 1) * 100]
  
  # welfare change CHN
  table_temp[, welfare_change_CHN := data_temp$equilibrium_new$welfare_change[country == "CHN", value]]
  
  # welfare change EU
  welfare_change = merge.data.table(data_temp$equilibrium_new$welfare_change, data_temp$initial_conditions$value_added, by = "country") %>% 
    setnames(c("country", "welfare_change", "value_added"))
  welfare_change[country %in% EU27, value_added_share := value_added / sum(value_added)]
  table_temp[, welfare_change_EU := welfare_change[country %in% EU27, sum(welfare_change * value_added_share)]]
  
  data_plot = rbind(data_plot, table_temp)
  rm(data_temp, trade_flow_new, welfare_change)
}

setnames(data_plot, c("trade_change", "welfare_change_CHN", "welfare_change_EU"), c("Trade flow", "Welfare China", "Welfare EU"))
data_plot = melt(data_plot, id.vars = "ntb_change")

data_plot %>% ggplot() +
  geom_line(aes(ntb_change, -1*value, color = variable)) +
  scale_y_log10() +
  scale_color_manual(values = c("#003f5c", "#bc5090", "#ff6361")) +
  # geom_hline(yintercept = 0) +
  # geom_vline(xintercept = 0) +
  coord_cartesian(clip = 'off') +
  labs(y = "Decrease in %", x = "NTB increase in %") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # panel.background = element_rect(fill = "white"),
        axis.line.y.left =element_line(color="black"),
        axis.line.x.bottom=element_line(color="black"))

ggsave(str_c("output/figures/plot_results_EU-CHN_retail_ntb_chnage_sensitivity.pdf"), width = 30, height = 20, units = "cm", dpi = 300))

