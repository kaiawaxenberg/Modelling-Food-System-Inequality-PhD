library(dplyr)
library(ggplot2)

##LOAD AND CLEAN ALL RESULTS DATA

scenarios = c("plus30", "plus15", "minus30", "minus15", "noChange")

#load raw results files for all scenarios
for (scenario in scenarios){
  setwd(paste0("C:/Users/kwaxenberg/Documents/results/resultsData/sensitivity/", scenario))
  
  assign(paste0("countryDemand_", scenario),  read.csv("countryDemand.txt") %>%
    select(Country, Year, Commodity, Demand, ConsumerPrice) %>%
    rename_with(.fn = ~paste0(., scenario), .cols = c(Demand, ConsumerPrice))
  )
  assign(paste0("countryDemandOpt_", scenario),  read.csv("countryDemandOpt.txt") %>%
           select(Country, Year, decile, commodity, gdpPc, population, price, rebasedKcal) %>%
           rename_with(.fn = ~paste0(., scenario), .cols = c(gdpPc, population, price, rebasedKcal))
  )
  assign(paste0("lc_", scenario),  read.csv("lc.txt") %>%
           select(Year, Cropland, Pasture) %>%
           rename_with(.fn = ~paste0(., scenario), .cols = c(Cropland, Pasture))
  )
}

#combine into a single table
countryDemandOpt = left_join(countryDemandOpt_plus30, countryDemandOpt_plus15, join_by(Country, Year, decile, commodity)) %>%
  left_join(countryDemandOpt_minus30, join_by(Country, Year, decile, commodity)) %>%
  left_join(countryDemandOpt_minus15, join_by(Country, Year, decile, commodity)) %>%
  left_join(countryDemandOpt_noChange, join_by(Country, Year, decile, commodity))

countryDemand = left_join(countryDemand_plus30, countryDemand_plus15, join_by(Country, Year, Commodity)) %>%
  left_join(countryDemand_minus30, join_by(Country, Year, Commodity)) %>%
  left_join(countryDemand_minus15, join_by(Country, Year, Commodity)) %>%
  left_join(countryDemand_noChange, join_by(Country, Year, Commodity))

lc = left_join(lc_plus30, lc_plus15, join_by(Year)) %>%
  left_join(lc_minus30, join_by(Year)) %>%
  left_join(lc_minus15, join_by(Year)) %>%
  left_join(lc_noChange, join_by(Year))

#remove excess tables from the environment
rm(countryDemand_plus30, countryDemand_plus15, countryDemand_noChange, countryDemand_minus15, countryDemand_minus30)
rm(countryDemandOpt_plus30, countryDemandOpt_plus15, countryDemandOpt_noChange, countryDemandOpt_minus15, countryDemandOpt_minus30)
rm(lc_plus30, lc_plus15, lc_noChange, lc_minus15, lc_minus30)

##PLOT TOTAL GLOBAL DEMAND FOR EACH SCENARIO OVER TIME
totalGlobalDemand = countryDemand %>% group_by(Year) %>% summarise(totalDemandPlus15 = sum(Demandplus15), 
                                                                   totalDemandPlus30 = sum(Demandplus30),
                                                                   totalDemandMinus15 = sum(Demandminus15),
                                                                   totalDemandMinus30 = sum(Demandminus30),
                                                                   totalDemandNoChange = sum(DemandnoChange))
ggplot(data=totalGlobalDemand, aes(x=Year)) +
  geom_line(aes(y = totalDemandPlus30, color = "Gini +30%")) + 
  geom_line(aes(y = totalDemandPlus15, color = "Gini +15%")) + 
  geom_line(aes(y = totalDemandNoChange, color = "Gini +0%")) + 
  geom_line(aes(y = totalDemandMinus15, color = "Gini -15%")) + 
  geom_line(aes(y = totalDemandMinus30, color = "Gini -30%")) + 
  labs(title = "Total global food demand",
       x = "Year",
       y = "Global food Demand (million T)",
       color = "Inequality scenario")

##PLOT TOTAL GLOBAL COMMODITY DEMAND FOR EACH SCENARIO OVER TIME
totalCommodityDemand = countryDemand %>% group_by(Year, Commodity) %>% summarise(totalDemandPlus15 = sum(Demandplus15), 
                                                                   totalDemandPlus30 = sum(Demandplus30),
                                                                   totalDemandMinus15 = sum(Demandminus15),
                                                                   totalDemandMinus30 = sum(Demandminus30),
                                                                   totalDemandNoChange = sum(DemandnoChange))
ggplot(data=totalCommodityDemand, aes(x=Year, color=Commodity)) +
  geom_line(aes(y = totalDemandPlus30, linetype = "Gini +30%")) + 
  geom_line(aes(y = totalDemandNoChange, linetype = "Gini +0%")) + 
  geom_line(aes(y = totalDemandMinus30, linetype = "Gini -30%")) + 
  labs(title = "Total global food demand",
       x = "Year",
       y = "Global commodity Demand (million T)",
       color = "Inequality scenario")

##PLOT COUNTRY COMMODITY DEMAND
country = "United Kingdom"
countryCommodityDemand = countryDemand %>% filter(Country== country) %>%
  group_by(Year, Commodity) %>% summarise(totalDemandPlus15 = sum(Demandplus15), 
                                         totalDemandPlus30 = sum(Demandplus30),
                                         totalDemandMinus15 = sum(Demandminus15),
                                         totalDemandMinus30 = sum(Demandminus30),
                                         totalDemandNoChange = sum(DemandnoChange))
ggplot(data=countryCommodityDemand, aes(x=Year, color=Commodity)) +
  geom_line(aes(y = totalDemandPlus30, linetype = "Gini +30%")) + 
  geom_line(aes(y = totalDemandNoChange, linetype = "Gini +0%")) + 
  geom_line(aes(y = totalDemandMinus30, linetype = "Gini -30%")) + 
  labs(title = "Total global food demand",
       x = "Year",
       y = "Global commodity Demand (million T)",
       color = "Inequality scenario")
