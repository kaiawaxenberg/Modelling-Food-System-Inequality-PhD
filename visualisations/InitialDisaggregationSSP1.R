library(dplyr)
library(ggplot2)

setwd("C:/Users/kwaxenberg/Documents/LandSymm/Disaggregation Results/results data")

#load in SSP1 results without inequality
countryDemandOpt_ssp1_0 = read.csv("SSP1_ssp15/countryDemandOpt.txt")
countryDemand_ssp1_0 = read.csv("SSP1_ssp15/countryDemand.txt")
lc_ssp1_0 = read.csv("SSP1_ssp15/lc.txt")

#load in SSP1 results with inequality
countryDemandOpt_ssp1_dec = read.csv("SSP1_inequality_test/countryDemandOpt.txt")
countryDemand_ssp1_dec = read.csv("SSP1_inequality_test/countryDemand.txt")
lc_ssp1_dec = read.csv("SSP1_inequality_test/lc.txt")

#create a graph of annual global demand for both models 
globalDemand0 = countryDemand_ssp1_0 %>% group_by(Year) %>% summarise(totalFoodDemand0 = sum(Demand))
globalDemandDec =countryDemand_ssp1_dec %>% group_by(Year) %>% summarise(totalFoodDemandDec = sum(Demand))
globalDemand = merge(globalDemand0, globalDemandDec, by="Year", all=T)

ggplot(data=globalDemand, aes(x=Year)) +
  geom_line(aes(y = totalFoodDemand0, color = "Stable inequality")) + 
  geom_line(aes(y = totalFoodDemandDec, color = "Changing inequality")) + 
  labs(title = "SSP1 food demand with and without inequality",
       x = "Year",
       y = "Global food Demand (million T)",
       color = "Scenario")
demand2100 = globalDemand[which(globalDemand$Year == 2100),]
(demand2100$totalFoodDemandDec-demand2100$totalFoodDemand0)/(demand2100$totalFoodDemand0+demand2100$totalFoodDemandDec)


##trying to understand the patterns
globalDemand0_commodity = countryDemand_ssp1_0 %>% group_by(Year, Commodity) %>% summarise(totalDemand = sum(Demand))

ggplot(data=globalDemand0_commodity, aes(x=Year,y = totalDemand, color = Commodity)) +
  geom_line()+
  labs(title = "SSP1 food demand",
       x = "Year",
       y = "Global food Demand (million T)",
       color = "Commodity")


