library(dplyr)
library(ggplot2)
library(forcats)
setwd("C:/Users/kwaxenberg")

#Load data
gamsDemand = read.csv("Documents/results/resultsData/calib/countryDemandOpt.txt") #load countryDemandOpt.txt from the calibration run
countryDemand = read.csv("Documents/results/resultsData/calib/countryDemand.txt") #load countryDemand.txt from the calibration run
baseDemand = read.csv("git/plumv2/data/base_consump.csv")
countries = read.csv("git/plumv2/data/countries.csv")
kcal = read.csv("git/plumv2/data/calories_per_t.csv")

gamsDemand = left_join(gamsDemand, countries, join_by(Country == Area))
gamsDemand =  gamsDemand %>% mutate(decile = as.double(substring(decile, 2)))

country = "IND"
year = 2020

zeros = gamsDemand %>% filter(plumRebased ==0)
undefined = gamsDemand %>% filter(status == "UNDEFINED_STAT")
unique(undefined$Country)
hungry = gamsDemand %>% filter(hungerFactor>0)

############ Plot kcal demand by income group for a single country ###############
countryYearDemand = gamsDemand %>% filter(Iso3 == country, Year == year, commodity != "Nonfood")

#add baseline kcal per person per day
baseDemandKcal = left_join(baseDemand, kcal, join_by(Iso3, plumDemandItem))
baseDemandKcal = baseDemandKcal %>% mutate(rebasedKcal = 40* baseCpc * kcalPerT/365, decile = 0) %>% 
  rename(commodity = plumDemandItem)
countryYearDemand_joined = bind_rows(list(countryYearDemand, baseDemandKcal[which(baseDemand$Iso3 == country),]), )

ggplot(countryYearDemand_joined, aes(x = decile, y = rebasedKcal, fill = commodity))+
  geom_col()+
  geom_col()+
  labs(title = "2020 UK Food Demand by income group",
       x = "Income Decile",
       y = "Consumption (kcal/day)",
       color = "Commodity group") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank())

################Check demand vs base demand##################
totalDemand = gamsDemand %>% filter(commodity != "Nonfood") %>% group_by(Iso3, Year, commodity) %>% summarise(rebasedTonnesPc = sum(plumRebased, na.rm=TRUE)/400)
totalDemand = left_join(totalDemand, baseDemand, join_by(Iso3 == Iso3, commodity == plumDemandItem))
ggplot(totalDemand, aes(x = baseCpc, y = rebasedTonnesPc, colour = commodity))+
  geom_point()

joinedDemand = left_join(gamsDemand, baseDemand, join_by(Iso3 == Iso3, commodity == plumDemandItem)) %>% filter(commodity != "Nonfood")
ggplot(joinedDemand, aes(x = baseCpc, y = plumRebased))+
  geom_point()

##############Check country demand against base FAO demand#################

countryJoined = left_join(countryDemand, countries, join_by(Country==Area)) %>%
  filter(!is.na(Iso3)) %>%
  select(Year, Iso3, Commodity, Demand, ConsumerPrice, Population)%>%
  left_join(baseDemand, join_by(Iso3 == Iso3, Commodity==plumDemandItem)) %>%
  mutate(plumCpc = (Demand * (10^6))/(Population * (10^3))) %>%
  mutate(fao_diff = (plumCpc - baseCpc))

countryJoined2020 = countryJoined %>% filter(Year ==2020)

ggplot(countryJoined2020, aes(x = baseCpc, y = plumCpc))+
  geom_point()

ggplot(countryJoined, aes(x = Year, y = fao_diff, color = Commodity))+
  geom_point()+
  labs(title = "Difference from baseline FAO data over calibration period",
       x = "Year",
       y = "Difference from baseline (tonnes/person/year)",
       color = "Commodity group") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))

ggplot(countryJoined, aes(x = Year, y = plumCpc, color = Commodity))+
  geom_point()

totalAnnualDemand = countryJoined %>% group_by(Iso3, Year) %>% summarise (totalBase = sum(baseCpc, na.rm=TRUE), totalPlum = sum(plumCpc, na.rm = TRUE)) %>% mutate(diff = totalBase-totalPlum)
