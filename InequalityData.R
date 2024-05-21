library(dplyr)
library(tidyr)

setwd("~/Inequality models/data")

#Read in data tables
deciles = read.csv("narayan_deciles.csv")
countries = read.csv("countries.csv")
ssp = read.csv("ssp.csv")
regions = read.csv("all.csv")

setdiff(deciles$ISO, countries$Iso3)
setdiff(countries$Iso3, deciles$ISO)


#Create an empty table for storing income decile data for ISO countries in PLUM across all SSP and year combinations
incomeGroup = unique(deciles$Category)
year = unique(deciles$year)
sce=unique(deciles$sce)
data = crossing(countries$Iso3, year, sce, incomeGroup)
data = data %>% rename("Iso3" ="countries$Iso3") %>%
  arrange(Iso3, year, sce, incomeGroup)

#Merge this new table with the deciles data from Narayan
data = left_join(data, deciles, join_by(Iso3==ISO, year==year,sce == sce,incomeGroup==Category)) %>% select(!country)

#Fill in missing income shares by the regional average
missingCountries= unique(data[which(is.na(data$pred_shares)),]$Iso3)

data = left_join(data, countries, join_by(Iso3))%>% select(!M49)
data = left_join(data, deciles, join_by(FaoArea==ISO, year==year,sce == sce,incomeGroup==Category))
data = left_join(data, regions, join_by(Iso3==alpha.3))
data = data %>% rename("FaoShares"="pred_shares.y", "pred_shares" = "pred_shares.x")

#First fill in missing income shares by Fao Region, then ISO subregion
data =data %>%  mutate(pred_shares = coalesce(pred_shares,FaoShares))
data=data %>% 
  group_by(sub.region, year,sce ,incomeGroup)%>%
  mutate(SubRegionShares = mean(pred_shares, na.rm=T))
data =data %>%  mutate(pred_shares = coalesce(pred_shares,SubRegionShares))

#Select only relevant variables
data = data %>% ungroup() %>% select(Iso3, year, incomeGroup, sce, pred_shares, Population)

#Add GDP to the dataset, using interpolation methods for years between SSPS

#Figure out the units for population
#Calculate GDP per capita for each income decile
data = data%>% mutate(gdppc =(gdp*pred_shares)/(Population/10))

#Compare these results to gdppc in narayan and national level
