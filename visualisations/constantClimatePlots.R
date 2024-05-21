require(ggplot2)
library(dplyr)

resultsRoot = "~/LandSymm/conclim-results"
ssp14Root = file.path(resultsRoot, "ssp14")
conclimRoot = file.path(resultsRoot, "conclim")

#################GLOBAL DEMAND ANALYSIS########################

#Import Demand data
ssp2ConclimDemand = read.csv(file.path(conclimRoot, "SSP2_demand.txt"))
ssp2Demand = read.csv(file.path(ssp14Root, "SSP2_demand.txt"))

#Create dataset with commodity-level demand for both scenarios
ssp2ConclimDemand = rename(ssp2ConclimDemand, MtConclim = Amount..Mt.)
ssp2Demand = rename(ssp2Demand, MtCC = Amount..Mt.)
ssp2Demand <- full_join(ssp2Demand, ssp2ConclimDemand, by = c("Year", "Commodity")) %>%
  arrange(Year, Commodity)
ssp2Demand = ssp2Demand %>% mutate(percentChange = ((MtCC - MtConclim)/(MtCC + MtConclim))*100)

#Calculate total annual demand
ssp2ConclimDemand$Year <- as.factor(ssp2ConclimDemand$Year)
ssp2Demand$Year <- as.factor(ssp2Demand$Year)
ssp2DemandTotal <- ssp2Demand %>%
  filter(Commodity != "energycrops") %>%
  group_by(Year) %>%
  summarise(totalFoodDemand = sum(Amount..Mt.))
ssp2ConclimDemandTotal <- ssp2ConclimDemand %>%
  filter(Commodity != "energycrops") %>%
  group_by(Year) %>%
  summarise(foodDemandConclim = sum(Amount..Mt.))
compareDemand <- full_join(ssp2DemandTotal, ssp2ConclimDemandTotal, by = "Year") %>%
  arrange(Year)
compareDemand = compareDemand %>% mutate(percentChange = ((totalFoodDemand - foodDemandConclim)/(totalFoodDemand + foodDemandConclim))*100)

#Plot demand by commodity
ggplot(ssp2Demand, aes(x = Year, y = MtCC, fill = Commodity)) +
  geom_bar(stat = "identity", position = "stack",colour = "NA", width = 1) +
  labs(title = "SSP2 Demand for Food Commodities",
       x = "Year",
       y = "Demand (Mt)") +
  theme_minimal()+
  theme(text=element_text(size = 20), legend.position= "bottom")+
  theme(legend.title=element_blank())

#Plot divergence in total demand
ggplot(compareDemand, aes(x = Year)) +
  geom_line(aes(y = totalFoodDemand, color = "Climate Change"), size = 1) +
  geom_line(aes(y = foodDemandConclim, color = "Constant Climate"), size = 1) +
  labs(title = "Global Food Demand - Climate Change Effect",
       x = "Year",
       y = "Total Food Demand (Mt)") +
  scale_color_manual(values = c("Climate Change" = "blue", "Constant Climate" = "red")) +
  theme_minimal()

#Plot divergence in commodity demand
ssp2DemandLong <- ssp2Demand %>% pivot_longer(
  cols = c(MtCC, MtConclim), 
  names_to = "climateScenario", 
  values_to = "demandMt")
ssp2DemandLong = subset(ssp2DemandLong, select= c(Year, Commodity, demandMt, climateScenario))

ggplot(ssp2DemandLong, aes(x = Year, y = demandMt, group = climateScenario, color = climateScenario)) +
  geom_line() +
  facet_wrap(~ Commodity, scales = "free_y", ncol = 4) +
  labs(title = "Commodity Demand SSP2 - Climate Change Effect",
       x = "Year",
       y = "Global Demand (Mt)") +
  theme_minimal()

#################PRICE ANALYSIS########################

#Import Price data
ssp2ConclimPrices = read.csv(file.path(conclimRoot, "SSP2_prices.txt"))
ssp2Prices = read.csv(file.path(ssp14Root, "SSP2_prices.txt"))

#Create table with commodity-level prices for both scenarios
ssp2ConclimPrices = rename(ssp2ConclimPrices, priceConclim = New.export.price)
ssp2Prices = rename(ssp2Prices, priceCC = New.export.price)
ssp2Prices <- full_join(ssp2Prices, ssp2ConclimPrices, by = c("Year", "Crop")) %>%
  arrange(Year, Crop)
ssp2Prices = ssp2Prices %>% mutate(percentDiff = ((priceCC - priceConclim)/(priceCC + priceConclim))*100)
ssp2Prices = subset(ssp2Prices, select = c(Year, Crop, priceCC, priceConclim))
ssp2Prices = ssp2Prices %>% filter(Crop != c("carbon", "roundwood", "fuelwood"))
ssp2PricesLong <- ssp2Prices %>% pivot_longer(
                               cols = c(priceCC, priceConclim), 
                               names_to = "climateScenario", 
                               values_to = "exportPrice")

#Plot divergence in prices
ggplot(ssp2PricesLong, aes(x = Year, y = exportPrice, group = climateScenario, color = climateScenario)) +
  geom_line() +
  facet_wrap(~ Crop, scales = "free_y", ncol = 4) +
  labs(title = "Export Price SSP2 - Climate Change Effect",
       x = "Year",
       y = "Export Price") +
  theme_minimal()

#################COUNTRY DEMAND ANALYSIS########################
#Import demand and country data
ssp2ConclimDemand = read.csv(file.path(conclimRoot, "SSP2_countryDemandOpt.txt"))
world_map = map_data("world")
map_countries = as.data.frame(unique(world_map$region))

#Match country names to mapping data
ssp2ConclimDemand$Year <- as.factor(ssp2ConclimDemand$Year)
ssp2ConclimDemand =  ssp2ConclimDemand %>% 
  mutate(Country = case_when(Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             Country == "Brunei Darussalam" ~ "Brunei",
                             Country == "Democratic People's Republic of Korea" ~ "North Korea",
                             Country == "Republic of Korea" ~ "South Korea",
                             Country == "Republic of Moldova" ~ "Moldova",
                             Country == "United States of America" ~ "USA",
                             Country == "United Kingdom" ~ "UK",
                             Country == "Syrian Arab Republic" ~ "Syria",
                             Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                             Country == "Turkiye" ~ "Turkey",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "Viet Nam" ~ "Vietnam",
                             Country == "Cabo Verde" ~ "Cape Verde",
                             Country == "Congo" ~ "Republic of Congo",
                             Country == "Cote d'Ivoire" ~ "Ivory Coast",
                             Country == "Czechia" ~ "Czech Republic",
                             Country == "Eswatini" ~ "Swaziland",
                             Country == "Iran (Islamic Republic of)" ~ "Iran",
                             Country == "Lao People's Democratic Republic" ~ "Laos",
                             Country == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
                             Country == "Trinidad and Tobago" ~ "Trinidad",
                             Country == "United Republic of Tanzania" ~ "Tanzania",
                             TRUE ~ Country))
unique(ssp2ConclimDemand$Country[which(!(ssp2ConclimDemand$Country %in% world_map$region))])

#Create data with total calories
ssp2ConclimDemandTotal <- ssp2ConclimDemand %>%
  filter(commodity != "Nonfood") %>%
  group_by(Country, Year) %>%
  summarise(rebasedKcal = sum(rebasedKcal))

#Plot the map
merged_data <- merge(world_map, ssp2ConclimDemandTotal[which(ssp2ConclimDemandTotal$Year==2050),], by.x = "region", by.y = "Country", all.x = TRUE)

ggplot() +
  geom_map( 
    data = merged_data, map = world_map, 
    aes(long, lat, map_id = region, fill= rebasedKcal)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Adjust color scale
  theme_void() +  # Removes axis and gridlines
  labs(title = "Daily Kilocalorie Demand per Capita (SSP2, 2050)", legend = "Kcal")

#Plot map for individual commodities
merged_data <- merge(world_map, ssp2ConclimDemand[which(ssp2ConclimDemand$Year==2050 & ssp2ConclimDemand$commodity=="CerealsStarchyRoots"),], by.x = "region", by.y = "Country", all.x = TRUE)

ggplot() +
  geom_map( 
    data = merged_data, map = world_map, 
    aes(long, lat, map_id = region, fill= rebasedKcal)) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +  # Adjust color scale
  theme_void() +  # Removes axis and gridlines
  labs(title = "Daily Starchy Staples Demand per Capita (SSP2, 2050)")

#################LAND USE ANALYSIS########################


