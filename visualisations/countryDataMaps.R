require(ggplot2)
require(maps)
library(mapdata)
library(dplyr)

resultsRoot = "~/LandSymm/conclim-results"
ssp14Root = file.path(resultsRoot, "ssp14")
conclimRoot = file.path(resultsRoot, "conclim")

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
