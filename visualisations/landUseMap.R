require(ggplot2)
require(maps)
library(mapdata)
library(dplyr)

resultsRoot = "~/LandSymm/conclim-results"
ssp14Root = file.path(resultsRoot, "ssp14")
conclimRoot = file.path(resultsRoot, "conclim")

#Load Land Cover Data for 2050
lu = read.table(file.path(conclimRoot, "SSP2_2050_LandCover.txt"), header= TRUE)
world <- map_data("world")

summed_lu <- lu %>%
  group_by(Lat, Lon) %>%
  summarise(Cropland = sum(Cropland), 
            Pasture = sum(Pasture),
            TotalArea = sum(TotalArea)) %>%
  mutate(Cropland_frac = Cropland/TotalArea,
         Pasture_frac = Pasture/TotalArea)

#Map a single land use type
crop_map <- ggplot() +
  geom_point(data = summed_lu, aes(x = Lon, y = Lat, colour=Cropland_frac), size = 0.05) +
  scale_colour_gradient(name ="Cropland", low = NA, high = "brown") + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill =NA, color = "lightgrey", linewidth=0.02)+
  theme_void()+
  labs(title = "Cropland in 2050 (SSP2 Constant Climate)")
print(crop_map)

pasture_map <- ggplot() +
  geom_point(data = summed_lu, aes(x = Lon, y = Lat, colour=Pasture_frac), size = 0.05) +
  scale_colour_gradient(name = "Pasture",low = NA, high = "green") +  
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill =NA, color = "lightgrey", linewidth=0.02)+
  theme_void()+
  labs(title = "Pasture in 2050 (SSP2 Constant Climate)")
print(pasture_map)


