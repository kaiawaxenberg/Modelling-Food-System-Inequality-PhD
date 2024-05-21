library(dplyr)
library(ggplot2)
library(data.table)

setwd("C:/Users/kwaxenberg/Documents/LandSymm/Inequality models/data")

########R script to translate the gini coefficient to income distribution##############
########Code adapted from Narayan et al, 2023#############

decileShares = read.csv("narayan_deciles.csv")%>% filter(year==2020, sce=="SSP2")
decileShares = decileShares %>% mutate(decile = as.double(substring(Category, 2)))

#Compute gini coefficient from income shares by decile
#note input data for this function must have columns "decile", "pred_shares", "ISO", "year", "ssp"
compute_gini_deciles<- function(df){
    
    df = df %>% 
    mutate(share_of_richer_pop = 1 - 0.1*decile) %>% 
     mutate(score = pred_shares *(0.1+ (2*share_of_richer_pop))) %>%   
     group_by(ISO, year) %>% 
     mutate(gini= 1- sum(score)) %>% # the gini coefficient is 1 - the area under the lorenz curve
      ungroup()
           
     return(df)}

giniBaseline = compute_gini_deciles(decileShares)%>% na.omit()
giniBaseline = giniBaseline %>% select(ISO, year, gini) %>% distinct()

#Add baseline gini for countries not included in Narayan
regions = read.csv("all.csv")
ssp = read.csv("ssp.csv") %>% filter(Year==2020, SSP=="SSP2") %>% mutate(gdpPc = gdp/population)
giniBaseline = left_join(giniBaseline, regions, join_by(ISO == alpha.3)) %>% select(ISO, year, gini, sub.region)
for(country in setdiff(ssp$Iso3, giniBaseline$ISO)){
    avgRegion = regions[which(regions$alpha.3==country),]$sub.region
    regionData = giniBaseline %>% filter(sub.region == avgRegion)
    regionAvgGini = mean(regionData$gini)
    giniBaseline = giniBaseline %>% add_row(year = 2020, ISO = country, gini = regionAvgGini, sub.region = avgRegion)
}


#Forecast gini coefficient by year for sensitivity analysis
perc_change = +0.3 #change this value for sensitivity analysis
yrs = seq(2021, 2100)
countries = unique(giniBaseline$ISO)
giniData = giniBaseline
for (country in countries){
  initialGini = giniData[which(giniData$year ==2020 & giniData$ISO == country),]$gini
  finalGini = initialGini + initialGini*perc_change
  for (yr in yrs){
    ratio = (yr - (head(yrs, n=1) -1)) / (tail(yrs, n=1) - (head(yrs, n=1) -1))
    giniData = giniData %>% add_row(year = yr, ISO = country, gini = (ratio*finalGini + (1- ratio)* initialGini))
  }
}

#Check a linear increase/decrease has been achieved
ggplot(data=giniData,aes(x=year,y=gini))+
  geom_smooth()


#join gdpPc data with the gini data
giniGdpData = left_join(giniData, ssp, join_by(ISO==Iso3)) %>% select(ISO, year, gini, gdpPc)

#Compute lognormal distribution from gini coefficient
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)

compute_lognormal_dist <- function(mean_income, gini, max_income, len_sample){
  
  sd <- 2 * erfinv(gini)
  m <- log(mean_income) -((sd^2)/2)  
  
  draws3 <- dlnorm(seq(0, max_income, length.out=len_sample), m, sd)
  
  draw_d <- as.data.frame(draws3)  %>% 
    mutate(gdpPc = seq(0, max_income, length.out=len_sample)) %>% 
    rename(density = draws3 )
  
  return(draw_d)
}

data_for_lognorm = giniGdpData %>% mutate(id = paste0(ISO,year)) %>% na.omit()
data_for_lognorm_split <- split(data_for_lognorm, data_for_lognorm$id)

compute_lognormal_country <- function(df){
  
  mean <- df$gdpPc
  gini <- df$gini
  max_income <- df$gdpPc*8.54
  len_sample <- 2000

  results <- compute_lognormal_dist(mean_income = mean,
                                    gini = gini,
                                    max_income = max_income,
                                    len_sample = len_sample)
  results$ISO = df$ISO
  results$year = df$year
  return(results)
}

computed_lognorm_model_list <- lapply(data_for_lognorm_split,compute_lognormal_country)

#Transate lognormal model to decile income shares
computed_lognorm_model <- rbindlist(computed_lognorm_model_list)
log_normal_downscaled_results = computed_lognorm_model %>%
  group_by(ISO, year) %>% 
  arrange(gdpPc) %>% 
  mutate(tot_density = sum(density),
         tot_income = sum(density * gdpPc),
         cut_off = tot_density * 0.1,
         cut_off_d1 = cut_off,
         cut_off_d2 = cut_off * 2,
         cut_off_d3 =  cut_off * 3,
         cut_off_d4 =  cut_off * 4,
         cut_off_d5 = cut_off * 5,
         cut_off_d6 =  cut_off * 6,
         cut_off_d7 =  cut_off * 7,
         cut_off_d8 =  cut_off * 8,
         cut_off_d9 =  cut_off * 9,
         cum_density = cumsum(density)) %>% 
  ungroup() %>% 
  mutate(decile = if_else(cum_density < cut_off_d1, "d1",
                    if_else(cum_density < cut_off_d2, "d2",
                    if_else(cum_density < cut_off_d3, "d3",
                    if_else(cum_density < cut_off_d4, "d4",
                    if_else(cum_density < cut_off_d5, "d5",
                    if_else(cum_density < cut_off_d6, "d6",
                    if_else(cum_density < cut_off_d7, "d7",
                    if_else(cum_density < cut_off_d8, "d8",
                    if_else(cum_density < cut_off_d9, "d9","d10")))))))))) %>% 
  group_by(ISO, year, decile) %>% 
  mutate(gdp_decile = sum(density* gdpPc),
         shares = gdp_decile/tot_income,
         gdpPc_decile = sum(density* gdpPc)/sum(density)
  ) %>% 
  ungroup()

downscaled_results_for_plum = log_normal_downscaled_results %>%select(ISO, year, decile, shares) %>% 
  arrange(ISO, year, decile)%>%distinct() %>% mutate(sce = "SSP2") %>% rename(Category = decile, pred_shares = shares) %>%
  relocate(sce, .after = year)

write.csv(downscaled_results_for_plum, file = "sensitivity_files/giniPlus30Perc.csv", row.names=FALSE)
