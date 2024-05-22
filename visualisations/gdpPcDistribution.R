library(dplyr)
library(ggplot2)
library(forcats)
library(spatstat.utils)
setwd("C:/Users/kwaxenberg")

#Load data
ssp = read.csv("git/plumv2/data/ssp.csv")
deciles = read.csv("Documents/LandSymm/Inequality models/data/narayan_deciles.csv")

ssp = ssp %>% mutate(gdpPc = gdp/population) %>% filter(Year ==2020, SSP == "SSP2")
deciles = deciles %>% filter(year ==2020)%>% left_join(ssp, join_by(ISO==Iso3))%>%na.omit()
deciles = deciles %>% mutate(gdpPc_decile = (gdp*pred_shares)/(population/10))

ggplot(data = deciles)+
  geom_histogram(aes(x=gdpPc), fill="lightgreen",
                   color="darkgreen",
                   alpha=0.7)+
  geom_histogram(aes(x=gdpPc_decile), fill="lightblue",
                   color="blue",
                   alpha=0.7)

max(deciles$gdpPc)
max(deciles$gdpPc_decile)

#calculate what fraction of the decile gdpPc is outside the original data
range2020=range(ssp$gdpPc)

outsideRangeAmount<- function(gdpPc_decile, range2020){
  if(gdpPc_decile<range2020[1]){
    amount = gdpPc_decile - range2020[1]
  } else  if(gdpPc_decile>range2020[2]){
    amount = gdpPc_decile - range2020[2]
  } else{
    amount = 0
  }
  return(amount)}
deciles = deciles %>% mutate(diff = as.double(lapply(deciles$gdpPc_decile, outsideRangeAmount,range2020 = range2020)))

#calculate the percent of all deciles in 2020 outside the original range 
count(deciles[which(deciles$diff!=0),])/count(deciles)
count(deciles[which(deciles$diff>0),])/count(deciles)
count(deciles[which(deciles$diff<0),])/count(deciles)

ggplot(data = deciles)+
  geom_density(aes(x=diff), fill="lightgreen",
                 color="darkgreen",
                 alpha=0.7)
