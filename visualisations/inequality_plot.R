library(dplyr)
library(ggplot2)

inequality_scenarios = read.csv(file.choose())
unique(inequality_scenarios$GCAM_region_name)

inequality_scenarios_filtered  = inequality_scenarios %>% 
  filter(category == "d1") %>%
  filter(model == "Historical data"|model == "PCA algorithm (Two Components)")%>%
  filter(GCAM_region_name =="EU-15") %>%
  mutate(sce = replace(sce, sce == "Historical data", "SSP2"))%>%
  select(GCAM_region_name, year, gini, model, sce)

ggplot(inequality_scenarios_filtered, aes(x = year, y = gini, group = sce, color =sce))+
  geom_line(size = 1) +
  labs(title = "Inequality in the EU-15",
       x = "Year",
       y = "Gini Coefficient",
       color = "Scenario") +
  theme_light()+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), text=element_text(size = 20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title = element_blank())+
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray", size = 1)+
  labs(legend = NULL) +
  scale_x_continuous(limits = c(1967, 2100))
