rm(list=ls())

require(tidyverse)
require(sf)
require(lme4)

e0 <- readRDS("temp_data/e0.rds")
map_file <- readRDS("raw_data/gadm36_DEU_2_sf.rds")

##Outcome variable - how has the gap with western average changed?
#Diff to western average

beta_convergence <- e0 %>%
  group_by(AGS, geo, Sex2, State_name, East) %>%
  summarise(e0_start = first(pct50),
            e0_diff = last(pct50)-first(pct50))

beta_convergence %>%
  group_by(Sex2, State_name) %>%
  summarise(e0_start = mean(e0_start),
            e0diff = mean(e0_diff)) %>%
  print(n = 100)

unconditional_beta = lm(e0_diff ~ e0_start+Sex2, data = filter(beta_convergence, East == "West"))
summary(unconditional_beta)

ggplot(beta_convergence, aes(x = e0_start, y = e0_diff))+
  geom_point(aes(colour = East))+
  geom_smooth(method = "lm")+
  facet_grid(East ~ Sex2)

