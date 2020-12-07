rm(list=ls())

library(tidyverse)

deaths <- readRDS("temp_data/deaths.rds")
pop <- readRDS("temp_data/mid_year_pop.rds")

mortality <- left_join(pop, deaths, by = c("year", "AGS", "geo", "age", "sex"))

saveRDS(mortality, file = "temp_data/mortality.rds")

#mortality <- mortality %>%
#  mutate(deaths = ifelse(deaths == 0, 1, deaths), mx = deaths/pop, logmx = log(mx))

#mortality %>%
#  ggplot(aes(x = age, y = logmx, group = AGS, colour = sex, alpha = year))+
#  geom_point()
