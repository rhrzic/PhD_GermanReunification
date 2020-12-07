rm(list=ls())

require(tidyverse)
require(sf)

map_file <- readRDS("raw_data/gadm36_DEU_2_sf.rds") %>%
  mutate(CC_2_num = as.numeric(CC_2))

load("results/e0-1997.RData")


e0.our.model %>%
  ggplot()+
  geom_histogram(aes(x = pct50))+
  facet_grid(Sex ~ .)

map_df <- left_join(e0.our.model, map_file, by = c("AGS" = "CC_2_num"))

map_df %>%
  filter(Sex == 1) %>%
  ggplot()+
  geom_sf(aes(fill = pct50, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(Sex ~ .)

