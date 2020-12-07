rm(list=ls())

require(tidyverse)
require(sf)

e0 <- readRDS("temp_data/e0.rds")

map_file <- readRDS("raw_data/gadm36_DEU_2_sf.rds")
map_df <- left_join(e0, map_file, by = c("AGS" = "CC_2"))

p1m <- map_df %>%
  filter(year %in% c(1997, 2003, 2009, 2015)) %>%
  filter(Sex2 == "M") %>%
  ggplot()+
  geom_sf(aes(fill = pct50, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(. ~ year)+
  theme_bw()+
  labs(fill = "e0")

ggsave("figures/p1m.png", p1m)

p1f <- map_df %>%
  filter(year %in% c(1997, 2003, 2009, 2015)) %>%
  filter(Sex2 == "F") %>%
  ggplot()+
  geom_sf(aes(fill = pct50, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(. ~ year)+
  theme_bw()+
  labs(fill = "e0")

ggsave("figures/p1f.png", p1f)
