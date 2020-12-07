rm(list=ls())

require(tidyverse)
require(sf)
require(lme4)

e0 <- readRDS("temp_data/e0.rds")
map_file <- readRDS("raw_data/gadm36_DEU_2_sf.rds")

##Outcome variable - how has the gap with western average changed?
#Diff to western average

western_average <- e0 %>%
  filter(East == "West") %>%
  group_by(year, Sex2) %>%
  summarise(western_e0 = mean(pct50))

western_average %>%
  group_by(Sex2) %>%
  summarise(diff = last(western_e0)-first(western_e0))

gap_to_western_average <- e0 %>%
  left_join(., western_average, by = c("year", "Sex2")) %>%
  mutate(e0_diff = pct50 - western_e0)

change_to_western_average <- gap_to_western_average %>%
  group_by(AGS, geo, Sex2, State_name, East) %>%
  summarise(e0_dd = last(e0_diff)-first(e0_diff))

change_to_western_average %>%
  filter(East == "West") %>%
  group_by(Sex2, State_name) %>%
  summarise(average = mean(e0_dd))

## Gap map ##

gap_map <- left_join(gap_to_western_average, map_file, by = c("AGS" = "CC_2"))

p5 <- gap_map %>%
  filter(year %in% c(1997, 2006, 2015)) %>%
  ggplot()+
  geom_sf(aes(fill = e0_diff, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(Sex2 ~ year)+
  scale_fill_gradient2()+
  labs(fill = "Gap to West German average life expectancy")+
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave("figures/p5.png", p5)

## Change relative to Western Germany map ##

change_to_western_map <- left_join(change_to_western_average, map_file, by = c("AGS" = "CC_2"))

p6 <- change_to_western_map %>%
  ggplot()+
  geom_sf(aes(fill = e0_dd, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(. ~ Sex2)+
  scale_fill_gradient2()+
  labs(fill = "Change in life expectancy compared to change in West German average")+
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave("figures/p6.png", p6)

## Regression analysis ##

#Hypotheses
#Effect of net outmigration of working aged individuals

migration <- readRDS("temp_data/migration.rds")

migration %>%
  ggplot(aes(x = year, y = st_net_rate, group = AGS))+
  geom_line()+
  facet_grid(age ~ .)

net_outmigration <- migration %>%
  group_by(AGS, geo, age) %>%
  summarise(st_net_rate = sum(st_net_rate))

early_outmigration <- migration %>%
  mutate(early = ifelse(year %in% c(1997, 2000, 2003, 2006), "Early", "Late")) %>%
  group_by(early, age, AGS, geo) %>%
  summarise(st_net_rate = sum(st_net_rate))

migration_map <- left_join(early_outmigration, map_file, by = c("AGS" = "CC_2"))

p7 <- migration_map %>%
  ggplot()+
  geom_sf(aes(fill = st_net_rate, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_gradient2()+
  labs(fill = "Net migration rate")+
  facet_grid(early ~ age)

ggsave("figures/p7.png", p7)

migration_model <- net_outmigration %>%
  left_join(change_to_western_average,. , by = c("AGS", "geo")) %>%
  mutate(e0_dd_bin = ifelse(sign(e0_dd) > 0, 1, 0),
         net_migration_bin = ifelse(sign(net_migration) > 0, "Inc", "Dec"),
         East_bin = ifelse(East == "West", F, T))

mig0 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin, data = migration_model, family = binomial)
mig1 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + East_bin, data = migration_model, family = binomial)
mig2 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin*East_bin, data = migration_model, family = binomial)
mig3 <- glm(e0_dd_bin ~ Sex2*net_migration_bin*East_bin, data = migration_model, family = binomial)

mig4 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + State_name, data = migration_model, family = binomial)

summary(mig0)
summary(mig1)
summary(mig2)
summary(mig3)
summary(mig4)

mm_mig0 <- glmer(e0_dd_bin ~ Sex2 + scale(net_migration) + East_bin+ (1|State_name), data = migration_model, family = binomial)
mm_mig1 <- glmer(e0_dd_bin ~ Sex2 + net_migration_bin + East_bin + (1|State_name), data = migration_model, family = binomial)

summary(mm_mig0)
summary(mm_mig1)

#Effect of healthcare availability

healthcare <- readRDS("temp_data/healthcare.rds")
pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  group_by(year, AGS, geo) %>%
  summarise(pop = sum(pop))

healthcare <- left_join(healthcare, pop, by = c("year", "AGS", "geo")) %>%
  mutate(hospitals_per = hospitals/pop,
         beds_per = beds/pop) %>%
  left_join(e0,., by =  c("year", "AGS", "geo"))

healthcare %>%
  ggplot(aes(x = year, y = beds_per, group = AGS, colour = East))+
  geom_line(size = 0.3, alpha = 0.3)

healthcare_change <- healthcare %>%
  group_by(AGS, geo) %>%
  summarise(hospitals_ave = mean(hospitals_per, na.rm = T),
            beds_ave = mean(beds_per, na.rm = T),
            hospitals_change = last(hospitals_per)-first(hospitals_per),
            beds_change = last(beds_per)-first(beds_per),
            hospitals_perc = hospitals_change / hospitals_ave,
            beds_perc = beds_change / beds_ave)

healthcare_change %>%
  ggplot()+
  geom_point(aes(x = hospitals_perc, y = beds_perc))

healthcare_map <- left_join(healthcare_model, map_file, by = c("AGS" = "CC_2"))

p8 <- healthcare_map %>%
  ggplot()+
  geom_sf(aes(fill = beds_change_bin, geometry = geometry), colour = "transparent") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_gradient2()+
  labs(fill = "Positive net change in hospital bed density")

ggsave("figures/p8.png", p8)

healthcare_model <- healthcare_change %>%
  left_join(change_to_western_average,. , by = c("AGS", "geo")) %>%
  mutate(e0_dd_bin = ifelse(sign(e0_dd) > 0, 1, 0),
         hospitals_change_bin = ifelse(sign(hospitals_change) > 0, "Inc", "Dec"),
         beds_change_bin = ifelse(sign(beds_change) > 0, 1, 0),
         East_bin = ifelse(East == "West", F, T))

hc2 <- glm(e0_dd_bin ~ Sex2 + hospitals_ave, data = healthcare_model, family = binomial)
hc3 <- glm(e0_dd_bin ~ Sex2 + hospitals_ave + East_bin, data = healthcare_model, family = binomial)

hc2 <- glm(e0_dd_bin ~ Sex2 + beds_ave, data = healthcare_model, family = binomial)
hc3 <- glm(e0_dd_bin ~ Sex2 + beds_ave + State_name, data = healthcare_model, family = binomial)

hc4 <- glm(e0_dd_bin ~ Sex2 + hospitals_change, data = healthcare_model, family = binomial)
hc5 <- glm(e0_dd_bin ~ Sex2 + hospitals_change + State_name, data = healthcare_model, family = binomial)

mm_hc5 <- glmer(e0_dd_bin ~ Sex2 + hospitals_change_bin + East_bin + (1 | State_name), data = healthcare_model, family = binomial)
summary(mm_hc5)

mm_hc6 <- glmer(e0_dd_bin ~ Sex2 + beds_change_bin + East_bin+ (1 | State_name), data = healthcare_model, family = binomial)
summary(mm_hc6)

hc6 <- glm(e0_dd_bin ~ Sex2 + beds_change, data = healthcare_model, family = binomial)
hc7 <- glm(e0_dd_bin ~ Sex2 + beds_change + State_name, data = healthcare_model, family = binomial)

summary(hc7)

hc8 <- glm(e0_dd_bin ~ Sex2 + hospitals_change_bin, data = healthcare_model, family = binomial)
hc9 <- glm(e0_dd_bin ~ Sex2 + hospitals_change_bin + East_bin, data = healthcare_model, family = binomial)
hc9a <- glm(e0_dd_bin ~ Sex2 + hospitals_change_bin*East_bin, data = healthcare_model, family = binomial)
hc9 <- glm(e0_dd_bin ~ Sex2 + hospitals_change_bin + State_name, data = healthcare_model, family = binomial)


hc10 <- glm(e0_dd_bin ~ Sex2 + beds_change_bin, data = healthcare_model, family = binomial)
hc11 <- glm(e0_dd_bin ~ Sex2 + beds_change_bin + East_bin, data = healthcare_model, family = binomial)
hc11a <- glm(e0_dd_bin ~ Sex2 + beds_change_bin*East_bin, data = healthcare_model, family = binomial)
hc12 <- glm(e0_dd_bin ~ Sex2 + beds_change_bin + State_name, data = healthcare_model, family = binomial)

summary(hc9)
summary(hc12)

mm_hc8 <- glmer(e0_dd_bin ~ Sex2 + hospitals_change_bin + (1 | State_name), data = healthcare_model, family = binomial)
mm_hc9 <- glmer(e0_dd_bin ~ Sex2 + hospitals_change_bin + East_bin + (1 | State_name), data = healthcare_model, family = binomial)

summary(mm_hc8)
summary(mm_hc9)

mm_hc10 <- glmer(e0_dd_bin ~ Sex2 + beds_change_bin + (1 | State_name), data = healthcare_model, family = binomial)
mm_hc11 <- glmer(e0_dd_bin ~ Sex2 + beds_change_bin + East_bin + (1 | State_name), data = healthcare_model, family = binomial)

summary(mm_hc10)
summary(mm_hc11)

complete_model <- left_join(migration_model, healthcare_model)

c0 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + hospitals_change_bin, data = complete_model, family = binomial)
c1 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + hospitals_change_bin + East_bin, data = complete_model, family = binomial)
c2 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + hospitals_change_bin + State_name, data = complete_model, family = binomial)

summary(c0)
summary(c1)
summary(c2)
  
c3 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + beds_change_bin, data = complete_model, family = binomial)
c4 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + beds_change_bin + East_bin, data = complete_model, family = binomial)
c5 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + beds_change_bin + State_name, data = complete_model, family = binomial)

summary(c3)
summary(c4)
summary(c5)

mm_c2 <- glmer(e0_dd_bin ~ Sex2 + net_migration_bin + hospitals_change_bin + East_bin + (1 | State_name), data = complete_model, family = binomial)
mm_c5 <- glmer(e0_dd_bin ~ Sex2 + net_migration_bin + beds_change_bin + East_bin + (1 | State_name), data = complete_model, family = binomial)

summary(mm_c2)
summary(mm_c5)

c6 <- glm(e0_dd_bin ~ Sex2 + net_migration_bin + hospitals_change_bin + beds_change_bin + State_name, data = complete_model, family = binomial)
summary(c6)

mm_c6 <- glmer(e0_dd_bin ~ Sex2 + scale(net_migration) + scale(hospitals_change) + scale(beds_change) + (1 | State_name), data = complete_model, family = binomial)
summary(mm_c6)
