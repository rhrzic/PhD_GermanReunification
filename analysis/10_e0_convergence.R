rm(list=ls())

require(tidyverse)
source("analysis/two-stage_decomp.R")

e0 <- readRDS("temp_data/e0.RDS")

e0 %>%
  filter(year %in% c(1997, 2015)) %>%
  group_by(year, Sex2, East) %>%
  summarise(east_ave = mean(pct50),
            east_range_low = range(pct50)[1],
            east_range_high = range(pct50)[2])

p2 <- e0 %>%
  group_by(year, Sex2, East) %>%
  mutate(east_ave = mean(pct50)) %>%
  ggplot()+
  geom_line(aes(x = year, y = pct50, color = East, group = AGS), alpha = 0.3, size = 0.15)+
  geom_line(aes(x = year, y = east_ave, color = East, group = East), size = 1)+
  facet_wrap(~ Sex2, scales = "free_y")+
  theme_bw()+
  labs(color = "Part of Germany")+
  xlab("Year")+
  scale_x_continuous(breaks = c(2000, 2006, 2012), labels = c("1999-01", "2005-07", "2011-13"))+
  ylab("Life expectancy at birth")+
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave("figures/p2.png", p2)

## Theil coefficient decomposition (between East-West and within)

e0 %>%
  group_by(Sex2, year) %>%
  do(onestage.Theil.decomp(df = ., macroregion = "East", ple = "pct50")) %>%
  filter(year %in% c(1997, 2015)) %>%
  group_by(Sex2) %>%
  summarise(Tbetween_diff = (last(Tbetween) - first(Tbetween)) / first(Tbetween),
            Twithin_diff = (last(Twithin) - first(Twithin)) / first(Twithin),
            Ttotal_diff = (last(Ttotal) - first(Ttotal)) / first(Ttotal))

e0 %>%
  group_by(Sex2, year) %>%
  do(onestage.Theil.decomp(df = ., macroregion = "East", ple = "pct50")) %>%
  filter(year %in% c(1997, 2015)) %>%
  group_by(Sex2) %>%
  summarise(Tbetween_prop = Tbetween / Ttotal,
            Twithin_prop = Twithin / Ttotal)

p3 <- e0 %>%
  group_by(Sex2, year) %>%
  do(onestage.Theil.decomp(df = ., macroregion = "East", ple = "pct50")) %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = Ttotal, color = "All inequality"))+
  geom_line(aes(y = Tbetween, color = "Between East and West Germany"))+
  geom_line(aes(y = Twithin, color = "Within East and West Germany"))+
  facet_grid(. ~ Sex2)+
  theme_bw()+
  labs(x = "Year",
       y = "Theil coefficient",
       color = "")+
  scale_color_manual(values = c("All inequality" = "black", "Between East and West Germany" = "red", 
                                "Within East and West Germany" = "green")) +
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave("figures/p3.png", p3)

#Explaining the east-west convergence

p4 <- e0 %>%
  group_by(AGS, Sex2) %>%
  mutate(e0_change = pct50 - first(pct50)) %>%
  ggplot(aes(x = year, y = e0_change, group = AGS, colour = East))+
  geom_line(alpha = 0.3)+
  facet_grid(Sex2 ~ .)+
  theme_bw()+
  labs(color = "Part of Germany")+
  xlab("Year")+
  ylab("Change in life expectancy since 1996-1998")

ggsave("figures/p4.png", p4)

