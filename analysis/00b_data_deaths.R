rm(list=ls())

library(tidyverse)

#Death counts

deaths <- read.csv("raw_data/deaths/death_counts.csv", header = FALSE, sep = ";", stringsAsFactors = F,
                   encoding = "latin1", na.strings = c("-", "."), skip = 7) %>%
  slice(., 1:(n()-4)) %>% 
  select(year = V1, AGS = V2, geo = V3, age = V4, male_deaths = V6, female_deaths = V7) %>%
  mutate(AGS = case_when(AGS == "02" ~ "02000",
                         AGS == "11" ~ "11000",
                         TRUE ~ AGS),
         year = as.numeric(year),
         geo = trimws(geo)) %>%
  filter(str_length(AGS) == 5 | AGS =="05334002") %>%
  pivot_longer(cols = male_deaths:female_deaths, names_to = "sex", names_pattern = "(.*)_deaths", values_to = "deaths")

#Implementing reforms

reformed_areas <- c("13071", "13072", "13076", "13074", "13073", "13075", "15001", "15086", "15083",  "15089", 
                    "15085", "15087", "15091", "15082", "15088", "15084", "05334", "03159")

redundant_areas <- c("15303", "15202", "15370", "15363", "14272", "14292", "14264",
                     "14171", "14191", "14188", "14181", "14284", "14286", "14263",
                     "14379", "14383", "14280", "14285", "14182", "14177", "14375",
                     "14374", "14389", "14287", "14290", "14178", "14166", "14173",
                     "14193", "14167", "14161", "14262", "14365", "13002", "13056", 
                     "13055", "13052", "13051", "13053", "13060", "13054", "13058", 
                     "13006", "13005", "13057", "13061", "13001", "13059", "13062",
                     "15101", "15358", "15362", "15355", "15153", "15352", "15367",
                     "15357", "15369", "15364", "15171", "15151", "15159", "15154",
                     "15265", "15261", "15256", "15268", "05354", "05334002", "15266",
                     "15260", "03152", "03156")

deaths_reformed <- deaths %>%
  mutate(code_new = case_when(AGS %in% c("13002", "13056", "13055", "13052") ~ "13071",
                              AGS %in% c("13051", "13053") ~ "13072",
                              AGS %in% c("13060", "13054") ~ "13076",
                              AGS %in% c("13058", "13006") ~ "13074",
                              AGS %in% c("13005", "13057", "13061") ~ "13073",
                              AGS %in% c("13001", "13059", "13062") ~ "13075",
                              AGS %in% c("15101") ~ "15001",
                              AGS %in% c("15358") ~ "15086",
                              AGS %in% c("15362", "15355") ~ "15083",
                              AGS %in% c("15153", "15352", "15367") ~ "15089",
                              AGS %in% c("15357", "15369", "15364") ~ "15085",
                              AGS %in% c("15171") ~ "15091",
                              AGS %in% c("15151", "15159", "15154") ~ "15082",
                              AGS %in% c("15266", "15260") ~ "15087",
                              AGS %in% c("15265", "15261") ~ "15088",
                              AGS %in% c("15256", "15268") ~ "15084",
                              AGS %in% c("05354", "05334002") ~ "05334",
                              AGS %in% c("03152", "03156") ~ "03159",
                              TRUE ~ AGS)) %>%
  group_by(year, code_new, age, sex) %>%
  mutate(deaths_new = ifelse((is.na(deaths) & AGS %in% reformed_areas), sum(deaths, na.rm = T), deaths)) %>% 
  filter(!(AGS %in% redundant_areas)) %>%
  select(year, AGS = code_new, geo, age, sex, deaths = deaths_new)

#Aligning age groups

deaths <- deaths_reformed %>%
  mutate(age = case_when(age %in% c("unter 1 Jahr", "1 bis unter 5 Jahre", "5 bis unter 10 Jahre") ~ 0,
                             age %in% c("10 bis unter 15 Jahre") ~ 10,
                             age %in% c("15 bis unter 20 Jahre") ~ 15,
                             age %in% c("20 bis unter 25 Jahre") ~ 20,
                             age %in% c("25 bis unter 30 Jahre") ~ 25,
                             age %in% c("30 bis unter 35 Jahre") ~ 30,
                             age %in% c("35 bis unter 40 Jahre") ~ 35,
                             age %in% c("40 bis unter 45 Jahre") ~ 40,
                             age %in% c("45 bis unter 50 Jahre") ~ 45,
                             age %in% c("50 bis unter 55 Jahre") ~ 50,
                             age %in% c("55 bis unter 60 Jahre") ~ 55,
                             age %in% c("60 bis unter 65 Jahre") ~ 60,
                             age %in% c("65 bis unter 70 Jahre", "70 bis unter 75 Jahre") ~ 65,
                             age %in% c("75 bis unter 80 Jahre", "80 bis unter 85 Jahre", "85 Jahre und mehr") ~ 75,
                            age == "Insgesamt" ~ 999)) %>%
  group_by(year, AGS, geo, age, sex) %>%
  summarise(deaths = sum(deaths, na.rm = T))

## Redistributing deaths

deaths_sum <- deaths %>%
  group_by(year, AGS, geo, sex) %>%
  filter(age != 999) %>%
  summarise(Sum = sum(deaths))

deaths_total <- deaths %>%
  filter(age == 999)

deaths_diff <- left_join(deaths_sum, deaths_total, by = c("year", "AGS", "geo", "sex")) %>%
  mutate(deaths_75 = deaths - Sum) %>%
  select(year, AGS, geo, sex, deaths_75)

deaths_imputed <- deaths %>%
  filter(age != 999) %>%
  left_join(., deaths_diff, by = c("year", "AGS", "geo", "sex")) %>%
  mutate(deaths = ifelse((age == 75 & deaths == 0), deaths_75, deaths),
         age = as.numeric(age)) %>%
  select(year, AGS, geo, age, sex, deaths)

## Pooling deaths

deaths_grouped <- deaths_imputed %>%
  mutate(year = case_when(year %in% 1996:1998 ~ 1997,
                          year %in% 1999:2001 ~ 2000,
                          year %in% 2002:2004 ~ 2003,
                          year %in% 2005:2007 ~ 2006,
                          year %in% 2008:2010 ~ 2009,
                          year %in% 2011:2013 ~ 2012,
                          year %in% 2014:2016 ~ 2015)) %>%
  group_by(year, AGS, geo, age, sex) %>%
  filter(year %in% 1997:2015) %>%
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  ungroup()

saveRDS(deaths_grouped, file = "temp_data/deaths.rds")

