rm(list=ls())

#Migration dataset

migration <- data.frame()

for (i in 1995:2016) {
  temp <- read.csv(paste0("raw_data/migration/12711-04-02-4 (",i,").csv"), header = FALSE, sep = ";", stringsAsFactors = F,
                   encoding = "latin1", na.strings = c("-", "."), skip = 10)
  temp$year <- i
  migration <- rbind(migration, temp)
  print(i)
}

migration <- migration %>%
  select(year, AGS = V1, geo = V2, age = V3, in_total = V4, out_total = V7) %>%
  mutate(AGS = case_when(AGS == "02" ~ "02000",
                         AGS == "11" ~ "11000",
                         TRUE ~ AGS),
         geo = trimws(geo)) %>%
  filter(str_length(AGS) == 5 | AGS %in% c("02", "11", "05334002"))

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

migration_reformed <- migration %>%
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
  group_by(year, code_new, age) %>%
  mutate(in_total = ifelse(is.na(in_total) & AGS %in% reformed_areas, sum(in_total, na.rm = T), in_total),
         out_total = ifelse(is.na(out_total) & AGS %in% reformed_areas, sum(out_total, na.rm = T), out_total)) %>% 
  filter(!(AGS %in% redundant_areas)) %>%
  select(year, AGS = code_new, geo, age, in_total, out_total)

migration <- migration_reformed %>%
  mutate(year = case_when(year %in% 1996:1998 ~ 1997,
                          year %in% 1999:2001 ~ 2000,
                          year %in% 2002:2004 ~ 2003,
                          year %in% 2005:2007 ~ 2006,
                          year %in% 2008:2010 ~ 2009,
                          year %in% 2011:2013 ~ 2012,
                          year %in% 2014:2016 ~ 2015)) %>%
  filter(year %in% 1997:2015) %>%
  mutate(age = case_when(age %in% c("25 bis unter 30 Jahre", "30 bis unter 50 Jahre") ~ 25,
                         age == "50 bis unter 65 Jahre" ~ 50,
                         age == "65 Jahre und mehr" ~ 65)) %>%
  filter(age %in% c(25, 50, 65)) %>%
  group_by(year, AGS, geo, age) %>%
  summarise(in_total = sum(in_total),
            out_total = sum(out_total)) %>%
  ungroup()

pop <- readRDS("temp_data/mid_year_pop.rds") %>%
  filter(age %in% c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75)) %>%
  mutate(age = case_when(age %in% c(25, 30, 35, 40, 45) ~ 25,
                         age %in% c(50, 55, 60) ~ 50,
                         age %in% c(65, 70, 75) ~ 65)) %>%
  group_by(year, AGS, geo, age) %>%
  summarise(pop = sum(pop))

ESP <- data.frame(age = c(25, 50, 65), ESP = c(33500, 19500, 19500))

net_outmigration <- migration %>%
  left_join(., pop, by = c("year", "AGS", "geo", "age")) %>%
  mutate(net = in_total - out_total,
         net_rate = net/pop) %>%
  left_join(., ESP, by = ("age")) %>%
  mutate(st_net_rate = net_rate*ESP)

saveRDS(net_outmigration, "temp_data/migration.rds")
