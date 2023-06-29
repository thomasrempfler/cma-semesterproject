
# Project: Computational Movement Analysis - Red deer

# Script Purpose: Prepare Migration Segments

# Date: 25.05.2023

# Update: 24.06.2023

# Author: Thomas Rempfler 

rm(list=ls())

library(tidyverse)
library(data.table)
library(lubridate)
# library(raster)
library(amt)
library(move)
library(sf)
# library(sfheaders)
# library(ggplot2)
library(mapview)
# library(rgdal)
# library(scales)
# library(here)
# library(readr)
# 
# library(terra)
library(nominatimlite)

### connection to MySQL 
library(RMySQL)
library(DBI)
library(RMariaDB)
#library(graphics)

source ("C:/Users/trempfler/Documents/R/source.R") # run source


# load gps data
con <-
  dbConnect(
    RMariaDB::MariaDB(),
    user = user,
    host = host,
    port = port,
    password = password,
    dbname = dbname,
    bringt = bringt)

# read table from MySQL ----
rd <- dbReadTable(con, "rd") %>% 
  dplyr::select(ID, Animal_no, id_collar, Deployment, Sex, Area, x, y, 
                acquisition_time, int, year_birth, hindfoot, lower_jaw, 
                age_at_measure, longitude, latitude) %>%
  rename(t = acquisition_time, tag = id_collar, deployment = Deployment, 
         sex = Sex, id = ID, area = Area, lon = longitude, lat = latitude) %>%
  filter(year(t) >= 2010) %>% 
  # filter(int %in% c(1:4)) %>%
  # filter(hour==0|hour==3|hour==6|hour==9|hour==12|hour==15|
  #          hour==18|hour==21) %>%
  mutate(sex = as.factor(sex), tag = as.factor(tag),
         int = as.factor(int),id = as.factor(id)) %>%
  arrange(area, id, t)

rd_ste <- dbReadTable(con, "rd_ste") %>% 
  dplyr::select(ID, Animal_no, id_collar, Deployment, Sex, Area, x, y, 
                acquisition_time, int, year_birth, hindfoot, lower_jaw, 
                age_at_measure, longitude, latitude) %>%
  rename(t = acquisition_time, tag = id_collar, deployment = Deployment, 
         sex = Sex, id = ID, area = Area, lon = longitude, lat = latitude) %>%
  filter(year(t) >= 2010) %>% 
  # filter(hour==0|hour==3|hour==6|hour==9|hour==12|hour==15|
  #          hour==18|hour==21) %>%
  filter(deployment != "9908930_26020" & deployment != "9903915_26012") %>% # depl with neg. steplengths?!
  mutate(sex = as.factor(sex), tag = as.factor(tag),
         int = as.factor(int),id = as.factor(id)) %>%
  arrange(area, id, t)


# Disconnect from the database
dbDisconnect(con)


# gps locs ----
locs <- rbind(rd, rd_ste) %>% 
  mutate(study = case_when(area == "SNP" ~ "RSN",
                           area == "OCH" ~ "ASG",
                           area == "WAL" ~ "VAL",
                           TRUE ~ as.character(area))) %>% 
  filter(!id %in% c(20168, 21045, 23106, 25019, 26020, 26023, 26051)) %>% 
  arrange(area, deployment, t) %>% 
  mutate(study = as.factor(study),
         id = as.factor(id),
         loc_nr = row_number()) %>% 
  select(-area) %>% 
  st_as_sf(coords = c("x", "y"), crs = 2056, remove = FALSE)

fwrite(locs, "//marmota/gisdata/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/origdata/locs.csv")


# define window of t +/- 15 days an run through sf (+/- 4 minutes)
locs_win <- locs %>% 
  mutate(t_min15 = t - 15*24*3600 - 240,
         t_plus15 = t + 15*24*3600 + 240,
         # loc_nr = as.factor(loc_nr),
         deployment = as.factor(deployment), 
         dep_num = as.numeric(deployment)) %>% 
  select(loc_nr, deployment, dep_num, t, t_min15, t_plus15, x, y)

fwrite(locs_win,"//marmota/gisdata/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/origdata/locs_win.csv")


# load and extract covariates ----
ele <- terra::rast("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/phd/gis_pub/lv95/ele_lv95.tif")

ele_extract <- extract(ele, locs)

locs$ele <- ele_extract$ele_lv95


# find moving and static phases in trajectories ----

# see movement_traj_static_win15.R

# read static ----
lf1 <- list.files("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/data_work/win15"); lf1 # a list of all files in folder
locs_list1 <- lapply(paste("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/data_work/win15",lf1, sep="/"),
                     read.csv, header = T, sep = ",", dec = ".") 

summary(locs_list1)

# and visualize the first element of the list
locs_list1[1]

# convert the list in a data frame and check
data <- do.call("rbind", locs_list1) %>% 
  dplyr::select(loc_nr, dep_num, t_min15, t_plus15, in_range, stepMean30)
head(data,20)
  

# join to locs
locs_join <- locs %>%
  left_join(data, by = "loc_nr")

fwrite(locs_join,"//marmota/gisdata/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/origdata/locs_migration.csv",
       row.names = FALSE, sep = ";")


# 
# 
# # extract tracks ----
# # homeranges from of MigrO
# hr_ing <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/ING_270122_%_913m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "ING")
# hr_asg <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/OCH_270122_%_948m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "ASG")
# hr_rae <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/RAE_270122_%_862m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "RAE")
# hr_snp <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/SNP_270122_%_733m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "SNP")
# hr_ste <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/stelvio_all_%_792m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "STE")
# hr_tig <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/TIG_270122_%_711m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "TIG")
# hr_tir <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/TIR_270122_%_757m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "TIR")
# hr_val <- st_read("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/z_ext/msc_fellmann/gis_pub/WAL_270122_%_778m_5_30_000d_hulls.shp") %>% 
#   mutate(study = "VAL")
# 
# 
# hr_all <- rbind(hr_ing, hr_asg, hr_rae, hr_snp, hr_ste, hr_tig, hr_tir, hr_val) %>% 
#   mutate(id=as.factor(substr(animal, 1, 5)),
#          year=as.integer(substr(animal, 11, 14)),
#          id_year=paste0(id,"_",year)) %>% 
#   filter(!id %in% c(20168, 21045, 23106, 25019, 26020, 26023, 26051)) %>% 
#   st_transform(2056) %>% 
#   arrange(animal, start) %>% 
#   mutate(hr_nr = row_number())
# 
# 
# # hr starts and ends ----
# hr_start_end <- hr_all %>% 
#   select(hr_nr, id, start, end) %>% 
#   mutate(id = as.character(id),
#          start = as.POSIXct(start, tz = "UTC"),
#          end = as.POSIXct(end, tz = "UTC")) %>% 
#   filter(year(start) >= 2010)
# 
# # check2 <- hr_start_end %>% distinct(id)
# # diff_id1 <- setdiff(check1, check2) # without 21045, 23106, 25019, 26023, 26051 -> alle nur wenige locs
# check_hr1 <- hr_start_end %>% distinct(hr_nr)
# 
# 
# # join gps and hr to filter out gps during hr
# locs_hr <- locs %>% 
#   inner_join(hr_start_end, by = "id") %>% 
#   filter(t >= start & t <= end) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 2056)
# 
# # check3 <- locs_hr %>% distinct(id)
# # diff_id2 <- setdiff(check2, check3) # without 20168, 26020 -> ok
# check_hr2 <- locs_hr %>% distinct(hr_nr)
# diff_hr1 <- setdiff(check_hr1, check_hr2) 
# 
# test_hr20103 <- locs_hr %>% 
#   filter(id == 20103)
# test_hr20266 <- locs_hr %>% 
#   filter(id == 20266)
# test_hr21036 <- locs_hr %>% 
#   filter(id == 21036)
# test_hr <- hr_start_end %>% 
#   filter(hr_nr == 541)
# mapview(test_hr21036) + mapview(test_hr)
# 
# 
# # migration
# mig_between <- locs %>% 
#   filter(!loc_nr %in% locs_hr$loc_nr) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 2056)
# 
# # check4 <- mig_between %>% distinct(id)
# # diff_id3 <- setdiff(check1, check4)
# 
# 
# # find out real start and end of mig by spatial join to hr (because of U.'s random filtering of 1 loc per day)
# temp_start_end <- mig_between %>% 
#   st_join(hr_start_end, left = TRUE, suffix = c("_locs", "_hr")) %>% 
#   filter(id_locs == id_hr)
# 
# # check5 <- temp_start_end %>% mutate(id = id_locs) %>% distinct(id)
# # diff_id4 <- setdiff(check4, check5) # bei 21052 start/end exakt wie von U. randomly gewählt
# check_hr3 <- temp_start_end %>% distinct(hr_nr)
# diff_hr2 <- setdiff(check_hr1, check_hr3)
# 
# # test_hr <- temp_start_end %>% 
# #   filter(hr_nr ==103)
# # 
# # test_id <- locs %>% 
# #   filter(id == "20103") %>% 
# #   filter(t >= "2015-09-11 22:00:43" & t <= "2015-11-02 16:00:43") %>% 
# #   st_as_sf(coords = c("x", "y"), crs = 2056)
# # 
# # test_hr20103 <- hr_start_end %>% 
# #   filter(id == 20103)
# # 
# # mapview(test_hr20103) + mapview(test_id)
# 
# 
# real_start_end <- temp_start_end %>% 
#   filter(t >= (start - 86400) & t <= (end + 86400)) %>% 
#   group_by(id_locs, hr_nr) %>% 
#   mutate(start_corr = min(t),
#          end_corr = max(t))
# 
# # check6 <- real_start_end %>% mutate(id = id_locs) %>% distinct(id)
# 
# # # add missing hr of hr_start_end
# # hr_add <- sf_to_df(hr_start_end, fill = TRUE) %>% 
# #   select(-c(sfg_id, polygon_id, linestring_id, x, y)) %>% 
# #   filter(hr_nr %in% diff_hr2$hr_nr) %>% 
# #   rename(id_locs = id, start_corr = start, end_corr = end) %>% 
# #   distinct(id_locs, hr_nr, start_corr, end_corr)
# 
# hr_real <- real_start_end %>% 
#   group_by(id_locs, hr_nr) %>% 
#   distinct(id_locs, hr_nr, start_corr, end_corr) %>% ungroup() #%>% 
#   # bind_rows(hr_add)
# 
# check_hr4 <- hr_real %>% distinct(hr_nr)
# diff_hr3 <- setdiff(check_hr1, check_hr4)
# 
# # hr with real start and end date
# hr <- hr_start_end %>% 
#   left_join(hr_real, by = "hr_nr") %>% 
#   mutate(hr_start = case_when(start < start_corr ~ start,
#                                start > start_corr ~ start_corr,
#                                is.na(start_corr) ~ start),
#          hr_end = case_when(end > end_corr ~ end,
#                              end < end_corr ~ end_corr,
#                              is.na(end_corr) ~ end))
# 
# check_hr5 <- hr %>% distinct(hr_nr)
# diff_hr4 <- setdiff(check_hr1, check_hr5)
# 
# 
# # extract migration ----
# mig_temp <- locs %>% 
#   inner_join(hr, by = "id") %>% 
#   filter(t > hr_start & t < hr_end) 
# 
# mig <- locs %>% 
#   filter(!loc_nr %in% mig_temp$loc_nr) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 2056)
# 
# 
# 
# # corr 28./29. Feb and 1. Mar ----
# corr_date <- hr %>% 
#   mutate(y_start = year(start),
#          y_end = year(end)) %>%
#   filter(month(start) == 3, day(start) == 1,
#          month(end) == 2, day(end) %in% c(28, 29)) %>% 
#   mutate(corr_start = case_when(
#     month(hr_start) < 3 ~ as.character(paste(y_start,
#                                              "03-01 00:00:00",
#                                              sep = "-")),
#     TRUE ~ as.character(hr_start)),
#     corr_end = case_when(
#       month(hr_end) > 2 ~ as.character(paste(y_end,
#                                              "03-01 23:59:39",
#                                              sep = "-")),
#       TRUE ~ as.character(hr_end)),
#     corr_start = as.POSIXct(corr_start, tz = "UTC"),
#     corr_end = as.POSIXct(corr_end, tz = "UTC")) %>% 
#   mutate(end_def = case_when(month(corr_end) == 3 ~ corr_end - 86400,
#                              TRUE ~ corr_end)) %>% 
#   select(hr_nr, id, start, end, id_locs, start_corr, end_corr, 
#          corr_start, end_def, geometry) %>% 
#   rename(hr_start = corr_start, hr_end = end_def) 
# 
# 
# hr1 <- hr %>% 
#   filter(!hr_nr %in% corr_date$hr_nr) %>% 
#   bind_rows(corr_date)
# 
# 
# # extract migration
# mig_temp1 <- locs %>% 
#   inner_join(hr1, by = "id") %>% 
#   filter(t > hr_start & t < hr_end) 
# 
# mig1 <- locs %>%
#   filter(!loc_nr %in% mig_temp1$loc_nr) %>%
#   mutate(mig = 1) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 2056) %>% 
#   dplyr::select(loc_nr, mig)
# 
# 
# # test <- mig %>%
# #   filter(id == 20099) %>%
# #   filter(year(t) == 2015)
# # 
# # test_hr <- hr1 %>%
# #   filter(id == 20099) %>%
# #   filter(year(start) == 2015)
# # 
# # mapview(test) + 
# #   mapview(test_hr)
# 
# 
# # locs with mig info ----
# locs_mig <- locs %>% 
#   left_join(mig1, by = "loc_nr") %>% 
#   mutate(mig = case_when(is.na(mig) ~ 0,
#                          TRUE ~ mig))
# 
# 
# fwrite(locs_mig, "code/ssf/locs_mig.csv", row.names=FALSE, sep=";")
# 
# 
# # Berechnung von sl korrigiert für Höhenmodell ----
# 
# # 1. trajectories mit Punkten alle 30 m interpolieren (DEM 30 m)
# test <- locs_mig %>% 
#   filter(mig ==1, id == 20103)
#   
# test %>% 
#   st_as_sf() %>% 
#   group_by(id) %>% 
#   summarise(do_union = FALSE) %>%
#   st_cast("LINESTRING") -> test_traj
# 
# st_line_sample(test_traj, density = 1/30)
# 
# # 2. verschneiden mit DEM
# 
# 
# 
# 
# 
# 
# 
# # Stopover ----
# # ähnliche, aber separate Analyse für Stopover:
# # hr raus, die nicht über Sommer (Juli/August) und Winter (Jan/Feb) gehen,
# # sonst sieht man die Stauräume nicht mehr (= Stopover)
# 
# 

