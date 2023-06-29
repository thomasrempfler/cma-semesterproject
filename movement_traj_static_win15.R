
# Project: Computational Movement Analysis - Red deer

# Script Purpose: Prepare Migration Segments

# Date: 25.05.2023

# Update: 13.06.2023

# Author: Christian Rossi, Sven Buchmann, Thomas Rempfler

# rm(list=ls())

log_file <- "Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/data_work/logfile15.txt"
log_conn <- file(log_file, open = "wt")

sink(log_conn,append = TRUE,type = "message")

library(tidyverse)
library(data.table)
library(sf)
library(lubridate)


# load df as sf
locs_win <- fread("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/origdata/locs_win.csv") %>% 
  st_as_sf(coords = c("x", "y"), crs = 2056, remove = FALSE)


## find moving and static phases in trajectories ----

# use cma_movement.bat to run code parallel with 40 cores

# n umber of tile each session calculates
arg <- commandArgs(trailingOnly = FALSE) # command line argument
k <- as.numeric(arg[6])
k

n.parallel <- split(seq(1:length(unique(locs_win$dep_num))), sort(seq(1:length(unique(locs_win$dep_num))) %% 40))

locs_win <- locs_win %>% filter(dep_num %in% n.parallel[[k]])


# create output table
output <- data.frame(loc_nr = numeric(),
                     deployment = logical(),
                     dep_nr = numeric(),
                     t = POSIXct(),
                     t_min15 = POSIXct(),
                     t_plus15 = POSIXct(),
                     x = numeric(),
                     y = numeric(),
                     in_range = logical(),
                     stepMean30 = numeric())



# loop through deployments and locs
for (d in unique(locs_win$dep_num)){
  
  locs_win_filter <- locs_win %>%
    filter(dep_num == d) %>%
    mutate(t_range = interval(min(t), max(t)),
           in_range = case_when(t_min15 %within% t_range &
                                  t_plus15 %within% t_range ~ TRUE,
                                TRUE ~ FALSE)) %>%
  dplyr::select(-t_range)
  
  
  for (l in 1:nrow(locs_win_filter)){
    
    # filter for data of current loop run
    now <- locs_win_filter[l,]
    
    obs_win <-  locs_win_filter %>% 
      filter(t >= as.POSIXct(now$t_min15, tz = "UTC"),
             t <= as.POSIXct(now$t_plus15, tz = "UTC")) 
    
    # calculate and append stepMean30 to now
    now$stepMean30 <- st_distance(obs_win, now) %>% as.numeric() %>% mean()
    
    # append now to output
    output <- rbind(output, now)
    
  }
  
print("dep_num fertig")
  
}

fwrite(output, paste0("Q:/prjdata/zool/ungulate/ung_monitoring/telemetry/reddeer/behaviour/code/movement/migration/data_work/win15/output_", k,".csv"))

sink()
close(log_conn)