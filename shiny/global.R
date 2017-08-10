library(shiny)
library(shinythemes)
library(leaflet)
#library(dplyr)
#library(tidyverse)
#library(DT)
library(data.table)

base.ind.dir <- "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs"
base.ind.dir <- "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "/Users/hana/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "~/tmpind"
             

#wrkdir <- '/home/shiny/apps/' # shiny path
wrkdir <- '/Users/hana/R/shinyserver/'
#wrkdir <- '/Users/hana/psrc/R/shinyserver'
# wrkdir <- 'C:/Users/CLam/Desktop/'

data <- 'parcel-viewer/data'
bld.data <- "new_buildings/data"

parcel.main <- 'parcels2014.rds'
parcel.att <- 'parcels_for_viewer.rds'

parcels <- readRDS(file.path(wrkdir, data, parcel.main))
attr <- readRDS(file.path(wrkdir, data, parcel.att))

parcels.attr <- parcels %>% left_join(attr, by = "parcel_id")

rm(attr)
rm(parcels)

building_types <- read.csv(file.path(wrkdir, bld.data, "building_types.csv"), stringsAsFactors = FALSE)[,c("building_type_id", "building_type_name")]
