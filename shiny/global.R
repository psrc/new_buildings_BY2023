library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(tidyverse)
#library(DT)
library(data.table)

base.ind.dir <- "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "/Users/hana/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "~/tmpind"
             

wrkdir <- '/home/shiny/apps/' # shiny path
#wrkdir <- '/Users/hana/R/shinyserver/'
#wrkdir <- '/Users/hana/psrc/R/shinyserver'
# wrkdir <- 'C:/Users/CLam/Desktop/'

data <- 'parcel-viewer/data'
bld.data <- "new_buildings/data"

parcel.main <- 'parcels2014.rds'
parcel.att <- 'parcels_for_viewer.rds'

parcels <- readRDS(file.path(wrkdir, data, parcel.main))
attr <- readRDS(file.path(wrkdir, data, parcel.att))

parcels.attr <- parcels %>% left_join(attr, by = "parcel_id")
parcels.attr <- parcels.attr[,-c(1:14,16:17)] # remove a few attributes to reduce size

rm(attr)
rm(parcels)

building_types <- read.csv(file.path(wrkdir, bld.data, "building_types.csv"), stringsAsFactors = FALSE)[,c("building_type_id", "building_type_name")]
ordered_building_type_names <- c("single_family_residential", "condo_residential", "multi_family_residential", 
                                 "commercial", "office", "industrial", "warehousing", "tcu")
building_types_selection <- subset(building_types, building_type_name %in% ordered_building_type_names)
rownames(building_types_selection) <- building_types_selection$building_type_name
building_types_selection <- building_types_selection[ordered_building_type_names,"building_type_id", drop=FALSE]
color.attributes <- c("year"="year_built", "bt"="building_type_id", 
                      "sizeres"="residential_units.x", "sizenonres"="non_residential_sqft")
