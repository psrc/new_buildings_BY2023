library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(tidyverse)
library(data.table)
library(leaflet.extras)
library(geoshaper)
library(sp)
library(DT)

# Shiny Server settings
rund <- "/media/aws-prod-file01modeldata/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
rund2 <- "/media/aws-prod-file01modeldata2/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
base.ind.dir <- list(LAws01 = file.path(rund, "awsmodel01"),
               LAws02 = file.path(rund, "awsmodel02"),
               LAws03 = file.path(rund, "awsmodel03"),
               LAws04 = file.path(rund, "awsmodel04"),
               LAws05 = file.path(rund, "awsmodel05"),
               LAws06 = file.path(rund, "awsmodel06"),
               LAws07 = file.path(rund, "awsmodel07"),
               LAws08 = file.path(rund, "awsmodel08"),
               NAws01 = file.path(rund2, "awsmodel01"),
               NAws02 = file.path(rund2, "awsmodel02"),
               NAws03 = file.path(rund2, "awsmodel03"),
               NAws04 = file.path(rund2, "awsmodel04"),
               NAws05 = file.path(rund2, "awsmodel05"),
               NAws06 = file.path(rund2, "awsmodel06"),
               NAws07 = file.path(rund2, "awsmodel07"),
               NAws08 = file.path(rund2, "awsmodel08"),
			Modelsrv5 = "/media/modelsrv5d/opusgit/urbansim_data/data/psrc_parcel/runs",
                     Modelsrv6 = "/media/modelsrv6d/opusgit/urbansim_data/data/psrc_parcel/runs",
                     Modelsrv8 = "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs",
                     Modelsrv3 = "/media/modelsrv3e/opusgit/urbansim_data/data/psrc_parcel/runs"
)
# base.ind.dir <- list(Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#                      Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#                      Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#                      Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
#              )
#base.ind.dir <- "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "/Users/hana/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
#base.ind.dir <- "~/tmpind"

wrkdir <- '/home/shiny/apps/' # shiny path
#wrkdir <- '/Users/hana/R/shinyserver/'
#wrkdir <- '/Users/hana/psrc/R/shinyserver'
# wrkdir <- 'C:/Users/CLam/Desktop/'

# scan for all directories in servers
allruns <- list()
for (b in 1:length(base.ind.dir)) {
  fdirlist <- list.dirs(base.ind.dir[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base.ind.dir[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  allruns[[length(allruns)+1]] <- dirlist
}
names(allruns) <- names(base.ind.dir) %>% toupper

bld.years <- c(2040, 2050)
bld.filename <- paste0("building__dataset_table__new_buildings__", bld.years, ".tab") %>%
  paste(sep = "", collapse = "|")

data <- 'parcel-viewer/data'
bld.data <- "new_buildings/data"

parcel.main <- 'parcels2014.rds'
parcel.att <- 'parcels_for_viewer.rds'

parcels <- data.table(readRDS(file.path(wrkdir, data, parcel.main)))
setkey(parcels, parcel_id)
attr <- data.table(readRDS(file.path(wrkdir, data, parcel.att)))
setkey(attr, parcel_id)

parcels <- attr %>% merge(parcels, all.x=TRUE)
parcels <- parcels[,-c(20:35)] # remove a few attributes to reduce size
parcels[, c("max_dua", "max_far", "building_sqft") := NULL] # remove these columns as they'll come from the plan_types table

rm(attr)

building_types <- read.csv(file.path(wrkdir, bld.data, "building_types.csv"), stringsAsFactors = FALSE)[,c("building_type_id", "building_type_name")]
ordered_building_type_names <- c("single_family_residential", "condo_residential", "multi_family_residential", 
                                 "commercial", "office", "industrial", "warehousing", "tcu")
building_types_selection <- subset(building_types, building_type_name %in% ordered_building_type_names)
rownames(building_types_selection) <- building_types_selection$building_type_name
building_types_selection <- building_types_selection[ordered_building_type_names,"building_type_id", drop=FALSE]
building_types <- data.table(building_types)
setkey(building_types, building_type_id)
color.attributes <- c("year"="year_built", "bt"="building_type_id", 
                      "sizeres"="residential_units.x", "sizenonres"="non_residential_sqft")

########
# Create a dataset of plan types from costraints table
########
constr <- fread(file.path(wrkdir, bld.data, "development_constraints.csv"))
setkey(constr, plan_type_id)

# Create tables of residential and non-res constraints that 
# count the number of constraints and get their minimum and maximum
resconstr <- constr[constraint_type == "units_per_acre", 
                    .(N_res_con=.N, max_dua = max(maximum), min_dua=min(minimum),
                      allow_mfr=any(generic_land_use_type_id == 2)),
                    by= plan_type_id]

nonresconstr <- constr[constraint_type == "far", 
                       .(N_nonres_con=.N, max_far = max(maximum), min_far=min(minimum),
                         allow_off=any(generic_land_use_type_id == 3),
                         allow_com=any(generic_land_use_type_id == 4)),
                       by= plan_type_id]

# Outer join of the two tables by plan_type_id
plantypes <- merge(resconstr, nonresconstr, all = TRUE)
# replace NAs with 0s
plantypes[is.na(N_res_con), N_res_con:=0]
plantypes[is.na(N_nonres_con), N_nonres_con:=0]
plantypes[is.na(allow_mfr), allow_mfr:=FALSE]
plantypes[is.na(allow_off), allow_off:=FALSE]
plantypes[is.na(allow_com), allow_com:=FALSE]

# select only mix-use plan types
plantypes <- plantypes[N_res_con > 0 & N_nonres_con > 0]

# Merge plan types with parcels table
setkey(parcels, plan_type_id)
setkey(plantypes, plan_type_id)

parcels <- merge(parcels, plantypes, all.x = TRUE)
setkey(parcels, parcel_id)

#### Adapted from https://redoakstrategic.com/geoshaper/ ---------------------
parcels$secondLocationID <- paste(as.character(parcels$parcel_id), "_selectedLayer", sep = "")
parcels[is.na(long), long := 0]
parcels[is.na(lat), lat := 0]
coordinates <- SpatialPointsDataFrame(parcels[,c('long', 'lat')], parcels)


