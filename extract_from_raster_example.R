##############################################
#
# Example of extracting from landuse/landcover
#
# Written by T. Gallo and M. Fidino
#
##############################################

library(raster)
library(sf)
library(dplyr)
library(mapview)
library(cli)

# load in utility functions.
source("spatial_utilities.R")

# Need a table of site locations
# Here I use a random sample of 10 Chicago sites as an example
# Data avaliable in same github repo
# Columns: LocationName, UTM_E, UTM_N, UTMZone, City
site_coords <- read.csv(
  "2019-04-25_SampleChicagoSites.csv",
  stringsAsFactors = FALSE
)

# Create spatial points
# You must put the correct CRS for respective city
sites <- sf::st_as_sf(
  site_coords,
  coords = c("UTM_E", "UTM_N"),
  crs = 26916
  )

# Visually inspect points to ensure the projection and location is correct
mapview(sites)

# We will use the High-res Landcover for NE Illinois for this example
#  Data can be downloaded from:
#  browseURL("https://datahub.cmap.illinois.gov/dataset/high-resolution-land-cover-ne-illinois-and-nw-indiana-2010")
#  Load iLULC map
# REPLACE FILE PATH WITH LOCAL FILE PATH

my_raster_path <- 
  "../../GIS/cmap/landcover_2010_chicagoregion.img"

# Check if the file exists, if not provide a warning.
check_path(my_raster_path)

# read it in
my_map <- raster::raster(my_raster_path)

#  For this example we will extract the proportion canopy cover (lulc class 1)
#    and create our own 'impervious cover' value, which is the sum of multiple
#    lulc classes.
lulc_prop <- extract_raster_prop(my_points = sites,
                                 my_buffer = 1000,
                                 my_raster_data = my_map,
                                 lulc_cats = list("tree" = 1,
                                                  "imperv" = 5:7
                                                 ),
                                 point_names = sites$LocationName
)
