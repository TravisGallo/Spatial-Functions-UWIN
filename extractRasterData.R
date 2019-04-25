library(raster)
library(doParallel)
library(sf)
library(dplyr)
library(mapview)

# Need a table of site locations
# Here I use a random sample of 10 Chicago sites as an example
# Data avaliable in same github repo
# Columns: LocationName, UTM_E, UTM_N, UTMZone, City
site_coords <- read.csv("2019-04-25_SampleChicagoSites.csv", stringsAsFactors = FALSE)

# create spatial points
# you must put the correct CRS for respective city
sites <- st_as_sf(site_coords, coords = c("UTM_E", "UTM_N"), crs = 26916)

# Visually inspect points to ensure the projection and location is correct
mapview(sites)

# We will use the national impervious cover map for this example
# Data can be downloaded from https://www.mrlc.gov/data?f%5B0%5D=category%3Aurban%20imperviousness
# Load impervious cover raster map - REPLACE FILE PATH WITH LOCAL FILE PATH
imp_map <- raster("~/Documents/GIS/nlcd_2011_impervious_2011_edition_2014_10_10/nlcd_2011_impervious_2011_edition_2014_10_10.img")

# Function to extract the mean value of raster cells within a given buffer
# Paramters
  # points: the spatial points created above
  # raster_data: a spatial raster with values in Band 1
  # buffer_: the radius of a buffer (m) that you want to extract data from around each site

extractRasterData <- function(points, raster_data, buffer_){
  
  # reproject the point data to match raster projection
  points_RP <- st_transform(points, st_crs(raster_data))
  
  # use the raster::extract function to extract the mean value of the raster data 
  # within a particular buffer around each site
  mean_extract <- extract(raster_data, points_RP, fun=mean, buffer= buffer_, df=TRUE)
  
  # create dataframe matching the sites with the extracted data
  df <- data.frame(LocationName = points_RP$LocationName, mean_value = mean_extract[,2])
  
  return(df)
  
}

# run function to extract the mean parameter value around each site
# For this example we extract the mean impervious cover within a 1km radius buffer
mean_imp <- getRasterData(sites, imp_map, 1000)

  