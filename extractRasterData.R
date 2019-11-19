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

# We will use the national impervious cover map for this example
#  Data can be downloaded from:
#  browseURL("https://www.mrlc.gov/data?f%5B0%5D=category%3Aurban%20imperviousness")
#  Load impervious cover raster map
# REPLACE FILE PATH WITH LOCAL FILE PATH

my_raster_path <- 
  "../../GIS/NLCD_2016_percent_impervious/NLCD_2016_Impervious_L48_20190405.img"

# Check if the file exists, if not provide a warning.
check_path(my_raster_path)

# read it in
imp_map <- raster::raster(my_raster_path)

# Function to extract the mean value of raster cells within a given buffer
# Parameters
  # points: the spatial points created above
  # raster_data: a spatial raster with values in Band 1
  # buffer_: the radius of a buffer (m) that you want to extract data from around each site

extract_raster_data <- function(my_points, my_buffer, my_raster_data){
  # This function has 3 arguments:
  #  my_points (sf object): The coordinates  of thesites you want to collect
  #                           spatial data from.
  #  my_buffer (numeric):   The radius of a buffer around each point form which
  #                           to extract cell values. If the distance between 
  #                           the sampling point and the center of a cell is 
  #                           less than or equal to the buffer, the cell is 
  #                           included. The buffer can be specified as a single
  #                           value, or a vector of the length of the number of
  #                           points. If the data are not projected (e.g.,
  #                           latitude/longitude), unit should be in meters.
  #                           Otherwise it should be in map units, which is also
  #                           typically meters.
  #  my_raster_data (raster): A raster object that you want to extract data 
  #                             from.
  # start command line reporting
  cat(
    paste0(
      cli::rule(line = 2),
      "\n\n"
    )
  )
  # Step 1. reproject the point data to match raster projection
  .cli_post("Reprojecting my_points to raster projection:")
  points_RP <- sf::st_transform(
    my_points, 
    sf::st_crs(my_raster_data)
  )
  .cli_post(pass = TRUE)
  
  # Step 2.
  # use the raster::extract function to extract the mean value of the raster data 
  # within a particular buffer around each site.
  .cli_post("Extracting spatial data:")
  mean_extract <- raster::extract(
    my_raster_data,
    points_RP,
    fun=mean,
    buffer= my_buffer,
    df=TRUE
  )
  .cli_post(pass = TRUE)
  
  # create dataframe matching the sites with the extracted data
  .cli_post("Summarizing data:")
  df <- data.frame(
    LocationName = points_RP$LocationName,
    mean_value = mean_extract[,names(my_raster_data)]
  )
  .cli_post(pass = TRUE)
  # end command line reporting
  cat(
    paste0(
      "\n",
      cli::rule(line = 2),
      "\n\n"
    )
  )
  return(df)
}

# run function to extract the mean parameter value around each site
# For this example we extract the mean impervious cover within a 1km radius buffer
mean_imp <- extract_raster_data(sites, 1000, imp_map)

npix <- (pi * 1^2) / .3^2


