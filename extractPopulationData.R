library(raster)
library(sf)
library(dplyr)
library(mapview)

# Need a table of site locations
# Here I use a random sample of 10 Chicago sites as an example
# Data avaliable in same github repo
# Columns: LocationName, UTM_E, UTM_N, UTMZone, City
site_coords <- read.csv("2019-04-25_SampleChicagoSites.csv", stringsAsFactors = FALSE)

# Create spatial points
# You must put the correct CRS for respective city
sites <- st_as_sf(site_coords, coords = c("UTM_E", "UTM_N"), crs = 26916)

# Visually inspect points to ensure the projection and location is correct
mapview(sites)


# Function to extract the housing units and population data from census layer
# Parameters
  # points: the spatial points created above
  # buffer_: the radius of a buffer (m) to extract data from around each site

extractPopulationData <- function(points, buffer_){
  
  # Load 2010 statewide census data for housing and population
  # Data can be downloaded from http://silvis.forest.wisc.edu/data/housing-block-change/
  # REPLACE FILE PATH WITH LOCAL FILE PATH
  pop_data <- st_read("~/Documents/GIS/UWIN_GIS/state_pop10_shapefiles", 
                      layer= "il_blk10_PLA")
  
  # Reproject the point data to match projection of population layer
  points_RP <- st_transform(points, st_crs(pop_data))
  
  # Create a buffer around each site
  points_buffer <- st_buffer(points_RP, dist = buffer_)
  
  # Extract population layer features that are contained within each buffers
  pop_intersection <- st_intersection(points_buffer, pop_data)
  
  # Summarize the results to calculate housing units and housing density
  # Note that this will order results by LocationName
  housing <- pop_intersection %>%
    # remove spatial geometry so you are left with a data frame
    st_set_geometry(NULL) %>%
    # group by Location Name
    group_by(LocationName) %>%
    # sum all the intersected pieces to get total housing units in each buffer
    summarize(hu = sum(HU10)) %>%
    # divide by area of buffer (km) to get housing density
    mutate(hd = as.numeric(hu/(st_area(points_buffer)/1e6)))
  
  # Summarize the results to calculate population and population density
  # Note that this will order results by LocationName
  population <- pop_intersection %>%
    # remove spatial geometry so you are left with a data frame
    st_set_geometry(NULL) %>%
    # group by Location Name
    group_by(LocationName) %>%
    # sum all the intersected pieces to get total population in each buffer
    summarize(pop = sum(POP10)) %>%
    # divide by area of buffer (km) to get population density
    mutate(pd = as.numeric(pop/(st_area(points_buffer)/1e6)))
  
  # Combine data - note both ordered by LocationName
  df <- data.frame(cbind(housing, population[,2:3]))
  
  return(df)
  
}

# Run function to calculate housing units, housing density, population and population density
# For this example we extract population data within a 1km radius buffer
population_data <- extractPopulationData(sites, 1000)

