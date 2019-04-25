library(raster)
library(sf)
library(dplyr)
library(mapview)

# Need a table of site locations
# Here I use a random sample of 10 Chicago sites as an example
# Data avaliable in same github repo
# Columns: LocationName, UTM_E, UTM_N, UTMZone, City
site_coords <- read.csv("2019-04-25_SampleChicagoSites.csv", stringsAsFactors = FALSE)

# create spatial points
# You must put the correct CRS for respective city
sites <- st_as_sf(site_coords, coords = c("UTM_E", "UTM_N"), crs = 26916)

# Visually inspect points to ensure the projection and location is correct
mapview(sites)


avaliableHabitat_NLCD <- function(points, buffer_){

  # Load 2010 national lancover data - REPLACE FILE PATH WITH LOCAL FILE PATH
  # Data can be downloaded from https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover
  lc_map <- raster("~/Documents/GIS/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")
  
  # Reproject points to mach landcover raster
  points_RP <- st_transform(points, crs = st_crs(lc_map))

  # Extract land cover data for each point, given a fixed radius buffer
  # This returns a list
  # Each list element contains the land cover catergory of each raster cell 
    # within the respective buffer feature
  lc_extract <- extract(lc_map, points_RP, buffer = buffer_)

  # Now we have a list element for each buffer/site 
  # We will summarize the information within each list element seperately
  # Then do it all at once with lapply
  summarizeAvaliableHabitat <- function(x){
    
    # Create a table of proportions for each land cover class
    proportions <- prop.table(table(x))
    
    # Summarize each site's data by proportion of each cover type
    # Convert to data frame
    landcover <- data.frame(cover = names(proportions), percent = as.numeric(proportions))
    
    # Sum across categories that we are considering habitat
    # Legend: https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend
    habitat <- sum(landcover[which(landcover$cover == 21),2], # developed open space
                   landcover[which(landcover$cover == 41),2], # forest
                   landcover[which(landcover$cover == 42),2], # forest
                   landcover[which(landcover$cover == 43),2], # forest
                   landcover[which(landcover$cover == 51),2], # shrub
                   landcover[which(landcover$cover == 52),2], # shrub
                   landcover[which(landcover$cover == 71),2], # herbaceous
                   landcover[which(landcover$cover == 72),2], # herbaceous
                   landcover[which(landcover$cover == 73),2], # herbaceous
                   landcover[which(landcover$cover == 74),2], # herbaceous
                   landcover[which(landcover$cover == 90),2], # wetland
                   landcover[which(landcover$cover == 95),2]) # wetland
    
    return(habitat)
    
  }
  
  # Loop through each list element and calculate the proportion of habitat
  # lapply returns a list so we use do.call to collapse the list into a vector
  habitat <- do.call(c, lapply(lc_extract, summarizeAvaliableHabitat))
  
  # Combine the data with Location Name
  avaliable_habitat <- data.frame(LocationName = points_RP$LocationName,
                                  avail_habitat = habitat)
  
  return(avaliable_habitat)
  
}

# Run the function to extract proportion of avaliable habitat around each site
# Here we use the Chicago data as an example and a 1km-radius buffer
avail_habitat_example <- avaliableHabitat_NLCD(sites, 1000)
  
  