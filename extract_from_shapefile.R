library(raster)
library(sf)
library(dplyr)
library(mapview)

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

# Load 2010 statewide census data for housing and population
# Data can be downloaded from:
# browseURL(http://silvis.forest.wisc.edu/data/housing-block-change/)
# REPLACE FILE PATH AND LAYER NAME WITH LOCAL FILE PATH
pop_data <- sf::st_read(
  "../../GIS/housing_density", 
  layer = "il_blk10_Census_change_1990_2010_PLA2"
)

# Function to extract data from a shape file and convert it to a density
extract_shp_density <- function(
  my_points,
  my_buffer,
  my_shp,
  layers = NULL
){
  ##############################################################################
  # This function has 4 arguments:
  #  my_points (sf object): The coordinates  of the sites you want to collect
  #                           spatial data from. The column name for the
  #                           names of the sites must be 'LocationName'.
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
  #  my_shp (sf):           A sf object of a shape file you want to extract
  #                           data from.
  #  layers (character):    A character vector of column names from my_shp
  #                           to summarise. Optional. If NULL, all numeric
  #                           columns are summarised.
  ##############################################################################
  
  # If an object is supplied to layers, make sure it is a character.
  warn <- NULL
  if(!is.null(layers)){
    if(!is.character(layers)){
      stop("A non-character object has been supplied to layers.")
    }
    # check if all the layers exist, warn if not.
    if(!all(layers %in% colnames(my_shp))){
      warn <- paste("\nWarning: 'my_shp' did not have all layers specified in",
                     "'layers' argument.\nMissing elements in 'layers' were",
                     "removed'.")
      # drop the elements in layer that lack columns in my_shp
      layers <- layers[which(layers %in% colnames(my_shp))]
      if(length(layers) == 0){
        layers <- NULL
      }
    }
  }
  # STEP 1
  # Reproject the point data to match projection of population layer
  .cli_post("Reprojecting my_points to raster projection:")
  
  points_RP <- sf::st_transform(
    my_points,
    sf::st_crs(my_shp)
  )
  
  .cli_post(pass = TRUE)
  # STEP 2 Create a buffer around each site
  .cli_post("Extracting spatial data:")

  points_buffer <- sf::st_buffer(
    points_RP,
    dist = my_buffer
  )
  
  # Extract population layer features that are contained within each buffers
  shp_intersection <- sf::st_intersection(
    points_buffer,
    my_shp
  )
  
  # Summarise the data if layers is not null
  if(!is.null(layers)){
    summary_data <- shp_intersection %>% 
      # remove spatial geometry so you are left with a data frame
      sf::st_set_geometry(NULL) %>% 
      # group by Location Name
      group_by(LocationName) %>% 
      # sum all the intersected pieces to get total housing units in each buffer
      dplyr::summarise_at(.vars = layers, .funs = sum) %>% 
      # divide by area of buffer, converting to km^2
      #  spatial data are the clumns we are summarizing
      #  sf::st_area(points_buffer) is the area we are buffering
      #  we modify the units 
      dplyr::mutate_if(is.numeric, 
                function(spatial_data){
                  spatial_data/units::set_units(
                    sf::st_area(
                      points_buffer
                    ),
                    km^2
                  )
                }
      ) %>% 
      arrange(LocationName)
  
  } else {
    summary_data <- shp_intersection %>% 
      # remove spatial geometry so you are left with a data frame
      sf::st_set_geometry(NULL) %>% 
      # group by Location Name
      group_by(LocationName) %>% 
      # sum all the intersected pieces to get total housing units in each buffer
      dplyr::summarise_if(is.numeric, .funs = sum) %>% 
      # divide by area of buffer, converting to km^2
      #  spatial data are the clumns we are summarizing
      #  sf::st_area(points_buffer) is the area we are buffering
      #  we modify the units 
      dplyr::mutate_if(is.numeric, 
                       function(spatial_data){
                         spatial_data/units::set_units(
                           sf::st_area(
                             points_buffer
                           ),
                           km^2
                         )
                       }
      )
    
  }
  
  .cli_post(pass = TRUE)
  if(!is.null(warn)){
    cat(
      cli::bg_black(
        cli::col_yellow(
          warn
        )
      )
    )
  }
  return(summary_data)
}

# Run function to calculate housing units, housing density, population 
#  and population density.  For this example we extract population data 
#  within a 1km radius buffer.
population_data <- extract_shp_density(
  sites,
  1000,
  pop_data,
  layers = c("POP10", "HU10")
)


