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


extract_lulc <- function(
  my_points,
  my_buffer,
  my_raster_data,
  lulc_cats = NULL
){
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
  #  my_raster_data (raster): A raster object that you want to extract data 
  #                             from.
  #  lulc_cats (numeric or list, optional): Land-use land-cover categories. 
  #                                 If this is a numeric then the data will
  #                                 be queried to return just those specific
  #                                 lulc categories (e.g., lulc_cats = c(1,2))
  #                                 will return the first two categories. If
  #                                 this is a list then each element must be a 
  #                                 numeric vector. If the numeric vector is
  #                                 > 1 for a specific list element then those
  #                                 categories will be summed together. This
  #                                 may be done to combine specific similar
  #                                 category types (e.g., combine 'building',
  #                                 'road', and 'other paved surfaces') to 
  #                                 generate an impervious surface category.
  #                                 The numeric or the list can be named. If
  #                                 they are then those names will be added
  #                                 to the returned object. If NULL, then all
  #                                 categories will be returned.
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
  # We need a sub-function to calculate the proportion of each category
  spatial_summary <- function(
    x,
    ncats = my_raster_data@data@max
  ){
    return(
      prop.table(
        tabulate(x, ncats)
      )
    )
  }
  
  .cli_post("Extracting spatial data:")
  prop_extract <- raster::extract(
    my_raster_data,
    points_RP,
    fun=spatial_summary,
    buffer= my_buffer,
    df=TRUE
  )
  .cli_post(pass = TRUE)
  
  prop_extract <- matrix(
    prop_extract[,names(my_raster_data)],
    ncol = my_raster_data@data@max,
    nrow = nrow(points_RP),
    byrow = TRUE
  )
  
  row.names(prop_extract) <- points_RP$LocationName

  # if lulc_cats is a list
  if(is.list(lulc_cats)){    
    prop_extract <- apply(
      prop_extract,
      1,
      function(x){
        sapply(
          lulc_cats,
          function(y)
            ifelse(
              length(y) == 1,
              x[y],
              sum(x[y])
            )
        )
      }
    )
    prop_extract <- t(prop_extract)
  }
  
# if it is a numeric
if(is.numeric(lulc_cats)){
  prop_extract <- prop_extract[,lulc_cats]
  if(!is.null(names(lulc_cats))){
    colnames(prop_extract) <- names(lulc_cats)
  }
}
  
  # create dataframe matching the sites with the extracted data
  .cli_post("Summarizing data:")
  df <- data.frame(
    LocationName = row.names(prop_extract),
    prop_extract,
    row.names = NULL
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
  return(df[order(df$LocationName),])
}

#  For this example we will extract the proportion canopy cover (lulc class 1)
#    and create our own 'impervious cover' value, which is the sum of multiple
#    lulc classes.
lulc_prop <- extract_lulc_data(sites,
                              1000,
                              my_map,
                              lulc_cats = list("tree" = 1,
                                               "imperv" = 5:7
                                          )
)
