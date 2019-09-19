library(raster)
library(sf)
library(dplyr)
library(mapview)
library(cli)

# Check to make sure you have the 2010 NLCD landcover data downloaded.
#  write either the full or relative path to the NLCD data. If you do
#  not have this file it can be downloaded from:
#  browseURL("https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover").
#  For the continental US it is "NLCD 2011 Land Cover (CONUS).
my_nlcd_path <- 
  "../../GIS/NLCD_2011_landcover/NLCD_2011_Land_Cover_L48_20190424.img"

# Check if the file exists, if not provide a warning.
if( file.exists( my_nlcd_path ) ){
  cat(
    paste(
      cli::col_green(
        cli::symbol$tick
      ),
      "NLCD layer exists")
  )
}else{
 # The error (warning to provide to the user.)
 err <- paste0(
   "NLCD layer not located at this file path.\n",
   "  Please download it. For the U.S. it can be\n",
   "  downloaded from:\n\n",
   "  https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover\n\n",
   "  The rest of the script will not work without a NLCD layer."
 )
 cat(
   paste(
     cli::col_red(
       cli::symbol$cross
     ),
     err
   )
 )
}
                      
# Need a table of site locations
# Here I use a random sample of 10 Chicago sites as an example
# Data avaliable in same github repo
# Columns: LocationName, UTM_E, UTM_N, UTMZone, City
site_coords <- read.csv(
  "2019-04-25_SampleChicagoSites.csv",
  stringsAsFactors = FALSE
)

# create spatial points
# You must put the correct CRS for respective city
sites <- sf::st_as_sf(
  site_coords,
  coords = c("UTM_E", "UTM_N"),
  crs = 26916
)

# Visually inspect points to ensure the projection and location is correct
mapview::mapview(sites)


available_habitat_NLCD <- function(my_points, my_buffer, my_nlcd_path){
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
  #  my_ncld_path (char.):  A file path to the nlcd raster. The file should be
  #                           an image file an the path should be a character
  #                           object. The file path could be relative path
  #                           (i.e., from your current working directory),
  #                           or a full path.

  # Utility for command line reporting
  cli_post <- function(my_text, pass = FALSE){
    if(pass){
      cat(
        paste(
          cli::col_green(
            cli::symbol$tick
          ),
          "\n"
        )
      )
      
    } else {
      cat(
        paste(
          cli::symbol$bullet,
          my_text,
          " "
        )
      )
    }
  }
  # start command line reporting
  cat(
    paste0(
      cli::rule(line = 2),
      "\n\n"
    )
  )
  # Step 1: Load the raster data
  cli_post("Loading raster:")
  lc_map <- raster::raster(my_nlcd_path)
  cli_post(pass = TRUE)

  # Step 2: Reproject points to mach landcover raster
  cli_post("Reprojecting my_points to raster projection:")
  points_RP <- sf::st_transform(
    my_points,
    crs = sf::st_crs(
      lc_map
    )
  )
  cli_post(pass = TRUE)
  # Step 3:
  # Extract land cover data for each point, given a fixed radius buffer
  # This returns a list
  # Each list element contains the land cover catergory of each raster cell 
    # within the respective buffer feature
  cli_post("Extracting spatial data:")
  lc_extract <- raster::extract(
    lc_map,
    points_RP,
    buffer = my_buffer
  )
  cli_post(pass = TRUE)
  
  # Now we have a list element for each buffer/site 
  # We will summarize the information within each list element seperately
  # Then do it all at once with lapply
  summarize_available_habitat <- function(x){
    
    # Create a table of proportions for each land cover class
    proportions <- prop.table(
      table(x)
    )
    # Summarize each site's data by proportion of each cover type
    # Convert to data frame
    landcover <- data.frame(
      cover = names(proportions),
      percent = as.numeric(proportions)
    )
    # Sum across categories that we are considering habitat
    # Legend: https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend
    habitat <- sum(
      landcover[which(landcover$cover == 21),2], # developed open space
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
      landcover[which(landcover$cover == 95),2]  # wetland
    )
    
   return(habitat)
  }
  
  # Loop through each list element and calculate the proportion of habitat
  # lapply returns a list so we use do.call to collapse the list into a vector
  cli_post("Summarizing data:")
  habitat <- do.call(
    c,
    lapply(
      lc_extract,
      summarize_available_habitat
    )
  )
  
  # Combine the data with Location Name
  available_habitat <- data.frame(
    location_name = points_RP$LocationName,
    avail_habitat = habitat
  )
  cli_post(pass = TRUE)
  # end command line reporting
  cat(
    paste0(
      cli::rule(line = 2),
      "\n\n"
    )
  )
  return(available_habitat)
}

# Run the function to extract proportion of available habitat around each site
# Here we use the Chicago data as an example and a 1km-radius buffer
avail_habitat_example <- available_habitat_NLCD(sites, 1000, my_nlcd_path)
  
  