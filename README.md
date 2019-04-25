# UWIN Spatial Functions UWIN
Various functions to process spatial data related to UWIN projects in R

## Example Data

2019-04-25_SampleChicagoSites.csv is a sample of 10 Chicago UWIN sites used across these function as an example dataset. To conduct the analysis for your sites, replace the sample Chicago data with your data following the same data format.

## Scripts

extractRasterData.R - This function extracts the mean value of a given parameter within a given area around each site. In this script I use the Chicago sample data and the NLCD Impervious Cover data (avaliable from https://www.mrlc.gov/data?f%5B0%5D=category%3Aurban%20imperviousness) to extract the mean impervious cover within a 1km-radius buffer around each sample site. To calculate tree canopy cover around each site, just replace the Imperviosu Cover raster with the NLCD Tree Canopy Raster avaliable from https://www.mrlc.gov/data?f%5B0%5D=category%3Acanopy

extractPopulationData.R - This function extracts total housing units, housing density, total population, and population density within a given buffer around each sample site. This function calls 2010 statewide census data (using Illinois as an example) that is avaliable from http://silvis.forest.wisc.edu/data/housing-block-change/.

## Notes
In all cases make sure all files are in your working directory or change local file paths. Additionally, be careful to use the correct projections for your region as noted in the scripts.

