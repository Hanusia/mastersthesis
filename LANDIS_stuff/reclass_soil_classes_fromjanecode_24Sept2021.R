library(rgdal)
library(raster)
library(sp)
#library(viridis)

## Read in raster of DEM
dem <- raster('C://Users//theha//Documents//layers_for_LANDIS//Jane_from_GEE//DEM_Hanusia_from_SRTM_utm18_WGS84.tif')

## Create function to extract lonlat from raster cell
## Function copied from  https://github.com/SWotherspoon/SGAT/blob/master/R/Raster.R

lonlatFromCell <- function(raster,cells,spatial=FALSE) {
  if(is.na(projection(raster)) || isLonLat(raster)) {
    xyFromCell(raster,cells,spatial=spatial)
  } else {
    p <- spTransform(xyFromCell(raster,cells,spatial=TRUE),
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    if(spatial) p else coordinates(p)
  }
}

## Create copies of dem raster to hold cell latitude and longitude
dem.lat <- dem.long <- dem
## Extract long and lat from raster cells using function and cast to data.frame
dem.df.ll <- data.frame(lonlatFromCell(dem, 1:ncell(dem)))
## Now set values for rasters of dem.long and dem.lat
dem.lat <- setValues(dem.lat, values = dem.df.ll$y)
dem.long <- setValues(dem.long, values = dem.df.ll$x)

## Try using latitude raster to modify how numbers are displayed in DEM
## First, set a latitude that will divide your northern and southern regions
## Modify this for your study region
lat.divide <- 43.5 #keeping this since it works for my study region

#### example ####
dem.class <- dem
## First, create a simple example by converting DEM to classified map with 2 classes
dem.class[which(dem[] > 450)] <- 2
dem.class[which(dem[] <= 450)] <- 1
## View both rasters in the plot window
plot(dem)
plot(dem.class)
## View latitude raster
plot(dem.lat)
dem.lat

## Example code, add a single scalar value to dem.class for all areas above a certain latitude
## Notice that only difference b/w right and left side of "<-" is the scalar value to add (5)
dem.class[which(dem.lat[] > lat.divide)] <- 5 + dem.class[which(dem.lat[] > lat.divide)]

## Now plot the result
plot(dem.class)

########

#applying the example above to my soil class file:

soil_raster <- raster('C://Users//theha//Documents//ArcGIS//Projects//LANDIS_stuff//soil_raster_24Sept2021.tif')
plot(soil_raster) #view the soil class raster to see how it is initially
soil_raster2 <- soil_raster #creating a copy to modify within the code
soil_raster2[which(dem.lat[] > lat.divide)] <- 100 + soil_raster2[which(dem.lat[] > lat.divide)] 
#this adds 100 to the soil index value of the raster for any cells above the latitude cutoff value
#functionally, this means that all cells in the northern part of my study area will have their soil index/ecoregion "reclassified" from x to x+100 
#so that GEE can evaluate them separately from the southern region

## Change variable name, directory path and new file name for your application.
writeRaster(soil_raster2, 'C://Users//theha//Documents//layers_for_LANDIS//soil_raster_reclassified_24Sept2021', 
            format = "GTiff")

