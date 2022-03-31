# --------------------------------------------------
# Modifying ecoregions map to create "roadside" ecoregions for EAB dispersal
# 17 Mar 2022
# HH
# --------------------------------------------------
#

library(terra) #using this instead of the raster() package

#importing initial files
ecoregions <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Jane_from_GEE/ecoregions_map_final_INT4S_22Feb2022.tif")
mgmtareas <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_INT4S_22Feb2022.tif")

#checking to make sure they have the same extent, projection, etc (see function documentation)
compareGeom(mgmtareas, ecoregions)
#ok good!

#FIRST, masking out the inactive values (from mgmt map to ecoregions)
ecoregions2 <- mask(x=ecoregions, #raster to "update"
                          mask=mgmtareas, #raster to use for mask vals
                          maskvalues=c(0, NA), #reassign cells that = 0 or NA in mask raster
                          updatevalue=0 #TO the value 0
)
#after this, ecoregions2 and mgmtareas SHOULD have the same # 0s
#(which will be higher than the # in ecoregions):
zerosum1 <- sum(values(mgmtareas)==0)
zerosum2 <- sum(values(ecoregions2)==0)
zerosum3 <- sum(values(ecoregions)==0)
zerosum1==zerosum2
zerosum2==zerosum3
#looks good!
#and ALSO should have the same # NAs (preferably zero??)
nasum1 <- sum(is.na(values(mgmtareas)))
nasum2 <- sum(is.na(values(ecoregions2)))
nasum1==nasum2
#alright so far so good! (both are zero :))

#ok, next step is to create the "roads" ecoregions
#by adding +200 to the ecoregion value of any cell w/in the roads buffer
#for this using the mgmt areas roads type = 6 b/c we're already using that raster

ecoregions3 <- ifel(test= mgmtareas==6, 
                    #if (for a given cell), roads present, then +200 to ecoregions #
                    yes=ecoregions2+200,
                    no=ecoregions2 #and otherwise, take existing val from ecoregions
               )

ecoregions3 #alright looks like it worked, woohoo!!
#now let's visualize them all:
plot(ecoregions)
plot(ecoregions2)
plot(ecoregions3)

#looks good!! 
#now to save 'ecoregions 3' to file:
#ACTUALLY, just realized I can't save to the data type I need in terra package
#so will need to do that with the raster package instead after saving -__-
#UPDATE: looks like terra package now has functionality for INT4S data type, which is what I need??
#I swear it didn't before! But whatever I'm not complaining!
writeRaster(x=ecoregions3,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/ecoregions_masked_roads_17March2022.tif",
            datatype="INT4S" #this is the data type I should be using now!
)

#let's just check it to confirm the data type is correct...
library(raster)
ecoregions3test <- raster("C:/Users/theha/Documents/layers_for_LANDIS/ecoregions_masked_roads_17March2022.tif")
dataType(ecoregions3test) #INT4S as it should be! :)
