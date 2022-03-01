# --------------------------------------------------
# trying out new packages for ArcGIS to create stand & management area maps
# for LANDIS!
# 02 Feb 2022
# HH
# --------------------------------------------------
#

# first: download new packages to try out! -------------------------------

#install.packages("terra") #to manipulate rasters...
library(terra)
help("terra")

#library(raster) #the older package Jane says prob won't keep being developed...

#install.packages("rasterize")
#install.packages("fasterize") #does what rasterize does, but...faster...
#install.packages("sf") #for so-called "simple features" (AKA vector data? e.g. polgons)

#directory <- getwd() #saving previous directory for regular project stuff to replace @ the end of this script
#setwd("C:/Users/theha/Documents/ArcGIS/Projects/LANDIS_stuff") #and setting new WD for now

# OK, first step is trying to fix/finish management area map! -------------------------------

#input files:
roads_raster <- rast("C:/Users/theha/Documents/ArcGIS/Projects/LANDIS_stuff/tl_roads_raster_6.tif")
mgmt_raster <- rast("C:/Users/theha/Documents/ArcGIS/Projects/LANDIS_stuff/reclass_mgmt_map_25Jan202.tif")

#check out extent, resolution, & other deets:
roads_raster
mgmt_raster

#OK, need to set to the same extent!
#SEE help("terra") sec. 8: "VIII. Getting and setting SpatRaster dimensions"
#LET'S TRY THIS...
#ext(mgmt_raster)
#ext(roads_raster) #VIEW extent (x/y min/max)
#ext(roads_raster) <- ext(mgmt_raster) #SET new extent...
#NO, this just stretched out the resolution! NOT good

#let's try with this extend function:
roads_ext <- extend(x=roads_raster, y=mgmt_raster)

compareGeom(mgmt_raster, roads_ext) #so far so good...?
#testing with some other things:
#roads_ext
#mgmt_raster
#ext(roads_ext)==ext(mgmt_raster) #TRUE, as it should be!
#ext(roads_ext)==ext(roads_raster) #and FALSE, as it also should be!

#now need to do some raster math!
#using function ifel() to create an if/else statement:
mgmt_roads_combined <- ifel(test= roads_ext==6, 
                            #if (for a given cell), roads present, then take that val (6)
                            yes=roads_ext,
                            no=mgmt_raster, #and otherwise, take val from mgmt_raster
                            filename="mgmt_map_roads_test_2Feb2022.tif") #save to this filename...

#IF this works (BIG if!), try doing it w/ an and statement as the test
#so it only takes in cells covered by both roads AND existing mgmt map
mgmt_roads_combined
#WAHOO, it worked! (only after implementing extend() as seen above!)
#HOWEVER, when I opened in arcmap, it seems to be invisible...
#and maybe back to floating point values...?
#OK, TRYING THIS AGAIN w/ DATA TYPE SPECIFIED! ("datatype" param.)
#NOTE: can set the DEFAULT data type for written files
#with terraOptions(datatype=)


mgmt_roads_combined2 <- ifel(test= roads_ext==6, 
                            #if (for a given cell), roads present, then take that val (6)
                            yes=roads_ext,
                            no=mgmt_raster, #and otherwise, take val from mgmt_raster
                            filename="mgmt_map_roads_test2_2Feb2022.tif", #save to this filename...
                            datatype="INT4U" #integer, 4-bit unsigned (only positive vals!)
                            ) 
#now let's test in Arc if THIS raster is useable w/ attribute table...
#update: I just needed to use the "Build Raster Attribute Table" tool LOL!

#OK, now to do the same thing, but w/o roads themselves
#(only the roads-buffers in forest areas)
mgmt_roads_combined3 <- ifel(test= (roads_ext==6 & mgmt_raster!=0), 
                             #if (for a given cell), roads present AND it's on an otherwise forested area,
                             yes=roads_ext, #then use the roads value,
                             no=mgmt_raster #and otherwise, take val from mgmt_raster
                             #NOT saving to file yet b/c I also want to mask it first!
) 
#values() function didn't seem that useful...
#would love to know how to basically create an attribute table
#for a raster/spatRaster within R...

#but ANYWAY, onwards to the mask!

#input ecoregions file to use as mask:
#REMINDER that it's in the layers_for_LANDIS folder!!
ecoregions_map  <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Jane_from_GEE/Hanusia_soilmu_a_Merge_colors_25Aug2021_add100_utm18N.tif")
ecoregions_map
plot(ecoregions_map)
#compare w/ my other working file:
compareGeom(ecoregions_map, mgmt_roads_combined3) #YAY looks good!!

#now, trying the mask tool:
mgmt_roads_masked <- mask(x=mgmt_roads_combined3, #raster to "update"
                          mask=ecoregions_map, #raster to use for mask vals
                          maskvalues=c(0, NA), #reassign cells that = 0 or NA in mask raster
                          updatevalue=0 #TO the value 0
                          )
mgmt_roads_masked
plot(mgmt_roads_masked) #OMG FINALLY IT WORKED YAY!!!

#NOW TO SAVE THE MANAGEMENT AREAS FILE: 
#NOTE: SAVING TO LAYERS_FOR_LANDIS FOLDER!
writeRaster(x=mgmt_roads_masked,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_2Feb2022.tif",
            datatype="INT4U" #forgot this the first time LOL! (and it DID default to floating point before, important to take note!)
            )

#OK, MANAGEMENT AREA MAP IS OFFICIALLY DONE!

# stand maps! -------------------------------
#setwd(directory)
#getwd() #reset working directory back to the default,
#wasn't worth the trouble if I forgot to reset it later...
#just need to SPECIFY file path each time I import/export!

#OK, let's try this--first by importing 3 parcel datasets, 
#then clipping them to the extent/mask of study area,
#then raster-izing them,
#(may switch the previous 2 steps--depending)
#and finally combining them together

#old.packages()
#how to update my packages??
#ANSWER: I needed to download & set up Rtools, as documented here: https://cran.r-project.org/bin/windows/Rtools/rtools40.html
#now, we should be good to go!

#library(terra)
library(fasterize)
help("fasterize")

#first step = import the parcel shapefiles!
MA_parcels <- vect("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/Mass_Statewide_parcels_SHP/L3_TAXPAR_POLY_ASSESS_WEST.shp")
VT_parcels <- vect("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/VT_parcel_polygons_statewide/FS_VCGI_OPENDATA_Cadastral_VTPARCELS_poly_standardized_parcels_SP_v1.shp")
GMNF_parcels <- vect("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/GMNF_stands_from_Jane/GMNFStands.shp")

#MA_parcels
#VT_parcels
#GMNF_parcels

#also import final mgmt areas file (from above!) to reference...
mgmtareas <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_2Feb2022.tif")

#next step = re-project them to correct coordinate systems...
#using mgmtareas (loaded in file vers. of what we created, above) for ref
MA_parcels_proj <- project(x=MA_parcels, y=mgmtareas)
VT_parcels_proj <- project(x=VT_parcels, y=mgmtareas)
GMNF_parcels_proj <- project(x=GMNF_parcels, y=mgmtareas)

MA_parcels_proj #projected to the correct geometry, WHEE
VT_parcels_proj
GMNF_parcels_proj

#note: gonna save these so I don't have to repeat this step AGAIN!
writeVector(x=MA_parcels_proj,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/MA_parcels_UTM18N.shp")
writeVector(x=VT_parcels_proj,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/VT_parcels_UTM18N.shp")
writeVector(x=GMNF_parcels_proj,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/GMNF_parcels_UTM18N.shp")

#then, will clip to study area extent... 
#(only need to do this for MA and VT state info)
#MAYBE- might be easier to do this in Arc???
#OR, just wait and do it after raster-izing??

#IN THAT CASE, need to look @ metadata first...

#then, need to view METADATA for each of these parcel/stand datasets
#to figure out which field is most useful for unique values!

#length(unique(GMNF_parcels_proj$OBJECTID))==length(GMNF_parcels_proj)
#GMNF = use OBJECTID

#length(unique(VT_parcels_proj$OBJECTID))==length(GMNF_parcels_proj)
#VT = use OBJECTID

#MA = MAYBE use LOC_ID?? need to test again first like:
#names(MA_parcels_proj) #NOT OBJECTID
#length(unique(MA_parcels_proj$LOC_ID))
#loc_ID_len <- 620335
#OK, 620,000 VS 660,000...that's a big difference, but...
#also seems about right for parcels being linked via tax code??
#and probably will only apply to urban parcels mostly anyway, 
#which won't end up in our final analysis regardless??

#just gonna look at one more thing...I think c(TOWN_ID, PLOT_ID) would work!
#start with this step next time! Then, raster-ize

#GMNF_parcels_raster <- fasterize(sf=GMNF_parcels_proj,
#                                 raster=mgmtareas,
#                                 field="OBJECTID",
#                                 fun="last")
#this isn't working for some reason...
#so, maybe I'll try doing it in Arc instead??

#alright, I ended up rasterizing them in Arc!
#next I need to load those back in, 
#reassign values for the FID ones (MA & GMNF),
#mask them,
#mosaic them,
#and finally figure out how to make sure
#they are each in only 1 management area!?

GMNF_parcels_rast <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/GMNF_parcels_raster.tif")
MA_parcels_rast <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/MA_parcels_raster.tif")
VT_parcels_rast <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/VT_parcels_raster.tif")

#OK let's try reassigning...
values(GMNF_parcels_rast) <- values(GMNF_parcels_rast) + 1
values(MA_parcels_rast) <- values(MA_parcels_rast) + 1

#now, we need to match the extent of the larger rectangle!
GMNF_parcels_rast <- extend(x=GMNF_parcels_rast, y=mgmtareas)
VT_parcels_rast <- extend(x=VT_parcels_rast, y=mgmtareas)
MA_parcels_rast <- extend(x=MA_parcels_rast, y=mgmtareas)
#compareGeom(VT_parcels_rast, mgmtareas)
#VT and GMNF match, but MA doesn't...is it too big??
#MA_parcels_rast <- crop(x=MA_parcels_rast, y=mgmtareas)
#compareGeom(MA_parcels_rast, mgmtareas) #now it's all good!

#next, it's mask time!
GMNF_parcels_mask <- mask(x=GMNF_parcels_rast,
                          mask=mgmtareas,
                          maskvalues=0, #reassign cells that = 0 in mask raster
                          updatevalue=0 #TO the value 0
                          )
GMNF_parcels_mask
#HMM, ok that doesn't look quite right...but we'll leave it for now...
#actually, I think we are OK; the rest are just NAs (b/c they didn't get masked by a 0 in the mgmtareas map!)
#can fix this by reassigning NAs as 0 values!
GMNF_parcels_mask <- ifel(test=is.na(GMNF_parcels_mask),
                          yes=0, no=GMNF_parcels_mask)

VT_parcels_mask <- mask(x=VT_parcels_rast,
                          mask=mgmtareas,
                          maskvalues=c(0), #reassign cells that = 0 in mask raster
                          updatevalue=0 #TO the value 0
)
#similarly, reassigning NA vals to 0:
VT_parcels_mask <- ifel(test=is.na(VT_parcels_mask),
                          yes=0, no=VT_parcels_mask)

MA_parcels_mask <- mask(x=MA_parcels_rast,
                        mask=mgmtareas,
                        maskvalues=c(0), #reassign cells that = 0 in mask raster
                        updatevalue=0 #TO the value 0
)
#similarly, reassigning NA vals to 0:
MA_parcels_mask <- ifel(test=is.na(MA_parcels_mask),
                        yes=0, no=MA_parcels_mask)

#OK, so far so good!
#now, let's find out how many unique values actually remain
#in the masked portion of each parcel raster:
MA_unique_count <- nrow(unique(MA_parcels_mask)) #40885 unique vals
VT_unique_count <- nrow(unique(VT_parcels_mask)) #37335 unique vals
GMNF_unique_count <- nrow(unique(GMNF_parcels_mask)) #4241 unique vals
#OK, that's significantly better than before!
sum(MA_unique_count, VT_unique_count, GMNF_unique_count)
#total # of unique stand ID values is less than 100,000
#SO, I could add the management area value/"zone"
#as a factor of 10 (e.g. 100,000s through 600,000s)

#BUT, first need to make sure each stand ID is UNIQUE among the group!
#maybe by re-assigning them based on their position in the "unique" framework?
#setting up a for loop?
#and then at the END merging them together (but just w/ raster math)....

#alright, gonna do this TOMORROW, so just gotta save the masked parcel rasters
#in the meantime...
writeRaster(x=GMNF_parcels_mask,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/GMNF_parcels_masked_3Feb2022.tif",
            datatype="INT4U" #I looked it up and 4-bit integer still has PLENTY of storage space for values!!
)
writeRaster(x=VT_parcels_mask,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/VT_parcels_masked_3Feb2022.tif",
            datatype="INT4U" #I looked it up and 4-bit integer still has PLENTY of storage space for values!!
)
writeRaster(x=MA_parcels_mask,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/MA_parcels_masked_3Feb2022.tif",
            datatype="INT4U" #I looked it up and 4-bit integer still has PLENTY of storage space for values!!
)

#FOR NEXT TIME: create a unique list/directory of unique vals for parcels across ALL 3 orig files,
#then create a for loop(s) reassigning a new val based on the original one?
#CAN'T just add managment area val in the millionths place
#b/c the way I assigned vals to these rasters using FID
#means they  have some values that are the same across (for example) VT vs MA!
#so, will have to go the for loop route (or accomplish the same thing w/ another method).
#will still need to figure out how to handle GMNF vs VT where they overlap...

#idea: ADD the max value of GMNF stands to VT stands,
#and then add the max value of GMNF+VT stands to MA stands
#(so they are all unique #s)

GMNF_parcels_mask <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/GMNF_parcels_masked_3Feb2022.tif")
VT_parcels_mask <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/VT_parcels_masked_3Feb2022.tif")
MA_parcels_mask <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/MA_parcels_masked_3Feb2022.tif")

#finding out TOTAL max val 
VT_max <- max(values(VT_parcels_mask))
GMNF_max <- max(values(GMNF_parcels_mask))
MA_max <- max(values(MA_parcels_mask))
MA_max + GMNF_max + VT_max
#OK, it's over a million...so will need to add stand val in the TEN MILLIONS spot, I guess?
#at least that still fits in a 4-bit unsigned integer!

#testing before transformation--overlapping/same values, what we are trying to fix!
#BUT need to exclude zeros from this!!!
sum((GMNF_parcels_mask[GMNF_parcels_mask!=0]) %in% (VT_parcels_mask[VT_parcels_mask!=0]))

#ANYWAY, I think we can make this work regardless......
#"math" wasn't working, maybe try an ifel statement instead? (to exclude zeros)
VT_parcels_mask <- ifel(test=VT_parcels_mask==0,
                        yes=VT_parcels_mask,
                        no=VT_parcels_mask+GMNF_max
                        )


#testing...
plot(VT_parcels_mask)
VT_parcels_mask
sum((GMNF_parcels_mask[GMNF_parcels_mask!=0]) %in% (VT_parcels_mask[VT_parcels_mask!=0]))
#looks good!!

#now to do the same thing with MA- prelim tests first:
sum((GMNF_parcels_mask[GMNF_parcels_mask!=0]) %in% (MA_parcels_mask[MA_parcels_mask!=0]))
sum((VT_parcels_mask[VT_parcels_mask!=0]) %in% (MA_parcels_mask[MA_parcels_mask!=0]))
plot(MA_parcels_mask)

#now do the same thing w/ an ifel statement:
MA_parcels_mask <- ifel(test=MA_parcels_mask==0,
                        yes=MA_parcels_mask,
                        no=MA_parcels_mask+GMNF_max+VT_max
)
#and evaluate again post-reclassification:
sum((GMNF_parcels_mask[GMNF_parcels_mask!=0]) %in% (MA_parcels_mask[MA_parcels_mask!=0]))
sum((VT_parcels_mask[VT_parcels_mask!=0]) %in% (MA_parcels_mask[MA_parcels_mask!=0]))
plot(MA_parcels_mask)
min(MA_parcels_mask[MA_parcels_mask!=0])
VT_max + GMNF_max
#OK, everything looks great!! all are now unique values (except 0).

#NOW, time to combine them!
#will start w/ GMNF & VT because there is some overlap there...
VT_GMNF_merge <- ifel(test=GMNF_parcels_mask==0,
                      yes=VT_parcels_mask, #if there's a val for GMNF, it overrides
                      no=GMNF_parcels_mask) #otherwise, default to VT val (incl zeros)

VT_GMNF_merge
plot(VT_GMNF_merge) #well, THAT doesn't look right...
#trying it flipped instead?
#WHY isn't this working??

#maybe try w/ MA first to see if THAT one works??
MA_VT_merge <- MA_parcels_mask + VT_parcels_mask
MA_VT_merge #max val is a bit greater than max of MA_parcels_mask, 
MA_parcels_mask #so there are a few vals that got added together (@ the border?)
plot(MA_VT_merge) #but...how many of those?
#creating a negation function (to mean NOT in):
'%!in%' = Negate('%in%')
#will this work?? TBD lol!
sum(unique(MA_VT_merge) %!in% unique(MA_parcels_mask) & unique(MA_VT_merge) %!in% unique(VT_parcels_mask))
#update: answer was 1, so looks like it's just one pixel whose value got overlapped...
#I can live with that!!
#but, OK- now that we have this merged raster, can we try adding in GMNF stands??

compareGeom(MA_VT_merge, GMNF_parcels_mask)
#GMNF_states_merge <- ifel(GMNF_parcels_mask==0,
#                          MA_VT_merge,
#                          GMNF_parcels_mask)
#plot(GMNF_states_merge)    
#WHY DOES THIS KEEP HAPPENING?!?!
#it keeps masking out the rest of Bennington co. in VT as well...
#maybe this is getting assigned the value 1 in GMNF stands?
#and that explains why there were so many 1s in that raster??
click(GMNF_parcels_mask) #I think this is what is happening!
#so basically, one stand (called number 1) might get covered up by this...
#and I will have to live with it!
#OK I THINK THIS IS BECAUSE I REASSIGNED VALUES TO GMNF AND MA YESTERDAY
#AND ADDED 1 TO ALL CELLS, INCLUDING ZERO VALS?!
#let's try this again...
GMNF_states_merge <- ifel(GMNF_parcels_mask<=1, #now, "covering" if GMNF is 0 or 1
                          MA_VT_merge, #with MA/VT values
                          GMNF_parcels_mask) #but GMNF overrides if it's over 1!
plot(GMNF_states_merge)  
click(GMNF_states_merge) #alright, I think we're good! Those values just look so small...

#NOW LET'S TRY AGAIN w/ IFEL to connect w/ management areas!!
plot(mgmtareas)

stands_mgmt_combined <- GMNF_states_merge+mgmtareas*10000000 #adding mgmt area in the 10-millionths place?
stands_mgmt_combined
plot(stands_mgmt_combined)
#I think this is OK! mgmt area represented by the 10-millionths place,
#and after combining nrow(unique
#(GMNF_states_merge)) vs nrow(unique(stands_mgmt_combined)),
#it definitely added *a fair amount* of "split up" stands where they didn't fall into
#the same management areas, so there are some new "fringe stands"
#but honestly, roads alone could have contributed a lot to this!!

nrow(values(mgmtareas)==6)
nrow(values(stands_mgmt_combined)>=60000000) #these 2 #s are equal, yay!
stands_mgmt_combined
#OK, I think we are good here?!
#just need to SAVE this stand map raster!
writeRaster(x=stands_mgmt_combined,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/stands_classified_4Feb2022.tif",
            datatype="INT4U" #forgot this the first time LOL! (and it DID default to floating point before, important to take note!)
)

compareGeom(mgmtareas, stands_mgmt_combined)
#now I want to look at this in Arc!
#looks GREAT! :) 

### UPDATE 22 FEB 2022 ###

# updating files w/ correct data type! -------------------------------

#need to convert from 32-bit (4-byte) unsigned integer to 32-bit signed integer.
#using the raster package in R because terra package doesn't have that type for some reason.
#code below copied/modified from the script I made to test this process 
#using the Biomass Harvest ext example files.

library(raster)
#input file to be changed (stand map that I created earlier this month)
StandMap <- raster("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/stands_classified_4Feb2022.tif")
#now let's take a look at it:
dataType(StandMap) #correct! (currently IN4U)

#now, reassigning the dataType to INT4S:
dataType(StandMap) <- "INT4S"
#testing again to see if this worked:
dataType(StandMap)
#great!

#now, write to file:
writeRaster(x=StandMap,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/stands_classified_INT4S_22Feb2022.tif",
            datatype="INT4S")

#and now to just repeat this exact process w/ management area map:
MgmtMap <- raster("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_2Feb2022.tif")
#now let's take a look at it:
dataType(MgmtMap) #correct! (currently IN4U)

#now, reassigning the dataType to INT4S:
dataType(MgmtMap) <- "INT4S"
#testing again to see if this worked:
dataType(MgmtMap)
#great!

#now, write to file:
writeRaster(x=MgmtMap,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_INT4S_22Feb2022.tif",
            datatype="INT4S")


#and because why not, let's also just check the data type for the ecoregions map:
EcoregionsMap <- raster("C:/Users/theha/Documents/layers_for_LANDIS/Jane_from_GEE/Hanusia_soilmu_a_Merge_colors_25Aug2021_add100_utm18N.tif")
dataType(EcoregionsMap) #this one is 2-bit unsigned...maybe we should convert it just in case???
#I don't think they actually NEED to match up, but couldn't hurt right?
dataType(EcoregionsMap) <- "INT4S"
dataType(EcoregionsMap)

#and rewrite to file:
writeRaster(x=EcoregionsMap,
            filename="C:/Users/theha/Documents/layers_for_LANDIS/Jane_from_GEE/ecoregions_map_final_INT4S_22Feb2022.tif",
            datatype="INT4S")
