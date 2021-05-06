## This script reads SSURGO data to extract cut points for slope and depth
## The cut points are applied to SSURGO data to create ecoregion classes
## Classes represent combinations of soil texture, slope and depth

library(Hmisc)
library(plyr)
library(dplyr)

#options(tibble.width = Inf)
options(dplyr.width = Inf,scipen=999, stringsAsFactors = F) # scipen=0 to turn scientific notation back on again.

# Read SR SSURGO
## CHECK PATH
#D2 <- read.csv("data\\soilmu_a_Merge_26Feb2021.csv",header=T)
#D2h <- read.csv("data\\mu_summary_wilt_hanusia_2021-03-10.csv", header=T) #this one would have truncated headers
D2 <- read.csv("~/mastersthesis_repo/mastersthesis/LANDIS_stuff/mu_summary_wilt.csv", header=T,fileEncoding = "UTF-8-BOM")
## Need mean elevation for each row in merged shapefile for this code. 
## Steps: (1) Open shape file and DEM in Arcmap. (2) Go to catalog, spatial analyst, zonal stats by table.
## (3) Specify shapefile for first entry, use FID as the field to summarize zonal stats. Specify DEM as the raster to summarize.
## Compute just the MEAN and tell it to ignore null or nodata values. It will probably default to writing the output to 
## a table in your default geodatabase.(4) To view the output, you can use a "join" with the shapefile. You will join with FID
## as the join field. When you open the joined attribute table, the last column will be the mean elevation. Try vizualizing this
## field. If you notice that Some of the soil polygons in southern unit are not getting summarized (mean = NULL), that is the problem I encountered.
## I don't know why, could be a problem with Shapefile? Maybe try this and see if you can figure it out. I put the topographic
## indices raster stack in the share drive data_Hanusia. Elevation is the first band of that raster stack.
#elev <- read.csv("data_shared\\Hanusia_files_for_soil_class_analysis-20210303\\Benn-Berk_soilmu_a_Merge_mean_elev_by_FID_zonal_stats.csv",header=T)
elev <- read.csv("~/mastersthesis_repo/mastersthesis/LANDIS_stuff/soilmu_a_Merge_elev_23April2021.csv", header=T,fileEncoding = "UTF-8-BOM") # "VALUE" relates to objectID in merged shapefile of ssurgo soils data.

# Edit some field names and formats - specific to Hanusia's table
names(D2)
#[1] "mukey"                 "FirstOfslopegradwta"   "FirstOfbrockdepmin"    "SumOfksat_weight1"     "SumOfsand_weight1"    
#[6] "SumOfsilt_weight1"     "SumOfclay_weight1"     "SumOffieldcap_weight1" "SumOfwiltpt_weight1"   "SumOfmaxdepth_weight" 

# You need your column names to agree with names above. They may get truncated if you first join and resave in Arcmap.
# Try to import the .csv table you export from Access directly here, before any joins to the merged shapefile. #OK, now we are good!
## Simplify some variable names and make sure they are classified as.numeric

names(D2)[13] <- "SumOfwiltpt_weight1"#re-naming my wilting point column (originally [1] "AvgOfwfifteenbar_r") to match up w/ Jane's code!

D2$slopegradw <- D2$FirstOfslo
D2$slopegradw <- D2$FirstOfslopegradwta # as.numeric()
D2$brockdepmi <- as.numeric(D2$FirstOfbrockdepmin)
D2$ksat <- as.numeric(D2$SumOfksat_weight1)
D2$sand <- as.numeric(D2$SumOfsand_weight1)
D2$silt <- as.numeric(D2$SumOfsilt_weight1)
D2$clay <- as.numeric(D2$SumOfclay_weight1)
D2$fieldcap <- as.numeric(D2$SumOffieldcap_weight1)
D2$wiltpoint <- as.numeric(D2$SumOfwiltpt_weight1)
D2$maxdepth <- as.numeric(D2$SumOfmaxdepth_weight)
## Calculate sum of sand, silt and clay, divide and multiply times 100 to get a percentage of each.
D2$compsum <- D2$sand + D2$silt + D2$clay
D2$sand <- D2$sand / D2$compsum * 100
D2$silt <- D2$silt / D2$compsum * 100
D2$clay <- D2$clay / D2$compsum * 100
## Create empty variable "TextClass" to be populated by rule-based classifier below
D2$TextClass <- "None"
attach(D2)

# Calc texture classes - This is the section in the code that creates soil texture classes from thresholds in sand, silt, clay
D2$TextClass[silt+(1.5*clay) < 15] <- "Sand"
D2$TextClass[(silt+(1.5*clay) >= 15) & (silt + (2*clay)<30)] <- "LoamySand"
D2$TextClass[((clay >=7)&(clay<20)&(sand>52)&(silt+(2*clay)>=30))] <- "SandyLoam"
D2$TextClass[((clay<7)&(silt<50)&(silt+(2*clay)>=30))] <- "SandyLoam"
D2$TextClass[((clay >=7)&(clay<27)&(silt>=28)&(silt<50)&(sand<=52))] <- "Loam"
D2$TextClass[((silt >=50)&(clay>=12)&(clay<27))] <- "SiltLoam"
D2$TextClass[((silt >=50)&(silt<80)&(clay<12))] <- "SiltLoam"
D2$TextClass[((silt >=80)&(clay<12))] <- "Silt"
D2$TextClass[((clay >=20)&(clay<35)&(silt<28)&(sand>45))] <- "SandyClayLoam"
D2$TextClass[((clay >=27)&(clay<40)&(sand>20)&(sand<=45))] <- "ClayLoam"
D2$TextClass[((clay >=27)&(clay<40)&(sand<=20))] <- "SiltyClayLoam"
D2$TextClass[((clay >=35)&(sand>45))] <- "SandyClay"
D2$TextClass[((clay >=40)&(silt>=40))] <- "SiltyClay"
D2$TextClass[((clay >=40)&(sand<=45)&(silt<40))] <- "Clay"

## Quick vizualize barplot of TextClass in your data
barplot(table(D2$TextClass))

## Link elevation data from shapefile back to mu_summary table by mukey
## This code assumes you have calculated mean elevation for each polygon in your merged shapefile.
## Ask Jane if you need help with how to do that.
elev$elev <- elev$ZonalSt_soilMerge.MEAN #updated this line to reflect actual name of my variable from elev file
elev$mukey <- elev$soilmu_a_Merge.MUKEY #also ADDED this line myself so we've got just the "MUKEY" variable name

## Join your merged shapefile attribute table (imported as "elev" here) with table D2, excluding any overlapping variables
elev2 <- elev %>% dplyr::select(!which((names(elev) %in% names(D2)))) %>% inner_join(D2, by = c("MUKEY" = "mukey"))
#this isn't working....I'll try another method instead!

elev2 <- merge(x=D2, y=elev, by="mukey")

# Figure out cut points to subdivide soil texture classes based on slope, depth or with elevation, if you calculated zonal means by oid
slopecuts <- quantile(elev2$slopegradw,probs=seq(0,1,1/3),na.rm=T)[3]
depthcuts <- quantile(elev2$maxdepth,probs=seq(0,1,0.2),na.rm=T)[2:3]
#depthcuts <- c(30)
# Visualize soil depth
with(elev2, hist(maxdepth, nclass=40)) # If most soils are deeper than 40cm, where most root action is, maybe not worth using as divider
#the vast majority of mine maxdepth values are 100...

elevcuts <- quantile(elev2$elev,probs=seq(0,1,0.1), na.rm=T)[c(6,10)]
## For New England, when dividing elevation into thirds, consider using 650 as upper cut, as it approximates mean for ecotone
elevcuts <- c(elevcuts[1],650)

# Apply cut points
elev2$slopebin <- as.numeric(cut2(elev2$slopegradw,unique(slopecuts)))
elev2$depthbin <- as.numeric(cut2(elev2$maxdepth,unique(depthcuts)))
elev2$elevbin <- as.numeric(cut2(elev2$elev,unique(elevcuts)))

# Identify unique combinations

names(elev2)[33] <- "Shape_Area" #renaming this variable with a bunch of stuff in front of it to "Shape_Area"

## In this step you can add more of the cut point variables, like slopebin or depthbin to create more classes if desired
Dout <- plyr::count(elev2[,c("TextClass","elevbin")]) #haven't aded any variables to this yet...
print(Dout)


sumArea <- elev2 %>% group_by(TextClass, elevbin) %>% summarize_at(vars(Shape_Area), list(~ sum(.,na.rm=T)))
names(sumArea)[3] <- "sum_Shape_Area"

# Ecoregion index
Dout$soilindex <- row.names(Dout)
Dout <- Dout %>% inner_join(sumArea)

# Append ecoregion classes to SSURGO data
Dcomb <- merge(elev2,Dout,by=c("TextClass","elevbin"))
# Compute available water holding capacity (AWC or WHC) as difference between Field Capacity and Wilting Point
Dcomb$WHC <- Dcomb$fieldcap - Dcomb$wiltpoint

# Compute weighted mean of soil variables (weighted by polygon area)
Dout2 <- Dcomb %>% group_by(soilindex) %>% summarize_at(vars(fieldcap,wiltpoint,WHC,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth,elev),list(~ weighted.mean(., Shape_Area,na.rm=T)))
Dout3 <- Dout %>% inner_join(Dout2)

## Inspect Dout tables with soil variable means. Then combine similar, weird or otherwise unmodelable classes
# Remap class values to 0, not modeled, masked or lump with another similar class
soilindex <- soilindexRemap <- Dout3$soilindex
# index 9 and 10 have no TextClass and don't summarize WHC, other vars, mask out to 0
soilindexRemap[which(soilindex == 9)] <- 0
soilindexRemap[which(soilindex == 10)] <- 0
# When lumping rare classes that don't have Elev, use minimum distance based on fieldcap:elev
# dist(Dout3[,6:16], method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
soilindexRemap[which(soilindex == 4)] <- 23
soilindexRemap[which(soilindex == 8)] <- 11
soilindexRemap[which(soilindex == 14)] <- 12
soilindexRemap[which(soilindex == 20)] <- 17
soilindexRemap[which(soilindex == 24)] <- 23

soilsRemapDf <- data.frame(soilindex = soilindex, soilindex2 = soilindexRemap)
## Add updated, remapped classes to table Dcomb and recalculate class means
Dcomb <- Dcomb %>% inner_join(soilsRemapDf, by=c("soilindex"="soilindex"))
Dout4 <- Dcomb %>% group_by(soilindex2) %>% summarize_at(vars(fieldcap,wiltpoint,WHC,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth,elev),list(~ weighted.mean(., Shape_Area,na.rm=T))) %>%
  arrange(soilindex2)
updateFreq <- Dout3 %>% inner_join(soilsRemapDf) %>% group_by(soilindex2) %>% summarize_at(vars(freq), list(~ sum(.,na.rm=T)))


Dout4 <- Dout3 %>% inner_join(soilsRemapDf) %>% dplyr::filter(!(is.na(elevbin) | TextClass == "None")) %>% 
  dplyr::select(TextClass, elevbin,soilindex2) %>% right_join(Dout4, by= c("soilindex2" = "soilindex2"))
Dout4 %>% inner_join(updateFreq)

names(Dcomb)[28]
names(Dcomb)[28] <- "OBJECTID" #renaming this variable with a bunch of stuff in front of it to "OBJECTID"

Dcombout <- Dcomb %>% dplyr::select(mukey,OBJECTID,Shape_Area,elev,elevbin,soilindex) %>% inner_join(soilsRemapDf, by=c("soilindex"="soilindex")) %>%
  arrange(OBJECTID)

## Write some output if you want --> You will want to join Dcombout to your merged soil shapefile, linking Object ID's
## Individual polygons (rows in your shapefile) can have the same MUKEY, but different soilindex2 based on mean Elevation by ObjectID.
#write.csv(Dcomb, file=outfile,row.names=FALSE)
#write.csv(Dcombout,"data\\VT-MA_soilindex2.csv",row.names=F)
#write.csv(Dout3,"data\\VT-MA_soilindex_means.csv",row.names=F)
#write.csv(Dout4,"data\\VT-MA_soilindex2remap_means.csv",row.names=F)

#Scale mean soil vars to mean=1 sd=0 to plot class comparisons
Dout4norm <- Dout4 %>% mutate(across(fieldcap:elev, ~scale(.x))) # ??where is across function from?

# Vizualize class soil variable means
plot(c(1,11), c(-4,4), xlab = "soil var", ylab = "Scaled mean", type = "n")
abline(h=0,lty=2)
for (i in 1:nrow(Dout4norm)) {
  lines(1:11, Dout4norm[i,4:ncol(Dout4norm)], type = "l", col = i)
  points(1:11, Dout4norm[i,4:ncol(Dout4norm)], pch = 21, bg = i, cex=1.5)  
}

text(1:11,rep(-3.5, 10), labels = names(Dout4norm)[4:ncol(Dout4norm)], srt = 90)

## Stopped here, need to continue with the rest...


# If you Need to add new Wilting Point values from w15bar SSURGO variable - may need to redo steps above
library(sp)
library(rgdal)
library(sf)


# Try visualizing soils classes you just created by plotting in sf format
# Read in shapefile of merged county data - use sf
shape <- st_read("data//soil_mu_a_vtma_mu_summary_elev_by_oid.shp")


shape <- shape %>% inner_join(Dcombout, by = c("OBJECTID" = "OBJECTID", "mukey_num" = "mukey"))

# Create a dataframe with the soilindex2 number and categorical colors generated by sf.colors
soilindex2 <- sort(unique(Dout4$soilindex2))
soilCols <- sf.colors(n = length(soilindex2), categorical = T)
soilCols <- data.frame(soilindex2,soilCols)

shape <- shape %>% inner_join(soilCols)

# plot polygon of soilindex 2 class, no outlines
plot(shape["soilindex2"],pch=20,cex=2,key.pos=1,col=shape$soilCols, lty=0)


# Read in shapefile of merged county data 
#soilPoly <-readOGR(dsn="data",layer="soil_mu_a_vtma_mu_summary_elev_by_oid")
# Read previously calculated means (with issues) for mean lat and long

# Now add mean Lat and Long calculated previously
#latlong <- latlong0 %>% select(soilindex2=soil,lat=lat,long=long) %>% mutate(soilindex2 = as.character(soilindex2))
#Dout5 <- Dout5 %>% inner_join(latlong)
