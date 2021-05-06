## This script reads SSURGO data to extract cut points for slope and depth
## The cut points are applied to SSURGO data to create ecoregion classes
## Classes represent combinations of soil texture, slope and depth

library(Hmisc)
library(plyr)
library(dplyr)

options(tibble.width = Inf)
# Read SR SSURGO
## CHECK PATH
#D2 <- read.csv("G:\\Adirondacks\\soils\\Analysis\\soil_merge2.csv") 
D2 <- read.csv("~/mastersthesis_repo/mastersthesis/LANDIS_stuff/mu_summary_wilt.csv", header=T, fileEncoding = "UTF-8-BOM")
#elev <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\data_gee\\ADK_DEM_by_soil_oid.csv",header=T)



D2$FID <- as.numeric(row.names(D2)) - 1
D2$slopegradw <- as.numeric(as.character(D2$FirstOfslopegradwta))
D2$brockdepmi <- as.numeric(as.character(D2$FirstOfbrockdepmin))
D2$ksat <- as.numeric(as.character(D2$SumOfksat_weight1))
D2$sand <- as.numeric(as.character(D2$SumOfsand_weight1))
D2$silt <- as.numeric(as.character(D2$SumOfsilt_weight1))
D2$clay <- as.numeric(as.character(D2$SumOfclay_weight1))
D2$fieldcap <- as.numeric(as.character(D2$SumOffieldcap_weight1))
D2$maxdepth <- as.numeric(as.character(D2$SumOfmaxdepth_weight))
D2$compsum <- D2$sand + D2$silt + D2$clay
D2$sand <- D2$sand / D2$compsum * 100
D2$silt <- D2$silt / D2$compsum * 100
D2$clay <- D2$clay / D2$compsum * 100
D2$TextClass <- "None"
D2 <- D2[D2$MUSYM != "W",]
attach(D2)

# Extract cut points from Savage River data
slopecuts <- quantile(D2$slopegradw,probs=seq(0,1,1/3),na.rm=T)[2:3]
depthcuts <- quantile(D2$maxdepth,probs=seq(0,1,1/3),na.rm=T)[2:3]

# Calc texture classes
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

# Apply cut points
D2$slopebin <- as.numeric(cut2(D2$slopegradw,unique(slopecuts)))
D2$depthbin <- as.numeric(cut2(D2$maxdepth,unique(depthcuts)))

# Identify unique combinations
Dout <- plyr::count(D2[,c("depthbin","TextClass","slopebin")])

# Ecoregion index
Dout$Index <- row.names(Dout)

# Apppend ecoregion classes to SSURGO data
Dcomb <- merge(D2,Dout,by=c("TextClass","depthbin","slopebin"))
## CHECK PATH
#outfile <- "C:/BRM/LANDIS_II/Projects/SavageRiver/savageriver_eco_index_031717.csv"
#write.csv(Dcomb, file=outfile,row.names=FALSE)

adkmerge <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soil_index_merget.csv",header=T)
names(adkmerge)

elev2 <- elev %>% dplyr::select("oid","mean") %>% dplyr::rename(elev = mean)

adk2 <- adkmerge %>% inner_join(elev2,by=c("OBJECTID"="oid"))

# Figure out new cut points with elevation, just for study area MUSYM
slopecuts2 <- quantile(adk2$slopegradw,probs=seq(0,1,1/3),na.rm=T)[3]
depthcuts2 <- quantile(adk2$maxdepth,probs=seq(0,1,0.2),na.rm=T)[2:3]
depthcuts2 <- c(30)
elevcuts2 <- quantile(adk2$elev,probs=seq(0,1,0.1))[c(6,10)]

# Apply cut points
adk2$slopebin2 <- as.numeric(cut2(adk2$slopegradw,unique(slopecuts2)))
adk2$depthbin2 <- as.numeric(cut2(adk2$maxdepth,unique(depthcuts2)))
adk2$elevbin2 <- as.numeric(cut2(adk2$elev,unique(elevcuts2)))

# Identify unique combinations
Dout <- plyr::count(adk2[,c("depthbin2","texcl","elevbin2")])

# Ecoregion index
Dout$soilindex2 <- row.names(Dout)

# Apppend ecoregion classes to SSURGO data
Dcomb <- merge(adk2,Dout,by=c("texcl","depthbin2","elevbin2"))

# Compute weighted mean of soil variables (weighted by polygon area)
Dout2 <- Dcomb %>% group_by(soilindex2) %>% summarize_at(vars(fieldcap,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth,elev),funs(weighted.mean(., Shape_Area,na.rm=T)))
Dout3 <- Dout %>% inner_join(Dout2) %>% dplyr::select(soilindex2,texcl,depthbin2,elevbin2,elev,freq,fieldcap:maxdepth)
Dcombout <- Dcomb %>% dplyr::select(OBJECTID,elev,slopebin2,elevbin2,depthbin2,soilindex2) %>%
            arrange(OBJECTID)
soilindex2remap <- Dcombout$soilindex2
soilindex2remap[which(soilindex2remap == 4)] <- 5
soilindex2remap[which(soilindex2remap == 11)] <- 10
soilindex2remap[which(soilindex2remap == 14)] <- 13
soilindex2remap[which(soilindex2remap == 17)] <- 16
soilindex2remap[which(soilindex2remap == 24)] <- 23
soilindex2remap[which(soilindex2remap == 31)] <- 32
soilindex2remap[which(soilindex2remap == 34)] <- 33
soilindex2remap[which(soilindex2remap == 38)] <- 39
soilindex2remap[which(soilindex2remap == 41)] <- 40
soilindex2remap[which(soilindex2remap == 42)] <- 43

Dcombout$soilindex2remap <- soilindex2remap
Dout4 <- Dout3 %>% filter(soilindex2 %in% unique(soilindex2remap))
freq4 <- data.frame(table(soilindex2remap))
Dout4 <- Dout4 %>% inner_join(freq4,by=c("soilindex2" = "soilindex2remap"))
## CHECK PATH
#outfile <- "C:/BRM/LANDIS_II/Projects/SavageRiver/savageriver_eco_index_031717.csv"
#write.csv(Dcomb, file=outfile,row.names=FALSE)
#write.csv(Dcombout,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2.csv",row.names=F)
#write.csv(Dout3,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2_means.csv",row.names=F)
#write.csv(Dout4,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2remap_means.csv",row.names=F)

aggregate(x=Dcomb,by=list(Dcomb$depthbin),FUN="mean")

# Need to add new Wilting Point values from w15bar SSURGO variable
library(sp)
library(rgdal)

# Function to cast variables stored as a factor to numeric in the correct way
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.char.factor <- function(x) {as.character(levels(x))[x]}

# Read in shapefile of merged county data 
w15bar <-readOGR(dsn="C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\ssurgo_from_Emma-20180731",layer="ny6_merge_wc15bar")
# Read previously calculated means (with issues) for mean lat and long
latlong0 <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\ADK_soils2_means_long_lat_wcs15bar.csv",header=T)


w15bardf <- w15bar@data
w15bardf <- w15bardf %>% select(-OBJECTID,-MUKEY)

# Loopt through and Convert Factors to character for both tables
for (i in 1:dim(w15bardf)[2]) {
  if (is.factor(w15bardf[,i])) {
    w15bardf[,i] <- as.char.factor(w15bardf[,i])
  }
}
str(w15bardf)
w15bardf$SPATIALVER <- as.numeric(w15bardf$SPATIALVER)

for (i in 1:dim(Dcomb)[2]) {
  if (is.factor(Dcomb[,i])) {
    Dcomb[,i] <- as.char.factor(Dcomb[,i])
  }
}

# Create a new MUKEY field to collate all the MUKEYS from previously joined tables
Dcomb$MUKEY_3 <- NA
mukey_cols <- c(grep("mukey",names(Dcomb),grep("MUKEY",names(Dcomb))))
nmukeys <- c()
mukeys <- list()

for (i in 1:dim(Dcomb)[1]) {
  mukey_vals <- unique(as.character(Dcomb[i,mukey_cols]))
  nchar_vals <- nchar(mukey_vals)
  mukey_vali <- mukey_vals[which(nchar_vals > 2)][1]
  Dcomb$MUKEY_3[i] <- mukey_vali
  nmukeys <- c(nmukeys,length(which(nchar_vals > 2)))
  mukeys <- c(mukeys,mukey_vals[which(nchar_vals > 2)])
}
table(nmukeys)

w15bardf2 <- w15bardf %>% select(MUKEY_1,WC15Bar) %>% filter(MUKEY_1 %in% sort(unique(Dcomb$MUKEY_3)))

# Join tables to get Wilting point summarized
Dcomb2 <- Dcomb %>% select(-OBJECTID) %>% left_join(w15bardf2,by=c("MUKEY_3"="MUKEY_1")) %>% distinct(.)
Dcomb3 <- Dcomb2 %>% filter(nchar(MUKEY_3) > 2)
dim(Dcomb3)

#Dcomb3 <- Dcomb3 %>% filter(!(ksat==0 & sand==0 & clay==0 & fieldcap==0 & maxdepth==0))
# Two classes (1,2) are affected by lack of attributes associated with unfinished MUKEYS in Franklin County
# Replace soil means for those classes with means calculated here 
w15barout2 <- Dcomb3 %>% group_by(soilindex2) %>% summarize_at(vars(elev,fieldcap,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth,WC15Bar,freq.y),funs(weighted.mean(., Shape_Area,na.rm=T))) %>%
              arrange(as.numeric(soilindex2)) %>% filter(soilindex2 %in% Dout4$soilindex2)
w15out <- w15barout2 %>% select(soilindex2,WC15Bar)
new12means <- w15barout2 %>% filter(soilindex2 %in% c("1","2")) %>% select(elev,freq.y,fieldcap,slopegradw,brockdepmi,ksat,sand,silt,clay,maxdepth)
Dout5 <- Dout4 %>% inner_join(w15out)
Dout5[1:2,min(which(names(Dout5) %in% names(new12means))):max(which(names(Dout5) %in% names(new12means)))] <- new12means

# Now add mean Lat and Long calculated previously
latlong <- latlong0 %>% select(soilindex2=soil,lat=lat,long=long) %>% mutate(soilindex2 = as.character(soilindex2))
Dout5 <- Dout5 %>% inner_join(latlong)
#write.csv(Dout5,"C:\\Users\\janer\\Dropbox\\Projects\\Adirondacks\\gis\\adk_soilindex2remap_means_w_wiltingpoint.csv",row.names=F)

