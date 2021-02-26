## This script reads SSURGO data to extract cut points for slope and depth
## The cut points are applied to SSURGO data to create ecoregion classes
## Classes represent combinations of soil texture, slope and depth

library(Hmisc)
library(plyr)

# Read SR SSURGO
## CHECK PATH
D2 <- read.csv("G:\\Adirondacks\\soils\\Analysis\\soil_merge2.csv") 
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
slopecuts <- quantile(D2$slopegradw,probs=seq(0,1,0.2))[2:6]
depthcuts <- quantile(D2$maxdepth,probs=seq(0,1,0.2))[2:6]

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
Dout <- count(D2[,c("depthbin","TextClass","slopebin")])

# Ecoregion index
Dout$Index <- row.names(Dout)

# Apppend ecoregion classes to SSURGO data
Dcomb <- merge(D2,Dout,by=c("TextClass","depthbin","slopebin"))
## CHECK PATH
outfile <- "C:/BRM/LANDIS_II/Projects/SavageRiver/savageriver_eco_index_031717.csv"
write.csv(Dcomb, file=outfile,row.names=FALSE)


aggregate(x=Dcomb,by=list(Dcomb$depthbin),FUN="mean")
