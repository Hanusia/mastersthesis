# --------------------------------------------------
# Creating explanatory (secondary) matrix for NMS analysis in PC-ORD
# 02 Feb 2021
# HH
# --------------------------------------------------
#

## IMPORTANT!!!!!!!!!!!!! ##
## THIS IS NOW MOVED TO 1-19-20_PC-ORD_prep_code SINCE IT WAS INEFFECTIVE TO HAVE A SEPARATE SCRIPT ANYWAY!!!!

#Per Jeri's book: Explanatory matrix not needed to run NMS (i.e. the initial free ordination), BUT needed to INTERPRET it!
#Important- explanatory variables evaluted independently, so just throw anything on there to try it out if it might be explanatory
#Structure- rows are the same as main matrix (e.g. sample units- in my case, "Sites")

#Step 1. Establish the matrix w/ rows as sites, same as primary (response) matrix.
#Step 2. Start adding variables as columns. (Can always add more on later.) For now:
  # Harvest status (categorical [C])
  #Ash harvest intensity (Quantitative [Q]) -measured by BA/ha of ash removed per site, I think?
    #remember to exclude stumps of higher decay classes- likely just fell, not harvested!
  # Presence/absence of EAB?? (C) AND/OR # of EAB markers present as a tally of the info we collected (Q)
  # Total harvest intensity (Q)- maybe also separated out by each species?? (Ask Tony)
  # Harvest type- based on that summary spreadsheet I sent Tony (C)- Is this a good idea??
  # Climate variables? Latitude? Altitude? Soil type??
  # Forest type- n. hardwood vs rich n. hardwood vs oaky hardwood? (C) How to delineate those?

#VARIABLES TONY SUGGESTED:
#% ash removal, total % ash in stand, state, year harvested, type of tx, beech component as proxy for forest type, % BA removed overall


stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv")

library(tibble)
library(ggplot2)

#changing name of plot_ID column since it had some weird characters in it
names(overstory_data)[1] <- "plot_ID"
names(plot_info)[1] <- "plot_ID"


#ditto for the first column, "Property" in stand_info
names(stand_info)[1]<- "Property"

overstory_plus <- merge(x=overstory_data,y=plot_info,by="plot_ID")

#converting stump height & diam to DBH- using James Westfall paper from USFS

overstory_plus$ASH_stump_DBH <- rep(NA)
overstory_plus$BA_sqm <- rep(NA)

#calculating BA for live trees & stumps using standard conversion from DBH to BA
#overstory_plus$BA_sqft <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*0.005454

overstory_plus$BA_sqm <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*pi/40000

#only calculating the stump DBH w/ these constants for ash
for(i in nrow(overstory_plus)){
  if(overstory_plus$species[i]=="FRAM"){
    overstory_plus$ASH_stump_DBH[i] <- 
      (overstory_plus$DBH_cm[i]*((1.3716/overstory_plus$height_m[i])^-0.1074) +
         0.0685*(1.3716-overstory_plus$height_m[i]) + 0)
    # and then converting that to BA, for ASH stumps only
    overstory_plus$BA_sqm[i] <- 
      overstory_plus$ASH_stump_DBH[i]*overstory_plus$ASH_stump_DBH[i]*pi/40000 
  }
}

glimpse(overstory_plus)

#now, for my first trick...find the range of ash basal area removed in harvested sites

#then, setting up a dataframe to hold info about each plot 
#w/ its basic stats, info abt ash removed, etc.

ash_cut <- data.frame("plot_ID" = plot_info$plot_ID, 
                      "harvest_status" = plot_info$harvest_status, 
                      "gap_status" = plot_info$gap_status, 
                      "live_ash_sum"=rep(0), #starting @ zero so they can be added to
                      "cut_ash_sum"=rep(0) #ditto
                      #,"any_live_ash"=rep("NA"), #but adding a separate var to tell
                      #"any_cut_ash"=rep("NA") #whether it actually went thru /was counted
)

#glimpse(ash_cut)

for (i in 1:nrow(overstory_plus)){ #iterating thru each tree/stump/snag in all plots
  
  #basically, adding the DBH of each live ash tree in a plot to the sum value
  if(overstory_plus$species[i]=="FRAM"){
    if(overstory_plus$status[i]=="live"){
      ash_cut$live_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] <- 
        ash_cut$live_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] +
        overstory_plus$BA_sqm[i]
      #using BA calculated from DBH calculated w/ the formula I used above!
    } else if(overstory_plus$status[i]=="stump") {
      ash_cut$cut_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] <- 
        ash_cut$cut_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] +
        overstory_plus$BA_sqm[i]
    }
  }
  
}

# Matrix of overstory parameters for stump to BA -------------------------------
#numbers/parameters from Westfall 2010
stump_to_DBH <- data.frame(
  "species" = unique(overstory_plus$species),
  "sp_group" = rep(NA),
  "B0" = rep(NA),
  "B1" = rep(NA),
  "estimates" = rep(FALSE)
)
stump_to_DBH$species

#assigning the values for each species
stump_to_DBH[stump_to_DBH$species=="ACSA",2] <- 7
stump_to_DBH[stump_to_DBH$species=="FRAM",2] <- 9
stump_to_DBH[stump_to_DBH$species=="OSVI",2] <- 11
stump_to_DBH[stump_to_DBH$species=="UNK",2] <- 0
stump_to_DBH[stump_to_DBH$species=="QURU",2] <- 14
stump_to_DBH[stump_to_DBH$species=="CAOV",2] <- 16
stump_to_DBH[stump_to_DBH$species=="FAGR",2] <- 12
stump_to_DBH[stump_to_DBH$species=="BEAL",2] <- 11
stump_to_DBH[stump_to_DBH$species=="ACRU",2] <- 18
stump_to_DBH[stump_to_DBH$species=="BENI",2] <- 11
stump_to_DBH[stump_to_DBH$species=="PRSE",2] <- 10
stump_to_DBH[stump_to_DBH$species=="TSCA",2] <- 4
stump_to_DBH[stump_to_DBH$species=="ACPE",2] <- 18
stump_to_DBH[stump_to_DBH$species=="TIAM",2] <- 13
stump_to_DBH[stump_to_DBH$species=="FRNI",2] <- 9
stump_to_DBH[stump_to_DBH$species=="BEPA",2] <- 11
stump_to_DBH[stump_to_DBH$species=="PRPE",2] <- 10
stump_to_DBH[stump_to_DBH$species=="POGR",2] <- 9
stump_to_DBH[stump_to_DBH$species=="ROPS",2] <- 17
stump_to_DBH[stump_to_DBH$species=="PODE",2] <- 9
stump_to_DBH[stump_to_DBH$species=="ACSP",2] <- 18
stump_to_DBH[stump_to_DBH$species=="ABBA",2] <- 3
stump_to_DBH[stump_to_DBH$species=="PIRU",2] <- 2
stump_to_DBH[stump_to_DBH$species=="POTR",2] <- 9
stump_to_DBH[stump_to_DBH$species=="PIST",2] <- 1
stump_to_DBH[stump_to_DBH$species=="BESP",2] <- 11
stump_to_DBH[stump_to_DBH$species=="BEPO",2] <- 11
stump_to_DBH[stump_to_DBH$species=="ULAM",2] <- 17
stump_to_DBH[stump_to_DBH$species=="CACO",2] <- 16
stump_to_DBH[stump_to_DBH$species=="HAVI",2] <- 0
stump_to_DBH[stump_to_DBH$species=="POSP",2] <- 9
stump_to_DBH[stump_to_DBH$species=="PISP",2] <- 2
stump_to_DBH[stump_to_DBH$species=="ACER",2] <- 18
stump_to_DBH[stump_to_DBH$species=="THOC",2] <- 6
stump_to_DBH[stump_to_DBH$species=="CACA",2] <- 16
stump_to_DBH[stump_to_DBH$species=="ULSP",2] <- 17
stump_to_DBH[stump_to_DBH$species=="BELE",2] <- 11
stump_to_DBH[stump_to_DBH$species=="ACSP2",2] <- 18
stump_to_DBH[stump_to_DBH$species=="ULRU",2] <- 17

unique(stump_to_DBH$sp_group)
stump_to_DBH


###well...I fixed it...and it was the easiest thing in the world...lol
for(i in 1:nrow(stump_to_DBH)){
  if(is.na(stump_to_DBH$sp_group[i])==FALSE){
    if(stump_to_DBH$sp_group[i]==7){
      stump_to_DBH$B0[i] <- -0.1323
      stump_to_DBH$B1[i] <- .2442
    } else if(stump_to_DBH$sp_group[i]==9){
      stump_to_DBH$B0[i] <- -0.1074
      stump_to_DBH$B1[i] <- .0685
    } else if(stump_to_DBH$sp_group[i]==11){
      stump_to_DBH$B0[i] <- -.1743
      stump_to_DBH$B1[i] <- .1376
    }else if(stump_to_DBH$sp_group[i]==14){
      stump_to_DBH$B0[i] <- -0.1651
      stump_to_DBH$B1[i] <- .1258 #PROJECTION based on group 17
      stump_to_DBH$estimates[i] <- TRUE
    }else if(stump_to_DBH$sp_group[i]==16){
      stump_to_DBH$B0[i] <- -.1578
      stump_to_DBH$B1[i] <- .0615
    }else if(stump_to_DBH$sp_group[i]==12){
      stump_to_DBH$B0[i] <- -.1171
      stump_to_DBH$B1[i] <- .0714
    }else if(stump_to_DBH$sp_group[i]==18){
      stump_to_DBH$B0[i] <- -.1382
      stump_to_DBH$B1[i] <- .1010
    }else if(stump_to_DBH$sp_group[i]==10){
      stump_to_DBH$B0[i] <- -.0720
      stump_to_DBH$B1[i] <- .0260 #PROJECTION based on group 8!
      stump_to_DBH$estimates[i] <- TRUE
    }else if(stump_to_DBH$sp_group[i]==4){
      stump_to_DBH$B0[i] <- -.1162
      stump_to_DBH$B1[i] <- .0686
    }else if(stump_to_DBH$sp_group[i]==13){
      stump_to_DBH$B0[i] <- -.1193
      stump_to_DBH$B1[i] <- .1009
    }else if(stump_to_DBH$sp_group[i]==17){
      stump_to_DBH$B0[i] <- -.1662
      stump_to_DBH$B1[i] <- .1258
    }else if(stump_to_DBH$sp_group[i]==3){
      stump_to_DBH$B0[i] <- -.1353
      stump_to_DBH$B1[i] <- .1451
    }else if(stump_to_DBH$sp_group[i]==2){
      stump_to_DBH$B0[i] <- -.1334
      stump_to_DBH$B1[i] <- .0740
    }else if(stump_to_DBH$sp_group[i]==1){
      stump_to_DBH$B0[i] <- -.1096
      stump_to_DBH$B1[i] <- .0588
    }else if(stump_to_DBH$sp_group[i]==6){
      stump_to_DBH$B0[i] <- -.1631
      stump_to_DBH$B1[i] <- .1517
    }else if(stump_to_DBH$sp_group[i]==0){ #for those who we just don't have an estimate......
      stump_to_DBH$B0[i] <- median(as.numeric(stump_to_DBH$B0),na.rm=TRUE)
      stump_to_DBH$B1[i] <- median(as.numeric(stump_to_DBH$B1),na.rm=TRUE)
      stump_to_DBH$estimates[i] <- TRUE
    }
  }
}
stump_to_DBH$sp_group[1]==7
stump_to_DBH

#now, adding a few columns to the overstory data frame
#to actually calculate this ish.

#converting to imperial units to make sure things work with these calculations
overstory_plus$diam_in <- overstory_plus$DBH_cm*.393701
overstory_plus$height_ft <- overstory_plus$height_m*3.28084
overstory_plus$stump_DBH_in <- rep(0)
overstory_plus$stump_DBH_cm <- rep(0)

#calculating stump DBH:
for(i in 1:nrow (overstory_plus)){
  if(overstory_plus$status[i]=="stump"){
    overstory_plus$stump_DBH_in[i] <- 
      (overstory_plus$diam_in[i]*(4.5/overstory_plus$height_ft[i])^
         stump_to_DBH$B0[overstory_plus$species[i]==stump_to_DBH$species]) +
      stump_to_DBH$B1[overstory_plus$species[i]==stump_to_DBH$species]*(4.5-overstory_plus$height_ft[i])
  }
}

#convert back to metric!
overstory_plus$stump_DBH_cm <- overstory_plus$stump_DBH_in*2.54

#convert diam to basal area
overstory_plus$BA_sqm[overstory_plus$status=="stump"] <- 
  overstory_plus$stump_DBH_cm[overstory_plus$status=="stump"]*overstory_plus$stump_DBH_cm[overstory_plus$status=="stump"]*pi/40000 

