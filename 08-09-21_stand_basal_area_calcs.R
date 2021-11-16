# --------------------------------------------------
# Basal area per stand calculations
# 09 Aug 2021
# HH
# --------------------------------------------------
#
#Repeating/copying/adding to some of the code from previous scripts, 
#to create my own "clean" script producing a CSV 
#& easily have this info on hand.

##############################################
#Below is copied from 1-19-20_PC-ORD_prep_code


stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)
library(tidyverse)

#glimpse(overstory_data)
#glimpse(plot_info)
#glimpse(stand_info)

##############################################

#merging the overstory df w/ the plot_info df on the basis of the plot_ID column
overstory_plus <- merge(x=overstory_data,y=plot_info,by="plot_ID")

#converting stump height & diam to DBH- using James Westfall paper from USFS
#IMPORTANT: these values are specific to white ash; dif species have dif constants!!
#IMPORTANT QUESTION: can I use measurements in cm for this?! I don't see why not...
#altho ACTUALLY we'd have to convert stump height-dbh relation to all meters or all feet.
#variables for this formula: dbhi = di * (4.5/hi)^B0 + B1(4.5-h) + Ei
#dbhi = estimated DBH for tree 'i'
#di = stump diameter (in.) for tree i;
#hi = stump height (ft.) for tree i;
#B0 & B1 = estimated fixed-effects parameters; 
#(for white ash, B0 = -0.1074 & B1 = 0.0685)
#Ei = random error for tree i

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

#first, creating a factor list of all the individual plot names
plot_names <- unique(overstory_plus$plot_ID)

#trying to find our "missing" plot_ID...
#length(unique(overstory_data$plot_ID)) #=/= the next line
#length(unique(plot_info$plot_ID))

#now subsetting to find out which plot_ID in plot_info is NOT in overstory_data

#subset(plot_info, !(plot_ID %in% overstory_data$plot_ID))

#length(plot_names)
#head(plot_names)
#stand_info$num_plots[stand_info$Stand_code=="GSF396"]
#now that that's resolved (# of individual plot_IDs), I've commented out the unnecessary lines

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

#checking to see if it worked!!
#glimpse(ash_cut)
#glimpse(ash_cut[ash_cut$harvest_status=="YES",])
#I think it worked!!

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

#as of this point, overstory_plus$BA_sqm contains the BA for each 
#live tree, snag, AND stump (projected) in each stand. BUT not yet aggregated @ the stand level.

# #now to aggregate @ the stand level -------------------------------


#I'll try it this way:
#aggregate function, summing up BA_sqm by stand name
#sooo much simpler than doing this with a for loop!!
BA_stand_sum <- aggregate(BA_sqm ~ Stand_name, data=overstory_plus, FUN=sum)
BA_stand_sum #this is the TOTAL (NOT per ha!) basal area of all trees, live/snag/stump (pre-harvest totals), in a stand

#testing the aggregate:
# sum(overstory_plus$BA_sqm[overstory_plus$Stand_name=="Algonquin State Forest stand 1-14"])
#it seems to have worked!

#now to merge w/ remaining stand info: 
stand_info_plus <- merge(x=stand_info, y=BA_stand_sum, by="Stand_name")
glimpse(stand_info_plus)
#update: it worked!

#next step: calculate BA in sqm PER HECTARE
stand_info_plus$BA_sqm_ha <- stand_info_plus$BA_sqm / #total BA in square meters per stand divided by
  (.04 * stand_info_plus$num_plots) #total area (in ha) of plots sampled in that stand, calculated by # of plots * 0.04 ha/plot!

#this column is the PRE HARVEST TOTAL BA (in sqm) PER HECTARE OF ALL LIVE & DEAD TREES!!

#NOW TO EXPORT IT AS A CSV: 
write.csv(stand_info_plus, file="stand_basal_area_dataframe_09Aug2021.csv")

##updates oct. 2021: trying to finagle this to give BA per stand BY SPECIES, not just aggregate w/ all species.
BA_stand_sum_sp <- aggregate(BA_sqm ~ Stand_name*species, data=overstory_plus, FUN=sum)
View(BA_stand_sum_sp) #this worked! 
#this is the TOTAL (NOT per ha!) basal area per species of all trees, live/snag/stump (pre-harvest totals), in a stand
write.csv(BA_stand_sum_sp, file="stand_basal_area_species_df_22Oct21.csv")

# Overstory aggregation for LANDIS -------------------------------
#for this purpose- using live trees + stumps with decay class 1 or 2, aggregated by PLOT and SPECIES

#first step- subset the data needed from gargantuan overstory_plus DF
overstory_LANDISprep <- overstory_plus[,c("plot_ID", "species", "DBH_cm", "status",
                                          "decay_class", "stump_DBH_cm", "BA_sqm")]

#now selecting out only live trees + stumps
overstory_LANDISprep<- overstory_LANDISprep[overstory_LANDISprep$status!="snag",]
overstory_LANDISprep <- overstory_LANDISprep[overstory_LANDISprep$decay_class < 3 | overstory_LANDISprep$status=="live",]

#next, convert BA_sqm to BA per ha:
#overstory_LANDISprep$BA_sqm_ha <- overstory_LANDISprep$BA_sqm/0.04 #dividing by area of each plot...
#update: not actually doing this @ this stage, only once combined w/ larger sapling data

#now, to aggregate to plot level w/ only the species...
overstory_LANDISprep <- overstory_LANDISprep[,c("plot_ID", "species", "BA_sqm")]

plot_BA_LANDIS <- aggregate(BA_sqm ~ plot_ID*species, data=overstory_LANDISprep, FUN=sum)
#renaming this one column
names(plot_BA_LANDIS)[3] <- "BA_sqm_overstory"


##############################################
# now to do the same thing with large sapling data! (all over 1in diam) -------------------------------

large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

#RECALCULATING
#first need to get data into long(er) format
#maybe try using uncount function?

#first, pivot_longer
large_sapling_data <- pivot_longer(data=large_sapling_data,
                                   cols=c("class_1", "class_2", "class_3"),
                                   names_to="class",
                                   values_to="tally",
                                   names_prefix="class_")
#now, let's try uncount!
large_sapling_data <- uncount(data=large_sapling_data,
                              weights=tally) #basically, making a separate row for each observation of a variable 

#it worked! now to convert to DBH (in)
large_sapling_data$class <- as.numeric(large_sapling_data$class) #first gotta convert this to numeric
large_sapling_data$DBH_in <- large_sapling_data$class + 0.5 #assigning midpoint values - basically the class + 0.5 to get to midpoint of each class (again, in in.)
#converting to cm instead of in
large_sapling_data$DBH_cm <- large_sapling_data$DBH_in*2.54
#and converting to BA 
large_sapling_data$sapling_BA_sqm <- large_sapling_data$DBH_cm*large_sapling_data$DBH_cm*pi/40000

#now to get the "good stuff"
large_sapling_data <- large_sapling_data[,c("plot_ID", "species", "sapling_BA_sqm")]
sapling_BA_LANDIS <-aggregate(sapling_BA_sqm ~ plot_ID*species, data=large_sapling_data, FUN=sum)


##############################################
# next step is to combine these two DFs -------------------------------
#need to combine: plot_BA_LANDIS and sapling_BA_LANDIS
#by species, and by plot
#then will need to finagle how to convert their BA to BA per ha (remembering that they are on DIFFERENT AMOUNTS OF AREA)
#let's just try this with merge to start??
over1in_BA_LANDIS <- merge(x=plot_BA_LANDIS, y=sapling_BA_LANDIS,
                           all=TRUE)
#this seemed to work!!
#next, convert NAs to 0s
over1in_BA_LANDIS[is.na(over1in_BA_LANDIS)] <- 0
#next, calculate BA per HA (total):
#basically, ADD both BAs for per-plot, per-species, then DIVIDE by sum of area

#area will be: total plot area in ha (0.04) + 3*total large-sapling plot area in ha (the TOTAL of that part, including the *3, is 120 sq m or .012 ha)
#so the total area = 0.04 + 0.012 = 0.052

over1in_BA_LANDIS$total_BA_ha <- (over1in_BA_LANDIS$BA_sqm_overstory + over1in_BA_LANDIS$sapling_BA_sqm)/ 0.052
#I think this worked, woo!

#NEXT STEP is to convert this data from long to wide format...I think.
#then I will replace the species codes w/ numerical codes.
#ACTUALLY, it's probably easier to do that part first?! 

##############################################

#converting my species codes to FIA area codes!
#ALSO- forgot that I need to SUBSET my data to only counties of interest...which to do first??
#ok I think order is: 1) convert to species codes, 2) convert to wide format w/ pivot.wider, and 3) subset (basically, rows) to only those in my study area.
#then I'll add lat/long for each plot.

#ok, first need to load in MY species codes key and the FIA one
#MERGE them based on common name
#and then ASSIGN NUMERIC FIA CODES based on MY 4-letter code

#first, loading in data

FIA_species_table <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/FIA/2021 Master Species FGver9-1_9_2021_final.csv")
my_species_table <- read.csv("C:/Users/theha/OneDrive - University of Vermont/Ash project/EAB_project_2020_additional_data.csv", fileEncoding = "UTF-8-BOM")

#question: should I remove instances that are dead??

#OK changed my mind- I'm gonna subset by location 1st!

all_BA_LANDIS <- merge(x=over1in_BA_LANDIS, y=plot_info[,c("plot_ID", "Stand_name")], all.x=TRUE)
all_BA_LANDIS <- merge(x=all_BA_LANDIS, y=stand_info[,c("Stand_name", "County")], all.x=TRUE)
all_BA_LANDIS <- all_BA_LANDIS[all_BA_LANDIS$County %in% c("Bennington", "Berkshire", "Orleans", "Caledonia"),]
#subsetting complete!

#next step: import waypoints
my_LANDIS_waypoints <- read.csv("C:/Users/theha/Documents/waypoint_textfiles/LANDIS_waypoints.csv")
#NOTE: will NOT append/merge these until the data is in WIDE format (1 row per plot)- 
#before I do that, 1st need to rename species, probably?!!?

all_BA_LANDIS$plot_ID[!(all_BA_LANDIS$plot_ID %in% my_LANDIS_waypoints$ident)]
#UPDATE: I ALSO need all the lat/longs from GMSF, and others in this list below. 
#(a few of them are truly missing, will need to track down in Avenza screenshots I think)
#this list includes: OMSFCRgap1, MP1314mat1, MP19gap4
#OK that's not so bad!!

#NOW BACK TO SPECIES SHIT
#let's upload what I have so far & then see where we're at.....

#first things first (I'm the realest (JK!!))
#see which of my species codes from the SUBSET I'm using aren't in MY list...
all_BA_LANDIS$species[!(all_BA_LANDIS$species %in% my_species_table$spec_code)]

#ok, executive decision: filtering out any dead saplings!
throwaway <- grep(pattern="\\w+-dead", x=all_BA_LANDIS$species, value=TRUE, fixed=FALSE, perl=TRUE)
# I HAD TO FIDDLE WITH THIS SO MUCH BUT THIS VERSION OF IT FINALLY WORKED!!!!
head(throwaway)
length(throwaway)

#OK next steps/to ask Jane tomorrow:
#-should I delete the "dead" saplings?
#what about trees that were only ID-ed to the genus level or unknown- get rid of those, too??
#what about if I can't find lat/long data for some of them?
#action items for ME tomorrow: first and biggest is finding out species list I want to include,
#next is finding lost lat/longs
#and fleshing out my species codes key
