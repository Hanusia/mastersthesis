# --------------------------------------------------
# LANDIS initial communities preparation of MY plot data
# 19 Nov 2021
# HH
# --------------------------------------------------
#

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)
library(tidyverse)

#update: Jane said NO NEED TO INCLUDE STUMPS for this step, which makes things easier!

#calculating BA for live trees using standard conversion from DBH to BA

overstory_data$BA_sqm <- overstory_data$DBH_cm*overstory_data$DBH_cm*pi/40000

# Overstory aggregation for LANDIS- initial community initialization step -------------------------------
#for this purpose- using live trees NO LONGER STUMPS, aggregated by PLOT and SPECIES

#first step- subset the data needed from overstory_data DF
overstory_LANDISprep <- overstory_data[,c("plot_ID", "species", "status", "BA_sqm")]

#now selecting out only LIVE trees
overstory_LANDISprep<- overstory_LANDISprep[overstory_LANDISprep$status=="live",]

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
#update: Jane says YES on this

#OK changed my mind- I'm gonna subset by location 1st!

all_BA_LANDIS <- merge(x=over1in_BA_LANDIS, y=plot_info[,c("plot_ID", "Stand_name")], all.x=TRUE)
all_BA_LANDIS <- merge(x=all_BA_LANDIS, y=stand_info[,c("Stand_name", "County")], all.x=TRUE)
all_BA_LANDIS <- all_BA_LANDIS[all_BA_LANDIS$County %in% c("Bennington", "Berkshire", "Orleans", "Caledonia"),]
#subsetting complete!

#next step: import waypoints
my_LANDIS_waypoints <- read.csv("C:/Users/theha/Documents/waypoint_textfiles/LANDIS_waypoints.csv", fileEncoding = "UTF-8-BOM")
#NOTE: will NOT append/merge these until the data is in WIDE format (1 row per plot)- 
#before I do that, 1st need to rename species, probably?!!?

all_BA_LANDIS$plot_ID[!(all_BA_LANDIS$plot_ID %in% my_LANDIS_waypoints$ident)]
#UPDATE: I ALSO need all the lat/longs from GMSF, and others in this list below. 
#(a few of them are truly missing, will need to track down in Avenza screenshots I think)
#this list includes: OMSFCRgap1, MP1314mat1, MP19gap4
#OK that's not so bad!!
#update 11-19: got these additional data points, woo!!

#NOW BACK TO SPECIES SHIT
#let's upload what I have so far & then see where we're at.....

#first things first (I'm the realest (JK!!))
#see which of my species codes from the SUBSET I'm using aren't in MY list...
#all_BA_LANDIS$species[!(all_BA_LANDIS$species %in% my_species_table$spec_code)]

#ok, executive decision: filtering out any dead saplings!
throwaway <- grep(pattern="\\w+-dead", x=all_BA_LANDIS$species, value=TRUE, fixed=FALSE, perl=TRUE)
# I HAD TO FIDDLE WITH THIS SO MUCH BUT THIS VERSION OF IT FINALLY WORKED!!!!
head(throwaway)
length(throwaway)
#actually NOT throwaway b/c I'm actually using it now to subset...
#and getting rid of dead sapling "species"
all_BA_LANDIS <- all_BA_LANDIS[!(all_BA_LANDIS$species %in% throwaway),]


#OK next steps/to ask Jane tomorrow: -> YES
#-should I delete the "dead" saplings?
#what about trees that were only ID-ed to the genus level or unknown- get rid of those, too?? -> not yet?
#what about if I can't find lat/long data for some of them? -> figured it out
#action items for ME tomorrow: first and biggest is finding out species list I want to include,
#next is finding lost lat/longs
#and fleshing out my species codes key

#continuing on 11/19:
#let's try connecting to species list now!

#throwawayagain <- data.frame("spec_code" = unique(all_BA_LANDIS$species))
#species_test <- merge(x=throwawayagain, y=my_species_table, all=TRUE,
#                      by="spec_code")
#View(species_test)

#now for real...
all_BA_LANDIS_sp <- merge(x=all_BA_LANDIS, 
                       y=my_species_table[,c("spec_code", "common_name")], 
                       by.x="species", by.y="spec_code", all.x=TRUE
                       )
all_BA_LANDIS_sp <- merge(x=all_BA_LANDIS_sp,
                          y=FIA_species_table[,c("FIA.Code", "Common.Name")], 
                          by.x="common_name", by.y="Common.Name", all.x=TRUE
                        )

#see which ones DIDN'T have their FIA codes associated...
unique(all_BA_LANDIS_sp[is.na(all_BA_LANDIS_sp$FIA.Code), "common_name"])

#now, exclude the shrubs & unknowns! leave in "just to species level" FOR NOW.
undesirables <- c("alternate-leafed dogwood", "unknown", "witch hazel (SHRUB)")
all_BA_LANDIS_sp <- all_BA_LANDIS_sp[!(all_BA_LANDIS_sp$common_name %in% undesirables),]
#this worked as I wanted it to. Excellent!

#next, I need to give the "FIA code" to the 'just genus' ones
just_genus <- c("birch species", "elm species", "maple species", "spruce species")

#doing a lil for loop b/c I'm too tired to think of another way to do this lol
for(i in 1:length(just_genus)) {all_BA_LANDIS_sp$FIA.Code[all_BA_LANDIS_sp$common_name==just_genus[i]] <- just_genus[i]}

#then pivot_wider again
#first subsetting to only the columns we need now
all_BA_LANDIS_wide <- all_BA_LANDIS_sp[,c("plot_ID", "FIA.Code", "total_BA_ha")]
all_BA_LANDIS_wide <- pivot_wider(data=all_BA_LANDIS_wide,
                                  names_from="FIA.Code",
                                  values_from="total_BA_ha")
#next, need to replace NAs with 0s
all_BA_LANDIS_wide[is.na(all_BA_LANDIS_wide)] <- 0

#and finally attach lat and long

all_BA_LANDIS_wide <- merge(x=all_BA_LANDIS_wide, 
                            y=my_LANDIS_waypoints[,c("ident", "Latitude", "Longitude")],
                            all.x=TRUE,
                            by.x="plot_ID", by.y="ident")

#we did it joe!!!
#let's check:
#sum(is.na(all_BA_LANDIS_wide$Longitude))

write.csv(x=all_BA_LANDIS_wide, file="LANDIS_stuff/Hanusiaplots_BA_sp_LANDISinit_19Nov2021.csv")
