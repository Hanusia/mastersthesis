# --------------------------------------------------
# LANDIS initial communities preparation of MY plot data
# 4 May 2022
# HH
# --------------------------------------------------
#

#MODIFYING THE BELOW CODE to include ONLY trees at least 5" DBH,
#to be consistent with FIA data!!
# 5" is equivalent to 12.7 cm.


stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)
library(tidyverse)

#update: Jane said NO NEED TO INCLUDE STUMPS for this step, which makes things easier!

#5/4 change here: FILTERING OUT TREES UNDER 5" DBH
nrow(overstory_data) #5439
overstory_data <- overstory_data[overstory_data$DBH_cm>=12.7,]
nrow(overstory_data) #4785

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
#update: not actually doing this @ this stage, just later one due to other code things from the past

#now, to aggregate to plot level w/ only the species...
overstory_LANDISprep <- overstory_LANDISprep[,c("plot_ID", "species", "BA_sqm")]

plot_BA_LANDIS <- aggregate(BA_sqm ~ plot_ID*species, data=overstory_LANDISprep, FUN=sum)
#renaming this one column
names(plot_BA_LANDIS)[3] <- "BA_sqm_overstory"

###
##ALSO TAKING OUT ALL THE SAPLINGS CODE SINCE WE NO LONGER NEED IT.
#(See the precursor code file to this one, from 11-19-21, for that code.)
###

#check what we've got so far:
View(plot_BA_LANDIS)
over5in_BA_LANDIS <- plot_BA_LANDIS #rename for the code that follows

#next, convert NAs to 0s
over5in_BA_LANDIS[is.na(over5in_BA_LANDIS)] <- 0
#next, calculate BA per HA (per plot):
over5in_BA_LANDIS$BA_sqm_ha <- over5in_BA_LANDIS$BA_sqm_overstory/0.04 #dividing by area of each plot...

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
all_BA_LANDIS <- merge(x=over5in_BA_LANDIS, y=plot_info[,c("plot_ID", "Stand_name")], all.x=TRUE)
all_BA_LANDIS <- merge(x=all_BA_LANDIS, y=stand_info[,c("Stand_name", "County")], all.x=TRUE)
all_BA_LANDIS <- all_BA_LANDIS[all_BA_LANDIS$County %in% c("Bennington", "Berkshire", "Orleans", "Caledonia"),]
#subsetting complete!
View(all_BA_LANDIS)

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

### DON'T NEED THE CODE that filters out dead saplings b/c we aren't using saplings now!!
### again, see past file from 11-19-21 for details on that.
#ok, executive decision: filtering out any dead saplings!

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
#which now (AS IN, AS OF MAY 4TH 2022) only consist of:
#"birch species" and "maple species"
all_BA_LANDIS_sp$FIA.Code[all_BA_LANDIS_sp$common_name=="birch species"] <- 370
all_BA_LANDIS_sp$FIA.Code[all_BA_LANDIS_sp$common_name=="maple species"] <- 310
#looks gucci!

#then pivot_wider again
#first subsetting to only the columns we need now
over5in_BA_LANDIS_wide <- all_BA_LANDIS_sp[,c("plot_ID", "FIA.Code", "BA_sqm_ha")]
over5in_BA_LANDIS_wide <- pivot_wider(data=over5in_BA_LANDIS_wide,
                                  names_from="FIA.Code",
                                  values_from="BA_sqm_ha")
#next, need to replace NAs with 0s
over5in_BA_LANDIS_wide[is.na(over5in_BA_LANDIS_wide)] <- 0

#and finally attach lat and long

over5in_BA_LANDIS_wide <- merge(x=over5in_BA_LANDIS_wide, 
                            y=my_LANDIS_waypoints[,c("ident", "Latitude", "Longitude")],
                            all.x=TRUE,
                            by.x="plot_ID", by.y="ident")

#we did it joe!!!
#let's check:
#sum(is.na(over5in_BA_LANDIS_wide$Longitude))

write.csv(x=over5in_BA_LANDIS_wide, file="LANDIS_stuff/Hanusiaplots_BA_sp_OVER5INDBH_LANDISinit_04May2022.csv")
