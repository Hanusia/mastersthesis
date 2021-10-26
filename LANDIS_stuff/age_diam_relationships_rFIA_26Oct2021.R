# --------------------------------------------------
# creating age-diameter relationships using rFIA
# 26 Oct 2021
# HH
# --------------------------------------------------
#

### note: beginning code copied from Jane's test_rFIA_to_read_merge_clip_FIA_plot_ code
library(rFIA)
library(parallel)
library(sf)
library(tidyverse)
library(stringi)
library(dplyr)
library(ggplot2)

# Detect how many cores are available on current computer.
detectCores(all.tests = F, logical = T)
# Update number of cores on your computer to run this code. This should be a subset of cores available.
nCores_my_computer <- 4
#MY computer has 8 cores available, so I will stick with 4 to run this code.

# Update study_area_prefix to a character string for output files specific to study area
study_area_prefix <- 'VT-MA' #going with this prefix for now

# Update My folder that I want to download FIA data into...update this to your directory structure.
my_fia_folder <- 'C:\\Users\\theha\\Documents\\layers_for_LANDIS\\FIA\\FIA_data'
# Update directories to write shapefiles or output data tables to
gis_dir <- 'C:\\Users\\theha\\Documents\\layers_for_LANDIS\\FIA\\gis_outputs'
out_data_dir <- 'C:\\Users\\theha\\Documents\\layers_for_LANDIS\\FIA\\data_outputs'

# This next command will download FIA data for the specified states.
# You only need to run this the first time. Comment it out unless you want to re-download.
#getFIA(c('VT', 'MA'), #I have now downloaded for VT and MA so commenting out for future runs
#      dir = my_fia_folder,
#       common = T,
#       load = T,
#       nCores = nCores_my_computer) # 

# Set common State FPs
state_fps = c(50,25) #find FP numbers at: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
state_abr = c("VT","MA")

# Read in shapefile of county boundaries for study area
cntysub <- st_read(paste(gis_dir,"\\",study_area_prefix, "_counties.shp", sep = ""))

# Determine relevant states and abreviations to sample FIA data overlapping counteis
stateFps_here = unique(cntysub$STATEFP)
stateAbr_here = state_abr[which(state_fps %in% stateFps_here)]

# Read FIA data for the states you downloaded
db <- readFIA(my_fia_folder, common = F, #changing 'common' to FALSE so I get the tables I need (specifically SITETREE)
              states = stateAbr_here, nCores = nCores_my_computer)

# Subset the state databases to counties in your shapefile
dbsub <- clipFIA(db, mask = cntysub, matchEval = T, designCD = c('1'), nCores = nCores_my_computer)

##############################################
### code below this line is mine! (Hanusia) ###

#pseudocode:
#for SITETREE table w/in dbsub (so just the counties in my study area), single out DIA and AGEDIA columns
#separate these data by SPECIES (column = SPCD)
#MAYBE: download a .csv that subsets these columns & ignores the other ones?? (so basically just plot ID, year, species, age, diam)
#GRAPH/regress diam-age for each species--start w/ a common one like sugar maple. 
#review/refer back to odom & ford for details on how they did this.
#imporant note: subset like this: dbsub$SITETREE$DIA
#seems like cu

#let's try this first with sugar maple...

#1st: extract SITETREE table to stand on its own

dbsub_sitetree <- dbsub$SITETREE
dbsub_sitetree$SPCD <- as.factor(dbsub_sitetree$SPCD) #changing species to a FACTOR variable (from continuous)
#update: only 18 species in this list- that should make things pretty easy!

allsp_agediam <- ggplot(data=dbsub_sitetree, 
                             aes(x=AGEDIA, y=DIA, shape=SPCD)) + geom_point()
#this one is plotting the relationship for ALL species, just using dif symbols for them.
allsp_agediam

##############################################
#sugar maple code (in column SPCD) is: 318
#now, for just sugar maple:
sugarmaple_agediam <- ggplot(data=dbsub_sitetree[dbsub_sitetree$SPCD==318,], 
                             aes(x=AGEDIA, y=DIA)) + geom_point()
#this one is ACTUALLY plotting the relationship for ALL species, just using dif symbols for them.
sugarmaple_agediam
#looks like I can get a pretty solid relationship here, WAHOO!!!
##############################################

#next steps:
#link species codes (SPCD column) to species names/something more intuitive for me
  #to do that, will import the FIA tree code masterlist % then merge based on codes
  #also at some point in the future, I'll need to match up FIA codes with my own 4-letter species codes.....
#will also need to convert diameter from inches to cm at some point??
  #should I do that now, or later?
#Odom and Ford used simple linear regression so I think that's gonna be my move, too!

#fist-next step: load in species name/code key to associate letter code & latin name w/ the numerical code.
fia_species_key <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/FIA/2021 Master Species FGver9-1_9_2021_final.csv")
#View(fia_species_key)
#adding/merging the variables of interest to my dataframe
dbsub_sitetree <- merge(x=dbsub_sitetree, 
                        y=fia_species_key[, c("FIA.Code", "Common.Name", "Genus",
                                              "Species", "PLANTS.Code")],
                        by.x="SPCD", by.y="FIA.Code", all=FALSE) #tells which columns to do the "merge" operation by (dif names in each file)

#now, graphing again w/ common names:
allsp_agediam2 <- ggplot(data=dbsub_sitetree, 
                        aes(x=AGEDIA, y=DIA, col=Common.Name)) + geom_point()
#this one is plotting the relationship for ALL species, just using dif symbols for them.
allsp_agediam2

#generate list of the species for which we have the age and diameter data within FIA plots in the study area.
species_with_agedata <- unique(dbsub_sitetree$Common.Name)
#species_with_agedata
#a follow-up step: subset/look @ age data for only the species I am interested in within this list?
#or should I create these graphs for all of them bc the more data the merrier?
#maybe the latter- altho they won't be northern hardwood forests, they are in the study area/counties of interest....
#ask Jane about this?

#another question- for the regression equation I should be trying to find age (y) as a function of diameter (x), right?
#since diameter is the info I have and age is the info I want...even though the IRL relationship is backwards from this x-y wise.

#next step: capture the intercept & slope for each species in this dataset. 
#Probably by setting up a new dataframe and creating a for loop?
#BUT FIRST, I should probably convert diam to cm before creating the regression equations 
#since the unit conversion will affect the equations.

dbsub_sitetree$DIA_cm <- dbsub_sitetree$DIA*2.54 #converting diameter to cm in a separate column.

##############################################
# example- y=mx+b where y is age, x is diameter, for only sugar maple site trees in my study area -------------------------------
regression_test_sugarmaple <- lm(formula = AGEDIA ~ DIA_cm, data = dbsub_sitetree[dbsub_sitetree$SPCD==318,])
regression_test_sugarmaple #this command gives the intercept and m value (as in y=mx+b)
#looking good!!

##############################################
# creating data frame for intercept and slope of linear regression equations for each species -------------------------------


#next step: setting up a new dataframe w/ a row for each species that is represented in the site tree data for my study area

dbsub_agediam <- data.frame("common_name" = species_with_agedata,
                            "intercept" = rep(0),
                            "slope" = rep(0))
#may need to add the other ID variables later, but for now, let's try with just common name...

for(i in 1:nrow(dbsub_agediam)){
  sp_model <- lm(formula = AGEDIA ~ DIA_cm, #again, y is age and x is diameter
                 data = dbsub_sitetree[dbsub_sitetree$Common.Name==dbsub_agediam$common_name[i],])
  #the above line is basically saying - generate this linear model for the subset of sitetree data 
  #where species = the species of the row I'm currently on in the new dataframe
  dbsub_agediam$intercept[i] <- sp_model$coefficients[1]
  dbsub_agediam$slope[i] <- sp_model$coefficients[2]
  #the above 2 lines assign intercept & slope, as calculated by lm for the species, to the correct species/row of the new dataframe
  dbsub_agediam$num_datapoints[i] <- nrow(dbsub_sitetree[dbsub_sitetree$Common.Name==dbsub_agediam$common_name[i],])
  #adding a column in the DF that tabulates how many data points went into the regression eq. for each species
  }

dbsub_agediam

#for the species that only had 1 tree as a data point, equations are NOT reliable.
#sooo I guess we can exclude them from the analysis??

#dataframe excluding those species:
dbsub_agediam_2 <- dbsub_agediam[dbsub_agediam$num_datapoints>1,]
dbsub_agediam_2
#just looking at the numbers, I feel like we should maybe also exclude the couple species
  #that resulted in majorly negative intercepts- namely northern white cedar & white spruce
  #these make sense in a way b/c they are probably slow-growing and were only cored when they were vv old?
  #plus they also have the fewest observations, at 4 and 8 each