# --------------------------------------------------
# PC-ord prep code!
# 14 Jan 2022
# HH
# --------------------------------------------------
#

#this differs from last month's code b/c I'm analyzing at the STAND level,
#not at the plot level!
#and differs from last WEEK'S code b/c I updated names/labels of treatment types/categories!

# intial setup stuff -------------------------------

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)
library(tidyverse)
library(dplyr)

#primary matrix is actually no different from before...right?? so just gonna leave it as-is from 01-03-2022 code...
# primary matrix: overstory BA (post-harvest) -------------------------------

#should I do this in BA/ha or just BA in sqm??
#need to do in BA/ha since we are at the stand level!!
#will do this below (at the point where they are associated w/ stand)

#first, calculate BA/ha from DBH
overstory_data$BA_sqm <- overstory_data$DBH_cm*overstory_data$DBH_cm*pi/40000

#now selecting out only LIVE trees
overstory_data<- overstory_data[overstory_data$status=="live",]

#now, to aggregate to STAND level w/ only the species...
#first need to associate w/ Stand_name:
overstory_data_PCORD <- merge(x=overstory_data, y=plot_info[,c("plot_ID", "Stand_name")])
#then to add # plots AND STAND_CODE:
overstory_data_PCORD <- merge(x=overstory_data_PCORD, y=stand_info[,c("Stand_name", "num_plots", "Stand_code")])
#remove irrelevant columns: 
overstory_data_PCORD <- overstory_data_PCORD[,c("Stand_code", "num_plots", "species", "BA_sqm")]
#actually aggregating:
stand_BA <- aggregate(BA_sqm ~ Stand_code*species, data=overstory_data_PCORD, FUN=sum)
#then to add # plots:
stand_BA <- merge(x=stand_BA, y=stand_info[,c("Stand_code", "num_plots")])

#deleting the one datapoint with no species (first row)
#stand_BA <- stand_BA[2:nrow(stand_BA),]

###calculating BA/HA instead of just BA: !!!!
stand_BA$BA_sqm_ha <- stand_BA$BA_sqm / #total BA in square meters per species, per stand divided by
  (.04 * stand_BA$num_plots) #total area (in ha) of plots sampled in that stand, calculated by # of plots * 0.04 ha/plot!

#now, remove superfluous columns: 
stand_BA <- stand_BA[,c("Stand_code", "species", "BA_sqm_ha")]
#also, need to remove any rows where the species name doesn't exist:
stand_BA <- stand_BA[stand_BA$species!="",]

#now pivot to species-as-columns format required by PC-ORD:
stand_BA_wide <- pivot_wider(data=stand_BA,
                            id_cols="Stand_code",
                            names_from="species",
                            values_from="BA_sqm_ha")

#now to coerce NAs to 0s
stand_BA_wide[is.na(stand_BA_wide)] <- 0

write.csv(stand_BA_wide, file="PCORD_overstoryBA_primarymatrix_STANDCODES_3Jan2022.csv")

#1/14 update: modifying this to use the new "Treatment" column of the stand_info table w/ better names!
#(and also has the two "other"/misc stands reclassified into existing categories)
# setting up secondary matrix w/ explanatory variables -------------------------------

#UPDATE 1/3/22: ACTUALLY, using voi_stand as the additional matrix for PC-ORD
#b/c we are using stand-level, NOT plot-level for this analysis!
#BUT switching out stand_code instead of stand_name...
#FINAL UPDATE: MOVED THIS STUFF INTO PCORD_prep_code_3Jan2022 file b/c it makes more sense to be there!!
voi_stand_code <- stand_info[, c("Stand_code", "State", "Ownership_cat", 
                                 "Treatment", "EAB_present", "forest_type")]
#also need to reorder alphabetically 
voi_stand_code <- arrange(voi_stand_code, Stand_code)
View(voi_stand_code)
write.csv(voi_stand_code, "PCORD_secondmatrix_UPDATEDstandattributes_14Jan2022.csv")


#hello again on Sunday Jan. 16th!

# understory data primary matrix (stand level, seedling + sapling tally/ha) -------------------------------

#first, write some pseudocode:
#will probably be easier to aggregate sapling (small + large) data together first (??), 
#save that, and then add in seedlings?

#actually, first I will look at what I've got from the univariate analysis code and see if I can use that/rework it.
#but, generally: need to aggregate each data set (sm/lg saplings/seedlings) BY azimuth, BY plot, then BY stand--
#and finally combine tallies for each class...

#ok, first step is to import relevant datasets
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")


View(seedling_data)
#using "aggregate" function as in other code (more_organized_analysis from October) to do this...
#I think the hardest part will actually be COMBINING them (seedlings + saplings, w/ dif species present.) At which stage to do that???
#And then at some point, I'll need to pivot_wider....

#gonna actually start by attaching Stand_name so we can aggregate by that from the get-go!
seedlings_sp <- merge (x=seedling_data, y=plot_info[,c("plot_ID", "Stand_name")])
#ugh forgot we actually need to do this as stand CODES -_-
seedlings_sp <- merge (x=seedlings_sp, y=stand_info[,c("Stand_name", "Stand_code")])
seedlings_sp <- aggregate(tally ~ Stand_code + species, data = seedlings_sp, FUN=sum)
#so far, so good! And all stands are represented in this- checked w/ length(unique(seedlings_sp$Stand_code))

##############################################
#to start, let's at least do the same for saplings. 
#I think it will probably make the most sense to combine at the STAND level, with a consistent # of stands.

#oooh, or maybe even merge w/ plot_info somehow? hmm so many options!!
#maybe...use rbind and then re-aggregate???
#or just ues merge? (multiple times?) #and then create a new column that adds the "tallies" for each plot(or stand)/species combo-row?
#basically, seems like there are lots of different ways I can do this!
#but next-to-last step will be: total tally, by species/stand in "long" format, that I can then use the "numplots" variable to get a per-ha value.
#and finally, will pivot_wider to get it into the right format for PC-ORD.

#RETURN TO THIS TOMORROW (Monday)
#and use the following webpage for guidance: https://clayford.github.io/dwir/dwr_05_combine_merge_rehsape_data.html

#basically, two paths I can follow are rbind --> aggregate, or merge --> sum column
##############################################

#ok gameplan: do the same aggregation process for saplings, then use rbind to connect them, 
#then aggregate again to summarize by stand/species, then pivot_wider!

View(small_sapling_data)
small_sapling_data$tally <- small_sapling_data$shrub + small_sapling_data$over_1_ft + small_sapling_data$over_4.5_ft
small_saplings_sp <- merge (x=small_sapling_data, y=plot_info[,c("plot_ID", "Stand_name")])
small_saplings_sp <- merge (x=small_saplings_sp, y=stand_info[,c("Stand_name", "Stand_code")])
View(small_saplings_sp)
small_saplings_sp <- aggregate(tally ~ Stand_code + species, data = small_saplings_sp, FUN=sum)

#IMPORTANT: need to figure out how to remove any that are DEAD....but prob do this after rbind so I only do it once!

#anyway, first, do this w/ large saplings too:
View(large_sapling_data)
large_sapling_data$tally <- large_sapling_data$class_1 + large_sapling_data$class_2 + large_sapling_data$class_3
large_saplings_sp <- merge (x=large_sapling_data, y=plot_info[,c("plot_ID", "Stand_name")])
large_saplings_sp <- merge (x=large_saplings_sp, y=stand_info[,c("Stand_name", "Stand_code")])
View(large_saplings_sp)
large_saplings_sp <- aggregate(tally ~ Stand_code + species, data = large_saplings_sp, FUN=sum)

#now to rbind (at least the saplings) together: 

saplings_stand <- rbind(large_saplings_sp, small_saplings_sp)
View(saplings_stand)

######
#AT THIS STAGE need to RENAME "ACPE " species:
saplings_stand[saplings_stand=="ACPE "] <- "ACPE"

#now to aggregate again by stand & species:
saplings_stand <- aggregate(tally ~ Stand_code + species, data = saplings_stand, FUN=sum)
#this has all stands represented (checked via > length(unique(saplings_stand$Stand_code)))

#now to remove DEAD species: 
#follow THIS code from Landis prep script for guidance: 
#ok, executive decision: filtering out any dead saplings!
throwaway <- grep(pattern="\\w+-dead", x=saplings_stand$species, value=TRUE, fixed=FALSE, perl=TRUE)
# I HAD TO FIDDLE WITH THIS SO MUCH BUT THIS VERSION OF IT FINALLY WORKED!!!!
head(throwaway)
length(throwaway)
#actually NOT throwaway b/c I'm actually using it now to subset...
#and getting rid of dead sapling "species"
saplings_stand <- saplings_stand[!(saplings_stand$species %in% throwaway),]
#nice!

#also want to get rid of "UNK" and "?" species:
saplings_stand <- saplings_stand[!(saplings_stand$species=="UNK" | saplings_stand$species=="?"),]

#Okie dokie! Now at THIS point, want to keep this DF as-is so I can add the seedling data to it, and also make a version of it w/ just saplings.

#first, just replicating the process I did above w/ looping in the seedlings:
understory_stand <- rbind(saplings_stand, seedlings_sp)
understory_stand <- aggregate(tally ~ Stand_code + species, data = understory_stand, FUN=sum)
#and then removing the "UNK" "species" from this combined dataset:
understory_stand <- understory_stand[!(understory_stand$species=="UNK" | understory_stand$species=="UNKHW"),]


#then, for each of the dataframes I have at this point (saplings_stand and understory_stand), need to bind numplots so I can calculate #/ha
#WAIT, I forgot this is going to be hard b/c they were sampled in different areas..... -_-
#could just divide by numplot and have "tally per plot" as the metric??? I Think that will work just fine...
saplings_stand <- merge(saplings_stand, stand_info[,c("Stand_code", "num_plots")])
understory_stand <- merge(understory_stand, stand_info[,c("Stand_code", "num_plots")])

saplings_stand$tally_per_plot <- saplings_stand$tally/saplings_stand$num_plots
understory_stand$tally_per_plot <- understory_stand$tally/understory_stand$num_plots


#I think final step is just to pivot_wider?!
saplings_stand_wide <- pivot_wider(data=saplings_stand, 
                                   id_cols="Stand_code",
                                   names_from="species",
                                   values_from="tally_per_plot")
View(saplings_stand_wide)

understory_stand_wide <- pivot_wider(data=understory_stand, 
                                   id_cols="Stand_code",
                                   names_from="species",
                                   values_from="tally_per_plot")
View(understory_stand_wide)


#annnd NOW to remove NAs: 
saplings_stand_wide[is.na(saplings_stand_wide)] <- 0
understory_stand_wide[is.na(understory_stand_wide)] <- 0

#also, put stands in alphabetical order so they match w/ the 2ndary matrix!
#JK, looks like they are already :) 

#UPDATE, there is an error w/ an "ACPE " name w/ a space in it!!
#need to go back to earlier in the code and fix that....
#udpate again: this is done! Did it in the "saplings_stand" dataframe so only needed to fix in 1 spot & re-ran ze code

#finally, just save as .csvs!
write.csv(saplings_stand_wide, file="PCORD_saplingsperplot_primarymatrix_17Jan2022.csv")
write.csv(understory_stand_wide, file="PCORD_understoryperplot_primarymatrix_17Jan2022.csv")

#all done- woohoo! Now to import to PC-ORD...

