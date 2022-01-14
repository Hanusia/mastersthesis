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
