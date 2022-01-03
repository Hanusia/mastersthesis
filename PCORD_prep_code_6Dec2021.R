# --------------------------------------------------
# PC-ord prep code!
# 06 Dec 2021
# HH
# --------------------------------------------------
#

# intial setup stuff -------------------------------

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)
library(tidyverse)

# primary matrix: overstory BA (post-harvest) -------------------------------

#first, calculate BA from DBH
overstory_data$BA_sqm <- overstory_data$DBH_cm*overstory_data$DBH_cm*pi/40000

#now selecting out only LIVE trees
overstory_data<- overstory_data[overstory_data$status=="live",]

#now, to aggregate to plot level w/ only the species...
overstory_data <- overstory_data[,c("plot_ID", "species", "BA_sqm")]

plot_BA <- aggregate(BA_sqm ~ plot_ID*species, data=overstory_LANDISprep, FUN=sum)
#deleting the one datapoint with no species

plot_BA <- plot_BA[2:nrow(plot_BA),]

plot_BA_wide <- pivot_wider(data=plot_BA,
                            id_cols="plot_ID",
                            names_from="species",
                            values_from="BA_sqm")

#since not all plots are represented in this, need to associate w/ full plot list:
plot_BA_wide <- merge(x=plot_info[,c("plot_ID", "harvest_status")], y=plot_BA_wide,
                 by="plot_ID", all=TRUE)

#remove extra column that I felt like I needed to add...
plot_BA_wide <- plot_BA_wide[,c(1,3:length(plot_BA_wide))]

#ok not ideal but I did do it...

#now to coerce NAs to 0s
plot_BA_wide[is.na(plot_BA_wide)] <- 0

#OK i think we're ready to 

#should I do this in BA/ha or just BA in sqm??
#proportions will be the same since all plots are the same size
#for now, will just do BA sqm

write.csv(plot_BA_wide, file="PCORD_overstoryBA_primarymatrix_6Dec2021.csv")
