# --------------------------------------------------
# ash regeneration data
# 06 Nov 2020
# HH
# --------------------------------------------------
#

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv")
library(tibble)
library(ggplot2)

#taking a look at what we've got!
glimpse(seedling_data)
glimpse(small_sapling_data)
glimpse(large_sapling_data)
#glimpse(plot_info)
#glimpse(stand_info)

#changing name of plot_ID column since it had some weird characters in it
names(seedling_data)[1] <- "plot_ID"
names(small_sapling_data)[1] <- "plot_ID"
names(large_sapling_data)[1] <- "plot_ID"
names(plot_info)[1] <- "plot_ID"
#names(seedling_data) #success!
#names(plot_info)

#ditto for the first column, "Property" in stand_info
names(stand_info)[1]<- "Property"
# names(stand_info) #works out!

##############################################

#merging the seedlings df w/ the plot_info df on the basis of the plot_ID column
seedlings_plus <- merge(x=seedling_data,y=plot_info,by="plot_ID")

#first, creating a factor list of all the individual plot names
plot_names <- unique(seedlings_plus$plot_ID)
length(plot_names) #not 230 b/c some plots didn't have any seedlings and that's OK!!


