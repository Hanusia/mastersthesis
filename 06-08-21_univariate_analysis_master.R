# --------------------------------------------------
# Basic univariate analysis
# 08 Jun 2021
# HH
# --------------------------------------------------
#

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")


library(tidyverse) #includes tidyr, dplyr, tibble!
library(ggplot2)
library(patchwork)


#ACTION PLAN: Follow Tony's advice re: simple univariate analysis (notes on p. 160 of my current bullet journal)
#Step 1: attach harvest status/treatment to variables of interest (look @ that table I sent Tony et al. with info about each stand)
#Step 2: Quantify variables of interest (look at papers doing similar things for ideas of what those variables are)
#Step 3: Run some ANOVAs
#Step 4: Make some simple graphs showing some of those ANOVA differences (if significant/even if not, maybe??)

# STEP 1: attaching treatment type & date since last tx to vars of interest -------------------------------

#Fields are now incorporated into the stand_info file
#Need to add them to the plot_info dataframe (using dplyr? or, a longer way would be with a for loop)

#trying it with merge, adding only a few columns from the stand_info dataframe....
plot_info_plus <- merge(x=plot_info, 
                        y=stand_info[,c(2,14:16)], #didn't work with column labels for some reason, only w/ numbers. MAKE SURE TO UPDATE NUMBERS IF THE SOURCE DATASHEET/EXCEL FILE CHANGES!!!
                        by="Stand_name", all.x=TRUE) #merging all plot_info w/ ONLY the following columns from stand_info: Stand_info (for merging), treatment_type, harvest_start_year, harvest_end_year (for addt'l info) 
glimpse(plot_info_plus)

#THIS WORKED! plot_info_plus now has treatment type and harvest year categories

# STEP 2: quantifying variables of interest -------------------------------

#STARTING this with seedling & sapling density! (next session...)

