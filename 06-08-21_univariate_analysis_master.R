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

# starting w/ seedling data; copying some of this over from 3-24-30_Shifting_Seasons_Summit_figure_prep

#tying seedling data to more explanatory plot data
seedlings_plus <- merge(x=seedling_data,y=plot_info,by="plot_ID")

#sub-step 1: tally # seedlings per plot
seedlings_per_plot <- plot_info_plus #creating a new df
seedlings_per_plot$num_seedlings <- rep(0) #adding the seedling tally column

#for loop to cycle through seedlings_plus, adding to the correct row in seedlings_per_plot
for(i in 1:nrow(seedlings_plus)){
  for(j in 1:nrow(seedlings_per_plot)){
    if(seedlings_plus$plot_ID[i]==seedlings_per_plot$plot_ID[j]){
      seedlings_per_plot$num_seedlings[j] = (seedlings_per_plot$num_seedlings[j] + seedlings_plus$tally[i]) 
      #tallying ALL seedlings in each plot!
    }
  }
}

#next sub-step: converting from 'seedlings per plot' to 'seedlings per stand'

seedlings_per_stand <- stand_info #creating a new dataframe to capture this stuff
seedlings_per_stand$num_seedlings <- rep(0)
seedlings_per_stand$seedling_area <- seedlings_per_stand$num_plots*3
#this is because each plot has 3 seedling subplots, each 1 sq meter in size
#so multiplying area of seedlings counts (3 sqm/plot) times num plots gives us total area of seedling counts in each stand

#now we need a for loop to iterate thru stand and seedling-per-plot info: 
for(i in 1:nrow(seedlings_per_stand)){
  for(j in 1:nrow(seedlings_per_plot)){
    if(seedlings_per_stand$Stand_name[i]==seedlings_per_plot$Stand_name[j]){
      #assuming we are talking about the same plot......
      #add up all the seedlings
      seedlings_per_stand$num_seedlings[i] = (
        seedlings_per_stand$num_seedlings[i] +
          seedlings_per_plot$num_seedlings[j]
      )
    }
  }
}
# it worked!

#now, one last step to calculate seedlings DENSITY:
seedlings_per_stand$seedlings_per_sqm <- seedlings_per_stand$num_seedlings/seedlings_per_stand$seedling_area
# Q: should I exclude RUBUS/other shrubby species from this count/density???

##############################################

#LATER, will do the same thing for sapling data.....for now, will try an ANOVA with this......

aov1 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =seedlings_per_stand)
summary(aov1)
#not significant.....

#should I try this WITHOUT rubus?

mean(seedlings_per_stand[seedlings_per_stand$treatment_type=="regen_focus","seedlings_per_sqm"])
mean(seedlings_per_stand[seedlings_per_stand$treatment_type=="thinning_focus","seedlings_per_sqm"])

p1 <- ggplot(data=seedlings_per_stand, aes(x=as.factor(treatment_type), y=seedlings_per_sqm)) +
  geom_boxplot() + 
  theme_classic()
print(p1)


#NEXT STEP: try this without rubus.

#THEN, try it again with saplings.
#will need to re-look at how I calculate area w/ the 2 classes of 