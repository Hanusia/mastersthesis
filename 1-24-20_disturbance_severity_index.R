# --------------------------------------------------
# disturbance severity index calculation
# 24 Jan 2021
# HH
# --------------------------------------------------
#
# Based on/inspired by: Peterson and Leach paper from 2008.


stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)

#From Peterson & Leach:
#Severity indicators = # trees down, BA trees down, % trees down, & % BA down
#"These variables were used as predictor variables in regressions of vegetation response to severity."
# % indices performed better than absolute ones
#Regeneration characteristics = seedling/sapling density, size, 
#species richness (mean per plot), species diversity (Shannon index), & 
#compositional change (Sorensen's: before/after, presence/absence; Chao: incl. abundance-
#I would have to do this comparing cut vs. unharvested instead of before vs. after)

#First step: Create matrices of the severity & regeneration indicators for each site
#(Already pretty much doing that/have done that for severity.)
#Second step: Once we've done that, use two-way ANOVAs to compare for harvested vs. not.
#(Two factors in the ANVOA P&L used were site & harvest status, but found no signif. site effect.)
#Third step: checks with goodness-of-fit tests, see which severity indicators are MOST important
#Fourth step: Use NMS (see: p. 414/ page 8 of the paper)

# Creating severity indicators matrices -------------------------------

#Indicator #1: Number of trees per hectare down (TOTAL, harvested + fallen)
sevind1_numtree <- matrix()

#Indicator #2: Total sum of BA per ha down per site (TOTAL, cut + natural causes)
sevind2_sumBA <- matrix()

#Indicator #3: Proportion of trees down vs. total trees (standing + down)
sevin3_pcttree <- matrix()

#Indicator #4: Proportion of BA down vs. total BA (standing + down)
sevind4_pctBA <- matrix()

#next Q: What does this matrix actually look like?? Rows = sites and columns =...
#One column equals the value for whatever disturb indicator I am using in that one
#Other is...categorical harvest status? For use in ANOVAS- any other vars??
#OR maybe we put these alllll in one matrix? W/ seperate cols for each indicator #

#UPDATE: I've asked Jess about this!
#we will discuss on Wednesday how Jess used a separate script for each trees, stumps, CWD, etc.
#This is dif from how I was planning to do it, basically mimicking what Peterson & Leach did

