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
ash_damage_data <- read.csv("ASH_DAMAGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ground_cover_data <- read.csv("GROUND_COVER_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tidyverse) #includes tidyr, dplyr, tibble!
library(ggplot2)
library(patchwork)
#install.packages("batman")
library(batman)

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
#glimpse(plot_info_plus)

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
aov1
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
#will need to re-look at how I calculate area w/ the 2 classes of saplings, b/c sample areas are different

##############################################

#Later today 6/15: #there are other shrubby/non-overstory species like rubus in the seedling data, too......
#so let's just try w/ those 6 focal species.

#choosing the top 8 regenerating sp in terms of how many plots they're present in.
#BUT excluding Rubus and Striped Maple since they aren't overstory species!
#so really, the top 6 overstory sp represented in # plots w/ regeneration.

some_seedlings_plot <- plot_info_plus
some_species <- c("ash_seedling_sum","sugar_maple_seedling_sum", #creating more columns to add to the df
                  "red_maple_seedling_sum", "yellow_birch_seedling_sum",
                  "beech_seedling_sum", "black_cherry_seedling_sum")
#adding those extra columns to tally species sums to seedlings by plot DF
some_seedlings_plot[some_species] <- rep(0)
glimpse(some_seedlings_plot)

#creating a vector of the 6 species we're looking at for use in the loop...
seedlings_vec <- c("FRAM", "ACSA", "ACRU", "BEAL", "FAGR", "PRSE")
#names(some_seedlings)

#for loop to sum up all the seedlings of these species, per plot...
for(i in 1:nrow(seedlings_plus)){ #iterating thru each seedling!
  for (j in 1:length(seedlings_vec)){ # 'j' will refer to each species separately!
    if(seedlings_plus$species[i]==seedlings_vec[j]){
      some_seedlings_plot[seedlings_plus$plot_ID[i]==some_seedlings_plot$plot_ID,14+j] <- #referring to correct column! CHANGED this from the code I copied it from.
        some_seedlings_plot[seedlings_plus$plot_ID[i]==some_seedlings_plot$plot_ID,14+j] +
        seedlings_plus$tally[i]
    }
  }
}

#glimpse(some_seedlings_plot)
#it worked!

#Now to transfer (again) to stand level......

#now need to transform to per-stand #s.

#doing this the same way as in Shifting Seasons figure code, with a few extra columns as needed!
some_seedlings_stand <- data.frame("stand_ID" = rep(stand_info$Stand_name, each=6),
                                   #repeating 6 times so there's one line per stand per species
                                   # making harvest status a binary variable of cut vs. not (not implicating "to be cut")
                                   "harvest_status" = rep(ifelse(stand_info$Harvest_status=="cut",TRUE,FALSE), each=6) ,
                                   "num_plots" = rep(stand_info$num_plots, each=6),
                                   "treatment_type" = rep(stand_info$treatment_type, each=6),
                                   #I could also add harvest start/end year, or other explan. variables of interest, in this step later...will keep it at this for nwo
                                   "species" = rep(seedlings_vec, times=nrow(stand_info)),
                                   "stand_sum" = rep(0)
                                   #"prop_ash_BA_cut" = rep(ash_cut_stand$prop_BA_cut, each=6)
)

#It worked!
glimpse(some_seedlings_stand)

#for loop to sum up all these seedlings per stand...
#THIS FOR LOOP IS NOT CURRENTLY WORKING. NOT SURE WHY CAUSE IT WORKED IN THE OTHER CODE
#MAY NEED TO REWRITE IT. COME BACK TO THIS!
#trying it another way: 
#UPDATE: this loop works now! I needed to remember that some_seedlings_stand$stand_ID is actually equvalent to some_seedlings_plot$Stand_name (dif column names!!)

for(i in 1:nrow(some_seedlings_plot)){ #iterating thru each PLOT!
  for (k in 1:length(seedlings_vec)){
    some_seedlings_stand$stand_sum[
      some_seedlings_stand$species==seedlings_vec[k] & 
        some_seedlings_stand$stand_ID==some_seedlings_plot$Stand_name[i]] <-
      some_seedlings_stand$stand_sum[
        some_seedlings_stand$species==seedlings_vec[k] & 
          some_seedlings_stand$stand_ID==some_seedlings_plot$Stand_name[i]] + 
      some_seedlings_plot[i,14+k]
  }
}

#DIVIDING the count by (# plots * 3) [# subplots per plot]
#to get count per meter-squared!!
some_seedlings_stand$seedlings_per_sqm <- some_seedlings_stand$stand_sum / 
  (some_seedlings_stand$num_plots*3)

#checking to see if it worked...
glimpse(some_seedlings_stand)

#now for some ANOVAS...look @ each species
for(i in 1:length(seedlings_vec)){
  aovx <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
              data =some_seedlings_stand[
                some_seedlings_stand$species==seedlings_vec[i],])
  summary(aovx)
}

aov2 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =some_seedlings_stand[some_seedlings_stand$species=="FRAM",])
summary(aov2)

aov3 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =some_seedlings_stand[some_seedlings_stand$species=="ACSA",])
summary(aov3)

aov4 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =some_seedlings_stand[some_seedlings_stand$species=="ACRU",])
summary(aov4)

aov5 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =some_seedlings_stand[some_seedlings_stand$species=="BEAL",])
summary(aov5)

aov6 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =some_seedlings_stand[some_seedlings_stand$species=="FAGR",])
summary(aov6)

aov7 <- aov(formula = seedlings_per_sqm ~ as.factor(treatment_type), 
            data =some_seedlings_stand[some_seedlings_stand$species=="PRSE",])
summary(aov7)

#NONE of these are significant.....
#let's graph it using facet_wrap to see what these relationships/differences are looking like...
p2 <- ggplot(data=some_seedlings_stand, 
              aes(x=treatment_type, y=seedlings_per_sqm)) +
  geom_boxplot() + theme_classic() + 
  labs (x="Treatment_type",
        y= "Number of seedlings \n per square meter")+
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p2)

##############################################

# Ok, now for SAPLING density as a response variable! -------------------------------

#Copying below/ modifying this from the shifting seasons summit code!

##############################################

#okey doke. Going to plot # of small saplings 
#(all species, &/or "main" 6 overstory sp? For now, I think the latter- same 6 sp as in p12) against BA ash cut.
glimpse(small_sapling_data)
#since they are separated by >1ft & >4.5ft (but all <1in DBH), I'm just gonna group those tallies together for now.
#Should be relatively easier to separate them later on if I want to.
#model this after p5
#seedlings_vec

#OK FIRST I need to create a new dataframe like sdling_stand_most. 
#This will be used link stand-level data to individual plot data for small saplings!

#finding out which seedlings showed up in the MOST plots:
#RESULT: same top 6 species (excluding understory sp and things like RUBUS)- makes my life easier!!

#copied from above: some_small_seedlings df to collect counts of each of 6 focal sp per plot
some_small_saplings <- data.frame("plot_ID" = plot_info$plot_ID, 
                                  "stand_ID" = plot_info$Stand_name,
                                  "harvest_status" = plot_info$harvest_status, 
                                  "gap_status" = plot_info$gap_status, 
                                  "treatment_type" = plot_info_plus$treatment_type,
                                  "ash_sapling_sum"=rep(0), #starting @ zero so they can be added to
                                  "sugar_maple_sapling_sum"=rep(0),
                                  "red_maple_sapling_sum"=rep(0),
                                  "yellow_birch_sapling_sum"=rep(0),
                                  "beech_sapling_sum"=rep(0),
                                  "black_cherry_sapling_sum"=rep(0)
)

#creating a vector of the 6 species we're looking at for use in the loop...
seedlings_vec <- c("FRAM", "ACSA", "ACRU", "BEAL", "FAGR", "PRSE")#just copying this one direct

#adding an incremental operator (I found it online)
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#for loop to sum up all the SMALL SAPLINGS of these species, per plot... (again based on above code)
for(i in 1:nrow(small_sapling_data)){ #iterating thru each small sapling!
  for (j in 1:length(seedlings_vec)){ # 'j' will refer to each species separately!
    if(small_sapling_data$species[i]==seedlings_vec[j]){
      some_small_saplings[small_sapling_data$plot_ID[i]==some_small_saplings$plot_ID,5+j] %+=% #referring to correct column!
        #NOTE: this number needs to change if the number/order of columns changes!!!!
        (small_sapling_data$over_1_ft[i]+small_sapling_data$over_4.5_ft[i]) #incl both size classes for saplings <1in DBH (NOT shrubs)
    }
  }
}

glimpse(some_small_saplings)

#For this FIRST step, mimicking some_seedlings_stand...
#& since I'm gonna use the same 6 species for now...I'm just gonna copy some_seedlings_stand.
#IF I WANTED TO USE DIFFERENT SPECIES THIS WOULD WORK DIFFERENTLY.
small_saplings_stand <- data.frame("stand_ID" = rep(stand_info$Stand_name, each=6),
                                   #repeating 6 times so there's one line per stand per species
                                   # making harvest status a binary variable of cut vs. not (not implicating "to be cut")
                                   "harvest_status" = rep(ifelse(stand_info$Harvest_status=="cut",TRUE,FALSE), each=6) ,
                                   "treatment_type" = rep(stand_info$treatment_type, each=6),
                                   #I could also add harvest start/end year, or other explan. variables of interest, in this step later...
                                   #will keep it at this for now
                                   "num_plots" = rep(stand_info$num_plots, each=6),
                                   "species" = rep(seedlings_vec, times=nrow(stand_info)),
                                   "stand_sum" = rep(0)
)

#NOW TO MIMICK THE FOR LOOP AND ACTUALLY GET ALL THE SMALL SAPLING TALLY DATA IN HERE.
#for loop to sum up all these SMALL SAPLINGS per stand...
for(i in 1:nrow(some_small_saplings)){ #iterating thru each PLOT!
  for (j in 1:length(seedlings_vec)){
    small_saplings_stand$stand_sum[
      small_saplings_stand$species==seedlings_vec[j] & 
        small_saplings_stand$stand_ID==some_small_saplings$stand_ID[i]] <-
      small_saplings_stand$stand_sum[
        small_saplings_stand$species==seedlings_vec[j] & 
          small_saplings_stand$stand_ID==some_small_saplings$stand_ID[i]] + 
      some_small_saplings[i,5+j] 
    #NOTE: this number NEEDS to change if the #/order of columns in the dataframe changes!!!!
  }
}

#DIVIDING the count by (# plots * 3 * 5) [# subplots per plot * area in sq m of each subplot]
#(so for small saplings, each subplot where they were tallied was 5 m^2, w/ 3 subplots/plot)
#to get count per meter-squared!!
small_saplings_stand$saplings_per_sqm <- small_saplings_stand$stand_sum / 
  (small_saplings_stand$num_plots*15)

#checking to see if it worked...
glimpse(small_saplings_stand)

##############################################
###NOW DO THE SAME WITH LARGE SAPLINGS, THEN COMBINE THEM###

#copied from above: some_small_seedlings df to collect counts of each of 6 focal sp per plot
some_large_saplings <- data.frame("plot_ID" = plot_info$plot_ID, 
                                  "stand_ID" = plot_info$Stand_name,
                                  "harvest_status" = plot_info$harvest_status, 
                                  "gap_status" = plot_info$gap_status, 
                                  "treatment_type" = plot_info_plus$treatment_type,
                                  "ash_sapling_sum"=rep(0), #starting @ zero so they can be added to
                                  "sugar_maple_sapling_sum"=rep(0),
                                  "red_maple_sapling_sum"=rep(0),
                                  "yellow_birch_sapling_sum"=rep(0),
                                  "beech_sapling_sum"=rep(0),
                                  "black_cherry_sapling_sum"=rep(0)
)

#creating a vector of the 6 species we're looking at for use in the loop...
seedlings_vec <- c("FRAM", "ACSA", "ACRU", "BEAL", "FAGR", "PRSE")#just copying this one direct

#for loop to sum up all the LARGE SAPLINGS of these species, per plot... (again based on above code)
for(i in 1:nrow(large_sapling_data)){ #iterating thru each large sapling!
  for (j in 1:length(seedlings_vec)){ # 'j' will refer to each species separately!
    if(large_sapling_data$species[i]==seedlings_vec[j]){
      some_large_saplings[large_sapling_data$plot_ID[i]==some_large_saplings$plot_ID,5+j] %+=% #referring to correct column!
        (large_sapling_data$class_1[i]+large_sapling_data$class_2[i]+large_sapling_data$class_3[i]) #incl all size classes for lg saplings btwn 1&4 in DBH
    }
  }
}

glimpse(some_large_saplings)


#For this FIRST step, mimicking some_seedlings_stand...
#& since I'm gonna use the same 6 species for now...I'm just gonna copy some_seedlings_stand.
#IF I WANTED TO USE DIFFERENT SPECIES THIS WOULD WORK DIFFERENTLY.
large_saplings_stand <- data.frame("stand_ID" = rep(stand_info$Stand_name, each=6),
                                   #repeating 6 times so there's one line per stand per species
                                   # making harvest status a binary variable of cut vs. not (not implicating "to be cut")
                                   "harvest_status" = rep(ifelse(stand_info$Harvest_status=="cut",TRUE,FALSE), each=6) ,
                                   "treatment_type" = rep(stand_info$treatment_type, each=6),
                                   "num_plots" = rep(stand_info$num_plots, each=6),
                                   "species" = rep(seedlings_vec, times=nrow(stand_info)),
                                   "stand_sum" = rep(0)
)

#NOW TO MIMICK THE FOR LOOP AND ACTUALLY GET ALL THE large SAPLING TALLY DATA IN HERE.
#for loop to sum up all these large SAPLINGS per stand...
for(i in 1:nrow(some_large_saplings)){ #iterating thru each PLOT!
  for (j in 1:length(seedlings_vec)){
    large_saplings_stand$stand_sum[
      large_saplings_stand$species==seedlings_vec[j] & 
        large_saplings_stand$stand_ID==some_large_saplings$stand_ID[i]] <-
      large_saplings_stand$stand_sum[
        large_saplings_stand$species==seedlings_vec[j] & 
          large_saplings_stand$stand_ID==some_large_saplings$stand_ID[i]] + 
      some_large_saplings[i,5+j]
  }
}



#DIVIDING the count by (# plots * 3 * 40) [# subplots per plot * area in sq m of each subplot]
#(so for large saplings, each subplot where they were tallied was 40 m^2, w/ 3 subplots/plot)
#to get count per meter-squared!!
large_saplings_stand$saplings_per_sqm <- large_saplings_stand$stand_sum / 
  (large_saplings_stand$num_plots*120)

#checking to see if it worked...
glimpse(large_saplings_stand)

#now to COMBINE small + lg saplings:

glimpse(large_saplings_stand)
glimpse(small_saplings_stand)

saplings_stand_final <- large_saplings_stand
saplings_stand_final$stand_sum %+=% small_saplings_stand$stand_sum #adding on the small saplings tallies to the existing large saplings tallies in the dataframe
saplings_stand_final$saplings_per_sqm <- rep(0)
saplings_stand_final$saplings_per_sqm <- 
  (saplings_stand_final$stand_sum / 
     ((large_saplings_stand$num_plots*120) +   (small_saplings_stand$num_plots*15)
))
#DENSITY IS NOT ADDITIVE so I NEED TO RETHINK THIS!!!!!!!!!!!!!!!!!!!!!!
#UPDATE: CHANGED DENSITY CALC TO TOTAL SAPLIGNS (lg + small) DIVIDED BY SUM OF TOTAL AREA (lg + small)

glimpse(saplings_stand_final)

#now they are combined into 1 df (saplings_stand_final), now to make plots! & anovas!

aov12 <- aov(formula = saplings_per_sqm ~ as.factor(treatment_type), 
            data =saplings_stand_final[saplings_stand_final$species=="FRAM",])
summary(aov12)

aov13 <- aov(formula = saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_final[saplings_stand_final$species=="ACSA",])
summary(aov13)

aov14 <- aov(formula = saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_final[saplings_stand_final$species=="ACRU",])
summary(aov14)

aov15 <- aov(formula = saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_final[saplings_stand_final$species=="BEAL",])
summary(aov15)

aov16 <- aov(formula = saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_final[saplings_stand_final$species=="FAGR",])
summary(aov16)

aov17 <- aov(formula = saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_final[saplings_stand_final$species=="PRSE",])
summary(aov17)

#let's graph it using facet_wrap to see what these relationships/differences are looking like...
p3 <- ggplot(data=saplings_stand_final, 
             aes(x=treatment_type, y=saplings_per_sqm)) +
  geom_boxplot() + theme_classic() + 
  labs (x="Treatment_type",
        y= "Number of saplings \n per square meter")+
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p3)

##############################################
#now to look at TOTAL sapling density all together (across species):

#CANNOT add densities, so I need to calculate separately for small & large saplings!
saplings_stand_all <- stand_info
saplings_stand_all["small_saplings_all"] <- rep(0)
saplings_stand_all["large_saplings_all"] <- rep(0)
saplings_stand_all["total_saplings"] <- rep(0)
saplings_stand_all["total_saplings_per_sqm"] <- rep(0)

for(i in 1:nrow(saplings_stand_final)){
    saplings_stand_all$small_saplings_all[
      saplings_stand_all$Stand_name==small_saplings_stand$stand_ID[i]] %+=%
      small_saplings_stand$stand_sum[i]
    saplings_stand_all$large_saplings_all[
      saplings_stand_all$Stand_name==large_saplings_stand$stand_ID[i]] %+=%
      large_saplings_stand$stand_sum[i]
}

saplings_stand_all["total_saplings"] <- saplings_stand_all$small_saplings_all+saplings_stand_all$large_saplings_all 
#adding together for the simple tally, but CANNOT use this # to calculate density!!
#using respective values of area for sm & lg saplings to calculate density
#BUT i still feel like this is wrong to add it together in this way?! keeping separate for now
saplings_stand_all["small_saplings_per_sqm"] <- (saplings_stand_all$small_saplings_all / saplings_stand_all$num_plots*15)
saplings_stand_all["large_saplings_per_sqm"] <- (saplings_stand_all$large_saplings_all / saplings_stand_all$num_plots*120) 
#one way to do it would be: (still not sure if this is correct, RE CHECK!!!) 
# total saplings (lg + small) / total area (lg + small)
saplings_stand_all["total_saplings_per_sqm"] <-  
  (saplings_stand_all$total_saplings / 
     ((saplings_stand_all$num_plots*120) +   (saplings_stand_all$num_plots*15)))
glimpse(saplings_stand_all)

#ok, now I tried to fix density for total saplings. 
aov18 <- aov(formula = large_saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_all)
summary(aov18)

aov19 <- aov(formula = small_saplings_per_sqm ~ as.factor(treatment_type), 
             data =saplings_stand_all)
summary(aov19)

aov20 <- aov(formula = total_saplings_per_sqm ~ as.factor(treatment_type), 
            data =saplings_stand_all)
summary(aov20)
#at small, large, and overall size classes, VERY signif dif in sapling numbers!

#should I try this WITHOUT rubus?

p4 <- ggplot(data=saplings_stand_all, aes(x=as.factor(treatment_type), y=total_saplings_per_sqm)) +
  geom_boxplot() + 
  theme_classic()
print(p4)

##############################################
# harvest date and ash health variables -------------------------------

#4 variables to add: harvest start year, harvest end year, harvest mean year, ash health index

#4 dataframes those variables need to be added to:
#seedlings_per_stand, some_seedlings_stand, saplings_stand_final, saplings_stand_all

glimpse(seedlings_per_stand) #45 rows (1 per stand), already has harvest start & end year
glimpse(some_seedlings_stand) #270 rows (6 per stand)
glimpse(saplings_stand_final) #270 rows (6 per stand)
glimpse(saplings_stand_all)  #45 rows (1 per stand), already has harvest start & end year

#first step: add start & end year vars to two saplings&seedlings by species

#OR, alt idea: create one MASTER HUGE dataframe w/ rows for each sp of interst & TOTAL as a separate row
#dataframe would need to duplicate everything 7 times
#gonna table that idea for right now because it would be a lot of work...might do it later though!

some_seedlings_stand$harvest_start_year <- rep(stand_info$harvest_start_year, each=6)
some_seedlings_stand$harvest_end_year <- rep(stand_info$harvest_end_year, each=6)

saplings_stand_final$harvest_start_year <- rep(stand_info$harvest_start_year, each=6)
saplings_stand_final$harvest_end_year <- rep(stand_info$harvest_end_year, each=6)

four_dfs <- list(seedlings_per_stand, some_seedlings_stand, saplings_stand_final, saplings_stand_all)
glimpse(four_dfs[[1]])

#calculating MEAN harvest year for each of the 4 dataframes of interest right now
for(i in 1:length(four_dfs)){
  four_dfs[[i]]$harvest_mean_year <- rep("na")
  for(j in 1:nrow(four_dfs[[i]])){
    if(four_dfs[[i]]$harvest_start_year[j] != "na"){
  four_dfs[[i]]$harvest_mean_year[j] <- ((as.numeric(four_dfs[[i]]$harvest_start_year[j]) + 
                                        as.numeric(four_dfs[[i]]$harvest_end_year[j])) / 2)
    }
  }
}

glimpse(four_dfs[[1]])

##############################################
#NOW, adding ash health indices!

glimpse(ash_damage_data)

#need to sum up indices per TREE
#then need to sum up indices per PLOT
#then need to average for ash heath # / tree / plot
#THEN need to summarize to the STAND level??
#calculate by adding canopy condition # + number of 'yes'es  for other binary categories

# final number = average ash health index per tree in the stand

#figure out how to use this to_logical function, OR ELSE just do a loop??
#to_logical(ash_health_index$DH,custom_false=c("","NO"))
#I thiiiink this is gonna be too complicated, so I'm just gonna do a loop instead...

ash_health_index <- ash_damage_data
#starting off the health total with the canopy condition, THEN
# the loop below will tally up the other binary categories and add them to this column
#BUT will also need to "zero out" for the NAs from a few rogue values in canopy_condition
ash_health_index$tree_health_total <- as.numeric(ash_health_index$canopy_condition)

for(i in 1:nrow(ash_health_index)){
  if(is.na(ash_health_index$tree_health_total[i])==TRUE){
    ash_health_index$tree_health_total[i] <- 0 #'zeroing out' a few coerced NAs
  }
  for(j in 4:9){
    if(ash_health_index[i,j]=="YES") { #adding on a count of 1 for any additional 'yes'es re: ash condition
      ash_health_index$tree_health_total[i] %+=% 1
    }
  }
}

glimpse(ash_health_index)
#this part is working now!

#Now, I need to attach the plot/stand-level data & try to aggregate everything to the stand level!
#Will do this by merging w/ plot_info_plus
#then tabulate average ash health per stand by(total ash health numbers all added together) / (total number of ash trees in all the plots in this stand)
nrow(ash_health_index)

ash_health_plus <- merge(x=ash_health_index, 
                              y=plot_info_plus,
                              by="plot_ID" #, all.x=TRUE #didn't include this part in OG code but should prob stay to make sure all ash health obs remain 
                         )
glimpse(ash_health_plus)

#OK, now to write a for loop to aggregate this all by stand!
#but first, set up an empty dataframe to contain it!

ash_health_stand <- stand_info #adding all the basic info from stand_info & the right # of columns!
ash_health_stand$health_index_total <- rep(0) #this will contain sum of ash health indices for each tree in the stand
ash_health_stand$ash_trees_total <- rep(0) #and this will contain total # of ash trees in the stand (as determined by the # of entries in that stand from the ash health dataset)
#ash_health_stand$avg_ash_health <- rep(0) #annnd this will be averaged out after we have the other numbers!

for(i in 1:nrow(ash_health_plus)){
  for(j in 1:nrow(ash_health_stand)){
    if(ash_health_plus$Stand_name[i]==ash_health_stand$Stand_name[j]){
      ash_health_stand$health_index_total[j] %+=% ash_health_plus$tree_health_total[i] #adding that individ tree's ash health index
      ash_health_stand$ash_trees_total[j] %+=% 1 #adding 1 for each ash tree is iterated thru in the stand!
    }
  }
}

ash_health_stand$avg_ash_health <- ash_health_stand$health_index_total/ash_health_stand$ash_trees_total #annnd this will be averaged out after we have the other numbers!
#so basically, HIGHER NUMBER ASH HEALTH = WORSE!!!!

#NEXT STEP: connect to seedling/sapling datasets!
#was gonna use a for loop / merge commands to iterate thru and add this to each DF, but the variable names aren't the same.......why did I do that?!

seedlings_per_stand <- merge(x=seedlings_per_stand, y=ash_health_stand[,c(2,20)],
                             by="Stand_name")
#updating variable name to stay in line w/ everyone else!
some_seedlings_stand$Stand_name <- some_seedlings_stand$stand_ID
#accidentally mis-merged these! So I should just start over and run all my code again...
some_seedlings_stand <- merge(x=some_seedlings_stand, y=ash_health_stand[,c(2,20)],
                             by="Stand_name")
#also need to update this name...
saplings_stand_final$Stand_name <- saplings_stand_final$stand_ID
saplings_stand_final <- merge(x=saplings_stand_final, y=ash_health_stand[,c(2,20)],
                              by="Stand_name")
saplings_stand_all <- merge(x=saplings_stand_all, y=ash_health_stand[,c(2,20)],
                              by="Stand_name")


# time to make models!! -------------------------------
#for now just w/ ash health, b/c harvest mean year is not currently a numeric var and has many nas.
m1 <- glm(data=seedlings_per_stand, 
          formula = seedlings_per_sqm ~ avg_ash_health)
summary(m1)
#result: it is signif!

m2 <- glm(data=some_seedlings_stand,
          formula=seedlings_per_sqm ~ avg_ash_health*species)
summary(m2)
#result: ACSA (& interaction w/ ash health) is signif (not others)!

#next, do the same w/ saplings:

m3 <- glm(data=saplings_stand_all, #looking @ saplings overall:
          formula=total_saplings_per_sqm ~ avg_ash_health)
summary(m3)
#only the intercept is significant for this one...not sure if that means anything useful!

m4 <- glm(data=saplings_stand_final, #looking at saplings by SPECIES
          formula = saplings_per_sqm ~ avg_ash_health*species)
summary(m4)
#once again, only ACSA is signif here! (@ the sapling, as well as at the seedling level)

#now to graph 'em!

p5 <- ggplot(data=seedlings_per_stand,
             aes(x=avg_ash_health, y=seedlings_per_sqm)) +
  geom_point(aes(col=factor(treatment_type))) + theme_classic() #+
  #geom_smooth(method=lm)
print(p5)
#might add a regression line later...TBD!

p6 <- ggplot(data=some_seedlings_stand,
             aes(x=avg_ash_health, y=seedlings_per_sqm)) + 
  geom_point(aes(col=factor(treatment_type))) + theme_classic() +
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p6)

#all saplings together:
p7 <- ggplot(data=saplings_stand_all,
             aes(x=avg_ash_health, y=total_saplings_per_sqm)) +
  geom_point(aes(col=factor(treatment_type))) + theme_classic() 
print(p7)

#saplings x species:
p8 <- ggplot(data=saplings_stand_final,
             aes(x=avg_ash_health, y=saplings_per_sqm)) + 
  geom_point(aes(col=factor(treatment_type))) + theme_classic() +
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p8)

##############################################
# Now doing models & graphs w/ harvest year... -------------------------------

#ACTUALLY, want to translate this to years SINCE harvest:

#FIRST creating a function to do this: 
years_since_harv <- function(dfx){ #input = a dataframe
  dfx$years_since_harvest <- rep(0) #creating that column in the df first
  for(i in 1:nrow(dfx)){ #iterating thru each row
    if((dfx$harvest_start_year[i]!="na")&(dfx$harvest_start_year[i]!="UNKNOWN")){ #if harvested...
  dfx$years_since_harvest[i] <- 2020 -  #calculating mean years SINCE harvested (as of 2020) 
    ((as.numeric(dfx$harvest_start_year[i]) + as.numeric(dfx$harvest_end_year[i]))/2)
    }
    else dfx$years_since_harvest[i] <- NA #ZEROING OUT the unharvested stands w/ 25s (yrs since harvest)
  }
  return(dfx) #returning the new MODIFIED dataframe...
}
#trying it out! 
seedlings_per_stand <- years_since_harv(seedlings_per_stand) #and assigning the modified output BACK TO the original df...
#update: it worked!!! now doing this w/ the remaining DFs:
some_seedlings_stand <- years_since_harv(some_seedlings_stand)
saplings_stand_final <- years_since_harv(saplings_stand_final)
saplings_stand_all <- years_since_harv(saplings_stand_all)

#now to run models, using ONLY harvested sites!
#don't need to specify that in the model; lines w/ NAs will be discounted per the messages I got!
m5 <- glm(data=seedlings_per_stand, 
          formula = seedlings_per_sqm ~ years_since_harvest)
summary(m5)
#result: not signif.

m6 <- glm(data=some_seedlings_stand,
          formula=seedlings_per_sqm ~ species*years_since_harvest)
summary(m6)
#only ACSA is significant. (not in combo w/ years since harvest...)

#next, do the same w/ saplings:

m7 <- glm(data=saplings_stand_all, #looking @ saplings overall:
          formula=total_saplings_per_sqm ~ years_since_harvest)
summary(m7)
#this one IS signif BUT only at 0.08 level...so not really...

m8 <- glm(data=saplings_stand_final, #looking at saplings by SPECIES
          formula = saplings_per_sqm ~ species*years_since_harvest)
summary(m8)
#signif: just ACSA on its own, and also years_since_harvest x FAGR (??)

#now to graph 'em!
#now to graph 'em!

p9 <- ggplot(data=seedlings_per_stand,
             aes(x=years_since_harvest, y=seedlings_per_sqm)) +
  geom_point(aes(col=factor(treatment_type))) + theme_classic() #+
#geom_smooth(method=lm)
print(p9)
#might add a regression line later...TBD!

p10 <- ggplot(data=some_seedlings_stand,
             aes(x=years_since_harvest, y=seedlings_per_sqm)) + 
  geom_point(aes(col=factor(treatment_type))) + theme_classic() +
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p10)

#all saplings together:
p11 <- ggplot(data=saplings_stand_all,
             aes(x=years_since_harvest, y=total_saplings_per_sqm)) +
  geom_point(aes(col=factor(treatment_type))) + theme_classic() 
print(p11)

#saplings x species:
p12 <- ggplot(data=saplings_stand_final,
             aes(x=years_since_harvest, y=saplings_per_sqm)) + 
  geom_point(aes(col=factor(treatment_type))) + theme_classic() +
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p12)

##############################################

# looking @ ash health parameters as Tony recommended re: Robinett & McCullough... -------------------------------

# First, comparing ash health vs individual tree diameter
#the way Robinett & McCullough did it was grouping into size classes & ANOVA...
#and then also just straight-up regression

#df in question = ash_health_plus
#first, need to transform DBH to numeric var & transform classes to numbers
ash_health_plus$DBH_adj <- rep(0) #adding a numeric var to fill in
for(i in 1:nrow(ash_health_plus)){
  if(ash_health_plus$DBH_cm[i]=="I") {
    ash_health_plus$DBH_adj[i] <- 2.5
  } else if(ash_health_plus$DBH_cm[i]=="II"){
    ash_health_plus$DBH_adj[i] <- 5.1
  } else if(ash_health_plus$DBH_cm[i]=="III"){
    ash_health_plus$DBH_adj[i] <- 7.6
  } else {
  ash_health_plus$DBH_adj[i] <- as.numeric(ash_health_plus$DBH_cm[i])
  }
}

#now, to separate them into classes: 
ash_health_plus$DBH_class <- rep(NA)
for(i in 1:nrow(ash_health_plus)){
  if(ash_health_plus$DBH_adj[i]>=10.0 & ash_health_plus$DBH_adj[i]<=20.0) {
    ash_health_plus$DBH_class[i] <- 1 #separate into 3 categories like R&M paper did
  } else if(ash_health_plus$DBH_adj[i]>=20.1 & ash_health_plus$DBH_adj[i]<=30.0) {
    ash_health_plus$DBH_class[i] <- 2
  } else if(ash_health_plus$DBH_adj[i]>=30.1) {
    ash_health_plus$DBH_class[i] <- 3
  }
}

#now, do an anova w/ diam class & health index!
aov21 <- aov(formula = tree_health_total ~ as.factor(DBH_class), 
             data =ash_health_plus)
summary(aov21)
#result: not significant.
#NEXT STEP: do this separated by EAB status (infested yes/no); waiting on VT data!

#next: do regression vs. just straight up DBH.
m9 <- glm(data=ash_health_plus,
          formula= tree_health_total ~ DBH_adj)
summary(m9)
#however, regression vs. diameter is VERY significant.

#time to graph it!
p13 <- ggplot(data=ash_health_plus,
              aes(x=DBH_adj, y=tree_health_total)) + 
  geom_point(aes(col=factor(treatment_type))) + theme_classic()
print(p13)
#REMEMBER! At this stage, "tree health total" is still the total ISSUES with the tree- higher number = worse!!

#Second, comparing ash health vs stand density (basal area, living+dead, all species)
#this is the variable Robinett & McCullough used in their paper!
#so in my case, want to use snags + stumps + live trees BA
#and regress that BA in sqm/ha against ash health index (instead of survival)
#will need to bring in formulas etc for calculating DBH from stumps...use code from PC-ORD prep!
#OR just run that code in its own script, export results (@ stand level) in a .csv, and add that .csv here?!

#update: the latter is what I ended up doing! 
#now to import that file here so I can merge the dataframes.

overstory_plus <- read.csv(file="stand_basal_area_dataframe_09Aug2021.csv")
glimpse(overstory_plus) #this input worked
#and as a reminder from the other script, its column BA_sqm_ha is basal area in units of sqm/ha for ALL LIVE AND DEAD TREES PER STAND
glimpse(ash_health_stand)
#now to append that number onto the ash_health_stand dataframe (also at the stand level):
ash_health_stand$total_preharvest_BA_sqm_ha <- overstory_plus$BA_sqm_ha
#extra-descriptive label to remind what we're talkin' about...

#now to see how these two vars correlate!
m10 <- glm(data=ash_health_stand,
          formula= avg_ash_health ~ total_preharvest_BA_sqm_ha)
summary(m10)
#ONLY the intercept is signif (e.g. signif dif from zero, which isn't exactly helpful!)

#time to graph it!
p14 <- ggplot(data=ash_health_stand,
              aes(x=total_preharvest_BA_sqm_ha, y=avg_ash_health)) + 
  geom_point(aes(col=factor(treatment_type))) + theme_classic()
print(p14)
#REMEMBER! At this stage, "avg_ash_health" is still the total ISSUES with the tree- higher number = worse!!
#lol there is basically NO clear relationship here.

# exploring ground cover data! -------------------------------
glimpse(ground_cover_data)

#step 1: append stand-level data to this df so we can aggregate @ the stand level
#step 2: avg 3 azimuths' data points for each plot per each cover type, then aggregate by stand
#step 3: chi-square test by treatment type, harvest status, & years since harvest.

#for step 1: I'll try using dplyr to do this?
#ground_cover_try <- ground_cover_data %>%
#  group_by(plot_ID, life_form) %>% #grouping by plot_ID first and then life_form second
#  summarize(avg_cover_class = mean(cover_class))
#this worked!! but there is still a separate row for each cover class within each 
#could do that with tidyr probably?
#glimpse(ground_cover_try)

#ground_cover_tidy <- ground_cover_try %>%
#  spread(life_form, avg_cover_class)
#glimpse(ground_cover_tidy)
#update: this DID work for what I wanted to happen!
#would then need to "gather" this back using the reverse once I append/aggregate stand info.
#testing w/ gather:
#ground_cover_gather <- ground_cover_tidy %>%
#  gather(life_form, avg_cover_class)
#glimpse(ground_cover_gather)
#this did NOT work, so maybe try pivot_wider for that instead if I do end up wanting to use it...
#ground_cover_gather <- ground_cover_tidy %>%
#  pivot_longer(cols=!plot_ID, names_to = "life_form", values_to = "avg_cover_class")
#glimpse(ground_cover_gather)
#update: pivot_longer DID work for this!
#next step = merge data w/ stand info. 

#TL;DR IS: we're gonna use ground_cover_tidy to merge w/ stand-level data, then use pivot_longer to transfer back to a format that works well for ANOVAS/chi-square tests.

#UPDATE FOR TONIGHT: not sure about the best analysis to do with this data.
#I was originally thinking chi-square, but b/c each ground cover class has a num. var. associated with it, internet resources are actually pointing me to MANOVA.
#email Tony to ask about this! Also ask if I need to treat cover class values differently b/c they are actually factors, e.g. Braun-Blanquet cover classes.

## 8-16-21 update ##
#Asked Tony about how to treat this data & he said assign midpoint vals to BB classes,
#then average to the plot level. (? not stand level??)
#use PerMANOVA for categorical vars & multiple regression for continuous ones.

#Step 1: Assigning midpoint values to each of the Braun-Blanquet classes
Braun_Blanquet_classes <- c(0:6)
Braun_Blanquet_midpoints <- c(0, 0.5, 3, 10.5, 23, 45.5, 80.5)
#some of the above stuff will be irrelevant b/c we don't want to avg the cover classes themselves, but the midpoints of each class!
ground_cover_data$midpoint_val <- rep(NA)

for(h in 1:nrow(ground_cover_data)){
for(i in 1:length(Braun_Blanquet_classes)){
  if(is.na(ground_cover_data$cover_class[h])==FALSE){ #NEEDED TO REMOVE NAs! b/c they mess up the if statements b/c no t/f!
  if(ground_cover_data$cover_class[h]==Braun_Blanquet_classes[i]){
    ground_cover_data$midpoint_val[h] <- Braun_Blanquet_midpoints[i]
  } 
  }
}
}
#Braun_Blanquet_classes[1]
#ground_cover_data$cover_class[1]

ground_cover_try <- ground_cover_data %>%
  group_by(plot_ID, life_form) %>% #grouping by plot_ID first and then life_form second
 summarize(avg_cover_val = mean(midpoint_val, na.rm = TRUE))
glimpse(ground_cover_try)

ground_cover_tidy <- ground_cover_try %>%
  spread(life_form, avg_cover_val)
glimpse(ground_cover_tidy)
#this *should* work now for PerMANOVA!

#installing the needed package:
#install.packages("PERMANOVA")
#library(PERMANOVA)
#turns out this package is VERY new so I'll try using adonis 1st.

#actually, it's looking like I should use the adonis function in the vegan package instead...
library(vegan)
#PERMANOVA AKA NPMANOVA

#next step: need to associate categorical variables w/ this dataframe (@ the plot level)
#vars of interest: for now just start w/ tx type!

#need to make input a matrix instead of dataframe!

mat_dimnames <- list(ground_cover_tidy$plot_ID, #row names (plot codes)
                     ground_cover_data$life_form[1:5]) #column names (life form codes)
ground_cover_mat <- as.matrix(x=ground_cover_tidy[,2:6],
                              dimnames=mat_dimnames)

run1 <- adonis(formula=ground_cover_mat ~ treatment_type, #using ground_cover_tidy as the DATA MATRIX so dissimilarity will be calculated in the function, and tx type as first var of interest 
               data=plot_info_plus #name of dataframe w/ treatment_type var (or other vars of interest)
               #leaving other parameters as default for now- permutations = 999 and method = "bray," sqrt = FALSE (but will try w/ it on as welL!)
)
#I think I need to make sure the plots are in the same order btwn matrix & dataframe!! so the variables they apply are the same??
#to compare:
ground_cover_tidy$plot_ID[1:5]

plot_info_plus$plot_ID[1:5]
  
#??why are they in different orders?
  
run2 <- adonis(formula=ground_cover_mat ~ treatment_type*site_ID, #using ground_cover_tidy as the DATA MATRIX so dissimilarity will be calculated in the function, and tx type as first var of interest 
                 data=plot_info_plus #name of dataframe w/ treatment_type var (or other vars of interest)
                 #leaving other parameters as default for now- permutations = 999 and method = "bray," sqrt = FALSE (but will try w/ it on as welL!)
  )