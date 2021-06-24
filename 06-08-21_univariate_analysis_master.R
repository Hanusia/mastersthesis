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
seedlings_vec

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

#NOW TO MIMICK THE FOR LOOP AND ACTUALLY GET ALL THE SMALL SAPLING TALLY DATA IN HERE.
#for loop to sum up all these SMALL SAPLINGS per stand...
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

glimpse(large_saplings_stand_most)
glimpse(small_saplings_stand_most)

saplings_stand_final <- large_saplings_stand
saplings_stand_final$stand_sum %+=% small_saplings_stand$stand_sum #adding on the small saplings tallies to the existing large saplings tallies in the dataframe
saplings_stand_final$saplings_per_sqm %+=% small_saplings_stand$saplings_per_sqm #ditto for density, to combine them
#DENSITY IS NOT ADDITIVE so I NEED TO RETHINK THIS!!!!!!!!!!!!!!!!!!!!!!

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

#NONE of these are significant.....
#let's graph it using facet_wrap to see what these relationships/differences are looking like...
p3 <- ggplot(data=saplings_stand_final, 
             aes(x=treatment_type, y=saplings_per_sqm)) +
  geom_boxplot() + theme_classic() + 
  labs (x="Treatment_type",
        y= "Number of saplings \n per square meter")+
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y")
print(p3)

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

glimpse(saplings_stand_all)


