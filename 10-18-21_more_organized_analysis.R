# --------------------------------------------------
# Re-running analyses of the data I collected summer 2020
# With new and improved methods/organization!
# 18 Oct 2021
# HH
# --------------------------------------------------
#

# reading in data, loading packages, etc. -------------------------------

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ash_damage_data <- read.csv("ASH_DAMAGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ground_cover_data <- read.csv("GROUND_COVER_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
CWD_data <- read.csv("CWD_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tidyverse) #includes tidyr, dplyr, tibble!
library(ggplot2)
library(patchwork)
#install.packages("batman")
library(batman)
library(ggthemes)
#library(ggsignif)

#note: adding harvest_mean_year variable to the stand_info table
stand_info$harvest_mean_year <- ((as.numeric(stand_info$harvest_start_year) + 
                                          as.numeric(stand_info$harvest_end_year)) / 2)

#also, creating plot_info_plus to initiate the dataframes below.
plot_info_plus <- merge(x=plot_info, 
                        y=stand_info, 
                        by="Stand_name", all.x=TRUE)  


# creating "variables of interest" (VOI) dataframeS to use consistently throughout code -------------------------------

#fist, voi_stand includes variables of interest that are relevant to ALL stands at the STAND level
voi_stand <- stand_info[, c("Stand_name", "State", "Ownership_cat", 
                            "treatment_type", "EAB_present", "forest_type")]

#next, voi_stand_cut includes variables of interest that are relevant to ONLY HARVESTED stands at the STAND level,
  #and INCLUDES ONLY HARVESTED STANDS
voi_stand_cut <- stand_info[stand_info$Harvest_status=="cut", c("Stand_name", "State", "Ownership_cat", 
                                "treatment_type", "EAB_present", "forest_type",
                                "Harvest_status", "harvest_mean_year")]
  
#second, voi_plot includes all variables of interest that are relevant to ALL stands at the PLOT level
  #this is essentially the same as voi_plot, with the addition of the plot_ID identifier.
voi_plot <- plot_info_plus[, c("plot_ID", "Stand_name", "State", "Ownership_cat", 
                               "treatment_type", "EAB_present", "forest_type")]

#next, voi_plot_cut includes variables of interest that are relevant to ONLY HARVESTED plots at the PLOT level,
#and INCLUDES ONLY HARVESTED PLOTS
#this is essentially the same as voi_stand_cut, with the addition of the plot_ID identifier AND "gap_status" for gap vs. matrix distinction.
voi_plot_cut <- plot_info_plus[plot_info_plus$Harvest_status=="cut", c("plot_ID", "Stand_name", "State", "Ownership_cat", 
                                                                "treatment_type", "EAB_present", "forest_type",
                                                                "Harvest_status", "harvest_mean_year",
                                                                "gap_status")]

# refresher/background on methods (GLMM and MMANOVA) -------------------------------
#to-do 11/3!

#deleting all the long rambling notes I had in here b/c I transfered it to my workflow document instead.
#basically, after talking to Maria and Tony, I will focus on GLMM because it can do everything MMANOVA can do!
#(and Maria says, maybe even just LMM because I can start by assuming normality, then see if those assumptions are violated.)

#Post- my conversation w/ Tony, I will start by looking ONLY at the factors of treatment type and forest type (fixed effects), plus stand (random effect).
#Then later on, I can add in other factors and see if they are actually important to the model or not.


# Pseudocode for allllll of this stuff! Write out before beginning to code. -------------------------------

#Step 1: Get response variables summarized by plot (looking @ 1 variable at a time)
#Step 2: Attach variables of interest to the plot-level data
#Step 3: Convert data from "wide" into "long" format (using dplyr or tidyr?)
#Step 4: Install packages needed for (G)LMM (nlmr? lme4?)
#Step 5: Run (G)LMM with 3 factors, again, one response variable at a time
#Step 6: Examine results + test assumptions of normality/other assumptions?
#Step 7: Visualize data graphically

# Overstory basal area as response variable -------------------------------

#step 1: summarize total BA by plot
overstory_data$BA_sqm <- overstory_data$DBH_cm*overstory_data$DBH_cm*pi/40000

livetree_plot_BA <- aggregate(BA_sqm ~ plot_ID, data=overstory_data, FUN=sum)
livetree_plot_BA$BA_sqm_ha <- livetree_plot_BA$BA_sqm/0.04 #total basal area (in sqm) in the plot, divided by the area of a plot (in ha)
View(livetree_plot_BA)

#step 2: attach VOI table to this one
livetree_plot_BA <- merge(x=livetree_plot_BA, y=voi_plot, by="plot_ID")
#did that, whoop dee do!

#step 3: convert data from "wide" into "long" format
livetree_plot_BA_long <- pivot_longer(data=livetree_plot_BA,
                                      cols=c("Stand_name", "treatment_type", "forest_type")) #selecting columns to pivot into "longer" format
View(livetree_plot_BA_long)

#step 4: install packages

#starting with nlme for now since Maria suggested it
#install.packages("nlme")
library(nlme)

#step 5: Run LMM!

#first, using function lme() from nlme package

#update: this function actually uses WIDE, instead of LONG data (so ignore the part above where I changed it)
model1 <- lme(fixed=BA_sqm_ha ~ treatment_type + forest_type, 
              data = livetree_plot_BA, 
              random = ~ 1 | Stand_name)

#step 6: examine results + test assumptions of normality

summary(model1)
anova(model1)

#remeber, SMALLER AIC value means BETTER model fit!
#it SEEMS that the degrees of freedom were interpreted correctly.
#how to parse out individual treatment types etc. and their effects?
#also, how to effectively graph this?

#let's try one more for fun.....
model2  <- lme(fixed=BA_sqm_ha ~ treatment_type + forest_type + Ownership_cat, 
                      data = livetree_plot_BA, 
                      random = ~ 1 | Stand_name)

anova(model2)
summary(model2)
#AIC for this one is...slightly better?!

model3  <- lme(fixed=BA_sqm_ha ~ treatment_type + forest_type + Ownership_cat + EAB_present, 
               data = livetree_plot_BA, 
               random = ~ 1 | Stand_name)

anova(model3)
summary(model3)


#TOMORROW: look into these questions, then start testing other response variables + other combinations of explanatory variables!


# calculating most prevalent overstory species -------------------------------

#using the table function:
table(overstory_data$species)

#sorting this with the most prominent at the top:
sort(table(overstory_data$species), decreasing=TRUE)

#now looking for ONLY live trees:
sort(table(overstory_data$species[overstory_data$status=="live"]), decreasing=TRUE)

#now looking at stumps (aka cut trees):
sort(table(overstory_data$species[overstory_data$status=="stump" & overstory_data$decay_class<3]), decreasing=TRUE)

#excluding UNK, we have the same top 5 species (that are also the same top 5 for seedlings, saplings etc.... assuming we exclue striped maple & rubus...altho also not including PRSE??)

#soooo I'm thinking we should go forward with these top 5 species as the "big 5"
sort(table(small_sapling_data$species), decreasing=TRUE)
sort(table(large_sapling_data$species), decreasing=TRUE)
sort(table(seedling_data$species), decreasing=TRUE)

#executive decision: sticking with these 5 species!!!
major_species <- c("ACSA", "FRAM", "FAGR", "BEAL", "ACRU")

# create a function that tests the variables & their interactions & applies AIC for dif response vars? -------------------------------
#ask Tony if I need to do this before doing it...

# more models w/ dif combos of variables -------------------------------

model4 <- lme(fixed=BA_sqm_ha ~ treatment_type,
                         data = livetree_plot_BA, 
                         random = ~ 1 | Stand_name)

anova(model4)
summary(model4)

model5 <- lme(fixed=BA_sqm_ha ~ treatment_type + forest_type + EAB_present,
              data = livetree_plot_BA, 
              random = ~ 1 | Stand_name)

model6 <- lme(fixed=BA_sqm_ha ~ treatment_type + Ownership_cat + EAB_present,
              data = livetree_plot_BA, 
              random = ~ 1 | Stand_name)

#model7 <- lme(fixed=BA_sqm_ha ~ treatment_type*forest_type*Ownership_cat*EAB_present,
#              data = livetree_plot_BA, 
#              random = ~ 1 | Stand_name)
#model 7 (interactions of all vars) isn't working- ignore for now

#using AIC to compare these models
#install.packages("AICcmodavg")
library(AICcmodavg)


models_list <- list(model1, model2, model3, model4, model5, model6)

model_names <- c('tx_forest_mod', 'tx_forest_ownership_mod', 'tx_forest_ownership_EAB_mod', 'tx_mod', 'tx_forest_EAB_mod', 'tx_ownership_EAB_mod')

aictab(cand.set = models_list, modnames = model_names)

# pivot to seedlings data! -------------------------------

#first step is to aggregate tallies by plot, total # & for species of interest
seedlings_per_plot <- aggregate(tally ~ plot_ID, data = seedling_data, FUN=sum)

#OK for doing it by species....since they are already summed per species per plot,
#It seems like the easiest way to do this is subset to species of interest, then pivot_wider
#JK I completely forgot about the different azimuths! -_-

#OK aggregating 1st to get rid of the azimuth subdivisions!
seedlings_sp <- aggregate(tally ~ plot_ID + species, data = seedling_data, FUN=sum)

#now continuing on w/ subset to species level...
seedlings_sp <- seedlings_sp[seedlings_sp$species=="ACSA" |
                                seedlings_sp$species=="FRAM" | 
                                seedlings_sp$species=="FAGR" |
                                seedlings_sp$species=="ACRU" |
                                seedlings_sp$species=="BEAL",]

seedlings_sp <-
  pivot_wider(names_from=species, values_from=tally, data=seedlings_sp)

#IMPORTANT NOTE: neither of the dataframes I've made so far today have the total # of rows = total # plots...
#this means when I merge them, will need to include all rows in the plot_info or equivalent DF

#also- we are still in tally format, not per area...
#since each plot has 3 subplots, w/ each subplot = 1 sqm area...
#each plot-level value can just divide by 3 to get per-sqm value.

#this just merged total tally + per-species tally, per plot.
seedlings_per_plot <- merge(x=seedlings_per_plot, y=seedlings_sp, all.x=TRUE, by="plot_ID")

#now to merge w/ plot data:
seedlings_plus <- merge(x=voi_plot, y=seedlings_per_plot, all.x=TRUE, by="plot_ID")

#now to replace NAs with 0s:
seedlings_plus[is.na(seedlings_plus)] <- 0

#and finally, create another DF where they're in seedlings PER SQUARE METER:
seedlings_plus_sqm <- seedlings_plus
seedlings_plus_sqm[,8:13] <- seedlings_plus_sqm[,8:13]/3

#now do a linear model:
  
  model8 <- lme(fixed=tally ~ treatment_type + forest_type, 
                data = seedlings_plus_sqm, 
                random = ~ 1 | Stand_name)
  anova(model8)
  summary(model8)                                                                   

  #test with just 1 species??
  
  
#urghhh I think we need to pivot this again.....
  seedlings_plus_sqm_long <- pivot_longer(data=seedlings_plus_sqm,
                                          cols=c("tally", "ACSA", "FRAM", "FAGR", "ACRU", "BEAL"),
                                          names_to = "species")
  
for (i in 1:5){
  loop_df <- seedlings_plus_sqm_long[seedlings_plus_sqm_long$species==major_species[i],]
  loop_model <- lme(fixed=value ~ treatment_type + forest_type, 
                    data = loop_df, 
                    random = ~ 1 | Stand_name)
  print(anova(loop_model))                        
}
  
  #looks like only BEAL has a signif effect by treatment type...
  #update 12/2: actually doesn't look like there's a signif effect at all??
  #why did I think there was??
  #and ACSA *almost* has a signif effect by forest type, which actually makes sense. 

  #next steps = plot this (similar/use code from p2 in my figures_workingdoc slideshow, but w/ plots instead of stands),
  #next step 2 = figure out * which * treatments/differences are impactful for BEAL (Tukey test??),
  #next step 3 = do the same thing for SAPLINGS!!!
  
  
#next step 1: plotting seedling densities
  
  facet_labels <- c("all seedlings", "sugar maple", "white ash", "American beech", "red maple", "yellow birch")
  #reordering levels of the species variable as a factor:
  seedlings_plus_sqm_long$species <- factor(seedlings_plus_sqm_long$species, levels=unique(seedlings_plus_sqm_long$species),
                                            labels=facet_labels)
  
  p2 <- ggplot(data=seedlings_plus_sqm_long, 
               aes(x=treatment_type, y=value, fill=treatment_type)) +
    geom_boxplot() + theme_few() + 
    scale_fill_manual(values=wes_palette("Chevalier1")) +
    labs (x="Harvest treatment",
          y= "Number of seedlings per square meter")+
    #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
    facet_wrap(~ species, nrow=2, ncol=3, scales="free_y") +
    scale_x_discrete(limits=c("na", "thinning_focus", "regen_focus", "other"), #this argument (limits) re-orders the x-axis
                     labels=c("not harvested", "removal focus", "regeneration focus", "other")) + #and this argument renames the labels
    theme(axis.text.x = element_text(angle=25, vjust=.7))
  print(p2)
  p2+ theme(legend.position = "none")
  
  

  #now to make it look better:
  #want to add borders all the way around each plot -> done by changing theme
  #want to fix labels for each species -> done by adding labels to the factor variable in the dataframe itself!
  #want to fix x-axis labels for treatment types/categories --> done
  #figure out what to do w/ all those outliers??
  #and then maybe do a comparison w/ bar graph vs. boxplot?
  #add indicator of significance for BEAL -> actually nevermind, guess it's not signif?
  
  #changed up by graphing log transform of response var!
  #***NEED THAT TRANSFORMATION TO BE REFLECTED IN THE GRAPH/AXIS LABELS TOO!!!
  p3 <- ggplot(data=seedlings_plus_sqm_long, 
               aes(x=treatment_type , y=log(value+1))) +
    geom_boxplot() + theme_few() + 
    labs (x="Harvest treatment",
          y= "Number of seedlings per square meter")+
    #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
    facet_wrap(~ species, nrow=2, ncol=3, scales="free_y") +
    scale_x_discrete(limits=c("na", "thinning_focus", "regen_focus", "other"), #this argument (limits) re-orders the x-axis
                     labels=c("unharvested", "removal", "regeneration", "other")) + #and this argument renames the labels
    theme(axis.text.x = element_text(angle=25, vjust=.7))
  print(p3)
  
  #THIS ISN'T WORKING- WILL NEED TO TROUBLESHOOT
  p4 <- ggplot(data=seedlings_plus_sqm_long, 
               aes(x=treatment_type, y=value)) +
    geom_bar(stat='identity') + theme_few() + 
    labs (x="Harvest treatment",
          y= "Number of seedlings per square meter")+
    #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
    facet_wrap(~ species, nrow=2, ncol=3, scales="free_y") +
    scale_x_discrete(limits=c("na", "thinning_focus", "regen_focus", "other"), #this argument (limits) re-orders the x-axis
                     labels=c("unharvested", "removal", "regeneration", "other")) + #and this argument renames the labels
    theme(axis.text.x = element_text(angle=25, vjust=.7))
  print(p4)
  #ACTION ITEM: need to add error bars to this!

  #testing significance of specific pairs of data:
  #first need to pull in the actual model: 
  test_df <- seedlings_plus_sqm_long[seedlings_plus_sqm_long$species==major_species[4],]
  test_model <- lme(fixed=value ~ treatment_type + forest_type, 
                    data = seedlings_plus_sqm_long[seedlings_plus_sqm_long$species=="yellow birch",], 
                    random = ~ 1 | Stand_name)
  anova(test_model)
  tukey1 <- TukeyHSD(test_model)
  #looks like this only works for ANOVAs anyway......
  #but we don't really need it b/c none of the relationships are signif anyway...
  
  
  #ok now to do the same w/ saplings!
  
  # SAPLING MODELS -------------------------------
  #first step is to get sapling data into the format I want it.
  #second step is to run models like I did with seedlings.
  #third step is to plot that ish.
  
  #starting with SMALL SAPLINGS;
  #first step is to aggregate tallies by plot, total # & for species of interest
  small_sapling_data$total_sum <- small_sapling_data$over_1_ft + small_sapling_data$over_4.5_ft
  small_saplings_plot <- aggregate(total_sum ~ plot_ID, data = small_sapling_data, FUN=sum)
  
  #OK for doing it by species....since they are already summed per species per plot,
  #It seems like the easiest way to do this is subset to species of interest, then pivot_wider
  #OK aggregating 1st to get rid of the azimuth subdivisions!
  small_saplings_sp <- aggregate(total_sum ~ plot_ID + species, data = small_sapling_data, FUN=sum)
  
  #now continuing on w/ subset to species level...
  small_saplings_sp <- small_saplings_sp[small_saplings_sp$species=="ACSA" |
                                 small_saplings_sp$species=="FRAM" | 
                                 small_saplings_sp$species=="FAGR" |
                                 small_saplings_sp$species=="ACRU" |
                                 small_saplings_sp$species=="BEAL",]
  

  #IMPORTANT NOTE: neither of the dataframes I've made so far today have the total # of rows = total # plots...
  #this means when I merge them, will need to include all rows in the plot_info or equivalent DF

  #this is merging total tally + per-species tally, per plot.
  small_saplings_plot$species <- rep("all")
  small_saplings_plot <- rbind(small_saplings_plot, small_saplings_sp)
  
  ##### now to get to the same point w/ LARGE saplings:
  large_sapling_data$total_sum <- large_sapling_data$class_1 + large_sapling_data$class_2 + large_sapling_data$class_3
  large_saplings_plot <- aggregate(total_sum ~ plot_ID, data = large_sapling_data, FUN=sum)
  
  #now for by sp:
  large_saplings_sp <- aggregate(total_sum ~ plot_ID + species, data = large_sapling_data, FUN=sum)
  large_saplings_sp <- large_saplings_sp[large_saplings_sp$species=="ACSA" |
                                           large_saplings_sp$species=="FRAM" | 
                                           large_saplings_sp$species=="FAGR" |
                                           large_saplings_sp$species=="ACRU" |
                                           large_saplings_sp$species=="BEAL",]  
  
  large_saplings_plot$species <- rep("all")
  large_saplings_plot <- rbind(large_saplings_plot, large_saplings_sp)
  
  
  
  ### QUESTION; ### do I need to do the pivot-wider then pivot-longer to make sure all species/plot combos are included?!
  #TBH...I think so!
#SO, NEXT STEPS INCLUDE:
  #1. pivot_wider both sm and lg saplings dfs
  #2. merge sm and lg dfs, then merge w/ plot_info
  #3. coerce NAs to 0
  #4. pivot_longer back via species (double check # of rows!)
  #5. add sm_saplings_tally and lg_saplings_tally to a new col
  #6. calculate saplings/area in another new col
  

small_saplings_plot_wide <- pivot_wider(small_saplings_plot, names_from="species", values_from="total_sum")
large_saplings_plot_wide <- pivot_wider(large_saplings_plot, names_from="species", values_from="total_sum")

#actually 1st merging EACH w/ plot_info

small_saplings_plot_wide <- merge(x=voi_plot, y=small_saplings_plot_wide, all.x=TRUE, by="plot_ID")
large_saplings_plot_wide <- merge(x=voi_plot, y=large_saplings_plot_wide, all.x=TRUE, by="plot_ID")

#then pivot_longer each
small_saplings_plot_long <- pivot_longer(small_saplings_plot_wide, 
                                         cols=c("all", "ACSA", "FRAM", "FAGR", "ACRU", "BEAL"),
                                         names_to="species",
                                         values_to="small_saplings_tally")

large_saplings_plot_long <- pivot_longer(large_saplings_plot_wide, 
                                         cols=c("all", "ACSA", "FRAM", "FAGR", "ACRU", "BEAL"),
                                         names_to="species",
                                         values_to="large_saplings_tally")

#THEN combine

all_saplings_plot <- cbind(small_saplings_plot_long, "large_saplings_tally" = large_saplings_plot_long$large_saplings_tally)

#THEN coerce NAs to 0s 

all_saplings_plot[is.na(all_saplings_plot)] <- 0

#then add...
all_saplings_plot$all_saplings_tally <- all_saplings_plot$small_saplings_tally + all_saplings_plot$large_saplings_tally

#finally, add a column where they're in seedlings PER SQM
all_saplings_plot$all_saplings_sqm <- all_saplings_plot$all_saplings_tally/(120+15) #REFER BACK TO JUNE UNIVARIATE_ANALYSIS_MASTER FOR DEETS ON WHY I DID THIS

  glimpse(all_saplings_plot)

  
#NEXT STEP IS TO  TEST/MODEL THESE, AND ALSO GRAPH THEM!
sp_models <- list()
  for (i in 1:5){
    loop_df <- all_saplings_plot[all_saplings_plot$species==major_species[i],]
    loop_model <- lme(fixed=all_saplings_sqm ~ treatment_type + forest_type, 
                      data = loop_df, 
                      random = ~ 1 | Stand_name)
    sp_models[i] <- loop_model
    print(anova(loop_model))                        
  }

##ugh just gonna do this manually for now b/c it's taking too long lol

saplingmod1 <- lme(fixed=all_saplings_sqm ~ treatment_type + forest_type, 
                   data = all_saplings_plot[all_saplings_plot$species=="all",], 
                   random = ~ 1 | Stand_name)
  
summary(saplingmod1)
anova(saplingmod1)

plot(saplingmod1) #residuals looking non-normal...
qqnorm(y=saplingmod1$residuals)

#so trying w/ a log transform like I did with seedlings below...
saplingmod1.1 <-  lme(fixed=log(all_saplings_sqm + 1) ~ treatment_type + forest_type, 
                      data = all_saplings_plot[all_saplings_plot$species=="all",], 
                      random = ~ 1 | Stand_name)
anova(saplingmod1.1)
plot(saplingmod1.1) #TBH this isn't much better...
qqnorm(y=saplingmod1.1$residuals)

saplingmod1.2 <-  lme(fixed=log(all_saplings_sqm + .01) ~ treatment_type + forest_type, 
                      data = all_saplings_plot[all_saplings_plot$species=="all",], 
                      random = ~ 1 | Stand_name)
anova(saplingmod1.2)
plot(saplingmod1.2) #this one is better BUT tx is no longer signif...
qqnorm(y=saplingmod1.2$residuals)

saplingmod1.3 <-  lme(fixed=sqrt(all_saplings_sqm) ~ treatment_type + forest_type, 
                      data = all_saplings_plot[all_saplings_plot$species=="all",], 
                      random = ~ 1 | Stand_name)
anova(saplingmod1.3)
plot(saplingmod1.3) #this one also looks somewhat better I think?? and is still signif
qqnorm(y=saplingmod1.3$residuals)

shapiro.test(saplingmod1.3$residuals)
#this is the only one of the above that passes the shapiro test, too!
#soooo this is what we're going with??
#library(MASS)
#install.packages("boxcoxmix")
library(boxcoxmix)
bc <- optim.boxcox(formula=all_saplings_sqm+1~treatment_type + forest_type,
                   groups = all_saplings_plot$Stand_name,
                   data = all_saplings_plot[all_saplings_plot$species=="all",])



saplingmod2 <- lme(fixed=all_saplings_sqm ~ treatment_type + forest_type, 
                   data = all_saplings_plot[all_saplings_plot$species=="ACSA",], 
                   random = ~ 1 | Stand_name)

summary(saplingmod2)
anova(saplingmod2)

plot(saplingmod2) #residuals looking non-normal...
qqnorm(saplingmod2$residuals)
shapiro.test(saplingmod2$residuals)

#so trying w/ a log transform like I did with seedlings below...
saplingmod2.1 <-  lme(fixed=log(all_saplings_sqm + 1) ~ treatment_type + forest_type, 
                      data = all_saplings_plot[all_saplings_plot$species=="ACSA",], 
                      random = ~ 1 | Stand_name)
anova(saplingmod2.1)
plot(saplingmod2.1) #TBH this isn't much better...
qqnorm(y=saplingmod2.1$residuals)

saplingmod2.2 <-  lme(fixed=log(all_saplings_sqm + .01) ~ treatment_type + forest_type, 
                      data = all_saplings_plot[all_saplings_plot$species=="ACSA",], 
                      random = ~ 1 | Stand_name)
anova(saplingmod2.2)
plot(saplingmod2.2) #this one is weirder, somehow...
qqnorm(y=saplingmod2.2$residuals)

saplingmod2.3 <-  lme(fixed=sqrt(all_saplings_sqm) ~ treatment_type + forest_type, 
                      data = all_saplings_plot[all_saplings_plot$species=="ACSA",], 
                      random = ~ 1 | Stand_name)
anova(saplingmod2.3)
plot(saplingmod2.3) #this one also weird??
qqnorm(y=saplingmod2.3$residuals)

#none of 'em pass the test according to Shapiro....
shapiro.test(saplingmod2$residuals)

#OK, I think for now I just need to plot these & deal with the post hoc analyses later!

facet_labels2 <- c("all saplings", "sugar maple", "white ash", "American beech", "red maple", "yellow birch")
#reordering levels of the species variable as a factor:
all_saplings_plot$species <- factor(all_saplings_plot$species, levels=unique(all_saplings_plot$species),
                                          labels=facet_labels2)

p5 <- ggplot(data=all_saplings_plot, 
             aes(x=treatment_type, y=all_saplings_sqm,
                 fill=treatment_type)
             ) +
  scale_fill_manual(values=wes_palette("Chevalier1")) +
  geom_boxplot() + theme_few() + 
  labs (x="Harvest treatment",
        y= "Number of saplings per square meter")+
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y") +
  scale_x_discrete(limits=c("na", "thinning_focus", "regen_focus", "other"), #this argument (limits) re-orders the x-axis
                   labels=c("not harvested", "removal focus", "regeneration focus", "other")) + #and this argument renames the labels
  theme(axis.text.x = element_text(angle=35, vjust=.8, hjust=.8)) #play around w/ this some more...
print(p5)
p5 + theme(legend.position="none")


p6 <- ggplot(data=all_saplings_plot, 
             aes(x=treatment_type, y=all_saplings_sqm,
                 fill=treatment_type)
) +
  #scale_fill_manual(values=wes_palette("Chevalier1")) +
  geom_boxplot(aes(group=forest_type)) + theme_few() + 
  labs (x="Harvest treatment",
        y= "Number of saplings per square meter")+
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y") +
  scale_x_discrete(limits=c("na", "thinning_focus", "regen_focus", "other"), #this argument (limits) re-orders the x-axis
                   labels=c("not harvested", "removal focus", "regeneration focus", "other")) + #and this argument renames the labels
  theme(axis.text.x = element_text(angle=35, vjust=.8, hjust=.8)) #play around w/ this some more...
print(p6)
#this isn't working....












# What I did with Maria: -------------------------------

  ## testing things again w/ log-transformed data...
  model10 <- lme(fixed=log(BA_sqm_ha) ~ treatment_type + forest_type, 
                 data = livetree_plot_BA, 
                 random = ~ 1 | Stand_name)
anova(model10)  
summary(model10)

hist(resid(model10))
qqnorm(resid(model10))
shapiro.test(resid(model10))
#results: data [i.e. residuals] deviates significantly from norm dist


#looks like model1 is pretty normal (BA sqm)
#but model8 is NOT (seedling density)
#testing with: plot(), qqnorm(), shapiro.test()
#try: log transform, maybe sqrt? (good for Poisson), maybe box-cox transformation (RESEARCH)

#trying the log transform (plus a small # so there is no log0)
model11 <- lme(fixed=log(tally+1) ~ treatment_type + forest_type, 
              data = seedlings_plus_sqm, 
              random = ~ 1 | Stand_name)
plot(model11) #this might be the way to go?

model12 <- lme(fixed=sqrt(tally) ~ treatment_type + forest_type, 
               data = seedlings_plus_sqm, 
               random = ~ 1 | Stand_name)
plot(model12)
anova(model11)
anova(model8)
anova(model12)
shapiro.test(model11$residuals)
shapiro.test(model12$residuals)

#potentially try transforming the basal area data as well?

model13 <- lme(fixed=tally ~ treatment_type + forest_type, 
               data = seedlings_plus_sqm[seedlings_plus_sqm$species=="white ash", ], 
               random = ~ 1 | Stand_name)
#can use lmer if don't need to account for plot type (varIdent)
#which enables for crossed random vars( vs just nested)
#plot residuals BY treatment type/group to see if variance is different --> need to use varIdent
#check for this, but prob not a big deal... UNLESS I need to look at the plot type
#***keep this in mind when thinking about diversity indices especially!!!!

#more power in a linear model (assuming assumptions are met)- non-linear (NLME/GLMM) less powerful, but still need to tell it which distribution to use!


#getting back to sapling data now......but should also recap post-meeting w/ Maria...


#UPDATE 17 JAN 2022
#IT'S A NEW ERA OF ACTUALLY GETTING MY RESULTS SECTION TOGETHER WOOHOO
#ALSO A NEW ERA OF HAVING ONLY 3 CATEGORIES FOR TREATMENT_TYPE!!!
#will have to redo some things basically. but that's ok!