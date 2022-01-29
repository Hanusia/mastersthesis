# --------------------------------------------------
# Chapter 1 data analysis part 100million...fresh for 2022!!
# 20 Jan 2022
# HH
# --------------------------------------------------
#

#first things first ([I'm the realest])...
stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ash_damage_data <- read.csv("ASH_DAMAGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ground_cover_data <- read.csv("GROUND_COVER_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
cwd_data <- read.csv("CWD_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tidyverse) #includes tidyr, dplyr, tibble!
library(ggplot2)
library(patchwork)
#library(batman) #Why did I have this one?? guess we'll see if I end up needing it...
library(ggthemes)
library(wesanderson)

# Ground cover class analysis w/ PerMANOVA -------------------------------
#Honestly IDK why I am starting with this, but it feels right, so I'm just gonna go with it!!

### BELOW IS COPIED FROM THE SCRIPT 06-08-21_univariate_analysis_master:

## 8-16-21 update ##
#Asked Tony about how to treat this data & he said assign midpoint vals to BB classes,
#then average to the plot level.
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

#View(ground_cover_data)

ground_cover_try <- ground_cover_data %>%
  group_by(plot_ID, life_form) %>% #grouping by plot_ID first and then life_form second
  summarize(avg_cover_val = mean(midpoint_val, na.rm = TRUE))
#View(ground_cover_try)
#Jan. 2022 note: this worked, yay! but in the time since I've worked on this, I think I started using aggregate()
#to do the same thing more efficiently...but we'll stick with this LOL

ground_cover_tidy <- ground_cover_try %>%
  spread(life_form, avg_cover_val)
View(ground_cover_tidy)
#this *should* work now for PerMANOVA!

#installing the needed package:
#install.packages("PERMANOVA")
#library(PERMANOVA)
#turns out this package is VERY new so I'll try using adonis 1st.

#actually, it's looking like I should use the adonis function in the vegan package instead...
library(vegan)
#PERMANOVA AKA NPMANOVA

#next step: need to associate categorical variables w/ this dataframe (@ the plot level)
#vars of interest: for now just start w/ tx type! (Jan '22: and maybe stand for a random var.???)

#need to make input a matrix instead of dataframe!
#Jan 2022 update: ACTUALLY, not sure if I do! let's do some more reading...
#annnnd we are gonna re-approach this!

#### JAN 2022 UPDATES ####
#adonis vs. adonis2 functions: adonis2 has more variables/can do more, but also is slower
#SOURCE TO CITE IN PAPER IS McArdle and Anderson (2001)

#For either, seems like response & independent variables need to be in SEPARATE dataframes 
# or matrices (e.g. dune and dune.env in documentation example)

#SO next step is to create a second dataframe w/ vars of interest in IDENTICAL order to the response df!
#(AKA alphabetical order!)
#so, basically the plot_voi table...?

#back to the ol' standby:
plot_info_plus <- merge(x=plot_info, 
                        y=stand_info, 
                        by="Stand_name", all.x=TRUE)  

#plot_info_plus$plot_ID[1:5]
#ground_cover_tidy$plot_ID[1:5]
#these two are NOT in the same order...let's fix that!
#alphabetizing:
plot_info_plus <- plot_info_plus[order(plot_info_plus$plot_ID),]
#and I think it's already in this order, but just to be on the safe side:
ground_cover_tidy <- ground_cover_tidy[order(ground_cover_tidy$plot_ID),]

#and now, I believe we gotta REMOVE the plot_ID field from the data matrix (ground_cover_tidy df)
ground_cover_tidy <- ground_cover_tidy[, 2:6] #all rows, all cols minus plot_ID

###OK, now I think we're ready to run the function!!

#first, just doing it w/ treatment type as the (only) ind var:
perman1 <- adonis(formula = ground_cover_tidy ~ Treatment, data=plot_info_plus)
perman1
#looks like it worked?! woohoo!
#perman1$aov.tab
#perman1$coefficients
#perman1$coef.sites 
#perman1$f.perms
#these outputs don't seem suuuper useful...
#perman1$terms

#NEXT QUESTION: how to determine, now, which SPECIFIC interactions are signif???
#can we use Tukey's test for this?

#also need to graph it!

#OK, let's run again incorporating forest type!
perman2 <- adonis(formula = ground_cover_tidy ~ Treatment*forest_type, data=plot_info_plus)
perman2
#RESULT: only treatment is signif, not forest type or their interaction. Good to know!!

#also, let's try again w/ sqrt of the data (using adonis2)...
perman3 <- adonis2(formula = ground_cover_tidy ~ Treatment*forest_type, 
                  data=plot_info_plus,
                  sqrt.dist=TRUE)
perman3
#once again: treatment is signif, and others are not!
#so, I feel confident in my result.

#MAJOR Q: how to address nested nature of my data here though??
#A: adonis can address this w/ the parameter STRATA (and maybe also PERMUTATIONS)
#Let's start by just trying w/ strata:

#using site as a block w/ the parameter 'strata':
perman4 <- adonis(formula = ground_cover_tidy ~ Treatment*forest_type, 
                  strata=plot_info_plus$Stand_name, 
                  data=plot_info_plus)
perman4
#the p-val for each var/interaction when accounting for these blocks is just 1...that seems...wrong??
#let's try again, w/ just 1 var this time:
perman5 <- adonis(formula = ground_cover_tidy ~ Treatment, 
                  data=plot_info_plus, 
                  strata=plot_info_plus$Stand_name
                  )
perman5
#once again, p-val is literally 1...

#taking a look @ stucture of the permutations w/ dif combos of vars:
hist(perman1$f.perms)
hist(perman2$f.perms)
hist(perman4$f.perms)
#based on the histogram & result,
#seems to me like something funky is happening......

#maybe try w/ adonis2 instead????
#copying the example in adonis documentation:
perm <- how(nperm=1000) #setting # of permutations as 1000
setBlocks(perm) <- with(data=plot_info_plus, Stand_name) #setting up the block factor of Stand
perman6 <- adonis2(formula = ground_cover_tidy ~ Treatment*forest_type, 
                   data=plot_info_plus,
                   permutations=perm)
perman6
hist(perman6$F)
#once again....this looks funky?!

#OK, from reading this thread online (https://github.com/vegandevs/vegan/issues/384),
#seems like the issue is that using strata restricts analysis to WITHIN each block.
#so, of course there's no dif in plots within the same site, b/c their values for ind vars w/in those blocks are all the same.
#to test this, I'll try doing it instead w/ gap_status, which DOES vary btwn plots w/in site, and see what I get.

#testing w/ gap_status var:
#first, without strata:
perman7 <- adonis(ground_cover_tidy~gap_status,
                  data=plot_info_plus)
perman7 #result: gap status is signif w/o controlling for blocks/sites.
#and now using strata:
perman8 <- adonis(ground_cover_tidy~gap_status,
                  data=plot_info_plus,
                  strata=plot_info_plus$Stand_name)
perman8 #result: gap status is STILL signifcant w/ addition of controlling for blocks!
#so, the problem lies in how I'm going about this, not that my data are inherently not signif...

#ACTION ITEM: ask Tony whether to worry about this/if he has a suggestion tomorrow?
#OR, could go with Maria's method, which requires doing it manually and seems much harder (LOL), 
#using the permutations script she very kindly shared w/ me
#to paraphrase the helpful thread I linked above, seems like the issue is 
#that I should have site as a RANDOM factor, not as a block...
#but there isn't a way to do this w/in adonis.

#FOR NOW: gonna talk about this w/ Tony tomorrow (and/or email),
#when I come back to it, remember to also move into the POST HOC ANALYSIS
#if I do end up with anything signif, using this as a guide: https://www-researchgate-net.ezproxy.uvm.edu/post/Posthoc_test_for_permanova_adonis

#actually, gonna go ahead and graph these b/c why not!

#Tony's rec: run PerMANOVA w/ just site as var to see if it's signif.
perman9 <- adonis(ground_cover_tidy~Stand_name,
                  data=plot_info_plus)
perman9

##############################################

#FRIDAY 1/28/22 UPDATE after talking to Tony:
#forest type can represent site(/random) effect in a way, so can just run this w/ tx*forest type as ind vars and call that good!

#Q: what about gap status? (Just gonna ignore it for now TBH!)

#SO, we wanna go w/ perman 2 for results!
#perman2

#stats to report = F & p

### AND NOW NEED TO DO POST HOC ANALYSIS...

#gonna try the answer from this link using package pairwiseAdonis: https://www-researchgate-net.ezproxy.uvm.edu/post/Posthoc_test_for_permanova_adonis

#install.packages('devtools')
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)
pair.mod<-pairwise.adonis(ground_cover_tidy,factors=plot_info_plus$Treatment)
pair.mod

#so, PAIRWISE result is that unharvested vs. removal is only signif dif.
#makes sense since that would make up the bulk of the "harvested" group anyway...
#maybe: re-do this as just harvested vs. unharvested?
#&recreate the graph accordingly? Food for thought!

#testing this:
perman10 <- adonis(formula = ground_cover_tidy ~ harvest_status, data=plot_info_plus)
perman10
#also very signif, which makes sense!!

#and does it interact w/ forest type?
perman11 <- adonis(formula = ground_cover_tidy ~ harvest_status*forest_type, data=plot_info_plus)
perman11 #and NOW the interaction is signif (maybe b/c of larger sample size by grouping together all harvested plots)...interesting!

#should I ask Tony about this?
#gonna also run a post-hoc test for this one: 
pair.mod2<-pairwise.adonis(ground_cover_tidy,factors=plot_info_plus$harvest_status*plot_info_plus$forest_type)
#JK, can't include the ~interaction~ in there...maybe try something creative?

#making a fake column/vector that combines the two variables of interest into its own kind of factor
plot_info_fake <- paste(plot_info_plus$harvest_status, plot_info_plus$forest_type)
unique(plot_info_fake)
pair.mod3 <-pairwise.adonis(ground_cover_tidy,factors=plot_info_fake)
pair.mod3
#OK, interesting result! not really sure what to make of it, to be honest LOL

#maybe email Tony from here and see what he says??

#one more thing first-check interaction w/ gap status:
#IMPORTANT CAVEAT that gap status is correlated w/ harvest status, b/c gaps are only present in harvested stands!!

##############################################
  
# Graphing ground cover classes! -------------------------------
#throwing together w/in 1 DF:
ground_cover_vars <- ground_cover_tidy 
ground_cover_vars$plot_ID <- plot_info_plus$plot_ID
ground_cover_vars$Treatment <- plot_info_plus$Treatment 
ground_cover_vars$forest_type <- plot_info_plus$forest_type
#LOL, actually need to spread these out first to graph 'em:
ground_cover_vars_long <- pivot_longer(ground_cover_vars,
                                  cols=B:S, #cols to pivot
                                  names_to="cover_class",
                                  values_to="percent_cover")

groundcover_p1 <- ggplot(data=ground_cover_vars_long,
                         aes(fill=as.factor(cover_class), y=percent_cover,
                             x=as.factor(Treatment))) +
                  geom_bar(position="dodge", stat="identity") + #places groups of bars next to each other & lets gg know I'm providing y-var
                  theme_few()
groundcover_p1
#woohoo! Looks like it worked :)
#but...why aren't my themes working??? LOL
#things to change: add forest type var, edit axis labels, edit legend

groundcover_p2 <- ggplot(data=ground_cover_vars_long,
                         aes(fill=as.factor(cover_class), y=percent_cover,
                             x=as.factor(Treatment))) +
  geom_bar(position="dodge", stat="identity") + #places groups of bars next to each other & lets gg know I'm providing y-var
  theme_few() + 
  scale_fill_manual(values=wes_palette("Darjeeling1", n=5),
                    name="Cover type", 
                    labels = c("Bryophytes", "Bare Soil", "Ferns and Allies", "Herbs", "Grasses and Sedges")) +
  labs (x="Treatment", y= "Average percent cover") +
  facet_wrap( ~ forest_type) #need to spell out forest type names...later (using param 'labeller')
print(groundcover_p2)
#alright...I think this is looking in good shape! Aside from 
#the forest type names, colors, & signif indicators,
#which I will add later.


# switching gears to....CWD! / dead wood in general. (whoo!) -------------------------------
#(side note: this whole thing I thought CWD stood for coarse woody debris, 
#but now I learn it stands for coarse woody detritus...consider me SHOOK)

#goals to analyze:
#CWD volume by tx & forest type
#CWD biomass by tx & forest type
#citation = Harmon et al. 2008? (for biomass...deal w/ that later)
#but for volume, using Harmon and Sexton 1996 fomula 7!
#aka: V=9.869*Sum(d^2/8L)
#analysis=LMM? Or just a regular ol' ANOVA?

#alsooo, snag volume & snag BA

### first step = convert CWD data to volume per HA (per stand)!

View(cwd_data)
#calculating volume per STAND, per total transect length, & Fraver et al. 2018 protocols.
#(although I may eventually have to calculate it per plot too....and that's OK!!! but sticking with one thing at a time here)
#sooo, this will basically entail setting up a for loop to iterate thru each piece of CWD...
#OR, could try doing it w/o a loop? let's see...

#first substep = convert diam to m (from cm) and square it

cwd_data$diam_m_sq <- (cwd_data$diam_cm/100)*(cwd_data$diam_cm/100)

#next step is to calculate total transect length per stand...
#maybe do this outside of the loop b/c it won't take as long that way?
#eg:
stand_info$transect_length_m <- stand_info$num_plots*3*11.3
#this will calculate the total transect length (in m) for each stand, given # plots, w/ 3 transects per plot
#now need to concatenate this # (and stand name!) w/ the cwd_data df, since that's the only other var we need to calculate volume

cwd_data <- merge(x=cwd_data, y=plot_info[, c("Stand_name", "plot_ID")])
#make sure it has 2127 entries/rows both before&after this merge!
#uh-oh...we're missing one!!
#lsty <- setdiff(x=unique(cwd_data$plot_ID), y=plot_info$plot_ID)
#OK, figured it out, so don't need the above line anymore (but keeping in b/c it's a useful example)
cwd_data <- merge(x=cwd_data, y=stand_info[, c("Stand_name", "transect_length_m")])
#NOTE: not all plots have CWD (as determined by length(unique(cwd_data$plot_ID)),
#but all stands do....so this should be OK for analysis.

#AT THIS POINT, divesting cwd_data into a new df b/c I might need to repeat this anlysis w/ plots, and don't want things to get overly confusing!!
cwd_data_stand <- cwd_data
View(cwd_data_stand)
#now, doin' the math of d^2/8L, which will eventually be summed for each stand to get final volume.
cwd_data_stand$diamsq_over_8L <- cwd_data_stand$diam_m_sq / (8*cwd_data_stand$transect_length_m)
#next, aggregating by stand (basically summing/doing the 'sigma' part of the formula here)
cwd_data_stand <- aggregate(formula=diamsq_over_8L ~ Stand_name, FUN=sum, data=cwd_data_stand)
#and finally, multiplying by the constant to get volume per area in m^3/m^2:
cwd_data_stand$stand_vol_m2_m3 <- cwd_data_stand$diamsq_over_8L*9.869 #THIS COL NAMED WRONG BUT IT'S OK!!!
#annnd now, convert to cubic m per hectare, FROM cubic m per square m:
cwd_data_stand$vol_m3_ha <- cwd_data_stand$stand_vol_m2_m3*10000
#gut check: are these values approx similar in scale to other reported values?
#yes!
#OK SO now looking back at all these steps, it probably WOULD HAVE been faster/more efficient to set up a for loop!!
#live and learn: 1-2 step processes may be easier to just use a standalone func like aggregate, but longer ones benefit from looping!
#altho, at least I wasn't repeating any steps manually...the functions did the work for me...so not a total fail!

### next step: calculate average + SE volume by tx & forest type

#first substep=attach those vars to df
cwd_data_stand <- merge(cwd_data_stand, stand_info[, c("Stand_name", "Treatment", "forest_type")])


#next step = calc mean per tx type & forest type.
#again, here is maybe the place for a loop....but is it actually faster b/c only repeating a couple times?? IDK
#well, it definitely would be a more ORGANIZED output in a loop, but it's late and I'm tired! and I know how to do a loop!! maybe I'll redo this tomorrow instead.
mean(cwd_data_stand$vol_m3_ha[cwd_data_stand$Treatment=="unharvested"])
mean(cwd_data_stand$vol_m3_ha[cwd_data_stand$Treatment=="regeneration"])
mean(cwd_data_stand$vol_m3_ha[cwd_data_stand$Treatment=="removal"])

#install.packages("plotrix") #to use easy standard error function
library(plotrix)

std.error(cwd_data_stand$vol_m3_ha[cwd_data_stand$Treatment=="unharvested"])
std.error(cwd_data_stand$vol_m3_ha[cwd_data_stand$Treatment=="regeneration"])
std.error(cwd_data_stand$vol_m3_ha[cwd_data_stand$Treatment=="removal"])

#and now for forest type:
mean(cwd_data_stand$vol_m3_ha[cwd_data_stand$forest_type=="NH"])
mean(cwd_data_stand$vol_m3_ha[cwd_data_stand$forest_type=="RNH"])

std.error(cwd_data_stand$vol_m3_ha[cwd_data_stand$forest_type=="NH"])
std.error(cwd_data_stand$vol_m3_ha[cwd_data_stand$forest_type=="RNH"])

#OK, that is enough for tonight!!!!
#when I revisit this, next steps will include:
#calculating biomass of CWD (seems a bit trickier, but hopefully i can steer in right direction...)
#and calculating/comparing CWD among tx & site groups (using ANOVA or LMM!)


### UPDATES 1/28/2022

#Tony says: in calculating volume, need to incorporate VOLUME REDUCTION FACTOR for decay classes 4 & 5,
#since they are likely no longer the shape assumed by that catchall equation!

#SO, I found a volume reduction factor (VRF) from Fraver et al. 2013 (and used in Russell et al. 2014)
#where calculated volume (for a piece!) is multiplied by 0.800 for DC 4, and 0.412 for DC 5.

#this is separate from the DENSITY reduction factor that will need to be taken into account for biomass!

#OK, so now to recalculate CWD volume taking all this into account:
##need to keep the decay class in play here!

#ALSO, need to maybe find another version of that equation for volume, b/c it takes stand/transect length 
#already into account....need to calculate volume *individually* per piece...
#cites Fraver et al. 2007: "Refining volume estimates of down woody debris" (seems promising!)
#JK, that one is too complicated for me!!
#Bolton and D'Amato 2011 also validates the same formula I've been using, which is pi-squared times
#the sum of each diameter squared divided by 8L.
#so...I THINK this is fine??? Just gotta figure out if it works to have volume in
#this format when it comes to using it to calculate biomass!
#but...maybe that's a question for another day!!

#OK, so basically need to grab & modify a bunch of the stuff I did w/ cwd_data
#to incorporate that decay class & calc volume at a per-item level first.

## COPIED FROM ABOVE (yesterday's code):

#first substep = convert diam to m (from cm) and square it
cwd_data$diam_m_sq <- (cwd_data$diam_cm/100)*(cwd_data$diam_cm/100)

#next step is to calculate total transect length per stand...
stand_info$transect_length_m <- stand_info$num_plots*3*11.3
#this will calculate the total transect length (in m) for each stand, given # plots, w/ 3 transects per plot
#now need to concatenate this # (and stand name!) w/ the cwd_data df, since that's the only other var we need to calculate volume

cwd_data <- merge(x=cwd_data, y=plot_info[, c("Stand_name", "plot_ID")])
#make sure it has 2127 entries/rows both before&after this merge!
#uh-oh...we're missing one!!
#lsty <- setdiff(x=unique(cwd_data$plot_ID), y=plot_info$plot_ID)
#OK, figured it out, so don't need the above line anymore (but keeping in b/c it's a useful example)
cwd_data <- merge(x=cwd_data, y=stand_info[, c("Stand_name", "transect_length_m")])
#NOTE: not all plots have CWD (as determined by length(unique(cwd_data$plot_ID)),
#but all stands do....so this should be OK for analysis.

#AT THIS POINT, divesting cwd_data into a new df b/c I might need to repeat this anlysis w/ plots, and don't want things to get overly confusing!!
cwd_data_calc <- cwd_data
View(cwd_data_calc)
#now, doin' the math of (pi-squared)*(d^2/8L), which will eventually be summed for each stand to get final volume.
cwd_data_calc$pisq_diamsq_over_8L <- pi * pi * cwd_data_calc$diam_m_sq / (8*cwd_data_calc$transect_length_m)

#NEXT, applying volume reduction factor for decay classes 4 & 5!
#nowww is a time to use a loop... (or could still do it without, probably, lol)
cwd_data_calc$vol_VRF_applied <- rep(0) #creating new col to store updated volume

for(i in 1:nrow(cwd_data_calc)){
  if(cwd_data_calc$decay_class[i]==4){ #for CWD of DC 4,
    cwd_data_calc$vol_VRF_applied[i] <- 0.800*cwd_data_calc$pisq_diamsq_over_8L[i]
  #applying the VRF of 0.800 for these logs & storing that val in a new column!
  } else if(cwd_data_calc$decay_class[i]==5){
    cwd_data_calc$vol_VRF_applied[i] <- 0.412*cwd_data_calc$pisq_diamsq_over_8L[i]
    #applying the VRF of 0.412 for these logs & storing that val in a new column!
    } else cwd_data_calc$vol_VRF_applied[i] <- cwd_data_calc$pisq_diamsq_over_8L
    #and for all other logs (in DC 1, 2, and 3), keep vol the same!
}

#getting an error message...is it the NA values??
#UPDATE: looks like it! It basically just stopped after encountering the first NA for DC!
#SOLUTION = remove NAs??

###STOPPED HERE AS OF 1/28, BELOW THE LINE NOT YET UPDATED!!
#next, aggregating by stand (basically summing/doing the 'sigma' part of the formula here)
cwd_data_calc <- aggregate(formula=diamsq_over_8L ~ Stand_name, FUN=sum, data=cwd_data_calc)

#annnd now, convert to cubic m per hectare, FROM cubic m per square m:
cwd_data_calc$vol_m3_ha <- cwd_data_calc$stand_vol_m2_m3*10000
