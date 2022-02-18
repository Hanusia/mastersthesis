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

###COMMENTING OUT THE SECTION BELOW B/C I REPLACED IT W/ UPDATED CODE FURTHER DOWN!!

##############################################
#View(cwd_data)
#calculating volume per STAND, per total transect length, & Fraver et al. 2018 protocols.
#(although I may eventually have to calculate it per plot too....and that's OK!!! but sticking with one thing at a time here)
#sooo, this will basically entail setting up a for loop to iterate thru each piece of CWD...
#OR, could try doing it w/o a loop? let's see...

#first substep = convert diam to m (from cm) and square it

#cwd_data$diam_m_sq <- (cwd_data$diam_cm/100)*(cwd_data$diam_cm/100)

#next step is to calculate total transect length per stand...
#maybe do this outside of the loop b/c it won't take as long that way?
#eg:
#stand_info$transect_length_m <- stand_info$num_plots*3*11.3
#this will calculate the total transect length (in m) for each stand, given # plots, w/ 3 transects per plot
#now need to concatenate this # (and stand name!) w/ the cwd_data df, since that's the only other var we need to calculate volume

#cwd_data <- merge(x=cwd_data, y=plot_info[, c("Stand_name", "plot_ID")])
#make sure it has 2127 entries/rows both before&after this merge!
#uh-oh...we're missing one!!
#lsty <- setdiff(x=unique(cwd_data$plot_ID), y=plot_info$plot_ID)
#OK, figured it out, so don't need the above line anymore (but keeping in b/c it's a useful example)
#cwd_data <- merge(x=cwd_data, y=stand_info[, c("Stand_name", "transect_length_m")])
#NOTE: not all plots have CWD (as determined by length(unique(cwd_data$plot_ID)),
#but all stands do....so this should be OK for analysis.

#AT THIS POINT, divesting cwd_data into a new df b/c I might need to repeat this anlysis w/ plots, and don't want things to get overly confusing!!
#cwd_data_stand <- cwd_data
#View(cwd_data_stand)
#now, doin' the math of d^2/8L, which will eventually be summed for each stand to get final volume.
#cwd_data_stand$diamsq_over_8L <- cwd_data_stand$diam_m_sq / (8*cwd_data_stand$transect_length_m)
#next, aggregating by stand (basically summing/doing the 'sigma' part of the formula here)
#cwd_data_stand <- aggregate(formula=diamsq_over_8L ~ Stand_name, FUN=sum, data=cwd_data_stand)
#and finally, multiplying by the constant to get volume per area in m^3/m^2:
#cwd_data_stand$stand_vol_m2_m3 <- cwd_data_stand$diamsq_over_8L*9.869 #THIS COL NAMED WRONG BUT IT'S OK!!!
#annnd now, convert to cubic m per hectare, FROM cubic m per square m:
#cwd_data_stand$vol_m3_ha <- cwd_data_stand$stand_vol_m2_m3*10000
#gut check: are these values approx similar in scale to other reported values?
#yes!
#OK SO now looking back at all these steps, it probably WOULD HAVE been faster/more efficient to set up a for loop!!
#live and learn: 1-2 step processes may be easier to just use a standalone func like aggregate, but longer ones benefit from looping!
#altho, at least I wasn't repeating any steps manually...the functions did the work for me...so not a total fail!

##############################################

## ALSO, MOVED THE PER-STAND CALCULATIONS DOWN BELOW TO FOLLOW NEW CODE! (BUT USING SAME VARS AS BEFORE)

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
  if(is.na(cwd_data_calc$decay_class[i])==FALSE){ 
    #filtering out any piece w/o a decay class, so as to not mess up subsequent if statements
  if(cwd_data_calc$decay_class[i]==4){ #for CWD of DC 4,
    cwd_data_calc$vol_VRF_applied[i] <- 0.800*cwd_data_calc$pisq_diamsq_over_8L[i]
  #applying the VRF of 0.800 for these logs & storing that val in a new column!
  } else if(cwd_data_calc$decay_class[i]==5){
    cwd_data_calc$vol_VRF_applied[i] <- 0.412*cwd_data_calc$pisq_diamsq_over_8L[i]
    #applying the VRF of 0.412 for these logs & storing that val in a new column!
    } else cwd_data_calc$vol_VRF_applied[i] <- cwd_data_calc$pisq_diamsq_over_8L[i]
    #and for all other logs (in DC 1, 2, and 3), keep vol the same!
  }
}

#getting an error message...is it the NA values??
#UPDATE: looks like it! It basically just stopped after encountering the first NA for DC!
#SOLUTION = remove NAs??

###STOPPED HERE AS OF 1/28, BELOW THE LINE NOT YET UPDATED!! 
#NEED TO DEBUG THE FOR LOOP ABOVE BY GETTING RID OF NAs FIRST 

## 1/31/22 update: fixed the for loop by adding a statement filtering out logs w/o a DC!
# (looks like it's just one, an ash log in the plot CWEunh3...shame b/c it's a pretty big ash log!)
# (and funny b/c I was just talking about Corinth today)

#next, convert to cubic m per hectare, FROM cubic m per square m:
cwd_data_calc$vol_m3_ha <- cwd_data_calc$vol_VRF_applied*10000

#finally, aggregating by stand (basically summing/doing the 'sigma' part of the formula here)
cwd_data_stand <- aggregate(formula=vol_m3_ha ~ Stand_name, FUN=sum, data=cwd_data_calc)

View(cwd_data_stand)


### next step: calculate average + SE volume by tx & forest type
#THIS STEP CUT&PASTED FROM 1/28, MOVED TO HERE

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





### CWD BIOMASS CALCULATION! ###

# using methods & info from Harmon et al. 2008, which includes a table of density reduction factors per sp/DC

#first step = find & import those tables!
#SEEMS to be helpfully uploaded here: 
# https://andrewsforest.oregonstate.edu/sites/default/files/lter/pubs/webdocs/reports/detritus/GTR_estimates_site/Templates/tables.shtml

#basically, mass = density * volume...
#the key is associating the CORRECT density val for each piece, based on sp & decay class...
#Q: to use absolute or relative density??
#A: use absolute; rel. density is just a proportion of the green wood/non-decayed density, not exactly helpful for my purposes!
# OR, as in Russell et al. 2016, use initial density * decay class reduction factor (DCRF)
#so...that is basically calculating the abs density on the spot, based on rel density.
#they ALSO cite Harmon et al. 2011; is that more updated values?? (Let's take a look now!)

#IMPORTANT Q: how to deal w/ CWD of unknown species?? b/c a LOT of ours is that...

#IMPORTANT ANsWER; this 2011 Harmon citation includes avg density values for hardwood/softwood by DC!!
# "Differences between standing and downed dead tree... etc Table 5 & 6)
# or go off of Harmon et al. 2008 Fig. 2 values?

#tables from Harmon et al. report in units of g/cm^3, so I'll need to convert units, too!

cwddensityvals <- read.csv("appendix_CWD_DENSITY_PREDICTIONS_FINAL_table.csv", fileEncoding = "UTF-8-BOM")
View(cwddensityvals)

cwddensity_metadata <- read.csv("appendix_CWD_DENSITY_PREDICTIONS_FINAL_metadata.csv", fileEncoding = "UTF-8-BOM")
View(cwddensity_metadata)

#IMPORTANT TO REMEMBER, MY SPECIES CODES OFTEN DIFFER FROM FIA ONES!!!!!!!
#refer to code I used to determine LANDIS species list to see how I converted between them...



#NEXT STEP = GET THE SAME (FIA) SPECIES CODES ASSOCIATED W/ MY CWD TABLE

#import species code list (or look @ my old code that did this??)
#USING SOME CODE FROM LANDIS_specieslist_using_plotoccurrences_3Dec2021.R

#first, loading in data
#FIA_species_table <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/FIA/2021 Master Species FGver9-1_9_2021_final.csv")
my_species_table <- read.csv("C:/Users/theha/OneDrive - University of Vermont/Ash project/EAB_project_2020_additional_data.csv", fileEncoding = "UTF-8-BOM")

#need to associate w/ part of the metadata file from Harmon et al...
#so first, subset it:
cwddensity_splist <- cwddensity_metadata[33:293, 2:4]
colnames(cwddensity_splist) <- cwddensity_splist[1,]
cwddensity_splist <- cwddensity_splist[2:nrow(cwddensity_splist),]

#NEXT, gonna create a "cleaner" data table for this specific set of species names/codes:

mycwd_splist <- data.frame("my_spcode" = unique(cwd_data_calc$species))
#next, merge w/ my own code list : common names:
mycwd_splist <- merge(x=mycwd_splist, y=my_species_table[,c("spec_code", "genus", "species")], 
                      all.x=TRUE, by.x="my_spcode", by.y="spec_code")


#and NOW, try merging w/ FIA species names from metadata file:
mycwd_splist <- merge(x=mycwd_splist, y=cwddensity_splist, 
                      all.x=TRUE, by=c("genus", "species"))
#YAYYY IT WORKED! Now just need to fill in the ones that didn't go over:
mycwd_splist$code[(mycwd_splist$my_spcode=="UNK" | mycwd_splist$my_spcode=="unk")] <- "UNKN"
mycwd_splist$code[mycwd_splist$my_spcode=="ACSP"] <- "ACER"
mycwd_splist$code[mycwd_splist$my_spcode=="BESP"] <- "BESP"
mycwd_splist$code[mycwd_splist$my_spcode=="PISP"] <- "PICE"
mycwd_splist$code[mycwd_splist$my_spcode=="TSCA"] <- "TSCA"
mycwd_splist$code[mycwd_splist$my_spcode=="UNKSW"] <- "SOFTW"

### OKAY TWO THINGS I REALIZED LOOKING AT THIS:
#1) MY SPECIES CODES WERE ACTUALLY THE EXACT SAME AS FIA'S IN PRETTY MUCH EVERY CASE HERE....
#   AT LEAST FOR THE ONES USED IN THIS TABLE...SO THIS MERGING STEP IS PRETTY POINTLESS AND A WASTE OF TIME!

#2) BUT, FILE my_species_table HAS ERRONEOUSLY CODED BETULA ALLEGHENSIS (yellow birch) AS BETULA LENTA (sweet birch)...
#   WHAT ARE THE IMPLICATIONS OF THIS???
#   IF I'M REMEMBERING CORRECTLY, I USED COMMON NAME (instead of genus/sp) TO ASSOCIATE THESE TABLES BEFORE,
#   SO IT SHOULD HAVE WORKED OUT OK FOR WHAT I USED THIS TABLE FOR IN THE PAST.
#   BUT I SHOULD REALLY DOUBLE CHECK ON THAT LATER, JUST DON'T HAVE ENERGY TO DO SO NOW.

#ANYWAY, getting back to the actual code, no need to do most of what's above...
#but may as well keep what I have so far??

#NEED TO DOUBLE CHECK THESE AGAINST THE ACTUAL DATA TABLE b/c there are some discrepancies w/ the metadata...
#ok, looks good!

#NEXT STEP=MERGE FIA CODE ONTO CWDDATACALC DATAFRAME!
cwd_data_calc <- merge(x=cwd_data_calc, y=mycwd_splist[,c("my_spcode", "code")],
                       all=TRUE, by.x="species", by.y="my_spcode")


#NEXT, I'm gonna add a couple rows to the actual cwddensityvals table to account for unk species.
#(drawing from Harmon et al. 2011, Table 4/5) to make sure the for loop can run smoothly!!

#ADDING VALUES FOR UNKNOWN SPECIES & UNKNOWN SOFTWOODS:
#first creating a vector of values for each species:
unkvals <- data.frame(NA, NA, "UNKN", NA, 0.40, NA, NA, 0.33, NA, NA, 0.26, NA, NA, 0.15, NA, NA, 0.11, NA, NA)
names(unkvals) <- names(cwddensityvals) #setting up an extra row for unknown species CWD

unkswvals <- data.frame(NA, NA, "SOFTW", NA, .38, NA, NA, 0.34, NA, NA, .27, NA, NA, .15, NA, NA, 0.11, NA, NA)
names(unkswvals) <- names(cwddensityvals) #setting up an extra row for unknown softwood CWD

#now to combine them.....

#testing123 <- rbind(cwddensityvals, unkvals, unkswvals)
cwddensityvals <- rbind(cwddensityvals, unkvals, unkswvals)
#woohoo it worked!

#AND THEN will build a for loop to associate the correct density value with each piece of CWD :) 

#new column to hold density val:
cwd_data_calc$density_g_cm3 <- rep(0)
colassign <- rep(0) #and intermediate var 

for(i in 1:nrow(cwd_data_calc)){ #goin' by row in the CWD table,
  if(is.na(cwd_data_calc$decay_class[i])==FALSE){ 
    #filtering out any piece w/o a decay class, so as to not mess up subsequent if statements
    for(j in 1:nrow(cwddensityvals)){ #now, iterating thru rows of the density table, AKA species
      if(cwd_data_calc$code[i]==cwddensityvals$code[j]){ #matching up the species...
         #NOW, finding the correct column to attach by decay class...doing some ~fun~ math:
          colassign[i] <- (cwd_data_calc$decay_class[i]-1)*3+5 #math to choose the right column density val based on DC
          #and the grand finale...attributing density val w/ correct sp & DC
          cwd_data_calc$density_g_cm3[i] <- cwddensityvals[j,colassign[i]]
      }
    }
  }
}

#WOOHOO, it worked :) 

#NEXT = fix units & calculate biomass!!

#GREAT NEWS!: target units for biomass are Mg (megagrams) per hectare.
#so to get there, need to multiply calculated volume (in cubic meters per ha)
#by a density in units of Mg per cubic meter.
#BUT, we are currently in grams per cubic centimeter....but because both 
#changes to the units are in the direction of 10^6, the conversion cancels out!
#so, the number of grams per cubic centimeter is the SAME as the number of 
#megagrams per cubic meter.
#we love to see it!!!

#OK, now can simply calculate biomass! (in Mg/ha)
cwd_data_calc$biomass_Mg_ha <- cwd_data_calc$vol_m3_ha*cwd_data_calc$density_g_cm3

#aggregate by stand: 
cwd_biomass_stand <- aggregate(formula=biomass_Mg_ha ~ Stand_name, FUN=sum, data=cwd_data_calc)
#and recombine w/ df cwd_data_stand:
cwd_data_stand <- merge(x=cwd_data_stand[, c("Stand_name", "vol_m3_ha", "Treatment", "forest_type")],
                        y=cwd_biomass_stand, by="Stand_name")

#and just to save from repeating future work:
#cwd_volume_stand <- cwd_data_stand[,c("Stand_name", "vol_m3_ha")]

#annnnd finally, calculate mean & SE per tx/forest type!
#ACTION ITEM: investigate how each sp/DC "uncertainty" value for density factors into calculating SE...

mean(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$Treatment=="unharvested"])
mean(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$Treatment=="regeneration"])
mean(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$Treatment=="removal"])

std.error(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$Treatment=="unharvested"])
std.error(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$Treatment=="regeneration"])
std.error(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$Treatment=="removal"])

#and now for forest type:
mean(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$forest_type=="NH"])
mean(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$forest_type=="RNH"])

std.error(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$forest_type=="NH"])
std.error(cwd_data_stand$biomass_Mg_ha[cwd_data_stand$forest_type=="RNH"])



#ALRIGHT, done with these calculations!!!

#steps for next time:
#1) look up/figure out how the density uncertainty values factor into stand-level/
#variable/level SE...
#   a) first, look @ examples of this in Harmon et al. 2008 (appendix A?)
#2) check for normality in the stand-level biomass & volume data...
#   a) graph residuals (?)
#   b) do the test I did with Maria
#   c) basically, just look at what I did with Maria LOL
#3) if all looks good, proceed w/ an ANOVA (since doing @ the stand level,
# don't need to account for random effect of site/stand) for tx*forest type!
#4) maybe--graph biomass by decay class (stacked) & tx/forest type?

##############################################

### 2/3/2022 return to CWD!

#when it comes to standard error- need to ask Tony how to incorporate those 
#density uncertainty values (& maybe also uncertainty for the vol. reduct. factors??)

#meanwhile, I should save the cwd_data_stand as a table file prob, so don't 
#need to re-run those calculations all the time??

#but FIRST, gonna run some ANOVAs!

cwd_vol_anova1 <- aov(vol_m3_ha ~ Treatment*forest_type, data=cwd_data_stand)
summary(cwd_vol_anova1) #Treatment is signif, but nothing else! Sounds good to me

#post hoc analysis w/ Tukey's test:
tukey1 <- TukeyHSD(cwd_vol_anova1)
tukey1

tuke2 <- TukeyHSD(cwd_vol_anova1, which="Treatment")
tuke2 #once again, it's just unharvested vs. removal that's signif 
#(unharvested vs regeneration soemwhat close, which also checks out
#and is similar to results from PerMANOVA)

cwd_biomass_anova1 <- aov(biomass_Mg_ha ~ Treatment*forest_type, data=cwd_data_stand)
summary(cwd_biomass_anova1) #same story- just treatment is signif!

tukey3 <- TukeyHSD(cwd_biomass_anova1)
tukey3

tukey4 <- TukeyHSD(cwd_biomass_anova1, which="Treatment")
tukey4 #again, pretty much the same pattern....

#forgot to check for normality first LOL (clairification: whether RESIDUALS are normal)
#let's try it...
#plot(cwd_vol_anova1) #HMM, this looks a little weird!
#qqnorm(y=cwd_vol_anova1$residuals) #looks a lil funky at the end?
#(qqnorm is redundant w/ "plot" for anova)

#remember, this is testing normality of the RESIDUALS
shapiro.test(cwd_vol_anova1$residuals)

#what about normality of the DATA itself?
#shapiro.test(cwd_data_stand$vol_m3_ha)
#qqnorm(cwd_data_stand$vol_m3_ha)

#OK, now [testing normality] for biomass!
#plot(cwd_biomass_anova1)
shapiro.test(cwd_biomass_anova1$residuals) #looks OK! (p-val above 0.05)
#and just for fun, normality of the data itself:
#shapiro.test(cwd_data_stand$biomass_Mg_ha) #NOT normal...but that's ok!

#slightly random...testing presence per decay class per tx?
#cwd_dc_test <- merge(x=cwd_data_calc, y=plot_info_plus[,c("plot_ID", "Treatment")], by="plot_ID")
#cwd_dc_test <- aggregate(diam_cm ~ Treatment*decay_class, data=cwd_dc_test, FUN = sum)
#RESULT: all decay classes present in each treatment!

##############################################

### IDEA: do a PerMANOVA of CWD decay class by treatment/forest type???
#maybe don't make things more complicated for myself than they need to be, though...
#IF i do- first step would be creating a "wider" format of biomass per decay class (cols) & stand (rows)
cwd_biomass_wider <- aggregate(biomass_Mg_ha ~ Stand_name*decay_class, data=cwd_data_calc, FUN=sum)
cwd_biomass_wider <- pivot_wider(data=cwd_biomass_wider, id_cols=Stand_name, 
                                 names_from=decay_class, values_from=biomass_Mg_ha)
#now we have it in wide format, replace NAs with 0s...
cwd_biomass_wider[is.na(cwd_biomass_wider)] <- 0
#and finally remove first col...or just do it in the permanova code??

#and finally, try a perMANOVA?
cwd_biomass_perman1 <- adonis(cwd_biomass_wider[,2:6] ~ Treatment*forest_type, data=cwd_data_stand)
cwd_biomass_perman1 #nothing significant--that makes it easy!!

cwd_biomass_perman2 <- adonis(cwd_biomass_wider[,2:6] ~ Treatment, data=cwd_data_stand)
cwd_biomass_perman2 #same story here!

##############################################

# graphing CWD biomass -------------------------------


#first need to get data in the right config. w/ DC:
cwd_data_calc <- merge(x=cwd_data_calc, y=plot_info_plus[,c("plot_ID", "Treatment", "forest_type")], by="plot_ID")
cwd_dc_graph <- aggregate(biomass_Mg_ha ~ Stand_name*decay_class, data=cwd_data_calc, FUN = sum)
cwd_dc_graph <- merge(x=cwd_dc_graph, y=stand_info[,c("Stand_name", "Treatment")], by="Stand_name")
cwd_dc_graph <- aggregate(biomass_Mg_ha ~ Treatment*decay_class, data=cwd_dc_graph, FUN = sum)

#SOMETHING IS STILL WRONG WITH THIS SOMEHOW!!
#check on process above: 
#sum(cwd_dc_graph$biomass_Mg_ha[cwd_dc_graph$Treatment=="unharvested"]) #wayy too high
#but what DOES work:
#sum(cwd_dc_graph$biomass_Mg_ha[cwd_dc_graph$Treatment=="regeneration"]/4)
#need to divide each val by the # of stands in its Treatment category...
cwd_dc_graph$mean_biomass <- rep(0)
cwd_dc_graph$mean_biomass[cwd_dc_graph$Treatment=="unharvested"] <- cwd_dc_graph$biomass_Mg_ha[cwd_dc_graph$Treatment=="unharvested"]/22
cwd_dc_graph$mean_biomass[cwd_dc_graph$Treatment=="removal"] <- cwd_dc_graph$biomass_Mg_ha[cwd_dc_graph$Treatment=="removal"]/19
cwd_dc_graph$mean_biomass[cwd_dc_graph$Treatment=="regeneration"] <- cwd_dc_graph$biomass_Mg_ha[cwd_dc_graph$Treatment=="regeneration"]/4
#OK, looks right now!!!

#library(ggthemes)
cwd_p1 <- ggplot(data=cwd_dc_graph, #well...THAT didn't work lol
                 aes(x=as.factor(Treatment), y=mean_biomass, fill=as.factor(decay_class))) +
          geom_bar(position="stack", stat="identity") + #creates a stacked chart 
          theme_few() +
           scale_fill_grey(name="Decay Class")
          #still need to add labels too...
          #AND maybe facet_wrap by forest type?? IDK yet
          #and add error bars! 
          #and maybe texture...
cwd_p1  
#but for now, this is a good BASIS of plotting to build on!!



#############################################

# standing dead wood (snags) (SWD) analysis! -------------------------------

#first step = filter overstory data to just snags & compute BA!
snag_data <- overstory_data[overstory_data$status=="snag",]
snag_data$BA_sqm <- (snag_data$DBH_cm/200)*(snag_data$DBH_cm/200)*pi
snag_data$BA_sqm_ha <- snag_data$BA_sqm/0.04 #dividing by the area of 1 plot, in ha

#second step = compute BA & volume
#LOOK FOR volume reduction factor for snags??? (or not needed b/c diam tape?)
#OR a different volume equation for SDT (standing dead trees) that accounts for vol loss
#especially at the top (e.g. not a perfect cylinder?)
#and for biomass, see Harmon et al. 2011 for values??

### alright, back to this on Friday, Feb. 4th!
#Tony emailed me back to say he prefers using snag volume calculation from 
#Tyrell and Crow 1994 to account for shape of dif decay classes!
#  According to their scheme vs. my DC definitions:
#  decay class 1(crown present) = paraboloid, VRF = 0.75
# Decay class 2&3 = halfway between paraboloid/cylinder, VRF=0.5
# Decay class 4&5 = cylinder, VRF=1
# Vol formula = BA * height * VRF

#attaching these VRFs as a column:
snag_data$snag_VRF <- rep(0)
snag_data$snag_VRF[snag_data$decay_class==1] <- 0.5 #paraboloid
snag_data$snag_VRF[snag_data$decay_class==2 | snag_data$decay_class==3] <- 0.75 #in between
snag_data$snag_VRF[snag_data$decay_class==4 | snag_data$decay_class==5] <- 1 #cylinder
sum(snag_data$snag_VRF==0) #8 NA values b/c no DC, so they won't be factored into vol.
sum(is.na(snag_data$DBH_cm)) #none w/o a diam...
sum(is.na(snag_data$height_m)) #1 w/o a height...

snag_data$vol_m3_ha <- snag_data$BA_sqm_ha * snag_data$height_m * snag_data$snag_VRF
#got volume calculated!

#now to calculate biomass (again, following what I did for CWD):

#first, need to figure out what sp I have/fill in any not in the density table for SDTs
unique(snag_data$species)


#and also need to import that density table for SDTs! (per species, decay class)
snag_dens_vals <- read.csv("Harmon2011_AppendixC_standing_dead_tree_density_values.csv")
snag_dens_metadata <- read.csv("Harmon_2011_AppendixC_metadata.csv")
View(snag_dens_vals)

#test which of these codes aren't in the dataset w/ my new negatory operator?
'%!in%' = Negate('%in%')
unique(snag_data$species[(snag_data$species) %!in% snag_dens_vals$Code]) 
#RESULT: UNK and PISP (ALSO NEED TO UPDATE ACSP TO ACER!!!)

#fixing this:
snag_data$Code <- snag_data$species
snag_data$Code[snag_data$Code=="ACSP"] <- "ACER"
snag_data$Code[snag_data$Code=="PISP"] <- "PICE"
#check again: 
unique(snag_data$Code[(snag_data$Code) %!in% snag_dens_vals$Code]) 
#so, now just need to add UNK values to the snag_dens_vals table using rbind (as above)
#first creating a vector of values for the unkown "species" (from Harmon et al. 2011 Table 4):
snagunkvals <- data.frame(NA, NA, "UNK", NA, 0.40, NA, NA, 0.38, NA, NA, 0.35, NA, NA, 0.25, NA, NA)
names(snagunkvals) <- names(snag_dens_vals) #setting up an extra row for unknown species snags
#now to combine them.....
#testing123 <- rbind(snag_dens_vals, snagunkvals)
snag_dens_vals <- rbind(snag_dens_vals, snagunkvals) #yay it worked!

#IMPORTANT NOTE: snag_dens_vals only goes through DC 4, 
#so I need to reassign all my DC5 snags to DC4!
snag_data$decay_class[snag_data$decay_class==5] <- 4
#summary(snag_data$decay_class)
#IN RETROSPECT, could have just dealt with this in the for loop...
#but I'm OK with how I handled it here!!

#NEXT STEP = create a for loop to associate correct density value w/ each piece
#new column to hold density val:
snag_data$density_g_cm3 <- rep(0)
colassign2 <- rep(0) #and intermediate var 

for(i in 1:nrow(snag_data)){ #goin' by row in the snag data table,
  if(is.na(snag_data$decay_class[i])==FALSE){ 
    #filtering out any snag w/o a decay class, so as to not mess up subsequent if statements
    for(j in 1:nrow(snag_dens_vals)){ #now, iterating thru rows of the density table, AKA species
      if(snag_data$Code[i]==snag_dens_vals$Code[j]){ #matching up the species...
        #NOW, finding the correct column to attach by decay class...doing some ~fun~ math:
        colassign2[i] <- (snag_data$decay_class[i]-1)*3+5 #math to choose the right column density val based on DC
        #and the grand finale...attributing density val w/ correct sp & DC
        snag_data$density_g_cm3[i] <- snag_dens_vals[j,colassign2[i]]
      }
    }
  }
}
#looks like it worked, yay!!

#OK, now can simply calculate biomass! (in Mg/ha)
#REMEMBER, the units conveniently translate, so no unit coversion needed...
snag_data$biomass_Mg_ha <- snag_data$vol_m3_ha*snag_data$density_g_cm3

#aggregate by stand: 
snag_data <- merge(snag_data, plot_info_plus[,c("plot_ID", "Stand_name")], by="plot_ID")
snag_data_stand <- aggregate(formula=cbind(BA_sqm_ha, vol_m3_ha, biomass_Mg_ha) ~ Stand_name,
                             data=snag_data, FUN=sum)
View(snag_data_stand)

snag_data_stand <- merge(snag_data_stand, stand_info[,c("Stand_name", "Treatment", "forest_type")],
                         by="Stand_name")

#let's just confirm this looks right...
#sum(snag_data$biomass_Mg_ha[snag_data$Stand_name=="Camel's Hump SP stand 2-2-4"])
#looks good!

#next step = calculate mean & SD values!
#maybe just easier to go ahead and put in a table for this one (and all of them?!):
stand_dead_att <- data.frame("Group" = c(unique(snag_data_stand$Treatment), unique(snag_data_stand$forest_type)),
                             "CWD_vol_mean" = rep(0),
                             "CWD_vol_SE" = rep(0),
                             "CWD_biomass_mean" = rep(0),
                             "CWD_biomass_SE" = rep(0),
                             "snag_BA_mean" = rep(0),
                             "snag_BA_SE" = rep(0),
                             "snag_vol_mean" = rep(0),
                             "snag_vol_SE" = rep(0),
                             "snag_biomass_mean" = rep(0),
                             "snag_biomass_SE" = rep(0)
)

#might make more sense to transpose this anyway...?
#BUT will keep it as-is for now!

#FIRST, FILL IN SNAG INFO:
#actually...gonna take a break here, I need to eat lunch!!
#but WHEN I GET BACK, will finish computing these stats!!! (w/ for loop to make it easier...)
#and then save as a .csv...
#and then also run some ANOVAs!
#OK, I think my brain is tired from all the shoveling lol,
#so this is definitely NOT the most efficient way to do this,
#but whatever I'll just do it like this anyway.

#adding a lil thingy for tx groups:
tx_group <- c("unharvested", "regeneration", "removal")
for(i in 1:3){ #i represents the STATISTIC (BA, vol, and biomass)
  for(j in 1:3){ #j represents the TX GROUP
  stand_dead_att[stand_dead_att$Group==tx_group[j], #selects row
                 (i-1)*2+6] <- #selects col that aligns w/ the statistic in output df
    mean(snag_data_stand[snag_data_stand$Treatment==tx_group[j], #subsetting by stand tx
                         i+1]) #and cols 2-4 in this input df 
  #another command to calculate STANDARD ERROR
  stand_dead_att[stand_dead_att$Group==tx_group[j], #selects row
                 (i)*2+5] <- #selects col that aligns w/ the statistic in output df
    std.error(snag_data_stand[snag_data_stand$Treatment==tx_group[j], #subsetting by stand tx
                         i+1]) #and cols 2-4 in this input df 
  }
}
#OK, it worked...let's test it...
mean(snag_data_stand$vol_m3_ha[snag_data_stand$Treatment=="removal"])
std.error(snag_data_stand$biomass_Mg_ha[snag_data_stand$Treatment=="regeneration"])


#OK, now to do the same thing w/ FOREST TYPES:
ft_group <- c("NH", "RNH")
for(i in 1:3){ #i represents the STATISTIC (BA, vol, and biomass)
  for(j in 1:2){ #j represents the FOREST TYPE GROUP
    stand_dead_att[stand_dead_att$Group==ft_group[j], #selects row
                   (i-1)*2+6] <- #selects col that aligns w/ the statistic in output df
      mean(snag_data_stand[snag_data_stand$forest_type==ft_group[j], #subsetting by stand tx
                           i+1]) #and cols 2-4 in this input df 
    #another command to calculate STANDARD ERROR
    stand_dead_att[stand_dead_att$Group==ft_group[j], #selects row
                   (i)*2+5] <- #selects col that aligns w/ the statistic in output df
      std.error(snag_data_stand[snag_data_stand$forest_type==ft_group[j], #subsetting by stand tx
                                i+1]) #and cols 2-4 in this input df 
  }
}

#OK, now let's fill in the rest of the table w/ CWD attributes!
#(basically need to rerun all code above to do this)
#also need to reorder columns for the loop to work easily:
cwd_data_stand2 <- cwd_data_stand[,c(1,2,5,3,4)]
for(i in 1:2){ #i represents the STATISTIC (vol and biomass)
  for(j in 1:3){ #j represents the TX GROUP
    stand_dead_att[stand_dead_att$Group==tx_group[j], #selects row
                   i*2] <- #selects col that aligns w/ the statistic in output df
      mean(cwd_data_stand2[cwd_data_stand2$Treatment==tx_group[j], #subsetting by stand tx
                           i+1]) #and cols 2-3 in this input df 
    #another command to calculate STANDARD ERROR
    stand_dead_att[stand_dead_att$Group==tx_group[j], #selects row
                   i*2+1] <- #selects col that aligns w/ the statistic in output df
      std.error(cwd_data_stand2[cwd_data_stand2$Treatment==tx_group[j], #subsetting by stand tx
                                i+1]) #and cols 2-3 in this input df 
  }
}
#cross-referenced against the table I already made, and it looks good!!!

#now, ditto for forest types / CWD attributes:
for(i in 1:2){ #i represents the STATISTIC (vol and biomass)
  for(j in 1:2){ #j represents the FOREST TYPE GROUP
    stand_dead_att[stand_dead_att$Group==ft_group[j], #selects row
                   i*2] <- #selects col that aligns w/ the statistic in output df
      mean(cwd_data_stand2[cwd_data_stand2$forest_type==ft_group[j], #subsetting by stand tx
                           i+1]) #and cols 2-3 in this input df 
    #another command to calculate STANDARD ERROR
    stand_dead_att[stand_dead_att$Group==ft_group[j], #selects row
                   i*2+1] <- #selects col that aligns w/ the statistic in output df
      std.error(cwd_data_stand2[cwd_data_stand2$forest_type==ft_group[j], #subsetting by stand tx
                                i+1]) #and cols 2-3 in this input df 
  }
}

#ALRIGHT, IDK if that was faster lol but it is all together now!
#save to file:
write.csv(x=stand_dead_att,
          file="stand_deadwood_attributes_4Feb2022.csv")

# SNAG BA, volume, & biomass ANOVAS -------------------------------
#NOW TO DO SOME ANOVAS & SAVE THE RESULTS! (will write & run snags FIRST, then rerun for CWD)
## basically copied this code from CWD testing above:

snag_BA_anova1 <- aov(BA_sqm_ha ~ Treatment*forest_type, data=snag_data_stand)
summary(snag_BA_anova1) #treatment is not signif, but forest type is...interesting?

tukey5 <- TukeyHSD(snag_BA_anova1)
tukey5 #soo, basically just between the 2 forest types...not that surprising?

snag_vol_anova1 <- aov(vol_m3_ha ~ Treatment*forest_type, data=snag_data_stand)
summary(snag_vol_anova1) #Nothing is signif, but forest type is close!

#just for fun- post hoc analysis w/ Tukey's test:
tukey6 <- TukeyHSD(snag_vol_anova1)
tukey6 

snag_biomass_anova1 <- aov(biomass_Mg_ha ~ Treatment*forest_type, data=snag_data_stand)
summary(snag_biomass_anova1) #Nothing is signif, but forest type is close!

tukey7 <- TukeyHSD(snag_biomass_anova1)
tukey7

tukey8 <- TukeyHSD(snag_biomass_anova1, which="forest_type")
tukey8 #again, pretty much the same pattern....

#checking these results for normality!!
plot(snag_BA_anova1)
shapiro.test(snag_BA_anova1$residuals) #oookay, so BA residuals are NOT normal...
#actually seems exponential?


plot(snag_vol_anova1) #this Q-Q plot also looks exponential...
shapiro.test(snag_vol_anova1$residuals) #again- resids NOT normal

#OK, now [testing normality] for biomass!
plot(snag_biomass_anova1) #same situation!
shapiro.test(snag_biomass_anova1$residuals) #once again, not normal!

#FIGURE OUT WHAT TO DO ABOUT THIS! (transform data?)
#BUT FIRST, TRANSFER ALL THESE RESULTS TO MY RESULTS DOC(s)

#also, saving these files just in case I need to pull them up again later!
write.csv(x=cwd_data_stand, file="cwd_data_stand_4Feb2022.csv")
write.csv(x=snag_data_stand, file="snag_data_stand_4Feb2022.csv")

#referencing this webpage: https://rpubs.com/cwoods/anova
#and I ALSO need to check if my variance is equal among groups (post-anova)??

#this page says: "To test whether the error terms for each group have similar 
#variance (homogeneity of variance, assmuption 2 above), we will use a plot of 
#the fitted.values vs. the predicted values."
#e.g.: 
#anova2 <- cwd_vol_anova1
#plot(anova2$residuals~anova2$fitted.values)
#lines(lowess(anova2$fitted.values,anova2$residuals), col="blue")
# "For this plot, you are looking for no patterns. If you see a cone where the 
#vertical variation on one side of the figure is smaller than the vertical 
#variation on the other side, the variation is not homogeneous and you need to
#transform your data."

#let's also try:
#anova3<-cwd_biomass_anova1
#plot(anova3$residuals~anova3$fitted.values)
#lines(lowess(anova3$fitted.values,anova3$residuals), col="blue")
#hmm...I will say, these do look uneven...but let's worry about snag results first??

#TRYING A TRANSFORMATION W/ SNAG DATA:
snag_BA_anova2 <- aov(sqrt(BA_sqm_ha) ~ Treatment*forest_type, data=snag_data_stand)
plot(snag_BA_anova1) #for comparison:
plot(snag_BA_anova2) #this is with sqrt transform. better, but still not great?
shapiro.test(snag_BA_anova2$residuals) #again...better, but not great!
summary(snag_BA_anova2) #now, forest type is even MORE signif than before!

#let's try a log transform?
snag_BA_anova3 <- aov(log(BA_sqm_ha) ~ Treatment*forest_type, data=snag_data_stand)
plot(snag_BA_anova3) #this is with log transform. TBH looks a lot better
shapiro.test(snag_BA_anova3$residuals) #we gucci!!
summary(snag_BA_anova3) #now, forest type is even MORE signif than before!
#WHY??

#OK, log-transforming the other snag variables:
snag_vol_anova2 <- aov(log(vol_m3_ha) ~ Treatment*forest_type, data=snag_data_stand)
plot(snag_vol_anova2) #this is with log transform. TBH looks a lot better
shapiro.test(snag_vol_anova2$residuals) #we gucci!!
summary(snag_vol_anova2) #again... forest type is even MORE signif than before!

snag_biomass_anova2 <- aov(log(biomass_Mg_ha) ~ Treatment*forest_type, data=snag_data_stand)
plot(snag_biomass_anova2) #this is with log transform. TBH looks a lot better
shapiro.test(snag_biomass_anova2$residuals) #we gucci!!
summary(snag_biomass_anova2) #again... forest type is still only signif var!

#OK, I think I'm done with this, but regardless data tables are saved
#in case I need to revisit & tinker with my ANOVAs!

##############################################

#what's next: returning to sapling/understory analysis?
#or calculating other stand-level attributes?

#back at it on Monday, Feb. 7th: going to start with other (easy) stand-level
#attributes!

#First up, TPA and QMD
trees_per_ha <- rep(0) 
#summarizing by # of live trees per plot
trees_per_ha <- overstory_data[overstory_data$status=="live",] %>%
  group_by(plot_ID) %>%
  summarize(num_trees = length(plot_ID))

#dividing the "count" per plot by the size of a plot (in ha)
trees_per_ha$per_ha <- trees_per_ha$num_trees/0.04

#next to summarize by stand:
trees_per_ha <- merge(x=trees_per_ha, 
                      y=plot_info_plus[,c("plot_ID", "Stand_name", "num_plots", "Treatment", "forest_type")],
                      by="plot_ID", all=TRUE)
trees_per_ha <- aggregate(per_ha ~ Stand_name, 
                          data=trees_per_ha, FUN=sum)
#(re-merge because the aggregate erased other columns LOL)
trees_per_ha <- merge(x=trees_per_ha, 
                      y=stand_info[,c("Stand_name", "num_plots", "Treatment", "forest_type")],
                      by="Stand_name", all=TRUE)
#correcting for the number of plots per stand
trees_per_ha$per_ha_stand <- trees_per_ha$per_ha/trees_per_ha$num_plots

#next, need to pull in the averages & add to that table I made last time:
deadwood_table <- read.csv("stand_deadwood_attributes_4Feb2022.csv")
View(deadwood_table)
stand_attributes <- deadwood_table
stand_attributes$TPH_mean <- rep(0)
stand_attributes$TPH_SE <- rep(0)

#honestly....might not do a for loop for this b/c I'm tired lol
#maybe I'll just do it AFTER calculating QMD!
#and then I'll also need to ANOVA it...

TPH_anova1 <- aov(per_ha_stand ~ Treatment, data=trees_per_ha)
#plot(TPH_anova1)
#shapiro.test(TPH_anova1$residuals) #looks good!
summary(TPH_anova1)

#need to look @ both together...
TPH_anova2 <- aov(per_ha_stand ~ Treatment*forest_type, data=trees_per_ha)
plot(TPH_anova2)
shapiro.test(TPH_anova2$residuals) #looks good!
summary(TPH_anova2) 
#both treatment AND forest type signif, but not their interaction!
#Interesting...

#now to do a post hoc test:
TPHtukey1 <- TukeyHSD(TPH_anova2)
TPHtukey1 #all pairwise individual interactions are signif
#EXCEPT for removal vs. regeneration

#OK, now to calculate QMD: 
#source for formula = 
#https://www-fs-fed-us.ezproxy.uvm.edu/pnw/olympia/silv/publications/opt/436_CurtisMarshall2000.pdf

#need to summarize by # live trees again, then append that back to overstory_data
#and divide each squared diam by it 
#use number PER STAND i guess???

# QUESTIONS: should I be calculating QMD (and TPH) as a statistic PER STAND
# and then averaging the stand-level values for analysis,
# or should I be calculating this as a statistic PER GROUP 
# (e.g. QMD using the entire group of trees in "unharvested" plots)?
#am currently doing the former and I think that's right, but want to check on this!

live_trees <- overstory_data[overstory_data$status=="live",]
live_trees <- merge(x=live_trees, y=plot_info_plus[, c("plot_ID", "Stand_name")],
                    all.x=TRUE, by="plot_ID") #getting stand_name associated 

num_trees <- live_trees #using num_trees to calculate n for QMD formula
num_trees <- num_trees %>%
            group_by(Stand_name) %>%
            summarize(n_stand = length(Stand_name))

live_trees$diam_cm_sq <- live_trees$DBH_cm^2
live_trees <- aggregate(diam_cm_sq ~ Stand_name, data=live_trees, FUN=sum)
#summarizing squared diameters per stand!

live_trees <- merge(live_trees, num_trees) #adding on the n value per stand
#calculating the remainder of QMD formula, putting all the pieces together
live_trees$QMD_cm <- sqrt(live_trees$diam_cm_sq/live_trees$n_stand)

#OK, now re-aggregating w/ variables:
live_trees <- merge(live_trees, stand_info[,c("Stand_name", "Treatment", "forest_type")])
#checkin' it out...
boxplot(live_trees$QMD_cm ~ live_trees$Treatment)
boxplot(live_trees$QMD_cm ~ live_trees$forest_type)

#ANOVAs for QMD:
QMD_anova1 <- aov(QMD_cm ~ Treatment*forest_type, data=live_trees)
plot(QMD_anova1)
shapiro.test(QMD_anova1$residuals) #seems OK......
summary(QMD_anova1) #only forest type is signif??
#post hoc test:
QMD_tukey1 <- TukeyHSD(QMD_anova1)
QMD_tukey1 #OK same result of course b/c only 2 forest types LOL

#OK, now I need to figure out the loop or SOMETHING for filling out the table!
stand_attributes$QMD_mean <- rep(0)
stand_attributes$QMD_SE <- rep(0)

#LOOPS COPIED AND MODIFIED FROM ABOVE!!!

#first need to get things in the same dataframe:
live_trees <- merge(live_trees, trees_per_ha[,c("Stand_name", "per_ha_stand")])
#also need to reorder columns for the loop to work easily:
live_trees2 <- live_trees[,c(1,7,4,5,6)]

for(i in 1:2){ #i represents the STATISTIC (TPH and QMD)
  for(j in 1:3){ #j represents the TX GROUP
    stand_attributes[stand_attributes$Group==tx_group[j], #selects row
                   11+i*2] <- #selects col that aligns w/ the statistic in output df
      mean(live_trees2[live_trees2$Treatment==tx_group[j], #subsetting by stand tx
                           i+1]) #and cols 2-3 in this input df 
    #another command to calculate STANDARD ERROR
    stand_attributes[stand_attributes$Group==tx_group[j], #selects row
                   i*2+12] <- #selects col that aligns w/ the statistic in output df
      std.error(live_trees2[live_trees2$Treatment==tx_group[j], #subsetting by stand tx
                                i+1]) #and cols 2-3 in this input df 
  }
}
#doinga lil check...
#mean(live_trees$QMD_cm[live_trees$Treatment=="unharvested"])

#now, ditto for forest types / CWD attributes:
for(i in 1:2){ #i represents the STATISTIC (TPH and QMD)
  for(j in 1:2){ #j represents the FOREST TYPE GROUP
    stand_attributes[stand_attributes$Group==ft_group[j], #selects row
                   i*2+11] <- #selects col that aligns w/ the statistic in output df
      mean(live_trees2[live_trees2$forest_type==ft_group[j], #subsetting by stand tx
                           i+1]) #and cols 2-3 in this input df 
    #another command to calculate STANDARD ERROR
    stand_attributes[stand_attributes$Group==ft_group[j], #selects row
                   i*2+12] <- #selects col that aligns w/ the statistic in output df
      std.error(live_trees2[live_trees2$forest_type==ft_group[j], #subsetting by stand tx
                                i+1]) #and cols 2-3 in this input df 
  }
}
#again- checking...
#std.error(live_trees$per_ha_stand[live_trees$forest_type=="RNH"])

#looks good! Now to export this table (gonna transpose it first):
#but FIRST-first gotta remove that annoying extra column... (and col names, to add back in next line)
stand_attributes2 <- stand_attributes[,3:length(stand_attributes)]
stand_attributes2 <- t(stand_attributes2)
colnames(stand_attributes2) <- stand_attributes$Group
write.csv(x=stand_attributes2, file="stand_attributes_7Feb2022.csv",
          row.names=TRUE, col.names=TRUE)
#alright, looks good!!
#that's all for this bit I think!
#just in case:
write.csv(x=live_trees, file="live_stand_TPH_QMD_7Feb2022.csv",
          row.names=FALSE, col.names=TRUE)

#I guess I still need to calculate BA for the group...
#not that it's hard LOL I'll just think about it next time
#(along with ash % BA harvested? will need to look back for that one)



### I'm back on Feb. 15th to do the rest of those things!! ###

# Basal area calculations & analysis (stand-level) (total & ash BA) -------------------------------

#First, gonna look at overall BA (for LIVE trees)
live_trees2 <- overstory_data[overstory_data$status=="live",]
View(live_trees2)
#BA = pi*r^2, converting DBH in cm to radius in m
live_trees2$BA_sqm <- pi*(live_trees2$DBH_cm/200)^2 
live_trees2 <- merge(live_trees2, plot_info_plus[,c("plot_ID", "Stand_name")], all.x = TRUE)
live_trees2 <- aggregate(formula=BA_sqm ~ Stand_name, FUN=sum, data=live_trees2)
live_trees2 <- merge(live_trees2, stand_info[,c("Stand_name", "num_plots")])
#calculating PER HECTARE BA based on # plots in each stand
live_trees2$BA_sqm_ha <- live_trees2$BA_sqm/(0.04*live_trees2$num_plots) 
#oops, forgot to attach explanatory variables....
live_trees2 <- merge(live_trees2, stand_info[, c("Stand_name", "Treatment", "forest_type")])

anova_liveBAstand <- aov(formula=BA_sqm_ha ~ Treatment*forest_type, data=live_trees2)
#take a look at it first...
plot(anova_liveBAstand)
shapiro.test(anova_liveBAstand$residuals) #looks OK to me!
#now to look @ results:
summary(anova_liveBAstand) 
#treatment is very signif; forest type is almost
TukeyHSD(anova_liveBAstand) #removal vs regen not dif, but both dif from unharv.
#That all makes sense!! Now I want to look @ ASH-specific BA remaining/BA removed,
#and then the PROPORTION removed or proportion remaining...

#But just gonna do calculations quickly first! 

#don't have it in me to do another for loop so I'm just gonna copy + paste 
#these values (total BA mean / SE) for the table, I think:
#OR, maybe read in the table & just append a few rows to it??

stand_att_table <- read.csv("stand_attributes_7Feb2022.csv")

#GOING TO THE BATHROOM, BUT WHEN I GET BACK, RENAME X COLUMN
#AND THEN USE RBIND TO ADD TO THIS!

#ACTUALLY, gonna do the ash stuff first!
#just to get peace of mind/validate my division between removal and regen
#categories of harvested sites......

#you know what, just gonna focus on WHITE ASH for this b/c only one site had black ash.
#(Or should I just go ahead and include black ash??)
live_ash <- overstory_data[(overstory_data$status=="live" & 
                              (overstory_data$species=="FRAM" | overstory_data$species=="FRNI")),]
#ok doing it both ways for now LOL
live_white_ash <- overstory_data[(overstory_data$status=="live" & overstory_data$species=="FRAM"),]

#let's just look at white ash for now: 
live_white_ash$BA_sqm <- pi*(live_white_ash$DBH_cm/200)^2 
live_white_ash <- merge(live_white_ash, plot_info_plus[,c("plot_ID", "Stand_name")], all.x = TRUE)
live_white_ash <- aggregate(formula=BA_sqm ~ Stand_name, FUN=sum, data=live_white_ash)
live_white_ash <- merge(live_white_ash, stand_info[,c("Stand_name", "num_plots")])
#calculating PER HECTARE BA based on # plots in each stand
live_white_ash$BA_sqm_ha <- live_white_ash$BA_sqm/(0.04*live_white_ash$num_plots) 
#oops, forgot to attach explanatory variables....
live_white_ash <- merge(live_white_ash, stand_info[, c("Stand_name", "Treatment", "forest_type")])

anova_livewhiteash <- aov(formula=BA_sqm_ha ~ Treatment*forest_type, data=live_white_ash)
#take a look at it first...
plot(anova_livewhiteash)
shapiro.test(anova_livewhiteash$residuals) #looks OK to me!
#now for the (first) moment of truth...
summary(anova_livewhiteash) #Treatment is only signif var; makes sense...
TukeyHSD(anova_livewhiteash) #but removal vs regen is not signif dif...
#Hmm, OK, this maybe (hopefully!!) will make more sense in terms of separating them
#by BA or proportion of ash REMOVED, not what's LEFT. (!?)

#need to go back into the archives for this.....maybe better to do it after lab meeting??
#NEXT STEP WHEN I COME BACK = GO BACK TO THE "ARCHIVES" FOR CODE THAT SPINS UP ASH DBH FROM STUMP HEIGHT,
#AND RE-CALCULATE ALL THAT SO I CAN USE IT IN ANALYSES NEXT

#ALSO DON'T FORGET I NEED TO RECORD RESULTS IN RESULTS DOC!!!


### BELOW CODE COPIED / MAYBE MODIFIED FROM SCRIPT 12-14-20_FEMC_conference_prep.R ###

#merging the overstory df w/ the plot_info df on the basis of the plot_ID column
overstory_plus <- merge(x=overstory_data,y=plot_info,by="plot_ID")

#converting stump height & diam to DBH- using James Westfall paper from USFS
#IMPORTANT: these values are specific to white ash; dif species have dif constants!!
#IMPORTANT QUESTION: can I use measurements in cm for this?! I don't see why not...
#altho ACTUALLY we'd have to convert stump height-dbh relation to all meters or all feet.
#variables for this formula: dbhi = di * (4.5/hi)^B0 + B1(4.5-h) + Ei
#dbhi = estimated DBH for tree 'i'
#di = stump diameter (in.) for tree i;
#hi = stump height (ft.) for tree i;
#B0 & B1 = estimated fixed-effects parameters; 
#(for white ash, B0 = -0.1074 & B1 = 0.0685)
#Ei = random error for tree i
#IN THE BELOW MODEL, I'm gonna convert things to use METRIC, not imperial, units!

overstory_plus$ASH_stump_DBH <- rep(NA)
overstory_plus$BA_sqm <- rep(NA)

#calculating BA for live trees & stumps using standard conversion from DBH to BA
#overstory_plus$BA_sqft <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*0.005454

#BUT SINCE WE WANNA DO IT USING METRIC UNITS:
#RIGHT NOW THIS IS ONLY DOING IT CORRECTLY FOR LIVE TREES, NOT USING THE CORRECT DATA FOR STUMPS!!
overstory_plus$BA_sqm <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*pi/40000

#only calculating the stump DBH w/ these constants for ash
for(i in 1:nrow(overstory_plus)){
  if(overstory_plus$species[i]=="FRAM"){
    overstory_plus$ASH_stump_DBH[i] <- 
      (overstory_plus$DBH_cm[i]*((1.3716/overstory_plus$height_m[i])^-0.1074) +
         0.0685*(1.3716-overstory_plus$height_m[i]) + 0)
    # and then converting that to BA, for ASH stumps only
    overstory_plus$BA_sqm[i] <- 
      overstory_plus$ASH_stump_DBH[i]*overstory_plus$ASH_stump_DBH[i]*pi/40000 
  }
}
#Honestly it's nice to see how far I've come b/c I could do what this loop does
#MUCH more efficiently now!!!

View (overstory_plus)
#WAIT A MINUTE......don't we want ONLY stumps of DC 1 or 2 in there???
#That's what I'm going with for now...
#Let's simplify + cut down this dataframe:
cut_white_ash <- overstory_plus[(overstory_plus$species=="FRAM" & 
                                   overstory_plus$status=="stump" &
                                   overstory_plus$decay_class<3),]
View(cut_white_ash)

cut_white_ash <- aggregate(formula= BA_sqm ~ Stand_name, data=cut_white_ash, FUN=sum)
names(cut_white_ash)[2] <- "stump_BA_sqm"

all_white_ash <- merge(x=live_white_ash, y=cut_white_ash, by="Stand_name", all=TRUE)
View(all_white_ash)
#calculating BA in sqm per HECTARE for CUT ash
all_white_ash$cut_BA_sqm_ha <- all_white_ash$stump_BA_sqm/(0.04*all_white_ash$num_plots)
names(all_white_ash)[4] <- "live_BA_sqm_ha" #renaming this column for clarity!!
#need to reassign zeroes!!
all_white_ash[is.na(all_white_ash)] <- 0

#looks like there is ZERO so-called "cut ash" in ANY of the unharvested stands...
#which does make sense, but...still!!

#now adding a sum total ash BA column for simplicity's sake...
all_white_ash$total_ash_BA <- all_white_ash$live_BA_sqm_ha + all_white_ash$cut_BA_sqm_ha

#OK, now for some ANOVAs!!!
#moment of truth LOL:
#doing an ANOVA first, but could also do a t-test to compare JUST regen vs removal??
anova_cutAshBA <- aov(cut_BA_sqm_ha ~ Treatment*forest_type, data=all_white_ash)
plot(anova_cutAshBA)
shapiro.test(anova_cutAshBA$residuals) 
#ok so residuals are NOT normally distributed.
#honestly, this makes a lot of sense b/c it's basically just zeroes across the board
#for the whole "unharvested" treatment type.
#SO, let's look at it first out of curiosity, but it's not really valid...
#we are probably better off just doing a T-test with regen vs removal...
summary(anova_cutAshBA)
TukeyHSD(anova_cutAshBA) #seeing here that removal vs regen IS signif 
#(along with unharvested vs removal as expected), but NOT regen vs unharvested...
#interesting, but ultimately NOT useful b/c our assumptions for ANOVA are not met!

#OK let's do a t-test of this instead:
#looking ONLY @ harvested stands (by excluding unharvested to just compare regen vs removal)
treatment_cutash_compare <- t.test(cut_BA_sqm_ha ~ Treatment, 
                                   data=all_white_ash[all_white_ash$Treatment!="unharvested",])
treatment_cutash_compare
#YES, there's a signif dif in cut ash BA per ha between regen & removal groups!
#(thank God LOL)

#now let's also look @ the proportion: 
all_white_ash$proportion_cut <- all_white_ash$cut_BA_sqm_ha / all_white_ash$total_ash_BA
#once again, the mean for basically all the unharvested plots is zero, 
#so let's just compare regen vs removal groups!
treatment_proportion_compare <- t.test(proportion_cut ~ Treatment, 
                                   data=all_white_ash[all_white_ash$Treatment!="unharvested",])
treatment_proportion_compare #once again, YES we are significant!!
#about 35% vs about 65% difference. (Makes sense to me!)

#OK feeling better now that this is all settled!!

#NOW, just need to calculate means & SEs for these groupings & add them to the table above.
library(plotrix)

names(stand_att_table)[1] <- "Statistic" #renaming weirdly named "X" column

#need to add rows w/ 6 columns to work...
BA_attributes <- data.frame(NA, 0, 0, 0, 0, 0)
names(BA_attributes) <- names(stand_att_table)
BA_attributes$Statistic <- as.character(BA_attributes$Statistic) 
#making first column a character type

#dataframes for this = live_trees2 and all_white_ash

#total BA mean + SE:
BA_attributes[1, ]<- c("all_live_BA_mean", 
                      mean(live_trees2$BA_sqm_ha[live_trees2$Treatment=="unharvested"]),
                      mean(live_trees2$BA_sqm_ha[live_trees2$Treatment=="regeneration"]),
                      mean(live_trees2$BA_sqm_ha[live_trees2$Treatment=="removal"]),
                      mean(live_trees2$BA_sqm_ha[live_trees2$forest_type=="NH"]),
                      mean(live_trees2$BA_sqm_ha[live_trees2$forest_type=="RNH"])
)

BA_attributes[2, ]<- c("all_live_BA_SE", 
                       std.error(live_trees2$BA_sqm_ha[live_trees2$Treatment=="unharvested"]),
                       std.error(live_trees2$BA_sqm_ha[live_trees2$Treatment=="regeneration"]),
                       std.error(live_trees2$BA_sqm_ha[live_trees2$Treatment=="removal"]),
                       std.error(live_trees2$BA_sqm_ha[live_trees2$forest_type=="NH"]),
                       std.error(live_trees2$BA_sqm_ha[live_trees2$forest_type=="RNH"])
)


#now doing the same for LIVE (white) ASH BA:
BA_attributes[3,]<- c("ash_live_BA_mean", 
                      mean(all_white_ash$live_BA_sqm_ha[all_white_ash$Treatment=="unharvested"]),
                      mean(all_white_ash$live_BA_sqm_ha[all_white_ash$Treatment=="regeneration"]),
                      mean(all_white_ash$live_BA_sqm_ha[all_white_ash$Treatment=="removal"]),
                      mean(all_white_ash$live_BA_sqm_ha[all_white_ash$forest_type=="NH"]),
                      mean(all_white_ash$live_BA_sqm_ha[all_white_ash$forest_type=="RNH"])
)

BA_attributes[4, ]<- c("ash_live_BA_SE", 
                       std.error(all_white_ash$live_BA_sqm_ha[all_white_ash$Treatment=="unharvested"]),
                       std.error(all_white_ash$live_BA_sqm_ha[all_white_ash$Treatment=="regeneration"]),
                       std.error(all_white_ash$live_BA_sqm_ha[all_white_ash$Treatment=="removal"]),
                       std.error(all_white_ash$live_BA_sqm_ha[all_white_ash$forest_type=="NH"]),
                       std.error(all_white_ash$live_BA_sqm_ha[all_white_ash$forest_type=="RNH"])
)


#next for CUT (white) ash BA:
BA_attributes[5,]<- c("ash_cut_BA_mean", 
                      mean(all_white_ash$cut_BA_sqm_ha[all_white_ash$Treatment=="unharvested"]),
                      mean(all_white_ash$cut_BA_sqm_ha[all_white_ash$Treatment=="regeneration"]),
                      mean(all_white_ash$cut_BA_sqm_ha[all_white_ash$Treatment=="removal"]),
                      mean(all_white_ash$cut_BA_sqm_ha[all_white_ash$forest_type=="NH"]),
                      mean(all_white_ash$cut_BA_sqm_ha[all_white_ash$forest_type=="RNH"])
)

BA_attributes[6, ]<- c("ash_cut_BA_SE", 
                       std.error(all_white_ash$cut_BA_sqm_ha[all_white_ash$Treatment=="unharvested"]),
                       std.error(all_white_ash$cut_BA_sqm_ha[all_white_ash$Treatment=="regeneration"]),
                       std.error(all_white_ash$cut_BA_sqm_ha[all_white_ash$Treatment=="removal"]),
                       std.error(all_white_ash$cut_BA_sqm_ha[all_white_ash$forest_type=="NH"]),
                       std.error(all_white_ash$cut_BA_sqm_ha[all_white_ash$forest_type=="RNH"])
)

#and finally for PROPORTION of white ash BA cut:
BA_attributes[7,]<- c("ash_cut_prop_mean", 
                      mean(all_white_ash$proportion_cut[all_white_ash$Treatment=="unharvested"]),
                      mean(all_white_ash$proportion_cut[all_white_ash$Treatment=="regeneration"]),
                      mean(all_white_ash$proportion_cut[all_white_ash$Treatment=="removal"]),
                      mean(all_white_ash$proportion_cut[all_white_ash$forest_type=="NH"]),
                      mean(all_white_ash$proportion_cut[all_white_ash$forest_type=="RNH"])
)

BA_attributes[8, ]<- c("ash_cut_prop_SE", 
                       std.error(all_white_ash$proportion_cut[all_white_ash$Treatment=="unharvested"]),
                       std.error(all_white_ash$proportion_cut[all_white_ash$Treatment=="regeneration"]),
                       std.error(all_white_ash$proportion_cut[all_white_ash$Treatment=="removal"]),
                       std.error(all_white_ash$proportion_cut[all_white_ash$forest_type=="NH"]),
                       std.error(all_white_ash$proportion_cut[all_white_ash$forest_type=="RNH"])
)

#now need to rbind w/ stand_att_table:

stand_att_table <- rbind(stand_att_table, BA_attributes)
#wheee it's all there!!
#now to re-save this table:

write.csv(x=stand_att_table, file="stand_attributes_15Feb2022.csv", row.names=FALSE)


#OK, now MAYBE re-do those 2 t-tests as ANOVAs instead??

#just gonna save what I have so far in my results doc before I finish this up!!!

#ALRIGHTY, ANOVAs revisited: 

#looking ONLY @ regen vs removal (crossed w/ forest type):
anova_cutAshBA_harveststands <- aov(cut_BA_sqm_ha ~ Treatment*forest_type, 
                                    data=all_white_ash[all_white_ash$Treatment!="unharvested",])
plot(anova_cutAshBA_harveststands) 
#hmm this looks a little weird...
shapiro.test(anova_cutAshBA_harveststands$residuals) 
#but Shapiro test is OK
summary(anova_cutAshBA_harveststands) 
#OK, when we're doing ANOVA, dif is only slightly signif...
#I wonder why that is??

TukeyHSD(anova_cutAshBA_harveststands)
#didn't really need to do a Tukey test for this LOL


#now let's try this with PROPORTION of ash cut...
anova_proportionashcut_harveststands <- aov(proportion_cut ~ Treatment*forest_type, 
                                            data=all_white_ash[all_white_ash$Treatment!="unharvested",])
plot(anova_proportionashcut_harveststands)
shapiro.test(anova_proportionashcut_harveststands$residuals) 
#OK this is NOT normal...
#but for curiosity's sake, let's take a look anyway:
summary(anova_proportionashcut_harveststands)
#so here, treatment is still signif, but again no interaction w/ forest type etc.


#done with overstory stuff fully, I think!!!
#unless I end up wanting to do some figures for this part...

#ANYWAY, next time I work on this (later this week!) 
#I will be moving (back) to UNDERSTORY LIENAR MODELING!!!

# understory composition, linear model analysis revisited... -------------------------------


###BELOW CODE COPIED/MODIFIED FROM PREVIOUS SCRIPT 10-18-21_more_organized_analysis.R ###


# pivot to seedlings data! -------------------------------

#first step is to aggregate tallies by plot, total # & for species of interest
seedlings_per_plot <- aggregate(tally ~ plot_ID, data = seedling_data, FUN=sum)
# aggregating to get rid of the azimuth subdivisions!
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

#voi_plot includes all variables of interest that are relevant to ALL stands at the PLOT level
#this is essentially the same as voi_plot, with the addition of the plot_ID identifier.
voi_plot <- plot_info_plus[, c("plot_ID", "Stand_name", "State", "Ownership_cat", 
                               "Treatment", "EAB_present", "forest_type")]


#now to merge w/ plot data:
seedlings_plus <- merge(x=voi_plot, y=seedlings_per_plot, all.x=TRUE, by="plot_ID")

#now to replace NAs with 0s:
seedlings_plus[is.na(seedlings_plus)] <- 0

#and finally, create another DF where they're in seedlings PER SQUARE METER:
seedlings_plus_sqm <- seedlings_plus
seedlings_plus_sqm[,8:13] <- seedlings_plus_sqm[,8:13]/3

#now do a linear model:

library(nlme)
library(lme4)

model8 <- lme(fixed=tally ~ Treatment + forest_type, 
              data = seedlings_plus_sqm, 
              random = ~ 1 | Stand_name)
anova(model8)
summary(model8)                                                                   
#no signif relationship w/ ALL species (maybe expected)

#Q: HOW to interpret results??
#and also HOW to ensure assumptions are being met?
#(re: normality ETC.)

plot(model8)
#OK that definitely does NOT look symmetrical etc!! LOL

#Q: need to find something that fits to a negative binomial
#distribution???

#from lme4 package documentation, can use function
#glmer.nb to fit negative binomial distributions for GLMM??
?glmer.nb
#let's try it I guess?? (use syntax for glmer())

#nbtest1_allseedlings <- glmer.nb(formula=tally ~ Treatment + forest_type + 
                        #for this funct, formula includes both fixed (above) & random (below) effects
#                                   (1 | Stand_name), data= seedlings_plus_sqm
#                                 )
#OK apparently non-integer values are a no-go!!
#SO let's just use the per-plot tally (NOT by area) instead!! (w/ just "seedlings_plus" dataset)
nbtest2_allseedlings <- glmer.nb(formula=tally ~ Treatment + forest_type + 
                                   #for this funct, formula includes both fixed (above) & random (below) effects
                                   (1 | Stand_name), data= seedlings_plus
)
anova(nbtest2_allseedlings)
#once again......how to interpret this??
plot(nbtest2_allseedlings) #hmm at least plot looks better...
summary(nbtest2_allseedlings)

#maybe try doing just regular glmer (w/o neg binomial dist) to compare??
#but apparently, for default family (Gaussian), should just use lmer instead:
#lmertest1_allseedlings <- lmer(formula=tally ~ Treatment + forest_type + 
                                      #for this funct, formula includes both fixed (above) & random (below) effects
#                                      (1 | Stand_name), data= seedlings_plus
#)
#anova(lmertest1_allseedlings)
#plot(lmertest1_allseedlings) #once again, this residuals plot looks BAD!!
#so seems like negative binomial dist is the way to go!! (@ least for seedlings!)
#summary(lmertest1_allseedlings)

#try with INTERACTION of Treatment x forest type??
nbtest3_allseedlings <- glmer.nb(formula=tally ~ Treatment*forest_type + 
                                   #for this funct, formula includes both fixed (above) & random (below) effects
                                   (1 | Stand_name), data= seedlings_plus
)
anova(nbtest3_allseedlings)
#once again......how to interpret this??
plot(nbtest3_allseedlings) #hmm at least plot looks better...
summary(nbtest3_allseedlings) #looks like nothing is signif here...

#now let's try with individual species???

#ok I think we need to pivot this again to use the loop effectively.....
seedlings_plus_long <- pivot_longer(data=seedlings_plus,
                                        cols=c("tally", "ACSA", "FRAM", "FAGR", "ACRU", "BEAL"),
                                        names_to = "species")

major_species <- c("ACSA", "FRAM", "FAGR", "BEAL", "ACRU")

#nbglmm_seedling_results <- data.frame("species"=rep(NA),
#                                      "results"=rep(NA)
#                                     )

for (i in 1:5){
  loop_df <- seedlings_plus_long[seedlings_plus_long$species==major_species[i],]
  #looking @ one species at a time...
  loop_model <- glmer.nb(formula= value ~ Treatment*forest_type + #fixed effects
                    (1 | Stand_name), data = loop_df) #random effect, & temp data frame
  #add species & corresponding results (summary(model)) to the dataframe:
  #nbglmm_seedling_results$species[i] <- major_species[i]
  #nbglmm_seedling_results$results[i] <- summary(loop_model)
  
  #ACTUALLY, let's just look at them for now...
  print(major_species[i])
  print(summary(loop_model))
  print("")
  print("")
}

#I think I can't store the entire summary() funct output in the df...
#need to fix that & then try re-running the loop?

#OR, maybe just wait 'til I talk to Tony re: which stats to report,
#and just look @outputs in the meantime?


#let's do 1 or 2 manually:
#first, w/ interaction:
nbtest1_ACSA <- glmer.nb(formula=ACSA ~ Treatment*forest_type + 
                                   #for this funct, formula includes both fixed (above) & random (below) effects
                                   (1 | Stand_name), data= seedlings_plus
)
#next, w/o interaction: 
nbtest2_ACSA <- glmer.nb(formula=ACSA ~ Treatment + forest_type + 
                           #for this funct, formula includes both fixed (above) & random (below) effects
                           (1 | Stand_name), data= seedlings_plus
)
#and finally, w/ just treatment: 
nbtest3_ACSA <- glmer.nb(formula=ACSA ~ Treatment + 
                           #for this funct, formula includes both fixed (above) & random (below) effects
                           (1 | Stand_name), data= seedlings_plus
)

 #& now to look at the outputs:
summary(nbtest1_ACSA)
summary(nbtest2_ACSA)
summary(nbtest3_ACSA)
#conclusion: Model 3 has the smallest BIC & AIC (by a small amount, but still!)
#so...should I just stick with looking only @ treatment (and site as the random effect)
#for seedling & sapling data going forward?!
#ANOTHER THING TO ASK TONY!!


### POTENTIALLY IMPORTANT UPDATE BELOW!!! ###
#Q for the model: how to change which factor they are "comparing" others against
# to unharvested (b/c seems like it's regen treatment right now)??
#looks like I should "relevel" the factor!!
#trying it out here:
seedlings_plus$Treatment <- factor(seedlings_plus$Treatment)
seedlings_plus$Treatment <- relevel(seedlings_plus$Treatment, ref="unharvested")

#and now let's RE-try one to compare:
nbtest4_ACSA <- glmer.nb(formula=ACSA ~ Treatment + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest4_ACSA)
#AHA, now we have a more meaningful baseline against which to compare :) 


#and just to be thorough let's re-try w/ interaction too to see if that changes anything:
nbtest5_ACSA <- glmer.nb(formula=ACSA ~ Treatment*forest_type + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest5_ACSA)
#still nothing important w/ forest type...so I feel confident discarding this?
#OR just include it to PROVE there is no interaction/important effect??

#ok I guess one issue w/ this approach is it's not directly comparing
#regen vs removal treatment...just comparing both against unharvested reference??


#getting back to this Friday 2/18. Need to flesh out some of these results
#So I can add something more informed to my abstract re: regen!!
#gonna finish with seedlings, do same thing w/ saplings, and then
#plot them to take a closer look.
#let's do the rest of the species lol!
nbtest1_FRAM <- glmer.nb(formula=FRAM ~ Treatment*forest_type + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest1_FRAM)

nbtest1_FAGR <- glmer.nb(formula=FAGR ~ Treatment*forest_type + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest1_FAGR)

#FAGR still failed to convergew/ this model type, 
#so let's try it w/o forest type...

nbtest2_FAGR <- glmer.nb(formula=FAGR ~ Treatment + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest2_FAGR) #OK this one didn't converge either!! Gonna skip it for now...


nbtest1_BEAL <- glmer.nb(formula=BEAL ~ Treatment*forest_type + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest1_BEAL) #OK, BEAL is also not converging....

#let's try again w/o forest type?
nbtest2_BEAL <- glmer.nb(formula=BEAL ~ Treatment + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest2_BEAL) #this one DID work, ok great!

#and finally ACRU
nbtest1_ACRU <- glmer.nb(formula=ACRU ~ Treatment*forest_type + 
                           (1 | Stand_name), data= seedlings_plus
)
summary(nbtest1_ACRU) #ACRU was not signif on treatment itself,
#but WAS signif w/ an interaction with forest type...interesting!!!


#NOW TO DO THE SAME WITH SAPLINGS!!!! (SEE BELOW)



## BELOW THIS LINE: NOT UPDATED/JUST COPIED FROM LAST SCRIPT ###


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



## ABOVE THIS LINE: NOT UPDATED/JUST COPIED FROM LAST SCRIPT ###


# SAPLINGS generalized linear mixed models -------------------------------

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
#all_saplings_plot$all_saplings_sqm <- all_saplings_plot$all_saplings_tally/(120+15) #REFER BACK TO JUNE UNIVARIATE_ANALYSIS_MASTER FOR DEETS ON WHY I DID THIS
#UPDATE 2/18/22, don't actually want or need to do this!!
#b/c negative binomial dist is for COUNT/INTEGER data

View(all_saplings_plot)

#NEXT STEP IS TO  TEST/MODEL THESE, AND ALSO GRAPH THEM!

#FIRST WILL DO FOR ALL SAPLINGS ACROSS SPECIES, OUTSIDE OF A LOOP:

nbmod_allsaplings_1 <- glmer.nb(formula=all_saplings_tally ~ Treatment*forest_type + 
                         (1 | Stand_name), 
                         data = all_saplings_plot[all_saplings_plot$species=="all",])
summary(nbmod_allsaplings_1)
#OKAY HOLD ON A MINUTE!! need to relevel these so unharvested is the baseline,
#but this is an INTERESTING result when compared to regen treatment!
#(both removal & unharvested tx have signif less overall saplings...)
all_saplings_plot$Treatment <- factor(all_saplings_plot$Treatment)
all_saplings_plot$Treatment <- relevel(all_saplings_plot$Treatment, ref="unharvested")

#now let's try again:
nbmod_allsaplings_2 <- glmer.nb(formula=all_saplings_tally ~ Treatment*forest_type + 
                                  (1 | Stand_name), 
                                data = all_saplings_plot[all_saplings_plot$species=="all",])
summary(nbmod_allsaplings_2)

#OK now let's try out this loop to iterate thru all the species:
sp_models <- list()
for (i in 1:5){
  loop_df <- all_saplings_plot[all_saplings_plot$species==major_species[i],]
  loop_model <- glmer.nb(formula=all_saplings_tally ~ Treatment*forest_type + 
                           (1 | Stand_name), data = loop_df)
  sp_models[i] <- loop_model
  
  print("")
  print("")
  print(major_species[i])
  print(summary(loop_model)) 
}

View(sp_models)

#OK SETTING THAT ALL ASIDE FOR NOW, LET'S LOOK AT SOME FIGURES!
### again, below is copied/modified from last script! ###
#OK, I think for now I just need to plot these & deal with the post hoc analyses later!

facet_labels2 <- c("all saplings", "sugar maple", "white ash", "American beech", "red maple", "yellow birch")
#reordering levels of the species variable as a factor:
all_saplings_plot$species <- factor(all_saplings_plot$species, levels=unique(all_saplings_plot$species),
                                    labels=facet_labels2)

saplings_p1 <- ggplot(data=all_saplings_plot, 
             aes(x=Treatment, y=all_saplings_tally,
                 fill=Treatment)) +
  #scale_fill_manual(values=wes_palette("Chevalier1")) +
  geom_bar(stat="identity") + 
  #theme_few() + 
  labs (x="Harvest treatment",
        y= "Average number of saplings per plot")+
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y") 
 # scale_x_discrete(limits=c("na", "thinning_focus", "regen_focus", "other"), #this argument (limits) re-orders the x-axis
 #                  labels=c("not harvested", "removal focus", "regeneration focus", "other")) + #and this argument renames the labels
#  theme(axis.text.x = element_text(angle=35, vjust=.8, hjust=.8)) #play around w/ this some more...
print(saplings_p1)
###THIS IS NOT WORKING! NEED TO CALCULATE MEAN VALUE INSTEAD!!
#p5 + theme(legend.position="none")
#something with stat IDENTITY???

plot()

#MAYBE: just create a "group means" dataset instead??


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












