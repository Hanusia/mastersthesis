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
library(vegan)
library(tidyverse)
library(lme4)

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

#OK, based on feedback from Tony, here's the new plan:
#generally, use the Peterson & Leach 2008 approach
#but use biomass as indicator instead of basal area
#analyze @ the plot level, but use stand as a random factor in the regression analysis
#also test if avg years since harvest is an impactful factor
#SO, basically need one big dataframe w/ both amount of biomass cut and proportion of biomass cut
#and then all of the regeneration variables, which are: 
#"Regeneration characteristics included in analyses were seedling/sapling density, 
#size, species richness, species diversity, and compositional change. 
#We calculated mean (per plot) richness (S, the number of species per sample) 
#and Shannon diversity (H0 ¼R pi log( pi), where pi is the proportion of individuals
#found in the ith species and log is to base 10; Kent and Coker 1994) for 
#salvaged and unsalvaged portions of the wind-disturbed forests as well as for 
#the undisturbed forest at NTSF." -Peterson and Leach 2008

#response variables I should use (I think) include:
#seedling/sapling density (pretty much done)
#sapling (?) sp richness
#sapling (?) Shannon diversity
#size???
#compositional change: Sorensen's index and the Chao-Sorensen similarity index
#I feel like it doesn't make sense to do the similarity indices b/c I don't have
#pre/post data....So instead, I'll stick w/ the sapling richness, diversity, & density
#OR, maybe use those similarity indices to compare harvested vs. unharvested??
#But I've basically already done that w/ the understory complementing PC-ORD...

#and my explanatory variables are amount biomass gone & proportion biomass gone

#methods for biomass calculation (VIA Kurth et al.) = "Biomass pools were estimated 
#using the species-specific allometric biomass equations in Jenkins et al. (2003) 
#and nutrient stocks in all woody components (trees, saplings, shrubs, FWM, and CWM) 
#were estimated using destructive sampling to derive species-specific nutrient
#concentrations as described in Klockow et al. (2014)"

#so, next step = read / look @ those papers for reference on calculating biomass.

#THEN, also need to figure out what the actual math will be re: regressions
#basically, just trying to fit regression equations based on disturbance severity,
#w/ time since harvest & site/stand as potential covariates?

#first step = "spin up" stump height to DBH using equations I've used before.
#then, calculate biomass of all stumps + live trees present.
#going to do this by just re-running some code from the script 08-09-21_stand_basal_area_calcs.R
#and then saving that file, and importing it into this script

overstory_plus <- read.csv("tree_stump_diams_CORRECTED_fromAugscript_06Apri2022.csv")
view(overstory_plus)

overstory_plus$functionaldbh <- overstory_plus$DBH_cm #assigning actual dbh as 'functional' dbh
#but then for stumps, replacing that w/ the stump DBH value caluclated in other script
overstory_plus$functionaldbh[overstory_plus$status=="stump"] <- overstory_plus$stump_DBH_cm[overstory_plus$status=="stump"]
#looks good to me!

treedata <- overstory_plus[,c("plot_ID", "Stand_name", "harvest_status", 
                              "species", "status", "decay_class", "functionaldbh")]
View(treedata)
#now, want to include ONLY stumps of DC 1 or 2 (b/c we can assume they were cut during harvest,
#whereas older ones probably fell naturally/otherwise)
#test first:
nrow(treedata[(treedata$status=="stump" & treedata$decay_class>2),]) #453 (new val)
nrow(treedata) #5439
treedata <- treedata[!(treedata$status=="stump" & treedata$decay_class>2),]
nrow(treedata) #4987 
#we are off by one number here......
#probably because it's including NAs! (so we are OK b/c there's 1 stump w/ no DC)

### added 4/6/22:
#and now to get rid of SNAGS as well: 
nrow(treedata[treedata$status=="snag",]) #431
treedata <- treedata[treedata$status!="snag",]
nrow(treedata) #4557
#ONCE AGAIN, we are off by one...presumably due to an NA value, so that's OK with me honestly!

####update, the data discrepancies described below have been RESOLVED!

##############################################
#IMPORTANT THING I JUST FOUND OUT: Hill Roberts site has trees WITHOUT 'status' info!!
#hopefully, this is just the live ones...?

#ADDRESSING THE PROBLEM WITH THE HILL ROBERTS DATA:
#gonna just assign the live class to those trees w/o a "status" 
#ONCE i get the NEW problem below sorted out...

#ALSO ONE MORE DATA DISCREPANCY: 
#there are a few live trees w/ a decay class associated with them (???)
#problematic <- overstory_plus[(overstory_plus$status=="live" & 
#                                !is.na(overstory_plus$decay_class)),]
#problematic2 <- overstory_data[(overstory_data$status=="live" & 
#                                  !is.na(overstory_data$decay_class)),]
#same 5 trees...

#Ok, now what about stumps/snags WITHOUT a decay class, they might be mis-IDed??
#problematic3 <- overstory_plus[((overstory_plus$status=="stump" | 
#                                   overstory_plus$status=="snag") &
#                                  is.na(overstory_plus$decay_class)),]
#ok damn, same issue here w/ 28 stumps/snags
#problematic4 <- overstory_data[((overstory_data$status=="stump" | 
#                                   overstory_data$status=="snag") &
#                                  is.na(overstory_data$decay_class)),]
#NOTE: problematic3 and problematic4 both still have 7 obs but those are 
#truthful to what the paper data sheets reflect! (e.g. DC wasn't recorded)

#NOW WE MUST INVESTIGATE!!! 
#update: investigation completed + fixed (for this analysis, at least!!),
#see workflow doc for details!!!
##############################################


#OK, now next step is basically to use these biomass allometric equations
#from Jenkins et al. 2003 to estimate above-ground biomass.

#thinking that to do this, I'll write a function to calculate biomass based on DBH
#for each tree; first just need to assign the number (?) to a table w/ all of the 
#species and their associated parameters.
#checking if that exists online first:
#oh hell yeah I found it!: http://www.fs.fed.us/ne/global/pubs/books/index.html
# originally via https://www.fs.usda.gov/treesearch/pubs/7058
#OK JK NEVER MIND this is actually the supplemental tables for a DIFFERENT publication
#BUT it still has the species info per group, it appears, so...gonna go with it!
#first uploading species list w/ associated group code:
specieslist <- read.csv("Jenkins_2004_biomass_tables/Table4_GTR-NE-319.csv")

#next trying to make a column to match w/ my species codes:
#specieslist$ID <- with(specieslist, paste0(gsub("(([A-Za-z]{2})[a-z& ]*)", "\\2", Genus), 
#                                  gsub("(([A-Za-z]{2})[a-z& ]*)", "\\2", Species)))
#specieslist$ID <- toupper(specieslist$ID)
unique(treedata$species[treedata$species %in% specieslist$ID])  #which of my sp DO have matches:
unique(treedata$species[!(treedata$species %in% specieslist$ID)]) #and which don't:

## paused here on 4/6/22...

#treedata2 <- merge(x=treedata, y=specieslist[,c("ID", "Species.group")],
#                        by.x="species", by.y="ID", all.x=TRUE, all.y=FALSE)
#UPDATE, this merge function did NOT really work...
#I might need to bring in my OWN species list....
#or else just do this manually...
my_species_table <- read.csv("C:/Users/theha/OneDrive - University of Vermont/Ash project/EAB_project_2020_additional_data.csv", fileEncoding = "UTF-8-BOM")
View(my_species_table)
#unique(my_species_table$common_name[my_species_table$common_name %in% specieslist$Common.name])

#OK this assigned MOST of the species to a group.
#but the ones that are left without include:
#unique(treedata$species[!(treedata$species %in% specieslist$ID)])
#and all of those EXCEPT for UNK, NA, and witch hazel do have corresponding values;
#we just gotta find them.
#so, should I ask Tony what to do about unk/NA values (mostly snags + a few stumps)?
#and similarly, should I be including snags in this @ all?
#no mention of them in Kurth et al.; Peterson and Leach says: "Fallen trees and
#snags that were obviously dead prior to 1999 were excluded from sampling."
#BUT does that approach change since we are using BIOMASS instead of basal area?

#TONY SAID: "In terms of your calculations, you only want to use cut stems from 
#the most recent harvest for calculating this.  As for what to do with unknown 
#species, I would average the values for the species group present on site for 
#those calculations. Finally, yes, I would do the seedlings and saplings 
#separately for your calculations."

#so this means: yes get rid of snags (as well as stumps w/ DC >=3, which I already did)
#gonna remove snags then re-run the code directly above to figure out how many/
#which species we don't have values for in the actual subset.

#EDITS MADE ABOVE/THROUGHOUT RE: SNAGS AND DATA ERRORS; PROCEEDING FROM HERE
#TO FLESH OUT THE BIOMASS CALCULATIONS

#starting here on 4/7/22:

#first need to remove the 1-2 lines w/o a species (or just replace it w/ "unk"!)
treedata2 <- treedata
treedata2$species[is.na(treedata2$species)] <- "UNK"
treedata2$species[treedata2$species==""] <- "UNK"
#ok I think we are good now...

treedata2 <- merge(x=treedata2, y=my_species_table[,c("spec_code", "common_name")], 
                   by.x="species", by.y="spec_code", all.x=TRUE, all.y=FALSE)

#and now to merge w/ group for biomass formula....
treedata2 <- merge(x=treedata2, y=specieslist[,c("Common.name", "Species.group")],
                       by.x="common_name", by.y="Common.name", all.x=TRUE, all.y=FALSE)

#now find out which ones didn't get assigned a group...
missingsp <- unique(treedata2$species[is.na(treedata2$Species.group)])
length(missingsp)
#and now just gonna assign them one by one...by manual lookup!
treedata2$Species.group[treedata2$species=="BESP"] <- "mb"
treedata2$Species.group[treedata2$species=="PODE"] <- "aa"
treedata2$Species.group[treedata2$species=="OSVI"] <- "mh"
treedata2$Species.group[treedata2$species=="ULSP"] <- "mh"
#going with group used by most maple species
treedata2$Species.group[treedata2$species=="ACER"] <- "mb" 
treedata2$Species.group[treedata2$species=="ACSP"] <- "mb"
treedata2$Species.group[treedata2$species=="PISP"] <- "sp"
#for witch hazel, using the group for other species in the same fam (Hamamelidaceae)
treedata2$Species.group[treedata2$species=="HAVI"] <- "mh"
treedata2$Species.group[treedata2$species=="BEPO"] <- "mb" #this is gray birch!
treedata2$Species.group[treedata2$species=="ULRU"] <- "mh"
treedata2$Species.group[treedata2$species=="UNK"] <- "unk"
#OK, now each line has a group! :) 

#NEXT step = creating a table that corresponds the species group codes w/ the 
#correct values/constants for the biomass formula...from Jenkins et al. 2003

#also need to make sure there aren't any in this list without a DBH...
sum(is.na(treedata2$functionaldbh)) #2
treedata2 <- treedata2[!is.na(treedata2$functionaldbh),] #removed those 2 rows!

#now, basically need to recreate Jenkins 2003 table 4!
biomass_formula <- data.frame("SpeciesGroup"=rep(NA), 
                              "B0"=rep(0), 
                              "B1"=rep(0))
#now inputting the values for each species group/type from table 4:
biomass_formula[1,] <- c("aa", -2.2094, 2.3867)
biomass_formula[2,] <- c("mb", -1.9123, 2.3651)
biomass_formula[3,] <- c("mh", -2.4800, 2.4835)
biomass_formula[4,] <- c("mo", -2.0127, 2.4342)
biomass_formula[5,] <- c("cl", -2.0336, 2.2592)
#ignoring doug-fir row b/c it's not needed...
biomass_formula[6,] <- c("tf", -2.5384, 2.4814)
biomass_formula[7,] <- c("pi", -2.5356, 2.4349)
biomass_formula[8,] <- c("sp", -2.0773, 2.3323)
#and finally, averaging together all group vals for "unk"
#just gotta convert to numeric cols first:
biomass_formula$B0 <- as.numeric(biomass_formula$B0)
biomass_formula$B1 <- as.numeric(biomass_formula$B1)
biomass_formula[9,] <- c("unk", mean(biomass_formula$B0[1:8]),
                         mean(biomass_formula$B1[1:8]))
#alright, we are good!

#the actual biomass equation (again, from Jenkins): 
# biomass=Exp(B0 + B1*ln(dbh))
#soo, now we just need to associate the right B0 and B1 vals to the treedata2 df...
#and then another col computing biomass using this formula!
treedata3 <- merge(x=treedata2, y=biomass_formula,
                   by.x="Species.group", by.y="SpeciesGroup",
                   all.x=TRUE)
View(treedata3) 
#need to make the cols we are calculating with numeric...
treedata3$functionaldbh <- as.numeric(treedata3$functionaldbh)
treedata3$B0 <- as.numeric(treedata3$B0)
treedata3$B1 <- as.numeric(treedata3$B1)
treedata3$biomass <- exp(treedata3$B0 + treedata3$B1*log(treedata3$functionaldbh))
#whoo!! now we've got biomass in kg for each live tree + recently-cut stump :) 

#next, need to separate by live + stump
livetreedata <- treedata3[treedata3$status=="live",]
cuttreedata <- treedata3[treedata3$status=="stump",]
nrow(treedata3) #4555
nrow(livetreedata) + nrow(cuttreedata) #and same!
livetreedata <- aggregate(formula=biomass~plot_ID, FUN=sum, data=livetreedata)
cuttreedata <- aggregate(formula=biomass~plot_ID, FUN=sum, data=cuttreedata)
#rename columns:
livetreedata <- rename(livetreedata, live_biomass=biomass)
cuttreedata <- rename(cuttreedata, cut_biomass=biomass)
#and maybe put in terms of per hectare?? (convert)
#buuuut, since we are dealing with *proportions,* maybe don't actually need to...
#and now need to merge w/ plot info...
View(plot_info)
plotbiomass <- merge(x=plot_info[,c("plot_ID", "harvest_status"),],
                     y=livetreedata, all=TRUE)
View(plotbiomass)
plotbiomass <- merge(x=plotbiomass, y=cuttreedata, all=TRUE)
#looks good!, just gotta replace NAs with 0s:
plotbiomass[is.na(plotbiomass)] <- 0
#and calculation proportion of biomass cut:
plotbiomass$propbiocut <- plotbiomass$cut_biomass/
  (plotbiomass$cut_biomass + plotbiomass$live_biomass)
#now let's check it out:
summary(plotbiomass$propbiocut)
summary(plotbiomass$propbiocut[plotbiomass$harvest_status=="NO"])
summary(plotbiomass$propbiocut[plotbiomass$harvest_status=="YES"])
#so, we can use the 'proportion biomass cut' as the disturbance severity index
#for harvested plots???
#or/also use the raw value of biomass cut (in kg, but can convert to per ha?)
#let's save this dataframe:
write.csv(x=plotbiomass, file="biomass_for_DSI_7Apr2022.csv")

#next steps = calculate RESPONSE variables!
#e.g. seedling & sapling density, Shannon diversity, and sp richness!
#for density, just need to import data tables from other script (that I hopefully saved??)
#richness should be relatively easy to calculate...
#and Shannon diversity, hopefully there's a simple formula for that one?

#let's import the csvs first to see what we're working with:
#OK, the 2 csvs I made on Feb. 28th do *not* include info for each species,
#just the total across all species and then each of my focal species.
#so...basically need to repurpose/redirect some of the code I used for those 
#to get an aggregate per species (count OF species for richness, 
#and count PER species for Shannon diversity index)
#IMPORTANT: need to figure out the FORMAT data must be in to input to Shannon index!

#ughhhh ok let's figure out the best way to do this...gotta look at past code
#although actually for richness, it might be easy-ish...
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
seedling_summary <- aggregate(formula=tally ~ plot_ID + species, FUN=sum,
                              data=seedling_data)
#seedling_richness <- aggregate(formula=species~plot_ID, FUN=count, data=seedling_summary)
#need to then pivot this to wider to generate a matrix for Shannon diversity
#and then actually can summarize how many columns (aka species) have non-zero vals
#to get a value for richness of seedlings...
seedling_summary_wide <- pivot_wider(data=seedling_summary,
                                     id_cols="plot_ID",
                                     names_from="species",
                                     values_from="tally")
View(seedling_summary_wide)
#looks good!, just gotta replace NAs with 0s:
seedling_summary_wide[is.na(seedling_summary_wide)] <- 0
seedling_richness <- seedling_summary_wide
seedling_richness$richness <- rep(0)
  for(i in 1:nrow(seedling_richness)){
    seedling_richness$richness[i] <-
      sum(seedling_summary_wide[i,2:ncol(seedling_summary_wide)]>0)
  }
#looks right!! now maybe add this to a 'master' dataframe?
dsi_data <- plotbiomass
View(dsi_data)
dsi_data <- merge(dsi_data, seedling_richness[,c("plot_ID", "richness")],
                  by="plot_ID", all=TRUE)
#rename col to specify SEEDLING richness: rename(x, newname=oldname)
dsi_data <- rename(dsi_data, seedling_richness=richness)
#and once again fill in zeros:

#NOW, to calculate Shannon diversity per plot...
#first need to make sure ALL plots are accounted for:
#this ALSO arranged them alphabetically by plot, so great!
seedling_summary_wide <- merge(x=seedling_summary_wide, y=plot_info[,c("plot_ID","Stand_name")],
                               by="plot_ID", all=TRUE)
#and replace NAs w/ 0s:
seedling_summary_wide[is.na(seedling_summary_wide)] <- 0
#first column and last column are NOT part of what should be calculated for Shannon diversity
seedling_summary_wide$Shannon <- diversity(x=seedling_summary_wide[,2:37],
                                           index="shannon")
#oops let's pick a more specific name!
seedling_summary_wide <- rename(seedling_summary_wide, seedling_shannon=Shannon)
dsi_data <- merge(dsi_data, seedling_summary_wide[,c("plot_ID", "seedling_shannon")])
#OK we basically now have everything for seedlings (except for total tally)!
#let's just quickly do that...
seedling_tally <- aggregate(formula= tally~plot_ID, FUN= sum, data= seedling_data)
seedling_tally$seedling_density <- seedling_tally$tally/3 #calculate plot-level seedling density per m^2
dsi_data <- merge(dsi_data, seedling_tally[,c("plot_ID", "seedling_density")],
                  all=TRUE)
#ok, NOW we have all the seedling indicators!
#next step is to calculate the sapling ones!
#but FIRST, gonna save this dataframe....
write.csv(dsi_data, file="biomass_seedlings_DSI_7Apr2022.csv")

#ORRRR I could just go ahead and model/plot out the seedling relationships first...
#TBH I'm gonna start with that, because they saplings data is gonna be a bitch to re-wrangle...
# (and for next time/future:
#dsi_data <- read.csv("biomass_seedlings_DSI_7Apr2022.csv")

dsi_data_cut <- dsi_data[dsi_data$harvest_status=="YES",]
View(dsi_data_cut)
dsi_data_cut <- merge(dsi_data_cut, plot_info[,c("plot_ID", "Stand_name")],
                      by="plot_ID", all.x=TRUE)
#now to calculate mean years since harvest...
stand_info$harvest_mean_year <- ((as.numeric(stand_info$harvest_start_year) + 
                                    as.numeric(stand_info$harvest_end_year)) / 2)
dsi_data_cut <- merge(dsi_data_cut, stand_info[,c("Stand_name", "harvest_mean_year")],
                      by="Stand_name", all.x=TRUE)
dsi_data_cut$years_since_harvest <- 2020-dsi_data_cut$harvest_mean_year
sum(is.na(dsi_data_cut$years_since_harvest))
#The info for which we don't have this is Groton...

#also need to remove zeros from seedling density etc vals!
dsi_data_cut$seedling_density[is.na(dsi_data_cut$seedling_density)] <- 0

testmod1 <- lmer(formula= seedling_density ~ propbiocut + (1 | Stand_name),
                  data= dsi_data_cut)
res <- resid(testmod1)
plot(res)
plot(fitted(testmod1), res) 
#alright these are looking non-normal...as expected?
#since this is density, should perhaps use neg binomial distribution for this one specifically?
#but do they correlate w/ years since harvest??
plot(x=dsi_data_cut$years_since_harvest, y=res) 
#doesn't really look like there's a relationship...
#length(dsi_data_cut$years_since_harvest)
#sum(is.na(dsi_data_cut$seedling_density))
summary(testmod1)

#let's try the same w/ Shannon, richness values
dsi_data_cut$seedling_richness[is.na(dsi_data_cut$seedling_richness)] <- 0

testmod2 <- lmer(formula= seedling_richness ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut)
res2 <- resid(testmod2)
plot(res2) #alright, these are looking properly scattered!
plot(fitted(testmod2), res2) #this looks a lil funky..... but just b/c of stand maybe?
plot(x=dsi_data_cut$years_since_harvest, y=res2) #also looks fine/no correlation!
summary(testmod2)

testmod3 <- lmer(formula= seedling_shannon ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut)
res3 <- resid(testmod3)
plot(res3) #alright, these are looking properly scattered!
plot(fitted(testmod3), res3) #this looks a lil funky..... but just b/c of stand/site effect? maybe?
plot(x=dsi_data_cut$years_since_harvest, y=res3) #also looks fine/no correlation!
summary(testmod3)
#ALRIGHT, I think (for seedlings at least), we can concludet that yrs since harvest doesn't matter!

plot(dsi_data_cut$propbiocut, dsi_data_cut$seedling_density)
plot(dsi_data_cut$propbiocut, dsi_data_cut$seedling_richness) #LOL YIKES
plot(dsi_data_cut$propbiocut, dsi_data_cut$seedling_shannon) #seems like no relaysh here either...

#q: what is T value in the summary output from these models??

#action item: re-run seedling density model w/ neg binomial distribution!
#REMINDER, we need integers for this, so converting back to 'seedlings per plot'
testmod4 <- glmer.nb(formula= seedling_density*3 ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut)
res4 <- resid(testmod4)
plot(res4) #alright, NOW these are looking properly scattered!
plot(fitted(testmod4), res4) #much better than before, at least?!
plot(x=dsi_data_cut$years_since_harvest, y=res4) #also looks fine/no correlation!
summary(testmod4)
#tl;dr = nothing signif happening here?? (is that the takeaway?)



#OK I should really move on to saplings...I just don't want to lol

#first, check to make sure there isn't the same species/genus issue for seedlings
#as I had for saplings when doing the PerMANOVA, b/c that affects calculations
#of richness, Shannon index, etc???
unique(seedling_summary$species)
#ok so we DID have this happening..............
#need to figure out what to do about this LATER.............

#alright, I'm back (as of 4/13/22)!
#I think the first step to figure out if the species/genus thing is an issue, 
#is to see if they existed within the same PLOT...because that's the only thing 
#that would throw off the calculations...

#testing maples 1st--up to 3 problem plots:
nrow(seedling_summary_wide[(seedling_summary_wide$ACPE>0 & seedling_summary_wide$ACER>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$ACRU>0 & seedling_summary_wide$ACER>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$ACSA>0 & seedling_summary_wide$ACER>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$ACSP2>0 & seedling_summary_wide$ACER>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$ACPE>0 & seedling_summary_wide$ACSP>0),]) #1
nrow(seedling_summary_wide[(seedling_summary_wide$ACRU>0 & seedling_summary_wide$ACSP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$ACSA>0 & seedling_summary_wide$ACSP>0),]) #2
nrow(seedling_summary_wide[(seedling_summary_wide$ACSP2>0 & seedling_summary_wide$ACSP>0),])

sum(seedling_summary_wide$ACSP) #only in 2 plots total....so I'm thinking we should just delete those??
seedling_summary_wide[seedling_summary_wide$ACSP>0,]
#plots are CH145unh4 and TNCLPunh3    
#UPDATE: THOSE TWO AREN'T CUT PLOTS ANYWAY SO NO ISSUES THERE
#OR just subtract 1 from the richness value of each of those plots...
#JK can't do that w/ Shannon index calculation...
#ALRIGHT let's see if this is a problem with other species FIRST then decide
#on a course of action!
nrow(seedling_summary_wide[(seedling_summary_wide$BEAL>0 & seedling_summary_wide$BESP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$BEFA>0 & seedling_summary_wide$BESP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$BEPA>0 & seedling_summary_wide$BESP>0),])
#birches all good!
nrow(seedling_summary_wide[(seedling_summary_wide$PRPE>0 & seedling_summary_wide$PRSP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$PRSE>0 & seedling_summary_wide$PRSP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$PRVI>0 & seedling_summary_wide$PRSP>0),])
#ditto cherries!
nrow(seedling_summary_wide[(seedling_summary_wide$ULAM>0 & seedling_summary_wide$ULSP>0),]) #1
nrow(seedling_summary_wide[(seedling_summary_wide$VIAC>0 & seedling_summary_wide$VISP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$VIAL>0 & seedling_summary_wide$VISP>0),])
nrow(seedling_summary_wide[(seedling_summary_wide$UNK>0 & seedling_summary_wide$UNKHW>0),])
#alright so we just have one problem elm...
seedling_summary_wide[seedling_summary_wide$ULSP>0,]
#it's in plot OMSFCRmat3, which IS in the cut plots dataset...so let's just deal with that individually

#QUESTION (maybe for Tony): should we just deal with this the same way we did for the NMS
#to stay consistent in how we are treating the data??????
#I mean, it's already a different data set anyway since we're only looking at cut plots...

problem_elm <- seedling_summary_wide[seedling_summary_wide$ULSP>0,]
problem_elm$ULSP <- problem_elm$ULSP + problem_elm$ULAM
problem_elm$ULAM <- 0
problem_elm
problem_elm_shannon <- diversity(x=problem_elm[,2:37],  index="shannon") #new Shannon val = 1.051136

#now to reassign those values (Shannon + richness):
dsi_data_cut[dsi_data_cut$plot_ID=="OMSFCRmat3",]
dsi_data_cut[dsi_data_cut$plot_ID=="OMSFCRmat3", "seedling_shannon"] <- problem_elm_shannon
dsi_data_cut[dsi_data_cut$plot_ID=="OMSFCRmat3", "seedling_richness"] <- 3 #existing richness of 4 -1

#NOW just need to re-run the models for seedling Shannon and richness!!!!
testmod5 <- lmer(formula= seedling_richness ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut)
res5 <- resid(testmod5)
plot(res5) #alright, these are looking properly scattered!
plot(fitted(testmod5), res5) #this looks a lil funky..... but just b/c of stand maybe?
plot(x=dsi_data_cut$years_since_harvest, y=res5) #also looks fine/no correlation!
summary(testmod5)

testmod6 <- lmer(formula= seedling_shannon ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut)
res6 <- resid(testmod6)
plot(res6) #alright, these are looking properly scattered!
plot(fitted(testmod6), res6) #this looks a lil funky..... but just b/c of stand/site effect? maybe?
plot(x=dsi_data_cut$years_since_harvest, y=res6) #also looks fine/no correlation!
summary(testmod6)


#follow-up question: HOW TO CALCULATE R-SQUARED FOR THE REGRESSION FROM THESE MODELS???
#this webpage: https://ecologyforacrowdedplanet.wordpress.com/2013/08/27/r-squared-in-mixed-models-the-easy-way/
#uses the function r.squaredGLMM from the package MuMIn, which works w/ lme4
install.packages("MuMIn")
library(MuMIn)
r.squaredGLMM(testmod4) #for density...result is a lil wonky b/c of the neg binomial distrib.
r.squaredGLMM(testmod5)
r.squaredGLMM(testmod6)
#LOL ok all r-squared values are extreeeemely low...as I suspected!!!

#that wraps up this bit I think; now it is REALLY time to move onto saplings...

#WAIT--SHOULD I ALSO REMOVE SPECIES OF UNKNOWN PROVENANCE?!
#honestly, I'm keeping it in! can contribute to richness + density

#WAIT I ALREADY HAVE THESE FROM PC-ORD INPUTS...DUH!!!
#*except* those are aggregated at the stand level...womp womp womp
#but I can probably use the same code, pretty much??
#update: will use PC-ORD code for saplings; seedlings I think will be a lot simpler 

##### following code is imported/modified from PCORD_prep_code_14Jan2022.R 
### IMPORTANT CAVEAT: ALSO NEED TO COMBINE SPECIES W/ 'UNK' SPECIES THE WAY I DID FOR PCORD/
#PERMANOVA ANALYSIS
#WAIT SHOULD I HAVE DONE THAT FOR SAPLINGS TOO??????
#View(small_sapling_data)
small_sapling_data$tally <- small_sapling_data$shrub + small_sapling_data$over_1_ft + small_sapling_data$over_4.5_ft
small_saplings_sp <- aggregate(tally ~ plot_ID + species, data = small_sapling_data, FUN=sum)
#IMPORTANT: need to figure out how to remove any that are DEAD....but prob do this after rbind so I only do it once!
#anyway, first, do this w/ large saplings too:
#View(large_sapling_data)
large_sapling_data$tally <- large_sapling_data$class_1 + large_sapling_data$class_2 + large_sapling_data$class_3
large_saplings_sp <- aggregate(tally ~ plot_ID + species, data = large_sapling_data, FUN=sum)
#now to rbind (at least the saplings) together: 
saplings_plot <- rbind(large_saplings_sp, small_saplings_sp)
View(saplings_plot)
######
#AT THIS STAGE need to RENAME "ACPE " species:
saplings_plot[saplings_plot=="ACPE "] <- "ACPE"
#now to aggregate again by plot & species:
saplings_plot <- aggregate(tally ~ plot_ID + species, data = saplings_plot, FUN=sum)
#this has ALMOST all plots represented (228) (checked via > length(unique(saplings_stand$Stand_code)) )
#now to remove DEAD species: 
#follow THIS code from Landis prep script for guidance: 
#ok, executive decision: filtering out any dead saplings!
throwaway <- grep(pattern="\\w+-dead", x=saplings_plot$species, value=TRUE, fixed=FALSE, perl=TRUE)
# I HAD TO FIDDLE WITH THIS SO MUCH BUT THIS VERSION OF IT FINALLY WORKED!!!!
head(throwaway)
length(throwaway)
#actually NOT throwaway b/c I'm actually using it now to subset...
#and getting rid of dead sapling "species"
saplings_plot <- saplings_plot[!(saplings_plot$species %in% throwaway),]
#nice!

#also want to get rid of "UNK" and "?" species:
#OR DO I???? #Update: it's only 5 plots total that have those, so it's no great loss!
saplings_plot <- saplings_plot[!(saplings_plot$species=="UNK" | saplings_plot$species=="?"),]

#Okie dokie! Now at THIS point, want to keep this DF as-is so I can add the seedling data to it, and also make a version of it w/ just saplings.

#then, for each of the dataframes I have at this point (saplings_stand and understory_stand), need to bind numplots so I can calculate #/ha
#WAIT, I forgot this is going to be hard b/c they were sampled in different areas..... -_-
#could just divide by numplot and have "tally per plot" as the metric??? I Think that will work just fine...
#saplings_stand <- merge(saplings_stand, stand_info[,c("Stand_code", "num_plots")])
#saplings_stand$tally_per_plot <- saplings_stand$tally/saplings_stand$num_plots

#I think final step is just to pivot_wider?!
saplings_summary_wide <- pivot_wider(data=saplings_plot, 
                                   id_cols="plot_ID",
                                   names_from="species",
                                   values_from="tally")
View(saplings_summary_wide)

#next we are replicating steps from what we did w/ SAPLINGS above!
#looks good!, just gotta replace NAs with 0s:
saplings_summary_wide[is.na(saplings_summary_wide)] <- 0
saplings_richness <- saplings_summary_wide
saplings_richness$richness <- rep(0)
for(i in 1:nrow(saplings_richness)){
  saplings_richness$richness[i] <-
    sum(saplings_summary_wide[i,2:ncol(saplings_summary_wide)]>0)
}
#looks right!! now add this to the same 'master' dataframe
View(dsi_data)
dsi_data <- merge(dsi_data, saplings_richness[,c("plot_ID", "richness")],
                  by="plot_ID", all=TRUE)
#rename col to specify saplings richness: rename(x, newname=oldname)
dsi_data <- rename(dsi_data, saplings_richness=richness)

#NOW, to calculate Shannon diversity per plot...
#first need to make sure ALL plots are accounted for:
#this ALSO arranged them alphabetically by plot, so great!
saplings_summary_wide <- merge(x=saplings_summary_wide, y=plot_info[,c("plot_ID","Stand_name")],
                               by="plot_ID", all=TRUE)
#and replace NAs w/ 0s:
saplings_summary_wide[is.na(saplings_summary_wide)] <- 0
#first column and last column are NOT part of what should be calculated for Shannon diversity
saplings_summary_wide$saplings_shannon <- diversity(x=saplings_summary_wide[,2:50],
                                           index="shannon")

dsi_data <- merge(dsi_data, saplings_summary_wide[,c("plot_ID", "saplings_shannon")])
#OK we basically now have everything for saplings (except for total tally)!
#let's just quickly do that...
saplings_tally <- saplings_summary_wide
saplings_tally$saplings_tally <- rep(0)
for(i in 1:nrow(saplings_tally)){
  for(j in 2:50){
    saplings_tally$saplings_tally[i] <- 
      saplings_tally$saplings_tally[i] + saplings_summary_wide[i,j]
  }
}
View(saplings_tally)
#don't think we actually want the line below because we have to use whole #s for neg binomial dist of total sapling density:
#saplings_tally$saplings_density <- saplings_tally$tally/3 #calculate plot-level saplings density per m^2
dsi_data <- merge(dsi_data, saplings_tally[,c("plot_ID", "saplings_tally")],
                  all=TRUE)
#ok, NOW we have all the sapling indicators!
#but FIRST, gonna save this dataframe....
write.csv(dsi_data, file="biomass_saplings_seedlings_DSI_7Apr2022.csv")

#BUT FIRST, NEED TO ALSO CHECK IF WE HAVE PROBLEM SPECIES/VAGUE CLASSIFICATIONS THAT ARE OCCURRING
unique(saplings_plot$species) #all the dif 'species' names

#the first step to figure out if the species/genus thing is an issue, 
#is to see if they existed within the same PLOT...because that's the only thing 
#that would throw off the calculations...
#testing maples 1st:
nrow(saplings_summary_wide[(saplings_summary_wide$ACPE>0 & saplings_summary_wide$ACER>0),])
nrow(saplings_summary_wide[(saplings_summary_wide$ACRU>0 & saplings_summary_wide$ACER>0),])
nrow(saplings_summary_wide[(saplings_summary_wide$ACSA>0 & saplings_summary_wide$ACER>0),])
nrow(saplings_summary_wide[(saplings_summary_wide$ACSP2>0 & saplings_summary_wide$ACER>0),])
nrow(saplings_summary_wide[(saplings_summary_wide$ACPE>0 & saplings_summary_wide$ACSP>0),]) #1 
nrow(saplings_summary_wide[(saplings_summary_wide$ACRU>0 & saplings_summary_wide$ACSP>0),])
nrow(saplings_summary_wide[(saplings_summary_wide$ACSA>0 & saplings_summary_wide$ACSP>0),]) #1
nrow(saplings_summary_wide[(saplings_summary_wide$ACSP2>0 & saplings_summary_wide$ACSP>0),])
#which ones?
saplings_summary_wide[saplings_summary_wide$ACSP>0,]
#only one plot, MP1314gap1, which has 'acsp' AND 'acsa' AND 'acpe'
#will deal with this LATER after done checking other species!
nrow(saplings_summary_wide[(saplings_summary_wide$PIRU>0 & saplings_summary_wide$PISP>0),])
nrow(saplings_summary_wide[(saplings_summary_wide$ULRU>0 & saplings_summary_wide$ULSP>0),]) #1
saplings_summary_wide[saplings_summary_wide$ULSP>0,]
#only one plot, HSF161mat3, has ULRU *and* ULSP
#for the elm one, I think we can safely do what we did for the seedling case and just
#combine them...
#but for the maple case, since it would be decreasing richness with 2 of the most (only?)
#influential/prevalent species in the plot, maybe just take out the "ACSP" ones??
#and then will need to recalculate richness as well...
#then just continue on and follow the same pattern as what I did for seedling models!
#and then record the results in my results master doc!

problem_elm2 <- saplings_summary_wide[saplings_summary_wide$ULSP>0,]
problem_elm2$ULSP <- problem_elm2$ULSP + problem_elm2$ULRU
problem_elm2$ULRU <- 0
problem_elm2
problem_elm2_shannon <- diversity(x=problem_elm2[,2:50],  index="shannon") #new Shannon val = 1.69202

#now to reassign those values (Shannon + richness):
#by FIRST creating the 'cut' subset of the dataset:
dsi_data_cut2 <- dsi_data[dsi_data$harvest_status=="YES",]
View(dsi_data_cut2)
dsi_data_cut2 <- merge(dsi_data_cut2, plot_info[,c("plot_ID", "Stand_name")],
                      by="plot_ID", all.x=TRUE)
#now to calculate mean years since harvest...
#stand_info$harvest_mean_year <- ((as.numeric(stand_info$harvest_start_year) + 
#                                    as.numeric(stand_info$harvest_end_year)) / 2)
dsi_data_cut2 <- merge(dsi_data_cut2, stand_info[,c("Stand_name", "harvest_mean_year")],
                      by="Stand_name", all.x=TRUE)
dsi_data_cut2$years_since_harvest <- 2020-dsi_data_cut2$harvest_mean_year
sum(is.na(dsi_data_cut2$years_since_harvest))
#Groton is still the problem stand here...

#and NOW to reassign Shannon + richness values for this problem plot:
dsi_data_cut2[dsi_data_cut2$plot_ID=="HSF161mat3",]
dsi_data_cut2[dsi_data_cut2$plot_ID=="HSF161mat3", "saplings_shannon"] <- problem_elm2_shannon
#I MESSED UP HERE, SO IF THIS PLOT IS AN ISSUE IN THE FUTURE, COME BACK TO THIS!!
dsi_data_cut2[dsi_data_cut2$plot_ID=="HSF161mat3", "saplings_richness"] <- 6 #existing richness of 7 -1

#NEXT, let's address the ACSP mess.
problem_maple <- saplings_summary_wide[saplings_summary_wide$ACSP>0,]
problem_maple
problem_maple$ACSP <- 0 #let's just remove that one...
problem_maple_shannon <- diversity(x=problem_maple[,2:50],  index="shannon") #new Shannon val = 1.289591
#and NOW to reassign Shannon + richness values for this problem plot:
dsi_data_cut2[dsi_data_cut2$plot_ID=="MP1314gap1",]
dsi_data_cut2[dsi_data_cut2$plot_ID=="MP1314gap1", "saplings_shannon"] <- problem_maple_shannon
dsi_data_cut2[dsi_data_cut2$plot_ID=="MP1314gap1", "saplings_richness"] <- 5 #existing richness of 6 -1
#alright, we are looking good!!

#now, just need to run the models like I did with seedlings for dsi_data_cut2...
#and also just check if there's a correlation with years since harvest??

#first, look at sapling density:
testmod7 <- glmer.nb(formula= saplings_tally ~ propbiocut + (1 | Stand_name),
                     data= dsi_data_cut2)
res7 <- resid(testmod7)
plot(res7) #alright, NOW these are looking properly scattered!
plot(fitted(testmod7), res7) #much better than before, at least?!
plot(x=dsi_data_cut$years_since_harvest, y=res7) #also looks fine/no correlation!
summary(testmod7) #proportion of biomass cut is *almost* significant here??

#next = sapling richness
testmod8 <- lmer(formula= saplings_richness ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut2)
res8 <- resid(testmod8)
plot(res8) #alright, these are looking properly scattered!
plot(fitted(testmod8), res8) #this looks a lil funky..... but just b/c of stand maybe?
plot(x=dsi_data_cut$years_since_harvest, y=res8) #also looks fine/no correlation!
summary(testmod8)

#and finally = sapling Shannon diversity
testmod9 <- lmer(formula= saplings_shannon ~ propbiocut + (1 | Stand_name),
                 data= dsi_data_cut2)
res9 <- resid(testmod9)
plot(res9) #alright, these are looking properly scattered!
plot(fitted(testmod9), res9) #this looks a lil funky..... but just b/c of stand/site effect? maybe?
plot(x=dsi_data_cut$years_since_harvest, y=res9) #also looks fine/no correlation!
summary(testmod9)

#now let's plot the 3 variables:
plot(dsi_data_cut2$propbiocut, dsi_data_cut2$saplings_tally) #maybe a sliiiight positive relationship here?
plot(dsi_data_cut2$propbiocut, dsi_data_cut2$saplings_richness) #wonky b/c of discrete vals for richness...
plot(dsi_data_cut2$propbiocut, dsi_data_cut2$saplings_shannon) #seems like no relaysh here either...


#and FINALLY, let's get some more R-squared values:
r.squaredGLMM(testmod7) #for density...result is a lil wonky b/c of the neg binomial distrib.
r.squaredGLMM(testmod8)
r.squaredGLMM(testmod9)
#once again, all R-squared vals are quite small...