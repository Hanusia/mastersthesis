# --------------------------------------------------
# LANDIS thesis subset runs processing + data analysis
# 16 Jun 2022
# HH
# --------------------------------------------------
#

# housekeeping / read in global variables -------------------------------

library(terra)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(plotrix)


workdir <- "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/thesis_runs"
rcp <- c("histclim", "rcp45", "rcp85")
gcm <- c("rep1-CCSM4", "rep2-CESM1-BGC", "rep3-HADGEM2-ES", "rep4-MPI-ESM-LR")
mgmt <- c("BAU", "removal", "retention")
species <- c(splist, "fraxamtx", "fraxnitx") #ALL species...
#(splist var from initial_community_map_exploration script)
#species


# preliminary thoughts...  -------------------------------

#for each variable I am analyzing, will create a dataframe w/ these variables to read into

#ALSO, remember that I'll need a way to analyze ash "species" TOGETHER sometimes...

#ACTION ITEM: check how output table of AGbiomass/total biomass is formatted 
#(e.g. is it recording MEAN biomass per species? Chck against total vals extracted from rasters)

#also will need to prep data for input to PC-ORD for ordination....

#NOTE: SEE notes from LANDIS workflow doc on 5/26 to recap/summarize Olivia's & Matthias' methods

#QUESTION FOR TONY/JANE: should I be using nonparametric stats for this since I only have 4 reps?
#IF SO, see Olson paper or Lucash et al. 2017 paper for details on this decision/
#which tests to use instead....

#NMS = most useful to compare RELATIVE biomass between species/scenarios
#but....is it the CHANGE in relative biomass???

#also can calculate Shannon diversity for relative biomass for species- incorporates evenness

#should I do analyses for dominant species along with ash?? (maybe the ones from my first chapter?)
#that would be: red maple, sugar maple, beech, and yellow birch (plus the two ash sp).....
#looks like those four are ALSO the most dominant sp (by avg biomass), at least @ year 0...
#still not sure why red maple is declining so much over time but probably more of a parameters issue

#ASK TONY/JANE:
 #should I also analyze these other species (of interest)?

#Olson used nonparametric tests including: Wilcoxon-Mann-Whitney test, Chi-squared test,
#Kruskal–Wallis test and post hoc Wilcoxon rank sum test
#Basically, ask Tony and Jane their thoughts on this

#Matthias also applied "general relativisation" before doing NMS in PC-ORD
#and just did NMS for relative biomass AT YEAR 100 (and at year 200), not CHANGE IN rel biom.

#basically, I don't think I should actually start these analyses til I talk to Tony and Jane...
#but good to be thinking about it first!

#So, to sum up:
#maps of the biomass reclass output x 9 pathways, at year 100 (just pick 1 replicate??), are good visual
#total (all sp.) AGB + individual species (of interest) total AND/OR rel. AGB @ year 100:
#summarize means (in a fig.?) + do 2-way ANOVA (or mixed model anova????? would model type be the random var??)
#for each of those values.

#also ask - should I report biomass in g/m2 or Mg/ha???
#probably Mg/ha, since that's what I did for my other chapter.....

# year 100 live aboveground biomass by sp -------------------------------

#back on 17 June 2022!

#Tony suggested using year 100 biomass & 2-way ANOVA to look @ mgmt x climate.

#use functions from other script to extract values of species AGB

#NOTE: from now on, AGB = live aboveground biomass

#main dataframe will be YEAR 100 biomass vals per sp (and maybe # cohorts too??)
spagbyear100 <- data.frame("species" = rep(NA), #record species,
                           "climate" = rep(NA), #climate,
                           "harvest" = rep(NA), #mgmt scenario,
                           "rep" = rep(NA),     #and replicate 
                           #                                "year" = rep(0),     #for each timestep
                           #                                "sumagb"= rep(0),    #and each var of interest
                           "meanagb_gm2" = rep(0),
                           #RELATIVE AGB compared to total agb w/ all species in that scenario/year/landscape
                           "relagb" = rep(0))
spagbyear100 <- spagbyear100[0,]
#this dataframe will be in LONG format

#also need one that summarizes the CHANGE in vars of interest from year 0 to 100
#update: maybe not??? (after talking w/ Tony...)

#changeagb_byspecies <- data.frame("species" = rep(NA), #record species,
#                                  "climate" = rep(NA), #climate,
#                                  "harvest" = rep(NA), #mgmt scenario,
#                                  "rep" = rep(NA),     #and replicate 
#                                  #and DELTA for each var of interest
#                                  "changesumagb"= rep(0),    
#                                  "changemeanagb" = rep(0),
#                                  #change in RELATIVE AGB compared to total agb w/ all species in that scenario/landscape
#                                  "changerelagb" = rep(0)
#  
#)

## ACTION ITEM: figure out how to efficiently repeat an analysis e.g. for each species
#and store the results/relevant outputs in a dataframe or similar...
#probably should look for examples of code doing this online.


#loop to calculate mean AGB + relative AGB:
#also gotta convert (absolute) values from g/m2 to Mg/ha
#but do that part after the loop!)
species
splab <- paste0(species, "g/m2")
splab

for(k in rcp){
  for(j in (gcm)){
    for(i in mgmt){ 
      #read in AGB summary table for this climate/mgmt/replicate:
      agbtab <- read.csv(paste(workdir, k, j, i, "output/AGbiomass/AG/biomass-AllYears.csv", sep="/"),
                          header=TRUE)
      #subsetting to JUST species agb in the year 100 (w/o Time col):
      agbtab <- agbtab[agbtab$Time==100,]
      agbtab <- agbtab[,-1]
      #calculate TOTAL year-100 AGB:
      totalagbyear100 <- sum(agbtab[1,])
      for(h in 1:length(species)){
        #create new row to append to total DF:
        addrow <- spagbyear100[1,]
        addrow$species[1] <- species[h]
        addrow$climate[1] <- k
        addrow$harvest[1] <- i
        addrow$rep[1] <- j
        addrow$meanagb_gm2[1] <- agbtab[1,h]
        addrow$relagb[1] <- addrow$meanagb_gm2/totalagbyear100
        
        #then append this to output dataframe:
        spagbyear100 <- rbind(spagbyear100, addrow)
      }
    }
  }
}
#convert abs agb to mg/ha
spagbyear100$meanagb_Mgha <- spagbyear100$meanagb_gm2/100

View(spagbyear100)


#now to ANALYZE:
#do some 2-factor ANOVAs...
#maybe even a MANOVA??? (but let's not get too carried away......)

#add a for loop here to execute those ANOVAs + share the outputs...

# ANOVAs loop results across response variables -------------------------------

#make a function for this??? 
##ok just gonna output all the results lmao

twowayaov <- function(df, spselect, resp){
  # df = dataframe to use for the anova
  # spselect = vector of species to analyze
  # resp = name of df column for chosen response variable
  for(i in spselect){
    model <- aov(resp ~ harvest*climate, data=df[df$species==spselect,])
    print(summary(model))
    print(TukeyHSD(model))
  }
}

# creating ash + "combined" ash species dataframes -------------------------------

ashsp <- c("fraxamer", "fraxnigr", "fraxamtx", "fraxnitx")
ashstatsyear100 <- spagbyear100[(spagbyear100$species %in% ashsp),]
View(ashstatsyear100)

#now, aggregating by overarching ash species...
#e.g. in this dataframe combinedashstats, 'regular' white ash (fraxamer) and
#'treated' white ash (fraxamtx) are grouped TOGETHER.
combinedashstats <- ashstatsyear100
combinedashstats$species[combinedashstats$species=="fraxamtx"] <- "fraxamer"
combinedashstats$species[combinedashstats$species=="fraxnitx"] <- "fraxnigr"
View(combinedashstats)

combinedashstats <- combinedashstats %>% 
  group_by(species, climate, harvest, rep) %>% 
  summarise(meanagb_gm2 = sum(meanagb_gm2),
            relagb = sum(relagb),
            meanagb_Mgha = sum(meanagb_Mgha))

# ANOVAs for ash biomass/rel. biomass @ year 100 -------------------------------

ashsp2 <- c("fraxamer", "fraxnigr")
#twowayaov(df=combinedashstats, 
#          spselect=ashsp2,
#          resp=names(combinedashstats)[7] #trying this to get at the right var name
#          )

#ok, this function is not working currently, so let's just try it manually:
allfram_year100agb <- aov(meanagb_Mgha ~ harvest*climate, 
                          data=combinedashstats[combinedashstats$species=="fraxamer",])
summary(allfram_year100agb)
print(TukeyHSD(allfram_year100agb))
#RESULT: main effect of mgmt is signif across climate regimes
#(and climates pairwise not *all* signif dif)

#now let's try for black ash:
allfrni_year100agb <- aov(meanagb_Mgha ~ harvest*climate, 
                          data=combinedashstats[combinedashstats$species=="fraxnigr",])
summary(allfrni_year100agb)
TukeyHSD(allfrni_year100agb)
#ONLY retention has signif dif (since black ash weren't really salvage logged)
#buttt also so does climate

#also look at the SUMMARY STATS!:
combinedashstats %>% 
  group_by(species, harvest, climate) %>% 
  summarise(mean_relagb = mean(relagb),
            se_relagb = std.error(relagb),
            mean_agb_Mgha = mean(meanagb_Mgha),
            se_agb_Mgha = std.error(meanagb_Mgha))


# year 100 occurrence of ash (# cells present) -------------------------------

# MAYBE NEED TO CREATE ANOTHER MASTER DF OF ALL CELL VALS FOR ASH SP ACROSS 
#SCENARIOS????
#because then I can also use that to summarize BY MANAGEMENT AREA......


#using the functions from my initial_community_map_exploration script

#ADD VAR HERE FOR COLUMN OF ASH OCCURRENCE TO FILL IN W/ LOOP
  
#### NEED TO FIX THIS LOOP!!!!!!!!!!!! ###

#doing this first for COMBINED ash sp. ('regular' and treated ash together);
#NEED TO FIGURE OUT HOW TO DO THIS IN THE LOOP SO WE AREN'T 'DOUBLE COUNTING' CELLS
#where both 'regular' and 'treated' ash are coexisting.

for(k in rcp){
  for(j in (gcm)){
    for(i in mgmt){ 
      filestring <- paste(workdir, k, j, i, "output/AGbiomass", sep="/")
      
      rasterstack <- readinspagb(specieslist=ashsp, parent=filestring, year=100)
      for(h in 1:length(ashsp)){
        numashsites <- sum(values(rasterstack[[h]], mat=FALSE)>0)
        


        #then append this to output dataframe:
        #ashoccurrenceyear100 <- rbind(ashoccurrence, addrow)
        
      }
    }
  }
}



# look at AGE DISTRIBUTION of ash??? (if possible...) -------------------------------





# assess ash variables BY MANAGEMENT AREA -------------------------------


