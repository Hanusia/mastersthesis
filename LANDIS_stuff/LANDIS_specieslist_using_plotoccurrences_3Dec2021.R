library(tidyverse)


#importing my + fia plot data...
myplot_data_LANDIS <- read.csv("LANDIS_stuff/Hanusiaplots_BA_sp_LANDISinit_19Nov2021.csv")

fiaplot_data_LANDIS <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/FIA/data_outputs/VT-MA_fia_plots_sp_ba.csv")

#using these commands to sum the # of non-zero values in each column (species)
myplot_sums <- colSums(myplot_data_LANDIS != 0)
#View(myplot_sums)
#glimpse(myplot_sums)

fiaplot_sums <- colSums(fiaplot_data_LANDIS != 0)
#View(fiaplot_sums)

#combining the two lists of sums & adding them together based on names (e.g. same species)
v3 <- c(myplot_sums, fiaplot_sums)
allplot_sums_added <- tapply(v3, names(v3), sum)
#View(allplot_sums_added)

#applying the same criteria as PC-ORD to create lists of which species are included and excluded
sp_included <- names(allplot_sums_added[allplot_sums_added>2])
#sp_included

sp_excluded <- names(allplot_sums_added[allplot_sums_added<3])
#sp_excluded

length(sp_excluded)
length(sp_included)
length(allplot_sums_added) # things ARE adding up...

#next step is to associate them back to their names
#and to do that, we'll need to remove the "X" from them

str_remove(string=sp_included, pattern="X") #it worked yay! I was making things too complicated for myself...


#import species code list (or look @ my old code that did this??)

#first, loading in data
FIA_species_table <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/FIA/2021 Master Species FGver9-1_9_2021_final.csv")
my_species_table <- read.csv("C:/Users/theha/OneDrive - University of Vermont/Ash project/EAB_project_2020_additional_data.csv", fileEncoding = "UTF-8-BOM")

#now generate list from this...
spnames_included <- FIA_species_table[FIA_species_table$FIA.Code %in% str_remove(string=sp_included, pattern="X"), 
                                      c("FIA.Code", "Common.Name", "Genus", "Species")]
spnames_included
nrow(spnames_included)
#MISSING FROM THIS IS "SPRUCE SPECIES" (IF we want to include it...there's only 3...I think it's OK to exclude)
#maybe ask Jane about this??

#now do the same for EXCLUDED species
spnames_excluded <- FIA_species_table[FIA_species_table$FIA.Code %in% str_remove(string=sp_excluded, pattern="X"), 
                                      c("FIA.Code", "Common.Name", "Genus", "Species")]
spnames_excluded
nrow(spnames_excluded)

#test for overlap...
intersect(spnames_included$FIA.Code, spnames_excluded$FIA.Code)
#cool!

#export list of included species to send to Jane
write.csv(x=spnames_included, file="LANDIS_stuff/species_list_for_LANDIS_initialcommunities_3Dec2021.csv")
#ADDED spruce spp to the actual file!
#final steps:
#backup github,
#update workflow doc,
#and finally, email Jane my species list

#### update 1/4/2022:
#refine this list further based on additional criteria!!
#in my first round of refinement (w/in an updated csv), I excluded:
#spruce spp., apple spp., American hornbeam, and serviceberry

#now, want to do some prioritization based on mean/relative BA!
#first, need to ADD my plots' data to FIA plots...yikes this might take a while
#maybe use rFIA package for this?
#or try to merge the existing "wide format" data sets together?
#the latter is probably the best idea...or maybe even just do them separately
#and don't worry about merging them!
#in the end, this is going to come down to some subjective decisions anyway!

View(myplot_data_LANDIS)
myplot_BA <- data.frame(spname = names(myplot_data_LANDIS[,-c(1,2, 31, 32)]), #column names AKA species codes, 
                           #excluding the first two cols of number and plot ID, and last 2 of lat/long
                           sumBA = colSums(myplot_data_LANDIS[,-c(1,2, 31, 32)]), #summing total BA per sp
                           meanBA = colMeans(myplot_data_LANDIS[,-c(1,2, 31, 32)])) #calculating MEAN BA per sp (per plot)
View(myplot_BA)

myplot_BA$sumBA_rel <- myplot_BA$sumBA/sum(myplot_BA$sumBA)*100 #calculating RELATIVE total BA by dividing each row by its total
#now this is represented in PERCENTAGE since I multiplied by 100
#next step: list species that fall under 1%

myplot_sumBA_rel_low <- myplot_BA$spname[myplot_BA$sumBA_rel<1]
#this produces a list of species 

#now doing the same with relative MEAN BA:
myplot_BA$meanBA_rel <- myplot_BA$meanBA/sum(myplot_BA$meanBA)*100 #calculating RELATIVE total BA by dividing each row by its total
#now this is represented in PERCENTAGE since I multiplied by 100
#WHY did these numbers come out exactly the same as above?? seems wrong...
#I guess they do end up the same...hmm.....(deleted lines where I test this manually)

#OK now doing the same w/ FIA data:
View(fiaplot_data_LANDIS)
fiaplot_BA <- data.frame(spname = names(fiaplot_data_LANDIS[,-c(1,2,3,4)]), #column names AKA species codes, 
                        #excluding the first two cols of number and plot ID, and last 2 of lat/long
                        sumBA = colSums(fiaplot_data_LANDIS[,-c(1,2,3,4)]), #summing total BA per sp
                        meanBA = colMeans(fiaplot_data_LANDIS[,-c(1,2,3,4)])) #calculating MEAN BA per sp (per plot)
View(fiaplot_BA)

#calculating RELATIVE total BA by dividing each row by its total
fiaplot_BA$sumBA_rel <- fiaplot_BA$sumBA/sum(fiaplot_BA$sumBA)*100 
#now this is represented in PERCENTAGE since I multiplied by 100
#next step: list species that fall under 1%

fiaplot_sumBA_rel_low <- fiaplot_BA$spname[fiaplot_BA$sumBA_rel<1]
#this produces a list of species 
nrow(fiaplot_BA)
length(fiaplot_sumBA_rel_low)
#so this would basically exclude a majority of the species...
#is that OK??

#NOW, I need to cross-reference these two lists with the species list I am working with currently!

#importing my working/in-progress species list:
working_splist <- read.csv("LANDIS_stuff/refined_species_list_for_LANDIS_initialcommunities_4Jan2022.csv")
View(working_splist)

#now need to remove Xs from names in lists of those under 1%
myplot_sumBA_rel_low <- str_remove(string=myplot_sumBA_rel_low, pattern="X")
fiaplot_sumBA_rel_low <- str_remove(string=fiaplot_sumBA_rel_low, pattern="X")

#convert to character so they are comparable
working_splist_codes <- as.character(working_splist$FIA.Code)
#working_splist_codes

#now create a for loop to look for my working list in either of these lists:
for (i in 1:length(working_splist_codes)){
  #OK, first new col created: T/F if my species is PRESENT in FIA data in region
  working_splist$FIA_present[i] <- working_splist_codes[i] %in% 
    str_remove(string=fiaplot_BA$spname, pattern="X")
  #second col: T/F if my species is RARE in FIA data in my region
  working_splist$FIA_lowrel[i] <- working_splist_codes[i] %in% 
    fiaplot_sumBA_rel_low
  #third col: T/F if my species is PRESENT in my plot data in region
  working_splist$my_present[i] <- working_splist_codes[i] %in% 
    str_remove(string=myplot_BA$spname, pattern="X")
  #fourth col: T/F if my species is RARE in my plot data in region
  working_splist$my_lowrel[i] <- working_splist_codes[i] %in% 
    myplot_sumBA_rel_low
}

#View(working_splist)

#where TRUE is the case for both: DEFINITELY exclude!
#where FALSE is true for both: DEFINITELY include
#where TRUE is one and FALSE is other- consider on a species-by-species basis

#also need to consider if, when answer is 'FALSE,' if that species was even PRESENT in the dataset to begin with!

#now need to set up another column with final determinations:

for(i in 1:nrow(working_splist)){
  #first determination: if both are ABSENT, don't need to consider abundance
  if(working_splist$FIA_present[i]==FALSE & 
     working_splist$my_present[i]==FALSE) {
    working_splist$status[i] <- "absent in both"
    working_splist$decision[i] <- "exclude"
    }
  #next possibility: absent in one dataset & abundant in the other
   else if((working_splist$FIA_present[i]==FALSE & #this combo: absent in FIA/abundant in my data
            working_splist$my_present[i]==TRUE & 
          working_splist$my_lowrel[i]==FALSE) | 
          (working_splist$my_present[i]==FALSE & #or this combo: absent in my data/abundant in FIA
           working_splist$FIA_present[i]==TRUE & 
           working_splist$FIA_lowrel[i]==FALSE)) {
     working_splist$status[i] <- "absent in one, abundant in other"
     working_splist$decision[i] <- "include"
     }
  #next possibility: absent in one set, rare in the other
  else if((working_splist$FIA_present[i]==FALSE & #this combo: absent in FIA/rare in my data
           working_splist$my_present[i]==TRUE & 
           working_splist$my_lowrel[i]==TRUE) | 
          (working_splist$my_present[i]==FALSE & #or this combo: absent in my data/rare in FIA
           working_splist$FIA_present[i]==TRUE & 
           working_splist$FIA_lowrel[i]==TRUE)) {
    working_splist$status[i] <- "absent in one, rare in other"
    working_splist$decision[i] <- "exclude"
  }
  #next possibility: rare in both
  else if(working_splist$FIA_lowrel[i]==TRUE & 
          working_splist$my_lowrel[i]==TRUE) {
    working_splist$status[i] <- "rare in both"
    working_splist$decision[i] <- "exclude"
  }
  #next possibility: abundant in both
  else if(working_splist$FIA_present[i]==TRUE &
          working_splist$FIA_lowrel[i]==FALSE & 
          working_splist$my_present[i]==TRUE &
          working_splist$my_lowrel[i]==FALSE) {
    working_splist$status[i] <- "abundant in both"
    working_splist$decision[i] <- "include"
  }
  #last (I think!!) possibility: abundant in one, rare in other
  else if((working_splist$FIA_lowrel[i]==TRUE & #this combo: rare in FIA/abundant in my data
           working_splist$my_present[i]==TRUE & 
           working_splist$my_lowrel[i]==FALSE) | 
          (working_splist$my_lowrel[i]==TRUE & #or this combo: rare in my data/abundant in FIA
           working_splist$FIA_present[i]==TRUE & 
           working_splist$FIA_lowrel[i]==FALSE)) {
    working_splist$status[i] <- "abundant in one, rare in other"
    working_splist$decision[i] <- "include"
  }
}

#wooohooo it worked!!
summary(as.factor(working_splist$status))
summary(as.factor(working_splist$decision))
View(working_splist)
