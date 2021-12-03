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