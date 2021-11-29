# --------------------------------------------------
# rFIA data for my initial communities LANDIS
# 23 Nov 2021
# HH
# --------------------------------------------------
#

## CODE BELOW COPIED + MODIFIED FROM JANE FOSTER, & SCRIPT test_rFIA_to_read_merge_clip_FIA_plot_data_1Sept2021
##############################################

library(rFIA)
library(parallel)
library(sf)
library(tidyverse)
library(stringi)
library(dplyr)

# Detect how many cores are available on current computer.
detectCores(all.tests = F, logical = T)
# Update number of cores on your computer to run this code. This should be a subset of cores available.
nCores_my_computer <- 4
#MY computer has 8 cores available, so I will stick with 4 to run this code.

# Update study_area_prefix to a character string for output files specific to study area
study_area_prefix <- 'VT-MA' #going with this prefix for now

# Update My folder that I want to download FIA data into...update this to your directory structure.
my_fia_folder <- 'C:\\Users\\theha\\Documents\\layers_for_LANDIS\\FIA\\FIA_data'
# Update directories to write shapefiles or output data tables to
gis_dir <- 'C:\\Users\\theha\\Documents\\layers_for_LANDIS\\FIA\\gis_outputs'
out_data_dir <- 'C:\\Users\\theha\\Documents\\layers_for_LANDIS\\FIA\\data_outputs'

# This next command will download FIA data for the specified states.
# You only need to run this the first time. Comment it out unless you want to re-download.
#getFIA(c('VT', 'MA'), #I have now downloaded for VT and MA so commenting out for future runs
#      dir = my_fia_folder,
#       common = T,
#       load = T,
#       nCores = nCores_my_computer) # 

# Set common State FPs
state_fps = c(50,25) #find FP numbers at: https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
state_abr = c("VT","MA")

# Read in shapefile of county boundaries for study area
cntysub <- st_read(paste(gis_dir,"\\",study_area_prefix, "_counties.shp", sep = ""))

# Determine relevant states and abreviations to sample FIA data overlapping counteis
stateFps_here = unique(cntysub$STATEFP)
stateAbr_here = state_abr[which(state_fps %in% stateFps_here)]

# Read FIA data for the states you downloaded
db <- readFIA(my_fia_folder, common = T,
              states = stateAbr_here, nCores = nCores_my_computer)

# Subset the state databases to counties in your shapefile
dbsub <- clipFIA(db, mask = cntysub, matchEval = T, designCD = c('1'), nCores = nCores_my_computer)

#don't need biomass so commenting out that part

# Compute trees per acre (TPA) and basal area per acre (BAA) using rFIA method
#changing this code to include trees above 1 in diameter, not just 5 in!
#I THINK this is fine and will just give me BA in sq. ft/acre, 
#which the next line below can convert to sqm/ha
tpa <- tpa(dbsub, returnSpatial = T, bySpecies = T, landType = 'forest', 
           treeType = 'live',
           method = 'TI', treeDomain = DIA >= 1, totals = F, 
           byPlot = T, nCores = nCores_my_computer)

# New Compute trees per acre (TPA)  using for saplings (<= 5 in DBH) rFIA method
#sap <- tpa(dbsub, returnSpatial = T, bySpecies = T, landType = 'forest', 
#           treeType = 'live',
#           method = 'TI', treeDomain = DIA <= 5, totals = F, 
#           byPlot = T, nCores = nCores_my_computer)
#I don't think I need this part (above), but just commenting it out for now in case I do

# Convert english units to metric
#agb$bioMgHa <- agb$BIO_ACRE * 2.2417#agb$BIO_AG_ACRE * 2.2417
#tpa$treesPerHa <- tpa$TPA / 0.404686
tpa$basalAreaPerHa <- tpa$BAA / 4.356 # foot2/acre to m2/ha
#sap$basalAreaPerHa <- sap$BAA / 4.356 # foot2/acre to m2/ha
#seed$treesPerHa <- seed$TPA / 0.404686
#ditto re: commenting out 

#CURRENTLY AT THIS POINT

# Create an 8-letter sppcode column, combining first 4 letters of genus and species.
# This method stopped working 2021-05-26. Oh well, I guess pipes are hard to debug.
# Hanusia update: It seems to work now, the tidyverse package is called "purrr" instead of "purr" (3 Rs...)

tpa <- tpa %>% 
  mutate(sppcode = stringi::stri_extract_all_words(tpa$SCIENTIFIC_NAME) %>% 
           map(str_sub, 1, 4)  %>% 
           map_chr(paste, collapse = "") %>% map(tolower) %>% unlist())
#don't actually need this since we're using species # codes instead of name codes,
#but leaving it in bc why not...

#section between bars below: commented out b/c I don't think I want/need it for this part,
#but leaving it in just in case
##############################################
# Make a list of all the possible species codes, this may include unobserved species
#spp_levels <- sort(unique(agb$sppcode))
#SPCD_levels <- sort(unique(agb$SPCD))
#sppcode_key <- agb %>% dplyr::select(SPCD, SCIENTIFIC_NAME, sppcode) %>% as.data.frame() %>% 
#  dplyr::select(-geometry) %>% distinct() %>% arrange(sppcode)

#NEED TO LOOK @ THIS MORE IN DEPTH (FROM ABOVE CODE) TO SEE IF SPECIES CONFLICT??
#update: after filtering thru the tpa dataframe, it appears this is still the case (e.g. silver maple only in 1 plot)
#so I will keep the below code as-is

# One plot with silver maple, sppcode conflicts with acersacc (sugar maple), drop plot
#agb <- agb %>% filter(!(SPCD == 317))
#tpa <- tpa %>% filter(!(SPCD == 317))
#seed <- seed %>% filter(!(SPCD == 317))
#sap <- sap %>% filter(!(SPCD == 317))
##############################################

# Reformat table by spreading species across columns, one row per plot
spba <- tpa %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, SPCD, basalAreaPerHa) %>% 
  tidyr::spread(., SPCD, basalAreaPerHa, fill = 0, drop = T)
#spsap <- sap %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, sppcode, basalAreaPerHa) %>% 
#  tidyr::spread(., sppcode, basalAreaPerHa, fill = 0, drop = T)
#this part, just modified to use the SPCD column instead of sppcode b/c we want the numeric species code

#once again, don't think I need the part below so just commenting it out
##############################################
# Calculate which species did not occur
#spp_sums <- spbio %>% as.data.frame() %>% dplyr::select(all_of(spp_levels)) %>% colSums()/nrow(spbio)
#spp_unobserved <- names(spp_sums)[which(spp_sums == 0)]
#spp_list <- spp_levels[-(which(spp_levels %in% spp_unobserved))]

# Remove unobserved species
#spbio <- spbio %>% dplyr::select(-all_of(spp_unobserved))
#spstems <- spstems %>% dplyr::select(-all_of(spp_unobserved))
#spba <- spba %>% dplyr::select(-all_of(spp_unobserved))
#spseed <- spseed %>% dplyr::select(-all_of(spp_unobserved))
#got an error here BUT I think it's OK...just some of the unobserved species don't exist in this df (which makes sense)
#spsap <- spsap %>% dplyr::select(-all_of(spp_unobserved))
##############################################

# Prepare tables to write to output in different formats
#spbio_df <- spbio %>% as.data.frame() %>% dplyr::select(-geometry)
#spstems_df <- spstems %>% as.data.frame() %>% dplyr::select(-geometry)
spba_df <- spba %>% as.data.frame() %>% dplyr::select(-geometry)
#spseed_df <- spseed %>% as.data.frame() %>% dplyr::select(-geometry)
#spsap_df <- spsap %>% as.data.frame() %>% dplyr::select(-geometry)

# Create point file of distinct plots
#plots <- agb %>% dplyr::select(PLT_CN,YEAR,pltID,PLOT_STATUS_CD) %>% distinct()
#plot(plots)

# Write output data files to csv or shapefile
st_write(spbio, dsn = paste(gis_dir,"\\",study_area_prefix, "_fia_plots_sp_bio.shp", sep = ""), 
         layer = paste(gis_dir,"\\",study_area_prefix, "_fia_plots_sp_bio.shp", sep = ""), driver = "ESRI Shapefile")
#also an error here-but it seems OK??? (e.g. the shapefile DID output to the folder)

#write.csv(x=spbio_df,  file=paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_bio.csv", sep = ""), row.names = F)
#write.csv(spstems_df,  paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_stems.csv", sep = ""), row.names = F)
write.csv(spba_df,  paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_ba.csv", sep = ""), row.names = F)

#ok this looks good, I just need to attach plot lat and long to it as well...
#looks like I want to use the dbsub$PLOT database/dataframe (?) to do that
#so gonna execute that with a merge
#and based on the FIA data documentation, it appears the field I want to link/merge by
#is CN in the PLOT table and PLT_CN in my existing Df
spba_df_latlong <- merge(x=spba_df, y=dbsub$PLOT[,c("CN", "LAT", "LON")], #just keeping the ID column + lat & long info for each row
                         all.x=TRUE, #b/c we want to keep each row of the existing DF, but not necessarily each row of PLOT if not needed
                         by.x="PLT_CN", by.y="CN") #IDing the unique columns that link up

names(spba_df_latlong)
head(spba_df_latlong)
#it worked!

#now to save THIS as a dataframe:
write.csv(spba_df_latlong,  paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_ba_latlong.csv", sep = ""), row.names = F)


##############################################
# Look at some seedling data vs. overstory
#seed %>% dplyr::filter(sppcode == "picerube") %>% arrange(desc(treesPerHa)) %>% 
#  data.frame() %>% head()
#ploti <- "2_33_7_1844" #"2_33_7_719"

#spseed_df %>% dplyr::filter(pltID == ploti)
#spsap_df  %>% dplyr::filter(pltID == ploti)
#spba_df %>% dplyr::filter(pltID == ploti)
