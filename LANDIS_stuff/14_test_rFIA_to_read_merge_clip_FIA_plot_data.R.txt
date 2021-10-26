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

# Update study_area_prefix to a character string for output files specific to study area
study_area_prefix <- 'N-NH'

# Update My folder that I want to download FIA data into...update this to your directory structure.
my_fia_folder <- 'C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_download\\FIA'
# Update directories to write shapefiles or output data tables to
gis_dir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\gis"
out_data_dir <- "C:\\Users\\janer\\Dropbox\\Projects\\Inspires\\data_processed"

# This next command will download FIA data for the specified states.
# You only need to run this the first time. Comment it out unless you want to re-download.
#getFIA(c('ME'),#c('NH','VT'), 
#      dir = my_fia_folder,
#       common = T,
#       load = T,
#       nCores = nCores_my_computer) # 

# Set common State FPs
state_fps = c(50,33,23)
state_abr = c("VT","NH","ME")

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

# Compute biomass using rFIA method by species and plot
agb <- biomass(dbsub, returnSpatial = T, bySpecies = T, landType = 'forest', 
               treeType = 'live',
               method = 'TI', treeDomain = DIA > 5, totals = F, 
               byPlot = T, nCores = nCores_my_computer)

# Compute trees per acre (TPA) and basal area per acre (BAA) using rFIA method
tpa <- tpa(dbsub, returnSpatial = T, bySpecies = T, landType = 'forest', 
           treeType = 'live',
               method = 'TI', treeDomain = DIA > 5, totals = F, 
           byPlot = T, nCores = nCores_my_computer)

# New Compute trees per acre (TPA)  using seedling rFIA method
seed <- seedling(dbsub, returnSpatial = T, bySpecies = T, landType = 'forest',
           method = 'TI', treeDomain = NULL, totals = F, 
           byPlot = T, nCores = nCores_my_computer)

# New Compute trees per acre (TPA)  using for saplings (<= 5 in DBH) rFIA method
sap <- tpa(dbsub, returnSpatial = T, bySpecies = T, landType = 'forest', 
           treeType = 'live',
           method = 'TI', treeDomain = DIA <= 5, totals = F, 
           byPlot = T, nCores = nCores_my_computer)

# New computer Net tree growth using rFIA vitalRates
## Net annual change
net <- vitalRates(dbsub,
                  treeType = 'all', # "all" indicates net growth
                  #grpBy = SITECLCD, # Grouping by site productivity class
                  bySpecies = TRUE, # also grouping by species
                  landType = 'forest',
                  byPlot = T,
                  method = 'TI',
                  treeDomain = DIA > 5,
                  totals = F,
                  variance = TRUE,
                  nCores = nCores_my_computer)

# Convert english units to metric
agb$bioMgHa <- agb$BIO_ACRE * 2.2417#agb$BIO_AG_ACRE * 2.2417
tpa$treesPerHa <- tpa$TPA / 0.404686
tpa$basalAreaPerHa <- tpa$BAA / 4.356 # foot2/acre to m2/ha
sap$basalAreaPerHa <- sap$BAA / 4.356 # foot2/acre to m2/ha
seed$treesPerHa <- seed$TPA / 0.404686

# Do this for TPA and BAA in tpa data table...

# Create an 8-letter sppcode column, combining first 4 letters of genus and species.
# This method stopped working 2021-05-26. Oh well, I guess pipes are hard to debug.
#agb <- agb %>% 
#  dplyr::mutate(sppcode = stringi::stri_extract_all_words(agb$SCIENTIFIC_NAME) %>% 
#           purr::map(str_sub, 1, 4)  %>% 
#           purr::map_chr(paste, collapse = "") %>% purr::map(tolower) %>% unlist())

#tpa <- tpa %>% 
#  mutate(sppcode = stringi::stri_extract_all_words(tpa$SCIENTIFIC_NAME) %>% 
#           map(str_sub, 1, 4)  %>% 
#           map_chr(paste, collapse = "") %>% map(tolower) %>% unlist())

# Rewrite above using a different approach - create 8 letter sppcode
agb <- agb %>%
  dplyr::mutate(genus = stringi::stri_extract_first_words(SCIENTIFIC_NAME),
                species = stringi::stri_extract_last_words(SCIENTIFIC_NAME),
                sppcode = tolower(paste(substr(genus, 1, 4), substr(species, 1, 4), sep="")))
tpa <- tpa %>%
  dplyr::mutate(genus = stringi::stri_extract_first_words(SCIENTIFIC_NAME),
                species = stringi::stri_extract_last_words(SCIENTIFIC_NAME),
                sppcode = tolower(paste(substr(genus, 1, 4), substr(species, 1, 4), sep="")))
# New 
seed <- seed %>%
  dplyr::mutate(genus = stringi::stri_extract_first_words(SCIENTIFIC_NAME),
                species = stringi::stri_extract_last_words(SCIENTIFIC_NAME),
                sppcode = tolower(paste(substr(genus, 1, 4), substr(species, 1, 4), sep="")))

sap <- sap %>%
  dplyr::mutate(genus = stringi::stri_extract_first_words(SCIENTIFIC_NAME),
                species = stringi::stri_extract_last_words(SCIENTIFIC_NAME),
                sppcode = tolower(paste(substr(genus, 1, 4), substr(species, 1, 4), sep="")))


# Make a list of all the possible species codes, this may include unobserved species
spp_levels <- sort(unique(agb$sppcode))
SPCD_levels <- sort(unique(agb$SPCD))
sppcode_key <- agb %>% dplyr::select(SPCD, SCIENTIFIC_NAME, sppcode) %>% as.data.frame() %>% 
                dplyr::select(-geometry) %>% distinct() %>% arrange(sppcode)

# One plot with silver maple, sppcode conflicts with acersacc (sugar maple), drop plot
agb <- agb %>% filter(!(SPCD == 317))
tpa <- tpa %>% filter(!(SPCD == 317))
seed <- seed %>% filter(!(SPCD == 317))
sap <- sap %>% filter(!(SPCD == 317))

# Reformat table by spreading species across columns, one row per plot
spbio <- agb %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, sppcode, bioMgHa) %>% 
  tidyr::spread(., sppcode, bioMgHa, fill = 0, drop = T)
spstems <- agb %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, sppcode, nStems) %>% 
  tidyr::spread(., sppcode, nStems, fill = 0, drop = T)
spba <- tpa %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, sppcode, basalAreaPerHa) %>% 
  tidyr::spread(., sppcode, basalAreaPerHa, fill = 0, drop = T)
spseed <- seed %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, sppcode, treesPerHa) %>% 
  tidyr::spread(., sppcode, treesPerHa, fill = 0, drop = T)
spsap <- sap %>% dplyr::select(PLT_CN, YEAR, pltID, PLOT_STATUS_CD, sppcode, basalAreaPerHa) %>% 
  tidyr::spread(., sppcode, basalAreaPerHa, fill = 0, drop = T)

# Calculate which species did not occur
spp_sums <- spbio %>% as.data.frame() %>% dplyr::select(all_of(spp_levels)) %>% colSums()/nrow(spbio)
spp_unobserved <- names(spp_sums)[which(spp_sums == 0)]
spp_list <- spp_levels[-(which(spp_levels %in% spp_unobserved))]

# Remove unobserved species
spbio <- spbio %>% dplyr::select(-all_of(spp_unobserved))
spstems <- spstems %>% dplyr::select(-all_of(spp_unobserved))
spba <- spba %>% dplyr::select(-all_of(spp_unobserved))
spseed <- spseed %>% dplyr::select(-all_of(spp_unobserved))
spsap <- spsap %>% dplyr::select(-all_of(spp_unobserved))

# Prepare tables to write to output in different formats
spbio_df <- spbio %>% as.data.frame() %>% dplyr::select(-geometry)
spstems_df <- spstems %>% as.data.frame() %>% dplyr::select(-geometry)
spba_df <- spba %>% as.data.frame() %>% dplyr::select(-geometry)
spseed_df <- spseed %>% as.data.frame() %>% dplyr::select(-geometry)
spsap_df <- spsap %>% as.data.frame() %>% dplyr::select(-geometry)

# Create point file of distinct plots
plots <- agb %>% dplyr::select(PLT_CN,YEAR,pltID,PLOT_STATUS_CD) %>% distinct()
plot(plots)

# Write output data files to csv or shapefile
st_write(spbio, dsn = paste(gis_dir,"\\",study_area_prefix, "_fia_plots_sp_bio.shp", sep = ""), 
         layer = paste(gis_dir,"\\",study_area_prefix, "_fia_plots_sp_bio.shp", sep = ""), driver = "ESRI Shapefile")
write.csv(spbio_df,  paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_bio.csv", sep = ""), row.names = F)
write.csv(spstems_df,  paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_stems.csv", sep = ""), row.names = F)
write.csv(spba_df,  paste(out_data_dir,"\\",study_area_prefix, "_fia_plots_sp_ba.csv", sep = ""), row.names = F)


# Create color lookup table for biomass by species
breaks <- hist(agb$bioMgHa)$breaks


# Look at some seedling data vs. overstory
seed %>% dplyr::filter(sppcode == "picerube") %>% arrange(desc(treesPerHa)) %>% 
  data.frame() %>% head()
ploti <- "2_33_7_1844" #"2_33_7_719"

spseed_df %>% dplyr::filter(pltID == ploti)
spsap_df  %>% dplyr::filter(pltID == ploti)
spba_df %>% dplyr::filter(pltID == ploti)
