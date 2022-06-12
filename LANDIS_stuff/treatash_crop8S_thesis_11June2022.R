# --------------------------------------------------
# treated ash placement on subset landscape for thesis
# 11 Jun 2022
# HH
# --------------------------------------------------
#

# goal = "place" treated ash (white and black) onto my subset landscape in 
# northern Bennington County (aka "crop8S") for MS thesis LANDIS model runs.

# white ash code for LANDIS = fraxamer
# treated white ash code = fraxamtx
# black ash code for LANDIS = fraxnigr
# treated black ash code = fraxnitx

# NOTE: some of the code below will be copied/modified from: 
# initial_community_map_exploration_29may2022.R
# which is where I started the initial/exploratory steps of this process.

#load packages
library(terra)
library(tidyverse)

#read in key files:
#first is the raw text file of init communities, second is in dataframe format
init_comm_txt <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/Initial_communities_VT-NH-MA_2022-05-27.txt")
init_comm_df <- read.csv("LANDIS_stuff/initcomms_dataframe.csv")

#read in subset region (crop8S) rasters in a stack:
layernames2 <- c("mask", "initclass", "initcomm", "ecoregions", "mgmtareas", "stands")
for(l in 1:length(layernames2)){
  newlayer <- rast(paste0("LANDIS_stuff/crop8S_rasters/", layernames2[l], "_crop8S", ".tif"))
  if(l==1){crop8s <- rast(newlayer, nlyrs=6)}
  crop8s <- c(crop8s, newlayer)
}

crop8s
crop8s_vals <- values(crop8s, dataframe=TRUE)
View(crop8s_vals)
names(crop8s_vals) <- layernames2

# finding white ash stands  -------------------------------

framstands_crop8S <- data.frame("stand"=unique(crop8s_vals$stands),
                                "num_plots"=rep(0),
                                "num_ash_plots"=rep(0))
View(framstands_crop8S)
#removing zeros:
framstands_crop8S <- framstands_crop8S[-1,]
crop8s_vals_nozero <- crop8s_vals[crop8s_vals$stands!=0,]

#(re)making this function:
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#creating 'whiteash_codes' variable
whiteash_codes <- init_comm_df$mapcode[init_comm_df$species=="fraxamer"]
head(whiteash_codes)

for(i in 1:nrow(crop8s_vals_nozero)){
  j <- crop8s_vals_nozero$stand[i]
  #if stand values match up, add 1 to total plot count.
  framstands_crop8S$num_plots[framstands_crop8S$stand==j] %+=% 1
  #and if THAT is true, ALSO tally how many of those plots have ash...
  if(crop8s_vals_nozero$initcomm[i] %in% whiteash_codes){
    framstands_crop8S$num_ash_plots[framstands_crop8S$stand==j] %+=% 1
  }
}
View(framstands_crop8S)

#next steps will be finding PROPORTION of ash cells in stands.
#but also, first filtering by stands of a certain SIZE (~10 acres/4 ha?):
framstands2_crop8S <- framstands_crop8S[framstands_crop8S$num_plots>=45,]
View(framstands2_crop8S)
framstands2_crop8S$prop_ash <- framstands2_crop8S$num_ash_plots/framstands2_crop8S$num_plots

framstands2_crop8S <- framstands2_crop8S[framstands2_crop8S$prop_ash>=0.15,]

framstands2_crop8S$mgmtarea <- trunc(framstands2_crop8S$stand/10000000)
framstandsperma_crop8S <- table(framstands2_crop8S$mgmtarea)
framstandsperma_crop8S
#okay, SO:
#there are 3,768 unique stands in my subset area.
#of those stands, there are 324 that fit the following criteria:
#1) stand is at least 4 ha/10 acres in size (num_plots>45)
#2) at least 15% of the stand's cells include ash (prop_ash>15)
#and of those 324 stands, 
#140 are private/family-owned,
#4 are corporate-owned,
#77 are private-other (NGO etc.) owned,
#71 are national forest owned,
#4 are public-state/local-owned,
#and 28 are in roadside ecoregions.

#next, let's look at the size (area in ha) of those stands, per ma.
framstands2_crop8S$area_ha <- framstands2_crop8S$num_plots*0.09 #each cell=0.09 ha
#?aggregate()

#using this handy tidyverse function to summarize what I want for each MA:
#number of ash stands, total size of those stands (in ha), 
#and avg size of those stands (in ha)
framstands_crop8s_byma <- framstands2_crop8S %>% 
  group_by(mgmtarea) %>% 
  summarise(numstands = n(),
            totalha = sum(area_ha),
            meanha = mean(area_ha),
            numashplots = sum(num_ash_plots))

framstands_crop8s_byma
#also adding the name for each category to add context:
framstands_crop8s_byma$owntype <- 
  c("priv_fam", "priv_corp", "priv_other", "pub_fed", "pub_state", "roadside")
View(framstands_crop8s_byma)

# finding black ash stands -------------------------------

frnistands_crop8S <- data.frame("stand"=unique(crop8s_vals$stands),
                                "num_plots"=rep(0),
                                "num_ash_plots"=rep(0))
#View(frnistands_crop8S)
#removing zeros:
frnistands_crop8S <- frnistands_crop8S[-1,]
crop8s_vals_nozero <- crop8s_vals[crop8s_vals$stands!=0,]

#creating 'blackash_codes' variable
blackash_codes <- init_comm_df$mapcode[init_comm_df$species=="fraxnigr"]
head(blackash_codes)

for(i in 1:nrow(crop8s_vals_nozero)){
  j <- crop8s_vals_nozero$stand[i]
  #if stand values match up, add 1 to total plot count.
  frnistands_crop8S$num_plots[frnistands_crop8S$stand==j] %+=% 1
  #and if THAT is true, ALSO tally how many of those plots have ash...
  if(crop8s_vals_nozero$initcomm[i] %in% blackash_codes){
    frnistands_crop8S$num_ash_plots[frnistands_crop8S$stand==j] %+=% 1
  }
}
View(frnistands_crop8S)

#next steps will be finding PROPORTION of ash cells in stands.
#but also, first filtering by stands of a certain SIZE (~10 acres/4 ha?):
frnistands2_crop8S <- frnistands_crop8S[frnistands_crop8S$num_plots>=45,]
View(frnistands2_crop8S)
frnistands2_crop8S$prop_ash <- frnistands2_crop8S$num_ash_plots/frnistands2_crop8S$num_plots

frnistands2_crop8S <- frnistands2_crop8S[frnistands2_crop8S$prop_ash>=0.15,]
#UPDATE: there's only ONE stand that fits the criteria for black ash
#(at least 10 acres/4 ha, and at least 15% of cells contain black ash)
#SO, instead, let's pick a different criteria for this one:

#first, find how many have ANY black ash:
nrow(frnistands_crop8S) 
nrow(frnistands_crop8S[frnistands_crop8S$num_ash_plots>0,]) #346 of them.
hist(frnistands_crop8S$num_ash_plots)

#so, let's redefine what a 'black ash stand' is...
frnistands2_crop8S <- frnistands_crop8S[frnistands_crop8S$num_ash_plots>0,]
frnistands2_crop8S$prop_ash <- frnistands2_crop8S$num_ash_plots/frnistands2_crop8S$num_plots
#let's see what we're working with...
hist(frnistands2_crop8S$num_ash_plots)
hist(frnistands2_crop8S$num_ash_plots[frnistands2_crop8S$num_ash_plots>1])
#basically, they are skewed towards verrrry few cells w/ black ash per stand...
nrow(frnistands_crop8S[frnistands_crop8S$num_ash_plots>1,]) #only 124 stands w/ @ least 2 black ash cells
nrow(frnistands_crop8S[frnistands_crop8S$num_ash_plots>=5,]) #and only 21 w/ @ least 5 black ash cells LOL.

#now, let's see what they are like in terms of management area...
frnistands2_crop8S$mgmtarea <- trunc(frnistands2_crop8S$stand/10000000)
frnistandsperma_crop8S <- table(frnistands2_crop8S$mgmtarea)
frnistandsperma_crop8S
#okay, SO:
#there are 3,768 unique stands in my subset area.
#of those, the ones with at least 1 cell of black ash include:
#224 in the private management class and 38 in the nonprofit management class.
#alsooo, interestingly, 71 are in the roadside management area...
#so that makes me think I should add black ash to the roadside salvage prescription
#in the harvest extension (follow up on this elsewhere)

#next, let's look at the size (area in ha) of those stands, per ma.
frnistands2_crop8S$area_ha <- frnistands2_crop8S$num_plots*0.09 #each cell=0.09 ha
#?aggregate()

#using this handy tidyverse function to summarize what I want for each MA:
#number of ash stands, total size of those stands (in ha), 
#and avg size of those stands (in ha)
frnistands_crop8s_byma <- frnistands2_crop8S %>% 
  group_by(mgmtarea) %>% 
  summarise(numstands = n(),
            totalha = sum(area_ha),
            meanha = mean(area_ha),
            numashplots = sum(num_ash_plots))

frnistands_crop8s_byma
#also adding the name for each category to add context:
frnistands_crop8s_byma$owntype <- 
  c("priv_fam", "priv_corp", "priv_other", "pub_fed", "pub_state", "roadside")
#View(frnistands_crop8s_byma)

#oh, and I also want to know: how many stands have both white AND black ash?
sum(whiteash_codes %in% blackash_codes)
sum(framstands2_crop8S$stand[framstands2_crop8S$mgmtarea==1] %in% 
      frnistands2_crop8S$stand) #74 stands in private ownership type
sum(framstands2_crop8S$stand[framstands2_crop8S$mgmtarea==3] %in% 
      frnistands2_crop8S$stand) #9 in NGO ownership type...

#REMEMBER the mgmt areas where I'm incorporating these treatments
#DIFFER for white vs black ash!!!


# generating new map codes containing treated ash -------------------------------

#basically gonna take all the mapcodes currently containing ash,
#create a NEW one that replaces it with "fraxamtx" or "fraxnitx,"
#and then append those initial community codes to the end of the doc.

max(init_comm_df$mapcode) 
#max code currently is 10366
#and some of the white ash codes are as small as 1...
#soooo, maybe start at 11,000 and just go up from there?
#might do the black ash ones first, since there are only five of them lol

View(init_comm_txt)

#blackash_codes_treated <- 11000 + c(1:length(blackash_codes))
#whiteash_codes_treated <- 11000 + length(blackash_codes) + c(1:length(whiteash_codes))
#looks great!
#HOWEVER, there are four black ash codes that are ALSO white ash codes.....
#maybe just need to make a NEW variable that includes ALL stands with 
#white and/or black ash??

ash_codes <- init_comm_df$mapcode[init_comm_df$species=="fraxamer" | init_comm_df$species=="fraxnigr"]
length(ash_codes) #742
#just confirmed that, in fact, FOUR of the black ash codes are ALSO white ash codes.
#so yeah, this one did include duplicates due to how I set up the function...
#maybe just use unique() to get the correct list?
ash_codes <- unique(ash_codes)
length(ash_codes) #alright NOW this is right!! yay :) 
#zero-ing out the variables I made above so  Idon't accidentally use them for something...
#whiteash_codes_treated <- 0
#blackash_codes_treated <- 0

ash_codes_treated <- 11000 + c(1:length(ash_codes))
View(as.data.frame(ash_codes_treated))

#now create a df that links them TOGETHER:
ashcodes_key <- data.frame("orig_mapcode"=ash_codes,
                           "treated_mapcode"=ash_codes_treated)
View(ashcodes_key)

#OK, now just need to figure out how to generate new init community codes from this...
#I guess kinda reverse-engineer what I did to make the init comm dataframe?
#basically do a loop and use the number of species/rows per mapcode as a variable
#to append them all as a text file?
#and maybe the FIRST step is just to generate a new initcomm dataframe
#with the "replacement" ash.

init_comm_df_treatedash <- init_comm_df[init_comm_df$mapcode %in% ash_codes,]
(length(unique(init_comm_df_treatedash$mapcode)))==nrow(ashcodes_key) #checking
View(init_comm_df_treatedash)

#now to replace the mapcodes:
for(i in 1:nrow(ashcodes_key)){
  init_comm_df_treatedash$mapcode[init_comm_df_treatedash$mapcode==ashcodes_key$orig_mapcode[i]] <- 
    ashcodes_key$treated_mapcode[i]
  #basically saying, when your orig map code matches this one in our list,
  #replace it with the corresponding TREATED mapcode.
}

#check to make sure this looks OK:
ashcodes_key[ashcodes_key$orig_mapcode==1,]
init_comm_df[init_comm_df$mapcode==1,]
init_comm_df_treatedash[init_comm_df_treatedash$mapcode==11001,]
#looks good!

#now to replace those ash with their treated counterparts:
nrow(init_comm_df_treatedash[init_comm_df_treatedash$species=="fraxamer",])
init_comm_df_treatedash$species[init_comm_df_treatedash$species=="fraxamer"] <- "fraxamtx"
nrow(init_comm_df_treatedash[init_comm_df_treatedash$species=="fraxamer",])
nrow(init_comm_df_treatedash[init_comm_df_treatedash$species=="fraxamtx",])

#nice! now do the same with black ash:
nrow(init_comm_df_treatedash[init_comm_df_treatedash$species=="fraxnigr",])
init_comm_df_treatedash$species[init_comm_df_treatedash$species=="fraxnigr"] <- "fraxnitx"
nrow(init_comm_df_treatedash[init_comm_df_treatedash$species=="fraxnigr",])
nrow(init_comm_df_treatedash[init_comm_df_treatedash$species=="fraxnitx",])
#looks good!

#NEXT step = CONVERT these init community mapcodes to a text file output format.

test_txtfile <- "    "
for(i in 1:nrow(ashcodes_key)){
  #for each unique mapcode that includes (treated) ash,
  mapcode <- paste("MapCode", ashcodes_key$treated_mapcode[i])
  
  test_txtfile <- rbind(test_txtfile, "   ", mapcode)
  
  #NEED TO FIGURE OUT THIS LOOP BETTER-- NOT READY!!!!
  for(j in 1:length(init_comm_df_treatedash$mapcode[init_comm_df_treatedash$mapcode==
                                                    ashcodes_key$treated_mapcode[i]])){
    subset <- init_comm_df_treatedash[init_comm_df_treatedash$mapcode==
                                                ashcodes_key$treated_mapcode[i],]
    spline <- paste(subset$species[j], subset$age[j])
    
    test_txtfile <- rbind(test_txtfile, spline)
  }
}

View(test_txtfile)

#this SEEMS right...?
#now to append onto the existing init communities file:

#write.table(x=init_comm_txt, file="LANDIS_stuff/initial_communities_ashtx_11June2022.txt",
#            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
#write.table(x=test_txtfile, file="LANDIS_stuff/initial_communities_ashtx_11June2022.txt",
#            quote=FALSE, append=TRUE, row.names=FALSE, col.names = FALSE)

#actually I do NOT like how that removed spaces from orig part of init comm file...
#gonna just manually copy + paste the original part of it, then append the tx ash mapcodes.

#ok now using actually the same code as above to append the treated ash mapcodes:
write.table(x=test_txtfile, file="LANDIS_stuff/initial_communities_ashtx_11June2022.txt",
            quote=FALSE, append=TRUE, row.names=FALSE, col.names = FALSE)


#okayyy, looks BEAUTIFUL! :)
#now, onto actually REPLACING those mapcodes in the MAP...

# figuring out placement of treated black ash -------------------------------

#so first of all, let's look at how many cells there are TOTAL with ash, per ma:
#basically just went back to the summary tibbles code above to add this, like so:
framstands_crop8s_byma
frnistands_crop8s_byma

#OK, starting with black ash, we'll want to protect those in the PRIVATE/family 
#ownership category
#and there's only 477 cells of those (in MA 1), soooo let's just say...
#protect half of them across the board. Let's go with that!

layernames2
initcomm_vals <- values(crop8s[[3]]) #just storing as a vector this time...
#reminder, just doing this for BLACK ASH for now!
mgmtarea_vals <- values(crop8s[[5]])
initcomm_cells_select_frni <- NA
for(i in 1:length(initcomm_vals)){
  if(initcomm_vals[i] %in% blackash_codes){
    #and looking for cells ONLY in the first management area:
    if(mgmtarea_vals[i]==1){
      initcomm_cells_select_frni <- c(initcomm_cells_select_frni, i)
    }
  }
}
#let's check how long this vector is... indexing CELL POSITION/NUMBER,
#NOT the actual cell VALUE
length(initcomm_cells_select_frni)
#that looks....right?!
head(initcomm_cells_select_frni)
#removing the first one that's just 'NA':
initcomm_cells_select_frni <- initcomm_cells_select_frni[-1]
#cool!

#now let's try subsetting them to half of the cells, AKA 477/2 =~ 238
initcomm_cells_select_frni <- sample(initcomm_cells_select_frni, 238)
length(initcomm_cells_select_frni)
head(initcomm_cells_select_frni) #coolio!

#now, need to RECLASSIFY those cells in the raster:

initcommraster_blackashtx <- crop8s[[3]]
#cycling thru only the values of this raster that are in our chosen subset...
for(i in initcomm_cells_select_frni){
  #identify the starting value...
  initval <- as.numeric(initcommraster_blackashtx[i])
  #match up w/ the corresponding replacement value...
  replaceval <- ashcodes_key$treated_mapcode[ashcodes_key$orig_mapcode==initval]
  initcommraster_blackashtx[i] <- replaceval
}

#let's test this out...
initcommraster_blackashtx[initcomm_cells_select_frni[100]]
#'twas replaced! yay :)

#now just time to 'place' treated WHITE ash!
#and although I was *momentarily* concerned about this,
#no issues w/ overlap of white and black ash treated plots b/c they are in dif MAs.

# placement of treated white ash -------------------------------

#okay.....next decision = where to put "treated" ash & how much!
#initial plan was to target public (state?) and private (NGO) stands...
#w/ ~75 ash stands in each of those MA categories
#could do about a third of them each (~25) w/ "ecosystem function" approach
#a.k.a. treated ash are spread throughout the stand
#and another third of them each (~25) w/ "preserve genetic diversity" approach
#a.k.a. treated ash are clustered together
#refer back to Tony's presentation for details on this.....
#from presentation- key slide: 

#"How many trees do you treat (uplands—northern hardwoods)?
#• Functional: 10-15 large diameter individuals distributed across stand
#– Distribute “large-tree” function that would be lost; costs allocated at site-level
#• Genetic: 10-15, dominant/codominant trees (8-16” DBH) in a
#localized area within stands (1-2 acres)
#– Keep costs down per site to allow for protection across many sites"

#SO, next step is maybe to look @ ash SIZE/age for prioritizing treatment??

# STARTING WITH THIS TOMORROW (SUNDAY) AND JUST GETTING IT DONE!!!