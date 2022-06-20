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
# twas replaced! yay :)
initcommraster_blackashtx

#also want to SAVE the vector for posterity:
write.csv(as.data.frame(initcomm_cells_select_frni), file="LANDIS_stuff/blackash_cells_treated_crop8S.csv")
#and also SAVE where JUST black ash were treated: 
writeRaster(initcommraster_blackashtx, filename="LANDIS_stuff/initcomm_crop8S_blackashtx.tif",
            datatype='INT4S')

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

#I am thinking that due to time constraints, I may not be able to add the 
#spatial element of functional vs genetic conservation approaches to ash treatment,
#and just apply different *frequencies*/levels/amounts of treatment to ash in stand.
#BUT, I COULD look into the function terra::patches.... although that's just IDing cell vals between NAs...
#as briefly described here: https://rspatial.org/spatial-terra/8-rastermanip.html
#soooo with that in mind, maybe NO spatial element.
#BUT, can explore that for later / full run,
#and at least for now it's easy to do similar to what I did for black ash, shouldn't be too hard!!
# ALSO think about SIZE OF TREES!!! (e.g. age of ash within those chosen mapcodes....)
#FOR TOMORROW!


#it's now tomorrow (sunday 6/12), and I am here to work on this...
#buuuut gonna do it at Starbucks instead!

#so my harvest prescriptions assumed that white ash @ age 65 ~ 15" DBH.
#and that white ash @ age 40 ~ 10" DBH.

#FUNCTIONAL preservation = saving LARGE-DIAMETER ash throughout, so let's say age 65 and up
  #and this one is lowest frequency/rate of treatment-- 10-15 trees (cohorts) per stand.
#GENETIC preservation = saving MEDIUM-SIZED (?) dominant/codominant ash,
#so let's say those in age classes 40-65...
#let's first see what we have for both of those...
#"old" white ash over age 65
whiteash_codes_large <- init_comm_df$mapcode[init_comm_df$species=="fraxamer" & init_comm_df$age>65]
length(whiteash_codes_large) #alright, we've got 227 of those. (unique mapcodes)
whiteash_codes_medium <- init_comm_df$mapcode[init_comm_df$species=="fraxamer" & 
                                                init_comm_df$age<65 & init_comm_df$age>=40]
length(whiteash_codes_medium) #and we have 56 of these...
head(whiteash_codes_medium)

#now to explore which STANDS have those sizes of ash...
View(framstands2_crop8S)
#first subset to JUST those in NGO-private & federally-owned stands.
framstandslarge_crop8S <- framstands2_crop8S[(framstands2_crop8S$mgmtarea %in% c(3,4)),]
nrow(framstandslarge_crop8S) #SHOULD be 77+71: and yes it's 148.
#now need to go thru and see how many of those are LARGE ash plots....
View(framstandslarge_crop8S)
framstandslarge_crop8S$num_large_ash_plots <- rep(0)
#and also see how many are medium in the same loop!!
framstandslarge_crop8S$num_med_ash_plots <- rep(0)

for(i in 1:nrow(crop8s_vals_nozero)){
  j <- crop8s_vals_nozero$stand[i]
  #if it's already an "ash stand" we're intersted in...
  if(j %in% framstandslarge_crop8S$stand){
    #and if THAT is true, ALSO tally how many of those plots have LARGE ash...
    if(crop8s_vals_nozero$initcomm[i] %in% whiteash_codes_large){
      framstandslarge_crop8S$num_large_ash_plots[framstandslarge_crop8S$stand==j] %+=% 1
    }
    #may as well check for MEDIUM ash in the same loop, why not?!
    if(crop8s_vals_nozero$initcomm[i] %in% whiteash_codes_medium){
      framstandslarge_crop8S$num_med_ash_plots[framstandslarge_crop8S$stand==j] %+=% 1
    }
  }
}
#OK I think this worked!
#GENERALLY, looks like we have more "large ash" plots than "medium ash" plots...
#Just need to decide between which STANDS are getting "large ash"/functional treatment,
#which are getting "medium ash"/genetic treatment,
#and which (a smaller number) are getting "treat all ash"
#since they can't overlap.
#thinking 20 functional, 20 genetic, and 10 complete stands per mgmt area (3-private/NGO and 4-public/fed)?
#first, look for how many have MORE medium-sized than large-sized ash sites:
nrow(framstandslarge_crop8S[framstandslarge_crop8S$num_large_ash_plots<framstandslarge_crop8S$num_med_ash_plots,])
#only eleven!
framstandslarge_crop8S[framstandslarge_crop8S$num_large_ash_plots<framstandslarge_crop8S$num_med_ash_plots,]

#now let's also see how many of them have AT LEAST 10 large ash sites/cohorts:
nrow(framstandslarge_crop8S[framstandslarge_crop8S$num_large_ash_plots>10,]) #118, so most of them
nrow(framstandslarge_crop8S[framstandslarge_crop8S$num_large_ash_plots>=15,]) #101, so *still* most of them


#for GENETIC, look for smaller (?) stands w/ @ least 10 med ash plots...
nrow(framstandslarge_crop8S[
  framstandslarge_crop8S$num_med_ash_plots>10,]) #was gonna include & ...$area_ha<4
    #LOL never mind, I ALREADY filtered out the ones smaller than 4 ha...
    #maybe go back to framstands_crop8S for that?!?

#so, my preliminary thought is (assuming there are enough qualifying plots in the latter),
#draw from stands >4 ha in size w/ @ least 10-15 LARGE ash for the functional conservation tx,
#and then draw from stands <4 ha in size w/ @ least 10-15 MEDIUM ash for the genetic conservation tx.
View(framstands_crop8S)
framstandsmed_crop8S <- framstands_crop8S
framstandsmed_crop8S$mgmtarea <- trunc(framstandsmed_crop8S$stand/10000000)
#limiting to those in MA 3 & 4:
framstandsmed_crop8S <- framstandsmed_crop8S[(framstandsmed_crop8S$mgmtarea %in% c(3,4)),]
View(framstandsmed_crop8S)
framstandsmed_crop8S$area_ha <- framstandsmed_crop8S$num_plots*0.09 #each plot = 0.09 ha
hist(framstandsmed_crop8S$area_ha) #most of these are quite small; good!
#now to find out how many MEDIUM-SIZED ash we've got:
framstandsmed_crop8S$num_med_ash_plots <- rep(0)
for(i in 1:nrow(crop8s_vals_nozero)){
  j <- crop8s_vals_nozero$stand[i]
  #if it's already an "ash stand" we're intersted in...
  if(j %in% framstandsmed_crop8S$stand){
    #then check how many of those plots contain MEDIUM-SIZED ash...
    if(crop8s_vals_nozero$initcomm[i] %in% whiteash_codes_medium){
      framstandsmed_crop8S$num_med_ash_plots[framstandsmed_crop8S$stand==j] %+=% 1
    }
  }
}
#now let's see how many fit our criteria...
nrow(framstandsmed_crop8S[framstandsmed_crop8S$area_ha<4 & #less than 4 ha in size
                            framstandsmed_crop8S$num_med_ash_plots>=10,]) #and at least 10 medium sized ash plots
#hmm okay; what about just:
max(framstandsmed_crop8S$num_med_ash_plots[framstandsmed_crop8S$area_ha<4])
#so the most # ash plots in a small stand is 7.
#OK, so this didn't work, so let's go back to framstandslarge_crop8S 
#and do everything there instead, to better control overlap:
nrow(framstandslarge_crop8S[framstandslarge_crop8S$num_med_ash_plots>=15,]) #32
nrow(framstandslarge_crop8S[framstandslarge_crop8S$num_med_ash_plots>=10,]) #48
#SO, those are less numerous than the big ash ones...let's see what sizes those stands are:
hist(framstandslarge_crop8S$area_ha[framstandslarge_crop8S$num_med_ash_plots>=10]) #48
#ok, *some* are on the smaller side; good!
hist(framstandslarge_crop8S$num_med_ash_plots[framstandslarge_crop8S$num_med_ash_plots>=10]) #48
View(framstandslarge_crop8S[framstandslarge_crop8S$num_med_ash_plots>=10,]) #48
framstandsmed <- framstandslarge_crop8S[framstandslarge_crop8S$num_med_ash_plots>=10,]
table(framstandsmed$mgmtarea[framstandsmed$area_ha<20])
table(framstandsmed$mgmtarea) #let's just pick 10 from each of these, 
table(framstandsmed$num_med_ash_plots)
table(framstandsmed$mgmtarea[framstandsmed$num_med_ash_plots<40])
#OKAY, that's what we've got, so maybe just go for it!!
#these 40 stands (30 in NGO ma and 20 in federal ma), each with between 10-40
#ash plots/cohorts, could be the ones I select for "medium" ash/genetic cons tx.
framstandsmed <- framstandsmed[framstandsmed$num_med_ash_plots<40,]
View(framstandsmed)
#soo, then I can just REPLACE all the MEDIUM-SIZED ash in those stands...
# variable framstandsmed is my OFFICIAL list of stansd to "treat" w/ genetic approach!!

#now to eliminate those ones from the running for framstandslarge:
framstandslarge <- framstandslarge_crop8S[!(framstandslarge_crop8S$stand %in% framstandsmed$stand),]
View(framstandslarge)
framstandslarge <- framstandslarge[framstandslarge$num_large_ash_plots>=10,]
hist(framstandslarge$area_ha)
nrow(framstandslarge[framstandslarge$area_ha>10,]) #51 stands greater than 10 ha in size...
nrow(framstandslarge[framstandslarge$num_large_ash_plots<25,]) #51 stands w/ <25 large ash plots...
table(framstandslarge$mgmtarea[framstandslarge$num_large_ash_plots<25]) #of those, 20 in MA 3 and 31 in MA 4 :)
hist(framstandslarge$area_ha[framstandslarge$num_large_ash_plots<25]) #annnnd size-wise, not that big...
table(framstandslarge$mgmtarea[framstandslarge$num_large_ash_plots<25
                               & framstandslarge$area_ha>6]) 
#but for those GREATER THAN 6 ha in size, and containing LESS THAN 25 large ash plots,
#we have exactly 40 (14 in MA 3 and 26 in MA 4)....that's actually perfect!!
hist(framstandslarge$prop_ash[framstandslarge$num_large_ash_plots<25
                              & framstandslarge$area_ha>6])
hist(framstandslarge$num_ash_plots[framstandslarge$num_large_ash_plots<25
                              & framstandslarge$area_ha>6])
#and they have a *decent* spread of TOTAL (all sizes) ash proportion/number...
#ok let's do it:
framstandslarge <- framstandslarge[framstandslarge$num_large_ash_plots<25
                                   & framstandslarge$area_ha>6,]
nrow(framstandslarge)
#okay- PERFECTO!!!!!

#now, we just need ANOTHER 10-20 stands to treat ALL ash; 
#preferably those with less overall/smaller...
View(framstands2_crop8S)
View(framstands2_crop8S[!(framstands2_crop8S$stand %in% framstandslarge$stand) &
                          !(framstands2_crop8S$stand %in% framstandsmed$stand),])
#subsetting to ash stands that will NOT be treated w/ 'large ash' or 'med ash' approach...
framstandscomplete <- framstands2_crop8S[!(framstands2_crop8S$stand %in% framstandslarge$stand) &
                                           !(framstands2_crop8S$stand %in% framstandsmed$stand),]
View(framstandscomplete)
#and also subsetting to only MA 3 & 4; leaves us with 68 stands.
framstandscomplete <- framstandscomplete[(framstandscomplete$mgmtarea %in% c(3,4)),]
hist(framstandscomplete$prop_ash)
hist(framstandscomplete$num_ash_plots[framstandscomplete$prop_ash>=0.25])
nrow(framstandscomplete[framstandscomplete$prop_ash>=0.25 &
                        framstandscomplete$num_ash_plots<100,])
hist(framstandscomplete$area_ha[framstandscomplete$prop_ash>=0.25 &
                                  framstandscomplete$num_ash_plots<100])
#none that are too huge... @ least 25% ash plots and <100 ash plots total
table(framstandscomplete$mgmtarea[framstandscomplete$prop_ash>=0.25 &
                                    framstandscomplete$num_ash_plots<100])
#we've got 11 in MA 3 and 10 in MA 4; perfect!
max(framstandscomplete$num_ash_plots[framstandscomplete$prop_ash>=0.25 &
                                       framstandscomplete$num_ash_plots<100])
table(framstandscomplete$mgmtarea[framstandscomplete$prop_ash>=0.25 &
                                    framstandscomplete$num_ash_plots<=60])
#muahaha perfect!! applying this filter now:
framstandscomplete <- framstandscomplete[framstandscomplete$prop_ash>=0.25 &
                                           framstandscomplete$num_ash_plots<=60,]
#okay SO. we have all of our collections of which stands to treat partially,
#using each conservation approach, and then the fewer amount to treat COMPLETELY!!

#next just need to set up a for loop, like I did for black ash, to change these in the init comm map.
#but first gonna save them as csvs for posterity:
#remember these are all ONLY LOOKING AT WHITE ASH, NOT LOOKING AT BLACK ASH!!!!
write.csv(framstandslarge, file="LANDIS_stuff/framstands_treatlargeash_functional.csv")
write.csv(framstandsmed, file="LANDIS_stuff/framstands_treatmedash_genetic.csv")
write.csv(framstandscomplete, file="LANDIS_stuff/framstands_treatallash_complete.csv")

#ok, need to start by MAKING A VECTOR OF CELLS TO BE REPLACED.
#similar to what I did with black ash, with a few differences/additions...
initcomm_cells_select_fram <- NA
for(i in 1:nrow(crop8s_vals)){
  #check for each situation individually....
  #first, check for large ash (check stand AND mapcode value:)
  if(crop8s_vals$stands[i] %in% framstandslarge$stand  & crop8s_vals$initcomm[i] %in% whiteash_codes_large){
      #if both those conditions are met, ADD this cell to the list of ones to be replaced.
    initcomm_cells_select_fram <- c(initcomm_cells_select_fram, i)
    #next check for membership in MEDIUM ash treatment stands / correct cells:
  } else if(crop8s_vals$stands[i] %in% framstandsmed$stand  & crop8s_vals$initcomm[i] %in% whiteash_codes_medium){
    initcomm_cells_select_fram <- c(initcomm_cells_select_fram, i)
    #and finally check for membership in the COMPLETE stand:
  } else if(crop8s_vals$stands[i] %in% framstandscomplete$stand  & crop8s_vals$initcomm[i] %in% whiteash_codes){
    initcomm_cells_select_fram <- c(initcomm_cells_select_fram, i)
    }
  }

head(initcomm_cells_select_fram)
length(initcomm_cells_select_fram)
#cool!! let's save this one too:
initcomm_cells_select_fram <- na.omit(initcomm_cells_select_fram)
write.csv(as.data.frame(initcomm_cells_select_fram), file="LANDIS_stuff/whiteash_cells_treated_crop8S.csv")
#okay awesome!! now we can use this vector to replace those cells with corresponding
#mapcodes of treated ash. :) 
#OK, one potential hiccup = how many of those are also including black ash....
#buuut honestly, I might just...not worry about that???
#or I guess let's see:
blackashinwhiteashtx <- (crop8s_vals$initcomm[initcomm_cells_select_fram] %in% blackash_codes)
head(blackashinwhiteashtx)
#only 16 of these cells also contain black ash...so yknow what, let's not worry about that!!
#or I guess could just...remove those ones?
#yeah I guess let's do that!
length(blackashinwhiteashtx)
length(initcomm_cells_select_fram)
blackashinwhiteashtx_cellvals <- NA
for(i in 1:length(initcomm_cells_select_fram)){
  if(blackashinwhiteashtx[i]==TRUE){
    blackashinwhiteashtx_cellvals <- c(blackashinwhiteashtx_cellvals, initcomm_cells_select_fram[i])
  }
}
blackashinwhiteashtx_cellvals
length(blackashinwhiteashtx_cellvals)
blackashinwhiteashtx_cellvals <- na.omit(blackashinwhiteashtx_cellvals)

initcomm_cells_select_fram_minusfrni <- initcomm_cells_select_fram[!(initcomm_cells_select_fram %in% blackashinwhiteashtx_cellvals)]
head(initcomm_cells_select_fram_minusfrni)
length(initcomm_cells_select_fram_minusfrni)
length(initcomm_cells_select_fram) - length(blackashinwhiteashtx_cellvals)
#OK, I think this did work!!
#save vector to file again:
write.csv(as.data.frame(initcomm_cells_select_fram_minusfrni), file="LANDIS_stuff/whiteash_cells_treated_crop8S_minusblackashcells.csv")


#first trying replacement with JUST the initial raster as input:
initcommraster_whiteashtx <- crop8s[[3]]
#cycling thru only the values of this raster that are in our chosen subset...
for(i in initcomm_cells_select_fram_minusfrni){
  #identify the starting value...
  initval <- as.numeric(initcommraster_whiteashtx[i])
  #match up w/ the corresponding replacement value...
  replaceval <- ashcodes_key$treated_mapcode[ashcodes_key$orig_mapcode==initval]
  initcommraster_whiteashtx[i] <- replaceval
}

#let's test this out...
initcommraster_whiteashtx[initcomm_cells_select_fram_minusfrni[100]]
# twas replaced! yay :)
initcommraster_whiteashtx
sum(values(initcommraster_whiteashtx)>11000)
#the right amount of cells were replaced--whoo!

#now SAVE where JUST white ash were treated: 
writeRaster(initcommraster_whiteashtx, filename="LANDIS_stuff/initcomm_crop8S_whiteashtx.tif",
            datatype='INT4S')

#now, repeat the process so we have BOTH in ONE raster:
initcommraster_ashtx <- initcommraster_blackashtx
#cycling thru only the values of this raster that are in our chosen subset...
for(i in initcomm_cells_select_fram_minusfrni){
  #identify the starting value...
  initval <- as.numeric(initcommraster_ashtx[i])
  #match up w/ the corresponding replacement value...
  replaceval <- ashcodes_key$treated_mapcode[ashcodes_key$orig_mapcode==initval]
  initcommraster_ashtx[i] <- replaceval
}

initcommraster_ashtx
sum(values(initcommraster_ashtx)>11000)
length(initcomm_cells_select_fram_minusfrni) + length(initcomm_cells_select_frni)
#whoo, looks good!!!
plot(initcommraster_ashtx)
plot(crop8s[[5]])
#save raster to file:
writeRaster(initcommraster_ashtx, filename="LANDIS_stuff/initcomm_crop8S_ashtx.tif",
            datatype='INT4S')

plot(ifel(initcommraster_ashtx>11000, 8, crop8s[[5]]))
#looks good!! :)

compareGeom(initcommraster_ashtx, crop8s[[3]])
sum(values(initcommraster_ashtx)>0)
sum(values(crop8s[[3]])>0)
#amazing! we did it Joe!!!
plot(rast("LANDIS_stuff/initcomm_crop8S_ashtx.tif"))
#looks just about right!!! :)