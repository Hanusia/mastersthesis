# --------------------------------------------------
# Initial communities map exploration
# 29 May 2022
# HH
# --------------------------------------------------
#

#first, loading needed packages
library(terra)
library(tidyverse)

#and then reading in input files

#first, initial community maps for each of my study regions
initcomm_N <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_N_init_comm_v1.tif")
initcomm_S <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_S_init_comm_v1.tif")

#then, the mask rasters that just have 0 or 1 values to denote active(1)/inactive(0) cells 
initcomm_mask <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_mask.tif")
initcomm_mask_N <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_mask_N.tif")
initcomm_mask_S <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_mask_S.tif")

#next, load in the initial communities text file in a way that's SEARCHABLE...
init_comm_txt <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/Initial_communities_VT-NH-MA_2022-05-27.txt")
View(init_comm_txt)
#alright, basically just has this as a file w/ 1 col, dif in each row...
#But I think I can use regular expressions to break it up?? (and maybe also the "fill" function from tidyverse)

#creating a new dataframe & renaming cols to correspond w/ parts of this input file
init_comm_df <- init_comm_txt
names(init_comm_df)[1] <- "input"
init_comm_df$mapcode <- rep(NA)
init_comm_df$species <- rep(NA)
init_comm_df$age <- rep(0) 
View(init_comm_df)

#now to start the formatting...first start by carring over anywhere there's a mapcode row to the mapcode col
#head(init_comm_df[("^MapCode" %in% init_comm_df$input),])
#this isn't working
#  ??grep()
#think this function will be my best bet....
#head(grep(pattern="MapCode", x=init_comm_df$input))
mapcoderows <- grep(pattern="MapCode", x=init_comm_df$input)
#there's def a better/faster way to do this but IDK it, so gonna just use a good ol for loop:
for(i in 1:nrow(init_comm_df)){
  if(i %in% mapcoderows) {
    init_comm_df$mapcode[i] <- init_comm_df$input[i]
  }
}
#whoopee, it worked!
#now let's try to FILL:
#?fill() #in the tidyverse
init_comm_df <- init_comm_df %>%
  fill(mapcode, .direction="down")
#yay, it worked!
#now let's remove the rows that are JUST mapcode, since they are now all associated w/ their species as they should be.
mapcoderowvals <- grep(pattern="MapCode", x=init_comm_df$input, value=TRUE)
init_comm_df <- init_comm_df[!(init_comm_df$input %in% mapcoderowvals),]
#alright cool.
#next = separate out species (again using regex?)
#putting the first "word" found in each row
#init_comm_df$species <- grep(pattern="^[a-z]+", x=init_comm_df$input, value=TRUE)
#sptest <- grep(pattern="[a-z]+", x=init_comm_df$input, value=TRUE)
#View(sptest)
#?separate() #ACTUALLY, seems like this function will do what I want BETTER
#View(init_comm_df %>% separate(input, into=c("species", "age"))) #testing out...
#LOL AMAZING THAT IS EXACTLY WHAT I NEEDED
init_comm_df <- init_comm_df %>% separate(input, into=c("species", "age"))
#real quick gonna do this so that mapcode number is separate too:
init_comm_df <- init_comm_df %>% separate(mapcode, into=c("label", "mapcode"))
init_comm_df <- init_comm_df[,c(1,2,4)] #removing the useless "label" col
#okay, NOW we can get into the meat of it!

blackash_codes <- init_comm_df$mapcode[init_comm_df$species=="fraxnigr"]
blackash_codes
whiteash_codes <- init_comm_df$mapcode[init_comm_df$species=="fraxamer"]

#OK, now let's see how many ACTIVE cells on the map include black ash...

#*actual* map values in below dataframe: 
N_mapvalues <- values(x=initcomm_N, na.rm=TRUE)
S_mapvalues <- values(x=initcomm_S, na.rm=TRUE)
sum(blackash_codes %in% N_mapvalues) #they only show up in 4 cells LOL
sum(blackash_codes %in% S_mapvalues) #and in only 1 in the S area LOL
#Ok, soooo black ash isn't going to end up being a major player here, AT ALL.

#next, let's see how the masks line up w/ other rasters I have.
mgmtareas <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_INT4S_22Feb2022.tif")
#??compareGeom()
compareGeom(initcomm_mask, mgmtareas)
#so they cover the same extent at least...
summary(values(initcomm_mask)) 
summary(values(mgmtareas))
sum(values(initcomm_mask, na.rm=TRUE, mat=FALSE)>0) #number ACTIVE cells: 49509256
sum(values(mgmtareas, na.rm=TRUE, mat=FALSE)>0) #number ACTIVE cells = 6644540
#okay...this is a pretty BIG difference in the number of active cells!
#but why??
#maybe wait on this until I can confer w/ Jane to figure out the big difference in magnitude...
#WAIT, actually I'm guessing this is b/c it HASN'T masked out the non-active counties!!
plot(initcomm_mask) #yeah, this seems like the issue!! OK glad I figured that out!
plot(mgmtareas)

#alright, next task (maybe?) = to mask-back-and-forth all the rasters...
#and also will need to figure out the classes thing?
#and then also figure out how to deal with raster STACKS to look @ multiple factors @ once
#e.g. stand + management area/ownership type + forest type/mapcode...

#addt'l rasters I need to read in to do this:
#stand map
#ecoregions raster (don't actually need for this exercise but will need to mask eventually)

stands <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/stands_classified_INT4S_22Feb2022.tif")
#plot(stands)

#first, need to "cut out" stands and mgmtareas to N and S extents:
stands_N <- crop(x=stands, y=initcomm_mask_N)
stands_S <- crop(x=stands, y=initcomm_mask_S)
#plot(stands_N) #testing: looks OK...
mgmtareas_N <- crop(x=mgmtareas, y=initcomm_mask_N)
mgmtareas_S <- crop(x=mgmtareas, y=initcomm_mask_S)
#plot(mgmtareas_N)
compareGeom(stands_N, initcomm_N)
#looks OK extent-wise...
#now to mask things back & forth!
#let's check first to see how close they are...
#sum(values(initcomm_mask_N, na.rm=TRUE, mat=FALSE)>0)
#sum(values(mgmtareas_N, na.rm=TRUE, mat=FALSE)>0)
#sum(values(stands_N, na.rm=TRUE, mat=FALSE)>0)
#ookay, still quite a bit more active space in the initcomm mask, 
#BUT that is OK bc it's not cut out yet to the counties. Makes sense!!
#?mask
initcomm_N_remask <- mask(x=initcomm_N, mask=mgmtareas_N,
                          maskvalues=0, updatevalue=0)
plot(initcomm_N_remask)
plot(initcomm_N)
#now let's check their overlap again...
sum(values(initcomm_N_remask, na.rm=TRUE, mat=FALSE)>0)
sum(values(mgmtareas_N, na.rm=TRUE, mat=FALSE)>0)
#alright, they are closer, just not yet identical!
#now to re-mask back in the other direction:
#sum(is.na(values(initcomm_N_remask))) #check first
mgmtareas_N_remask <- mask(x=mgmtareas_N, mask=initcomm_N_remask,
                           maskvalues=0, updatevalue=0)
sum(values(initcomm_N_remask, na.rm=TRUE, mat=FALSE)>0)
sum(values(mgmtareas_N_remask, na.rm=TRUE, mat=FALSE)>0)
#alrighty, now they are the same, yay!
#time to do stands as well:
stands_N_remask <- mask(x=stands_N, mask=initcomm_N_remask,
                        maskvalues=0, updatevalue=0)
sum(values(stands_N_remask, na.rm=TRUE, mat=FALSE)>0)
#looks good!

#now for the southern ones:
initcomm_S_remask <- mask(x=initcomm_S, mask=mgmtareas_S,
                          maskvalues=0, updatevalue=0)
plot(initcomm_S_remask)
plot(initcomm_S)
#now let's check their overlap again...
sum(values(initcomm_S_remask, na.rm=TRUE, mat=FALSE)>0)
sum(values(mgmtareas_S, na.rm=TRUE, mat=FALSE)>0)
#alright, they are closer, just not yet identical!
#now to re-mask back in the other direction:
sum(is.na(values(initcomm_S_remask))) #check first...21 NAs...
mgmtareas_S_remask <- mask(x=mgmtareas_S, mask=initcomm_S_remask,
                           maskvalues=c(0,NA), updatevalue=0)
sum(values(initcomm_S_remask, na.rm=TRUE, mat=FALSE)>0)
sum(values(mgmtareas_S_remask, na.rm=TRUE, mat=FALSE)>0)
#Ok, now that we have accounted for NAs, they are fully OK!
#time to do stands as well:
stands_S_remask <- mask(x=stands_S, mask=initcomm_S_remask,
                        maskvalues=c(0,NA), updatevalue=0)
sum(values(stands_S_remask, na.rm=TRUE, mat=FALSE)>0)
#AND JUST DON'T FORGET THAT WE HAVEN'T DONE THE ECOREGIONS ONE YET EITHER!!
#THAT WILL ALSO NEED TO BE RE MASKED, IT JUST HASN'T HAPPENED YET!

#but FOR NOW, moving on to a stack!
initcomm_mapcodes_N <- values(initcomm_N_remask, dataframe=TRUE)
head(initcomm_mapcodes_N)
#this isn't quite what I want yet...
#let's try stacking them first??
stack_N <- c(initcomm_N_remask, mgmtareas_N_remask, stands_N_remask)
stack_S <- c(initcomm_S_remask, mgmtareas_S_remask, stands_S_remask)
stack_N_vals <- values(stack_N, dataframe=TRUE)
View(stack_N_vals)
names(stack_N_vals) <- c("initcomm", "mgmtarea", "stand")
#Ok, again, same with the S region
stack_S_vals <- values(stack_S, dataframe=TRUE)
View(stack_S_vals)
names(stack_S_vals) <- c("initcomm", "mgmtarea", "stand")

#NOW, to try playing around with the stand characteristics...
#first, trying to generate a list of STANDS where white ash is present in @ least 15% of cells
whiteash_count_N <- data.frame("stand"=unique(stack_N_vals$stand),
                               "num_plots"=rep(0),
                               "num_ash_plots"=rep(0))
#View(whiteash_count_N)
#removing zeros:
#whiteash_count_N <- whiteash_count_N[-1,]
#for(i in 1:nrow(stack_N_vals)){
#  for(j in 1:length(unique(stack_N_vals$stand))){
#    if(stack_N_vals$stand[i]==whiteash_count_N$stand[j]){
      #if stand values match up, add 1 to total plot count.
#    whiteash_count_N$num_plots[j] <- whiteash_count_N$num_plots[j] + 1
     #and if THAT is true, ALSO tally how many of those plots have ash...
#    if(stack_N_vals$initcomm[i] %in% whiteash_codes){
#      whiteash_count_N$num_ash_plots[j] <- whiteash_count_N$num_ash_plots[j] + 1
#    }
#    }
#  }
#}
#in retrospect, I maybe should have removed the all-zero rows first...
#to make it a little less work for the for loop to do...
#or else going about this a different way, like by first subsetting the stack_N_vals
#by which of them contain ash...but no that wouldn't have worked either!
#OK, after this loop is done processing, I should see how it turned out.
#and I at LEAST need to save my newly re-masked rasters for stand, mgmtarea, and initcomm
#to a file folder somewhere.

#OK this is taking a long time...poorly designed for loop by me
#going to go back to remove zeros
#actually doing it in a new for loop:

#first making a plus-equals operator/function:
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#removing zeros:
#whiteash_count_N <- whiteash_count_N[-1,] #DON'T NEED TO REPEAT THIS LINE
stack_N_vals_nozero <- stack_N_vals[stack_N_vals$stand!=0,]
for(i in 1:nrow(stack_N_vals_nozero)){
  #for(j in 1:length(unique(stack_N_vals$stand))){
    #if(stack_N_vals$stand[i]==whiteash_count_N$stand[j]){
  j <- stack_N_vals_nozero$stand[i]
      #if stand values match up, add 1 to total plot count.
      whiteash_count_N$num_plots[whiteash_count_N$stand==j] %+=% 1
      #and if THAT is true, ALSO tally how many of those plots have ash...
      if(stack_N_vals_nozero$initcomm[i] %in% whiteash_codes){
        whiteash_count_N$num_ash_plots[whiteash_count_N$stand==j] %+=% 1
      }
   # }
  #}
}

#now, repeating the same process with the southern region.
whiteash_count_S <- data.frame("stand"=unique(stack_S_vals$stand),
                               "num_plots"=rep(0),
                               "num_ash_plots"=rep(0))
View(whiteash_count_S)
#removing zeros:
whiteash_count_S <- whiteash_count_S[-1,]
stack_S_vals_nozero <- stack_S_vals[stack_S_vals$stand!=0,]
for(i in 1:nrow(stack_S_vals_nozero)){
  #for(j in 1:length(unique(stack_S_vals$stand))){
  #if(stack_S_vals$stand[i]==whiteash_count_N$stand[j]){
  j <- stack_S_vals_nozero$stand[i]
  #if stand values match up, add 1 to total plot count.
  whiteash_count_S$num_plots[whiteash_count_S$stand==j] %+=% 1
  #and if THAT is true, ALSO tally how many of those plots have ash...
  if(stack_S_vals_nozero$initcomm[i] %in% whiteash_codes){
    whiteash_count_S$num_ash_plots[whiteash_count_S$stand==j] %+=% 1
  }
  # }
  #}
}
## WARNING: ACTUALLY ADDED THESE VALUES TO WHITEASH_COUNT_N INSTEAD OF WHITEASH_COUNT_S
# UGHHHHH SO THAT ONE (WHITEASH_COUNT_N) IS NO LONGER VALID AT ALL!!!


#next steps will be finding PROPORTION of ash cells in stands.
#but also, first filtering by stands of a certain SIZE (~10 acres/4 ha?):
whiteash_count_N2 <- whiteash_count_N[whiteash_count_N$num_plots>=45,]
whiteash_count_N2$prop_ash <- whiteash_count_N2$num_ash_plots/whiteash_count_N2$num_plots
View(whiteash_count_N2)

ash_stands_S <- whiteash_count_S[whiteash_count_S$num_plots>=45,]
View(ash_stands_S)
ash_stands_S$prop_ash <- ash_stands_S$num_ash_plots/ash_stands_S$num_plots

ash_stands_N <- whiteash_count_N2[whiteash_count_N2$prop_ash>=0.15,]
View(ash_stands_N)

ash_stands_S <- ash_stands_S[ash_stands_S$prop_ash>=0.15,]


#finally also gonna save the rasters...
writeRaster(x=initcomm_N_remask, 
            filename="LANDIS_stuff/initcomm_N_remask_30May2022.tif", 
            datatype='INT4S')
writeRaster(x=initcomm_S_remask, 
            filename="LANDIS_stuff/initcomm_S_remask_30May2022.tif", 
            datatype="INT4S")
writeRaster(x=mgmtareas_N_remask,
            filename="LANDIS_stuff/mgmtareas_N_remask_30May2022.tif", 
            datatype="INT4S")
writeRaster(x=mgmtareas_S_remask,
            filename="LANDIS_stuff/mgmtareas_S_remask_30May2022.tif", 
            datatype="INT4S")
writeRaster(x=stands_N_remask,
            filename="LANDIS_stuff/stands_N_remask_30May2022.tif", 
            datatype="INT4S")
writeRaster(x=stands_S_remask,
            filename="LANDIS_stuff/stands_S_remask_30May2022.tif", 
            datatype="INT4S")

# Monday's progress - forest CLASSES -------------------------------
### (resuming this on Mon. morning, 5/30/2022) ###

#and NOW, we can sort them by management area/type...
#that may be the last level of sorting??

#ash_stands_N <- merge(x=ash_stands_N, y=stack_N_vals_nozero[,c("stand", "mgmtarea")],
#                      all.x=TRUE, by="stand")
#this (line above) accidentally merged them separately line-by-line...not ideal...
#maybe just base off the first digit instead
ash_stands_N$mgmtarea <- trunc(ash_stands_N$stand/10000000)
#now to see how many "ash stands" are in each mgmt area:
standsperma_N <- table(ash_stands_N$mgmtarea)

ash_stands_S$mgmtarea <- trunc(ash_stands_S$stand/10000000)
standsperma_S <- table(ash_stands_S$mgmtarea)

#REMINDER OF THE MANAGEMENT AREA CATEGORIES
#1 = private/family, 2 = private/corporate/TIMO, 3 = private/NGO/other, 
#4 = public/federal, 5 = public/state/local, 6 = roadside

#priorities for ash "treatment" = private/NGO & public/state stands (3&5)
#there's much more of both MAs in the southern study area, ash stand-wise

#alright, let's take a pause from this process real quick to read in the forest CLASS maps...
initclass_N <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/rs_lstopo_GMM_class_VT-MA_N_v1")
initclass_N
plot(initclass_N)
#alright, at least it is view-able!! 
#also just need to mask it...
initclass_S <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/rs_lstopo_GMM_class_VT-MA_S_v1")
initclass_S
plot(initclass_S)

#now, to read initial community maps BACK in for masking purposes:
initcomm_N_remask <- rast("LANDIS_stuff/initcomm_N_remask_30May2022.tif")
initcomm_S_remask <- rast("LANDIS_stuff/initcomm_S_remask_30May2022.tif")
#and also the stand & mgmt maps while we're at it:
mgmtareas_N_remask <-rast("LANDIS_stuff/mgmtareas_N_remask_30May2022.tif")
mgmtareas_S_remask <- rast("LANDIS_stuff/mgmtareas_S_remask_30May2022.tif")
stands_N_remask <- rast("LANDIS_stuff/stands_N_remask_30May2022.tif")
stands_S_remask <- rast("LANDIS_stuff/stands_S_remask_30May2022.tif")


plot(initcomm_N_remask)
#and then use them for masking:
sum(is.na(values(initclass_N))) #so there ARE plenty of NA values.
compareGeom(initcomm_N_remask, initclass_N) #and they DO cover same extent.
initclass_N_remask <- mask(x=initclass_N, mask=initcomm_N_remask,
                           maskvalues=c(0,NA), updatevalue=0)
plot(initclass_N_remask)
#check same #s of active cells:
sum(values(initcomm_N_remask, na.rm=TRUE, mat=FALSE)>0) #number ACTIVE cells: 2752605
sum(values(initclass_N, na.rm=TRUE, mat=FALSE)>0) #before mask- number ACTIVE cells: 5285432
sum(values(initclass_N_remask, na.rm=TRUE, mat=FALSE)>0) #after mask- number ACTIVE cells: 2752579
#this is ALMOST the same as initcomm_N_remask, but just a little different...
#we will need to re-remask all these again (??), but for now, I'm OK with this.
#actually, maybe not? Since the init classes are more of a tool for starting than an actual input?
#CHECK WITH JANE ON THIS

#another random Q: is there a function in R to find the part of a raster with
#the most diversity in values? / highest variance in values or something like that?
#would need to make those values categorical first...
#look into this after some more examination
#maybe also do another "stack" to find an area w/ more diversity in mgmt type, too??

#in the meantime, following the same process w/ the southern region:
initclass_S_remask <- mask(x=initclass_S, mask=initcomm_S_remask,
                           maskvalues=c(0,NA), updatevalue=0)
plot(initcomm_S_remask)
plot(initclass_S)
plot(initclass_S_remask)
#again, check the # of active cells:
#check same #s of active cells:
sum(values(initcomm_S_remask, na.rm=TRUE, mat=FALSE)>0) #number ACTIVE cells: 3771382
sum(values(initclass_S, na.rm=TRUE, mat=FALSE)>0) #before mask- number ACTIVE cells: 7317248
sum(values(initclass_S_remask, na.rm=TRUE, mat=FALSE)>0) #after mask- number ACTIVE cells: 3771365
#once again, SUPER close!
#and not sure if we actually need to re-remask everything again here.
#for now, focus on ID-ing a small subset region for initial analysis.
help(terra)
#look for a function, then also make new "stacks" w/ init class vals.
#will ultimately need to use the "crop" function
table(values(initclass_N_remask))
table(values(initclass_S_remask))
#IDEA: maybe do a chi-square analysis to see if my chosen subset is approx
#representative of the larger area?? (e.g. chi-sq of forest cover classes prevalence,
#comparing full regional extent to the subset)
#(or maybe that is not really necessary, but could be interesting at least.)
#another idea = measure "evenness" of N and S classes & MAs.
initclass_N_summary <- table(values(initclass_N_remask))
initclass_S_summary <- table(values(initclass_S_remask))
glimpse(initclass_N_summary)
#install.packages("asbio")
#library(asbio)
#??evenness()
#evenness(initclass_N_summary)
plot(initclass_N_summary) #0 is far outweighing things but otherwise, looks OK
plot(initclass_S_summary) #S region looks qualitatively more evern overall, tho.
initclass_N_summary <- as.data.frame(initclass_N_summary)
initclass_S_summary <- as.data.frame(initclass_S_summary)
names(initclass_N_summary) <- c("class", "freq")
names(initclass_S_summary) <- c("class", "freq")
plot(initclass_N_summary[-1,]) #now plotting w/o 0
plot(initclass_S_summary[-1,]) #now plotting w/o 0
#ok, actually I'd say N region is more even EXCEPT for 1 class
#S region has more variation but not just 1 class above the rest.

#exec decision: gonna try for a subset in S region so I can hopefully get some of each MA type!
#first step=stack em
newstack_S <- c(initclass_S_remask, initcomm_S_remask, mgmtareas_S_remask, stands_S_remask)
#plot(initclass_S_remask)
#womp womp... R session aborted... back to the drawing board I guess
#gonna just redo some things...but hopefully just those things since my last save/session backup??
#FIRST THINGS FIRST, need to save ash_stands dfs so I don't lose those since they were time consuming to make LOL.
write.csv(x=ash_stands_N, file="LANDIS_stuff/ash_stands_N_30May2022.csv")

#okay so I DO need to redo the SOUTHERN end of it...but will do that LATER??
#or at least start the loop whenever it is that I have a break.
write.csv(x=ash_stands_S, file="LANDIS_stuff/ash_stands_S_30May2022.csv")

#FOR NOW, LET'S START BY GETTING BACK TO THE INIT CLASS STUFF TO FIND A SUBSET
#alright, we are BACK with newstack_S for now!
newstack_S_vals <- values(newstack_S, data.frame=TRUE)
View(newstack_S_vals)
names(newstack_S_vals) <- c("initclass", "initcomm", "mgmtarea", "stand")

#trying to find a small area that will include multiple ownership types- might not get nat'l plus many others though...
#looks like maybe around y=~4650000 and y=~655000 we have some variety?

#experimenting w/ creating a "new" raster to use as a cookie cutter:
subset_1_cookiecutter <- rast(nrows=500, ncols=500, nlyrs=1,
                              xmin=655001, xmax=655500,
                              ymin=4650001, ymax=4650500,
                              crs="epsg:32618", res=c(30,30))
subset_1_cookiecutter
#OK, this did *work*, BUT the number of grid cells is too small.
#it basically divided my 'nrow' and 'ncol' vals of 500 
#by the resolution values of 30m.
#SO, we need to multiply nrows, ncols, and the range of vals for x and y by 30.
30*500 #15,000
#let's try this again...
subset_2_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=655001, xmax=670000,
                              ymin=4650001, ymax=4665000,
                              crs="epsg:32618", res=c(30,30))
subset_2_cookiecutter
#ok, now THAT looks more like it!!
#now let's try to use it as a "cookie cutter"
subset_S_1 <- crop(x=newstack_S, y=subset_2_cookiecutter)
plot(subset_S_1$layer)
#alright, not quite on the money...but looks like we did get some good variety!
plot(subset_S_1$`VT-MA_S_init_comm_v1`)
plot(subset_S_1$mgmt_areas_masked_INT4S_22Feb2022) #got 3 dif MAs (1, 5, and 6)
#I think we're OK in the y-direction, but need to adjust to the "left" in the X-direction.
subset_3_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=644001, xmax=659000,
                              ymin=4650001, ymax=4665000,
                              crs="epsg:32618", res=c(30,30))
subset_3_cookiecutter
subset_S_2 <- crop(x=newstack_S, y=subset_3_cookiecutter)
plot(subset_S_2$layer)
#okay, that's definitely much better!
#still not sure why it's looking like a rectangle instead of a square,
#but I'm just not gonna worry too much about it lol.
#looks like we have some decent variation in the forest type/class.
#let's check out the other layers:
plot(subset_S_2$`VT-MA_S_init_comm_v1`)
#ok, definitely NOT as much variation here...
plot(subset_S_2$mgmt_areas_masked_INT4S_22Feb2022)
#but we DO have 4 dif MAs now: 1, 3, 5, and 6. that's great!
plot(subset_S_2$stands_classified_INT4S_22Feb2022)
subset_S_2_vals <- (values(subset_S_2, dataframe=TRUE))
View(subset_S_2_vals)
names(subset_S_2_vals) <- c("initclass", "initcomm", "mgmtarea", "stand")
subset_S_2_classsum <- as.data.frame(table(subset_S_2_vals$initclass))
subset_S_2_classsum
plot(subset_S_2_classsum[-1,]) #some a lot more prevalent than others, but they are all PRESENT...
#maybe this is the subset to stick with???
#will need to look @ ecoregions as well, first...
#and re-remask everything to make sure it looks OK...
#then follow up with Jane once I have done that?

ecoregions <- rast("C:/Users/theha/Documents/layers_for_LANDIS/ecoregions_masked_roads_17March2022.tif")
ecoregions
plot(ecoregions)
ecoregions_subset <- crop(x=ecoregions, y=subset_3_cookiecutter)
compareGeom(ecoregions_subset, subset_S_2$layer) #apparently, "extents don't match"
#but why??
plot(ecoregions_subset)
plot(subset_S_2$layer)
ecoregions_subset
subset_3_cookiecutter
#hmm okay...why is this just causing the ecoregions subset to be only 325 rows???
subset_S_2$layer
subset_S_2$`VT-MA_S_init_comm_v1`
#okayy wait a minute...why are these also like 300 by 500???
#I am confusion?????
subset_S_2
#ALRIGHT, seems like the ymin value is NOT good here...
plot(initcomm_S_remask)
#need to move everything UP!!
#let's try this instead (shifted ymin/ymax values to fit better inside)
subset_4_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=644001, xmax=659000,
                              ymin=4670001, ymax=4685000,
                              crs="epsg:32618", res=c(30,30))
subset_4_cookiecutter
#alright now I want to line up numbers better, so we are renumbering the files this time:
subset_S_4 <- crop(x=newstack_S, y=subset_4_cookiecutter)
plot(subset_S_4$layer) #now THAT is a square!!! LOL
subset_S_4$layer
#looks like we have some decent variation in the forest type/class.
#let's check out the other layers:
plot(subset_S_4$`VT-MA_S_init_comm_v1`)
#ok, definitely NOT as much variation here...
plot(subset_S_4$mgmt_areas_masked_INT4S_22Feb2022)
#alsoooo looks like we have ALL the management areas in here now, so that's cool?!?!
plot(subset_S_4$stands_classified_INT4S_22Feb2022)
subset_S_4_vals <- (values(subset_S_4, dataframe=TRUE))
View(subset_S_4_vals)
names(subset_S_4_vals) <- c("initclass", "initcomm", "mgmtarea", "stand")
subset_S_4_classsum <- as.data.frame(table(subset_S_4_vals$initclass))
subset_S_4_classsum
plot(subset_S_4_classsum[-1,]) #some a lot more prevalent than others, but they are all PRESENT...
#again, some classes are quite a lot more prevalent, but a fair-ish amount of others too.....
#classes 23 and 24 are the MOST prevalent; are those ones with a fair amount of ash?
#lmao, NO they are not...is this a dealbraker??
plot(initclass_S_remask)
#the ones with more ash are more towards the lower end of the class #s...
plot(initclass_N_remask)
plot(table(values(initclass_N_remask)))
#the classes that INCLUDE ash = 2, 3, 4, 7, 8, 14, 15, 16, 18, 19, 21, 32


### back at it on Tuesday 5/31 ###
#just gonna stick with this subset (#4) for now to try it out for LANDIS.
#but gonna add ecoregions raster to the stack first?...and need to mask it too.
#orrr maybe I'll just do that after?

#OK, we have our new "stack" again, now let's try comparing w/ ecoregions & saving all the layers...

ecoregions_subset_S_4 <- crop(x=ecoregions, y=subset_4_cookiecutter)
compareGeom(ecoregions_subset_S_4, subset_S_4)
#alright, geom is ok!
#now to mask back-and-forth...
sum(is.na(values(ecoregions_subset_S_4, mat=FALSE))) 
sum(is.na(values(subset_S_4, mat=FALSE))) 
#first checking for NAs, and there are none
sum(values(subset_S_4$layer, na.rm=TRUE, mat=FALSE)>0) 
#checking 1st layer # 0s = 209,553
sum(values(subset_S_4$stands_classified_INT4S_22Feb2022, na.rm=TRUE, mat=FALSE)>0) 
#last layer is the same # zeros, so I think it's safe to assume they are the same??
sum(values(ecoregions_subset_S_4, na.rm=TRUE, mat=FALSE)>0) 
#alright, this one has MORE zeroes: 211,112
#sooo, we will need to mask back-and-forth both ways, it looks like.
ecoregions_subset_S_4_remask <- mask(x=ecoregions_subset_S_4, mask=subset_S_4$layer,
                                     maskvalues=0, updatevalue=0)
subset_S_4_remask <- mask(x=subset_S_4, mask=ecoregions_subset_S_4_remask,
                          maskvalues=0, updatevalue=0)
#checking zero values again:
sum(values(subset_S_4_remask$layer, na.rm=TRUE, mat=FALSE)>0) 
#checking 1st layer # 0s = 209,553
sum(values(subset_S_4_remask$stands_classified_INT4S_22Feb2022, na.rm=TRUE, mat=FALSE)>0) 
#last layer is the same # zeros, so I think it's safe to assume they are the same??
sum(values(ecoregions_subset_S_4_remask, na.rm=TRUE, mat=FALSE)>0) 
#wait, why did this one go up SO much???? did it compare to EACH layer??
#update: maybe...?? But when I redid the remask operation above w/ just one layer, looks more normal.
table(values(ecoregions_subset_S_4_remask, mat=FALSE))
#I forgot we are looking for the TOTAL number of NON-zero vals, not the reverse!!
#soo, actually we might be fine now?
#didn't need the remask for the stack I guess? just check this using all.equal
all.equal(subset_S_4, subset_S_4_remask) #yep they didn't change!
#just the ecoregions one did.
#but I think now we're ready to download & test these?!
#once we also get other input files into place, such as the ecoregions climate files...

writeRaster(x= subset_S_4$layer, 
            filename="LANDIS_stuff/subset_practicerun1_30May2022/S4subset_initclass.tif", 
            datatype='INT4S')
writeRaster(x= subset_S_4$`VT-MA_S_init_comm_v1`, 
            filename="LANDIS_stuff/subset_practicerun1_30May2022/S4subset_initcomm.tif", 
            datatype='INT4S')
writeRaster(x= subset_S_4$mgmt_areas_masked_INT4S_22Feb2022, 
            filename="LANDIS_stuff/subset_practicerun1_30May2022/S4subset_mgmtareas.tif", 
            datatype='INT4S')
writeRaster(x= subset_S_4$stands_classified_INT4S_22Feb2022, 
            filename="LANDIS_stuff/subset_practicerun1_30May2022/S4subset_stands.tif", 
            datatype='INT4S')
writeRaster(x= ecoregions_subset_S_4_remask, 
            filename="LANDIS_stuff/subset_practicerun1_30May2022/S4subset_ecoregions.tif", 
            datatype='INT4S')

#OKAY, now gonna use another script to parse out the ecoregions files!!


# Wednesday June 1st -------------------------------

### back to this on Wednesday 6/1/22 ###

#first task = calculate # ACTIVE cells in my subset vs in whole landscape.

ecoregions_subset_S_4_remask <- rast("LANDIS_stuff/subset_practicerun1_30May2022/S4subset_ecoregions.tif")
subset_activenum <- sum(values(ecoregions_subset_S_4_remask, na.rm=TRUE, mat=FALSE)>0)
#copied from above:
#ecoregions <- rast("C:/Users/theha/Documents/layers_for_LANDIS/ecoregions_masked_roads_17March2022.tif")
plot(ecoregions)
fullarea_activenum <- sum(values(ecoregions, na.rm=TRUE, mat=FALSE)>0)
fullarea_activenum
subset_activenum/fullarea_activenum

#now let's look at some of the outputs of the biomass extension...

fram_agbio_0 <- rast("LANDIS_stuff/subset_practicerun1_30May2022/output/AGbiomass/fraxamerAG/biomass-0.img")
fram_agbio_10 <- rast("LANDIS_stuff/subset_practicerun1_30May2022/output/AGbiomass/fraxamerAG/biomass-10.img")
fram_agbio_50 <- rast("LANDIS_stuff/subset_practicerun1_30May2022/output/AGbiomass/fraxamerAG/biomass-50.img")

plot(fram_agbio_0)
plot(fram_agbio_10)
plot(fram_agbio_50)

#alright, also want to look @ max age in init comm file:
#max(init_comm_df$age) #99 is the oldest tree (?)
#that's not right....maybe b/c it's as character type right now?
max(as.numeric(init_comm_df$age)) #148- that makes more sense!

#OK, let's bring in the (AG) biomass table and plot it over time...
#Q: how to most effectively compare this to the ACTUAL data (e.g. Jane's calculated biomass)??

agbiomass <- read.csv(file="LANDIS_stuff/subset_practicerun1_30May2022/output/AGbiomass/AG/biomass-AllYears.csv")
View(agbiomass)
agbiomass_long <- pivot_longer(data=agbiomass, cols=!Time, #all species columns
                               names_to="species", values_to="biomass_gpersqm")
View(agbiomass_long)

plot(x=agbiomass_long$Time, y=agbiomass_long$biomass_gpersqm)

#for looking @ total biomass:
#?apply
agbiomass$total <- apply(agbiomass[,-1], 1, sum)
plot(x=agbiomass$Time, y=agbiomass$total)

#doing this more comprehensively w/ ggplot:
library(ggplot2)
biomass_subsetS4 <- ggplot(data=agbiomass_long, aes(x=Time, y=biomass_gpersqm)) +
  geom_area(aes(fill=species))
plot(biomass_subsetS4)
#looks good!
