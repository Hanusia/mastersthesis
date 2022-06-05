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

###

#okay, NEXT TASK FOR TODAY (p.m.) = DEFINITIVELY remask / create master mask file for each entire region.
#going to use the ecoregions file as the reference for this + the initcomm_mask files

#and then after that, ID the plots that I want to analyze for testing w/ the PnEToutputsites extension. 

#initcomm_mask_N <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_mask_N.tif")
#initcomm_mask_S <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_mask_S.tif")
# copied these files from above:

plot(ecoregions)
#also, still have ecoregions file loaded from above.
#need to CROP ecoregions to each of the 2 study areas, then create a "master mask" w/ each region.

ecoregions_N <- crop(ecoregions, initcomm_mask_N)
ecoregions_S <- crop(ecoregions, initcomm_mask_S)
#also checking for NAs:
sum(is.na(values(ecoregions, mat=FALSE))) 
sum(is.na(values(initcomm_mask_N, mat=FALSE))) #this one DOES have NAs. (and assuming the southern region does too)
initcomm_mask_N
plot(initcomm_mask_N)
sum(values(ecoregions_N, na.rm=TRUE, mat=FALSE)>0) 
sum(values(initcomm_mask_N, na.rm=TRUE, mat=FALSE)>0) 

mask_N <- ifel(test=(initcomm_mask_N==1 & ecoregions_N>0), yes=1, no=0) #let's see if this works...
#the ifel statement above is selecting the cells that are included for the initcomm mask AND ecoregions, making all others zero
mask_N
plot(mask_N)
sum(is.na(values(mask_N, mat=FALSE))) #mannn this still has lots of NAs!! next trying to remove them...
mask_N <- mask(x=mask_N, mask=initcomm_mask_N, 
               maskvalues=c(0, NA), updatevalue=0)
sum(is.na(values(mask_N, mat=FALSE)))  #alright, NOW we have no NAs!
plot(mask_N)
sum(values(mask_N, mat=FALSE)>0) #checking if this value is similar to what I calculated this AM...(via ecoregions)

#now need to make an overall S region mask as well:
mask_S <- ifel(test=(initcomm_mask_S==1 & ecoregions_S>0), yes=1, no=0)
mask_S
sum(is.na(values(mask_S, mat=FALSE))) #again, plenty of MA vals
mask_S <- mask(x=mask_S, mask=initcomm_mask_S, 
               maskvalues=c(0, NA), updatevalue=0)
sum(is.na(values(mask_S, mat=FALSE)))  #alright, NOW we have no NAs!
plot(mask_S)
sum(values(mask_S, mat=FALSE)>0) #checking if this value is similar to what I calculated this AM...(via ecoregions)
#alright, looking good!

#OK, and basically redoing these stacks from above; can disregard what I did earlier in this code:
#just had to re-load in these rasters real quick...
#initcomm_N <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_N_init_comm_v1.tif")
#initcomm_S <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/VT-MA_S_init_comm_v1.tif")
#mgmtareas <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/mgmt_areas_masked_INT4S_22Feb2022.tif")
#stands <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/stands_classified_INT4S_22Feb2022.tif")
#ecoregions <- rast("C:/Users/theha/Documents/layers_for_LANDIS/ecoregions_masked_roads_17March2022.tif")
#initclass_N <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/rs_lstopo_GMM_class_VT-MA_N_v1")
#initclass_S <- rast("C:/Users/theha/Documents/layers_for_LANDIS/veg_maps/veg_maps/rs_lstopo_GMM_class_VT-MA_S_v1")

#now creating the stacks
stack_N <- c(initclass_N, initcomm_N, crop(mgmtareas, mask_N), crop(stands, mask_N), crop(ecoregions, mask_N))
stack_S <- c(initclass_S, initcomm_S, crop(mgmtareas, mask_S), crop(stands, mask_S), crop(ecoregions, mask_S))
#subset these layers by subset(stack_N, 1) or subset(stack_N, c(2:3)), OR stack_N[[1]]
plot(subset(stack_N, 1))
#5 layers per stack; apply this method to each layer
#let's test out one beforehand tho: 
sum(is.na(values(stack_N[[1]], mat=FALSE))) #yeahhhh so we still got lots of NAs here...
  for(i in 1:5){
  stack_N[[i]] <- mask(x=stack_N[[i]], mask=mask_N,
                             maskvalues=c(0,NA), updatevalue=0)
  stack_S[[i]] <- mask(x=stack_S[[i]], mask=mask_S,
                             maskvalues=c(0,NA), updatevalue=0)
  
  }
#so far so good, I think.....
#let's check it...
sum(is.na(values(stack_N[[1]], mat=FALSE)))
#does this work???
stack_N[is.na(stack_N)] <- 0
#seemingly, yes?!
#let's try another...
sum(is.na(values(stack_S[[1]], mat=FALSE)))
stack_S[is.na(stack_S)] <- 0
sum(is.na(values(stack_S[[1]], mat=FALSE)))
#alright groovy...
#now to test the number of plots:
sum(values(mask_N, mat=FALSE)>0)  
sum(values(mask_S, mat=FALSE)>0)  
for(i in 1:5){
  sum(values(stack_N[[i]], mat=FALSE)>0)  
  sum(values(stack_S[[i]], mat=FALSE)>0)  
} #this didn't seem to work??

#TBH, a better way to do this would be:
active_N <- sum(values(mask_N, mat=FALSE)>0)  
active_S <- sum(values(mask_S, mat=FALSE)>0)  
for(i in 1:5){
  print(sum(values(stack_N[[i]], mat=FALSE)>0)  == active_N)
  print(sum(values(stack_S[[i]], mat=FALSE)>0)  == active_S)
}
#OKAY I'M CONCERNED.....
#basically, all BUT the first layer of each stack are working
#ALTHOUGH TO BE HONEST, THE INIT_CLASS RASTER DOESN'T ACTUALLY NEED TO BE INCLUDED IN THE LANDIS INPUT...
#SOOOO MAYBE IT'S OKAY???
print(sum(values(stack_N[[1]], mat=FALSE)>0))
print(sum(values(stack_S[[1]], mat=FALSE)>0))
#they are each just a liiiitle bit off...and honestly I do think it's the NAs probably?????
#but anyway...it's time to download these puppies
layernames <- c("initclass", "initcomm", "mgmtareas", "stands", "ecoregions")
 #too tired to figure out otherwise so I'm just gonna do a separate loop for N and S regions lmao
 for(l in 1:5){
    writeRaster(x=stack_N[[l]], 
                filename=paste0("LANDIS_stuff/region_rasters/", layernames[l], "_N", ".tif"), 
                datatype='INT4S')
  }
#and here's the one for S rastesr
for(l in 1:5){
  writeRaster(x=stack_S[[l]], 
              filename=paste0("LANDIS_stuff/region_rasters/", layernames[l], "_S", ".tif"), 
              datatype='INT4S')
}
#let's take a look...
l #right now, l=5, so this SHOULD plot the ecoregions for S
plot(rast(paste("LANDIS_stuff/region_rasters/", layernames[l], "_S", ".tif")))
plot(initcomm_mask_S)
plot(mask_S) #TBH, this is a LOT fewer active cells than before...but maybe that's just the way it is???

#ok, for now, at least we have the region rasters that are all with the same mask, other than the init class ones LOL
#and the FINAL VALUES of active cells are:
active_N
active_S
active_N + active_S

#also want to save those masks:
writeRaster(x=mask_N, 
            filename=paste("LANDIS_stuff/region_rasters/mask_N.tif"), 
            datatype='INT4S')
writeRaster(x=mask_S, 
            filename=paste("LANDIS_stuff/region_rasters/mask_S.tif"), 
            datatype='INT4S')


#OKAY, next step for comparing inital biomass vals, using extract() function from terra to find cells
#that correspond w/ FIA plots.

#first need to read in a table w/ long/lat values for FIA plots in the study area:

fiaplots <- vect(x="C:/Users/theha/Documents/LANDIS_docs/FIA_VT-NH-MA_fia_summaries_fromJane/VT-NH-MA_fia_plot_coords.shp")
plot(fiaplots)
#alright cool...
View(values(fiaplots))
fiaplots

#then, bring in the init comm files for N and S regions:
initcomm_N <- rast("LANDIS_stuff/region_rasters/initcomm_N.tif")
initcomm_N
plot(initcomm_N)
initcomm_S <- rast("LANDIS_stuff/region_rasters/initcomm_S.tif")
extract(initcomm_N, fiaplots)
#not working for some reason.....??
#is this because their extents don't match????
#let's try setting the extent of a new vect:
e=ext(initcomm_N)
fiaplots_N <- vect("C:/Users/theha/Documents/LANDIS_docs/FIA_VT-NH-MA_fia_summaries_fromJane/VT-NH-MA_fia_plot_coords.shp", 
                   crs="epsg:32618", 
                   extent=e)
fiaplots_N
View(values(fiaplots_N))
?ext
?vect
#WHY is this not working.....I am confusion
#and I think that means it's time to log off and go to bed lmao.
?terra::extract
#alright, maybe I need to specify which package is using extract()??
#since it's a function in both the raster and terra packages...

fiaplots_N <- terra::extract(initcomm_N, fiaplots, cells=TRUE, xy=TRUE)
View(fiaplots_N)
plot(fiaplots_N)

initcomm_N
#maybe I should just do this with the active mask instead??
mask_N <- rast("LANDIS_stuff/region_rasters/mask_N.tif")
fiaplots_N <- terra::extract(mask_N, fiaplots, cells=TRUE, xy=TRUE)
View(fiaplots_N)

#Just trying to updated the terra package to see if that makes any difference with this function......
#install.packages("terra")
#library(terra) #lol never mind it's still the older version...not sure why that is happening

fiaplots_N <- terra::extract(mask_N, fiaplots, cells=TRUE)


#maybe I should try just doing this with the raster package instead????
library(raster)
mask_N_rast <- raster("LANDIS_stuff/region_rasters/mask_N.tif")
fiaplots_sf <- shapefile("C:/Users/theha/Documents/LANDIS_docs/FIA_VT-NH-MA_fia_summaries_fromJane/VT-NH-MA_fia_plot_coords.shp")
mask_N_rast
fiaplots_sf
fiaplots_sf_N <- extract(mask_N_rast, fiaplots_sf, method='simple', cellnumbers=TRUE, df=TRUE)
View(fiaplots_sf_N)
#alright THIS seemed to actually work, it's just that many of the rows are NAs b/c not all plots fit in this extent
#and that's okay!!!
nrow(fiaplots_sf_N[fiaplots_sf_N$mask_N==1,]) #how many are located in ACTIVE cells in N region:
#1843 (??) is this including NAs??
nrow(fiaplots_sf_N[fiaplots_sf_N$mask_N==0,]) #it MUST be, bc the math ain't mathin'...
head(na.rm(fiaplots_sf_N))
fiaplots_sf_N <- na.omit(fiaplots_sf_N)
#okay this makes more sense now.
nrow(fiaplots_sf_N[fiaplots_sf_N$mask_N==1,]) #ACTUAL number of plots in ACTIVE cells in this df = 84
#now to generate a list of fifty of those: 
rowColFromCell(object=mask_N_rast, cell=head(fiaplots_sf_N$cells[fiaplots_sf_N$mask_N==1]))
#alright cool cool... now adding rows and cols to the dataframe
fiaplots_sf_N$row <- rowFromCell(object=mask_N_rast, cell=fiaplots_sf_N$cells)
fiaplots_sf_N$col <- colFromCell(object=mask_N_rast, cell=fiaplots_sf_N$cells)
#QUESTION: what is the input value needed for PnET-ouput-sites to NAME those cells/sites??
#basically can just be: Site "name," row, column
#want to associate by "id" of fia plots, AKA row numbers in the orig. fiaplots shapefile/associated data...

fiaplots_sf_N_select <- fiaplots_sf_N[fiaplots_sf_N$mask_N==1, c("ID", "row", "col")]
View(fiaplots_sf_N_select) #84 plots; TBH that seems doable??
#now to generate a table WITH HEADER:
write.table(x="LandisData     PNEToutputsites", file="LANDIS_stuff/fiaplots_N_testsites.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
write.table(x=fiaplots_sf_N_select, 
            file="LANDIS_stuff/fiaplots_N_testsites.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE, col.names=FALSE)

#now to do the same thing w/ the south!
#BUT, could just get the north one up and running first...??

#in the meantime, let's just take a quick look @ the current white ash biomass map of the N region.....
frambioN_year0 <- rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_N/output/fraxamer/biomass-0.img")
plot(frambioN_year0)
#yeahh so we don't have a TON...but a fair amount of prominent ashy areas sprinkled throughout
#maybe use that moving window function from the terra package to ID the ash-iest area??
frambioN_year0
summary(values(frambioN_year0))
hist(values(frambioN_year0))
hist(values(frambioN_year0)[values(frambioN_year0)!=0])
ecoregions_N <- rast("LANDIS_stuff/region_rasters/ecoregions_N.tif")
mgmtareas_N <- rast("LANDIS_stuff/region_rasters/mgmtareas_N.tif")
plot(ecoregions_N)
plot(mgmtareas_N)
ecoregions_S <- rast("LANDIS_stuff/region_rasters/ecoregions_S.tif")
mgmtareas_S <- rast("LANDIS_stuff/region_rasters/mgmtareas_S.tif")
plot(ecoregions_S)
plot(mgmtareas_S)

#random fun detail from Tony: that band of green/higher ash biomass running thru
#the middle of my northern study area lines up with the Waits River Formation,
#a bedrock band that has cause rich soils in that area
#let's look at it with sugar maple, too?? and/or maybe beech as well?
plot(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_N/output/acersacc/biomass-0.img"))
plot(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_N/output/fagugran/biomass-0.img"))
plot(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_N/output/tiliamer/biomass-0.img"))
plot(frambioN_year0)


#ALRIGHT, let's generate the OTHER list of FIA plots to use for pnetoutputsites!
library(raster)
mask_S_rast <- raster("LANDIS_stuff/region_rasters/mask_S.tif")
#fiaplots_sf <- shapefile("C:/Users/theha/Documents/LANDIS_docs/FIA_VT-NH-MA_fia_summaries_fromJane/VT-NH-MA_fia_plot_coords.shp")
mask_S_rast
fiaplots_sf
fiaplots_sf_S <- raster::extract(mask_S_rast, fiaplots_sf, method='simple', cellnumbers=TRUE, df=TRUE)
View(fiaplots_sf_S)
#alright THIS seemed to actually work, it's just that many of the rows are NAs b/c not all plots fit in this extent
#and that's okay!!!
fiaplots_sf_S <- na.omit(fiaplots_sf_S)
#okay this makes more sense now.
nrow(fiaplots_sf_S[fiaplots_sf_S$mask_S==1,]) #ACTUAL number of plots in ACTIVE cells in this df = 176
#now to generate a list of fifty of those: 
rowColFromCell(object=mask_S_rast, cell=head(fiaplots_sf_S$cells[fiaplots_sf_S$mask_S==1]))
#alright cool cool... now adding rows and cols to the dataframe
fiaplots_sf_S$row <- rowFromCell(object=mask_S_rast, cell=fiaplots_sf_S$cells)
fiaplots_sf_S$col <- colFromCell(object=mask_S_rast, cell=fiaplots_sf_S$cells)
#QUESTION: what is the input value needed for PnET-ouput-sites to NAME those cells/sites??
#basically can just be: Site "name," row, column
#want to associate by "id" of fia plots, AKA row numbers in the orig. fiaplots shapefile/associated data...

fiaplots_sf_S_select <- fiaplots_sf_S[fiaplots_sf_S$mask_S==1, c("ID", "row", "col")]
View(fiaplots_sf_S_select) 
#now to select 50 plots randomly!: 
fiaplots_sf_S_select <- fiaplots_sf_S_select[sample(1:nrow(fiaplots_sf_S_select), 50),]
#alright, cool cool!

#now to generate a table WITH HEADER:
write.table(x="LandisData     PNEToutputsites", file="LANDIS_stuff/fiaplots_S_testsites.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
#and turns out we actually DO need the column names: 
names(fiaplots_sf_S_select) <- c("PNEToutputsites", "Row", "Column")
write.table(x=fiaplots_sf_S_select, 
            file="LANDIS_stuff/fiaplots_S_testsites.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE, col.names=TRUE)
#great!

#next step = how to take those INPUTS and analyzed them compared w/ the actual FIA plot biomass data...
#buut first, maybe start the run w/ the southern region + FIA plots to do the same thing??

#okay, let's take a look @ the southern region's ash biomass!
frambioS_year0 <- rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_S/biomass_S/output/fraxamer/biomass-0.img")
#actually trying this a different way...
#frambioS <- system.file("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_S/biomass_S/output/fraxamer/biomass-0.img",
#                        package="terra")
#frambioS_year0 <- rast(frambioS, crs="epsg:32618", extent=ext(ecoregions_S))
#welllll this also didn't work, so I'm going back to the first way...
plot(frambioS_year0)
#alright, pretty variable!
#maybe use that moving window function from the terra package to ID the ash-iest area??
frambioS_year0
summary(values(frambioS_year0))
hist(values(frambioS_year0))
hist(values(frambioS_year0)[values(frambioS_year0)!=0])
#ecoregions_S <- rast("LANDIS_stuff/region_rasters/ecoregions_S.tif")
#mgmtareas_S <- rast("LANDIS_stuff/region_rasters/mgmtareas_S.tif")
plot(ecoregions_S)
plot(mgmtareas_S)
plot(frambioS_year0)
#crs(frambioS_year0) <- "epsg:32618"
frambioS_year0
#not sure how to make it look less weirdly stretched...
#frambioS_year0_2 <- terra::project(x=frambioS_year0, y=ecoregions_S,
#                                   align=TRUE, SRC_METHOD=NO_GEOTRANSFORM)
#alright, this isn't working, so let's try doing it another way instead...
crs(frambioS_year0) <- "epsg:32618" #starting w/ getting the same crs
frambioS_year0_2 <- resample(x=frambioS_year0, y=ecoregions_S)


#compareGeom(frambioS_year0, ecoregions_S)
#ext(frambioS_year0)

#alright let's just try one more thing: 
#making a NEW raster with the same extent / geometry as ecoregions,
#then transferring the VALUES from the ash biomass raster.
library(terra)
frambioS_init <- rast(ecoregions_S)
frambioS_year0
frambioS_init
ecoregions_S
values(frambioS_init)<-values(frambioS_year0)
plot(frambioS_year0)
plot(frambioS_init)
#okay, FINALLY this worked!!!!!
plot(ecoregions_S)
plot(mgmtareas_S)


# Friday June 3rd -------------------------------
### back at it on 6/3/2022! ###

# first priority today = comparing biomass w/ southern region to FIA plots!!

#first step, I guess, is to load in all the raster images of biomass for each species...
#then to extract their values at each of the FIA plots/cells...

#then also associate the row/cell val w/ the FIA plot in question...
#and THEN, I guess, AVERAGE the biomass for each species across the 50 cells.
#and also make sure to convert units!

#also need the list of species!
splist <- read.csv("C:/Users/theha/Documents/LANDIS_docs/Hanusia_plot_data_init_communities/species_list_code_key_hanusia.csv")
splist <- splist$sppcode
sp_biomass0 <- rast(frambioS_year0, nlyrs=length(splist))
for(i in 1:length(splist)){
 # file <- system.file(paste0("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_S/biomass_S/output/", splist[i], "/biomass-0.img"),
 #                     package="terra")
  newlayer <- rast(paste0("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_S/biomass_S/output/", splist[i], "/biomass-0.img"))
  sp_biomass0[[i]] <- newlayer
}

#ALRIGHT, now let's try this:
plot(sp_biomass0[[2]]) #plotting striped maple,,,not much but that's ok!
plot(sp_biomass0[[9]]) #this one's ash, as expected!
splist

#ok, NOW we need to associate each of the VALUES with the correct FIA plot.
#redoing some things from above...
fiaplots_sf_S <- fiaplots_sf_S[fiaplots_sf_S$mask_S==1,]
View(fiaplots_sf_S)
fiaplots_sf_S
#fiaplots_sf is what will be LINKING the actual calculated biomass w/ plotid, and LANDIS calculated biomass w/ rowid
fiaplots_sf
S_biomass_fromLANDIS <- data.frame("cell"=fiaplots_sf_S$cells)
#names(S_biomass_fromLANDIS) <- c(splist, "cell")
S_biomass_fromLANDIS #we currently just have the ONE column...
for(i in 1:length(splist)){
  #okay GREAT, this seems to work now...this is going thru species & attaching biomass
  biomass_g_m2 <- terra::values(sp_biomass0[[i]])[S_biomass_fromLANDIS$cell,]
  #attaching that as a new col to the existing dataframe...
  S_biomass_fromLANDIS <- cbind(S_biomass_fromLANDIS, biomass_g_m2)
  names(S_biomass_fromLANDIS)[i+1]<- paste0(splist[i], "_gm2") #and name the col w/ the species
}
View(S_biomass_fromLANDIS)
#wooooo we did it Joe!!!

#now need to associate FIA PLOT with the same CELL:
S_biomass_FIA <- read.csv("C:/Users/theha/Documents/LANDIS_docs/Hanusia_plot_data_init_communities/VT-MA_fia_plots_sp_bio.csv")
#rowkey <- read.csv("C:/Users/theha/Documents/LANDIS_docs/Hanusia_plot_data_init_communities/Init_comm_pltIds_mapcodes_key_VT-NH-MA_2022-05-27.csv")
#View(rowkey)
View(S_biomass_FIA)
#I actually need to use the FIA plots shapefile/what was extracted from it to associate them...
#S_biomass_FIA <- merge(x=S_biomass_FIA, y=rowkey, 
#                       by="pltID", all.x=TRUE, all.y=FALSE)
#YOU KNOW WHAT, i think it's better to associate the biomass from LANDIS w/ the FIA pltID as our common link.
fiaplots_sf$pltID #this is what we can associate w/ rowID by ROW in fiaplots_sf_S
S_biomass_fromLANDIS <- merge(S_biomass_fromLANDIS, fiaplots_sf_S[,c("cells", "ID")],
                              by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in S_biomass_fromLANDIS$ID){
  S_biomass_fromLANDIS$pltID[S_biomass_fromLANDIS$ID==i] <- 
    fiaplots_sf$pltID[i]
}

#OK, now to convert from Mg/ha to g/m2...
for(i in splist){
   newcol <- (S_biomass_FIA[,i])*100 #CONVERTING units
  S_biomass_FIA <- cbind(S_biomass_FIA, newcol)
  names(S_biomass_FIA)[ncol(S_biomass_FIA)] <- paste0(i, "_gm2")
  #renaming the last column that I just added...
}
#looking good!!
#also need to subset to just the plots in LANDIS...
S_biomass_FIA_subset <- S_biomass_FIA[S_biomass_FIA$pltID %in% S_biomass_fromLANDIS$pltID,]
View(S_biomass_FIA_subset) #looks gr8!
#now to aggregate mean vals:
meanbio_sp_S <- data.frame(spcies=splist,
                           fia_bio=rep(0), 
                           landis_bio=rep(0))
View(meanbio_sp_S)
#gonna do another loop for this...
#testing method for the loop:
#i <- 1
#mean(S_biomass_FIA_subset[,42+i])
#[1] 256.2796
# mean(S_biomass_FIA_subset$abiebals_gm2)
#[1] 256.2796 #looks right to me!
for(i in 1:length(splist)){
  meanval <- mean(S_biomass_FIA_subset[,42+i])
  meanbio_sp_S$fia_bio[i] <- meanval
  meanval2 <- mean(S_biomass_fromLANDIS[,1+i])
  meanbio_sp_S$landis_bio[i] <- meanval2
}
#THIS IS NOW THE DATAFRAME TO COMPARE:
write.csv(meanbio_sp_S, "LANDIS_stuff/biomass_test_S_1.csv")
#let's also add a column for whether the values are within 20% of each other:
meanbio_sp_S$close <- ifelse((abs(meanbio_sp_S$landis_bio-meanbio_sp_S$fia_bio)/meanbio_sp_S$fia_bio)<=.2, "yes", "no")
#NOW, the goal would be to do this again after adjusting PnET species parameters....probably w/ folN??
#and maybe will just rerun the northern region one after adjusting the parameters as well...


# choosing new landscape subset below ##############################################


#OK, current priority for me = CHOOSE A SUBSET OF THE LANDSCAPE TO ANALYZE
#let's start by reexamining each layer of the southern region...
ecoregions_S <- rast("LANDIS_stuff/region_rasters/ecoregions_S.tif")
plot(ecoregions_S)
initclass_S <- rast("LANDIS_stuff/region_rasters/initclass_S.tif")
plot(initclass_S)
initcomm_S <- rast("LANDIS_stuff/region_rasters/initcomm_S.tif")
plot(initcomm_S)
mgmtareas_S <- rast("LANDIS_stuff/region_rasters/mgmtareas_S.tif")
plot(mgmtareas_S)
stands_S <- rast("LANDIS_stuff/region_rasters/stands_S.tif")
plot(stands_S)
mask_S <- rast("LANDIS_stuff/region_rasters/mask_S.tif")
plot(mask_S)
plot(frambioS_year0)

#let's just take a look at another "cookie cutter"...
#first stacking up all the layers that I have
stack_S <- c(mask_S, initclass_S, initcomm_S, ecoregions_S, mgmtareas_S, stands_S)
#then choosing a new area--let's look at this one first??
subset_5_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=640001, xmax=659000,
                              ymin=4770001, ymax=4785000,
                              crs="epsg:32618", res=c(30,30))
plot(mgmtareas_S)
plot(crop(stack_S[[5]], subset_5_cookiecutter))
#hmm...not exactly what we want...let's try moving it more north and to the east??
subset_6_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=650001, xmax=665000,
                              ymin=4775001, ymax=4790000,
                              crs="epsg:32618", res=c(30,30))
plot(crop(stack_S[[5]], subset_6_cookiecutter))
#okayyy that's a pretty interesting mix?! (and actually a square lol)
#but still want to move up a bit more to cut out the missing corner..
subset_7_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=650001, xmax=665000,
                              ymin=4776001, ymax=4791000,
                              crs="epsg:32618", res=c(30,30))
plot(crop(stack_S[[5]], subset_7_cookiecutter))

#ANOTHER THING TO CONSIDER = whether any of my study plots are in there??
#and all in Bennington co. are only in GMNF, so need a good chunk of that in there!
#unless I go for the first subset after all...in Massachusetts
#MY sites in the upper GMNF were all about east of Rte 7, so this area SHOULD include them!

#gonna move it north ONE more time to get LESS TIMO in there...
subset_8_cookiecutter <- rast(nrows=15000, ncols=15000, nlyrs=1,
                              xmin=650001, xmax=665000,
                              ymin=4779001, ymax=4794000,
                              crs="epsg:32618", res=c(30,30))
plot(crop(stack_S[[5]], subset_8_cookiecutter))
#this even includes a few plots of state land!
#hmm... let's try working with this for a bit:
test_crop <- crop(stack_S, subset_8_cookiecutter)
#let's look @ distribution of management areas/types:
test_crop[[5]]
table(values(test_crop[[5]]))
#this seems OK...we've got a little bit of everything! :)
length(unique(values(test_crop[[6]]))) #now looking @ different STANDS:
#so, apparently 3,769 different parcels/stands...  
hist(unique(values(test_crop[[6]])))
#and the majority of those are different PRIVATE parcels and different STANDS w/in GMNF.
#I def want to look at those individ stands too???
#should I just break out arcmap again?
plot(test_crop[[6]])

#let's look at these files again;
#they were what I used in the initial stand/management area mask
#from the script management_stand_maps_2Feb2022.R:
GMNF_parcels_mask <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/GMNF_parcels_masked_3Feb2022.tif")
VT_parcels_mask <- rast("C:/Users/theha/Documents/layers_for_LANDIS/Properties_Stands/VT_parcels_masked_3Feb2022.tif")
plot(crop(GMNF_parcels_mask, test_crop))
plot(crop(VT_parcels_mask, test_crop))
#the VT plots are clearly broken up by blocks so not the most helpful for me...
#buuut honestly, this looks pretty good!!
#let's also just subset the ash biomass here to make sure we have a good range:
plot(frambioS_init)
plot(crop(frambioS_init, test_crop)) #cool cool; we still have a pretty good range!
#let's also take a look at other attributes...namely, forest class and ecoregions (reclassify)
plot(test_crop[[2]]) #ooh yeah, at least in terms of forest type, we have a LOT more variety here!!
hist(values(initclass_S))
hist(values(test_crop[[2]]))
#now to try and reclassify ecoregions so it shows up better...
plot(test_crop[[4]])
unique(values(test_crop[[4]]))
#I think we basically just need to bring the roads vals down to a similar number, like 50? or even 30...
eco_subset_reclass <- ifel(test_crop[[4]]>200, yes=30, no=test_crop[[4]]) #reclassifying all road ecoregions as 30!
plot(eco_subset_reclass) #okay this is solid variety, I think!

#let's see, what's next...maybe check on MY plots??
my_plots <- read.csv("C:/Users/theha/Documents/LANDIS_docs/Hanusia_plot_data_init_communities/Hanusiaplots_BA_sp_LANDISinit_19Nov2021_updatedcodes.csv")
View(my_plots)
my_plots <- my_plots[,c("Latitude", "Longitude")]

my_plots_subset <- vect(my_plots, 
                        geom=c("Longitude", "Latitude"),
                        crs="+proj=longlat")
my_plots_subset
plot(my_plots_subset)
my_plots_subset <- project(my_plots_subset, "epsg:32618")
#basically, I had to SET the crs when I created the vector (to latlong) before I could PROJECT it to the other system!
my_plots_subset #ok NOW we are doing fine I think.
my_plots_subset <- crop(my_plots_subset, test_crop)
plot(my_plots_subset)
plot(test_crop[[1]])
plot(my_plots_subset, add=TRUE)
#so we DO have a few plots in there (though only in one corner). Good!!
plot(eco_subset_reclass)
points(my_plots_subset)
#ok finally got this to work!!!
#okay...what next??
#I guess save each of the layers in this stack?

layernames2 <- c("mask", "initclass", "initcomm", "ecoregions", "mgmtareas", "stands")
for(l in 1:nlyr(test_crop)){
  writeRaster(x=test_crop[[l]], 
              filename=paste0("LANDIS_stuff/crop8S_rasters/", layernames2[l], "_crop8S", ".tif"), 
              datatype='INT4S',
              overwrite=TRUE  #had to add this since there was a mistake in how I made them/ordered/named the layers initially!!
              )
}
#great!

#now, NEXT task will be to try adjusting ecoregion parameters.....
#but let's save that for later?!

# back at it on Saturday, June 4th! -------------------------------
### 6/4/2022 ###

#CURRENTLY: testing adjusted parameters in my landscape subset!
# first up, check how many / which FIA plots are in my subset
# then do a few adjustments to species parameters for additional runs to try
# start with HalfSat first, then try some of the generic/global params??
# e.g. MaintResp?

#ok, for the fia plots, thing, for some reason terra:;extract did NOT work for me last time,
#but raster::extract did. So, just gonna do that again!
#orrrr could do crop like above... with my plot? maybe I'll try that
library(raster)
#fiaplots_sf <- shapefile("C:/Users/theha/Documents/LANDIS_docs/FIA_VT-NH-MA_fia_summaries_fromJane/VT-NH-MA_fia_plot_coords.shp")
fiaplots_sf
mask_crop8S <- raster("LANDIS_stuff/crop8S_rasters/mask_crop8S.tif")
mask_crop8S
fiaplots_crop8S <- raster::extract(mask_crop8S, fiaplots_sf, method='simple', cellnumbers=TRUE, df=TRUE)
View(fiaplots_crop8S)
fiaplots_crop8S <- na.omit(fiaplots_crop8S) #removing the (many) NAs:
#okay this makes more sense now.
nrow(fiaplots_crop8S[fiaplots_crop8S$mask_crop8S==1,]) 
#ACTUAL number of plots in ACTIVE cells in this df = 6......
#is this enough to calibrate to??? idk man.....

#but anyway, let's try some runs next!
#and then while they're going, work on building out FUNCTIONS to speed up the process of 
#comparing biomass w/ actual FIA plots.

#first run will just be using the SAME parameters, but in the subset.
#second run will be reducing HalfSat values for ALL species to 70% of what they are currently,
#and compare/see how the outputs differ.
#ALSO, don't forget to compare ABOVEGROUND biomass!!!
#MAYBE also incorporate my plots in there??? (later)...

#alright, ALSO, I need to rewrite the ecoregionparameters file to refer to the climate files 
#in a DIFFERENT location.
#doing that in the other ecoregion script!
#alright, did that and I THINK we are set up + ready for a run.
# - run 1 in this subset is using the SAME species / PnET parameters to establish a baseline.
# DIFFERENCES = different subset, also made the start year 2015
# and only outputting AGB so I can't get confused...

##ACTION ITEM: check to make sure I named the rasters correctly...
plot(rast("LANDIS_stuff/crop8S_rasters/ecoregions_crop8S.tif"))
#alright, I definitely did NOT!
#let's do that again......
#OKAY, just went back and CORRECTED layernames_2 above the loop where I output the rasters;
#now we should be good to go.

#cool cool, run 1 is spinning up!
#for run 2, want to read in the PnETSpeciesParameters and output w/ reduced HalfSat values.

pnetspparams <- read.table("LANDIS_stuff/subset_practicerun1_30May2022/PnETSpeciesParameters.txt",
                           header=TRUE, skip=1, sep="", fill=TRUE,
                           blank.lines.skip=TRUE,) #telling it to skip the first line when reading in the file
?read.table
View(pnetspparams)
#OK, i see what the issue was...didn't fill out the old field for my newly added species. that is fine!!
#let's get rid of that LAST column anyway...
pnetspparams <- pnetspparams[,-ncol(pnetspparams)]

#and now let's change HalfSat...
pnetspparams2 <- pnetspparams
View(pnetspparams2)
pnetspparams2$HalfSat <- as.numeric(pnetspparams2$HalfSat)*0.7
#just did some funky things to the comment lines @ the end; that is fine...
#BUT I do need to round it back to an integer!!
pnetspparams2$HalfSat <- round(as.numeric(pnetspparams2$HalfSat))
#ok, now let's write this new one to file!
write.table(x="LandisData     PnETSpeciesParameters", file="LANDIS_stuff/PnETSpeciesParameters2.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
write.table(x=pnetspparams2, 
            file="LANDIS_stuff/PnETSpeciesParameters2.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE)
#alright, this worked! Now let's try another run w/ these dif params.
#run 2 = all same inputs as run 1 EXCEPT w/ new HalfSat values.

#NOW, need to create some functions to automate the processes of comparing biomass vals...

#first one = read in species biomass maps
#still have the vector splist? 
splist #yes!
readinspagb <- function(specieslist, parent, year){
  #for ABOVEGROUND biomass per species.
  #specieslist = vector of species names to be read in
  #parent = filepath (character) to access species raster files
  #year = age of biomass-files to access
  for(i in 1:length(specieslist)){
    newlayer <- rast(paste0(parent, "/", specieslist[i], "AG/biomass-", year, ".img"))
    if(i==1){sp_agb <- rast(newlayer, nlyrs=length(specieslist))} 
    #in the first loop iteration, creating the output raster stack.
    sp_agb[[i]] <- newlayer
    #then adding the newest species to that stack.
  }
  return(sp_agb)
  #and returning the raster stack w/ all species biomass.
}

crop8S_run1_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run1/output/AGbiomass", year=0)
#ok, this seemingly worked (???) so let's try it!
splist
plot(crop8S_run1_spagb[[1]]) #balsam fir
plot(crop8S_run1_spagb[[4]]) #sugar maple
plot(crop8S_run1_spagb[[9]]) #white ash
#looks good!

#next steps will be to ASSOCIATE FIA plots & their biomass w/ the corresponding cells in LANDIS.
#definitely try to recycle some of this from when I did it yesterday, though????
View(S_biomass_FIA)
#for example, this looks like it's still usable! just need to crop to my subset
crop8S_biomass_FIA <- S_biomass_FIA[S_biomass_FIA$rowID %in% fiaplots_crop8S$ID,]
View(crop8S_biomass_FIA)
#hmmm now that one only has THREE plots......
#let's try reproducing what I did yesterday???
#FIRST, need to make fiaplots_crop8S only the ACTIVE plots!!!
fiaplots_crop8S <- fiaplots_crop8S[fiaplots_crop8S$mask_crop8S==1,]

##BELOW = COPIED CODE FROM YESTERDAY THAT I NEED TO MODIFY (AND PROB MAKE INTO FUNCTIONS?!)
#replacing "S_biomass_fromLANDIS" w/ crop8S_biomass_LANDIS
#and replacing "fiaplots_sf_S" with "fiaplots_crop8S"

#crop8S_biomass_LANDIS <- data.frame("cell"=fiaplots_crop8S$cells)
#names(crop8S_biomass_LANDIS) <- c(splist, "cell")
#crop8S_biomass_LANDIS #we currently just have the ONE column...
#NOTE: gonna do this in the function below instead.....

#let's make this next part into a function?!
#input the raster stack of species biomass + the species list; output a dataframe??
extractspagb <- function(specieslist, sprasters, cellsin){
  #specieslist = species list
  #sprasters = stack of species rasters (like those created as output of
  #the previous function I made!!!: readinspagb)
  #also, will CREATE the dataframe IN the function (but not in the loop) based on cellsin.
    #cellsin = vector list of cells from which to extract the biomass values.
  #          (e.g. this can be a column of another dataframe)
  dfin <- data.frame("cell"=cellsin)
  for(i in 1:length(specieslist)){
    #going thru species & attaching biomass of each as a new col to the dataframe
    #first, extracting the vals for the specified cells only
    agb_g_m2 <- terra::values(sprasters[[i]])[cellsin,]
    #attaching this as a new col to the existing df
    dfin <- cbind(dfin, agb_g_m2)
    #and just renaming that FINAL column:
    names(dfin)[ncol(dfin)] <- paste0(splist[i], "_gm2")
  }
  return(dfin)
}

#okayyy let's try it out now...
crop8S_biomass_LANDIS <- extractspagb(specieslist=splist,
                                      sprasters=crop8S_run1_spagb,
                                      cellsin=fiaplots_crop8S$cells)
View(crop8S_biomass_LANDIS)
#wooooo we did it Joe!!!
#okayyy, turns out this does NOT help me much bc 
#many of those species don't happen to be present in those 6 cells...
#I guess...for now, maybe just keep going w/ function-izing this process??

#cropping out a bunch of this that isn't needed; basically I just have biomass_S_FIA

crop8S_biomass_LANDIS <- merge(crop8S_biomass_LANDIS, fiaplots_crop8S[,c("cells", "ID")],
                              by.x="cell", by.y="cells")

#gonna do this next part in a loop...
for (i in crop8S_biomass_LANDIS$ID){
  crop8S_biomass_LANDIS$pltID[crop8S_biomass_LANDIS$ID==i] <- 
    fiaplots_sf$pltID[i]
}

#also need to subset to just the plots in LANDIS...
crop8S_biomass_FIA <- S_biomass_FIA[S_biomass_FIA$pltID %in% crop8S_biomass_LANDIS$pltID,]
View(crop8S_biomass_FIA) #looks gr8!

#now to aggregate mean vals- doing this PER RUN:
meanagb_crop8S <- data.frame(species=splist,
                             fia_agb=rep(0))
#and gonna do another function I guess???
#tho ACTUALLY, I unforunately set this up badly to start with...so maybe not...
#compromise: I'll just add the FIA biomass manually for now,
#since it's the same for the entire subset,
#then do a function to add the LANDIS biomass.
for(i in 1:length(splist)){
  meanval <- mean(crop8S_biomass_FIA[,42+i])
  meanagb_crop8S$fia_agb[i] <- meanval
  #meanval2 <- mean(crop8S_biomass_LANDIS[,1+i])
  #meanbio_sp_S$landis_bio[i] <- meanval2
}

#alright, now this is a function to return a vect of mean species agb for selected cells
#from the LANDIs biomass dataframe,
#and then the command I'll use will be to append it to the existing mean df.
bindmeanagb <- function(specieslist, input_agb){
  #specieslist = list of species, as before
  #input_agb = dataframe w/ a column for each species to summarize
  #NOTE: the way this function is written will presume that SPECIES START IN SECOND COL.
  outputcol <- rep(0, times=length(specieslist))
  for(i in 1:length(specieslist)){
  meanval <- mean(input_agb[,1+i])
  outputcol[i] <- meanval
  }
  return(outputcol)
}
#OK let's try this:
meanagb_crop8S$landis_agb_run1 <- bindmeanagb(specieslist=splist, input_agb=crop8S_biomass_LANDIS)
#ALRIGHT, looks good!!! 
#NOW, let's try this again w/ run 2 to see how values compare when I decreased HalfSat:

#gonna use the functions I made above :) 
crop8S_run2_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run2/output/AGbiomass", year=0)
crop8S_biomass_LANDIS_run2 <- extractspagb(specieslist=splist,
                                      sprasters=crop8S_run2_spagb,
                                      cellsin=fiaplots_crop8S$cells)
View(crop8S_biomass_LANDIS_run2)
#hmmm ok...looking good I think??
#next copying the parts of this that I haven't yet made into functions:
crop8S_biomass_LANDIS_run2 <- merge(crop8S_biomass_LANDIS_run2, fiaplots_crop8S[,c("cells", "ID")],
                               by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in crop8S_biomass_LANDIS_run2$ID){
  crop8S_biomass_LANDIS_run2$pltID[crop8S_biomass_LANDIS_run2$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanagb_crop8S$landis_agb_run2 <- bindmeanagb(specieslist=splist, input_agb=crop8S_biomass_LANDIS_run2)
#ookay...looks like decreasing HalfSat values across the board DID result 
#in increasing biomass, for MOST species. But...
#a) not by enough to get close to FIA plot vals
#and b) some OTHER species no longer appeared to grow/initialize(?) at all!!
meanagb_crop8S$landis_agb_run2 / meanagb_crop8S$landis_agb_run1
#so: reducing HalfSat values to 70% of previous val
#ended up increasing mean AGB for present species by x1.15,
#BUT it also ELIMINATED some species entirely.
#maybe let's try changing some GLOBAL parameters for the next run??
#for example: change MaintResp in generic pnet params (lower, to increase total biomass...)
#and also maybe try changing FolN??

#alright, RUN 3 will be the same as RUN 1 re: HalfSat & other parameters,
#but with an updated MaintResp value of 0.0007 (from 0.001)
#gonna go ahead and make this change just in the file since it's a single line, but documenting it here.
#while that's running, next ACTION ITEM = document what I've done today IN GOOGLE DOC!

#current runs = taking ~8 minutes from start to spin up biomass, then another ~11 min to grow cohorts to age 5.
View(meanagb_crop8S)
#OK, now let's take a look at run 3....
#gonna use the functions I made above :) 
crop8S_run3_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run3/output/AGbiomass", year=0)
crop8S_biomass_LANDIS_run3 <- extractspagb(specieslist=splist,
                                           sprasters=crop8S_run3_spagb,
                                           cellsin=fiaplots_crop8S$cells)
View(crop8S_biomass_LANDIS_run3)
#hmmm ok...looking good I think??
#next copying the parts of this that I haven't yet made into functions:
crop8S_biomass_LANDIS_run3 <- merge(crop8S_biomass_LANDIS_run3, fiaplots_crop8S[,c("cells", "ID")],
                                    by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in crop8S_biomass_LANDIS_run3$ID){
  crop8S_biomass_LANDIS_run3$pltID[crop8S_biomass_LANDIS_run3$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanagb_crop8S$landis_agb_run3 <- bindmeanagb(specieslist=splist, input_agb=crop8S_biomass_LANDIS_run3)
#alright, ended up same as run1...

#EXEC DECISION: next run will try REPLACING all FolN values with those from 
#Gustafson in the calibration guide for PnET-Succession, v4.0 
#(and referring back to v3.0 values as needed for other sp not listed in v4.0!)
#this will be in file called: PnETSpeciesParameters3
#again, just editing this one in-file...
#but will still have SAME halfsat & other values as run 1, again to compare against that as a baseline!

#alright, I think that once I do this run, I should email Jane with what I've found so far!
#and then move back to the MANAGEMENT stuff- revisit mgmt/harvest prescriptions 
#to share with Tony & Nate for feedback,
#and also look WITHIN MY SUBSET AREA at how I want to place "treated" ash
#and where I have ash-rich stands...

#OK, now let's take a look at run 4....
#gonna use the functions I made above :) 
#note: it would likely be simpler in the long run to make a mega-function calling all of these functions lol.
#but I'm just not gonna do that for now!
crop8S_run4_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run4/output/AGbiomass", year=0)
crop8S_biomass_LANDIS_run4 <- extractspagb(specieslist=splist,
                                           sprasters=crop8S_run4_spagb,
                                           cellsin=fiaplots_crop8S$cells)
View(crop8S_biomass_LANDIS_run4)
#hmmm ok...looking good I think??
#next copying the parts of this that I haven't yet made into functions:
crop8S_biomass_LANDIS_run4 <- merge(crop8S_biomass_LANDIS_run4, fiaplots_crop8S[,c("cells", "ID")],
                                    by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in crop8S_biomass_LANDIS_run4$ID){
  crop8S_biomass_LANDIS_run4$pltID[crop8S_biomass_LANDIS_run4$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanagb_crop8S$landis_agb_run4 <- bindmeanagb(specieslist=splist, input_agb=crop8S_biomass_LANDIS_run4)

#once again, this turned out EXACTLY THE SAME as run 1....but why?!?!
#that just seems.....IDK a bit fishy?!
#let's look at a different thing to test this:
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run1/output/AGbiomass/fraxamerAG/biomass-0.img")))
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run2/output/AGbiomass/fraxamerAG/biomass-0.img")))
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run3/output/AGbiomass/fraxamerAG/biomass-0.img")))
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run4/output/AGbiomass/fraxamerAG/biomass-0.img")))
#hmm.....ok I guess they are they same?!
#that just seems a bit hard to believe honestly......

#OK, I think I'll try using the Hubbard Brook PAR values in place of the TerraClimate ones.
#going to do that swap in the ecoregions script...maybe after eating??
#JK, I'm just doing it now LOL!

#finally, looking @ run 5 (same as run 1, but using Hubbard Brook PAR values):
crop8S_run5_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run5/output/AGbiomass", year=0)
crop8S_biomass_LANDIS_run5 <- extractspagb(specieslist=splist,
                                           sprasters=crop8S_run5_spagb,
                                           cellsin=fiaplots_crop8S$cells)
View(crop8S_biomass_LANDIS_run5)
#next copying the parts of this that I haven't yet made into functions:
crop8S_biomass_LANDIS_run5 <- merge(crop8S_biomass_LANDIS_run5, fiaplots_crop8S[,c("cells", "ID")],
                                    by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in crop8S_biomass_LANDIS_run5$ID){
  crop8S_biomass_LANDIS_run5$pltID[crop8S_biomass_LANDIS_run5$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanagb_crop8S$landis_agb_run5 <- bindmeanagb(specieslist=splist, input_agb=crop8S_biomass_LANDIS_run5)
#OKAY- this got a few species MUCH closer!
#but others are still QUITE far off....
#sooo, maybe the way to go is to adjust FolN values WITH these new climate vars??

#next run (6) = using Hubbard Brook PAR vals PLUS higher FolN vals!
#run 6:

crop8S_run6_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run6/output/AGbiomass", year=0)
crop8S_biomass_LANDIS_run6 <- extractspagb(specieslist=splist,
                                           sprasters=crop8S_run6_spagb,
                                           cellsin=fiaplots_crop8S$cells)
View(crop8S_biomass_LANDIS_run6)
#next copying the parts of this that I haven't yet made into functions:
crop8S_biomass_LANDIS_run6 <- merge(crop8S_biomass_LANDIS_run6, fiaplots_crop8S[,c("cells", "ID")],
                                    by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in crop8S_biomass_LANDIS_run6$ID){
  crop8S_biomass_LANDIS_run6$pltID[crop8S_biomass_LANDIS_run6$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanagb_crop8S$landis_agb_run6 <- bindmeanagb(specieslist=splist, input_agb=crop8S_biomass_LANDIS_run6)
#OKAY- this got a few species MUCH closer!
#but others are still QUITE far off....
#so, I think next step = try re-tuning w/ the entire S landscape,
#with these new parameters
#and ALSO try looking @ growth rate from yr 0 to yr 5...
#THEN it'll come down to tweaking FolN vals to adjust RELATIVE growth...
#we'll also want to zoom in to individual cells & look @ the competitive dynamics
#between species on a site level...
#and FINALLY, the last step will be tweaking the ESTABLISHMENT parameters
#(will do that part in the subset, and run for the entire time scale to view
#ending # of cohorts, and use PnETOutputSites for ~ 10-50 (?) cells
#to examine establishment params)

#let's compare TOTAL landscape AGB of ash again:
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run1/output/AGbiomass/fraxamerAG/biomass-0.img")))
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run2/output/AGbiomass/fraxamerAG/biomass-0.img")))
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run5/output/AGbiomass/fraxamerAG/biomass-0.img")))
mean(values(rast("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/crop8S_test/run6/output/AGbiomass/fraxamerAG/biomass-0.img")))
#hmmmMMMMmmm ok... maybe JUST increase par then??
#I think we really do need to do this across the landscape, w/ the 2 sets of FolN values...
#and then can take a closer look @ my own species.





# looking at ash stands in landscape subset + where to place treated ash! -------------------------------
#
#I may need to get arcmap involved here, too,
#for locating in SPECIFIC orientations across a stand
#e.g. when trying to retain ash's ecosystem functions.

#read in rasters in a stack:
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

framstands_crop8S <- data.frame("stand"=unique(crop8s_vals$stands),
                               "num_plots"=rep(0),
                               "num_ash_plots"=rep(0))
View(framstands_crop8S)
#removing zeros:
framstands_crop8S <- framstands_crop8S[-1,]
crop8s_vals_nozero <- crop8s_vals[crop8s_vals$stands!=0,]

#(re)making this function:
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#NOTE: also RE-RAN the code from LAST WEEKEND (??!!?) that generates the init_comm_df
#and whiteash_codes dataframes!
#also saving init_comm_df so I DON'T have to re-generate that :)
write.csv(init_comm_df, "LANDIS_stuff/initcomms_dataframe.csv", row.names=FALSE)
#whiteash_codes <- init_comm_df$mapcode[init_comm_df$species=="fraxamer"]
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
standsperma_crop8S <- table(framstands2_crop8S$mgmtarea)
standsperma_crop8S
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
aggregate()

#using this handy tidyverse function to summarize what I want for each MA:
#number of ash stands, total size of those stands (in ha), 
#and avg size of those stands (in ha)
framstands_crop8s_byma <- framstands2_crop8S %>% 
  group_by(mgmtarea) %>% 
  summarise(numstands = n(),
            totalha = sum(area_ha),
            meanha = mean(area_ha))

framstands_crop8s_byma
#also adding the name for each category to add context:
framstands_crop8s_byma$owntype <- 
  c("priv_fam", "priv_corp", "priv_other", "pub_fed", "pub_state", "roadside")
View(framstands_crop8s_byma)

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

#also, maybe do a reclass map w/ ash plots in a dif color?? 
crop8s
ashstandsreclass_crop8s <- ifel(test=crop8s[[1]]==0, #first: if inactive,
                                yes=0, #value stays as zero
                                       #then if it is active, 
                                      #check if it's plot w/ SOME ash,
                                no=ifel(test=(crop8s[[6]] %in% whiteash_codes),
                                        #if so, assign val of 10,
                                        yes=10,
                                        #and if not, go with mgmtarea code of 1-6
                                        no=crop8s[[5]]))
#if this doesn't work- maybe try extracting by white ash plots instead 
#so there's another raster to compare to??
#and yeah, it's indeed not working!
?extract
#not sure if this will work either...
#so gonna call it a night for now...
#UNLESS maybe my new outputs are done?!
#UPDATE: indeed they are!
#each took about 4.5 hours to complete.

#OK, let's take a look.....

# switching gears again, back to analyze S region biomass outputs w/ dif inputs/params -------------------------------

#the df to attach to is called meanbio_sp_S
#using the same functions but WILL NEED TO ALTER THINGS SO IT'S DRAWING FROM
#CELLS OF ALL THE FIA PLOTS IN THE SOUTHERN STUDY REGION
bioS_run1_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_S/run1/output/AGbiomass", year=0)
#bioS_run1_spagb
#plot(bioS_run1_spagb[[1]])
#next extracting cell VALUES for the selected plots/active cells corresponding w/ FIA plots
bioS_LANDIS_run1 <- extractspagb(specieslist=splist,
                                           sprasters=bioS_run1_spagb,
                                           cellsin=fiaplots_sf_S$cells)
View(bioS_LANDIS_run1)
#next copying the parts of this that I haven't yet made into functions:
bioS_LANDIS_run1 <- merge(bioS_LANDIS_run1, fiaplots_sf_S[,c("cells", "ID")],
                                    by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in bioS_LANDIS_run1$ID){
  bioS_LANDIS_run1$pltID[bioS_LANDIS_run1$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanbio_sp_S$landis_agb_run1 <- bindmeanagb(specieslist=splist, input_agb=bioS_LANDIS_run1)
#ok, interesting.......across the whole landscape, just increasing par vals actually is LOWER agb...
#altho remember what we're comparing it to before (first landis_bio run) is the TOTAL biomass, not agb! so not a good comparison

#now do this all again for run 2:
bioS_run2_spagb <- readinspagb(splist, "C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/practice_runs/biomass_S/run2/output/AGbiomass", year=0)
#bioS_run2_spagb
#plot(bioS_run2_spagb[[1]])
#next extracting cell VALUES for the selected plots/active cells corresponding w/ FIA plots
bioS_LANDIS_run2 <- extractspagb(specieslist=splist,
                                 sprasters=bioS_run2_spagb,
                                 cellsin=fiaplots_sf_S$cells)
View(bioS_LANDIS_run2)
#next copying the parts of this that I haven't yet made into functions:
bioS_LANDIS_run2 <- merge(bioS_LANDIS_run2, fiaplots_sf_S[,c("cells", "ID")],
                          by.x="cell", by.y="cells")
#gonna do this next part in a loop...
for (i in bioS_LANDIS_run2$ID){
  bioS_LANDIS_run2$pltID[bioS_LANDIS_run2$ID==i] <- 
    fiaplots_sf$pltID[i]
}
#and then the last function to ADD a col to the mean dataframe:
meanbio_sp_S$landis_agb_run2 <- bindmeanagb(specieslist=splist, input_agb=bioS_LANDIS_run2)
#ok GREAT, run 2 values are all in the right BALLPARK, now just need to tune
#species relative to each other!!!

#next thing to find out = whether TOTAL mean (landscape-level) biomass val per sp
#from the SUBSET LANDSCAPE is a good approximation / representative sample
#of the corresponding southern region-wide FIA plot sample.
#first to see if the TOTAL mean vals are similar:
splist
#subset - raw mean biomass of sugar maple:
mean(values(crop8S_run5_spagb[[4]]))
#full S region - raw mean biomass of sugar maple:
mean(values(bioS_run1_spagb[[4]])) #ok that's very different...
#but is that b/c it's a drastically different amount of active cells?
#I guess could divide those subset means by the % active cells...
#FROM subset landis-log: 
# "Sites: 197,765 active (79.1%), 52,235 inactive (20.9%)"
#so, 
mean(values(crop8S_run5_spagb[[4]]))/0.791

#what about:
mean(values(crop8S_run6_spagb[[4]]))/0.791
#oof okay, that is NOT a representative mean val! :) 
#I think my next moves on this stuff should WAIT until I have time to touch base 
#again with Jane.
#BUT, got some valuable info, validating overall trends that we are TUNING 
#FolN values in light of the higher PAR values to get competitive interactions right!
#in the meantime: work more on the actual HARVEST PRESCRIPTIONS (priority!!!),
#and revise ch. 1 writing,
#and also work on SWAPS PROJECT STUFF