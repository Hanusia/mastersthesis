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
View(whiteash_count_N)
#removing zeros:
whiteash_count_N <- whiteash_count_N[-1,]
for(i in 1:nrow(stack_N_vals)){
  for(j in 1:length(unique(stack_N_vals$stand))){
    if(stack_N_vals$stand[i]==whiteash_count_N$stand[j]){
      #if stand values match up, add 1 to total plot count.
    whiteash_count_N$num_plots[j] <- whiteash_count_N$num_plots[j] + 1
     #and if THAT is true, ALSO tally how many of those plots have ash...
    if(stack_N_vals$initcomm[i] %in% whiteash_codes){
      whiteash_count_N$num_ash_plots[j] <- whiteash_count_N$num_ash_plots[j] + 1
    }
    }
  }
}
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
  whiteash_count_N$num_plots[whiteash_count_N$stand==j] %+=% 1
  #and if THAT is true, ALSO tally how many of those plots have ash...
  if(stack_S_vals_nozero$initcomm[i] %in% whiteash_codes){
    whiteash_count_N$num_ash_plots[whiteash_count_N$stand==j] %+=% 1
  }
  # }
  #}
}

#next steps will be finding PROPORTION of ash cells in stands.
#but also, first filtering by stands of a certain SIZE (~10 acres/4 ha?):
whiteash_count_N2 <- whiteash_count_N[whiteash_count_N$num_plots>=45,]
whiteash_count_N2$prop_ash <- whiteash_count_N2$num_ash_plots/whiteash_count_N2$num_plots
View(whiteash_count_N2)
ash_stands_S <- whiteash_count_S[whiteash_count_S$num_plots>=45,]
ash_stands_S$prop_ash <- ash_stands_S$num_ash_plots/ash_stands_S$num_plots

ash_stands_N <- whiteash_count_N2[whiteash_count_N2$prop_ash>=0.15,]
View(ash_stands_N)
ash_stands_S <- ash_stands_S[ash_stands_S$prop_ash>=0.15,]

#and NOW, we can sort them by management area/type...
#that may be the last level of sorting??

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

