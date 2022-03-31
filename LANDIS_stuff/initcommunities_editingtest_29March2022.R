# --------------------------------------------------
# testing functions to edit initial communities map + txt file
# 29 Mar 2022
# HH
# --------------------------------------------------
#

#using EXAMPLE files from the PnET Succession example package,
#I am testing out ways I want to alter my initial communities input files
# (text and map) specifically to "treat" resistant ash.

#first, inputting those files:
#starting with the map!

library(terra)
initmap <- rast("LANDIS_stuff/initial-communities-example.gis")
#warning message that we can't get the projection, but that's OK
#Because LANDIS doesn't care about the map projection!
initmap
plot(initmap) #I see values from 1-6 representing our map code types

#note: these example files do NOT include harvest extension, therefore
#no stand/management area map files. But--maybe I could mock-up my own?
#then once I do that, can combine them into a "raster stack" (per the
#terra package) to cross-reference map code AND mgmt type, for example...

#creating a raster, basically giving it same dimensions as initmap
maexamp <- standexamp <- rast(nrow=99, ncol=99, nlyrs=1,
                              xmin=50, xmax=9950, ymin=50, ymax=9950)
#now let's make sure they match up...
compareGeom(initmap, maexamp)
compareGeom(initmap, standexamp)
#looks good!

#next, assigning fake values to these rasters:
ncell(maexamp) #num cells in this raster
halfval <- round(ncell(maexamp)/2) #num half the cells in this raster
values(maexamp) <- c(rep(x=1, times=halfval), rep(x=2, times=halfval+1))
#basically divvied up into 2 management areas, 1 on top and 2 on bottom
plot(maexamp) #looks right!
#next need to do a similar thing w/ stand codes (also divide simply):
values(standexamp) <- values(maexamp) #start by giving same values as MAs
values(standexamp) <- c(rep(x=11, times=halfval/2), #then divide into 4 stands
                        rep(x=12, times=halfval/2),
                        rep(x=21, times=halfval/2),
                        rep(x=22, times=halfval/2+1))
plot(standexamp) #so far so good!!

#now, onto the text file:
#so, for this example, it's probably possible/easy to just look at 
#individual mapcodes and manually choose which ones I want to replace.
#but for my own data, there will be so MANY mapcodes that 
#I should figure out a way to sort through those with code as well.
#seems like the main issue is they're not in a table currently?

mapcodes <- read.csv("LANDIS_stuff/initial_communities_example.csv")
#this is the initial txt file plopped into Excel, divided by spaces/tabs 
#into separate cells, then saved as a csv
View(mapcodes)
names(mapcodes) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7") #renaming cols
#next, gonna set up a loop to re-create this in a more organized fashion
which(mapcodes$c1=="MapCode") #gives rows where MapCode starts
mapcodes2 <- data.frame("code"=NA,  #setting up empty second dataframe 
                        "species"=NA, #to hold organized values
                        age=NA)
View(mapcodes2)

#now to set up a for loop...
codestart <- which(mapcodes$c1=="MapCode") #list of rows where MapCode starts
for(i in 1:nrow(mapcodes)){ #iterating thru each row of input table
  for(j in 1:length(codestart)){
    currentcode <- codestart[j] #setting up the bounds of each mapcode
    nextcode <- codestart[j+1]
    if(i>currentcode & i<nextcode-1){ 
      #for the lines between one mapcode and another:
      mapcodes2$code[i] <- mapcodes[currentcode,]
    }
  }
}


#INTERLUDE: trying this a different way w/ separate_rows function!
library(tidyverse)
mapcodesv2 <- read.csv("LANDIS_stuff/initial_communities_example2.csv")
View(mapcodesv2)
names(mapcodesv2) <- c("c1", "c2")
#DELETE rows w/ the map code type "name" so they are no longer in the mix:
typenames <- grep(">>", x=mapcodesv2$c1, value=TRUE) #finding rows containing ">>"
mapcodesv2 <- mapcodesv2[!(mapcodesv2$c1 %in% typenames),] #and removing those rows from the df
#great! So now we JUST have the mapcodes and the species/ages.

#ok let's try this separate_rows thing...
mapcodesv2 <- separate_rows(data=mapcodesv2, c2, sep=" ")
#so far so good...
#next, try the same thing with the for loop, but JUST to add the mapcode to each...

mapcodesv3 <- data.frame("code"=rep(NA, times=nrow(mapcodesv2)),  #setting up empty second dataframe 
                        "species"=rep(NA, times=nrow(mapcodesv2)), #to hold organized values
                        "age"=rep(NA, times=nrow(mapcodesv2)))
View(mapcodesv3)

#now to set up a for loop...
#how to best index this when the cell just CONTAINS the mapcode string and isn't entirely COMPRISED of it??
#maybe, first need to regex to split up the mapcode cells??
#need to go meet but this is what I'll work with when I get back to this!!!

codestart <- grep("MapCode", x=mapcodesv2$c1, value=FALSE) #list of ROWS containing the "mapcode" value
for(i in 1:nrow(mapcodesv2)){ #iterating thru each row of input table
  for(j in 1:length(codestart)){
    currentcode <- codestart[j] #setting up the bounds of each mapcode
    if(j<length(codestart)){
      nextcode <- codestart[j+1] #continuing to set up bounds (if it's not the last one)
    if(i>currentcode & i<(nextcode-1)){ 
      #for the lines between one mapcode and another:
      mapcodesv3$code[i] <- mapcodesv2[currentcode,"c1"]
      mapcodesv3$species[i] <- mapcodesv2[i, "c1"]
      mapcodesv3$age[i] <- mapcodesv2[i, "c2"]
    }
    } else { #and then for the final set of mapcodes:
      mapcodesv3$code[i] <- mapcodesv2[currentcode,"c1"]
      mapcodesv3$species[i] <- mapcodesv2[i, "c1"]
      mapcodesv3$age[i] <- mapcodesv2[i, "c2"]
    }
  }
}

#NOTE: the loop isn't working; also, I think I'm just gonna pause on this
#Because I'm fairly confident that I can do what I need to do,
#And it will just need to be tailored to the actual dataset regardless! :)
