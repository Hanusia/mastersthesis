# --------------------------------------------------
# processing climate files by ecoregion to get in LANDIS format
# 31 May 2022
# HH
# --------------------------------------------------
#

library(tidyverse)

#reading in historical climate data file from Jane
histclim <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/Jane_from_GEE/processed_climate_tables/VTMA_TopoWx_Terraclimate_historical_pnetclim.csv")
#and also the CO2 data
histco2 <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/co2_mm_mlo_cleaned.csv")

#first step will be to apply GLOBAL changes
#(including: update DOY to month,
# add CO2 val to each year/month, 
# and change years to encompass past + future (for now)).

#then, I'll write a loop to take in that file & 
#output different, individual text files by ecoregion.

View(histclim)
#first will need to convert doy to month...
unique(histclim$doy) #uses same 12 values
round(histclim$doy[1:12]/30) #this works for all months EXCEPT jan.
histclim$month <- round(histclim$doy/30)
histclim$month[histclim$month==0] <- 1
unique(histclim$month)
#alright, good on that now...
#now to add CO2 data...
View(histco2)
nrow(histclim)
histclim <- merge(x=histclim, y=histco2[,1:3],
                  by=c("year","month"), 
                  all.x=TRUE, all.y=FALSE)
#looks like this did work, yay!
#next want to rename columns...
names(histclim)
#and also cutting out doy col, no longer needed... currently 4th col
histclim <- histclim[, -4]
names(histclim) <- c("Year", "Month", "Ecoregion", "Tmax", "Tmin", "PAR", "Prec", "CO2")
#looks good!
glimpse(histclim)
max(histclim$Year)
#next, need to replace years 1970 & 2016 w/ ranges that encapsulate a whole time series...
histclim$Year <- as.character(histclim$Year) 
histclim$Year[histclim$Year==1970] <- "1800-1970"
histclim$Year[histclim$Year==2016] <- "2016-2150"
#OK, now I think all global changes have been made...
#Now I think it's time for a loop that SUBSETS the climate data for ecoregion & makes it into a separate file!

unique(histclim$Ecoregion)
length(unique(histclim$Ecoregion)) #33 ecoregions (not counting 0 or the roadside copy ones)- that is correct

#let's try this with just one ecoregion FIRST (subsetting index val by [1]):
for(i in unique(histclim$Ecoregion)[1]){ #in this case, i is the ACTUAL ECOREGION ITSELF!!
  #let's just test something first...
  #print(head(histclim[histclim$Ecoregion==i,]))
  #print(i)
  #seems to be working; let's proceed!
  #this subsets the climate table to data rows of a SINGLE ecoregion + all columns EXCEPT for ecoregion!
  clim_subset <- histclim[histclim$Ecoregion==i,-3]
  write.table(x=clim_subset, file=paste("LANDIS_stuff/subset_practicerun1_30May2022/histclim_by_ecoregion/eco_", i, "_clim_31May2022.txt", sep=""),
              sep="        ", quote=FALSE, append=FALSE,)
}
#OK, this worked, but (not sure if it actually matters) months are out of order.
#testing w/ arrange function:
histclim[1:50,]
tester <- histclim %>% arrange(Month) 
#says it can't use on a double class...
#?arrange()
View(tester)
#looks right; let's rearrange histclim that way & then redo the test loop above!
histclim <- histclim %>% arrange(Month) 
#but ALSO need to arrange by year FIRST...
histclim <- histclim %>% arrange(Year) 
#OK, now it's looking how I want it to!!

#now for the REAL full loop:
for(i in unique(histclim$Ecoregion)){ #in this case, i is the ACTUAL ECOREGION ITSELF!!
  #let's just test something first...
  #print(head(histclim[histclim$Ecoregion==i,]))
  #print(i)
  #seems to be working; let's proceed!
  #this subsets the climate table to data rows of a SINGLE ecoregion + all columns EXCEPT for ecoregion!
  clim_subset <- histclim[histclim$Ecoregion==i,-3]
  write.table(x=clim_subset, file=paste("LANDIS_stuff/subset_practicerun1_30May2022/histclim_by_ecoregion/eco_", i, "_clim_31May2022.txt", sep=""),
              sep="        ", quote=FALSE, append=FALSE, row.names=FALSE)
}
#whoop, we got it!!
#NOW, just need to append these climate tables to the ecoregions file: 

#first, input the ecoregions file
#then, get ecoregion 'index' e.g. number, or -200 for roadside ones
#then do another for loop appending the filename of the correct climate file to the ecoregion table
#and finally save the new ecoregions table as a .txt as well.
#I will do this when I get back!!

#Alright, back and ready to do this
ecoregion_params <- read.csv("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/EcoregionParameters_withroadside_17March2022.csv")
View(ecoregion_params)
names(ecoregion_params)[1] <- "EcoregionParameters"
ecoregion_params <- ecoregion_params[ecoregion_params$EcoregionParameters!=0,]
#creating a for loop to add to the climate file names:
j=0
for(i in ecoregion_params$EcoregionParameters){
j <- ifelse(i<200, i, i-200) #modifying 'j' for the roadside ecoregions
  #then using j to plug into the correct file name for each ecoregion
  ecoregion_params$ClimateFileName[ecoregion_params$EcoregionParameters==i] <- paste("histclim_by_ecoregion/eco_", j, "_clim_31May2022.txt", sep="")
}
#lookin' good :)

write.table(x=ecoregion_params, 
            file="LANDIS_stuff/subset_practicerun1_30May2022/EcoregionParameters_subsetS4.txt", 
            sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)

ecoregions_file <- data.frame("Active"=c(rep(x="yes", times=66),"no"),
                              "MapCode"=c(ecoregion_params$EcoregionParameters,0),
                              "Name"=c(ecoregion_params$EcoregionParameters,0),
                              "Description"=rep("description")
)
View(ecoregions_file)                              

#NEXT JUST NEED TO SAVE THIS FILE
#AND THEN BRING IN ANY OTHER FILES NEEDED TO DO THIS PRACTICE RUN (mostly just scenario I think??)
 write.table(x=ecoregions_file, 
             file="LANDIS_stuff/subset_practicerun1_30May2022/Ecoregions_subsetS4.txt", 
             sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)                            
#DON'T FORGET TO REMOVE ROWNAMES!!!
 #had to go back and do this in climate files too......!!!
#update: just did that. Soo, after this meeting with Michelle & the ORISE fellows,
 #I think I just need to get the rest of my input files together to do this run?!
 #OK, also just need to add column of ecoregion "description"
                              
 
 # Doing this FOR REAL/FINAL w/ climate tables from Jane! -------------------------------
  #Wednesday, June 1st, 2022
 
 #need a loop(s) that will:
 # 1) read in all ecoregion input files
 # 2) randomize + append additional yearly climate vars from the past 30 yrs going up to 2120
 # 3) (probably just also) change headers to be capitalized 'Year' and 'Month' #no longer need to do this; Jane did!
 # 4) write those completed files to a new folder in github
 # 5) (maybe) repeat this process a couple times (or at least steps 2 + 4) w/ dif random seeds to get a few dif options
 # 6) THEN, also need a loop to append these new filenames to the EcoregionParameters file & spit out one of those as well!!
 
 length(unique(histclim$Ecoregion))
 #just use this as the variable:
 ecoregion <- unique(histclim$Ecoregion)
ecoregion 

### this space is for testing out parts of the for loop ###

#first, testing table read-in:
y <- 1
input <- read.table(paste("C:/Users/theha/Documents/layers_for_LANDIS/climate_files/histclimate_ecoregion_fromJane/TerraClimate_historical_eco_", y, ".txt", sep=""),
                    header=TRUE)
View(input)

# THIS NEXT LINE IS JUST TO ACCOUNT FOR THE PARTIAL YEAR OF 1958'S SEPARATE DATA & REMOVE IT:
input <- input[-c(13:22),]

#next need to add columns
#?rbind
future <- input[0,] #adding just column names to new dataframe
#View(future)
#maybe choose + add the random years data FIRST, then just change the year label after...
for(i in 1:100){
  randyr <- sample(x=1991:2020, size=1, replace=TRUE) #choosing a random year from the past 30, w/ replacement
  randclim <- input[input$Year==randyr,] #subsetting the chosen year's monthly climate data
future <- rbind(future,randclim) #and adding the chosen random year onto the 'future' dataframe
}
future$Year <- rep(x=2021:2120, each=12) #now, renaming the years to the range of future ones
output <- rbind(input, future)
View(output)
#and then we'd just need to write the 'output' table to file:
write.table(x=output, file=paste("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", y, ".txt", sep=""),
            sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)
#looks great, just the ONLY issue I see now is that it still includes the 1958 yr separately....

### now that we've tested it and each piece is working, let's try it out...

for(y in ecoregion){ #in this case, y is the ACTUAL ECOREGION CODE/NUMBER ITSELF!!
  input <- read.table(paste("C:/Users/theha/Documents/layers_for_LANDIS/climate_files/histclimate_ecoregion_fromJane/TerraClimate_historical_eco_", y, ".txt", sep=""),
                      header=TRUE)

  # THIS NEXT LINE IS JUST TO ACCOUNT FOR THE PARTIAL YEAR OF 1958'S SEPARATE DATA & REMOVE IT:
  input <- input[-c(13:22),]
  
  future <- input[0,] #adding just column names to new dataframe
  #choose + add the random years data FIRST, then just change the year label after...
  for(i in 1:100){
    randyr <- sample(x=1991:2020, size=1, replace=TRUE) #choosing a random year from the past 30, w/ replacement
    randclim <- input[input$Year==randyr,] #subsetting the chosen year's monthly climate data
    future <- rbind(future,randclim) #and adding the chosen random year onto the 'future' dataframe
  }
  future$Year <- rep(x=2021:2120, each=12) #now, renaming the years to the range of future ones
  output <- rbind(input, future)
  #and then last step = write the 'output' table to file:
  write.table(x=output, file=paste("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", y, ".txt", sep=""),
              sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)
}
 

#UPDATE BELOW:
#rewriting this loop so it's the same year chosen for each ecoregion, for each future year.
#e.g. : the year 2030 is randomly selected to get climate data from 1998, and this is the same for each ecoregion.
#as opposed to the way I did it above, where each ecoregion got a different year independently.

#SO, starting off by determining the source years ahead of time: 
#randyrs1 <- rep(x=sample(x=1991:2020, size=1, replace=TRUE), times=100) #this did NOT work; each year was 2008
randyrs1 <- replicate(100, sample(x=1991:2020, size=1, replace=TRUE)) #okay now THIS looks good!!
length(randyrs1)

for(y in ecoregion){ #in this case, y is the ACTUAL ECOREGION CODE/NUMBER ITSELF!!
  input <- read.table(paste("C:/Users/theha/Documents/layers_for_LANDIS/climate_files/histclimate_ecoregion_fromJane/TerraClimate_historical_eco_", y, ".txt", sep=""),
                      header=TRUE)
  
  # THIS NEXT LINE IS JUST TO ACCOUNT FOR THE PARTIAL YEAR OF 1958'S SEPARATE DATA & REMOVE IT:
  input <- input[-c(13:22),]
  
  future <- input[0,] #adding just column names to new dataframe
  #choose + add the random years data FIRST, then just change the year label after...
  for(i in 1:100){
    #this time around, using pre-established vector of years to sample from for each future year, same for each ecoregion.
    randclim <- input[input$Year==randyrs1[i],] #subsetting the chosen year's monthly climate data
    future <- rbind(future,randclim) #and adding the chosen random year onto the 'future' dataframe
  }
  future$Year <- rep(x=2021:2120, each=12) #now, renaming the years to the range of future ones
  output <- rbind(input, future)
  #and then last step = write the 'output' table to file:
  write.table(x=output, file=paste("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", y, ".txt", sep=""),
              sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)
}
#OK now this is what we've got!
#FOR FUTURE LOOPS/ITERATIONS/VERSIONS OF THIS, make sure to save to a different folder instead!!


#NEXT TASK = update the EcoregionParameters file!

 #PREVIOUS CODE FROM YESTERDAY BELOW:
 # JUST NEED TO ADD ADDITIONAL HEADER ABOVE THE COL NAMES

ecoregion_params <- read.csv("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/EcoregionParameters_withroadside_17March2022.csv")
View(ecoregion_params)
names(ecoregion_params)[1] <- "EcoregionParameters"
names(ecoregion_params)[10] <- "WinterSTD"
ecoregion_params <- ecoregion_params[ecoregion_params$EcoregionParameters!=0,]
#creating a for loop to add to the climate file names:
j=0
for(i in ecoregion_params$EcoregionParameters){
  j <- ifelse(i<200, i, i-200) #modifying 'j' for the roadside ecoregions
  #then using j to plug into the correct file name for each ecoregion
  ecoregion_params$ClimateFileName[ecoregion_params$EcoregionParameters==i] <- paste("TerraClimate_historical_eco_", j, ".txt", sep="")
}
#lookin' good :)

#NOTE: to get that additional header...maybe create a text doc with just the header, then append the ecoregion_params file to it??
write.table(x="LandisData     EcoregionParameters", file="LANDIS_stuff/histclim_eco/EcoregionParameters.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
write.table(x=ecoregion_params, 
            file="LANDIS_stuff/histclim_eco/EcoregionParameters.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE)
#LOOKS GOOD!! and reminder that this was a 2-step process to generate the first header, and then append the bulk of the EcoregionParameters file onto it!!!         


 