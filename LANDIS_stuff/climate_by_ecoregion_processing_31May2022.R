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

 #PREVIOUS CODE COPIED FROM YESTERDAY BELOW:
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

#I think I need a quick walking-around break, then will prep to do another practice run!!
#maybe even on Bertha?!
 #but first just gonna back up to Github.

#OK, also gonna look @ historical par values again to answer Jane's question about what to use for the future ones...

#plot(output$Year[output$Month==1], output$PAR[output$Month==1]) #looking @ Jan par values over time for this ecoregion
#View(input)
y #alright so this is currently for ecoregion 123. Just wanna use input instead to discount future vals.
plot(input$Year[input$Month==1], input$PAR[input$Month==1]) #yeah again, no clear patterns here
plot(input$Year[input$Month==7], input$PAR[input$Month==7])

# Saturday, June 4th -------------------------------
## back on 6/4/2022 to redo the ecoregion parameters file! ##

#basically gonna do the same thing, but need to change the file path name.
ecoregion_params2 <- ecoregion_params
View(ecoregion_params2)
#appending a different path name:
ecoregion_params2$ClimateFileName <- 
  paste0("../../", ecoregion_params2$ClimateFileName)

write.table(x="LandisData     EcoregionParameters", file="LANDIS_stuff/histclim_eco/EcoregionParameters2.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
write.table(x=ecoregion_params2, 
            file="LANDIS_stuff/histclim_eco/EcoregionParameters2.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE)
#LOOKS GOOD!! and reminder that this was a 2-step process to generate the first header, and then append the bulk of the EcoregionParameters file onto it!!!         


#also gonna produce new (historic pathway) climate files w/ PAR values from Hubbard Brook!!
#will first read in the tables I already collated from Jane
#then will replace their PAR with the Hubbard Brook one
#and re-save them to a new folder!

HBclim <- read.table("C:/Users/theha/Documents/layers_for_LANDIS/HB_Climate_1794.txt",
                     header=TRUE)
View(HBclim)
#adding month val...
HBclim$Month <- rep(1:12)

#now THIS is the df I will be altering for my own purposes...
HBclimmod <- HBclim[,c("Year", "Month", "PAR")]
View(HBclimmod)
#adding aggregate of the yrs 1800-1958:
pastparavg <- data.frame("Year"=rep("1800-1958"), 
                         "Month"=1:12,
                         "PAR"=rep(0))
for(i in 1:12){
  pastparavg$PAR[i] <- mean(HBclimmod$PAR[(HBclimmod$Year>=1800 & HBclimmod$Year<=1958
                                           & HBclimmod$Month==i)])
}
futureparavg <- data.frame("Year"=rep(2101:2120, each=12),
                           "Month"=1:12,
                           "PAR"=rep(0))
#averaging the "last" 30 years of the century projected PAR to extrapolate to 2200...
for(i in 1:12){
  futureparavg$PAR[futureparavg$Month==i] <- mean(HBclimmod$PAR[(HBclimmod$Year>=2071 & HBclimmod$Year<=2100
                                           & HBclimmod$Month==i)])
}
View(futureparavg)

#and finally, link them all together...
HBclimmod <- rbind(pastparavg, 
                   HBclimmod[(HBclimmod$Year>=1959 & HBclimmod$Year<=2100),],
                   futureparavg)
HBclimmod$PAR <- round(HBclimmod$PAR, digits=4)

### NOTE: one weird thing to flag for Jane = the year 2100 PAR values are dramatically reduced
#from 2099, etc....so maybe just make that one part of the average/aggregate, too???

#now to read in & replace existing clim files...draw from/modify what I did above:
#also, confirm we still have ecoregion vector var:
ecoregion

#let's just test this again first / confirm same # rows:
input <- read.table(paste0("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", 1, ".txt"),
                    header=TRUE)
nrow(input)
nrow(HBclimmod)
#nice!

for(y in ecoregion){ #in this case, y is the ACTUAL ECOREGION CODE/NUMBER ITSELF!!
  input <- read.table(paste0("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", y, ".txt"),
                      header=TRUE)
  input$PAR <- HBclimmod$PAR
  #and then last step = write the 'output' table to file:
  write.table(x=input, file=paste("LANDIS_stuff/histclim_eco/HubbardBrook_PAR/TerraClimate_historical_eco_", y, "_HBPAR.txt", sep=""),
              sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)
}
#looks good, EXCEPT I need to truncate all the averaged PAR vals to 4 decimal places...
#gonna just add that in above
#update: rounded to 4 decimal places; looks good!

#NOW, just need to make the EcoregionParams file that corresponds w/ these.....
ecoregion_params3 <- ecoregion_params
View(ecoregion_params3)
#appending a different path name:
#except I alsoooo need to change the end of the names w/ the HB appendage on them.
#so gonna do a loop, as done above.
for(i in ecoregion_params3$EcoregionParameters){
  j <- ifelse(i<200, i, i-200) #modifying 'j' for the roadside ecoregions
  #then using j to plug into the correct file name for each ecoregion
  ecoregion_params3$ClimateFileName[ecoregion_params3$EcoregionParameters==i] <- 
    paste0("../../histclim_eco/HubbardBrook_PAR/TerraClimate_historical_eco_", j, "_HBPAR.txt")
}

write.table(x="LandisData     EcoregionParameters", file="LANDIS_stuff/histclim_eco/EcoregionParameters3_HBPAR.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
write.table(x=ecoregion_params3, 
            file="LANDIS_stuff/histclim_eco/EcoregionParameters3_HBPAR.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE)
#looks good; now to move these to where they belong in OneDrive & try another run!


## back on June 8th 2022 ##

# re-doing the Hubbard Brook PAR values (slightly) based on convo w/ Jane, just adjusting slightly :)
# and using for PRACTICE RUNS for now, not yet for actual THESIS RUNS since I haven't structured it yet...??



HBclim <- read.table("C:/Users/theha/Documents/layers_for_LANDIS/HB_Climate_1794.txt",
                     header=TRUE)
View(HBclim)
#adding month val...
HBclim$Month <- rep(1:12)

#now THIS is the df I will be altering for my own purposes...
HBclimmod <- HBclim[,c("Year", "Month", "PAR")]
View(HBclimmod)
#adding aggregate of the yrs ACTUALLY 1959-1978, though what we are CREATING is the 1800-1958 input line:
pastparavg <- data.frame("Year"=rep("1800-1958"), 
                         "Month"=1:12,
                         "PAR"=rep(0))
for(i in 1:12){
  pastparavg$PAR[i] <- mean(HBclimmod$PAR[(HBclimmod$Year>=1959 & HBclimmod$Year<=1978
                                           & HBclimmod$Month==i)])
}
futureparavg <- data.frame("Year"=rep(2100:2120, each=12),
                           "Month"=1:12,
                           "PAR"=rep(0))
#averaging the "last" 30 years of the century projected PAR to extrapolate to 2200...
for(i in 1:12){
  futureparavg$PAR[futureparavg$Month==i] <- mean(HBclimmod$PAR[(HBclimmod$Year>=2070 & HBclimmod$Year<=2099
                                                                 & HBclimmod$Month==i)])
}
View(futureparavg)

#and finally, link them all together...
HBclimmod <- rbind(pastparavg, 
                   HBclimmod[(HBclimmod$Year>=1959 & HBclimmod$Year<=2099),],
                   futureparavg)
HBclimmod$PAR <- round(HBclimmod$PAR, digits=4)

#OK, now let's SAVE this vector to append to all the others:
write.csv(HBclimmod, "LANDIS_stuff/HBclim_PARvals.csv")



#now to read in & replace existing clim files...draw from/modify what I did above:
#also, confirm we still have ecoregion vector var:
ecoregion

#let's just test this again first / confirm same # rows:
input <- read.table(paste0("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", 1, ".txt"),
                    header=TRUE)
nrow(input)
nrow(HBclimmod)
#nice!

for(y in ecoregion){ #in this case, y is the ACTUAL ECOREGION CODE/NUMBER ITSELF!!
  input <- read.table(paste0("LANDIS_stuff/histclim_eco/TerraClimate_historical_eco_", y, ".txt"),
                      header=TRUE)
  input$PAR <- HBclimmod$PAR
  #and then last step = write the 'output' table to file:
  #renaming ANOTHER table w/ this name:
  write.table(x=input, file=paste("LANDIS_stuff/histclim_eco/HBPAR2/TerraClimate_historical_eco_", y, "_HBPAR.txt", sep=""),
              sep="   ", quote=FALSE, append=FALSE, row.names=FALSE)
}

#Okay looks good!!!

#NEXT = NEED A NEW ECOREGION PARAMETERS FILE

ecoregion_params4 <- ecoregion_params
View(ecoregion_params4)
#appending a different path name:
#except I alsoooo need to change the end of the names w/ the HB appendage on them.
#so gonna do a loop, as done above.
for(i in ecoregion_params4$EcoregionParameters){
  j <- ifelse(i<200, i, i-200) #modifying 'j' for the roadside ecoregions
  #then using j to plug into the correct file name for each ecoregion
  ecoregion_params4$ClimateFileName[ecoregion_params4$EcoregionParameters==i] <- 
    paste0("../../histclim_eco/HBPAR2/TerraClimate_historical_eco_", j, "_HBPAR.txt")
}

write.table(x="LandisData     EcoregionParameters", file="LANDIS_stuff/histclim_eco/EcoregionParameters4_HBPAR2.txt",
            quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
write.table(x=ecoregion_params4, 
            file="LANDIS_stuff/histclim_eco/EcoregionParameters4_HBPAR2.txt", 
            sep="   ", quote=FALSE, append=TRUE, row.names=FALSE)



## Friday, June 10th ##

#Goal: Read in all of the future-scenario climate files Jane made,
#replace their PAR values with Hubbard Brook,
#and output them to the appropriate folder structure.
#And ALSO make the associated ecoregion parameter files!

#variables: ecoregion, RCP (pathway), GCM (model)
ecoregion #this one is still good from above
rcp <- c("rcp45", "rcp85")
gcm <- c("CCSM4", "CESM1-BGC", "HADGEM2-ES", "MPI-ESM-LR")

#now writing a function that does three things:
#takes in existing climate files,
#replaces the PAR values with the Hubbard Brook ones,
#and outputs the updated files in the thesis_runs folder

#NOTE: actually, looks like these files (the future path ones from Jane)
#have future years climate data from 2006-2020 appendeded to them,
#so I also need to REMOVE those lines (& then verify that 
#the # of lines is the SAME)

#OK, I aded an if statement to the loop below, so that I can remove the 
#offending rows as needed (assuming this will be for all the files)

#but before I run the whole loop, want to test w/ one file to make sure it won't
#substitute the PAR column if it's not the same number of rows:
input <- read.table(paste0("C:/Users/theha/Documents/layers_for_LANDIS/climate_files/pathway_climatefiles_fromJane/", 
                           gcm[1], "_", "rcp45", "_eco_", y, ".txt"),
                    header=TRUE)
View(input)
nrow(input) #2136 rows
input$PAR <- HBclimmod$PAR
#yes, this throws up an error, as it should!
#now let's see if we remove the offending rows...
input <- input[-c(757:936),]
nrow(input) #now 1956, woohoo :) 
input$PAR <- HBclimmod$PAR
#OK this looks all good!!

for(k in rcp){
  for(j in 1:length(gcm)){
    
    for(y in ecoregion){ #in this case, y is the ACTUAL ECOREGION CODE/NUMBER ITSELF!!
      input <- read.table(paste0("C:/Users/theha/Documents/layers_for_LANDIS/climate_files/pathway_climatefiles_fromJane/", 
                                 gcm[j], "_", k, "_eco_", y, ".txt"),
                          header=TRUE)
      
      #adding an if statement to test for number of rows:
      if(nrow(input)==2136){
        #if it has a bunch of extra rows (which I assume all of them will?,)
        #then I will remove the superfluous ones.
        input <- input[-c(757:936),]
      } else if(nrow(input)==2137){ #for the special case of weirdos.....
        #see below with the saga of an extra line from 2005 sneaking in
        #then I will remove the superfluous ones.
        input <- input[-c(757:937),]
      }
      
      input$PAR <- HBclimmod$PAR
      #and then last step = write the 'output' table to file:
      #renaming ANOTHER table w/ this name:
      write.table(x=input, file=paste0("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/thesis_runs/", 
                                    k, "/rep", j, "-",  gcm[j], "/climate/",
                                    gcm[j], "_", k, "_eco_", y, "_HBPAR.txt"),
                  sep=" ", quote=FALSE, append=FALSE, row.names=FALSE)
    }
    
    
  }
}

#hit an error, but only @ the part w/ HADGEM2-ES in RCP85 folder...?
#looking at the first input text file of that one, it does seem to have an extra line...
#but the question is, WHERE??
#and is that true for all of them??
#let's check...
for(j in 1:length(gcm)){
    for(y in ecoregion){ #in this case, y is the ACTUAL ECOREGION CODE/NUMBER ITSELF!!
      input <- read.table(paste0("C:/Users/theha/Documents/layers_for_LANDIS/climate_files/pathway_climatefiles_fromJane/", 
                                 gcm[j], "_", "rcp85", "_eco_", y, ".txt"),
                          header=TRUE)
      #adding an if statement to test for number of rows:
#     if(nrow(input)==2136){
        #if it has a bunch of extra rows (which I assume all of them will?,)
        #then I will remove the superfluous ones.
#       input <- input[-c(757:936),]
#      }
print(paste(gcm[j], nrow(input)))     
     }
  }

#okay- so it's JUST the HADGEM2-ES files in rcp8.5 that's the issue
#but where?? can i use compare function to find it?
#ahhh, I think I see an issue....an extra line of the future input from 2005 (!) 
#snuck in there, at line 758 of the text files/ line 757 of the dataframe...
#SO, solution is to add to the loop above, removing an EXTRA line if the 
#initial nrow = 2137

#update: after implementing that additional if statement in the loop above, seems to be 
#working smoothly!!
#now, all I need to do is to output the correct ecoregion parameter files.
#in retrospect, I guess I could have named all the climate files the same and then
#had just one ecoregion parameters file....but this is fine, I'm already here lol

View(ecoregion_params)

#creating a for loop to add to the climate file names AND 
#write ecoregion parameter files to their correct folders:
for(g in rcp){
  for(h in 1:length(gcm)){
    ecoparamsmod <- ecoregion_params
    for(i in ecoparamsmod$EcoregionParameters){
      j <- ifelse(i<200, i, i-200) #modifying 'j' for the roadside ecoregions
      #then using j to plug into the correct file name for each ecoregion
      ecoparamsmod$ClimateFileName[ecoparamsmod$EcoregionParameters==i] <- 
        paste0("../climate/", gcm[h], "_", g, "_eco_", j, "_HBPAR.txt")
    }
    #NOTE: to get that additional LANDIS header, create a text doc with just the header, then append the ecoparamsmod file to it
    write.table(x="LandisData     EcoregionParameters", 
                file=paste0("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/thesis_runs/", 
                            g, "/rep", h, "-",  gcm[h], "/climate/EcoregionParameters.txt"),
                quote=FALSE, append=FALSE, row.names=FALSE, col.names = FALSE)
    write.table(x=ecoparamsmod, 
                file=paste0("C:/Users/theha/OneDrive - University of Vermont/Ash project/LANDIS_files/thesis_runs/", 
                            g, "/rep", h, "-",  gcm[h], "/climate/EcoregionParameters.txt"), 
                sep=" ", quote=FALSE, append=TRUE, row.names=FALSE)
  }
}
#yayyy, it worked! :)

#next step is just to repeat in the histclim folder...

#BUT I can do that tomorrow...
#update: did that "manually" since it was the same set of files as in practice_run folder,
#and changed the filepath names using find --> replace in the text file.

