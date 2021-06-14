# --------------------------------------------------
#trying to prep for PC-ORD now
# 19 Jan 2020
# HH
# --------------------------------------------------
#

### BELOW IS ALL COPIED FROM 12-14-20 ### 
### update 2/1/2021: NOW THERE ARE MODIFICATIONS towards the end ###

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tibble)
library(ggplot2)

#glimpse(overstory_data)
#glimpse(plot_info)
#glimpse(stand_info)

#changing name of plot_ID column since it had some weird characters in it
#names(overstory_data)[1] <- "plot_ID"
#names(plot_info)[1] <- "plot_ID"
#names(overstory_data) #success!
#names(plot_info)

#ditto for the first column, "Property" in stand_info
#names(stand_info)[1]<- "Property"
# names(stand_info) #works out!
#These lines are no longer needed since I added "fileEncoding = "UTF-8-BOM"" in the read.csv command!

##############################################

#merging the overstory df w/ the plot_info df on the basis of the plot_ID column
overstory_plus <- merge(x=overstory_data,y=plot_info,by="plot_ID")

#converting stump height & diam to DBH- using James Westfall paper from USFS
#IMPORTANT: these values are specific to white ash; dif species have dif constants!!
#IMPORTANT QUESTION: can I use measurements in cm for this?! I don't see why not...
#altho ACTUALLY we'd have to convert stump height-dbh relation to all meters or all feet.
#variables for this formula: dbhi = di * (4.5/hi)^B0 + B1(4.5-h) + Ei
#dbhi = estimated DBH for tree 'i'
#di = stump diameter (in.) for tree i;
#hi = stump height (ft.) for tree i;
#B0 & B1 = estimated fixed-effects parameters; 
#(for white ash, B0 = -0.1074 & B1 = 0.0685)
#Ei = random error for tree i

overstory_plus$ASH_stump_DBH <- rep(NA)
overstory_plus$BA_sqm <- rep(NA)

#calculating BA for live trees & stumps using standard conversion from DBH to BA
#overstory_plus$BA_sqft <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*0.005454

overstory_plus$BA_sqm <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*pi/40000

#only calculating the stump DBH w/ these constants for ash
for(i in nrow(overstory_plus)){
  if(overstory_plus$species[i]=="FRAM"){
    overstory_plus$ASH_stump_DBH[i] <- 
      (overstory_plus$DBH_cm[i]*((1.3716/overstory_plus$height_m[i])^-0.1074) +
         0.0685*(1.3716-overstory_plus$height_m[i]) + 0)
    # and then converting that to BA, for ASH stumps only
    overstory_plus$BA_sqm[i] <- 
      overstory_plus$ASH_stump_DBH[i]*overstory_plus$ASH_stump_DBH[i]*pi/40000 
  }
}

glimpse(overstory_plus)

#now, for my first trick...find the range of ash basal area removed in harvested sites

#first, creating a factor list of all the individual plot names
plot_names <- unique(overstory_plus$plot_ID)

#trying to find our "missing" plot_ID...
#length(unique(overstory_data$plot_ID)) #=/= the next line
#length(unique(plot_info$plot_ID))

#now subsetting to find out which plot_ID in plot_info is NOT in overstory_data

#subset(plot_info, !(plot_ID %in% overstory_data$plot_ID))

#length(plot_names)
#head(plot_names)
#stand_info$num_plots[stand_info$Stand_code=="GSF396"]
#now that that's resolved (# of individual plot_IDs), I've commented out the unnecessary lines

#then, setting up a dataframe to hold info about each plot 
#w/ its basic stats, info abt ash removed, etc.

ash_cut <- data.frame("plot_ID" = plot_info$plot_ID, 
                      "harvest_status" = plot_info$harvest_status, 
                      "gap_status" = plot_info$gap_status, 
                      "live_ash_sum"=rep(0), #starting @ zero so they can be added to
                      "cut_ash_sum"=rep(0) #ditto
                      #,"any_live_ash"=rep("NA"), #but adding a separate var to tell
                      #"any_cut_ash"=rep("NA") #whether it actually went thru /was counted
)

#glimpse(ash_cut)

for (i in 1:nrow(overstory_plus)){ #iterating thru each tree/stump/snag in all plots
  
  #basically, adding the DBH of each live ash tree in a plot to the sum value
  if(overstory_plus$species[i]=="FRAM"){
    if(overstory_plus$status[i]=="live"){
      ash_cut$live_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] <- 
        ash_cut$live_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] +
        overstory_plus$BA_sqm[i]
      #using BA calculated from DBH calculated w/ the formula I used above!
    } else if(overstory_plus$status[i]=="stump") {
      ash_cut$cut_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] <- 
        ash_cut$cut_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] +
        overstory_plus$BA_sqm[i]
    }
  }
  
}

#checking to see if it worked!!
#glimpse(ash_cut)
#glimpse(ash_cut[ash_cut$harvest_status=="YES",])
#I think it worked!!

# Matrix of overstory parameters for stump to BA -------------------------------
#numbers/parameters from Westfall 2010

stump_to_DBH <- data.frame(
  "species" = unique(overstory_plus$species),
  "sp_group" = rep(NA),
  "B0" = rep(NA),
  "B1" = rep(NA),
  "estimates" = rep(FALSE)
)
stump_to_DBH$species

#assigning the values for each species
stump_to_DBH[stump_to_DBH$species=="ACSA",2] <- 7
stump_to_DBH[stump_to_DBH$species=="FRAM",2] <- 9
stump_to_DBH[stump_to_DBH$species=="OSVI",2] <- 11
stump_to_DBH[stump_to_DBH$species=="UNK",2] <- 0
stump_to_DBH[stump_to_DBH$species=="QURU",2] <- 14
stump_to_DBH[stump_to_DBH$species=="CAOV",2] <- 16
stump_to_DBH[stump_to_DBH$species=="FAGR",2] <- 12
stump_to_DBH[stump_to_DBH$species=="BEAL",2] <- 11
stump_to_DBH[stump_to_DBH$species=="ACRU",2] <- 18
stump_to_DBH[stump_to_DBH$species=="BENI",2] <- 11
stump_to_DBH[stump_to_DBH$species=="PRSE",2] <- 10
stump_to_DBH[stump_to_DBH$species=="TSCA",2] <- 4
stump_to_DBH[stump_to_DBH$species=="ACPE",2] <- 18
stump_to_DBH[stump_to_DBH$species=="TIAM",2] <- 13
stump_to_DBH[stump_to_DBH$species=="FRNI",2] <- 9
stump_to_DBH[stump_to_DBH$species=="BEPA",2] <- 11
stump_to_DBH[stump_to_DBH$species=="PRPE",2] <- 10
stump_to_DBH[stump_to_DBH$species=="POGR",2] <- 9
stump_to_DBH[stump_to_DBH$species=="ROPS",2] <- 17
stump_to_DBH[stump_to_DBH$species=="PODE",2] <- 9
stump_to_DBH[stump_to_DBH$species=="ACSP",2] <- 18
stump_to_DBH[stump_to_DBH$species=="ABBA",2] <- 3
stump_to_DBH[stump_to_DBH$species=="PIRU",2] <- 2
stump_to_DBH[stump_to_DBH$species=="POTR",2] <- 9
stump_to_DBH[stump_to_DBH$species=="PIST",2] <- 1
stump_to_DBH[stump_to_DBH$species=="BESP",2] <- 11
stump_to_DBH[stump_to_DBH$species=="BEPO",2] <- 11
stump_to_DBH[stump_to_DBH$species=="ULAM",2] <- 17
stump_to_DBH[stump_to_DBH$species=="CACO",2] <- 16
stump_to_DBH[stump_to_DBH$species=="HAVI",2] <- 0
stump_to_DBH[stump_to_DBH$species=="POSP",2] <- 9
stump_to_DBH[stump_to_DBH$species=="PISP",2] <- 2
stump_to_DBH[stump_to_DBH$species=="ACER",2] <- 18
stump_to_DBH[stump_to_DBH$species=="THOC",2] <- 6
stump_to_DBH[stump_to_DBH$species=="CACA",2] <- 16
stump_to_DBH[stump_to_DBH$species=="ULSP",2] <- 17
stump_to_DBH[stump_to_DBH$species=="BELE",2] <- 11
stump_to_DBH[stump_to_DBH$species=="ACSP2",2] <- 18
stump_to_DBH[stump_to_DBH$species=="ULRU",2] <- 17

unique(stump_to_DBH$sp_group)
stump_to_DBH


###well...I fixed it...and it was the easiest thing in the world...lol
for(i in 1:nrow(stump_to_DBH)){
  if(is.na(stump_to_DBH$sp_group[i])==FALSE){
    if(stump_to_DBH$sp_group[i]==7){
      stump_to_DBH$B0[i] <- -0.1323
      stump_to_DBH$B1[i] <- .2442
    } else if(stump_to_DBH$sp_group[i]==9){
      stump_to_DBH$B0[i] <- -0.1074
      stump_to_DBH$B1[i] <- .0685
    } else if(stump_to_DBH$sp_group[i]==11){
      stump_to_DBH$B0[i] <- -.1743
      stump_to_DBH$B1[i] <- .1376
    }else if(stump_to_DBH$sp_group[i]==14){
      stump_to_DBH$B0[i] <- -0.1651
      stump_to_DBH$B1[i] <- .1258 #PROJECTION based on group 17
      stump_to_DBH$estimates[i] <- TRUE
    }else if(stump_to_DBH$sp_group[i]==16){
      stump_to_DBH$B0[i] <- -.1578
      stump_to_DBH$B1[i] <- .0615
    }else if(stump_to_DBH$sp_group[i]==12){
      stump_to_DBH$B0[i] <- -.1171
      stump_to_DBH$B1[i] <- .0714
    }else if(stump_to_DBH$sp_group[i]==18){
      stump_to_DBH$B0[i] <- -.1382
      stump_to_DBH$B1[i] <- .1010
    }else if(stump_to_DBH$sp_group[i]==10){
      stump_to_DBH$B0[i] <- -.0720
      stump_to_DBH$B1[i] <- .0260 #PROJECTION based on group 8!
      stump_to_DBH$estimates[i] <- TRUE
    }else if(stump_to_DBH$sp_group[i]==4){
      stump_to_DBH$B0[i] <- -.1162
      stump_to_DBH$B1[i] <- .0686
    }else if(stump_to_DBH$sp_group[i]==13){
      stump_to_DBH$B0[i] <- -.1193
      stump_to_DBH$B1[i] <- .1009
    }else if(stump_to_DBH$sp_group[i]==17){
      stump_to_DBH$B0[i] <- -.1662
      stump_to_DBH$B1[i] <- .1258
    }else if(stump_to_DBH$sp_group[i]==3){
      stump_to_DBH$B0[i] <- -.1353
      stump_to_DBH$B1[i] <- .1451
    }else if(stump_to_DBH$sp_group[i]==2){
      stump_to_DBH$B0[i] <- -.1334
      stump_to_DBH$B1[i] <- .0740
    }else if(stump_to_DBH$sp_group[i]==1){
      stump_to_DBH$B0[i] <- -.1096
      stump_to_DBH$B1[i] <- .0588
    }else if(stump_to_DBH$sp_group[i]==6){
      stump_to_DBH$B0[i] <- -.1631
      stump_to_DBH$B1[i] <- .1517
    }else if(stump_to_DBH$sp_group[i]==0){ #for those who we just don't have an estimate......
      stump_to_DBH$B0[i] <- median(as.numeric(stump_to_DBH$B0),na.rm=TRUE)
      stump_to_DBH$B1[i] <- median(as.numeric(stump_to_DBH$B1),na.rm=TRUE)
      stump_to_DBH$estimates[i] <- TRUE
    }
  }
}
stump_to_DBH$sp_group[1]==7
stump_to_DBH

#now, adding a few columns to the overstory data frame
#to actually calculate this ish.

#converting to imperial units to make sure things work with these calculations
overstory_plus$diam_in <- overstory_plus$DBH_cm*.393701
overstory_plus$height_ft <- overstory_plus$height_m*3.28084
overstory_plus$stump_DBH_in <- rep(0)
overstory_plus$stump_DBH_cm <- rep(0)

#calculating stump DBH:
for(i in 1:nrow (overstory_plus)){
  if(overstory_plus$status[i]=="stump"){
    overstory_plus$stump_DBH_in[i] <- 
      (overstory_plus$diam_in[i]*(4.5/overstory_plus$height_ft[i])^
         stump_to_DBH$B0[overstory_plus$species[i]==stump_to_DBH$species]) +
      stump_to_DBH$B1[overstory_plus$species[i]==stump_to_DBH$species]*(4.5-overstory_plus$height_ft[i])
  }
}

#convert back to metric!
overstory_plus$stump_DBH_cm <- overstory_plus$stump_DBH_in*2.54

#convert diam to basal area
overstory_plus$BA_sqm[overstory_plus$status=="stump"] <- 
  overstory_plus$stump_DBH_cm[overstory_plus$status=="stump"]*overstory_plus$stump_DBH_cm[overstory_plus$status=="stump"]*pi/40000 

##############################################

# NOW TRYING TO CREATE PRIMARY MATRIX FOR PC-ORD -------------------------------
#AKA: basal area of standing trees in each stand, NOT cut trees
#So that we can compare composition of what's remaining

#trying to make this *as* a matrix (vs. as a dataframe)

#species_list <-as.character(unique(overstory_plus$species))
#species_list_cut <- as.character(paste(species_list,"_cut", sep=""))
#matcolnames <- c(species_list,species_list_cut)

BA_sp <- matrix(data=rep(0), nrow=45, ncol=40)
colnames(BA_sp) <- unique(overstory_plus$species)
rownames(BA_sp) <- unique(stand_info$Stand_code)
#Matrix is in place!!
#First convet into a dataframe.
#now to fill it up.
#THEN will divide to make it BA/ha.
#then I can graph it!

standcode <- unique(stand_info$Stand_code)
BA_sp_df <- as.data.frame(BA_sp)
BA_sp_df <- cbind(standcode, BA_sp_df)
#BA_sp_df <- data.frame(row.names=matcolnames)
overstory_plus2 <- merge(x=overstory_plus,y=stand_info,by="Stand_name")

#adding an incremental operator (I found it online)
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#adding a simple intermediary matrix to try to make this work.....
spec_df <- data.frame(sp = colnames(BA_sp_df)[2:40], num = 1:length(colnames(BA_sp_df)[2:40]))
spec_df$sp

for(i in 1:nrow(overstory_plus2)){
  #if(overstory_plus2$species[i]!=""){
  #find the right row (Based on stand code)
  j=which(BA_sp_df$standcode==overstory_plus2$Stand_code[i])[1]
  if(overstory_plus2$status[i]=="live"){
    k=spec_df$num[as.character(spec_df$sp)==as.character(overstory_plus2$species[i])]+1
    BA_sp_df[j,k] %+=% overstory_plus2$BA_sqm[i]
  }
  #  }    
}
##I FINALLY GOT THIS MATRIX TO WORK BABYYY

#changed from "stand_info_2" to "stand_info" below since stand_info_2 doesn't exist?!
BA_ha_df <- cbind(BA_sp_df, "num_plots"=stand_info$num_plots) #now to get it into BA/ha...
#just add on the # plots column since stands are in the same order
#each plot = 400 square meters = 0.04 hectares
#so, BA/stand /(.04 ha * n plots/stand)

for (i in 2:(ncol(BA_ha_df)-1)){
  for(j in 1:nrow(BA_ha_df)){
    BA_ha_df[j,i] = BA_ha_df[j,i]/(0.04*BA_ha_df$num_plots[j])
  }
}
#I DID IT!! this dataframe (BA_ha_df) has basal area per hectare per species per stand!!!
#NOW I'm removing the num_plots col (the last one) since I no longer need it:
BA_ha_df <- BA_ha_df[,1:41]

##############################################

# THE BELOW SECTION WAS TESTING THAT I DON'T NEED TO RUN AGAIN IN FUTURE 
# BUT DON'T WANT TO DELETE, FOR POSTERITY'S SAKE -------------------------------

#testing something...to make sure all species were taken care of
#first for elm: which sites did it have BA>0?
#BA_ha_df[BA_ha_df$ULAM>0,BA_ha_df$ULAM] #ans: a few, OK
#next for bitternut hickory: asking which sites did it have BA>0?
#BA_ha_df[BA_ha_df$CACO>0,BA_ha_df$CACO] #NONE...WHY????
#BA_ha_df[BA_ha_df$CAOV>0,BA_ha_df$CAOV] #NONE...WHY????
#BA_sp_df[BA_sp_df$CAOV>0,BA_sp_df$CAOV] #NONE...WHY????
#glimpse(overstory_plus2)
#overstory_plus2[overstory_plus_2[overstory_plus2$species=="CACO",],overstory_plus2$DBH_cm]
#sum(overstory_plus2$species=="BELE")
#overstory_plus2$DBH_cm[overstory_plus2$species=="CACO"] # so the DBH DOES exist in overstory_plus2...
#overstory_plus2$status[overstory_plus2$species=="CACO"] #and it IS  alive...now let's check for a sp I actually care about
#overstory_plus2$DBH_cm[overstory_plus2$species=="BELE"] #ditto, we still have DBH
#overstory_plus2$BA_sqm[overstory_plus2$species=="BELE"] #and it still has BA
#overstory_plus2$status[overstory_plus2$species=="BELE"] #and it's considered live......
#sum(overstory_plus2$status=="live")

#overstory_plus2[overstory_plus2$species=="BELE",]
#overstory_plus2$BA_sqm[1071] #it's in there all right...


# BA_sp_df[BA_sp_df$standcode=="OSFCD1","BELE"] #THERE WAS NEVER AN ISSUE IT JUST SHOWED UP AS 0 ELSEWHERE!!!
# BA_ha_df[BA_ha_df$standcode=="OSFCD1","BELE"]
#let's replicate this with BENI just to make sure...
# sum(overstory_plus2$species=="BENI")
# overstory_plus2[overstory_plus2$species=="BENI",c("status", "BA_sqm", "Stand_code")] #checking in which stands BENI live exists
# sum(overstory_plus2$BA_sqm[overstory_plus2$species=="BENI" & overstory_plus2$Stand_code=="BRSP"]) #this num should be same as...
# BA_sp_df[BA_sp_df$standcode=="BRSP","BENI"] #YASSSSS
# BA_ha_df[BA_ha_df$standcode=="BRSP","BENI"] # & now to make sure it converted correctly to sqm/ha...
# sum(BA_ha_df$BENI)
# sum(BA_ha_df$CACO)
# sum(BA_ha_df$THOC)

##############################################

#per Tony's advice- I should also do a separate primary matrix for REGENERATION (ask: Just seedlings??)

# --------------------------------------------------
# Creating explanatory (secondary) matrix for NMS analysis in PC-ORD
# 02 Feb 2021
# HH
# --------------------------------------------------
#

#Per Jeri's book: Explanatory matrix not needed to run NMS (i.e. the initial free ordination), BUT needed to INTERPRET it!
#Important- explanatory variables evaluted independently, so just throw anything on there to try it out if it might be explanatory
#Structure- rows are the same as main matrix (e.g. sample units- in my case, "Sites")

#Step 1. Establish the matrix w/ rows as sites, same as primary (response) matrix.
#Step 2. Start adding variables as columns. (Can always add more on later.) For now:
# Harvest status (categorical [C])
#Ash harvest intensity (Quantitative [Q]) -measured by BA/ha of ash removed per site, I think?
#remember to exclude stumps of higher decay classes- likely just fell, not harvested!
# Presence/absence of EAB?? (C) AND/OR # of EAB markers present as a tally of the info we collected (Q)
# Total harvest intensity (Q)- maybe also separated out by each species?? (Ask Tony)
# Harvest type- based on that summary spreadsheet I sent Tony (C)- Is this a good idea??
# Climate variables? Latitude? Altitude? Soil type??
# Forest type- n. hardwood vs rich n. hardwood vs oaky hardwood? (C) How to delineate those?

#VARIABLES TONY SUGGESTED:
#% ash removal, total % ash in stand, state, year harvested, type of tx, beech component as proxy for forest type, % BA removed overall
#SEE SETUP OF DATAFRAME BELOW FOR VARIABLE NAMES:

#PREPARATION FOR percentage variables: create an analogue to BA_sp_df that represents CUT trees

#basically creating this the same way I did BA_sp_df
BA_sp_cut <- as.data.frame(BA_sp)
BA_sp_cut <- cbind(standcode, BA_sp_cut)

#similarly using the same for loop to find & match species, but instead of live trees
#I'm adding BA for stumps of decay class 1 or 2

for(i in 1:nrow(overstory_plus2)){
  #if(overstory_plus2$species[i]!=""){
  #find the right row (Based on stand code)
  j=which(BA_sp_cut$standcode==overstory_plus2$Stand_code[i])[1]
  if(overstory_plus2$status[i]=="stump"){
    if(is.na(overstory_plus2$decay_class[i])==FALSE&(overstory_plus2$decay_class[i]<3)){ #includes only stumps w/ DC 1 or 2
    k=spec_df$num[as.character(spec_df$sp)==as.character(overstory_plus2$species[i])]+1
    BA_sp_cut[j,k] %+=% overstory_plus2$BA_sqm[i]
    }
  }
  #  }    
}
#loooooks like it worked!!

#prop of ash cut/(ash cut + ash standing) B/C FRAM IS COL 3 IN BOTH DFs
#explan_mat$perc_ash_removed <- BA_sp_cut[,3] / (BA_sp_df[,3]+BA_sp_cut[,3]) 

#testing the above...
BA_sp_cut[2,3] / (BA_sp_df[2,3]+BA_sp_cut[2,3])

#now doing a similar-ish thing for perc_ash_total
explan_mat$perc_ash_total <- (BA_sp_df[,3]+BA_sp_cut[,3]) / sum((BA_sp_df[,2:41]+BA_sp_cut[,2:41])) 

#testing the above (w/ row 2- MP19 I think): 
#(BA_sp_df[2,3]+BA_sp_cut[2,3]) / sum((BA_sp_df[2,2:41]+BA_sp_cut[2,2:41])) 
(BA_sp_df[,3]+BA_sp_cut[,3]) / sum(is.na((BA_sp_df[,2:41])==FALSE+is.na(BA_sp_cut[,2:41]))==FALSE) 

#actually setting up the dataframe:

explan_mat <- data.frame("stand_code" = stand_info$Stand_code,
                         "harvest_status" = stand_info$Harvest_status, #categorical variable
                         "state" = stand_info$State,#categorical variable
                         "year_harvested" = rep(x=0, times=45), #quantitative variable- what to do w/ unharvested stands??
                         "treat_type" = rep("TBD", times=45), #categorical variable- treatment type (based on that table I sent Tony)
                         #"forest_type" = rep(NULL),% BA beech as proxy for NH / rich NH forest type
                         "perc_ash_total" = rep(x=0, times=45), #quantitative variable- % ash of all species, all BA (both standing and harvested)
                         "perc_ash_removed" = rep(x=0, times=45), #quantitative variable- BA ash removed / BA ash in stand
                         "perc_BA_removed" = rep(x=0, times=45) #quantitative variable- BA cut / total BA (live + cut)
)


sum((BA_sp_df[,2:41]+BA_sp_cut[,2:41]))
(BA_sp_df[2,3]+BA_sp_cut[2,3]) / sum((is.na((BA_sp_df[2,2:41])==FALSE)+(is.na(BA_sp_cut[2,2:41]))==FALSE)) 
(BA_sp_df[2,3]+BA_sp_cut[2,3]) / sum(((BA_sp_df[2,2:41])+(BA_sp_cut[2,2:41]))) 
sum((BA_sp_df[2,2:41])+(BA_sp_cut[2,2:41]))
sum((is.na(BA_sp_df[2,2:41])==FALSE)+(is.na(BA_sp_cut[2,2:41])==FALSE))
#^the above line of code is just COUNTING UP THE FALSES....NOT OK!!

sum((BA_sp_df[,2:41]+BA_sp_cut[,2:41])) 

## ACTION ITEMS FOR NEXT TIME
#actually comment the rest of this code that is NOT yet commented out
#troubleshoot what ever is going on up there
#update workflow doc!