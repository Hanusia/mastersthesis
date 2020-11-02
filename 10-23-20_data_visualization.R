#hanusia higgins
#Oct. 23 2020
#initial data exploration/visualization

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv")

library(tibble)
glimpse(overstory_data)
glimpse(plot_info)
glimpse(stand_info)

#changing name of plot_ID column since it had some weird characters in it
names(overstory_data)[1] <- "plot_ID"
names(plot_info)[1] <- "plot_ID"
#names(overstory_data) #success!
#names(plot_info)

#ditto for the first column, "Property" in stand_info
names(stand_info)[1]<- "Property"
# names(stand_info) #works out!

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
#IN THE BELOW MODEL, I'm gonna convert things to use METRIC, not imperial, units!

overstory_plus$ASH_stump_DBH <- rep(NA)
#only calculating the stump DBH w/ these constants for ash
for(i in nrow(overstory_plus)){
  if(overstory_plus$species[i]=="FRAM"){
  overstory_plus$ASH_stump_DBH[i] <- 
    (overstory_plus$DBH_cm[i]*((1.3716/overstory_plus$height_m[i])^-0.1074) +
    0.0685*(1.3716-overstory_plus$height_m[i]) + 0)
}
}
#calculating BA for live trees & stumps using standard conversion from DBH to BA
overstory_plus$BA_sqm <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*0.005454


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
        overstory_plus$DBH_cm[i]
      #FOR NOW, treating cut stump diam as DBH - can correct later by
      #adding a column to overstory_plus that calculates BA or similar
      #w/ a formula for diam x height & using that # instead of stump "DBH"
    } else if(overstory_plus$status[i]=="stump") {
      ash_cut$cut_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] <- 
        ash_cut$cut_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] +
        overstory_plus$DBH_cm[i]
    }
  }
  
}

#checking to see if it worked!!
glimpse(ash_cut)
glimpse(ash_cut[ash_cut$harvest_status=="YES",])
