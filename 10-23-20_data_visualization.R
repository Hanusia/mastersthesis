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
glimpse(overstory_plus)

#now, for my first trick...find the range of ash basal area removed in harvested sites

#first, creating a factor list of all the individual plot names
plot_names <- unique(overstory_plus$plot_ID)
length(plot_names)
head(plot_names)

#then, setting up a dataframe to hold info about each plot 
#w/ its basic stats, info abt ash removed, etc.



if(overstory_plus$harvest_status="YES"){
  if(overstory_plus$gap_status="NO"){
    
  }
}


