# --------------------------------------------------
# Subsetting the Massachusetts parts of my data for Dave Orwig
# 02 Nov 2021
# HH
# --------------------------------------------------
#

#inputting each .csv as-is
stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ash_damage_data <- read.csv("ASH_DAMAGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ground_cover_data <- read.csv("GROUND_COVER_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
CWD_data <- read.csv("CWD_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")


# first: attach state to plot-level data -------------------------------

plot_info <- merge(x=plot_info, y=stand_info[, c("Stand_name", "State")], 
                   by="Stand_name", all.x=TRUE)

#checking to make sure this merge works
head(plot_info)

# creating a function that attaches state, and site, to each data table -------------------------------

AttachState <- function(dtable) {
  dtable <- merge(x=dtable, y=plot_info[, c("plot_ID", "State")],
                  by="plot_ID", all.x=TRUE
  )
  return(dtable)
}

#putting all data files into a list for easier indexing
data_tables <- list("stand_info" = stand_info,
                   "plot_info" = plot_info,
                   "overstory_data" = overstory_data,
                   "large_sapling_data" = large_sapling_data,
                   "small_sapling_data" = small_sapling_data,
                   "ash_damage_data" = ash_damage_data,
                   "CWD_data" = CWD_data,
                   "seedling_data" = seedling_data,
                   "ground_cover_data" = ground_cover_data)


for(i in 3:length(data_tables)){
  data_tables[[i]] <- AttachState(data_tables[[i]])
}

# now, filtering by state -------------------------------

data_tables_MA <- list()
#using the AttachState function for the tables other than stand_info and plot_info:

for(i in 1:length(data_tables)){
  data_tables_MA[[i]] <- data_tables[[i]][data_tables[[i]]$State=="MA", ]
}

#finally, removing the "state" label from other tables (except for site_infO)

#update: trying to use lapply for this?? (instead of a for loop)
data_tables_MA_2 <- lapply(data_tables_MA[2:9], function(x) x[!(names(x) %in% c("State"))])

# finally, outputting as .csv s -------------------------------
#the stand_info one "stands" alone in this code bc we actually DO still want it to include State as a column
write.csv(x=data_tables_MA[[1]], file="MA_plots_for_Dave\\stand_info.csv")

#as for the rest of them: 

for(i in 1:length(data_tables_MA_2)){
  filename <- paste("MA_plots_for_Dave\\", names(data_tables[i+1]), ".csv", sep="")
  write.csv(x=data_tables_MA_2[[i]], file=filename)
}

#woohoo, it worked!
