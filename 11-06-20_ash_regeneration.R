# --------------------------------------------------
# ash regeneration data
# 06 Nov 2020
# HH
# --------------------------------------------------
#

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv")
library(tibble)
library(ggplot2)

#taking a look at what we've got!
glimpse(seedling_data)
glimpse(small_sapling_data)
glimpse(large_sapling_data)
#glimpse(plot_info)
#glimpse(stand_info)

#changing name of plot_ID column since it had some weird characters in it
names(seedling_data)[1] <- "plot_ID"
names(small_sapling_data)[1] <- "plot_ID"
names(large_sapling_data)[1] <- "plot_ID"
names(plot_info)[1] <- "plot_ID"
#names(seedling_data) #success!
#names(plot_info)

#ditto for the first column, "Property" in stand_info
names(stand_info)[1]<- "Property"
# names(stand_info) #works out!

##############################################

#merging the seedlings df w/ the plot_info df on the basis of the plot_ID column
seedlings_plus <- merge(x=seedling_data,y=plot_info,by="plot_ID")

#first, creating a factor list of all the individual plot names
plot_names <- unique(seedlings_plus$plot_ID)
length(plot_names) #not 230 b/c some plots didn't have any seedlings and that's OK!!

ash_seedlings <- data.frame("plot_ID" = plot_info$plot_ID, 
                            "stand_ID" = plot_info$Stand_name,
                            "harvest_status" = plot_info$harvest_status, 
                            "gap_status" = plot_info$gap_status, 
                            "ash_seedling_sum"=rep(0) #starting @ zero so they can be added to
)
nrow(ash_seedlings)
nrow(seedlings_plus)

#for loop to sum up all the ash seedlings...
for(i in 1:nrow(seedlings_plus)){ #iterating thru each seedling!
  if(seedlings_plus$species[i]=="FRAM"){ #only looking @ ash seedlings for now
    #ash_cut$live_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] <- 
    #  ash_cut$live_ash_sum[ash_cut$plot_ID==overstory_plus$plot_ID[i]] +
    #  overstory_plus$BA_sqm[i]
    ash_seedlings$ash_seedling_sum[seedlings_plus$plot_ID[i]==ash_seedlings$plot_ID] <-
      ash_seedlings$ash_seedling_sum[seedlings_plus$plot_ID[i]==ash_seedlings$plot_ID] +
      seedlings_plus$tally[i]
  }
}

#checking to see if it worked...
glimpse(ash_seedlings)
#glimpse(ash_seedlings[ash_seedlings$harvest_status=="YES",])
#sum(ash_seedlings$ash_seedling_sum) #now it's working!!
#sum(seedlings_plus$tally[seedlings_plus$species=="FRAM"]) 
#so based on that ^ there should be a total of 233 ash seedlings

# seedling PLOTS -------------------------------

#now to create some basic boxplots comparing ash seedlings
# in harvested and non-harvested sites
p0=boxplot(formula=ash_seedling_sum~harvest_status, data=ash_seedlings)
p0
#boxplot is NOT very useful as both are clustered around 0...
#maybe a "dot plot" would be better??
ash_seedlings_all <- aov(formula=ash_seedling_sum~harvest_status, data=ash_seedlings)
print(ash_seedlings_all)
print(summary(ash_seedlings_all)) #this one IS significant!!

#let's try a dot plot (then maybe a density plot?)
p1 <-ggplot(data=ash_seedlings, aes(x=harvest_status, y=ash_seedling_sum)) +
  geom_dotplot(binaxis='y', stackdir='center')
p1

#now let's try a density plot:
p2 <-ggplot(data=ash_seedlings, aes(x=ash_seedling_sum)) + # number of seedlings on x-axis
  geom_density(aes(fill=factor(harvest_status)), #separating by treatment
               alpha=0.6)
p2 #this still doesn't look great IMO...but probably better than the boxplot

#now looking # gap vs. matrix within harvested sites
p3=boxplot(formula=ash_seedling_sum~gap_status, 
        data=ash_seedlings[ash_seedlings$harvest_status=="YES",])
#this is still not great......is it even significant?
ash_seedlings_harvested <- aov(formula=ash_seedling_sum~gap_status, 
                               data=ash_seedlings[ash_seedlings$harvest_status=="YES",])
print(ash_seedlings_harvested)
print(summary(ash_seedlings_harvested)) #non-signif dif...womp womp

##############################################
#I really want to look at seedling density VS. harvest intensity!! 
#and then maybe do the same for small & large saplings?!

# incorporating details from harvest status! -------------------------------

#basically need to bring the ash_cut df into this script, then merge them, I think
#COPIED FROM 10-23-20 SCRIPT
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv")
names(overstory_data)[1] <- "plot_ID"
overstory_plus <- merge(x=overstory_data,y=plot_info,by="plot_ID")

#IN THE BELOW MODEL, I'm gonna convert things to use METRIC, not imperial, units!

overstory_plus$ASH_stump_DBH <- rep(NA)
overstory_plus$BA_sqm <- rep(NA)

#calculating BA for live trees & stumps using standard conversion from DBH to BA
#overstory_plus$BA_sqft <- overstory_plus$DBH_cm*overstory_plus$DBH_cm*0.005454

#BUT SINCE WE WANNA DO IT USING METRIC UNITS:
#RIGHT NOW THIS IS ONLY DOING IT CORRECTLY FOR LIVE TREES, NOT USING THE CORRECT DATA FOR STUMPS!!
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
  
  plot_names <- unique(overstory_plus$plot_ID)
  ash_cut <- data.frame("plot_ID" = plot_info$plot_ID, 
                        "stand_ID" = plot_info$Stand_name,
                        "harvest_status" = plot_info$harvest_status, 
                        "gap_status" = plot_info$gap_status, 
                        "live_ash_sum"=rep(0), #starting @ zero so they can be added to
                        "cut_ash_sum"=rep(0) #ditto
                        #,"any_live_ash"=rep("NA"), #but adding a separate var to tell
                        #"any_cut_ash"=rep("NA") #whether it actually went thru /was counted
  )
  
  for (i in 1:nrow(overstory_plus)){ #iterating thru each tree/stump/snag in all plots
    
    #basically, adding the BA of each live ash tree in a plot to the sum value
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
}

#NOW to merge these two dataframes!
ash_seedlings_and_harvest <- merge(x=ash_seedlings,y=ash_cut,by="plot_ID")
glimpse(ash_seedlings_and_harvest) 
#it worked! just need to keep harvest & gap status variable name changes in mind

#adding proportion of ash BA cut:
ash_seedlings_and_harvest$prop_BA_cut <- ash_seedlings_and_harvest$cut_ash_sum /
  (ash_seedlings_and_harvest$live_ash_sum+ash_seedlings_and_harvest$cut_ash_sum)

#OK, now I want to create a REGRESSION of proportion of BA cut x # ash seedlings.
#looking @ harvested sites only:
p4 <- ggplot(data=ash_seedlings_and_harvest[ash_seedlings_and_harvest$harvest_status.x=="YES",],
             aes(x=prop_BA_cut, y=ash_seedling_sum)) + 
      geom_point(color="#000099") +
      geom_smooth(method=lm)
p4 #regression line is not very impressive due to all the zeros.

#WHAT IF we do this by STAND instead of plot? would things shape up better?
#would be harder to show harvest intensity that way...but not impossible!


##############################################

# same thing but by STAND instead of PLOT -------------------------------
stand_names<- unique(seedlings_plus$Stand_name)

ash_seedlings_stand <- data.frame("stand_ID" = stand_info$Stand_name, 
 #                           "harvest_status" = stand_info$Harvest_status, 
 # making harvest status a binary variable of cut vs. not (not implicating "to be cut")
                            "harvest_status" = ifelse(stand_info$Harvest_status=="cut",TRUE,FALSE) ,
                           "ash_seedling_sum"=rep(0) #starting @ zero so they can be added to
)

#for loop to sum up all the ash seedlings...
for(i in 1:nrow(ash_seedlings)){ #iterating thru each PLOT!
    ash_seedlings_stand$ash_seedling_sum[ash_seedlings$stand_ID[i]==ash_seedlings_stand$stand_ID] <-
      ash_seedlings_stand$ash_seedling_sum[ash_seedlings$stand_ID[i]==ash_seedlings_stand$stand_ID] +
      #seedlings_plus$tally[i]
      ash_seedlings$ash_seedling_sum[i]
}

#checking to see if it worked...
glimpse(ash_seedlings_stand)

# more boxplots! ash seedlings @ stand-level -------------------------------
#now to create some basic boxplots comparing ash seedlings
# in harvested and non-harvested sites
p10=boxplot(formula=ash_seedling_sum~harvest_status, data=ash_seedlings_stand)
p10
#boxplot is NOT very useful as both are clustered around 0...
#maybe a "dot plot" would be better??
anova1 <- aov(formula=ash_seedling_sum~harvest_status, data=ash_seedlings_stand)
print(anova1)
print(summary(anova1)) #this one IS significant.

dev.off()
#let's try a dot plot (then maybe a density plot?)
p11 <-ggplot(data=ash_seedlings_stand, aes(x=harvest_status, y=ash_seedling_sum)) +
  geom_dotplot(binaxis='y', stackdir='center')
p11

#now let's try a density plot:
p12 <-ggplot(data=ash_seedlings_stand, aes(x=ash_seedling_sum)) + # number of seedlings on x-axis
  geom_density(aes(fill=factor(harvest_status)), #separating by treatment
               alpha=0.6)
p12 #this still doesn't look great IMO...but probably better than the boxplot

#do the same thing for cut/live ash by STAND
ash_cut_stand <- data.frame("stand_ID" = stand_info$Stand_name, 
        # making harvest status a binary variable of cut vs. not (not implicating "to be cut")
                      "harvest_status" = ifelse(stand_info$Harvest_status=="cut",TRUE,FALSE) ,
                      "live_ash_sum"=rep(0), #starting @ zero so they can be added to
                      "cut_ash_sum"=rep(0) #ditto
                      #,"any_live_ash"=rep("NA"), #but adding a separate var to tell
                      #"any_cut_ash"=rep("NA") #whether it actually went thru /was counted
)

#for loop to sum up harvested BA by stand...
for(i in 1:nrow(ash_cut)){ #iterating thru each PLOT!
  ash_cut_stand$live_ash_sum[ash_cut$stand_ID[i]==ash_cut_stand$stand_ID] <-
    ash_cut_stand$live_ash_sum[ash_cut$stand_ID[i]==ash_cut_stand$stand_ID] +
    ash_cut$live_ash_sum[i] #adding total BA per plot......
  ash_cut_stand$cut_ash_sum[ash_cut$stand_ID[i]==ash_cut_stand$stand_ID] <-
    ash_cut_stand$cut_ash_sum[ash_cut$stand_ID[i]==ash_cut_stand$stand_ID] +
    ash_cut$cut_ash_sum[i]
}

#adding proportion of ash BA cut:
ash_cut_stand$prop_BA_cut <- ash_cut_stand$cut_ash_sum /
  (ash_cut_stand$live_ash_sum+ash_cut_stand$cut_ash_sum)

glimpse(ash_cut_stand)

#now to merge THESE two dataframes:
ash_seed_cut_stand <- merge(x=ash_seedlings_stand, y=ash_cut_stand, by="stand_ID")
glimpse(ash_seed_cut_stand)

#NOW to look at a regression with STAND-LEVEL data:
#looking @ harvested sites only:
p4 <- ggplot(data=ash_seed_cut_stand[ash_seed_cut_stand$harvest_status.x==TRUE,],
             aes(x=prop_BA_cut, y=ash_seedling_sum)) + 
  geom_point(color="#000099") +
  geom_smooth(method=lm) +
  ylim(0,15) + #to crop that outlier 
  xlim(0.2,1) #to crop a DIFFERENT outlier
p4 #there's a biiiig outlier.....
max(ash_seed_cut_stand$ash_seedling_sum)
summary(p4)

p5 <- ggplot(data=ash_seed_cut_stand[ash_seed_cut_stand$harvest_status.x==TRUE,],
             aes(x=prop_BA_cut, y=ash_seedling_sum)) + 
  geom_point(color="#000099") #+
#  geom_smooth(method=lm) +
#  ylim(0,15) + #to crop that outlier 
#  xlim(0.2,1) #to crop a DIFFERENT outlier
p5 #there's a biiiig outlier.....
