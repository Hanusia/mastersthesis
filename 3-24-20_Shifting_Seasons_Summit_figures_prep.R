# --------------------------------------------------
# Shifting Seasons Summit figure prep??
# 24 Mar 2021
# HH
# --------------------------------------------------
#

###BELOW COPIED FROM 11-11-20 GRAD SYMPOSIUM PREP###


# first things first: load in the data, etc etc -------------------------------

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv")


library(tibble)
library(ggplot2)
library(patchwork)

#changing name of plot_ID column since it had some weird characters in it
names(seedling_data)[1] <- "plot_ID"
names(small_sapling_data)[1] <- "plot_ID"
names(large_sapling_data)[1] <- "plot_ID"
names(plot_info)[1] <- "plot_ID"
names(overstory_data)[1] <- "plot_ID"
#ditto for the first column, "Property" in stand_info
names(stand_info)[1]<- "Property"

#for first figure: looking @ range of ash BA cut

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
  
  ash_cut <- data.frame("plot_ID" = plot_info$plot_ID, 
                        "stand_ID" = plot_info$Stand_name,
                        "harvest_status" = plot_info$harvest_status, 
                        "gap_status" = plot_info$gap_status, 
                        "live_ash_sum"=rep(0), #starting @ zero so they can be added to
                        "cut_ash_sum"=rep(0) #ditto
                        
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

#adding proportion of ash BA cut:
ash_cut$prop_BA_cut <- ash_cut$cut_ash_sum /
  (ash_cut$live_ash_sum+ash_cut$cut_ash_sum)

#adding a column for BA in sq meters PER HECTARE
ash_cut$BA_sqm_per_ha_live <- ash_cut$live_ash_sum/0.0401149965937
ash_cut$BA_sqm_per_ha_cut <- ash_cut$cut_ash_sum/0.0401149965937


# now creating FIRST BOXPLOT: live ash BA in harvested vs. unharvested plots -------------------------------

p <- ggplot(ash_cut, aes(x=harvest_status, y=BA_sqm_per_ha_live)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x="Was the plot harvested?", y="Basal area of standing \n ash trees (sq. m/ha)") +
  annotate(geom="text", x=2, y=40, label="***", color="blue")

print(p)

#need to do a t-test to confirm this dif is signif......

t.test(ash_cut$BA_sqm_per_ha_live~ash_cut$harvest_status)
#we good! very significant (p<0.0001)

# now creating SECOND BOXPLOT: range of cut ash BA in all harvested sites -------------------------------

p1 <- ggplot(ash_cut[ash_cut$harvest_status=="YES",], 
             aes(x="", y=BA_sqm_per_ha_cut)) +
  geom_boxplot() +
  theme_classic()
print(p1)

#doing it as a density plot, still in units of sq m per ha
p2 <- ggplot(ash_cut[ash_cut$harvest_status=="YES",], 
             aes(x=BA_sqm_per_ha_cut)) +
  geom_density(fill="plum") +
  theme_classic() + 
  labs(x="Basal area of cut ash trees (sq. m/ha)", y="density")
print(p2)

#now doing it as a density plot by proportion of ash cut
p3 <- ggplot(ash_cut[ash_cut$harvest_status=="YES",], 
             aes(x=prop_BA_cut)) +
  geom_density(fill="sky blue") +
  theme_classic() + 
  labs(x="Proportion of ash basal area (sq. m) harvested", y="density")
print(p3)


# going into the SEEDLING data!!! -------------------------------

seedlings_plus <- merge(x=seedling_data,y=plot_info,by="plot_ID")

#finding out which seedlings showed up in the MOST plots:
how_many_seedlings = data.frame("species"=unique(seedlings_plus$species), 
                                "num_plots_present"=rep(0))
for(i in 1:nrow(how_many_seedlings)){
  how_many_seedlings$num_plots_present[i] <-
    sum(seedlings_plus$species==how_many_seedlings$species[i])
}
#now see which are present in the most plots
how_many_seedlings[order(how_many_seedlings$num_plots_present),]

#choosing the top 8 regenerating sp in terms of how many plots they're present in.
#BUT excluding Rubus and Striped Maple since they aren't overstory species!
#so really, the top 6 overstory sp represented in # plots w/ regeneration.
some_seedlings <- data.frame("plot_ID" = plot_info$plot_ID, 
                             "stand_ID" = plot_info$Stand_name,
                             "harvest_status" = plot_info$harvest_status, 
                             "gap_status" = plot_info$gap_status, 
                             "ash_seedling_sum"=rep(0), #starting @ zero so they can be added to
                             "sugar_maple_seedling_sum"=rep(0),
                             "red_maple_seedling_sum"=rep(0),
                             "yellow_birch_seedling_sum"=rep(0),
                             "beech_seedling_sum"=rep(0),
                             "black_cherry_seedling_sum"=rep(0)
)

#creating a vector of the 6 species we're looking at for use in the loop...
seedlings_vec <- c("FRAM", "ACSA", "ACRU", "BEAL", "FAGR", "PRSE")
names(some_seedlings)

#for loop to sum up all the seedlings of these species, per plot...
for(i in 1:nrow(seedlings_plus)){ #iterating thru each seedling!
  for (j in 1:length(seedlings_vec)){ # 'j' will refer to each species separately!
    if(seedlings_plus$species[i]==seedlings_vec[j]){
      some_seedlings[seedlings_plus$plot_ID[i]==some_seedlings$plot_ID,4+j] <- #referring to correct column!
        some_seedlings[seedlings_plus$plot_ID[i]==some_seedlings$plot_ID,4+j] +
        seedlings_plus$tally[i]
    }
  }
}

#glimpse(some_seedlings)

# STAND-LEVEL DATA -------------------------------

#creating the stand-level HARVEST data:
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

#now need to transform to per-stand #s.
some_seedlings_stand <- data.frame("stand_ID" = rep(stand_info$Stand_name, each=6),
                                   #repeating 6 times so there's one line per stand per species
                                   # making harvest status a binary variable of cut vs. not (not implicating "to be cut")
                                   "harvest_status" = rep(ifelse(stand_info$Harvest_status=="cut",TRUE,FALSE), each=6) ,
                                   "num_plots" = rep(stand_info$num_plots, each=6),
                                   "species" = rep(seedlings_vec, times=nrow(stand_info)),
                                   "stand_sum" = rep(0),
                                   "prop_ash_BA_cut" = rep(ash_cut_stand$prop_BA_cut, each=6)
)

#It worked!
#glimpse(some_seedlings_stand)

#for loop to sum up all these seedlings per stand...
for(i in 1:nrow(some_seedlings)){ #iterating thru each PLOT!
  for (j in 1:length(seedlings_vec)){
    some_seedlings_stand$stand_sum[
      some_seedlings_stand$species==seedlings_vec[j] & 
        some_seedlings_stand$stand_ID==some_seedlings$stand_ID[i]] <-
      some_seedlings_stand$stand_sum[
        some_seedlings_stand$species==seedlings_vec[j] & 
          some_seedlings_stand$stand_ID==some_seedlings$stand_ID[i]] + 
      some_seedlings[i,4+j]
  }
}

#DIVIDING the count by (# plots * 3) [# subplots per plot]
#to get count per meter-squared!!
some_seedlings_stand$seedlings_per_sqm <- some_seedlings_stand$stand_sum / 
  (some_seedlings_stand$num_plots*3)

#checking to see if it worked...
glimpse(some_seedlings_stand)

#AHEM: now EXCLUDING one stand b/c it was just too WEIRD
sdlg_stand_most <- some_seedlings_stand[some_seedlings_stand$stand_ID!="Shaker State Forest",]

# now creating THIRD SLIDE: seedlings x ash harvest intensity -------------------------------

#using the dataframe EXCLUDING shaker state forest since there was NO ASH CUT there apparently!
p4 <- ggplot(data=sdlg_stand_most, aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point(aes(col=species)) + theme_classic()
print(p4)
#this one w/ all seedling species together DOESN'T really work b/c ACSA has so many
#more than any other species!

#adding a FORMULA/FUNCTION for linear regression!( thanks Google!)

lm_eqn <- function(speci){
  m <- lm(seedlings_per_sqm ~ prop_ash_BA_cut, 
          data=sdlg_stand_most[sdlg_stand_most$species==speci & sdlg_stand_most$harvest_status==TRUE,]);
  eq <- substitute(italic(r)^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

#so instead, let's do it separately by species....
#and for these scatterplots, look ONLY @ harvested sites (can incorporate unharv ones in a boxplot instead)
p5 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="FRAM" & 
                                    sdlg_stand_most$harvest_status==TRUE,], 
             aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() +
  labs (x="Proportion of ash basal area (sq. m) \n harvested per stand",
        y= "Number of white ash \n seedlings per square meter") +
  geom_smooth (method=lm) +
  geom_text(x=.5, y=2, label=lm_eqn("FRAM"), parse=TRUE)

print(p5)

#out of curiosity, let's look at everything BUT sugar maple...
#p6 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species!="ACSA",], 
#             aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
#  geom_point(aes(col=species))
#print(p6)

#OK, now onto the other species!
p7 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="ACSA" & 
                                    sdlg_stand_most$harvest_status==TRUE,], 
             aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() + 
  labs (x="Proportion of ash basal area (sq. m) \n harvested per stand",
        y= "Number of sugar maple \n seedlings per square meter")+
  geom_smooth (method=lm)+
  geom_text(x=.5, y=25, label=lm_eqn("ACSA"), parse=TRUE)
print(p7)

p8 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="ACRU" & 
                                    sdlg_stand_most$harvest_status==TRUE,], 
             aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() + 
  labs (x="Proportion of ash basal area (sq. m) \n harvested per stand",
        y= "Number of red maple \n seedlings per square meter")+
  geom_smooth (method=lm)+
  geom_text(x=.5, y=2, label=lm_eqn("ACRU"), parse=TRUE)
print(p8)

p9 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="BEAL" & 
                                    sdlg_stand_most$harvest_status==TRUE,], 
             aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() + 
  labs (x="Proportion of ash basal area (sq. m) \n harvested per stand",
        y= "Number of yellow birch \n seedlings per square meter")+
  geom_smooth (method=lm)+
  geom_text(x=.5, y=.5, label=lm_eqn("BEAL"), parse=TRUE)
print(p9)

p10 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="FAGR" & 
                                     sdlg_stand_most$harvest_status==TRUE,], 
              aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() + 
  labs (x="Proportion of ash basal area (sq. m) \n harvested per stand",
        y= "Number of American beech \n seedlings per square meter")+
  geom_smooth (method=lm)+
  geom_text(x=.5, y=.5, label=lm_eqn("FAGR"), parse=TRUE)
print(p10)

p11 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="PRSE" & 
                                     sdlg_stand_most$harvest_status==TRUE,], 
              aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() + 
  labs (x="Proportion of ash basal area (sq. m) \n harvested per stand",
        y= "Number of black cherry \n seedlings per square meter")+
  geom_smooth (method=lm) +
  geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
print(p11)

#patchwork it all together!!
(p5 | p7 | p8) / (p9 | p10 | p11)

## LET'S TRY A DIF VERSION OF THIS USING FACET_WRAP

#this looks good!! just wanna change the label names to common names.
new_labels <- c(ACRU = "red maple, R^2=0.0917", 
                ACSA = "sugar maple, R^2=0.273", 
                BEAL = "yellow birch, R^2=0.00492",
                FAGR = "American beech, R^2=0.126", 
                FRAM = "white ash, R^2=0.0158", 
                PRSE = "black cherry, R^2=0.0144")

p12 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$harvest_status==TRUE,], 
              aes(x=prop_ash_BA_cut, y=seedlings_per_sqm)) +
  geom_point() + theme_classic() + 
  labs (x="Proportion of ash basal area (sq. m) harvested per stand",
        y= "Number of seedlings \n per square meter")+
  geom_smooth (method=lm) +
  #geom_text(x=.5, y=.3, label=lm_eqn("PRSE"), parse=TRUE)
  facet_wrap(~ species, nrow=2, ncol=3, scales="free_y", 
             labeller=labeller(species=new_labels))
print(p12)



### DENSITY PLOTS ###

#also going to try a density plot looking @ seedlings in harvested vs. unharvested plots...
p100 <- ggplot(data=sdlg_stand_most[sdlg_stand_most$species=="ACSA",], 
               aes(x=stand_sum, alpha=0.6)) +
  geom_density(aes(fill=harvest_status)) + theme_classic() #+ 
# labs (x="Proportion of ash basal area (sq. m) harvested",
#      y= "Number of sugar maple \n seedlings per stand")
print(p100)


#11TH HOUR (MORE LIKE SECOND HOUR) THOUGHT: DO I NEED TO CALCULATE SEEDLINGS 
#AS AN AVG PER METER-SQUARED INSTEAD OF JUST THE RAW #?
#PROBABLY!!
#Can solve this by adding a column to the seedlings dataframe dividing the # of seedlings
#by the number of plots/subplots w/in that stand!
#glimpse(stand_info)
#I'm going back up into the code to build this into the dataframe!
#UPDATE: I did it. :/

#okay, before I COMPLETELY go to bed...let's look at boxplots?!
#JK- I'm actually going to bed now! It's 4 a.m.!!!

# NEW STUFF STARTED IN MARCH 2021 -------------------------------

#first up: disaggregate data on basal area of cut ash per plot 
#based on plot status, e.g. gap vs. matrix
#basically modifying plot 1 (the purple one) to have one measure for gap plots 
#and another for matrix (harvested) plots
p20 <- ggplot(ash_cut[ash_cut$harvest_status=="YES",], 
       aes(x=BA_sqm_per_ha_cut, alpha=0.6)) + #alpha=opacity
  geom_density(aes(fill=gap_status)) + #separating density measures by gap status
  theme_classic() + 
  labs(x="Basal area of cut ash trees (sq. m/ha)", y="density")
print(p20)

#Should I t-test the total/density of ash BA cut (sq m./ha) for gap vs. matrix?
#To add some kind of significance to this graph (aka p20)?
#maybe try to make the other plot first, and then revisit this......