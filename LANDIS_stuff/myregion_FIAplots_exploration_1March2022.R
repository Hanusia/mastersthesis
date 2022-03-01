# --------------------------------------------------
# working with FIA data for my LANDIS study region
# 01 Mar 2022
# HH
# --------------------------------------------------
#

#first gotta load in the data set:

fia <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/FIA/data_outputs/VT-MA_fia_plots_sp_ba.csv")
View(fia)

#OK, my first goal is to look @ how many of the plots have black ash...
#FIA code number for black ash is 543, so that column is called "X543"
nrow(fia) #number of sampled FIA plots within my study area
nrow(fia[fia$X543>0,]) #how many plots is there at least some black ash BA?
#OK 10 out of 300...that's only like 3 percent...
#maybe my criteria for the harvest in R should just be ANY black ash present??
#and in that case, maybe don't worry about a black ash stand "improvement" harvest
#since there's so little, just model the basket wood harvest instead.

#plotting to take a look at it:
hist(fia$X543)
hist(fia$X543[fia$X543>0])

#find out the *amounts* of black ash BA in plots where it's present:
fia$X543[fia$X543>0] #basically maxes out at 6 (assuming this is sqm per ha)

#and then the *proportions* in which it is present in those plots:
fia_blackash_present <- fia[fia$X543>0,]
fia_blackash_present$totalBA <- rowSums(fia_blackash_present[,5:ncol(fia_blackash_present)])

fia_blackash_present$proportion <- fia_blackash_present$X543 / fia_blackash_present$totalBA
#yeahhh, this makes up a VERY small proportion of plots.
#so basically, let's say that ANY stand with at least SOME black ash
#can be harvested for basket wood,
#and otherwise, don't worry about harvesting those stands for black ash health
#because honestly, there are so very few of them!!
