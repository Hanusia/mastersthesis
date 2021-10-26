# --------------------------------------------------
# forest type: an investigation
# 22 Oct 2021
# HH
# --------------------------------------------------
#


#ok I found out that I...already did this?! 
#or at least the first part of it...
#in file stand_basal_area_calcs
#THANKS PAST HANUSIA!!!

stand_ba_preharvest <- read.csv("stand_basal_area_dataframe_09Aug2021.csv")
View(stand_ba_preharvest)
stand_ba_preharvest <- as.data.frame(stand_ba_preharvest)
#update: I didn't actually do this by species...LOL
#just total BA. which is still important!

#But, maybe I can just pull out the beech part...

stand_ba_species <- read.csv("stand_basal_area_species_df_22Oct21.csv")

#just generated the above dataframe in the same file referenced before- BA per species per stand (total, NOT per ha)

#now...to compare % beech??

stand_ba_beech <- stand_ba_species[stand_ba_species$species=="FAGR",]
stand_ba_beech$beech_BA_sqm <- stand_ba_beech$BA_sqm

#now to combine into same DF to easily calculate:

beech_question <- merge(x=stand_ba_preharvest[, c("Stand_name", "BA_sqm"
)], y=stand_ba_beech[,c("Stand_name", "beech_BA_sqm")], 
                        by="Stand_name", all.x=TRUE)
head(beech_question)

#adding 0 for beech_BA_sqm for a few sites that had no beech recorded
beech_question[is.na(beech_question$beech_BA_sqm), "beech_BA_sqm"] <- 0

beech_question$prop_beech <- beech_question$beech_BA_sqm/beech_question$BA_sqm

beech_question[beech_question$prop_beech>=0.3, ]

##additional work 10/25: going to try to analyze this w/ forest types to see if there's a signif dif.

stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
head(stand_info)
beech_question <- merge(x=beech_question, y=stand_info[, c("Stand_name", "forest_type")])

#By beech raw BA per site
forest_type_test_1 <- t.test(formula = beech_BA_sqm ~ forest_type, data = beech_question)
forest_type_test_1

#by proportion of beech BA per site
forest_type_test_2 <- t.test(formula = prop_beech ~ forest_type, data = beech_question)
forest_type_test_2

#result: both were signif. Now do the same for SM and *maybe* ash?

##############################################
### now doing the same for sugar maple ###
stand_ba_sugarmaple <- stand_ba_species[stand_ba_species$species=="ACSA",]
stand_ba_sugarmaple$sugarmaple_BA_sqm <- stand_ba_sugarmaple$BA_sqm

#now to combine into same DF to easily calculate:

sugarmaple_question <- merge(x=stand_ba_preharvest[, c("Stand_name", "BA_sqm"
)], y=stand_ba_sugarmaple[,c("Stand_name", "sugarmaple_BA_sqm")], 
by="Stand_name", all.x=TRUE)
head(sugarmaple_question)

#calculating proportion BA
sugarmaple_question$prop_sugarmaple <- sugarmaple_question$sugarmaple_BA_sqm/sugarmaple_question$BA_sqm

sugarmaple_question <- merge(x=sugarmaple_question, y=stand_info[, c("Stand_name", "forest_type")])

#By sugarmaple raw BA per site
forest_type_test_3 <- t.test(formula = sugarmaple_BA_sqm ~ forest_type, data = sugarmaple_question)
forest_type_test_3

#by proportion of sugarmaple BA per site
forest_type_test_4 <- t.test(formula = prop_sugarmaple ~ forest_type, data = sugarmaple_question)
forest_type_test_4

#result: both of these NOT significant...

