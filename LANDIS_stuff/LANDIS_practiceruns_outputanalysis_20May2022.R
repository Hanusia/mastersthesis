# --------------------------------------------------
# LANDIS practice run outputs analysis
# 20 May 2022
# HH
# --------------------------------------------------
#

library(tidyverse)

run5_spinup <- read.csv("C:/Users/theha/Documents/LANDIS_docs/practice_runs/practicerun5_20May2022/output/biomass-AllYears.csv")
run6_initbio <- read.csv("C:/Users/theha/Documents/LANDIS_docs/practice_runs/practicerun6_20May2022/output/biomass-AllYears.csv")

View(run5_spinup)
View(run6_initbio)

run5_long <- pivot_longer( data=run5_spinup, cols=names(run5_spinup)[names(run5_spinup)!="Time"],
                           names_to="species", values_to="run5bio"
)

run6_long <- pivot_longer( data=run6_initbio, cols=names(run6_initbio)[names(run6_initbio)!="Time"],
                           names_to="species", values_to="run6bio"
)

