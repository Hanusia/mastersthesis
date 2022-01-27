# --------------------------------------------------
# Chapter 1 data analysis part 100million...fresh for 2022!!
# 20 Jan 2022
# HH
# --------------------------------------------------
#

#first things first ([I'm the realest])...
stand_info <- read.csv("STAND_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
plot_info <- read.csv("PLOT_INFO_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
seedling_data <- read.csv("SEEDLINGS_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
overstory_data <- read.csv("TREES_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
small_sapling_data <- read.csv("SAPLINGS_SMALL_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
large_sapling_data <- read.csv("SAPLINGS_LARGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ash_damage_data <- read.csv("ASH_DAMAGE_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")
ground_cover_data <- read.csv("GROUND_COVER_EAB_project_2020_cleaned_data.csv", fileEncoding = "UTF-8-BOM")

library(tidyverse) #includes tidyr, dplyr, tibble!
library(ggplot2)
library(patchwork)
#library(batman) #Why did I have this one?? guess we'll see if I end up needing it...
library(ggthemes)
library(wesanderson)

# Ground cover class analysis w/ PerMANOVA -------------------------------
#Honestly IDK why I am starting with this, but it feels right, so I'm just gonna go with it!!

### BELOW IS COPIED FROM THE SCRIPT 06-08-21_univariate_analysis_master:

## 8-16-21 update ##
#Asked Tony about how to treat this data & he said assign midpoint vals to BB classes,
#then average to the plot level.
#use PerMANOVA for categorical vars & multiple regression for continuous ones.

#Step 1: Assigning midpoint values to each of the Braun-Blanquet classes
Braun_Blanquet_classes <- c(0:6)
Braun_Blanquet_midpoints <- c(0, 0.5, 3, 10.5, 23, 45.5, 80.5)
#some of the above stuff will be irrelevant b/c we don't want to avg the cover classes themselves, but the midpoints of each class!
ground_cover_data$midpoint_val <- rep(NA)

for(h in 1:nrow(ground_cover_data)){
  for(i in 1:length(Braun_Blanquet_classes)){
    if(is.na(ground_cover_data$cover_class[h])==FALSE){ #NEEDED TO REMOVE NAs! b/c they mess up the if statements b/c no t/f!
      if(ground_cover_data$cover_class[h]==Braun_Blanquet_classes[i]){
        ground_cover_data$midpoint_val[h] <- Braun_Blanquet_midpoints[i]
      } 
    }
  }
}

#View(ground_cover_data)

ground_cover_try <- ground_cover_data %>%
  group_by(plot_ID, life_form) %>% #grouping by plot_ID first and then life_form second
  summarize(avg_cover_val = mean(midpoint_val, na.rm = TRUE))
#View(ground_cover_try)
#Jan. 2022 note: this worked, yay! but in the time since I've worked on this, I think I started using aggregate()
#to do the same thing more efficiently...but we'll stick with this LOL

ground_cover_tidy <- ground_cover_try %>%
  spread(life_form, avg_cover_val)
View(ground_cover_tidy)
#this *should* work now for PerMANOVA!

#installing the needed package:
#install.packages("PERMANOVA")
#library(PERMANOVA)
#turns out this package is VERY new so I'll try using adonis 1st.

#actually, it's looking like I should use the adonis function in the vegan package instead...
library(vegan)
#PERMANOVA AKA NPMANOVA

#next step: need to associate categorical variables w/ this dataframe (@ the plot level)
#vars of interest: for now just start w/ tx type! (Jan '22: and maybe stand for a random var.???)

#need to make input a matrix instead of dataframe!
#Jan 2022 update: ACTUALLY, not sure if I do! let's do some more reading...
#annnnd we are gonna re-approach this!

#### JAN 2022 UPDATES ####
#adonis vs. adonis2 functions: adonis2 has more variables/can do more, but also is slower
#SOURCE TO CITE IN PAPER IS McArdle and Anderson (2001)

#For either, seems like response & independent variables need to be in SEPARATE dataframes 
# or matrices (e.g. dune and dune.env in documentation example)

#SO next step is to create a second dataframe w/ vars of interest in IDENTICAL order to the response df!
#(AKA alphabetical order!)
#so, basically the plot_voi table...?

#back to the ol' standby:
plot_info_plus <- merge(x=plot_info, 
                        y=stand_info, 
                        by="Stand_name", all.x=TRUE)  

#plot_info_plus$plot_ID[1:5]
#ground_cover_tidy$plot_ID[1:5]
#these two are NOT in the same order...let's fix that!
#alphabetizing:
plot_info_plus <- plot_info_plus[order(plot_info_plus$plot_ID),]
#and I think it's already in this order, but just to be on the safe side:
ground_cover_tidy <- ground_cover_tidy[order(ground_cover_tidy$plot_ID),]

#and now, I believe we gotta REMOVE the plot_ID field from the data matrix (ground_cover_tidy df)
ground_cover_tidy <- ground_cover_tidy[, 2:6] #all rows, all cols minus plot_ID

###OK, now I think we're ready to run the function!!

#first, just doing it w/ treatment type as the (only) ind var:
perman1 <- adonis(formula = ground_cover_tidy ~ Treatment, data=plot_info_plus)
perman1
#looks like it worked?! woohoo!
perman1$aov.tab
#perman1$coefficients
#perman1$coef.sites 
#perman1$f.perms
#these outputs don't seem suuuper useful...
perman1$terms

#NEXT QUESTION: how to determine, now, which SPECIFIC interactions are signif???
#can we use Tukey's test for this?

#also need to graph it!

#OK, let's run again incorporating forest type!
perman2 <- adonis(formula = ground_cover_tidy ~ Treatment*forest_type, data=plot_info_plus)
perman2
#RESULT: only treatment is signif, not forest type or their interaction. Good to know!!

#also, let's try again w/ sqrt of the data (using adonis2)...
perman3 <- adonis2(formula = ground_cover_tidy ~ Treatment*forest_type, 
                  data=plot_info_plus,
                  sqrt.dist=TRUE)
perman3
#once again: treatment is signif, and others are not!
#so, I feel confident in my result.

#MAJOR Q: how to address nested nature of my data here though??
#A: adonis can address this w/ the parameter STRATA (and maybe also PERMUTATIONS)
#Let's start by just trying w/ strata:

#using site as a block w/ the parameter 'strata':
perman4 <- adonis(formula = ground_cover_tidy ~ Treatment*forest_type, 
                  strata=plot_info_plus$Stand_name, 
                  data=plot_info_plus)
perman4
#the p-val for each var/interaction when accounting for these blocks is just 1...that seems...wrong??
#let's try again, w/ just 1 var this time:
perman5 <- adonis(formula = ground_cover_tidy ~ Treatment, 
                  data=plot_info_plus, 
                  strata=plot_info_plus$Stand_name
                  )
perman5
#once again, p-val is literally 1...

#taking a look @ stucture of the permutations w/ dif combos of vars:
hist(perman1$f.perms)
hist(perman2$f.perms)
hist(perman4$f.perms)
#based on the histogram & result,
#seems to me like something funky is happening......

#maybe try w/ adonis2 instead????
#copying the example in adonis documentation:
perm <- how(nperm=1000) #setting # of permutations as 1000
setBlocks(perm) <- with(data=plot_info_plus, Stand_name) #setting up the block factor of Stand
perman6 <- adonis2(formula = ground_cover_tidy ~ Treatment*forest_type, 
                   data=plot_info_plus,
                   permutations=perm)
perman6
hist(perman6$F)
#once again....this looks funky?!

#OK, from reading this thread online (https://github.com/vegandevs/vegan/issues/384),
#seems like the issue is that using strata restricts analysis to WITHIN each block.
#so, of course there's no dif in plots within the same site, b/c their values for ind vars w/in those blocks are all the same.
#to test this, I'll try doing it instead w/ gap_status, which DOES vary btwn plots w/in site, and see what I get.

#testing w/ gap_status var:
#first, without strata:
perman7 <- adonis(ground_cover_tidy~gap_status,
                  data=plot_info_plus)
perman7 #result: gap status is signif w/o controlling for blocks/sites.
#and now using strata:
perman8 <- adonis(ground_cover_tidy~gap_status,
                  data=plot_info_plus,
                  strata=plot_info_plus$Stand_name)
perman8 #result: gap status is STILL signifcant w/ addition of controlling for blocks!
#so, the problem lies in how I'm going about this, not that my data are inherently not signif...

#ACTION ITEM: ask Tony whether to worry about this/if he has a suggestion tomorrow?
#OR, could go with Maria's method, which requires doing it manually and seems much harder (LOL), 
#using the permutations script she very kindly shared w/ me
#to paraphrase the helpful thread I linked above, seems like the issue is 
#that I should have site as a RANDOM factor, not as a block...
#but there isn't a way to do this w/in adonis.

#FOR NOW: gonna talk about this w/ Tony tomorrow (and/or email),
#when I come back to it, remember to also move into the POST HOC ANALYSIS
#if I do end up with anything signif, using this as a guide: https://www-researchgate-net.ezproxy.uvm.edu/post/Posthoc_test_for_permanova_adonis

#actually, gonna go ahead and graph these b/c why not!

# Graphing ground cover classes! -------------------------------
#throwing together w/in 1 DF:
ground_cover_vars <- ground_cover_tidy 
ground_cover_vars$plot_ID <- plot_info_plus$plot_ID
ground_cover_vars$Treatment <- plot_info_plus$Treatment 
ground_cover_vars$forest_type <- plot_info_plus$forest_type
#LOL, actually need to spread these out first to graph 'em:
ground_cover_vars_long <- pivot_longer(ground_cover_vars,
                                  cols=B:S, #cols to pivot
                                  names_to="cover_class",
                                  values_to="percent_cover")

groundcover_p1 <- ggplot(data=ground_cover_vars_long,
                         aes(fill=as.factor(cover_class), y=percent_cover,
                             x=as.factor(Treatment))) +
                  geom_bar(position="dodge", stat="identity") + #places groups of bars next to each other & lets gg know I'm providing y-var
                  theme_few()
groundcover_p1
#woohoo! Looks like it worked :)
#but...why aren't my themes working??? LOL
#things to change: add forest type var, edit axis labels, edit legend

groundcover_p2 <- ggplot(data=ground_cover_vars_long,
                         aes(fill=as.factor(cover_class), y=percent_cover,
                             x=as.factor(Treatment))) +
  geom_bar(position="dodge", stat="identity") + #places groups of bars next to each other & lets gg know I'm providing y-var
  theme_few() + 
  scale_fill_manual(values=wes_palette("Darjeeling1", n=5),
                    name="Cover type", 
                    labels = c("Bryophytes", "Bare Soil", "Ferns and Allies", "Herbs", "Grasses and Sedges")) +
  labs (x="Treatment", y= "Average percent cover") +
  facet_wrap( ~ forest_type) #need to spell out forest type names...later (using param 'labeller')
print(groundcover_p2)
#alright...I think this is looking in good shape! Aside from 
#the forest type names, colors, & signif indicators,
#which I will add later.