# --------------------------------------------------
# visualizing ecoregion climate parameters for LANDIS
# 04 May 2022
# HH
# --------------------------------------------------
#

library(ggplot2)
library(tidyverse)

#install.packages("ggnewscale")
library(ggnewscale)

historical <- read.csv("C:/Users/theha/Documents/layers_for_LANDIS/Jane_from_GEE/processed_climate_tables/VTMA_TopoWx_Terraclimate_historical_pnetclim.csv")

View(historical)
nrow(historical) #18612
historic2 <- aggregate(formula = cbind(Tmax, Tmin, par, pptcm) ~ 
                         Ecoregion + year, data = historical, FUN=mean)
View(historic2)
nrow(historic2)
nrow(historic2)*12 #matches up w/ above, as expected!!

#to include multiple color scales, trying code from this source:
#https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/
#which regeres to the package "ggnewscale"

p1 <- ggplot(data=historic2, aes(x=year, y=Tmax)) +
        geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                      low = "gray", high = "purple") 
#let's try just one first...
plot(p1)

p1 +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic2, Ecoregion > 80)) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green")

#Looks OK, now let's do the same thing with Tmin:
p2 <- ggplot(data=historic2, aes(x=year, y=Tmin)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(p2)

p2 +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic2, Ecoregion > 80)) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green")

#I think Jane said I just need to do this for temperatures...
#just kidding, precip too!
#and I actually should be doing it for year-month, so gonna just do precip here
#then switch to year-month data...

p3 <- ggplot(data=historic2, aes(x=year, y=pptcm)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#plot(p3)
p3 +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic2, Ecoregion > 80)) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green")

#now to re-assign data by year-month:

#first, to convert col from doy to month:
historic3 <- historical
historic3$month <- rep(1:12)
View(historic3)
historic3$yearmonth <- historic3$year*10 + historic3$month

#now to re-graph everything:
p4 <- ggplot(data=historic3, aes(x=yearmonth, y=Tmax)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(p4)
p4 +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3, Ecoregion > 80)) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 
#trendlines not seeming to work...maybe just leave it at points

#also a bit hard to parse out with month-by-month data,
#maybe should look at a smaller subset (say, 10 years) with the month-by-month data?

p4subset <- ggplot(data=subset(historic3, year>=2000), aes(x=yearmonth, y=Tmax)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(p4subset)
p4subset +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3, (year>=2000 & Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 

#OK, and now let's do *this* again w/ Tmin:
p5subset <- ggplot(data=subset(historic3, year>=2000), aes(x=yearmonth, y=Tmin)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#plot(p5subset)
p5subset +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3, (year>=2000 & Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 

#and finally, again for precipitation:
p6subset <- ggplot(data=subset(historic3, year>=2000), aes(x=yearmonth, y=pptcm)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#plot(p6subset)
p6subset +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3, (year>=2000 & Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 

#now let's try with a line instead??
p4line  <- ggplot(data=subset(historic3, year>=2000), aes(x=yearmonth, y=Tmax, color=Ecoregion)) +
  geom_line(group=Ecoregion) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(p4line)

p4line +  new_scale("color") +
  geom_line(aes(group=Ecoregion), data = subset(historic3, (year>=2000 & Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 

#this doesn't seem to be working very well......
#So, gonna leave it at this for now at least for internal data vis purposes
#and if we end up wanting to use these graphs, can clean them up / make them pretty later

###
#UPDATE after meeting w/ Jane: 
#she suggested trying to plot vars looking at only ONE month each year...
#e.g. January for Tmax, maybe July? for Tmin, and same for precip??

tmin_Jan <- ggplot(data=historic3[historic3$month==1,], aes(x=year, y=Tmin)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(tmin_Jan)
tmin_Jan +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3[historic3$month==1,], (Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 
#ecoregion values are pretty tightly clustered w/in their zone and very different between zones! (S vs N)
#northern ecoregions in particular are even more tightly clustered

#now let's try...Tmax in July??
tmax_July <- ggplot(data=historic3[historic3$month==7,], aes(x=year, y=Tmax)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(tmax_July)
tmax_July +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3[historic3$month==7,], (Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 
#this one looks MUCH more dispersed than Tmin! + much more overlap between N & S
#but the general trend is as expected, most S ecoregions have higher max temps than N ones

#finally, let's look at precipitation in May...because...why not?! LOL
precip_May <- ggplot(data=historic3[historic3$month==5,], aes(x=year, y=pptcm)) +
  geom_point(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
#let's try just one first...
plot(precip_May)
precip_May +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3[historic3$month==5,], (Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 
#interesting; the trend here REALLY varies on which is higher than the other! (N vs S)
#overall a lot of variation, and not necessarily much overlapping, but not a consistent trend...


#now let's try with a line graph:
tmin_Jan_line <- ggplot(data=historic3[historic3$month==1,], aes(x=year, y=Tmin)) +
  geom_point(aes(color=Ecoregion)) +
  geom_line(aes(color=Ecoregion)) +
  scale_color_gradient("Southern Ecoregions", limits = c(0, 30), 
                       low = "gray", high = "purple") 
plot(tmin_Jan_line)
tmin_Jan_line +  new_scale("color") +
  geom_point(aes(color=Ecoregion), data = subset(historic3[historic3$month==1,], (Ecoregion >80))) +
  geom_line(aes(color=Ecoregion), data = subset(historic3[historic3$month==1,], (Ecoregion >80))) +
  scale_color_gradient("Northern Ecoregions", limits = c(100, 130), 
                       low = "orange", high = "green") 

#still really unsure if this is working???
#seems like there's just one trendline for each section of ecoregions...
#test:
ggplot(data=subset(historic3[historic3$month==1,], year>2010), aes(x=year, y=Tmin)) +
  geom_line(aes(color=Ecoregion)) +
  scale_color_gradient("Ecoregions", limits = c(0, 130), 
                       low = "gray", high = "purple") 
#yeah we definitely just have one trendline happening here...WHY??
#alright I cannot figure out the trendline thing!! leaving it alone for now.