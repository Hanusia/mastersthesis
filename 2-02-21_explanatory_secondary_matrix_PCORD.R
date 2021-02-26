# --------------------------------------------------
# Creating explanatory (secondary) matrix for NMS analysis in PC-ORD
# 02 Feb 2021
# HH
# --------------------------------------------------
#

#Per Jeri's book: Explanatory matrix not needed to run NMS (i.e. the initial free ordination), BUT needed to INTERPRET it!
#Important- explanatory variables evaluted independently, so just throw anything on there to try it out if it might be explanatory
#Structure- rows are the same as main matrix (e.g. sample units- in my case, "Sites")

#Step 1. Establish the matrix w/ rows as sites, same as primary (response) matrix.
#Step 2. Start adding variables as columns. (Can always add more on later.) For now:
  # Harvest status (categorical [C])
  #Ash harvest intensity (Quantitative [Q]) -measured by BA/ha of ash removed per site, I think?
    #remember to exclude stumps of higher decay classes- likely just fell, not harvested!
  # Presence/absence of EAB?? (C) AND/OR # of EAB markers present as a tally of the info we collected (Q)
  # Total harvest intensity (Q)- maybe also separated out by each species?? (Ask Tony)
  # Harvest type- based on that summary spreadsheet I sent Tony (C)- Is this a good idea??
  # Climate variables? Latitude? Altitude? Soil type??
  # Forest type- n. hardwood vs rich n. hardwood vs oaky hardwood? (C) How to delineate those?