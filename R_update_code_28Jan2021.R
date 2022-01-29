install.packages("installr")
library(installr)

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old_Jan2022.rda")

updateR()

load("installed_old_Jan2022.rda")
installedpkgs.new <- rownames(installed.packages(priority = "NA"))
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()
