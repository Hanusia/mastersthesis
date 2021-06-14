install.packages("installr")
library(installr)

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

updateR()

load("installed_old.rda")
installedpkgs.new <- rownames(installed.packages(priority = "NA"))
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()
