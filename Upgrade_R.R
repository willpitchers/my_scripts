#Upgrade R, by your father

#--run in the old version of R
setwd("/Users/WhereYouWantIt")
packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages")

#--run in the new version
setwd("WhereItIs")
load("Rpackages")
for (p in setdiff(packages, installed.packages()[,"Package"]))
install.packages(p)


#Specific to my computer
setwd("~/Desktop")
packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages")

#--run in the new version
setwd("~/Desktop")
load("Rpackages")
for (p in setdiff(packages, installed.packages()[,"Package"]))
install.packages(p)