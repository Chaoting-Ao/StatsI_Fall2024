#####################
# load libraries
# set wd
setwd("~/Documents/GitHub/StatsI_Fall2024/problemSets/PS02/my_answers")
getwd()
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2"),  pkgTest)
install.packages("stargazer")

#####################
# Question 1
#####################

######
# here is answer (b)

#df = (3-1) * (2-1) = 2
p_value = pchisq(6.4326, df=2, lower.tail=FALSE)
print(p_value)
#This gives a p-value of approximately 0.0401

#####################
# Question 2
#####################

######
# here is answer (b)

# Read the data
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# Run the regression
lm <- lm(water ~ female, data=data)

# Display the results
summary(lm)
