#####################
# load libraries
# set wd
setwd("~/Documents/GitHub/StatsI_Fall2024/problemSets/PS04/my_answers")
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

install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#####################
# Question 1
#####################

######
# here is answer (a)

# Create a new variable 'professional' where 'prof' is coded as 1, others as 0
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

######
# here is answer (b)

# Create an interaction term between 'income' and 'professional'
Prestige$income_professional <- Prestige$income * Prestige$professional

# Run the linear regression model with interaction term
model1 <- lm(prestige ~ income + professional + income_professional, data=Prestige)
summary(model1) 

#####################
# Question 2
#####################

######
# here is answer (a)

# Results
coef_assigned <- 0.042  
se_assigned <- 0.016    

# Calculate t-statistic
t_value_assigned <- coef_assigned / se_assigned 

# Calculate two-sided p-value
p_value_assigned <- 2 * pt(-abs(t_value_assigned), df=128)  

# Output results
t_value_assigned  
p_value_assigned  

######
# here is answer (b)

# Results
coef_assigned <- 0.042  
se_assigned <- 0.013    

# Calculate t-statistic
t_value_assigned <- coef_assigned / se_assigned 

# Calculate two-sided p-value
p_value_assigned <- 2 * pt(-abs(t_value_assigned), df=128)  

# Output results
t_value_assigned  
p_value_assigned
