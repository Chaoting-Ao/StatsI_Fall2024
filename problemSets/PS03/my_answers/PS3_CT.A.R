#####################
# load libraries
# set wd
setwd("~/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers")
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

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#####################
# Question 1
#####################

######
# here is answer 1

# Extract the dependent and independent variables
Y <- inc.sub$voteshare  
anes <- anes[complete.cases(anes$caseid), ]
X <- inc.sub$difflog            
n <- length(Y)

# Add intercept term
X_matrix <- cbind(1, X)

# Transpose the X matrix
Xt <- t(X_matrix)

# Calculate X'X
XtX <- Xt %*% X_matrix

# Calculate the inverse of (X'X)
XtX_inv <- solve(XtX)

# Calculate X'Y
XtY <- Xt %*% Y

# Calculate the estimated beta coefficients
beta_hat <- XtX_inv %*% XtY

# Print the regression coefficients
print(beta_hat)

######
# here is answer 2

# Plot the scatter plot
pdf("scatter_plot1.pdf")
plot(X, Y, main="voteshare vs difflog",
     xlab="difflog", ylab="voteshare", pch=19)

# Add the regression line
abline(a=beta_hat[1], b=beta_hat[2], col="blue")
dev.off()

######
# here is answer 3

# Calculate the predicted values
Y_hat <- X_matrix %*% beta_hat

# Calculate the residuals
residuals <- Y - Y_hat

# Save the residuals
residuals_model1 <- residuals

# View the first few residuals
head(residuals_model1)

#####################
# Question 2
#####################

######
# here is answer 1

# Extract the dependent and independent variables
Y <- inc.sub$presvote          
X <- inc.sub$difflog            
n <- length(Y)

# Add intercept term
X_matrix <- cbind(1, X)

# Transpose the X matrix
Xt <- t(X_matrix)

# Calculate X'X
XtX <- Xt %*% X_matrix

# Calculate the inverse of (X'X)
XtX_inv <- solve(XtX)

# Calculate X'Y
XtY <- Xt %*% Y

# Calculate the estimated beta coefficients
beta_hat <- XtX_inv %*% XtY

# Print the regression coefficients
print(beta_hat)

######
# here is answer 2

# Plot the scatter plot
pdf("scatter_plot2.pdf")
plot(X, Y, main="presvote vs difflog",
     xlab="difflog", ylab="presvote", pch=19)

# Add the regression line
abline(a=beta_hat[1], b=beta_hat[2], col="blue")
dev.off()

######
# here is answer 3

# Calculate the predicted values
Y_hat <- X_matrix %*% beta_hat

# Calculate the residuals
residuals <- Y - Y_hat

# Save the residuals
residuals_model2 <- residuals

# View the first few residuals
head(residuals_model2)

#####################
# Question 3
#####################

######
# here is answer 1

# Extract the dependent and independent variables
Y <- inc.sub$voteshare          
X <- inc.sub$presvote            
n <- length(Y)

# Add intercept term
X_matrix <- cbind(1, X)

# Transpose the X matrix
Xt <- t(X_matrix)

# Calculate X'X
XtX <- Xt %*% X_matrix

# Calculate the inverse of (X'X)
XtX_inv <- solve(XtX)

# Calculate X'Y
XtY <- Xt %*% Y

# Calculate the estimated beta coefficients
beta_hat <- XtX_inv %*% XtY

# Print the regression coefficients
print(beta_hat)

######
# here is answer 2

# Plot the scatter plot
pdf("scatter_plot3.pdf")
plot(X, Y, main="voteshare vs presvote",
     xlab="presvote", ylab="voteshare", pch=19)

# Add the regression line
abline(a=beta_hat[1], b=beta_hat[2], col="blue")
dev.off()

#####################
# Question 4
#####################

######
# here is answer 1

# Ensure that residuals_model1 and residuals_model2 have the same length
length(residuals_model1)
length(residuals_model2)

# Assign the dependent and independent variables
Y_resid <- residuals_model1     
X_resid <- residuals_model2
n <- length(Y_resid)

# Add intercept term
X_matrix_resid <- cbind(1, X_resid)

# Transpose the X matrix
Xt_resid <- t(X_matrix_resid)

# Calculate X'X
XtX_resid <- Xt_resid %*% X_matrix_resid

# Calculate the inverse of (X'X)
XtX_inv_resid <- solve(XtX_resid)

# Calculate X'Y
XtY_resid <- Xt_resid %*% Y_resid

# Calculate the estimated beta coefficients
beta_hat_resid <- XtX_inv_resid %*% XtY_resid

# Print the regression coefficients
print(beta_hat_resid)

######
# here is answer 2

# Plot the scatter plot
pdf("scatter_plot4.pdf")
plot(X_resid, Y_resid, main="residuals_model1 vs residuals_model2",
     xlab="residuals_model2", ylab="residuals_model1", pch=19)

# Add the regression line
abline(a=beta_hat_resid[1], b=beta_hat_resid[2], col="blue")
dev.off()

#####################
# Question 5
#####################

######
# here is answer 1

# Assign the dependent and independent variables
Y <- inc.sub$voteshare          
X1 <- inc.sub$difflog 
X2 <- inc.sub$presvote 
n <- length(Y)

# Add intercept term
X_matrix <- cbind(1, X1, X2)

# Transpose the X matrix
Xt <- t(X_matrix)

# Calculate X'X
XtX <- Xt %*% X_matrix

# Calculate the inverse of (X'X)
XtX_inv <- solve(XtX)

# Calculate X'Y
XtY <- Xt %*% Y

# Calculate the estimated beta coefficients
beta_hat <- XtX_inv %*% XtY

# Print the regression coefficients
print(beta_hat)
