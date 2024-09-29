#####################
# load libraries
# set wd
setwd("~/Documents/GitHub/StatsI_Fall2024/problemSets/PS01/my_answers")
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
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

######
# here is answer number 1 

# As the sample size is less than 30 (n=25), it's more appropriate to use 
# the t-distribution rather than the z-distribution.
# The t-distribution better reflects the uncertainty in sampling distribution 
# for small sample sizes.
# Use t.test function to directly calculate the confidence interval
CI_result <- t.test(y, conf.level = 0.90, alternative = "two.sided", mu = 0)

# Print the results
cat("90% Confidence Interval:", round(CI_result$conf.int[1], 2), "to", round(CI_result$conf.int[2], 2), "\n")

# Print mean for comparison
cat("Sample Mean:", round(mean(y), 2), "\n")

#here are my conclusions
# We are 90% confident that the true population mean IQ for students in this school
# falls between 93.96 to 102.92. The sample mean IQ is 98.44.

######
# here is answer number 2

# Perform one-tailed t-test
test_result <- t.test(y, mu = 100, alternative = "greater")

# Print results
cat("Hypothesis Test Results:\n")
cat("t-statistic:", round(test_result$statistic, 4), "\n")
cat("p-value:", round(test_result$p.value, 4), "\n")
cat("95% Confidence Interval:\n")
cat("  Lower bound:", round(test_result$conf.int[1], 2), "\n")
cat("  Upper bound:", round(test_result$conf.int[2], 2), "\n")
cat("Sample mean:", round(test_result$estimate, 2), "\n")

#here are my conclusions
# The calculated t-statistic is -0.5957 and the p-value is 0.7215.
# Since the p-value (0.7215) is greater than our significance level (0.05),
# we fail to reject the null hypothesis. There is not enough statistical
# evidence to conclude that the average IQ in this school is significantly
# higher than 100 (the national average).
# The school counselor should interpret this result as indicating that
# the school's average IQ is not significantly different from the national
# average of 100, based on this sample.


#####################
# Problem 2
#####################

# read in expenditure data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
# explore the data
str(expenditure)  # Examine the structure of the dataframe
summary(expenditure)  # Get summary statistics for all variables

######
# here is answer number 1 

#plot all of the bivariates relationships between the outcome and the predictors
#as well as the predictors themselves
#To visualize relationships between these variables
#we can create a scatter plot matrix using the pairs() function
pdf("plot_pairs.pdf")
pairs(expenditure[, c("Y", "X1", "X2", "X3")])
dev.off()
# Calculate correlation coefficients
cor(expenditure[,c("Y", "X1", "X2", "X3")])

#here are my conclusions
#Y and X1 show a strong positive correlation (r = 0.5317). The scatterplot clearly displays a positive linear relationship.
#Y and X2 have a moderate positive correlation (r = 0.4483). The scatterplot indicates a positive relationship, though not as strong as Y-X1.
#Y and X3 also exhibit a moderate positive correlation (r = 0.4637). The scatterplot shows a similar positive trend.
#X1 and X2 have a weak positive correlation (r = 0.2056). The scatterplot shows a more dispersed pattern with no clear linear trend.
#X1 and X3 demonstrate a strong positive correlation (r = 0.5925). This positive relationship is clearly visible in the scatterplot.
#X2 and X3 show a weak positive correlation (r = 0.2210). The scatterplot displays a somewhat random distribution of points.

######
# here is answer number 2

#Plot the relationship between Y and Region
#To visualize this relationship, we can use a boxplot
pdf("plot_boxplot.pdf")
boxplot(Y ~ Region, data = expenditure,
        main = "Per Capita Expenditure on Housing Assistance by Region",
        xlab = "Region",
        ylab = "Per Capita Expenditure ($)")
dev.off()

#here are my conclusions
#On average, Region 4 (West) has the highest per capita expenditure on housing assistance. 
#This can be seen from the box plot, where Region 4 
#has the highest median (represented by the thick black line in the middle of the box) 
#and the largest interquartile range.


######
# here is answer number 3

#Plot the relationship between Y and X1
pdf("plot_X1&Y.pdf")
plot(expenditure$X1, expenditure$Y,
     xlab="X1: Per Capita Personal Income",
     ylab="Y: Per Capita Expenditure on Housing Assistance",
     main="Relationship between Income and Housing Assistance Expenditure")
# Add regression line
abline(lm(Y ~ X1, data=expenditure), col="red")
dev.off()

# Calculate correlation coefficient
cor(expenditure$Y, expenditure$X1)

#install stargazer packages
install.packages("stargazer")
library(stargazer)

# Run and show regression analysis
regression1 <- lm(Y ~ X1, data=expenditure)

output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("regression_output1.tex", regression1)

#Plot the relationship between Y and X1 and region
#display different regions with different types of symbols and colors.
pdf("plot_X1&Y&region.pdf")
plot(expenditure$X1, expenditure$Y,
     col = expenditure$Region,  
     pch = as.numeric(expenditure$Region),  # Different symbol for each region
     xlab = "X1: Per Capita Personal Income",
     ylab = "Y: Per Capita Expenditure on Housing Assistance",
     main = "Relationship between Income and Housing Assistance Expenditure")
# Add legend
legend("topright", 
       legend = c("Northeast", "North Central", "South", "West"),
       col = 1:4, 
       pch = 1:4)
dev.off()

#here are my conclusions
#The relationship between Y (Per Capita Expenditure on Housing Assistance) 
#and X1 (Per Capita Personal Income) shows a positive correlation.

#The scatter plot displays a clear upward trend, with the 
#red regression line indicating a positive relationship. As personal income 
#increases, there's a general tendency for housing assistance expenditure to increase as well.

#The correlation coefficient between X1 and Y is 0.5317212, indicating a moderate positive correlation.

#The regression coefficient for X1 is 0.025, which is statistically significant 
#at the 1% level (p<0.01). This means that for every $1 increase in per capita 
#personal income, we expect an average increase of $0.025 in per capita housing assistance expenditure.

#The R² value is 0.283, indicating that about 28.3% of the variation in 
#housing assistance expenditure is explained by personal income.

#The F-statistic (18.920, p<0.01) suggests that the overall model is statistically significant.

