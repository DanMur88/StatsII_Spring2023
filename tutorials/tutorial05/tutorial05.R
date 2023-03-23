#####################
# load libraries
# set wd
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

lapply(c("MASS", "nnet"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establish just as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?

# Inspect structure of dataframe
str(workingMoms)

#Convert character variables to factors
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))
workingMoms$attitude <- factor(workingMoms$attitude,
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree", "Disagree",
                                          "Agree", "Strongly Agree"))

# Fit proportional odds logistic regression model
model_fit <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(model_fit)

# Calculate p-value
ctable <- coef(summary(model_fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable,"p value" = p))

# Calculate confidence intervals
(ci <- confint(model_fit))

# Convert to odds ratio
exp(cbind(OR = coef(model_fit), ci))

# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.

# Set reference level for 
workingMoms$attitude <- relevel(workingMoms$attitude, ref="Strongly Disagree")

# Run model
mult_log <- multinom(attitude ~ ., data=workingMoms)
summary(mult_log)

# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?

0.27+0.35
0.26+0.11

