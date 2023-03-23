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

lapply(c('nnet', 'stargazer', 'MASS', 'pscl', 'AER'),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# Read in data
data <- read.csv("data/gdpChange.csv")

# Inspect data
head(data)
summary(data)
str(data)

# Wrangle data
data$REG <- as.factor(data$REG)
data$OIL <- as.factor(data$OIL) 
data$GDPWdiff2[data$GDPWdiff < 0] <- "negative"
data$GDPWdiff2[data$GDPWdiff == 0] <- "no change"
data$GDPWdiff2[data$GDPWdiff > 0] <- "positive"
data$GDPWdiff <- data$GDPWdiff2
data$GDPWdiff2 <- NULL

# Change GDPWdiff to unordered factor with "no change" as reference
data$GDPWdiff <- factor(data$GDPWdiff, ordered = FALSE)
data$GDPWdiff <- relevel(data$GDPWdiff, ref="no change")

# Run unordered multinomial logit regression model
mod_unordered <- multinom(GDPWdiff ~ REG + OIL, data = data)
summary(mod_unordered)
stargazer(mod_unordered, type="latex")

# Change GDPWdiff to ordered factor
data$GDPWdiff <- factor(data$GDPWdiff, ordered = TRUE)
data$GDPWdiff <- factor(data$GDPWdiff, 
                        levels = c("negative", "no change", "positive"),
                        labels = c("Negative", "No Change", "Positive"))

# Run ordered multinomial logit regression model
mod_ordered <- polr(GDPWdiff ~ REG + OIL, data = data, Hess = TRUE)
summary(mod_ordered)

#####################
# Problem 2
#####################

# Read in data
data2 <- read.csv("data/MexicoMuniData.csv")

# Inspect data
head(data2)
summary(data2)
str(data2)

with(data2,
     list(mean(PAN.visits.06), var(PAN.visits.06)))

# Wrangle data
data2$competitive.district <- as.factor(data2$competitive.district)
data2$PAN.governor.06 <- as.factor(data2$PAN.governor.06)

# Run poisson regression
vis_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                   data=data2, family=poisson)
summary(vis_poisson)

# Extract coefficients
cfs <- coef(vis_poisson)
cfs

# Exponentiate coefficients
exp_cfs <- exp(cfs)
exp_cfs

# Predicted no. of visits for a competitive district that is zero-marginal and has a PAN governer
exp(cfs[1] + cfs[2]*1 + cfs[3]*0 + cfs[4]*1)

