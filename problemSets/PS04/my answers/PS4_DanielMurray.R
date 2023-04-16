#################
# Problem Set 4 #
#################

#Install and load packages
install.packages("eha")
install.packages("survival")
library("eha")
library("survival")
library("stargazer")

# Load in dataset
data("infants")

# Run cox proportional hazard model
infants_surv <- with(infants, Surv(enter, exit, event))
cox <- coxph(infants_surv ~ age + sex, data = infants)
summary(cox)
stargazer(cox, type = "latex")
