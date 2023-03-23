#######################
# Stats 2: tutorial 4 #
#######################

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

install.packages("ggplot2")
library("ggplot2")
library("stargazer")
library("tidyverse")

lapply(c("ggplot2"),  pkgTest)

## More on logits: visualising and goodness of fit

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# 1. This time, let's analyse the data in more detail. Run some checks to see if 
#    the data are well distributed. Try some simple plots to get an idea of the 
#    relationship between variables. Drop those errors too.

ggplot(aes(asvab, hsgrad, group = intact), data = graduation) +
  geom_point(aes(colour = intact, size = income)) +
  geom_smooth(method = "glm")

# 2. Last week we created a kitchen sink model, with nsibs as a binned factor. 
#    Here was the code:
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod_1 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")

# Create a more parsimonious model of your own choice. Select three predictor 
# variables, run the regression, and check with summary.

mod_2 <- glm(hsgrad ~ nonwhite + fhs + income + nsibs_cut + asvab,
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")


stargazer(mod_1, mod_2, type="text")
anova(mod_null, mod_2, test = "LRT")

# 3. a) Create a new data frame comprising the outcome variable and two columns 
#       of fitted values, one from mod_1 and another from mod_2.

graduation <- graduation[graduation$nsibs >= 0,]

df <- data.frame(hsgrad = graduation$hsgrad, 
                 mod1_val = mod_1$fitted.values,
                 mod2_val = mod_2$fitted.values)

# 3. b) Create a pipe (without reassigning) whereby you reorder your new 
#       dataframe according to the fitted values of mod_1, create a new rank 
#       variable, then create a scatterplot of rank by fitted value, 
#       colored by the outcome variable.


df %>% 
  arrange(mod1_val) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, mod1_val)) +
  geom_point(aes(colour = hsgrad), alpha = 0.5) +
  scale_y_continuous(limits = c(0,1))

# 3. c) Do the same for mod_2. Compare the results.

df %>% 
  arrange(mod2_val) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, mod2_val)) +
  geom_point(aes(colour = hsgrad), alpha = 0.5) +
  scale_y_continuous(limits = c(0,1))

# 4. Calculate McFadden's Pseudo R squared for both models. 
#    Which model explains more variance?
#    What are the p values?

mod_null <- glm(hsgrad ~1, 
                data = graduation[,!names(graduation) %in% c("nsibs")], 
                family = "binomial")

1-(logLik(mod_1)/logLik(mod_null))
1-(logLik(mod_2)/logLik(mod_null))
