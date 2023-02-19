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

lapply(c("stringr", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# Create data with Cauchy distribution
set.seed(2023)
data_emp <- rcauchy(1000, location=0, scale=1)

# Create function to implement Kolmogorov-Smirnov test comparing data to normal distribution
ks_test <- function(data) {
  # Create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # Generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  sum_val <- 0
  for (i in 1:length(data)) {
    sum_val <- sum_val + (exp(-(((2*i)-1)^2)*(pi^2)/((8*D)^2)))
  }
  p_val <- (sqrt(2*pi)/D) * sum_val
  # Print results
  print(cat("D =", D, "\n"))
  print(cat("P-value =", p_val, "\n"))
}

# Run function with our sample data
ks_test(data_emp)

# Check results against built-in K-S test function
ks.test(data_emp, "pnorm")

#####################
# Problem 2
#####################

# Create data
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Visualise data as scatterplot
plot(data$x, data$y, ylab='Y', xlab='X')

# Code log of likelihood function
linear.lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma_sqrd <- theta[k+1]**2
  e <- y - X%*%beta
  logl <- -0.5*n*log(2*pi) - 0.5*n*log(sigma_sqrd) - 
    ((t(e)%*%e)/(2*sigma_sqrd))
  return(-logl)
}

# Find parameters that maximize the function
linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), hessian=TRUE, 
                    y=data$y, X=cbind(1, data$x), method = "BFGS")

linear.MLE$par

# Check results against ordinary least squares
OLS <- lm(y ~ x, data=data)
stargazer(OLS, type="latex")
