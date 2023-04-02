##########################
##########################
### Replication for ######
### Latura Weeks #########
### Corporate Board ######
### Quotas and Gender ####
### Equality Policies in #
### the Workplace ########
##########################
##########################


### Analyses carried out in R version 4.1.2 (2021-11-01) -- base R

rm(list = ls())

#########################
## Install packages #####
#########################
install.packages("multiwayvcov")
install.packages("lmtest")
install.packages("stargazer")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("MatchIt")
install.packages("aod")
install.packages("clubSandwich")
install.packages("reshape2")
install.packages("tidyverse")
#########################

#########################
library("multiwayvcov")
library("lmtest")
library("stargazer")
library("dplyr")
library("ggplot2")
library("tidyverse")
########################

##########################
# Information to Reconstruct the Analysis Datasets:
#
# Please see Supplemental Information Appendix for full
# details about how we constructed the analysis
# datasets from the original data sources (company reports).
# Section II discusses our quantatitive data, and Section IV
# discusses the qualitative data. 
##########################

##########################

load("data_final_25Jan2022.RData")
head(data_final)
nrow(data_final) ## 962 

###########################
###########################
## Replicates Figure 1 ####
###########################
###########################

#######Left panel (overall attention) 

plotd <- subset(data_final, select = c(
  "year", 
  "company", 
  "country", 
  "prop_attn", 
  "prop_lead")) 

make_grey <- ggplot(data=plotd, aes(x=year, y=prop_attn, col=country)) +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,]) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,]) + 
  xlab("Year") + 
  ylab("Attention") + 
  guides(shape=FALSE) + 
  theme(legend.title = element_blank()) +
  ggtitle("Overall attention to gender equality issues,\n before and after quota law (2011)") + 
  theme(plot.title = element_text(hjust = 0.5))

make_grey <- make_grey + scale_colour_grey()
make_grey

#######Right panel (leadership) 

make_grey2 <- ggplot(data=plotd, aes(x=year, y=prop_lead, col=country)) +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,]) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,]) + 
  xlab("Year") + 
  ylab("Attention") + 
  guides(shape=FALSE) + 
  theme(legend.title = element_blank()) +
  ggtitle("Attention to gender gap in leadership,\n before and after quota law (2011)") + theme(plot.title = element_text(hjust = 0.5)) 

make_grey2 <- make_grey2 + scale_colour_grey()
make_grey2

###########################
###########################
## Replicates Table 2 #####
###########################
###########################

options(scipen=999)

m1 <- lm(prop_attn ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1, data_final$company)
m1_se <- as.matrix(coeftest(m1, vcov_company)) 

m2<-lm(prop_lead ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m2, data_final$company)
m2_se <- as.matrix(coeftest(m2, vcov_company)) 

m3<-lm(prop_pay ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m3, data_final$company)
m3_se <- as.matrix(coeftest(m3, vcov_company)) 

m4<-lm(prop_family ~ year_f + company + quota + sustain + pct_rev_change, 
                 data=data_final)
vcov_company <- cluster.vcov(m4, data_final$company)
m4_se <- as.matrix(coeftest(m4, vcov_company)) 

m5<-lm(prop_harass ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m5, data_final$company)
m5_se <- as.matrix(coeftest(m5, vcov_company))

## Note: Stargazer code uses SE and p-values from clustered SE models

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
p = list(m1_se[,4], 
                    m2_se[,4],
                    m3_se[,4],
                    m4_se[,4],
                    m5_se[,4]),
covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
type = "text",  out="Table2.doc")

# Note: Stargazer notes (beneath table) for p-values do not output correctly in Word
# -- have to add in manually "*p<0.05; **p<0.01; ***p<0.001"

### Table in Latex: 

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
p = list(m1_se[,4], 
                    m2_se[,4],
                    m3_se[,4],
                    m4_se[,4],
                    m5_se[,4]),
covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")))


#########################################################
## Interpreting size of effects 

mean(data_final$prop_attn) ## avg share of reports devoted to equality is 0.06

mean(data_final$attn) ## avg number of words per report is 9.79 or about 10 words

## averages pre to post quota in both countries 

## Italy

mean(data_final$attn[data_final$country=="Italy" & data_final$quota==0]) 
##10.3 words
mean(data_final$attn[data_final$country=="Italy" & data_final$quota==1]) 
## 14.5 words

## Greece

mean(data_final$attn[data_final$country=="Greece" & data_final$year<2011]) 
##5.14
mean(data_final$attn[data_final$country=="Greece" & data_final$year>2010]) 
## 6.54

###########################
###########################
## Replicates Figure 2 ####
###########################
###########################

### Left panel, number of women 

##Plot female vs male members by year

plot_female_number <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_no_year = mean(fem_sum, na.rm=T), .groups = 'drop')

plot_female_number <- as.data.frame(plot_female_number)


plot_fem <- ggplot(plot_female_number,
       aes(x=year, y=mean_no_year, col=country, group=country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 10) +
  scale_x_continuous(breaks = c( 
    2007,
    2008, 
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017)) +
  labs(y="Mean Number of Women Board Members",
       x="",
       title="",
       subtitle = "") +
 scale_y_continuous(breaks=seq(0.0, 10, 1), limits=c(0, 10))+
  theme(plot.title = element_text(size=12))+  theme_classic() + scale_colour_grey(start=0.8, end=0.2)
plot_fem


### Right panel, share of women 

plot_female_share <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_share_year = mean(fem_share, na.rm=T), .groups = 'drop')

plot_female_share <- as.data.frame(plot_female_share)

plot_fem2 <- ggplot(plot_female_share,
       aes(x=year, y=mean_share_year, col=country, group=country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 50) +
  scale_x_continuous(breaks = c( 
    2007,
    2008, 
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017)) +
  labs(y="Mean Share of Women Board Members",
       x="",
       title="",
       subtitle = "") +
  theme(plot.title = element_text(size=12))+  theme_classic() + scale_colour_grey(start=0.8, end=0.2)
plot_fem2
 

###########################
###########################
## Replicates Table 3 #####
###########################
###########################

m1dd<-lm(prop_attn ~ company + quota:year2011 + quota:year2012 + 
quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1dd, data_final$company)
m1_dd2 <- as.matrix(coeftest(m1dd, vcov_company)) 
 

m1ddb<-lm(fem_share ~ company + quota:year2011 + quota:year2012 + 
quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1ddb, data_final$company)
m1_dd4 <- as.matrix(coeftest(m1ddb, vcov_company)) 
 

m1ddc<-lm(prop_attn ~ company + quota:year2011 + quota:year2012 + 
quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
sustain + pct_rev_change + fem_share, 
       data=data_final)
vcov_company <- cluster.vcov(m1ddc, data_final$company)
m1_dd6 <- as.matrix(coeftest(m1ddc, vcov_company)) 
 

### Note: Need to manually delete year FE from stargazer output  

stargazer(m1dd, m1ddb, m1ddc,
          font.size = "small",
          title="Table 3: Effects of Gender Quota Law Over Time",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Share women", "Overall"),
order=c("quota:year2011", "quota:year2012",  
"quota:year2013", "quota:year2014", "quota:year2015", "quota:year2016", "quota:year2017",
"fem_share", "sustain", "pct_rev_change"),
          se = list(m1_dd2[,2], 
                    m1_dd4[,2],
                    m1_dd6[,2]  ),
 p = list(m1_dd2[,4], 
                    m1_dd4[,4],
                    m1_dd6[,4]  ),
                             omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("company", "Constant"),
          column.sep.width = "-15pt",
add.lines = list(c("Company FEs", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
 type = "html",
 out="Table3.doc")

# Note: Stargazer notes (beneath table) for p-values do not output correctly
# -- have to add in manually "*p<0.05; **p<0.01; ***p<0.001"

#### Table in Latex:


stargazer(m1dd, m1ddb, m1ddc,
          font.size = "small",
          title="Table 3: Effects of Gender Quota Law Over Time",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Share women", "Overall"),
order=c("quota:year2011", "quota:year2012",  
"quota:year2013", "quota:year2014", "quota:year2015", "quota:year2016", "quota:year2017",
"fem_share", "sustain", "pct_rev_change"),
          se = list(m1_dd2[,2], 
                    m1_dd4[,2],
                    m1_dd6[,2]  ),
 p = list(m1_dd2[,4], 
                    m1_dd4[,4],
                    m1_dd6[,4]  ),
                             omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("company", "Constant"),
          column.sep.width = "-15pt",
add.lines = list(c("Company FEs", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.")


###########################
###########################
## Replicates Table 4 #####
###########################
###########################


mshock<-lm(prop_attn ~ company + factor(year) + chg_14_10 + sustain  + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(mshock, data_final$company)
mshock_se <- as.matrix(coeftest(mshock, vcov_company)) 

mschock2<-lm(prop_attn ~ company+ factor(year) + below_mean_change_14_10 + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(mschock2, data_final$company)
mschock2_se <- as.matrix(coeftest(mschock2, vcov_company)) 
 

mshock3<-lm(prop_attn ~ company + factor(year) + above_mean_change_14_10 +  sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(mshock3, data_final$company)
mshock3_se <- as.matrix(coeftest(mshock3, vcov_company)) 
 

stargazer(mshock, mschock2, mshock3,
          font.size = "small",
          title="Table 4: Effects of Quota Shocks",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Overall",
            "Overall"),
          se = list(mshock_se[,2], 
                    mschock2_se[,2], 
                    mshock3_se[,2]),
    p = list(mshock_se[,4], 
                    mschock2_se[,4], 
                    mshock3_se[,4]),
                             omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year", "company", "Constant"),
          column.sep.width = "-15pt", star.cutoffs=c(.05, .01, .001),
add.lines = list(c("Company FEs", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes")),
digits = 3, digits.extra=0,
notes = "Robust standard errors clustered around company in parentheses.",
 type = "html",  out="Table4.doc")

#### Table in Latex: 

stargazer(mshock, mschock2, mshock3,
          font.size = "small",
          title="Table 4: Effects of Quota Shocks",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Overall",
            "Overall"),
          se = list(mshock_se[,2], 
                    mschock2_se[,2], 
                    mshock3_se[,2]),
    p = list(mshock_se[,4], 
                    mschock2_se[,4], 
                    mshock3_se[,4]),
                             omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year", "company", "Constant"),
          column.sep.width = "-15pt", star.cutoffs=c(.05, .01, .001),
add.lines = list(c("Company FEs", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes")),
digits = 3, digits.extra=0,
notes = "Robust standard errors clustered around company in parentheses.")

### Note that in the Table above star cutoff is set to .05, .1 symbol to be added 

############## compare effect sizes above and below mean

mshocks10<-lm(prop_attn ~  company + factor(year) + below_mean_change_14_10 + above_mean_change_14_10 +  sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(mshocks10, data_final$company)
mshocks10_se <- as.matrix(coeftest(mshocks10, vcov_company)) 

## Wald Chisq test -- for footnote 10

library(aod)
library(clubSandwich)

mwald<-lm(prop_attn ~  company + factor(year) + below_mean_change_14_10 + above_mean_change_14_10 +  sustain + pct_rev_change, 
       data=data_final)
V_sep <- vcovCR(mwald, cluster = data_final$company, type = "CR2")

C_sep <- constrain_equal(c("below_mean_change_14_10","above_mean_change_14_10"), coefs = coef(mwald))
Wald_test(mwald, constraints = C_sep, vcov = V_sep) ##p=.827


#################################
#################################
###  Online Appendix 
#################################
#################################

###########################
###########################
## Replicates Table A1 ####
###########################
###########################

library(MatchIt)

###load data 
mydata = read.csv("Data_for_matching_country_level.csv") 
nrow(mydata)

##subset to Italy + countries which did not have quota 2010-2016
mydata<-mydata[ which(mydata$Include_matching==1),]
nrow(mydata) ##18 rows, 17 countries to use in matching to Italy 

##subset to only variables that will be used in matching 
head(mydata)
vars<-c("country", "avg_share_women_2010", "flfp_ratio_2010", "gdp_per_capita_2010", 
"no_listed_2010", "womenpar", "family_spending_gdp")
newdata <- mydata[vars]

head(newdata)

##create indicator for quota country (Italy); all other countries are 0

newdata$ind<-0
newdata$ind[newdata$country=="Italy"]<-1

nearest.match<-matchit(formula=ind ~ avg_share_women_2010 + flfp_ratio_2010 + gdp_per_capita_2010 + family_spending_gdp + womenpar, data=newdata, method="nearest", distance="mahalanobis")

nearest.match$match.matrix
## matches to Greece (row.name = 9); Table A1 reports all countries and variable values

## to see these values
newdata


###########################
###########################
## Replicates Table A4 ####
###########################
###########################

vars<-c("year", "quota", "sustain", "pct_rev_change", "prop_attn",
"prop_lead", "prop_pay", "prop_family", "prop_harass","company", "country", "translated", "fem_share", 
"chg_14_10", "below_mean_change_14_10", "above_mean_change_14_10")
testing<-data_final[,vars]
nrow(testing)
stargazer(testing, title="Table A4: Summary Statistics for Sample")

### How many companies 
t(as.data.frame(table(testing$company))[,2]) 

## in  which countries
t(as.data.frame(table(testing$company[testing$country=="Italy"]))[,2]) 
t(as.data.frame(table(testing$company[testing$country=="Greece"]))[,2]) 

## how many reports in each country sample
table(testing$country)

## how many were translated in each country
table(testing$country, testing$translated)


###########################
###########################
## Replicates Table A5 ####
###########################
###########################

m1 <- lm(prop_attn ~ year_f + company  + quota, 
       data=data_final)
vcov_company <- cluster.vcov(m1, data_final$company)
m1_se <- as.matrix(coeftest(m1, vcov_company)) 

m2<-lm(prop_lead ~ year_f + company +  quota, 
       data=data_final)
vcov_company <- cluster.vcov(m2, data_final$company)
m2_se <- as.matrix(coeftest(m2, vcov_company)) 

m3<-lm(prop_pay ~ year_f + company + quota, 
       data=data_final)
vcov_company <- cluster.vcov(m3, data_final$company)
m3_se <- as.matrix(coeftest(m3, vcov_company)) 

m4<-lm(prop_family ~ year_f + company  + quota, 
                 data=data_final)
vcov_company <- cluster.vcov(m4, data_final$company)
m4_se <- as.matrix(coeftest(m4, vcov_company)) 

m5<-lm(prop_harass ~ year_f + company +  quota, 
       data=data_final)
vcov_company <- cluster.vcov(m5, data_final$company)
m5_se <- as.matrix(coeftest(m5, vcov_company))


stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table A5: Regression Results, No Controls",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
    p = list(m1_se[,4], 
                    m2_se[,4],
                    m3_se[,4],
                    m4_se[,4],
                    m5_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
add.lines = list(c("Company FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$"),
                           c("Year FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")

###########################
###########################
## Replicates Table A6 ####
###########################
###########################

## For this table, restrict data to only Italy 

data_final_italy<-data_final[ which(data_final$country=="Italy"), ]
nrow(data_final_italy)

m1a<-lm(prop_attn ~ company + year_f  + sustain + pct_rev_change, 
       data=data_final_italy)
vcov_company <- cluster.vcov(m1a, data_final_italy$company)
m1a_se <- as.matrix(coeftest(m1a, vcov_company)) 

m2a<-lm(prop_lead ~ company + year_f + sustain + pct_rev_change , 
       data=data_final_italy)
vcov_company <- cluster.vcov(m2a, data_final_italy$company)
m2a_se <- as.matrix(coeftest(m2a, vcov_company)) 

m3a<-lm(prop_pay ~ company + year_f + sustain + pct_rev_change , 
       data=data_final_italy)
vcov_company <- cluster.vcov(m3a, data_final_italy$company)
m3a_se <- as.matrix(coeftest(m3a, vcov_company)) 

m4a<-lm(prop_family ~ company + year_f + sustain + pct_rev_change , 
                 data=data_final_italy)
vcov_company <- cluster.vcov(m4a, data_final_italy$company)
m4a_se <- as.matrix(coeftest(m4a, vcov_company)) 

m5a<-lm(prop_harass ~ company + year_f + sustain + pct_rev_change , 
       data=data_final_italy)
vcov_company <- cluster.vcov(m5a, data_final_italy$company)
m5a_se <- as.matrix(coeftest(m5a, vcov_company))


stargazer(m1a, m2a, m3a, m4a, m5a,
          font.size = "small",
          title="Table A6: Regression Results, Italy Only",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1a_se[,2], 
                    m2a_se[,2],
                    m3a_se[,2],
                    m4a_se[,2],
                    m5a_se[,2]),
     p = list(m1a_se[,4], 
                    m2a_se[,4],
                    m3a_se[,4],
                    m4a_se[,4],
                    m5a_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("company", "Constant"),
add.lines = list(c("Company FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$"),
                           c("Year FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")


###########################
###########################
## Replicates Table A7 ####
###########################
###########################

## For this table, drop the year 2017 

data_final_no17<-data_final[ which(data_final$year<2017), ]
nrow(data_final_no17)


m1b<-lm(prop_attn ~ year_f + company + sustain + pct_rev_change + quota, 
       data=data_final_no17)
vcov_company <- cluster.vcov(m1b, data_final_no17$company)
m1b_se <- as.matrix(coeftest(m1b, vcov_company)) 

m2b<-lm(prop_lead ~ year_f + company + sustain + pct_rev_change + quota, 
       data=data_final_no17)
vcov_company <- cluster.vcov(m2b, data_final_no17$company)
m2b_se <- as.matrix(coeftest(m2b, vcov_company)) 

m3b<-lm(prop_pay ~ year_f + company + sustain + pct_rev_change + quota, 
       data=data_final_no17)
vcov_company <- cluster.vcov(m3b, data_final_no17$company)
m3b_se <- as.matrix(coeftest(m3b, vcov_company)) 

m4b<-lm(prop_family ~ year_f + company + sustain + pct_rev_change + quota, 
                 data=data_final_no17)
vcov_company <- cluster.vcov(m4b, data_final_no17$company)
m4b_se <- as.matrix(coeftest(m4b, vcov_company)) 

m5b<-lm(prop_harass ~ year_f + company + sustain + pct_rev_change + quota, 
       data=data_final_no17)
vcov_company <- cluster.vcov(m5b, data_final_no17$company)
m5b_se <- as.matrix(coeftest(m5b, vcov_company))


stargazer(m1b, m2b, m3b, m4b, m5b,
          font.size = "small",
          title="Table A7: Regression Results, Dropping Year 2017",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1b_se[,2], 
                    m2b_se[,2],
                    m3b_se[,2],
                    m4b_se[,2],
                    m5b_se[,2]),
   p = list(m1b_se[,4], 
                    m2b_se[,4],
                    m3b_se[,4],
                    m4b_se[,4],
                    m5b_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
 add.lines = list(c("Company FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$"),
                           c("Year FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")


###########################
###########################
## Replicates Table A8 ####
###########################
###########################

## This model includes leads :

##  lead2.quota      Quota(t+2)
##  lead1.quota      Quota(t+1)

m1_laglead<-lm(prop_attn ~ year_f + company + sustain + pct_rev_change +  lead2.quota + lead1.quota + 
                quota, data = data_final)
vcov_company <- cluster.vcov(m1_laglead, data_final$company)
m1_laglead_se <- as.matrix(coeftest(m1_laglead, vcov_company) )

m2_laglead<-lm(prop_lead ~ year_f + company + sustain + pct_rev_change +  lead2.quota + lead1.quota + 
                  quota, data = data_final)
vcov_company <- cluster.vcov(m2_laglead, data_final$company)
m2_laglead_se <-coeftest(m2_laglead, vcov_company) 

m3_laglead<-lm(prop_pay ~ year_f + company + sustain + pct_rev_change +  lead2.quota + lead1.quota + 
                  quota, data = data_final)
vcov_company <- cluster.vcov(m3_laglead, data_final$company)
m3_laglead_se <- as.matrix(coeftest(m3_laglead, vcov_company))

m4_laglead<-lm(prop_family ~ year_f + company + sustain + pct_rev_change +  lead2.quota + lead1.quota + 
                  quota, data = data_final)
vcov_company <- cluster.vcov(m4_laglead, data_final$company)
m4_laglead_se <- as.matrix(coeftest(m4_laglead, vcov_company))

m5_laglead<-lm(prop_harass ~ year_f + company + sustain + pct_rev_change +  lead2.quota + lead1.quota + 
                  quota, data = data_final)
vcov_company <- cluster.vcov(m5_laglead, data_final$company)
m5_laglead_se <- as.matrix(coeftest(m5_laglead, vcov_company))


stargazer(m1_laglead, m2_laglead, m3_laglead, m4_laglead, m5_laglead,
          font.size = "small",
          title="Table A8: Regression Results, Including Leads",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_laglead_se[,2], 
                    m2_laglead_se[,2],
                    m3_laglead_se[,2],
                    m4_laglead_se[,2],
                    m5_laglead_se[,2]),
        p = list(m1_laglead_se[,4], 
                    m2_laglead_se[,4],
                    m3_laglead_se[,4],
                    m4_laglead_se[,4],
                    m5_laglead_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
 add.lines = list(c("Company FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$"),
                           c("Year FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")



###########################
###########################
## Replicates Table A9 ####
###########################
###########################

## quota_12 = quota cutoff is set to 2012
## (switches from 0 to 1 in 2012 rather than 2011)


m1c<-lm(prop_attn ~ year_f + company + sustain + pct_rev_change + quota_12, 
       data=data_final)
vcov_company <- cluster.vcov(m1c, data_final$company)
m1c_se <- as.matrix(coeftest(m1c, vcov_company)) 

m2c<-lm(prop_lead ~ year_f + company + sustain + pct_rev_change + quota_12, 
       data=data_final)
vcov_company <- cluster.vcov(m2c, data_final$company)
m2c_se <- as.matrix(coeftest(m2c, vcov_company)) 

m3c<-lm(prop_pay ~ year_f + company + sustain + pct_rev_change + quota_12, 
       data=data_final)
vcov_company <- cluster.vcov(m3c, data_final$company)
m3c_se <- as.matrix(coeftest(m3c, vcov_company)) 

m4c<-lm(prop_family ~ year_f + company + sustain + pct_rev_change + quota_12, 
                 data=data_final)
vcov_company <- cluster.vcov(m4c, data_final$company)
m4c_se <- as.matrix(coeftest(m4c, vcov_company)) 

m5c<-lm(prop_harass ~ year_f + company + sustain + pct_rev_change + quota_12, 
       data=data_final)
vcov_company <- cluster.vcov(m5c, data_final$company)
m5c_se <- as.matrix(coeftest(m5c, vcov_company))


stargazer(m1c, m2c, m3c, m4c, m5c,
          font.size = "small",
          title="Table A9: Regression Results, Setting Quota Cutoff to 2012",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1c_se[,2], 
                    m2c_se[,2],
                    m3c_se[,2],
                    m4c_se[,2],
                    m5c_se[,2]),
 p = list(m1c_se[,4], 
                    m2c_se[,4],
                    m3c_se[,4],
                    m4c_se[,4],
                    m5c_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
 add.lines = list(c("Company FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$"),
                           c("Year FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")




###########################
###########################
## Replicates Table A10 ####
###########################
###########################

## prop_attn_no_lead = variable which excludes leadership category from overall attention

m1d<-lm(prop_attn_no_lead ~ year_f + company + sustain + pct_rev_change + quota, 
       data=data_final)
vcov_company <- cluster.vcov(m1d, data_final$company)
m1d_se <- as.matrix(coeftest(m1d, vcov_company)) 

m1dd<-lm(prop_attn_no_lead ~ company + quota:year2011 + quota:year2012 + 
quota:year2013 + quota:year2014 + quota:year2015 + quota:year2016 + quota:year2017 + 
year2007 + year2008 + year2009 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 +
sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1dd, data_final$company)
m1_dd2 <- as.matrix(coeftest(m1dd, vcov_company)) 


m1dda<-lm(prop_attn_no_lead ~ company + factor(year) + chg_14_10 +  sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1dda, data_final$company)
m1_dd7 <- as.matrix(coeftest(m1dda, vcov_company)) 

stargazer(m1d, m1dd, m1dda,
          font.size = "small",
          title="Table A10: Regression Results, Excluding Leadership from Overall Attention",
          align=TRUE, dep.var.labels=c(
            "Overall (No Leadership)"),
          se = list(m1d_se[,2], 
                    m1_dd2[,2],
                    m1_dd7[,2]),
      p = list(m1d_se[,4], 
                    m1_dd2[,4],
                    m1_dd7[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("company", "Constant"),
 add.lines = list(c("Company FEs", "$Yes$", "$Yes$", "$Yes$"),
                           c("Year FEs", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")

## Note that year FE need to be deleted from stargazer output for the table. 


###########################
###########################
## Replicates Table A11 ####
###########################
###########################

### Load the hand coded data from 20 companies 

load("company_level_20companies_merged.RData")
head(mer1)
nrow(mer1)

m1ef<-lm(prop_attn ~ year_f + company_f + sustain + quota, data=mer1)
vcov_company <- cluster.vcov(m1ef, mer1$company_f)
m1ef_se<- coeftest(m1ef, vcov_company) 

m2ef<-lm(prop_lead ~ year_f + company_f + sustain  + quota, data=mer1)
vcov_company <- cluster.vcov(m2ef, mer1$company_f)
m2ef_se<- coeftest(m2ef, vcov_company) 

m3ef<-lm(prop_pay ~ year_f + company_f + sustain + quota, data=mer1)
vcov_company <- cluster.vcov(m3ef, mer1$company_f)
m3ef_se<- coeftest(m3ef, vcov_company) 

m4ef<-lm(prop_family ~ year_f + company_f + sustain + quota, data=mer1)
vcov_company <- cluster.vcov(m4ef, mer1$company_f)
m4ef_se<- coeftest(m4ef, vcov_company) 

m5ef<-lm(prop_harass ~ year_f + company_f + sustain + quota, data=mer1)
vcov_company <- cluster.vcov(m5ef, mer1$company_f)
m5ef_se<- coeftest(m5ef, vcov_company)

stargazer(m1ef, m2ef, m3ef, m4ef, m5ef,
          font.size = "small",
          title="Table A11: ",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1ef_se[,2], 
                    m2ef_se[,2],
                    m3ef_se[,2],
                    m4ef_se[,2],
                    m5ef_se[,2]),
    p = list(m1ef_se[,4], 
                    m2ef_se[,4],
                    m3ef_se[,4],
                    m4ef_se[,4],
                    m5ef_se[,4]),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
add.lines = list(            c("Company FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$"),
      c("Year FEs", "$Yes$", "$Yes$", "$Yes$", "$Yes$", "$Yes$")),
digits = 3, digits.extra=0,
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Robust standard errors clustered around company in parentheses.",
          column.sep.width = "-15pt")

### Note : .1 symbols added to latex output

###########################
###########################
## Replicates Figure A1 ####
###########################
###########################

### Load the news coverage data 

data<- read.csv("snoq.csv")
head(data)

### plots of news coverage 
dat2 <- reshape2::melt(data, id.var='year')
head(dat2)

## note that dat2 is a factor variable 

p<- ggplot(dat2, aes(x=year, y=value, col=variable)) + geom_line()

p + scale_colour_discrete(name="News Coverage",
                         breaks=c("donne_cda", "donne_cda_quota", "donne_cda_snoq"),
                         labels=c("1. Women + Board of Directors", "2. (within 1) Gender quota", "3. (within 1) Se non ora quando"))+
 geom_vline(xintercept = 2011, linetype="dotted", 
                 size=1.4) + scale_x_continuous(name="Year") +
  scale_y_continuous(name="Number of News Articles")+
  theme_minimal() + ggtitle("")

######################
######################
######          ######
###### MY TWIST ######
######          ######
######################
######################

# Create duplicate dataset
data_final2 <- data_final

# Plot distributions of revenue and word count
hist(data_final2$revenue_num, nclass=100)
hist(data_final2$word_count, nclass=100)

summary(data_final2$revenue_num)

# Add new columns for and revenue (in billions) and word count (10K words)  
data_final2$revenue_num2 <- data_final2$revenue_num/1000000000
data_final2$word_count2 <- data_final2$word_count/10000

# Plot prop_attn against revenue_num
p1 <- data_final2 %>% 
  ggplot(aes(x = data_final2$revenue_num2, y = data_final2$prop_attn)) +
  geom_point(colour = "black") +
  geom_smooth(se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

p1

# Plot prop_attn against word_count2
p2 <- data_final2 %>% 
  ggplot(aes(x = data_final2$word_count2, y = data_final2$prop_attn)) +
  geom_point(colour = "black") +
  geom_smooth(se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

p2

# Run OLS regression using new variable selection (word_count & revenue_num)
m1_twist <- lm(prop_attn ~ year_f + company + quota + sustain + pct_rev_change +
                 word_count2 + I(word_count2^2) + I(word_count2^3) + 
                 revenue_num2 + I(revenue_num2^2) + I(revenue_num2^3), 
               data=data_final2)

vcov_company_twist <- cluster.vcov(m1_twist, data_final2$company)
m1_twist_se <- as.matrix(coeftest(m1_twist, vcov_company)) 

m2_twist<-lm(prop_lead ~ year_f + company + quota + sustain + pct_rev_change +
               word_count2 + I(word_count2^2) + I(word_count2^3) + revenue_num2 + I(revenue_num2^2) + I(revenue_num2^3), 
       data=data_final2)
vcov_company_twist <- cluster.vcov(m2_twist, data_final2$company)
m2_twist_se <- as.matrix(coeftest(m2_twist, vcov_company)) 

m3_twist<-lm(prop_pay ~ year_f + company + quota + sustain + pct_rev_change +
               word_count2 + I(word_count2^2) + I(word_count2^3) + revenue_num2 + I(revenue_num2^2) + I(revenue_num2^3), 
       data=data_final2)
vcov_company_twist <- cluster.vcov(m3_twist, data_final2$company)
m3_twist_se <- as.matrix(coeftest(m3_twist, vcov_company)) 

m4_twist<-lm(prop_family ~ year_f + company + quota + sustain + pct_rev_change +
               word_count2 + I(word_count2^2) + I(word_count2^3) + revenue_num2 + I(revenue_num2^2) + I(revenue_num2^3), 
       data=data_final2)
vcov_company_twist <- cluster.vcov(m4_twist, data_final2$company)
m4_twist_se <- as.matrix(coeftest(m4_twist, vcov_company)) 

m5_twist<-lm(prop_harass ~ year_f + company + quota + sustain + pct_rev_change +
               word_count2 + I(word_count2^2) + I(word_count2^3) + revenue_num2 + I(revenue_num2^2) + I(revenue_num2^3), 
       data=data_final2)
vcov_company_twist <- cluster.vcov(m5_twist, data_final2$company)
m5_twist_se <- as.matrix(coeftest(m5_twist, vcov_company))

## Note: Stargazer code uses SE and p-values from clustered SE models

stargazer(m1_twist, m2_twist, m3_twist, m4_twist, m5_twist,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_twist_se[,2], 
                    m2_twist_se[,2],
                    m3_twist_se[,2],
                    m4_twist_se[,2],
                    m5_twist_se[,2]),
          p = list(m1_twist_se[,4], 
                   m2_twist_se[,4],
                   m3_twist_se[,4],
                   m4_twist_se[,4],
                   m5_twist_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Pct Revenue Change",
                             "Word Count", "(Word Count)2", "(Word Count)3",
                             "Revenue Number", "(Revenue Number)2", "(Revenue Number)3"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "text",  out="Table2.doc")

# Run partial F-test
anova(m1_twist, m1)
