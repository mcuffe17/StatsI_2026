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

library(tidyverse)
library(dplyr)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/inc.sub.csv")

#Q1
#calculating the correlation coefficient, checking to see if linear 
coefficient <- cor.test(inc.sub$voteshare, inc.sub$difflog)
coefficient$estimate

# the corelation coefficient is 0.6060866, means moderatly strong linear relationship 

# This creates a simple linear regression model where voteshare is the outcome variable and difflog is the predictor variable. 
reg_model <- lm(voteshare ~ difflog, data = inc.sub)
summary(reg_model)


#predictor = x , outcome = y

#scatterplot of voteshare vs. difflog with regression line
plotq1b <- inc.sub %>% 
  ggplot(aes(x = difflog, y = voteshare)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Difference in log campaign spending (incumbent - challenger)",
       y = "Incumbent vote share")
plotq1b
ggsave("q1_scatterplot.png", plotq1b, width = 6, height = 4)

#residuals as its own object 
res1 <- residuals(reg_model)


#Get the intercept and slope from summary(reg_model)
#voteshare_predictor = [intercept] + [slope] * difflog

# voteshare_predictor = 0.579031 + 0.041666 · difflog



#Q2
#calculating the correlation coefficient, checking to see if linear 
coefficient2 <- cor.test(inc.sub$presvote, inc.sub$difflog)
coefficient2$estimate

# the corelation coefficient for Q2 is 0.2965653, meaning there is  a postive linear relationship, but not super strong 

# regression: presvote on difflog
#presvote is the outcome and difflog is the predictor.
reg_model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(reg_model2)


# scatterplot with regression line
plotq2b <- inc.sub %>%
  ggplot(aes(x = difflog, y = presvote)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Difference in log campaign spending (incumbent - challenger)",
       y = "Presidential candidate vote share (incumbent's party)")
plotq2b
ggsave("q2_scatterplot.png", plotq2b, width = 6, height = 4)

# residuals saved as their own object
res2 <- residuals(reg_model2)


#Get the intercept and slope from summary(reg_model2)
#presvote_predictor = [intercept] + [slope] * difflog

# presvote_predictor = 0.507583 + 0.023837 · difflog



#Q3
reg_model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(reg_model3)

plotq3b <- inc.sub %>%
  ggplot(aes(x = presvote, y = voteshare)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  labs(x = "Presidential candidate vote share (incumbent's party)",
       y = "Incumbent vote share")
plotq3b
ggsave("q3_scatterplot.png", plotq3b, width = 6, height = 4)


#Get the intercept and slope from summary(reg_model3)
#voteshare_predictor = [intercept] + [slope] * presvote

# voteshare_predictor = 0.441330 + 0.38018 · presvote



#Q4 
# outcome = residuals from Q1 (res), explanatory = residuals from Q2 (res2)
reg_model4 <- lm(res1 ~ res2)
summary(reg_model4)

plotq4b <- ggplot(data.frame(res1 = res1, res2 = res2), aes(x = res2, y = res1)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(x = "Residuals of presvote ~ difflog (Q2)",
       y = "Residuals of voteshare ~ difflog (Q1)")
plotq4b
ggsave("q4_scatterplot.png", plotq4b, width = 6, height = 4)

#Get the intercept and slope from summary(reg_model4)
#res_predictor = [intercept] + [slope] * res2

#to find the slope
coef(reg_model4)["res2"] 

# res_predictor = -1.942e-18 + 0.256877· res2


#Q5
reg_model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(reg_model5)

#Get the intercept and slope from summary(reg_model5)
#res_predictor = [intercept] + [slope] * difflog + presvote

coef(reg_model5)["presvote"]

# voteshare_predictor = 0.4486442 + 0.0355 · difflog + 0.2569 * presvote

