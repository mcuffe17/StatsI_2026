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

lapply(c(),  pkgTest)

library(tidyr)
library(ggplot2)

#####################
# Problem 1
##Education A school counselor was curious about the average of IQ of the students in her school and took a random sample of 25 students’ IQ scores. 
##The following is the data set is below: 
##Find a 90% confidence interval for the average student IQ in the school
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

##to find the confidence interval you must find the mean, variance, standard deviation, standard of error, margin of error 
##first step is find the mean 

sum_y <- sum(y)
print(sum_y)

length_y <- length(y)
print(length_y)

#mean = sum_y / Length_y

mean_y <- sum_y / length_y

print (mean_y)

##alternatively you can find the mean by going mean(y), as seen below:

mean(y)

##next you need to find the standard deviation. first step is to get the sum of every individual data value subtract the mean squared 
##to find this we can use a loop to go through each data value and subtract the mean, then square the results 
data_minus_mean <- 0 
for (i in 1:length(y)){
  data_minus_mean[i] <- y[i] - mean(y)
}
print (data_minus_mean)

squared_data <- data_minus_mean ^2

print(squared_data)

round(squared_data, digits = 2)


sum_SD <- sum(squared_data)

print(sum_SD)

## now that we have the sum of every individual data value subtracted the mean and squared, we need the sum_SD over the number of data values in this sample, aka the length(y) - 1

variance_y <- sum_SD / (length_y - 1)
 
print(variance_y)

standard_deviation <- sqrt(variance_y)

print(standard_deviation)

##alternatively you can also use sd(x)

sd_formula <- sd(y)

print(sd_formula)

##so now that we have our mean and our standard deviation, we can find the confidence interval for the average student IQ
## so the first step in doing the formula is finding the Standard Error (SE), which is just the SD over the square root of our sample number 

SE <- standard_deviation / (sqrt(length_y))
print(SE)

##the next step requires finding the Critical Value t*, this requires the sample number minus 1 (-1), and a two-tailed significance level of alpha = 0.10, so giving 0.05 in each tail)

critical_value<- abs(qt(p = .05, df = 24))
print(critical_value)

##next we need to find the margin of error which critical value by the Standard Error

margin_error <-  critical_value* SE

print(margin_error)

## and the last step in finding the confidence interval is to subtract and add the margin of error from the mean 

lower_limit <- mean(y) - margin_error
upper_limit <- mean(y) + margin_error

print(lower_limit)
print(upper_limit)

##You can state with 90% confidence that the true average of the entire population falls somewhere between 93.96 and 102.92.



##	Next, the school counselor was curious whether the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country. Using the same sample, conduct the appropriate hypothesis test with α = 0.05.

##To answer this question- we need to look at a lot of the same descriptive statistics we found in question 1.
##The Metrics: 
##Mean = 98.44 
##length = 25 
##Variance = 171.4233
##Standard Deviation = 13.09287 
##Standard Error = 2.618575
##Critical Value = 1.710882
##Margin of error = 4.480072
##lower limit = 93.95993
##upper limit = 102.9201
##alpha = 0.05


##Null Hypothesis (H0): μ ≤= 100 (The school's average IQ is less than or equal to the national average).
##Alternative Hypothesis (H1): μ > 100 (The school's average IQ is higher than the national average).

## first step is find the t-value using the standard of error and mean we used in question 1 
## so we need to find the mean - hypothetical mean / the standard of error


national_average <- 100
t_test <- (mean(y) - national_average) / SE 
print(t_test)

## this measures how far the sample data landed from the target, the national average. The data landed -0.596 standard errors below the target.

##my alternative hypothesis (H1) is that the school's sample meam IQ (98.44) is higher than the national average (100), therefore only need to look at the upper tail. this means a 1 sided test 

#the upper-tail p-value using the pt formula 
p_value <- pt(t_test, df = length_y - 1, lower.tail = FALSE)
print(p_value)


# Logical true/false check: Did our t test beat the critical value?
if (t_test <= critical_value) {
  print("Hypothesis Outcome: FAIL TO REJECT the null hypothesis.")
  print("why becuase the calculated t-test did not exceed the critical value.")
} else {
  print("Hypothesis Outcome: REJECT the null hypothesis.")
  print("why becuase the calculated t-test exceeded the critical value.")
}
#not only did the t-statistic equal = -0.596, far below the critical value of 1.711, the p-value was p = 0.72. p = 0.72 > 0.05,
#therfore we fail to reject the H0. this concludes there is insufficient evidence that the school' sample mean of the average IQ is higher than the national average of 100. 
#this is also consistent with the 90 confidence interval.



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
install.packages("GGally")
install.packages("tidyverse")
library(tidyverse)




summary(expenditure) #i want to see what i am working with for the table, this allows me to see the basic stats
head(expenditure) #very much like the summary but returns only the first 6 rows so i can see the variable layout, i can see Y, X1, X2, X3, as their actual data points, the state would be treated as a data point.

#im only pulling out the numeric data points- as state and region don't belong in a scatter plot 
expenditure_data1 <- (expenditure[, c("Y", "X1", "X2", "X3")])
round(cor(expenditure_data1), 2)


library(GGally)
ggpairs(expenditure_data1)
plot_ggpairs <- ggpairs(expenditure_data1)
print(plot_ggpairs)
ggsave("ggpairs_matrix.pdf", plot = plot_ggpairs, width = 8, height = 6)
##found inspiration to chart the plot/ graph like this via https://www.rdocumentation.org/packages/
##The relationships among Y, X1, X2, and X3 are displayed in the graph with the correlations. From the graph, you can see that X1, X2, and X3 each have a positive relationship with per capita shelter/housing expenditures (Y). The correlation between Y and X1, or personal income, has the strongest relationship with shelter expenditure data (r = 0.53), followed by X3/urbanization (r = 0.46) and financial insecurity (X2, r = 0.45). Each of these datasets represents states with higher income, more financially insecure residents, and/or more urban populations spending more per capita on housing assistance. Yet, none of these variables represents all of the variation in spending- the data in each panel resembles a moderate upward trend in spending.
##However, among the variables, the strongest correlation in the matrix is between income (X1) and urbanization (X3), with a correlation of r = 0.60, meaning that states with more residents living in urban areas have a higher income per capita. As the relationship between X1 and X3 is stronger than either variable’s relationship with Y, it may be worth investigating multicollinearity, as a lot of the information about X1 and X3 may overlap. From the graph, the lower correlation scores between variables such as income and financial insecurity (X1-X2, r = 0.21), and urbanization and financial insecurity (X2-X3, r = 0.22), suggest that financial insecurity may be an umbrella term for complexities not represented well within the data, such as debt or higher cost of living, rather than immediately associating financial insecurity with lower income.
##Overall, the matrix gives a plausible understanding of the states. For example, wealthier, more urban states may have higher tax brases that allocate more resources towards establishing social service infrastructure.

expenditure_labeled <- expenditure_data1
expenditure_labeled$Region <- factor(expenditure$Region, 
                             labels = c("Northeast", "North Central", "South", "West"))
plot_boxplot <-ggplot(expenditure_labeled, aes(x = Region, y = Y)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Housing Expenditure by Region",
       x = "Region",
       y = "Per capita housing expenditure")
print(plot_boxplot)
ggsave("boxplot_region.pdf", plot = plot_boxplot, width = 7, height = 5)


  aggregate(Y ~ Region, data = expenditure_labeled, FUN = mean)
  
  ##According the boxplots above, the region that has the highest per capita expenditure on housing assistance is the West or (4). Its mean sits at 88.3, whereas the next highest is the North Central region (2) with an average of 83.9. 
  ## The boxplot lines show medians, the means come from aggregate- but the West is also highest on the median, so the conclusion is the same.
  
  
  plot_y_x1 <-ggplot(expenditure, aes(x = X1, y = Y)) +
    geom_point() +
    labs(title = "Housing Expenditure vs Personal Income",
         x = "Per capita personal income (X1)",
         y = "Per capita housing expenditure (Y)")

  print(plot_y_x1)
  ggsave("scatter_Y_X1.pdf", plot = plot_y_x1, width = 7, height = 5)
  
  ##The graph between Y and X1 shows a positive correlation. Although from question 2.a it was already established in the matrix there was a moderately positive correlation (r = 0.53).  Therefore, as income rises across various states and regions, the housing expenditure tends to rise as well. However, the relationship isn't too tight as there are points that scatter somewhat upwards rather than a straight line of pure correlating evidence. This means income explains some, but not all, of the variation in housing spending within each state.
  
  plot_y_X1_regions <- ggplot(expenditure_labeled, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
    geom_point() +
    labs(title = "Housing Expenditure vs Personal Income by Region",
         x = "Per capita personal income (X1)",
         y = "Per capita housing expenditure (Y)",
         color = "Region",
         shape = "Region")
  print(plot_y_X1_regions)
  ggsave("scatter_Y_X1_regions.pdf", plot = plot_y_X1_regions, width = 7, height = 5)
  
##the positive relationship between income and housing expenditures, as seen in the previous graphs splits states by region. The southern states (the blue squares) sit towards the bottom left of the graph, as a group they are clustered at the low income, low spending end, sitting at 2000 in income and 80 in spending. meanwhile, the northeastern states (red circle) include a few of the highest income points, with spednding having a large range from 60-120. the north central states (green triangle) form a a tight little cluster in the middle of both spending and income variable. with 42 - 129 within spending at similar incomes levels. income doesn't explain their spending. 
##part of the income and spending relationship reflects difference bwteen each region. there are still lots of variations within each region that income doesn't account for
