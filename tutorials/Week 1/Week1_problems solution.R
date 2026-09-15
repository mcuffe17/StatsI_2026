# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function

sum_y <-sum(y)
print(sum_y)

# (2) find mean of y using your "own" function
# now do the same thing, but faster using the built-in R function

mean_y <- sum(y)/length(y)
print(mean_y)

# (3) find sum of demeaned values
demeaned <- 0 
for (i in 1:length(y)){
  demeaned[i] <- y[i] - mean(y)
}
print(demeaned)

#orrrrr empty <- y - mean(y)

demeaned <- y - mean(y)

print(demeaned)

# (4) calculate sum of squared error

squared_error <- demeaned ^2

print(squared_error)
###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median 

?median
sort(quantilesVec)
med <- median(quantilesVec)

print(med)

# (2) calculate quantiles

?quantile
quantile_quantilesVec(quantilesVec, c(0.25,0.5,0.75), type = 1)
print(quantile_quantilesVec)

boxplot(quantilesVec, col = "lightblue")

# (3) make a histogram of state median income
state.x77[,2]

hist((state.x77[,2]), main = "distribution of state median income", xlab = "median state income")


# remember to save your plot as a pdf
pdf("medianstatehist.pdf")
hist((state.x77[,2]), main = "distribution of state median income", xlab = "median state income")
dev.off()

