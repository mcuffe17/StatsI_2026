
##Question 1.a

##Make the contingency table 
con_tab <- matrix(c(14, 6, 7,
                7, 7, 1),
              nrow = 2, byrow = TRUE,
              dimnames = list(Class   = c("Upper", "Lower"),
                              Outcome = c("Not stopped", "Bribe requested", "Stopped/warning")))
print(con_tab)

# Step 1: totals of each row, column and the overall test
row_tot <- rowSums(con_tab)
col_tot <- colSums(con_tab)
n       <- sum(con_tab)

print(row_tot)
print(col_tot)
print(n)


# Step 2: expected counts = (row total * column total) / grand total
# multiplies every row total by every column total, giving the full grid at once
##this is the distribution according to expected counts under independence
expected <- (row_tot %*% t(col_tot)) / n
print(expected)

# Step 3: chi-square statistic = sum of ((O)bserved - (E)xpected ^2 / E over all cells)
chi_sq <- sum((con_tab - expected)^2 / expected)
print(chi_sq)


#alternativly can also do this : 
result <- chisq.test(con_tab)
print(result)
#extract the expected counts directly
result$expected


##Question 1.b
# Step 1: degrees of freedom and p-value
df <- (nrow(con_tab) - 1) * (ncol(con_tab) - 1)
p_val  <- pchisq(chi_sq, df = df, lower.tail = FALSE)

print(df)
print(p_val)


#so the question asks what happens when the alpha is equal to 0.1 or 10%
alpha <- 0.1

if (p_val <= alpha){
  print("results are statistically significant. Reject the null hypothesis")
}else{
  print("results are not statistically significant. Fail to reject the null hypothesis")
}
#this result means that there is not enough statistical evidence at the 10% significance level to suggest that the variables are related 


#Question 1.c
#standard error for each cell 
row_prop <- row_tot / n
col_prop <- col_tot / n
se <- sqrt(expected * ((1 - row_prop) %*% t(1 - col_prop)))
print(se)

#standardized residuals: (O - E) / se
st_re <- (con_tab - expected) / se
print(round(st_re, 3))


#Question 1.d

#so the alpha being 0.1 means that the cutoff is the z-score: +/- 1.645 for a two-tailed significance level (with 0.05 in each tail)
#The bribe request cell's residuals come out at +1.64 and -1.64. so just under the alpha. meaning that lower class drivers get asked for bribes more than expected with the independence- but since still before the alpha cutoff it is not significant
#the standardized residuals also show that upper-class drivers were stopped or warned somewhat more than expected (+1.52), though still under the cutoff.
#The residuals then tell you how far each cell is from what independence predicts, and none quite reaches significance. 
#Under independence- its roughly standard normal, so shows which cells drive a chi-square result, and the overall test can’t do that

tab_out <- matrix(sprintf("%.0f (%.2f) [%.2f]", con_tab, expected, st_re),
                  nrow = nrow(con_tab), dimnames = dimnames(con_tab))
noquote(tab_out)

round(100 * prop.table(con_tab, margin = 1), 1)

#this table shows row percentages of the observed counts since the classes have different sizes 
#this makes it easier to understand. roughly the same amount of each class was not stopped but lower-class drivers were asked for bribes about twice as often (46.7% vs 22.2%) and were rarely stopped or warned (6.7% vs 25.9%)


#Question 2.a

#Do villages with a female-reserved leader get more new or repaired drinking-water facilities?

#The effect of reservation is the slope β₁ in water = β₀ + β₁ · reserved + ε

#H₀: β₁ = 0. Reserving the GP head position for women has no effect on the number of new or repaired drinking-water facilities.
#H₁: β₁ ≠ 0. Reserving the GP head position for women has an (unknown: either positive or negative) effect on the number of new or repaired drinking-water facilities.


#Question 2.b

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#nn = observations, x = reserved, y = water
x <- women$reserved
y <- women$water
nn <- length(y)

print(x)
print(y)
print(nn)

#Compute the means x̄ and ȳ.
x_bar <- mean(x)
y_bar <- mean(y)

print(x_bar)
print(y_bar)

#Intercept: β̂₀ = ȳ − β̂₁·x̄
#Slope: β̂₁ = Σ(xᵢ − x̄)(yᵢ − ȳ) / Σ(xᵢ − x̄)²
Sxy <- sum((x - x_bar) * (y - y_bar))
Sxx <- sum((x - x_bar)^2)
b1  <- Sxy / Sxx
b0  <- y_bar - b1 * x_bar

print(Sxy)
print(Sxx)
print(b1)
print(b0)



fitted_values <- b0 + b1 * x              #prediction for each village       
residuals <- y - fitted_values            #actual minus predicted
sigma2 <- sum(residuals^2) / (nn - 2)     #measure of how noisy the data is and n − 2 instead of n because we estimated two things (b0 and b1) from the data, which uses up two “degrees of freedom.”
se_b1  <- sqrt(sigma2 / Sxx)              #standard error of b1. noise into uncertainty

print(fitted_values)
print(residuals)
print(sigma2)
print(se_b1)

#t-statistic, critical value, two-tailed p-value
t_stat <- b1 / se_b1
t_crit <- qt(0.975, df = nn - 2)
p_value  <- 2 * pt(-abs(t_stat), df = nn - 2)

print(t_stat)
print(t_crit)
print(p_value)

# 95% confidence interval
ci <- c(b1 - t_crit * se_b1, b1 + t_crit * se_b1)
print(ci)

results <- c(n = nn, b0 = b0, b1 = b1, se = se_b1,
             t = t_stat, t_crit = t_crit, p = p_value)
print(results)


#alternatively, can do this too 
summary(lm(water ~ reserved, data = women))


#The p-value answers: “If the null were true and reservation had no effect ---> how likely is it that I’d see an estimate this far from zero?”
#If p < 0.05, that’s unlikely, so you reject H₀.Estimated regression coefficient is large relative to its standard error, meaning the relationship is statistically significant
#If p ≥ 0.05, you fail to reject H₀. You don’t “accept” it, you just lack enough evidence against it.


if (abs(t_stat) > t_crit) {
  print("Reject the null hypothesises (H₀: β₁ = 0), as the t-stat was larger than the critical value")
  }else{
  print("Fail to reject H₀- not enough statisticsal evidence  against it")
}

#Question 2.c

#what the bivarate regression is trying to answer if the village was split in two groups, reserved = 1 and not reserved = 0, what is the comparison of the average number of water facilities within each.
#the regression is a way of doing the comparison and whether the gap is real or chance. 

#b0 (intercept): the average number of water facilities in the unreserved group.
#b1 (the coefficient to interpret): the difference between the two group averages, or the comparison. ((reserved minus unreserved) how many more or less, facilities do reserved villages have)
#se (standard error): how much b1/coefficient would change is there was a different sample of the villages. 
#t-statistic (b1 / se): measured how many standard errors (se's) away from zero. a larger t-stat (+/- 2) means difference is too big to be chance 
#p-value: if there was no effect, so the null (H₀) is true, what is the probability of an estimate this extreme? so when: p < 0.05 ---> and from there you reject the null it just means that this is unlikly that this difference is chance. 

#the coefficient on the reserved is 9.25. This means, that on average the villages in GP reserved for women leaders has over 9 more new or repaired drinking water facilities than the village in unreserved GPs.
#the p-value is 0.01970, which is below 0.05, so reject the null hypothesizes. meaning that there is statistically significant evidence that the reservation policy affects the number of drinking water facilities
#because the reservation was randomly assigned, the interpretation of this estimate as the effect of reservation









