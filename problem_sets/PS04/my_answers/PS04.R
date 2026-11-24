#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices",
                      "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# load necessary packages
lapply(c("car"), pkgTest)


data(Prestige)
help(Prestige)


head(Prestige)
str(Prestige)


#Q1: 

# : making the dummy varrible :

  Prestige$professional <- ifelse(Prestige$type == "prof", 1,
                                ifelse(Prestige$type %in% c("bc", "wc"), 0, NA))

  table(Prestige$type, Prestige$professional, useNA = "ifany")

  Prestige$professional <- factor(Prestige$professional,
                                levels = c(0, 1),
                                labels = c("non-professional", "professional"))

#b : linear model :
  q1model <- lm(prestige ~ income * professional, data = Prestige)
  summary(q1model)

#c : Prediction equation:
  
  #Intercept: 21.142
  #Income: 0.003171
  #Professional: 37.781
  #Income:Professional : -0.002326
  
  #ŷ = 21.142 + 0.00317 * income + 37.781 * professional - 0.00233 (income * professional)

#d : Interpretation of income:
   
  #this is the slope of income, when professional is 0. therefore- this applies to blue and white collar workers. 
  #for non-professional- every additional dollar in income has a 0.0032 point increase in predicted prestige. 
  # or it can be written like this : +$1,000 = 3.17-point increase

#e : Interpretation of professional: 
  
  #this is difference in prestige between professionals and non professionals when income predicted as 0. 
  #with a $0 income, professionals are predicted to score 37.78 points higher than non-professionals.
  #this is baseline assumpsion 
  

#f: Effect of income for professionals:
  
  #measure effects of professionals and non-professionals different income slopes
  #plus professional into equation: 
  
  #Prediction equation:
  #ŷ = 21.142 + 0.00317 * income + 37.781 * professional - 0.00233 (income * professional)
  
  #for non-professionals (professional = 0): ŷ = 21.142 + 0.00317 * income + 37.781 * 0 - 0.00233 (income * 0)
  #ŷ = 21.142 + 0.003171 * income
  #income slope : 0.003171.
  
  #for professionals (professional = 1): ŷ = 21.142 + 0.00317 * income + 37.781 * 1 - 0.00233 (income * 1)
  #ŷ = 21.142 + 0.003171 * income + 37.781 - 0.002326 * income
  #ŷ = 58.923 + 0.000845 * income
  
  
  #the change per $1000 on a prestige score for professional ocumpations:
    #Δŷ= 1000 * 0.000845 
    #Δŷ= 0.845
  
  #ex: 
    #at $2000: 58.923 + 0.000845 (2000) = 60.613
    #at $3000: 58.923 + 0.000845 (3000) = 61.458
        #61.458 - 60.613 =  0.845

  b <- coef(q1model)
  1000 * (b[2] + b[4])

#g: Effect of being professional at income = $6,000:
  
  #the effect of professional depends on income = $6,000, so can plug that number into income in the formula 
  
  #Each group predictions at income =$6,000:
    #Non-professionals (professional = 0)
        #ŷ₀ = 21.142 + 0.00317 * (6000) + 37.781 * (0) - 0.00233 (6000 * 0)
        #ŷ₀ = 21.142 + 0.00317 * (6000)
        #ŷ₀ = 40.168  
    #Professionals (professional = 1)
        #ŷ₁ = 21.142 + 0.00317 * (6000) + 37.781 * 1 - 0.00233 (6000 * 1)
        #ŷ₁ = 63.963
     #Comparing ŷ₁ and ŷ₀:
     # ŷ₁ -  ŷ₀ = 63.993 - 40.168 = 23.83
  
    #if two people both earn $6000, but one as a non-professional job the other as a professional job.
    #the latter - will be predicted to have a 23.8 point increase in prestige
  
  b <- coef(q1model)
  b[3] + b[4] * 6000
  
  
  
  #Q2:
  
  #values from table 
  coeff  <- c(assigned = 0.042, adjacent = 0.042, constant = 0.302)
  se   <- c(assigned = 0.016, adjacent = 0.013, constant = 0.011)
  n    <- 131
  k    <- 3                 # 2 predictors + constant
  df   <- n - k             # residual degrees of freedom = 128
  alpha <- 0.05
  R2   <- 0.094
  
  
  #a
  #Assigned row: coefficient 0.042, standard error 0.016
  coeff1 <- 0.042
  se1 <- 0.016
  
    #Hypotheses :
      #H0 : β assigned = 0 , HA: β assigned ≠ 0 
      #The null says the signs do nothing. The alternative says they have some effect.
  
    #Test Statistic : 
      # t = coefficient / standard error 
      # t = 0.042 / 0.016 = 2.625 
      t1 <- coeff1 / se1
      #The estimate is about 2.6 standard errors away from 0.
  
    #Critical value :
      #The degrees of freedom are $131 - 3 = 128 
      #alpha = 0.05
      critical_value1 <- qt(p = 0.975, df = 128)
  
      
      p1 <- 2 * pt(abs(t1), df = 128, lower.tail = FALSE)
      print(p1)
      
      
      # Logical true/false check: Did our t test beat the critical value?
      if (abs(t1) <= critical_value1) {
        print("Hypothesis Outcome: FAIL TO REJECT the null hypothesis.")
        print("why becuase the calculated t-test did not exceed the critical value.")
      } else {
        print("Hypothesis Outcome: REJECT the null hypothesis.")
        print("why becuase the calculated t-test exceeded the critical value.")
      }
      
      #At the 5% level, the effect of the signs is statistically significant. Precincts assigned yard signs gave Cuccinelli about 4.2 percentage points more of the vote than comparison precincts, so the signs did affect vote share. Because precincts were randomly assigned, this can be treated this as a causal effect.
  
      #b
      #Adjacent row: coefficient 0.042, standard error 0.013
      coeff2 <- 0.042
      se2 <- 0.013
      
      #Hypotheses :
      #H0 : β Adjacent = 0 , HA: β Adjacent ≠ 0 
      #The null says being next to a sign precinct does nothing. The alternative says they have some effect.
      
      #Test Statistic : 
      # t = coefficient / standard error 
      # t = 0.042 / 0.013 = 3.230769
      t2 <- coeff2 / se2
      #The estimate is about 3.23 standard errors away from 0.
      
      #Critical value :
      #The degrees of freedom are $131 - 3 = 128 
      #alpha = 0.05
      critical_value2 <- qt(p = 0.975, df = 128)
      
      
      p2 <- 2 * pt(abs(t2), df = 128, lower.tail = FALSE)
      print(p2)
      
      
      # Logical true/false check: Did our t test beat the critical value?
      if (abs(t2) <= critical_value2) {
        print("Hypothesis Outcome: FAIL TO REJECT the null hypothesis.")
        print("why becuase the calculated t-test did not exceed the critical value.")
      } else {
        print("Hypothesis Outcome: REJECT the null hypothesis.")
        print("why becuase the calculated t-test exceeded the critical value.")
      }
      
      #At the 5% level, being adjacent to a precinct with yard signs has a statistically significant effect. Precincts next to sign precincts gave Cuccinelli about 4.2 percentage points more of the vote than precincts that neither had signs nor bordered a sign precinct.
      #However, This suggests the signs had a spillover effect: voters in neighboring precincts were influenced even though no signs were posted in their own precinct.
      
      
  #c 
  #The constant is the predicted value of the outcome when every predictor equals 0.
  #Constant (0.302) : precincts with no signs that also don't have any signs adjacent to it gave Cuccinelli about 30.2% of votes
  #Those precincts were untouched by the signs, this is our best estimate of what Cuccinelli’s vote share looks like without the treatment:
      #Assigned (0.302 + 0.042 = 0.344) : precincts with signs gave him about 4.2 percentage points more than that baseline (constant), so about 34.4%
      #Adjacent (0.302 + 0.042 = 0.344) : precincts next to sign precincts also gave him 34.4%
  #The constant’s standard error is small (0.011), so this baseline is estimated fairly precisely
      
  #d
      R2   <- 0.094
  #R2 is the share of the variation in the outcome that the model explains. Here R2 = 0.094, so the yard sign variables explain about 9.4% of the variation in Cuccinelli’s vote share across precincts
  #Alternativly- that means about 90.6% of the variation is not explained by the model
  #So- Yard signs and being next to them have a statistically significant effect, but most of the variation in vote share comes from other factors not seen in the model
