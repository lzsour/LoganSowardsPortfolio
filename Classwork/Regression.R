# HillsRegression
# Data read in from: http://www.statsci.org/data/general/hills.txt 
# the record time in Scottish hill races using race distance and hill climb



hills <- read.table(header = TRUE, text = "
Race	Distance	Climb	Time
Greenmantle	2.5	650	16.083
Carnethy	6	2500	48.35
CraigDunain	6	900	33.65
BenRha	7.5	800	45.6
BenLomond	8	3070	62.267
Goatfell	8	2866	73.217
BensofJura	16	7500	204.617
Cairnpapple	6	800	36.367
Scolty	5	800	29.75
Traprain	6	650	39.75
LairigGhru	28	2100	192.667
Dollar	5	2000	43.05
Lomonds	9.5	2200	65
CairnTable	6	500	44.133
EildonTwo	4.5	1500	26.933
Cairngorm	10	3000	72.25
SevenHills	14	2200	98.417
KnockHill	3	350	78.65
BlackHill	4.5	1000	17.417
CreagBeag	5.5	600	32.567
KildconHill	3	300	15.95
MeallAnt-Suidhe	3.5	1500	27.9
HalfBenNevis	6	2200	47.633
CowHill	2	900	17.933
NBerwickLaw	3	600	18.683
CreagDubh	4	2000	26.217
Burnswark	6	800	34.433
LargoLaw	5	950	28.567
Criffel	6.5	1750	50.5
Acmony	5	500	20.95
BenNevis	10	4400	85.583
Knockfarrel	6	600	32.383
TwoBreweries	18	5200	170.25
Cockleroi	4.5	850	28.1
MoffatChase	20	5000	159.833
") 

# Evidence of Correct Data
head(hills)
tail(hills)


plot(x = hills$Distance, y = hills$Time, main = "Race Time vs. Distance",
     xlab = "Distance", ylab = "Time")
identify(x = hills$Distance, y = hills$Time, hills$Race)
# Unusual Results at BensofJura and KnockHill (Outliers, vertical direction)


plot(x = hills$Climb, y = hills$Time, main = "Race Time vs. Climb",
     xlab = "Climb", ylab = "Time")
identify(x = hills$Climb, y = hills$Time, hills$Race)
# Unusual Results at KnockHill, SevenHills, and LairigGhru (Outliers, vertical direction)



# Extended EDA (Data Diagnostics)

# Assumption of Normality
#   Outliers are a violation of Normality

hills.out <- lm(Time ~ Distance + Climb, data=hills)

# Assess normality assumption
#  (Concerned about shape, outliers)

# compute R-studentized residuals (R = remove observation when computing residual
#             studentized residuals = standardized (not unit dependent) residual check)
hills.R <- rstudent(hills.out)

subset(hills.R, hills$Race == "CairnTable")
# Normal generally close to 0?

# graphic
hist(hills.R)
hist(hills.R, freq=FALSE)
my.z <- seq(-3,3, length= 100)
lines(my.z, dnorm(my.z,0,1), lty=2, col="royalblue")
# Doesn't appear to be normal due to very large outliers

# Alternative Graphic
plot(density(hills.R))
my.z <- seq(-3,3, length= 100)
lines(my.z, dnorm(my.z,0,1), lty=2, col="royalblue")

# These plots emphasize the lack of normality

# Hypothesis test for Normality
#  Shapiro-Wilk Test of Normality
# Ho: dist is normal
# Ha: dist is not normal

shapiro.test(hills.R)
#  p-value < 0.001 therefore we reject the null hypothesis, so non-normal (due to outliers)


# Hunt for outliers

# Kildcon Hill?
subset(resid(hills.out), hills$Race=="KildconHill")
#  This isn't a good way to look at it... try R-studentized residual
#  because it has a normal dist
subset(hills.R, hills$Race=="KildconHill")
# Within 1 so looks alright


# Knock Hill?
subset(hills.R, hills$Race=="KnockHill")
# Very large result, much greater than 2 (3 is basically the large cutoff)


# identify all outliers (will likely filter these from analysis)
subset(hills, abs(hills.R) > 3)




# Influential Observations
# (Unusual in the horizontal direction)

# Good influential(happy to have something!)

# Bad influential (changing the fit elsewhere)


# Diagnostics for influence
# Leverage (weight an object has in predicting itself)
# 0 is small leverage, 1 is large leverage (or influence)
hills.leverage <- lm.influence(hills.out)$hat

# leverage for Ben Nevis
subset(hills.leverage, hills$Race == "BenNevis")

# Cook's Distance
hills.cd <- cooks.distance(hills.out)

# Cook's Distance for Moffat Chase
subset(hills.cd, hills$Race=="MoffatChase")

# Rule of thumb: Leverage Threshold = [2 * # of parameters estimated (incuding y intercept)] / # of obs
2*3 / 35

# Cook's Distance: threshhold = 4 / [# of obs - # parameters estimated (incuding y intercept)]
4 / (35-3)

# Identify Influential Observations
subset(hills$Race, hills.leverage > 2*3/35)

# Alternative to above 
subset(hills$Race, hills.leverage > 2* length(coef(hills.out))/ dim(hills)[1])

# Identify using Cooks's
subset(hills$Race, hills.cd > 4/(35-3))
# Alternative to above 
subset(hills$Race, hills.cd > 4/ (dim(hills)[1] - length(coef(hills.out))))


# Investigate Larig Ghru Race (good or bad influential)
par(mfrow=c(1,2)) #two plots one row two columns therefore sideby side

plot(Time~Distance, data=hills)
points(Time~Distance, data=subset(hills, Race== "LairigGhru"),
       col="red", pch=19) #Overlay a specific point


plot(Time~Climb, data=hills)
points(Time~Climb, data=subset(hills, Race== "LairigGhru"),
       col="red", pch=19)


par(mfrow=c(1,1)) #Returns to one graph one window

# Bad differential on climb
# Doesn't really belong with the data set, because really long but really short climb
# Doesn't fit it with the rest of the data




# filter the dataset to races that our model predicts
# List of things we will remove: 
#     Knock Hill(Outlier, we think it was misrecorded)
#     Lairig Ghru(Bad Influention.. short climb for such a long race)
#     Bens of Jura(outlier and bad influential... terrain is boggy and rocky) highlights a weakness of our model

# Take out the bad rows
hills1 <- hills[c(-18,-11,-7),] #show dataset is filtered once


# Alternate Easy EDA
plot(hills.out)





# Analysis:
# Research Question: Inference: Does hill length and climb have a statistically significant 
# effect on the time to complete the race

# Response variable: Record Time in minutes
# Explanatory variables: Hill climb and race length

# Model:
# Time = Beta0 + Beta1*Distance + Beta2*Climb + Epsilon
#       Where epsilon ~ N(0, sigma^2)
#   model assumptions: additive model (straight line), 
#                      constant variation after adjusting for effect of X, 
#                      outcome of one 
#                      observation doesn't effect outcome of another observation, 
#                      variation is normally distributed

# Fit model:
hills.out <- lm(Time~Climb + Distance, data = hills1) #methane is not adding co2, it is creating 2 columns

# table of estimates and std errors
summary(hills.out)


# If all other factors were held constant, for every one increase in Climb, we expect 
# an increase of 0.0077 in time

# If all other factors were held constant, for every one increase in Distance, we expect
# an increase of 6.8377 in time

# Graphic
# Two ways to visualize/ graphics to demonstrate the regression coefficients
library(car)

# Partial Regression Plot (AKA Added Variable Plots)
avPlots(hills.out)
# Sort of shows a distnce from the means or deviations of the average

# Component Plus Residual Plot
crPlots(hills.out)
# distance from average
# purple line is a sort of best fit line and if it matches blue line it is a good fit


# Testing Null Hypothesis:
# Ho: No climb and Distance effect
# Ho: Beta1 = Beta2 = 0
# Ha: An effect exists for time and/or distance
# Ha: At least 1 Betaj != 0

# Test Ho: No effect
# ANOVA F-test (get F-statistic and p-value from summary)
# p-value < .001
# p-value very small, therefore time and/or distance has a statistically 
# significant effect on race



# There is an effect but could be just Distance, Climb, or both
# Look back at individual parts and their p-value in summary
# Because p-values for Climb and Distance after adjusting for factors are both less than 0.05 
#     we conclude that both are statistically significant

# 95% Confidence Intervals
confint(hills.out)[-1,]

# We are 95% confident that the true mean effect of climb on time is between 0.0048 to 0.0106
# We are 95% confident that the true mean effect of Distance on time is between 5.8829 to 7.7924



# Data Features:
#   Continuous response variable
#   More than one continuous explanatory variable
#   Rows of observations (either randomized experiment or observational study)

# Strengths:
#   Model with interpretable estimates of effects
#   Graphics that support inference conclusions
#   Statistical Significance of model effect on response variable
#   Estimate of effect with confidence interval
#   Statistical significance of each explanatory variable effect on response variable


# Weaknesses:
#   explanatory variable must be continuous
#   model assumptions must be satisfied: additive model (straight line), 
#     constant variation after adjusting for effect of X, outcome of one 
#     observation doesn't effect outcome of another observation, variation is 
#     normally distributed
#   Data only useful within a certain range

