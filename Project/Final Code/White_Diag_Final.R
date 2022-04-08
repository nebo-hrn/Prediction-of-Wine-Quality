library(readr)
library(dplyr)
library(tidyverse)
library(corrplot)
library(moments)
library(car)
library(ggplot2)
library(ggrepel)
library(gridExtra)

white <- read_csv("winequality-white.csv")

sum(is.na(white))
white <- na.omit(white)
#Reading in the data

dfw <- as.data.frame(white)

w.mlm <- lm(quality ~.,data = dfw)

summary(w.mlm)

#Simple multiple linear regression and a summary for p-value
#P-value: Predictors with p-value > 0.05: Citric Acid (0.81759),
#Chlorides (0.65097), Total Sulfur Dioxide (0.44979)

dfw <- dfw %>% dplyr::select("quality", everything())

corrplot(
  cor(dfw),
  type = "upper",
  method = "shade",
  title = "White Wine Correlation Plot",
  mar=c(0,0,2,0),
  diag = FALSE,
  tl.srt = 45,
  addCoef.col = "black",
  addCoefasPercent = TRUE
)
#Moving quality to the first column so it shows up at the top of the corrplot
#and creating the corrplot

#Variables of interest: Low correlation with Quality: Citric Acid, Free Sulfur
#Dioxide. High correlations with predictors: Density - Residual Sugar (84),
#Density - Alcohol (-78)

par(mfrow = c(2, 2))
plot(w.mlm)
mtext("White Wine Diagnosis Plots", side = 3, line = -2, outer = TRUE)

#Plotting the diagnostic plots for w.mlm

par(mfrow= c(1,1))
#This resets the plot window

source(file = "rss_regress_funcs_v2.R")
#R-script containing numerous functions for verifying L.I.N.E assumptions

residFit(w.mlm, title2 = "White Wine using residFit()")

summary(w.mlm)

#Linearity: Both the Residuals vs Fitted plot and the R-squared:(0.2819),
#indicate that linearity assumption is satisfied. A distinct pattern is noted
#and explained due to ordinal nature of the response variable. The distinct
#downward slope between fitted values 3 - 4 can be explained by the limited
#number of observations as well as the outlier at labelled point 2782.

acf(w.mlm$residuals)

dwt(w.mlm)

#Independent-errors: A number of values past lag 0 actually exceed the 95%
#confidence interval lines for the ACF plot, but they are very close so it is
#not cause for concern. Additionally the D-W statstic at 1.62 suggests some
#positive auto-correlation. Overall, not much concern as this test is mainly
#for time-series data

w.skew <- skewness(w.mlm$residuals)
w.kurt <- kurtosis(w.mlm$residuals)
  
white.standard_res = rstandard(w.mlm, sd = 16)
ggplot() +
  geom_histogram(aes(white.standard_res)) +
  labs (
    x = "Standarized Residual",
    y = "Count",
    title = "Histogram of lm-calculated standardized residuals for White wine",
    subtitle = paste("skewness:",w.skew,",","kurtosis:",w.kurt, sep = " ")
  )

#Histogram plot: Shows a normal distribution, with a kurtosis value of 4.1
#there is a likelihood of more "extreme" values

normalQQ(w.mlm, title2 = "White wine using normalQQ()")

#QQ plot: Q-Q plot appears normal with some slight deviation at the ends
#indicating possible outliers. Observations "2782" and "4746" potential
#candidates

residLeverage(w.mlm, title2 = "White wine using residLeverage()")

#Residuals vs Leverage plot: This plot shows one candidate for an outlier, 
# "2782"

cooksDistance(w.mlm, title2 = "White wine using cooksDistance()")

#Cook's Distance plot: This plot agrees with the residuals vs leverage plot
# "2782" is a strong candidate for outlier

#Normality: Normality assumption seems to be satisfied with a couple
#observations potentially being outliers identified.

residFit(w.mlm, title2 = "White wine using residFit()")

scaleLocation(w.mlm, title2 = "White Wine data using scaleLocation()")

#Equal-variances: Not much information gained due to ordinal response variable
#pattern

corrplot(
  cor(dfw),
  type = "upper",
  method = "shade",
  diag = FALSE,
  tl.srt = 45,
  addCoef.col = "black",
  addCoefasPercent = TRUE
)

vif(w.mlm)

#Multicollinearity: The correlation plot indicates a high positive correlation
#between Density and Residual Sugar (84), Density and Alcohol are highly
#negatively correlated (-78) but do not cross the 80 threshold. Additionally,
#VIF agrees with the initial analysis, with Density (28.23) and Residual Sugar
#(12.64) having VIF values over 10


#Results Interpretation: Based on all the information above two courses of
#action were taken moving forward.

#1) The observation "2782" is removed from all datasets. This was demed a low
#risk decision as removing 1 observation from a dataset of 4898 observations
#will not change the results.

#2) A dataset without the predictors Density since it has high correlation with
#other predictors as well as a very high VIF value.

#All exploratory analysis will be retested using the updated datasets. All
#future data analysis will include datasets with all predictors and with all
#predictors less Density.

dfw <- dfw[-2782,]
dfw2 <- subset(dfw, select = -density)

#Updated datasets created

w.mlm <- lm(quality ~.,data = dfw)

summary(w.mlm)

corrplot(
  cor(dfw),
  type = "upper",
  method = "shade",
  title = "White Wine Correlation Plot",
  mar=c(0,0,2,0),
  diag = FALSE,
  tl.srt = 45,
  addCoef.col = "black",
  addCoefasPercent = TRUE
)

par(mfrow = c(2, 2))
plot(w.mlm)
mtext("White Wine Diagnosis Plots", side = 3, line = -2, outer = TRUE)

par(mfrow= c(1,1))
#This resets the plot window

residFit(w.mlm, title2 = "White Wine using residFit()")

summary(w.mlm)

acf(w.mlm$residuals)

dwt(w.mlm)

w.skew <- skewness(w.mlm$residuals)
w.kurt <- kurtosis(w.mlm$residuals)

white.standard_res = rstandard(w.mlm, sd = 16)
ggplot() +
  geom_histogram(aes(white.standard_res)) +
  labs (
    x = "Standarized Residual",
    y = "Count",
    title = "Histogram of lm-calculated standardized residuals for White wine",
    subtitle = paste("skewness:",w.skew,",","kurtosis:",w.kurt, sep = " ")
  )

normalQQ(w.mlm, title2 = "White wine using normalQQ()")

residLeverage(w.mlm, title2 = "White wine using residLeverage()")

cooksDistance(w.mlm, title2 = "White wine using cooksDistance()")

residFit(w.mlm, title2 = "White wine using residFit()")

scaleLocation(w.mlm, title2 = "White Wine data using scaleLocation()")

vif(w.mlm)

#Re-tested exploratory analysis with "2782" observation removed. No real change
#noted with some values actually higher like Density and Residual Sugar VIF at
#39.46 and 16.099 respectively, furthering the argument for Density to be
#removed. May also removed "4746" but not enough evidence to make this
#necessary

w.mlm2 <- lm(quality ~.,data = dfw2)

summary(w.mlm2)

corrplot(
  cor(dfw2),
  type = "upper",
  method = "shade",
  title = "White Wine Correlation Plot",
  mar=c(0,0,2,0),
  diag = FALSE,
  tl.srt = 45,
  addCoef.col = "black",
  addCoefasPercent = TRUE
)

par(mfrow = c(2, 2))
plot(w.mlm2)
mtext("White Wine Diagnosis Plots", side = 3, line = -2, outer = TRUE)

par(mfrow= c(1,1))
#This resets the plot window

residFit(w.mlm2, title2 = "White Wine using residFit()")

summary(w.mlm2)

acf(w.mlm2$residuals)

dwt(w.mlm2)

w.skew2 <- skewness(w.mlm2$residuals)
w.kurt2 <- kurtosis(w.mlm2$residuals)

white.standard_res2 = rstandard(w.mlm2, sd = 16)
ggplot() +
  geom_histogram(aes(white.standard_res2)) +
  labs (
    x = "Standarized Residual",
    y = "Count",
    title = "Histogram of lm-calculated standardized residuals for White wine",
    subtitle = paste("skewness:",w.skew2,",","kurtosis:",w.kurt2, sep = " ")
  )

normalQQ(w.mlm2, title2 = "White wine using normalQQ()")

residLeverage(w.mlm2, title2 = "White wine using residLeverage()")

cooksDistance(w.mlm2, title2 = "White wine using cooksDistance()")

residFit(w.mlm2, title2 = "White wine using residFit()")

scaleLocation(w.mlm2, title2 = "White Wine data using scaleLocation()")

vif(w.mlm2)
#Re-tested exploratory analysis with "2782" observation removed and Density
#predictor removed. All VIF values are below 10, no correlations are above 80.
#P-values have also dropped. Citric Acid p-value is still above 0.05 and may
#be a candiate for future removal if data analysis does not yield any results.