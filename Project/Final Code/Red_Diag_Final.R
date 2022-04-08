library(readr)
library(dplyr)
library(tidyverse)
library(corrplot)
library(moments)
library(car)
library(ggplot2)
library(ggrepel)
library(gridExtra)


red <- read_csv("winequality-red.csv")

sum(is.na(red))
red <- na.omit(red)
#Reading in the data


dfr <- as.data.frame(red)

r.mlm <- lm(quality ~.,data = dfr)

summary(r.mlm)
#Simple multiple linear regression and a summary for p-value
#P-value: Predictors with p-value > 0.05. Fixed Acidity (0.3357),
#Citric Acid (0.2150), Residual Sugar (0.2765), Density (0.4086)

dfr <- dfr %>% dplyr::select("quality", everything())

corrplot(
  cor(absorp),
  type = "upper",
  method = "shade",
  title = "Red Wine Correlation Plot",
  mar=c(0,0,2,0),
  diag = FALSE,
  tl.srt = 45,
  addCoef.col = "black",
  addCoefasPercent = TRUE
)

#Moving quality to the first column so it shows up at the top of the corrplot
#and creating the corrplot

#Variables of interest: Low correlation with Quality: Residual Sugar

par(mfrow = c(2, 2))
plot(r.mlm)
mtext("Red Wine Diagnosis Plots", side = 3, line = -2, outer = TRUE)

#Plotting the diagnostic plots for r.mlm

par(mfrow= c(1,1))
#This resets the plot window

source(file = "rss_regress_funcs_v2.R")
#R-script containing numerous functions for verifying L.I.N.E assumptions

residFit(r.mlm, title2 = "Red Wine using residFit()")

summary(r.mlm)

#Linearity: Both the Residuals vs Fitted plot and the R-squared:(0.3606),
#indicate that linearity assumption is satisfied. Same pattern in plotted points
#observed as with white wine dataset for the same reason.

acf(r.mlm$residuals)

dwt(r.mlm)

#Independent-errors: A number of values past lag 0 actually exceed the 95%
#confidence interval lines for the ACF plot, but they are very close so it is
#not cause for concern. Additionally the D-W statstic at 1.75 suggests some
#positive auto-correlation. Overall, not much concern as this test is mainly
#for time-series data

r.skew <- skewness(r.mlm$residuals)
r.kurt <- kurtosis(r.mlm$residuals)

red.standard_res = rstandard(r.mlm, sd = 16)
ggplot() +
  geom_histogram(aes(red.standard_res)) +
  labs (
    x = "Standarized Residual",
    y = "Count",
    title = "Histogram of lm-calculated standardized residuals for Red wine",
    subtitle = paste("skewness:",r.skew,",","kurtosis:",r.kurt, sep = " ")
  )

#Histogram plot: Shows a normal distribution, with a kurtosis value of 3.71
#there is a likelihood of more "extreme" values

normalQQ(r.mlm, title2 = "Red wine using normalQQ()")

#QQ plot: Appears normal with no clear indications otherwise

residLeverage(r.mlm, title2 = "Red wine using residLeverage()")

#Residuals vs Leverage plot: Everything appears well within expected values

cooksDistance(r.mlm, title2 = "Red wine using cooksDistance()")

#Cook's Distance plot: No values identified by this plot

#Normality: The normality assumption appears satisfied with no clear indications
#of any outliers

residFit(r.mlm, title2 = "Red wine using residFit()")

scaleLocation(r.mlm, title2 = "Red Wine data using scaleLocation()")

#Equal-variances:Not much information gained due to ordinal response variable
#pattern

corrplot(
  cor(dfr),
  type = "upper",
  method = "shade",
  diag = FALSE,
  tl.srt = 45,
  addCoef.col = "black",
  addCoefasPercent = TRUE
)

vif(r.mlm)

#Multicollinearity: There we no indications of multicollinearity or any
#predictors with high VIF values

#The exploratory analysis did not indicate any courses of action be taken. 
#Potential predictor removal may take place when examing results of data
#analysis models




