df <- read.csv("A2_data.csv")
Decathlon <- subset(df, sport == "Decathlon")
Heptathlon <- subset(df, sport == "Heptathlon")

 ### Research Q1: Is there a difference in high_jump performance between athletes in the different sports (Decathlon and Heptathlon)? ###

hist(Decathlon$high_jump)
hist(Heptathlon$high_jump)

shapiro.test(Decathlon$high_jump)
shapiro.test(Heptathlon$high_jump)

### We assume that the samples are independent due to the nature of highjump being an individual event ###
### Observing the histograms of both subsets we see the dist is approx normal with no major outliers ###
### Additionally, Shapiro-Wilk normailty test suggests samples do not differ significantly from a norm dist ###



### If we assume there to be no difference then our h0 for this question is xbar1=xbar2 ###
## Our hA therefore is xbar1!=xbar2 
t.test(Decathlon$high_jump, Heptathlon$high_jump)

### The 95% CI for the two sample t-test to determine the difference in high jump performance is ###
### 0.206 - 0.248 ###
### our p value < 2.2e-16 which is < 0.05 therefore there is significant evidence to reject null ###
### and assume that there is a difference in high jump performance between groups ###




### Research Q2: Is there a difference in shotput performance for Heptathlon athletes based on countries? ###
table(Heptathlon$country) ### Check the groups to assess normality##

countries <- unique(Heptathlon$country)
# Loop to check normality in each subset
for (country in countries) {
  subset_data <- Heptathlon[Heptathlon$country == country, ]
  cat("Shapiro-Wilk Test for:", country, "\n")
  print(shapiro.test(subset_data$shotput))
  cat("\n")
}

### Shapiro-Wilk test fails for UKR so will check qqnorm plot
subsetUKR <- Heptathlon[Heptathlon$country == "UKR",]
qqnorm(subsetUKR$shotput)
qqline(subsetUKR$shotput)
### The right hand tail outlier suggests this doesnt follow a normal dist and will be excluded from study

noUKR <- subset(Heptathlon, countries != "UKR")
mod.aov <- lm(noUKR$shotput~noUKR$country, data = noUKR)
anova(mod.aov)

### Here our F statistic is 2.4293 and corresponding p-value of 0.09412. This does not provide us strong enough evidence to reject 
### the null hypothesis that there is no difference between an athletes country (EST, RUS, NED) and shotput performance. ###

## Research Q3: Is there a linear relationship between age and hurdles for decathalon competitors?
### H0: there is no linear relationship b1 = 0
### HA: there is a linear relationship b1 != 0
hurdleAge <- lm(Decathlon$hurdles ~ Decathlon$age)

### We check the linearity and variance to see if the model is fit for inference ###
plot(Decathlon$hurdles ~ Decathlon$age, 
     xlab = "Age (days)", 
     ylab = "Hurdles Time (s)", 
     main = "Hurdle Time vs Age")
abline(lm(Decathlon$hurdles ~ Decathlon$age), col = "red")
plot(hurdleAge, which = 1)
plot(hurdleAge, which = 2)

### In the residuals vs fitted plot, the points have a even spread across y = 0 with no major deviations in the 
### red line we can assume that a linear model is appropriate for the data.

summary(hurdleAge)

### The p-value of our linear model for age and hurdles time is 0.00187 giving us
### reason to reject the null hypothesis and assume that age is a predictor for hurdle score.
### The estimate for the slope is -1.266e-4 suggesting that an increase in one day age reduces 
### hurdle time by 1.266e-4 seconds on average.
### The strength of correlation for these variables is weak with an R-squared value of 0.1087
 
