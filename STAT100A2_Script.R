df <- read.csv("A2_data.csv")
Decathlon <- subset(df, sport == "Decathlon")
Heptathlon <- subset(df, sport == "Heptathlon")

 ### Research Q1: Is there a difference in high_jump performance between athletes in the different sports (Decathlon and Heptathlon)? ###

hist(Decathlon$high_jump)
hist(Heptathlon$high_jump)

### We assume that the samples are independent due to the nature of highjump being an individual event ###
### Observing the histograms of both subsets we see the dist is approx normal with no major outliers ###


### If we assume there to be no difference then our h0 for this question is xbar1=xbar2 ###
## Our hA therefore is xbar1!=xbar2 
t.test(Decathlon$high_jump, Heptathlon$high_jump)

### The 95% CI for the two sample t-test to determine the difference in high jump performance is ###
### 0.206 - 0.248 ###
### our p value < 2.2e-16 which is < 0.05 therefore there is significant evidence to reject null ###
### and assume that there is a difference in high jump performance between groups ###

### Research Q2: is there a difference in shotput performance for Heptathlon athletes based on countries? ###
mod.aov <- lm(Heptathlon$shotput~Heptathlon$country, data = Heptathlon)
anova(mod.aov)

### Here our 
