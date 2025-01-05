df <- read.csv("A2_data.csv")
Decathlon <- subset(df, sport == "Decathlon")
Heptathlon <- subset(df, sport == "Heptathlon")

 ### Research Q1: Is there a difference in high_jump performance between athletes in the different sports (Decathlon and Heptathlon)? ###

hist(Decathlon$high_jump)
hist(Heptathlon$high_jump)

### We assume that the samples are independent due to the nature of highjump being an individual event ###
### Observing the histograms of both subsets we see the dist is approx normal with no major outliers ###


### If we assume there to be no difference then our h0 for this question is xbar1=xbar2 ###
## Our hA there for is xbar1!=xbar2 