rm(list = ls())

#Question 1.1
m = 11
n = 7

#initialize a matrix (n+1)x(m+1) as a placeholder for the deviations
D <- matrix(0,nrow = n+1, ncol = m+1)

#loop over the rectangular mxn grid to calculate D for all points (x,y) along the path
for (x in 0:m) {
  for (y in 0:n) {
    D[y+1,x+1] <- max(c(x/m - y/n, y/n - x/m))
  }
}

#calculate mean and standard deviation of D
mean(D)
sd(D)

#Question 1.2
#Let event A represent all values of D > 0.6
#Let event B represnt all values of D > 0.2

#Probability of A
Prob_A = length(D[D>0.6])/length(D)

#Probability of B
Prob_B = length(D[D>0.2])/length(D)

#Conditional probabaility = P(A and B)/P(B) = P[A]P[B]/P[B]
cond_prob = (Prob_A*Prob_B)/Prob_B

#Question 1.3
m = 23
n = 31

#initialize a matrix (n+1)x(m+1) as a placeholder for the deviations
D <- matrix(0,nrow = n+1, ncol = m+1)

#loop over the rectangular mxn grid to calculate D for all points (x,y) along the path
for (x in 0:m) {
  for (y in 0:n) {
    D[y+1,x+1] <- max(c(x/m - y/n, y/n - x/m))
  }
}

#calculate mean and standard deviation of D
mean(D)
sd(D)

#Question 1.4
#Let event A represent all values of D > 0.6
#Let event B represent all values of D > 0.2

#Probability of A
Prob_A = length(D[D>0.6])/length(D)

#Probability of B
Prob_B = length(D[D>0.2])/length(D)

#Conditional probabaility = P(A and B)/P(B) = P[A]P[B]/P[B]
cond_prob = (Prob_A*Prob_B)/Prob_B

#Question 2

#importing the datasets
setwd("Z:/Data Incubator")
montana <- read.csv("MT_cleaned.csv")
vermont <- read.csv("VT_cleaned.csv")


#Question 2.1 - Proportion of male drivers
male_prop <- nrow(subset(montana, driver_gender == "M"))/nrow(montana)

#Question 2.2
#proportion of out of state and in state
out_of_state <- subset(montana, out_of_state == T)
in_state <- subset(montana, out_of_state == F)

#prop of out of state plates arrested in Montana
out_of_state_prop <- nrow(subset(out_of_state, is_arrested == T))/nrow(subset(in_state, is_arrested == T))

#Question 2.3 chi-square test
tbl1 <- table(montana$out_of_state, montana$is_arrested)
chisq.test(tbl1)

#Question 2.4 - Proportion of speeding
speeding_df <- subset(montana, grepl("speeding", montana$violation, ignore.case = T)==T)
speeding_prop <- nrow(speeding_df) /nrow(montana)

#Question 2.5
#proportion of DUI in Montana
montana_dui_df <- subset(montana, grepl("DUI", montana$violation, ignore.case = T)==T)
montana_dui_prop <- nrow(montana_dui_df)/nrow(montana)

#proportion of DUI in Vermont
vermont_dui_df <- subset(vermont, grepl("DUI", vermont$violation, ignore.case = T)==T)
vermont_dui_prop <- nrow(vermont_dui_df)/nrow(vermont)

#odds pf DUI in Montana
montana_dui_prop/vermont_dui_prop

#Question 2.8

montana_copy <- montana
montana_copy$hours <- format(strptime(montana_copy$stop_time,"%H:%M"),'%H')
class(montana_copy$hours)
montana_copy <- montana_copy[ , c("id","hours")]

vermont_copy <- vermont
vermont_copy$hours <- format(strptime(vermont_copy$stop_time,"%H:%M"),'%H')
vermont_copy <- vermont_copy[ , c("id","hours")]

combined_df <- rbind(montana_copy, vermont_copy)

library(plyr)
hours <- na.omit(count(combined_df, "hours"))

max(hours$freq) - min(hours$freq)

############# Project #################

