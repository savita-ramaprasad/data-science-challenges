test_table <- read.csv("~/Downloads/take_home_challenge/challenge_2/Translation_Test/test_table.csv", stringsAsFactors = F)
# In order to know who was in the test group and who wasn't we need to have user's country information which we can get from user_table
user_table <- read.csv("~/Downloads/take_home_challenge/challenge_2/Translation_Test/user_table.csv", stringsAsFactors = F)

# join test_table with user_table on user_id
conversion <- merge(test_table, user_table, by = "user_id", all.x = T)
conversion <- conversion[ conversion$country != "Spain",]
#few checks
sum((conversion[conversion$country == "Spain" ,]$test)) # = 0 so Spain guys are never in test group 
length(unique(conversion$user_id)) == dim(conversion)[1]
#[1] TRUE so all ids are unique, each person appears just once


library("dummies")
conversion.dummy <- dummy.data.frame(conversion) 

#let's check if it is a RCT, informally  

sapply(conversion.dummy[conversion.dummy$test == 0 , ], mean)
sapply(conversion.dummy[conversion.dummy$test == 1, ], mean)
sapply(conversion.dummy[conversion.dummy$test == 0 & conversion.dummy$country != "Spain", ], var)
sapply(conversion.dummy[conversion.dummy$test == 1, ], var)

# Looks like it is a RCT with respect to non- country variables. For indicators of countries, mean and var does not match well.

sapply(conversion.dummy[conversion.dummy$test == 0, ], sum)
sapply(conversion.dummy[conversion.dummy$test == 1, ], sum)

# Also number of observations are not exactly equal. e.g. Argentina has 37377 obs in test and 9356 obs in control.  
# mean of conversion for test = 0 is larger than mean of conversion for test = 1. Looks like test result may be negative. Let's check if it is significantly larger for control.

test <- subset(conversion, test == 1)
control <- subset(conversion, test == 0 & country != "Spain") # let's remove Spain from control since nothing changed in Spain. 

t.test(test$conversion, control$conversion)

	# Welch Two Sample t-test

# data:  test$conversion and control$conversion
# t = -7.3345, df = 385340, p-value = 2.229e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
 # -0.006167678 -0.003566475
# sample estimates:
# mean of x  mean of y 
# 0.04342471 0.04829179 

# mean of test (x) is significantly smaller than control (y). This could be cause 1. maybe not enough test data 2. Bias introduced so that test/control not really random. 
library("dplyr")
# let's see variability in test and control data
tbyc <- conversion %>% group_by(date) %>% summarize(tbyc =  mean(conversion[test == 1])/mean(conversion[test == 0]))

lines(as.factor(tbyc$date), tbyc$tbyc)

#let's look at each country's test result. 
test <- subset(conversion, test == 1)
control <- subset(conversion, test == 0)
A <- tapply(test$conversion, test$country, mean)
B <- tapply(control$conversion, control$country, mean) # let's remove spain from control since it does not appear when in the test.
B <- B[names(B) != "Spain"]
#list countries for which conversion in test is greater than conversion in control
names(A)[as.numeric(A)-as.numeric(B) > 0]  

#Chile, Costa Rica, Mexico, Nicaragua, Panama, Paraguay, Peru, Uruguay. So for these countries the website with localized translations seem to be working better than the one spanish language for all! Let's if these changes are significant. 

# x <- tapply(test$conversion, test$country, identity)
# y <- tapply(control$conversion, control$country, identity)
# t.test(x, y )
# tapply(conversion, conversion$country, function(x) t.test(x[x$test == 1,]$conversion, x[x$test == 0,]$conversion) )
# tapply cannot be applied to the entire data frame and that is what I need so will use dplyr.

# inner join test_table with user_table on user_id while doing by country
conversion_inner <- merge(test_table, user_table, by = "user_id")
 
conversion_inner[conversion_inner$country != "Spain", ] %>% group_by(country) %>% summarize(p_value = t.test(conversion[test == 0], conversion[test == 1])$p.value) 
# none of differences are significant so test not negetive!

