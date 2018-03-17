library(data.table)
fraud <- fread( "~/Google Drive/take_home_challenge/challenge_4/Fraud/fraud.csv")
library("bit64")
ip_country <- fread( "~/Google Drive/take_home_challenge/challenge_4/Fraud/IpAddress_to_Country.csv")


country.ip <- function(ip) {
	 
	 x <- ip_country[ lower_bound_ip_address <= ip & upper_bound_ip_address >= ip, country ]
	 if (length(x) == 1) return(x)
	 else return(NA_character_)
	 
	}

start = Sys.time()
fraud$country <- sapply(fraud$ip_address, country.ip )
print(Sys.time() - start)

write.csv( fraud, "~/Google Drive/take_home_challenge/challenge_4/Fraud/fraud.csv")

#alternative
sub.fraud <- fraud[1:20]
start <- Sys.time()
sub.fraud$country <- sub.fraud[ , .(country = sapply(ip_address, country.ip))]
print(Sys.time() - start)
length(unique(fraud$user_id)) # = 151,112 all new users, so can't use past behavior.
fraud[ signup_time == purchase_time , .N ] # = 0. lets extract signup and purchase date. 

fraud$signup_time <- as.POSIXct(fraud$signup_time)
fraud$purchase_time <- as.POSIXct(fraud$purchase_time)

fraud$signup_week <- format(fraud$signup_time, "%U")
fraud$purchase_week <- format(as.Date(fraud$purchase_time), "%U")

barplot(table(format(fraud$purchase_time, "%B")))
hist(fraud$purchase_value) 
fraud[  , .N , by = .(sex, class)]
f <- fraud[ , .(odds = sum(class == 1)/.N, age), by = (age)]
plot(f$age, f$odds)
#fraud$signup_date <- substr(fraud$signup_time, 1, 10)
#fraud$purchase_date <- substr(fraud$purchase_time, 1, 10)
#fraud.subset <- fraud[1:60 ]

fraud[ as.Date(signup_time) == as.Date(purchase_time), .N ] # = 8204. these people signed up and purchased on the same day. 

fraud[is.na(signup_time), .N]
length(unique(fraud$device_id)) # 13156 repeats on the device, prolly by family memnbers. 

#dict <- c( F = 30,  M = 44)
#sub.fraud$avgage <- dict[sub.fraud$sex]
#age <- ifelse( sub.fraud$age == 24, dict[sub.fraud$sex]  , sub.fraud$age )

# variables: 
fraud$sign_pur_diff <- with(fraud, as.POSIXct(purchase_time) - as.POSIXct(signup_time))
#uniq.dev <- unique(sub.fraud$device_id)
fraud$dev.mult.users <-  table(fraud$device_id)[fraud$device_id] >  1  
# unique ip address
fraud$not.uniq.ip <- table(as.character(fraud$ip_address))[as.factor(fraud$ip_address)] > 1 
fraud$purchase_week <- format(as.Date(fraud$purchase_time), "%U")
fraud$purch_day_wk <- format(as.Date(fraud$purchase_time), "%a")

#fraud[ , .(sign_pur_diff = sign_purch) ]
names(fraud)[names(fraud) == "sign_purch"] <- "sign_pur_diff" # works
names(fraud)[names(fraud) == "purchase_day_of_year"] <- "purch_day_wk" # works

# fill missing values in country:
fraud$country[is.na(fraud$country)] <- "Not_found"
# reducing country categories by keeping top 50 countries
top50 <- names(sort(table(fraud$country), dec = T))[1:50]
fraud$country <- with(fraud, ifelse(country %in% top50, country, "Other"))

# delete unwanted variables
fraud[ , c("V1", "user_id", "signup_time", "device_id", "ip_address", "purchase_time")] <- NULL

total_fraud_wk <- fraud[ , .(total = sum(class == 1)), by = .(purchase_week)]
with(total_fraud_wk, plot(purchase_week, total)
# based on the plot, one could have levels: weeks 00, 01 and 02 and "rest of the weeks". 
#fraud$purchase_wk <- with(fraud, ifelse(!(purchase_week %in% c("00", "01", "02")), "03-50", purchase_week))
# this migtht not be reqd with random forests, since it will split approprately. so using week number as numeric

fraud$purchase_wk <- as.numeric(fraud$purchase_week)
# delete purchase_week
fraud$purchase_week <- NULL

#convert to data.frame
fraud <- as.data.frame(fraud)
# convert character variables into factors
fraud[sapply(fraud, is.character)] <- lapply(fraud[sapply(fraud, is.character)], as.factor)

# convert class to factor
fraud$class <- as.factor(fraud$class)

x <- fraud[ , ! (names(fraud) %in% "class") ]
library(randomForest)
rf <- randomForest( x = x , y = fraud$class)

# probabilty predictions for training data. 

p_prob <- predict(rf, newdata = x, type = "prob")
# check if predictions match for thresh = 0.5
#p_prob is matrix. p_prob[, 1] = prob of being 0. Therefore predictions at T = 0.5 is 
pred_0.5 <- ifelse(p_prob[ , 2] > 0.5, 1, 0 )
#comapre with predictions from the rf function.
p_response <- predict(rf, data = fraud, type = "response") # this is a factor. convert to numerical
p_response <- as.numeric(as.character(p_response))
head(p_response)
p_prob[(p_response != pred_0.5)]
p_response[(p_prob[ , 2] == 0.5)] # rows 6394, 75552, one has been assigned 1 and 0 for prob = 0.5
sum( p_response != pred_0.5) #= 1 so almost same

fpr_tpr <- function( thr, prob = p_prob[, 2] ){
	
	 true <- as.numeric(as.character(fraud$class))
	 true_0 <- sum(true == 0)
	 true_1 <- sum(true == 1)
	 pred_thr <- ifelse(prob > thr, 1, 0)
	 fp <- sum(true == 0 & pred_thr == 1)
	 tp <- sum(true == 1 & pred_thr == 1)
	 fpr <- fp/(true_0)
	 tpr <- tp/(true_1)
	 return(c(fpr, tpr))
}

threasholds = seq(0.001, 0.999, 0.009)
roc_values <- data.frame(t(sapply(threasholds, fpr_tpr)))
colnames(roc_values) <- c("fpr", "tpr")
plot(roc_values$fpr, roc_values$tpr, type = "l")
abline(a = 0, b = 1, col = "red")

write.csv(roc_values , "~/Google Drive/take_home_challenge/challenge_4/Fraud/roc_values.csv")
#in fact we don't need a threashold, since we have the probabilty that a person commits a fraud. 
#for example: 
u <- fraud[2, -6]
prob <- predict(rf, newdata = u, type = "prob") #gives probability that that person commits a fraud. Using that one can make a decision, say by having two threasholds. T1 and T2.

# 1. if prob(fraud = 1) < T1 --> innocent.
# 2. < T1 and > T2 --> further review - create additional verification step- like enter code sent on phone number. 
# 3. > T2 most likely fraud - put session on hold - review manually. 
# the good feature about the current model is that false positive for a wide range of threashold values. So a non-fraud won't be labbeled as fraud. but since true positive rate is not very high, frauds  may go undetected, so additionals check may be required. 

# further let's see which variables are important
 importance(rf)
               # MeanDecreaseGini
# purchase_value       1816.52789
# source                313.98526
# browser               546.60445
# sex                   228.00008
# age                  1563.97618
# country              1313.84146
# sign_pur_diff        6236.54362
# dev.mult.users       3075.85805
# not.uniq.ip            22.03062
# purch_day_wk          738.14537
# purchase_wk          6230.42813

# looks like purchase week and signup purchase diff is important. 
# hard to see using tree the increase of probabilty of fraud due to the variable since the tree is so big. getTree(rf, 2, TRUE) gives the second tree. can use fewer rows ~ 100 to create the tree, so can track the rows to see the effect of variable. an be misleading but can still give insight. 
# can use visual. 

fraud <- data.table(fraud)
fraud$class <- as.numeric(as.character(fraud$class))
sig_purch_p <- fraud[ , .(ratio = sum(class == 1)/ (.N)), by = .(as.numeric(sign_pur_diff))]
sp <- sig_purch_p
plot( x = sp[, 1][[1]] , y = as.numeric(sp$ratio))
# looks like people who people who create account and make purchase on the same day are more likely to cheat. 0 diff is 0.93 prob of fraud. 
