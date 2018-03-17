conversion <- read.csv("~/Downloads/take_home_challenge/challenge_1/conversion_data.csv", stringsAsFactors = F)

str(conversion)
# 5 independent variables. 3 numeric

numberNA <- sapply(conversion, function(x) sum(is.na(x))) # vector of num of missing values
colWithNA <- names(conversion)[(numberNA != 0)] # columns with missing values
# = character(0), no missing values in the data

conversionNum <- conversion[sapply(conversion, is.numeric)] # numeric colomns of conversion
cor(conversionNum)
                            age    new_user total_pages_visited   converted
age                  1.00000000  0.01234259         -0.04592222 -0.08879735
new_user             0.01234259  1.00000000         -0.08254145 -0.15237387
total_pages_visited -0.04592222 -0.08254145          1.00000000  0.52899396
converted           -0.08879735 -0.15237387          0.52899396  1.00000000

summary(conversion)
  countryChina    countryGermany      countryUK        countryUS           age            new_user     
 Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   : 17.00   Min.   :0.0000  
 1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 24.00   1st Qu.:0.0000  
 Median :0.0000   Median :0.00000   Median :0.0000   Median :1.0000   Median : 30.00   Median :1.0000  
 Mean   :0.2423   Mean   :0.04129   Mean   :0.1532   Mean   :0.5632   Mean   : 30.57   Mean   :0.6855  
 3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.: 36.00   3rd Qu.:1.0000  
 Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :123.00   Max.   :1.0000  
   sourceAds       sourceDirect     sourceSeo      total_pages_visited   converted      
 Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   : 1.000      Min.   :0.00000  
 1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.: 2.000      1st Qu.:0.00000  
 Median :0.0000   Median :0.000   Median :0.0000   Median : 4.000      Median :0.00000  
 Mean   :0.2806   Mean   :0.229   Mean   :0.4903   Mean   : 4.873      Mean   :0.03226  
 3rd Qu.:1.0000   3rd Qu.:0.000   3rd Qu.:1.0000   3rd Qu.: 7.000      3rd Qu.:0.00000  
 Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :29.000      Max.   :1.00000  

# few German users, many Chinese users. 50% use search engines. 3.2% convert.  


# I will do the following steps. 1. create dummies for categorical variables 2. set random 10% of the sample as test data.
# 3. set remaining row to training data. 4. implement logistic regression and get predicted probabilites. 5. find mean sq error. 

# create dummy variables for all categorical variables
library("dummies")
conversion <- dummy.data.frame(conversion) 

testRows <- sample(1:nrow(conversion), 0.1*nrow(conversion)) # 10 percent random sample for test data 
test <- conversion[testRows, ]
training <- conversion[-testRows, ]

logit.reg <- glm(formula = converted ~  age + new_user + total_pages_visited + countryChina + countryUS + countryUK + sourceAds + sourceDirect, data = training, family = binomial(link = "logit"))

Coefficients:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -6.485532   0.114905 -56.442  < 2e-16 ***
age                 -0.073999   0.002502 -29.579  < 2e-16 ***
new_user            -1.753462   0.037573 -46.669  < 2e-16 ***
total_pages_visited  0.758075   0.006545 115.826  < 2e-16 ***
countryChina        -3.927790   0.141169 -27.823  < 2e-16 ***
countryUS           -0.631205   0.071361  -8.845  < 2e-16 ***
countryUK           -0.268258   0.077433  -3.464 0.000531 ***
sourceAds            0.020707   0.042051   0.492 0.622426    
sourceDirect        -0.137314   0.046554  -2.950 0.003182 ** 


T = 0.5 # threashold value
test.prob <- predict(logit.reg, test, type = "response")
test.predict <- ifelse(test.prob >= T, 1, 0)
conversionRate <- sum(test.predict)/length(test.predict)

test.true <- test$converted
# evaluate error
rmse <- sqrt(mean((test.prob - test.true)^2))  # doesn't depend on T

# fpr and tpr
sum(test.predict == 1)  # total values for which 1 was predicted
fp <- sum(test.true == 0 & test.predict == 1) # num of negative events wrongly categorized as positive
tp <- sum(test.true[test.predict == 1] == 1) 
total.n <- sum(test.true  == 0)
total.p <- sum(test.true == 1)
fpr <-  fp/total.n # prob that you reject null when null is true = 0.004, which is good
tpr <-  tp/total.p  # prob that you reject null when null is false. 0.697 ~ 0.7, which means 70% of conversions are predicted "conversion". Not bad.

# confusion matrix for training and test data
library("caret")
library("e1071")
train.prob <- predict(logit.reg, training, type = "response")
train.predict <- ifelse(train.prob >= 0.5, 1, 0)

t(confusionMatrix(factor(train.predict), factor(training$converted), positive = '1')
         Prediction
Reference      0      1
        0 274340   1078  1078/275418 = 0.0039 FPR 
        1   2854   6308  6308/9162 = 0.68 TPR

t(confusionMatrix(factor(test.predict), factor(test$converted), pos = '1')$table)
         Prediction
Reference     0     1
        0 30459   123    FPR = 0.004
        1   314   724    TPR 0.69
        
# similar values of FPR and TPR show we are not overfitting
        
# threashold selection; although T selecting is not very important in this context we'll see how f0(x) and f1(x) looks. 

f0x <- test.prob[test.true == 0]
f1x <- test.prob[test.true == 1]
plot(density(f0x))
par(new = T)
plot(density(f1x))

# from plot of distributions it looks like we would gain by reducing T, let's try 0.3. Confusion matrix for threashold = 0.3
confusionMatrix(factor(test.predict), factor(test$converted), pos = '1')
         Prediction
Reference     0     1
        0 30296   286   FPR = 0.009
        1   238   800   TPR = 0.77 

# Even though FPR increases slightly this T is better at predicting a conversion when a conversions occurs so I would choose T = 0.3.
# since our model has a fairly good predictive power, we can extract trustworthy insights from the coefficients.

1) Younger people are more likely to convert:
Use marketing channels which are used by young people
2) Old account users are more likely to convert:
Targeted emails with offers to bring old account users back to the site. 
3) Germans seem to convert but currently are a small percentage of the users. Market more to Germans. (Can have German translation, site specific to Germany)
4) Check if something is wrong with Chinese version of site since they are many users but do not convert as much
5) Let's plot conversion and age.
x = unique(conversion$age)
y = tapply(  conversion$converted,conversion$age, mean)
y shows that prob of conversion reduces with ages, try improving UI to make site useful for older people.
6) since total pages shows intent of purchase, someone with high total_pages_visited can be sent offers. Send reminders to people when they have something in the cart. 


  

