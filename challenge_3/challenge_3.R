library(data.table)
retention <- fread( "~/Google Drive/take_home_challenge/challenge_3/employee_retention_data.csv")
head(retention)
dim(retention)
length(unique(retention$company_id)) # 12 companies 
#so rows of data is ~ 365*5*12. Almost 5 years for 12 companies 

retention.dt[ , c("quit_date", "join_date")] <-  lapply(retention.dt[ , c("quit_date", "join_date")], as.Date)
#or 
retention.dt[ , c("quit_date", "join_date")] <-  lapply(retention.dt[ , .(quit_date, join_date)], as.Date)

#retention.dt[ , .(quit_date, join_date)] <-  lapply(retention.dt[ , .(quit_date, join_date)], as.Date)  won't work since "." function can be used for subsetting but cannot be used for assignment. 

# udf: head count of employees for a given date for a pair of vector quit date and join date.
count.date <- function(join_date, quit_date, date){
	  count1 <- sum(join_date[ is.na(quit_date) ]  <= date) 
	  count2 <- sum(join_date[ !is.na(quit_date) ]  <= date & quit_date[ !is.na(quit_date)]   > date ) 
		count1 + count2
} 
# given a date returns employee headcount for each company for that date
get.headcount.list <- function(x){ 
	
	retention[ , .(day = x, employee_headcount =  count.date(join_date, quit_date, x)), by = .(company_id)]
	 
	}
seq.date <- seq(as.Date("2011-01-24"), as.Date("2015-12-13") , by = "day")
start = Sys.time()
# headcount has the hc for each companies for each date.
headcount <- rbindlist(lapply(seq.date, get.headcount.list)) 
print(Sys.time() - start)

write.csv(headcount, "~/Google Drive/take_home_challenge/challenge_3/headcount.csv")


# another, more concise way to do the same thing. 
start <- Sys.time()
headcount2 <- rbindlist(lapply(seq.date, function(date) {retention[(is.na(quit_date) & (join_date <= date)) | ( !is.na(quit_date) & join_date <= date & quit_date > date), .( .N , date) , by = .(company_id)][order(company_id)] }))
print(Sys.time() - start)

library(rpart)
retention$quit <- as.factor(with(retention, ifelse(!is.na(quit_date), 1, 0)))


lm1 <- lm(as.numeric(quit) ~ salary + factor(company_id) + factor(dept) + seniority, data = retention )
# people with higher salaries are less likely to quit, since they are quite happy with what they are making in the present company they don't feel the need to quit.
#the more experience one had the more likely they were to quit. however, seniority may be linked with salary. 
#sale and marketing folks quit more compare to customer_service. 

salary.by.dept <- retention[ , .(mean(salary)), by = dept ]
barplot(  height = salary.by.dept$V1 , names = substr(salary.by.dept$dept, 1,5))


salary_senior <- retention[seniority < 98 , .(mean(salary)), by = seniority ][order(seniority)]
plot(salary_senior$seniority, salary_senior$V1)
#seniority of 98 , 99 years doesn't make sense.

#  fitting a tree also gives us the range of value of the variables for which the probability of quitting is high and for which values of the variable low. This is not true for a linear model, which just gives a single linear trend. the number of variables that we care about (salary, company id, dept, seniority ) are few in number, making analyzing the result of a tree easier. 

tree1 <- rpart(quit ~ salary + factor(company_id) + factor(dept) + seniority, data = retention)
plot(tree1, uniform = T)
text(tree1, use.n = T, all = T, cex = 0.8)
# people with salary greater than 246000 are less likely (which agrees with the linear model) 
# increasing the depth of the tree. 
tree2 <- rpart(quit ~ salary + factor(company_id) + factor(dept) + seniority, data = retention, maxdepth = 2, cp = 0.001)
plot(tree2, uniform = T)
text(tree2, use.n = T, all = T, cex = 0.8)
# increasing the depth shows people with good salary (greater than 246000) and people with low salary (< 61500) are less likely to quit. Therefore giving us more information than a linear model. People with lower salaries might want to gain more experience (since they tend to have lower experience) which will help them command a higher salary in future. People with lower salaries may also be getting as many offers from other companies. There is however no outside company information in this data set, so will not be able to explore that. 
# from the two models above, looks like salary is the most important variable. 

# the probabality values of quitting or not quititing are close to 0.5. This does not give us enough confidence in the result/ split. Salary is not able to split the data into 0, 1 properly. We're looking for a almost degerate distribution at the terminal nodes. Therefor lets look at a different model.  

# we don't have data on the number of years spent in the current company before the employee quit. 
retention$days.before.quit <- with(retention, ifelse(!is.na(quit_date), as.Date(quit_date) - as.Date(join_date), as.Date("2015-12-23") - as.Date(join_date) ))
with(retention[retention$quit == 1], hist(days.before.quit, breaks = 100) )
# few people quit within a year and most people seem to quit after their first year. Another smaller peak at year 2. People quit at year anniversaries. Again typical of employee behaviour. People stay to get sign-on bonus and stocks.  

let us see what drives people to quit within a year/ 13 months of joining. 
retention$early_quit <- with(retention, ifelse( days.before.quit <= 396, 1, 0))
# Consider people who join just before the data collection stops, let's say in June 2015. These guys have a label quit = 0, but may have actually been early quitters. We don't have enough data on them to label them. So exclude non- quitters who join late. 
# rather keep early quitters and non quitters who join 13 months before data collection end date (2015-12-13) -- early quitters and early joiners.

early_quit_join <- retention[early_quit == 1 | ( quit == 0 &  join_date < (as.Date("2015-12-13") - (365 + 31)))]
tree3 <- rpart(as.factor(early_quit) ~ salary + factor(company_id) + factor(dept) + seniority , data = early_quit_join , maxdepth = 2)
# since most people in this data set are early quitters - data is already fairly degenate. (0.3, 0.6) - no variable has enough information gain after split. 

month_of_year <- as.numeric(format(retention$quit_date, "%m"))
month_of_year <- month_of_year[!is.na(month_of_year)]
barplot(table(month_of_year))
# people seem to quit less during december, this might be due to holiday season.


growth <- function(id, join){
	
	hc_60 = headcount[company_id == id  & day == join + 60, employee_headcount  ]
	hc <- headcount[company_id == id  & day == join, employee_headcount  ]
	(hc_60 - hc)/hc
}
size_company <- function(id, join){
	
	headcount[company_id == id & day == join, employee_headcount ]
	}

growth <- retention[join_date < (as.Date("2015-12-13") - 60) , .(growth_comp = mapply(growth ,company_id, join_date))]
comp.growth <- retention[join_date < (as.Date("2015-12-13") - 60)]
comp.growth$growth <- growth$growth_comp

# fast growing companies probably have better employees who have great opportunites outside of the company and therefore more likely to quit

size <- retention[ join_date < (as.Date("2015-12-13") - 60), .(size = mapply(size_company, company_id , join_date))]
comp.growth$size <- size$size 

write.csv(comp.growth, "~/Google Drive/take_home_challenge/challenge_3/comp.growth.csv")

tree <- rpart(quit ~ salary + size + factor(company_id) + factor(dept) + seniority, data = comp.growth, maxdepth = 3, cp = 0.0001)

1) root 23921 10411 1 (0.4352243 0.5647757)  
  2) growth< 0.01954911 9432  2347 0 (0.7511662 0.2488338) *
  3) growth>=0.01954911 14489  3326 1 (0.2295535 0.7704465) *

# subset.retention <- retention.dt[join_date <= "2011-02-24",   ]
# subset.retention$join_date <- as.Date(subset.retention$join_date)
# retention.headcount <- merge(subset.retention, headcount, by.x = c("join_date", "company_id"), by.y = c("day", "company_id" ), all.x = T)

with(headcount[headcount$company_id == 1, ], plot(x = day, y = employee_headcount))
#number of people who quit in date-x to date - y days. this can be a colomn for dataframe. 
date = as.Date("2015-10-30")


retention.quit <- retention[ !is.na(retention$quit_date), ]

count.quit <- function(id) {
#	cid <- retention.quit[employee_id == id, company_id  ] 
#	qd <- retention.quit[ employee_id == id , quit_date ]
	retention.quit[company_id == company_id[employee_id == id] & quit_date %in% seq(quit_date[employee_id == id] - 5, quit_date[employee_id == id], by = "day"), .N ]
	
}

s <- sapply( retention.quit$employee_id[1:5], count.quit )

# alternatively.
s <- retention.dt[  , .N  , by = .( quit_date)][order( quit_date)]
# then use lapply over date = quit_date



#seq.date <- seq(as.Date("2011-11-20"), as.Date("2011-11-26") , by = "day")

r <- rbindlist(lapply(unique(retention.dt[ !is.na(quit_date) ,  quit_date])[1:20]
, function(date) { retention.dt[ quit_date %in% seq(date - 15, date - 10 , by = "day"), .(.N, date)]}))

t <- merge(r, s, by.x = c("date"),  by.y = c("quit_date"), all.x = T)


data = retention.dt[  join_date < as.Date("2015-12-13") - 365 ]


