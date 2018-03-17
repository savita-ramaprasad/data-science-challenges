library(data.table)
ad_table <- fread("~/Google Drive/take_home_challenge/challenge_20/ad_table.csv")
ad_table[ , .(mean(avg_cost_per_click)) , by = ad][order(V1)]
ad_table[ , .(var(avg_cost_per_click)) , by = ad]

ad_table[ , .N , by = ad]

# create profit coloumn

ad_table$profit <- with(ad_table, total_revenue - clicked*avg_cost_per_click)
ad_table[ , .(mean_profit = mean(profit)) , by = ad][order(mean_profit)]
ad_table[ , head(.SD, 2) , by = ad]

ad_table$profit_per_click <- with(ad_table, profit/clicked)

# want to maximise conversion rate/ probability of click and profit per click. 

ad_table$prob_convert <- with(ad_table, converted/shown)
ad_table$metric <- with(ad_table, prob_convert*profit_per_click)

ad_table[ , .(mean_metric = mean(metric, na.rm = T)) , by = ad][order(mean_metric)]
# 5 best ad groups based on this metric would be  27, 31, 14, 16 and 2. 


ads_shown <- ad_table[ , .(ad, date, shown )][order(ad)]
ads_shown <- ads_shown[  , .(shown_for = c(shown[2: length(shown)], 0), shown, date), by = ad]
ads_shown <- ads_shown[ ,  .(ad, date, shown_for, shown, shown_dif = shown_for - shown  )   , by = ad ]
ads_shown <- ads_shown[, .SD[(1: (length(shown_dif)-1))] , by = ad ]

plot(x = 1:length(ads_shown[ad == "ad_group_1", shown_dif]) ,y = ads_shown[ad == "ad_group_1", shown_dif], type  = "b" )


lags <- ads_shown[  , .(lag_2 = diff(shown, lag = 3, difference = 1)) , by = ad]
plot(x = 1:length(lags[ad == "ad_group_1", lag_2]) ,y = lags[ad == "ad_group_1", lag_2], type  = "b" )
lines(x = 1:length(lags[ad == "ad_group_2", lag_2]) ,y = lags[ad == "ad_group_2", lag_2], type  = "b" , col = "red")


# categorizing ad groups.
#ad_subset <-  ad_table[ , .(avg_cost_per_click, ad)][order(ad)]
#av_cost_for <- 
#ad_subset[ , .(cost_forward = c(avg_cost_per_click[2:length(avg_cost_per_click)], 0), avg_cost_per_click), by = ad ]

#cost_diff <- av_cost_for[  , .(cost_diff = cost_forward - avg_cost_per_click), by = ad]
#av_cost_diff <- cost_diff[ , .(diff= cost_diff[1: (length(cost_diff)-1)]), by = ad]

#tot_dec <- av_cost_diff[ , .(total_dec = sum(diff)) , by = ad ]

#$category <- av_cost_diff$total_dec >0 ob
#tot_dec$category[tot_dec$total_dec > 0] = "inc"
#tot_dec$category[(tot_dec$total_dec == 0)] = "flat"
#tot_dec$category[(tot_dec$total_dec < 0)] = "dec"


# categorizing the ad groups.
slope <- function(z) { mz = mean(z, na.rm = T)
		x = 1:length(z[z!=0]) 
		y = z /mz
	inc = round(coef(lm(y[z != 0] ~ x + x*x))[2], 3)  
	if (inc > 0) return("inc")
	if (inc == 0) return("flat")
	if (inc < 0) return("dec")
	}

ad_table[ ,  slope(avg_cost_per_click) , by = ad ]

# visualizing the same by plotting ma and trend line.
ma <- function(x, n = 5) {filter(x, rep(1/n, n), sides = 1 )}
plots <- function(z) { plot(x = 1: length(z), z, typ = "l")
	lines(x = 1: length(z), y =  ma(z), typ = "l", col = "red" ) 
	x = 1:length(z[z!=0])
	lines(predict(lm(z[z != 0] ~ x  )), col = "blue" ) }
	#	lines(predict(lm(z[z != 0] ~ x + I(x^2) )), col = "green" )  }
 	#	lines(predict(loess(z[z != 0] ~ x )), col = "green" )  }

	
pdf(file = "~/Desktop/plots.pdf", onefile = T )
par(mfrow = c(3,5), mar = c(5,1,4,1) )
ad_table[ , plots( avg_cost_per_click) , by = ad]
dev.off()


pdf(file = "~/Desktop/plots.pdf", onefile = T )
par(mfrow = c(3,5), mar = c(5,1,4,1) )
ad_table[ , plots(shown) , by = ad]
dev.off()


