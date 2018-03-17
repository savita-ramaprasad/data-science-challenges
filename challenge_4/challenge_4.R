fraud <- fread( "~/Google Drive/take_home_challenge/challenge_4/Fraud/Fraud_Data.csv")
library("bit64")
ip_country <- fread( "~/Google Drive/take_home_challenge/challenge_4/Fraud/IpAddress_to_Country.csv")


country.ip <- function(ip){
	 
	 x <- ip_country[ lower_bound_ip_address <= ip & upper_bound_ip_address >= ip, country ]
	 if (length(x) == 1) return(x)
	 else return(NA_character_)
	 
	}

sub.fraud <- fraud[1:200]
start = Sys.time()
fraud$country <- sapply(fraud$ip_address, country.ip )
print(Sys.time() - start)

write.csv( fraud, "~/Google Drive/take_home_challenge/challenge_4/Fraud/fraud.csv")

#alternative
start <- Sys.time()
sub.fraud$country <- sub.fraud[ , .(country = sapply(ip_address, country.ip))]
print(Sys.time() - start)

