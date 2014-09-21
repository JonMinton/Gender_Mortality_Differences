counts <- read.csv("data/counts.csv")
rates <- mutate(counts, death_rate=death_count/population_count)
rates$death_count <- NULL
rates$population_count <- NULL
rates <- subset(rates, sex!="total" & age <=80)
rates_wide <- recast(rates, formula=country + year + age ~ sex, id.var=.(country, year, age, sex))
rates_wide <- mutate(rates_wide, 
                     difference = male-female,
                     ratio=male/female,
                     
                     log_ratio = log(ratio),
                     per_10thousand = difference * 10000
)

rates_wide$ratio[is.nan(rates_wide$ratio)] <- NA
rates_wide$log_ratio[is.nan(rates_wide$log_ratio)] <- NA
rates_wide$ratio[is.infinite(rates_wide$ratio)] <- NA
rates_wide$log_ratio[is.infinite(rates_wide$log_ratio)] <- NA

