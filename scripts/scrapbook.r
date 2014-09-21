# create a synthetic cohort for the following countries:
# i) Norway
# ii) France
# iii) USA



rates_subset <- subset(counts, 
                       subset=country=="NOR" | country=="FRATNP" | country=="USA",
                       select=c("country", "year" ,"age", "sex", "death_rate")
)

# For these plot the data on a bathtub curve along with the ratio between genders

# Look at up to the age of 50
rates_subset <- subset(rates_subset, subset=age<=50)

# for each country, 
# find the last year
ddply(rates_subset, .(country), function(x) max(x$year))
# This shows the newest common year is 2009
rates_subset <- mutate(rates_subset, cohort_year=year - age)

g1 <- ggplot(subset(rates_subset, year==2009), aes(x=age, y=log(death_rate), group=sex))
g2 <- g1 + geom_line()
g3 <- g2 + facet_grid(country ~ sex)
g3



