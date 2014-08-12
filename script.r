# 12 /8 / 2014

# Main script for gender mortality differences paper


# Actions:
# 1) Create mortality bathtub curve showing differences in mortality from males to females
# 2) reproduce existing figures included

rm(list=ls())


###########################################################################################################
###########################################################################################################
# Run some of the existing scripts and analyses using the newer data : use this to produce the 
# derived data 

source("Scripts/LoadPackages.R")

RequiredPackages(
  c(
    "plyr",
    "reshape2",
    "lattice",
    "repmis",
    "RCurl",
    "devtools",
    "httr",
    "digest",
    "ggplot2",
    "stringr",
    "car"
  )
)


# Counts is the population count and death count data from the HMD in a single file
# The script for creating this file is available from Jonathan Minton
# please contact nate.minton@gmail.com for further information
counts <- read.csv("data/counts.csv")

# create a synthetic cohort for the following countries:
# i) Norway
# ii) France
# iii) USA

counts <- mutate(counts, death_rate=death_count/population_count)

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


