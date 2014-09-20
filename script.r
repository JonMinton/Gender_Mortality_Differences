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
    "latticeExtra",
    "repmis",
    "RCurl",
    "devtools",
    "httr",
    "digest",
    "ggplot2",
    "stringr",
    "car",
    "RColorBrewer"
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


#########################

### What I want now: 

# tile wrap
# male and females as different lines on same graphs
# ratios as separate tile wrap

png("images/male_female_mort_tile_2005.png", height=1000, width=800)
g1 <- ggplot(
  subset(counts, year==2005 & sex!="total" & age <=80),
  aes(x=age, y=death_rate)
  )

g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap(~ country, ncol=5)
g4 <- g3 + labs(y="Death rate", x="Age (years)") 
g5 <- g4 + scale_y_log10()
g6 <- g5 + theme_minimal()
print(g6)

dev.off()

# To do : 
# 1) make country labels neater
# 2) remove countries with few observations
# 3) fix Germany (East, West etc)?
# 4) consider doing for other years

# 
#### males per female




################################################################################################

# rates: 

rates <- counts
rates <- mutate(rates, death_rate = death_count/population_count)

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


##################
png("images/ratios_2005.png", height=1000, width=800)

g1 <- ggplot(
  data=subset(
    rates_wide,
    subset=age <=60 & year==2005
  ),
  aes(x=age, y=ratio)
  )

g2 <- g1 + geom_line()
g3 <- g2 + facet_wrap(~ country, ncol=5)
g4 <- g3 + labs(y="Mortality rate ratio", x="Age (years)") 
g5 <- g4 + scale_y_log10()
g6 <- g5 + theme_minimal()
print(g6)
dev.off()
############################################


g1 <- ggplot(
  data=subset(
    rates_wide,
    subset=age <=60 & year==1970
  ),
  aes(x=age, y=ratio)
)

g2 <- g1 + geom_line()
g3 <- g2 + facet_wrap(~ country, ncol=5)
g4 <- g3 + labs(y="Mortality rate ratio", x="Age (years)") 
g5 <- g4 + scale_y_log10()
g6 <- g5 + theme_minimal()
print(g6)
####################################################################################

# TO DO
# 1) Function for d_ply for automation production for different 
# countries
# 2) change labels of legends to reflect rates per 1000
# 3) automate range considered for country

draw_fun <- function(x, max_age=50, 
                    min_year=1950, 
                    max_year=2000,
                     out_dir="images/excess/"
                       ){
  
  min_year <- max(
    min(x$year),
    min_year
    )
  
  max_year <- min(
    max(x$year),
    max_year
    )
  tmp <- max(
    abs(c(min(x$log_ratio, na.rm=T), max(x$log_ratio, na.rm=T)))
    )
  tmp <- tmp - (tmp %% 0.25) + 0.25
  scale_limit <- max(4, tmp)
  rm(tmp)  
  
  p1 <- levelplot(
    log_ratio ~ year * age , 
    data = subset(
      x,
      subset= age <= max_age & year >=min_year & year <=max_year
    ),
    at = seq(from= -scale_limit, to = scale_limit, by=0.25),
    col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
    main = NULL
  )
  
  tmp <- max(
    abs(c(min(x$per_10thousand, na.rm=T), max(x$per_10thousand, na.rm=T)))
  )
  tmp <- tmp - (tmp %% 5) + 5
  scale_limit <- max(300, tmp)
  rm(tmp)  
  
  
  p2 <- contourplot(
    per_10thousand ~ year * age, 
    data = subset(
      x,
      subset= age <= max_age & year >=min_year & year <=max_year
    ), 
    at=seq(
      from=-scale_limit,
      to=scale_limit,
      by=5
      )
    )
  
  p3 <- p1 + p2
  
  this_country <- x$country[1]
  
  png(
    filename=paste0(
      out_dir,
      "excess_",
      this_country,
      ".png"),
    width=1000,
    height=1000
      )
  print(p3)
  
  dev.off()  
  while(names(dev.cur())[1] !="RStudioGD"){
    dev.off()
  }
}

d_ply(
  rates_wide,
  .(country),
  draw_fun,
  max_age=60,
  .progress="text"
  )


