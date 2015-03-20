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
    "animation",
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

source("scripts/manage_data.r")

#########################

### What I want now: 

# tile wrap
# male and females as different lines on same graphs
# ratios as separate tile wrap

png("images/male_female_mort_tile_2005.png", height=1000, width=800)
this_year <- 2005

g1 <- ggplot(
  subset(rates, year==this_year & sex!="total" & age <=80),
  aes(x=age, y=death_rate)
  )

g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap(~ country, ncol=5)
g4 <- g3 + labs(y="Death rate", x="Age (years)") 
g5 <- g4 + scale_y_log10()
g6 <- g5 + theme_minimal() + ggtitle(this_year)
print(g6)

dev.off()

# Spool the above for all years

spool_bathtubs <- function(this_year, dta=rates, min_age = 0, max_age = 80){
  png(
    paste0("images/bathtubs/bathtubs_", this_year, ".png"),
    height=1000, width=800
  )
  
  g1 <- ggplot(
    subset(
      dta, 
      subset= (year==this_year & sex !="total" & age <= max_age & age >= min_age)
      ),
    aes(x=age, y=death_rate)
  )
  g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
  g3 <- g2 + facet_wrap(~ country, ncol=5)
  g4 <- g3 + labs(y="Death rate", x="Age (years)") 
  g5 <- g4 + scale_y_log10(limits=c(0.0001, 0.1))
  g6 <- g5 + theme_minimal() + ggtitle(this_year)
  print(g6)
  dev.off()
}

years <- 1900:2010

l_ply(years, spool_bathtubs)

# To do : 
# 1) make country labels neater
# 2) remove countries with few observations
# 3) fix Germany (East, West etc)?
# 4) consider doing for other years

# 
#### males per female



##################

spool_ratios <- function(this_year, dta=rates_wide, min_age = 0, max_age = 60){
  png(
    paste0("images/ratios/ratios_", this_year, ".png"),
    height=1000, width=800
  )
  
  g1 <- ggplot(
    subset(
      dta, 
      subset= (year==this_year & age <= max_age & age >= min_age)
    ),
    aes(x=age, y=ratio)
  )
  g2 <- g1 + geom_line()
  g3 <- g2 + facet_wrap(~ country, ncol=5)
  g4 <- g3 + labs(y="Mortality rate ratio", x="Age (years)") 
  g5 <- g4 + scale_y_log10(limits=c(0.1, 100))
  g6 <- g5 + theme_minimal() + ggtitle(this_year)
  print(g6)
  dev.off()
}

years <- 1900:2010

l_ply(years, spool_ratios)

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
                    min_year=1933, 
                    max_year=2010,
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
#   while(names(dev.cur())[1] !=("RStudioGD" | "null device")){
#     dev.off()
#   }
}

d_ply(
  rates_wide,
  .(country),
  draw_fun,
  max_age=60,
  .progress="text"
  )



#########################################################################################
# age-mortality, males and females, and ratios, for USA, in years 1933 and 2010

rates_usa <- subset(
  rates, 
  subset=country=="USA" & (year == 1933 | year == 2010) & sex !="total" 
  )



png("images/usa_1933_2010.png", height=500, width=800)

g1 <- ggplot(
  subset(rates_usa, subet = age <= 80),
  aes(x=age, y=death_rate)
)

g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap(~ year)
g4 <- g3 + labs(y="Death rate", x="Age (years)") 
g5 <- g4 + scale_y_log10()
g6 <- g5 + theme_minimal() 
print(g6)

dev.off()


### Excess plots as levelplots only - reds only - dif scale


# TO DO
# 1) Function for d_ply for automation production for different 
# countries
# 2) change labels of legends to reflect rates per 1000
# 3) automate range considered for country

draw_fun <- function(x, max_age=50, 
                     min_year=1950, 
                     max_year=2000,
                     out_dir="images/excess_level_reds/"
){
  
  min_year <- max(
    min(x$year),
    min_year
  )
  
  max_year <- min(
    max(x$year),
    max_year
  )
  x$log_ratio[x$log_ratio < 0] <- 0
  
  tmp <- max(
    abs(c(min(x$log_ratio, na.rm=T), max(x$log_ratio, na.rm=T)))
  )
  tmp <- tmp - (tmp %% 0.125) + 0.125
  scale_limit <- max(2, tmp)
  rm(tmp)  
  
  p1 <- levelplot(
    log_ratio ~ year * age , 
    data = subset(
      x,
      subset= age <= max_age & year >=min_year & year <=max_year
    ),
    at = seq(from= 0, to = scale_limit, by=0.125),
    col.regions = colorRampPalette(brewer.pal(5, "Reds"))(64),
    main = NULL
  )
   
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
  print(p1)
  
  dev.off()  

}

d_ply(
  rates_wide,
  .(country),
  draw_fun,
  max_age=60,
  .progress="text"
)


#### Levelplot only 

draw_fun <- function(x, max_age=50, 
                     min_year=1950, 
                     max_year=2000,
                     out_dir="images/excess_level/"
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
  print(p1)
  
  dev.off()  
  
}

d_ply(
  rates_wide,
  .(country),
  draw_fun,
  max_age=60,
  .progress="text"
)

