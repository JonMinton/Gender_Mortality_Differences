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


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(lattice)
require(latticeExtra)
require(ggplot2)
require(rgl)
require(RColorBrewer)

require(spatstat)
require(fields)



# Counts is the population count and death count data from the HMD in a single file
# The script for creating this file is available from Jonathan Minton
# please contact nate.minton@gmail.com for further information


# Data from HMD -----------------------------------------------------------



counts <- read.csv("data/counts.csv") %>%
  tbl_df
####################################################################################


# Derived data ------------------------------------------------------------



derived <- counts  %>% 
  mutate(death_rate = death_count/population_count)  %>% 
  select(-death_count, -population_count)  %>% 
  filter(sex!="total")  %>% 
  spread(key=sex, value=death_rate)  %>% 
  mutate(
    difference = male - female, 
    ratio = (male+0.000001)/(female+0.000001), # Small continuity correction 
    log_ratio = log(ratio), 
    per_10thousand = difference * 10000
    )


## Blurring differences 

# Want to do some smoothing to make the contour lines less busy
fn <- function(input, smooth_par=2){
  this_country <- input$country[1]
  
  dta <- input %>%
    select(year, age, difference) %>%
    spread(key=age, value=difference) 
  ages <- names(dta)[-1]
  years <- dta$year
  dta$year <- NULL
  dta <- as.matrix(dta)
  rownames(dta) <- years
  colnames(dta) <- ages
  dta_blurred <- as.matrix(blur(as.im(dta), sigma=smooth_par))  
  rownames(dta_blurred) <- rownames(dta)
  colnames(dta_blurred) <- colnames(dta)
  output <- data.frame(
    year=years, 
    country=this_country,
    dta_blurred
  )
  output <- output %>%
    gather(key=age, value=difference, -year, -country)
  
  output$age <- output$age %>%
    str_replace("X", "") %>%
    as.character %>%
    as.numeric
  
  return(output)
}


dif_logs_blurred <- derived %>%
  select(country, year, age, difference) %>%
  filter(age <=80) %>%
  ddply(., .(country), fn, smooth_par=1.0)

dif_logs_blurred <- dif_logs_blurred %>%
  tbl_df

derived <- dif_logs_blurred  %>% 
  rename(dif_smoothed = difference)  %>% 
  left_join(derived)  %>% 
  mutate(per_10thousand_smoothed = dif_smoothed * 10000)  


# New fig 2a at 300dpi ----------------------------------------------------

derived %>%
  filter(country=="USA" & year %in% c(1933, 2010) & age <= 60) %>%
  arrange(year, age) %>%
  ggplot(data=.) +
  geom_line(aes(x=age, y=ratio)) + 
  geom_ribbon(aes(x=age, ymax=ratio, ymin=1, fill="red", alpha=0.4)) +
  facet_wrap( ~ year) + 
  labs(x="Age (years)", y="Mortality rate ratio") + 
  theme(legend.position="none")
ggsave(filename="images/new_2a.tiff", width=12, height=6, units="cm")


# new fig 2b at 300dpi ----------------------------------------------------

derived  %>% 
  filter(country =="USA" & year %in% c(1933, 2010))  %>% 
  arrange(year, age)  %>% 
  ggplot(data=.) +
  geom_line(aes(x=age, y=ratio)) + 
  facet_wrap(~year) + 
  labs(x="Age (Years)", y="Ratio of death rates (male:female)")

ggsave(filename="images/new_2b.tiff", width=16, height=8, units="cm", dpi=300)




# ratio SCPs, spooled ----------------------------------------------------
# Now using smoothed estimates

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
    main = NULL,
    xlab=list(label="Year", cex=1.2),
    ylab=list(label="Age", cex=1.2),
    scales=list(cex=1.2),
    colorkey=list(labels=list(cex=1.2))
  )
  
  tmp <- max(
    abs(c(min(x$per_10thousand_smoothed, na.rm=T), max(x$per_10thousand_smoothed, na.rm=T)))
  )
  tmp <- tmp - (tmp %% 5) + 5
  scale_limit <- max(300, tmp)
  rm(tmp)  
  
  
  p2 <- contourplot(
    per_10thousand_smoothed ~ year * age, 
    
    data = subset(
      x,
      subset= age <= max_age & year >=min_year & year <=max_year
    ), 
    at=seq(
      from=-scale_limit,
      to=scale_limit,
      by=5
    ),
    col="darkgrey",
    labels=list(col="black", cex=1.2),
    labels.stype="align"
  )
  
  p3 <- p1 + p2
  
  this_country <- x$country[1]
  
  png(
    filename=paste0(
      out_dir,
      "excess_",
      this_country, "_(", min_year, "_", max_year, ").png"),
    width=25,
    height=25,
    unit="cm", res=300
  )
  print(p3)
  
  dev.off()  

}

d_ply(
  derived,
  .(country),
  draw_fun,
  max_age=60,
  .progress="text"
)


# Lattice plot with many figures ------------------------------------------

# Multiple plots in a single image ----------------------------------------

# smoothed ratio plots, borrowing from lexis surface difference map --------

# Periods : 1933 to 2010
# Countries: USA, Canada, England & Wales, Netherlands, France,Sweden, Japan
# Switzerland

# Array 
# Canada, England & Wales, Netherlands, France
# Sweden, Japan, Switzerland, USA


countries_to_keep <- c(
  "CAN", "GBRTENW", "NLD", "FRATNP",
  "SWE", "JPN", "CHE", "USA" 
)

derived_2 <- derived  %>% 
  filter(country %in% countries_to_keep)

derived_2$country <- revalue(
  derived_2$country,
  replace=c(
    "CAN"="Canada",
    "GBRTENW"="England & Wales",
    "NLD"="The Netherlands",
    "FRATNP"="France",
    "SWE"="Sweden",
    "JPN" = "Japan",
    "CHE" = "Switzerland",
    "USA"= "United States of America"
  )                        
)

derived_2 <- derived_2 %>%
  filter(year >=1933 & year <= 2010 & age <=60)


derived_2$country <- droplevels(derived_2$country)

derived_2$country <- factor(derived_2$country, 
                            levels=c("Canada", "England & Wales", "The Netherlands", "France",
                                     "Sweden", "Japan", "Switzerland", "United States of America")
                            
                            )
  


tmp <- max(
  abs(c(min(derived_2$log_ratio, na.rm=T), max(derived_2$log_ratio, na.rm=T)))
)
tmp <- tmp - (tmp %% 0.25) + 0.25
scale_limit <- max(4, tmp)
rm(tmp)  

p1 <- levelplot(
  log_ratio ~ year * age | country, 
  data = derived_2, layout = c(4,2),
  at = seq(from= -scale_limit, to = scale_limit, by=0.25),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main = NULL,
  xlab=list(label="Year", cex=1.2),
  ylab=list(label="Age", cex=1.2),
  scales=list(cex=1.2, alternating=1),
  colorkey=list(labels=list(cex=1.2)),
  par.settings=list(strip.background=list(col="lightgrey"))
)

tmp <- max(
  abs(c(min(derived_2$per_10thousand_smoothed, na.rm=T), max(derived_2$per_10thousand_smoothed, na.rm=T)))
)
tmp <- tmp - (tmp %% 5) + 5
scale_limit <- max(300, tmp)
rm(tmp)  


p2 <- contourplot(
  per_10thousand_smoothed ~ year * age | country, 
  data = derived_2,   layout = c(4,2),
  at=seq(
    from=-scale_limit,
    to=scale_limit,
    by=5
  ),
  col="darkgrey",
  labels=list(cex=0.9)
)


p3 <- p1 + p2
png(
  "images/tiled_difference.png",  
  height=20, width=40,
  units="cm", res=300
)
print(p3)
dev.off()




# Borrowing from : 
# http://stackoverflow.com/questions/17022379/image-smoothing-in-r



# clp - composite plot, log -----------------------------------------------

tiff(
  "figures/clp_all.tiff",  
  height=20, width=40,
  units="cm", res=300
)
all_lev <- dif_logs %>%
  filter(sex!="total" & country !="europe" & age <=90) %>%
  levelplot(
    lmort ~ year * age | country + sex,
    data=., 
    region=T,
    ylab="Age in years",
    xlab="Year",
    at = seq(from= -1.2, to = 1.2, by=0.2),
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
    scales=list(alternating=3),
    main=NULL,
    par.settings=list(strip.background=list(col="lightgrey"))
  )

all_cont <- dif_logs_blurred %>%
  filter(sex!="total" & country !="europe" & age <=80) %>%
  contourplot(
    lmort ~ year + age | country + sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    at=0,
    labels=F,
    main=NULL
  )

print(all_lev + all_cont)
dev.off()






# USA, period mortality, 1933 and 2010 ------------------------------------

counts %>% 
  filter(
    country=="USA" & sex !="total" & age <=80 &
      year %in% c(1933, 2010)
    ) %>%
  mutate(Sex = ifelse(sex=="male", "Male", "Female")) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  ggplot(.) +
  geom_line(aes(x=age, y=death_rate, group = Sex, colour=Sex, linetype=Sex)) + 
  facet_wrap(~ year) + 
  theme(legend.position="top") + 
  scale_y_log10() + 
  labs(y="Death rate", x="Age (years)") 

ggsave(filename="images/usa_1933_2010.png", 
       height=10, width=10, units="cm",
       dpi=300
       )


# Ratios, reds only, spooled ----------------------------------------------



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
    main = NULL,
    xlab=list(label="Year", cex=2),
    ylab=list(label="Age", cex=2),
    scales=list(cex=2),
    colorkey=list(labels=list(cex=2))
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
    main = NULL,
    xlab=list(label="Year", cex=2),
    ylab=list(label="Age", cex=2),
    scales=list(cex=2),
    colorkey=list(labels=list(cex=2))
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


# Spooling ----------------------------------------------------------------

#########################

### What I want now: 


# Bathtub curves, period, 2005 --------------------------------------------

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

# Bathtub curves, period, spool by year -----------------------------------

spool_bathtubs <- function(this_year, dta=rates, min_age = 0, max_age = 80){
  png(
    paste0("images/bathtubs/bathtubs_", this_year, ".png"),
    height=20, width=20, units="cm", res=300
  )
  
  p <- derived %>%
    select(year, country, age, female, male) %>%
    gather(key=sex, value=death_rate, -year, -country, -age) %>%
    filter(year==this_year & age <= max_age & age >= min_age) %>%
    ggplot(data=., aes(x=age, y=death_rate)) +
    geom_line(aes(colour=sex, linetype=sex)) + 
    facet_wrap( ~ country, ncol = 5) + 
    labs (x="Age (years)", y="Death rate") +
    scale_y_log10(limits=c(0.001, 0.1)) +
    theme_minimal() +
    ggtitle(this_year)
  
  print(p)
  dev.off()
}

years <- 1900:2010

l_ply(years, spool_bathtubs, .progress="text")




# Ratio curves, period, spool by year -------------------------------------


spool_ratios <- function(this_year, dta=rates_wide, min_age = 0, max_age = 60){
  png(
    paste0("images/ratios/ratios_", this_year, ".png"),
    height=25, width=20, res=300, units="cm"
  )
  
  p <- derived %>%
    filter(year==this_year & age <= max_age & age >=min_age) %>%
    ggplot(data=., aes(x=age, y=ratio)) +
    geom_line() +
    facet_wrap( ~ country, ncol=5) + 
    labs(x="Age (years)", y="Mortality rate ratio") + 
    scale_y_log10(limits=c(0.1, 100)) +
    theme_minimal() + 
    ggtitle(this_year)
  print(p)
  dev.off()
}

years <- 1900:2010

l_ply(years, spool_ratios, .progress="text")

############################################


# Ratio curves, period, 1970 only -----------------------------------------

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
