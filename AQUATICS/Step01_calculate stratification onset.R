library(tidyverse)

# read in the temp data with thermoclines
# and the max depth data (lowest temp depth excluded)

temp.all = read.csv("./formatted data/LTER weekly temperature/weekly temp with thermoclines.csv")
max.depths=read.csv("./formatted data/max depths LTER lakes.csv")

# filter out years with missing data at the beginning of the year
temp.all = temp.all %>% filter(!(lake_year %in% c("AL_1981", "CB_1981", "MO_1995", 
                                                  "SP_1981", "TB_1981")))

temp.all = temp.all %>% mutate(year = year(date), lake_year = paste(lake, year, sep = ""))

#==============================================================================#
##### first date where the thermocline is not calculated as 0.5 m #####

temp.all = temp.all %>% mutate(doy = yday(date))

# filter out late summer so we don't get end of season stratification end dates
temp.all.summer = temp.all %>% filter(doy < 230)

# print thermocline plots with dates restricted to spring and summer
# pdf("./figures/thermocline/LTER thermocline rLakeAnalyzer spring and summer.pdf", height = 6, width = 8)
# for(i in 1:length(lake_years)){
#   
#   lake_year.cur = lake_years[i]
#   temp.all.cur = temp.all.summer %>% filter(lake_year == lake_year.cur)
#   
#   print( ggplot(temp.all.cur, aes(x = yday(date), y = thermo, color = as.factor(year(date))))+
#            geom_point(color = "steelblue4")+
#            geom_line(color = "steelblue4")+
#            labs(title = lake_year.cur)+
#            theme_classic())
#   
# }
# 
# dev.off()

# remove Trout Bog and Crystal Bog because their stratification is intermittent
temp.all.summer = temp.all.summer %>% filter(!(lake %in% c("TB", "CB")))

# replace NA values with 0, indicating the water column is fully mixed
temp.all.summer = temp.all.summer %>% mutate(thermo = replace(thermo, is.na(thermo), 0))

# get all remaining lake year combinations
lake_years = unique(temp.all.summer$lake_year)

# create dataframe to store stratification dates in
strat.dates = data.frame(matrix(nrow = length(lake_years), ncol = 3))
names(strat.dates) = c("lake", "year", "strat.date")

# look through all of the temp data and get first date
# where thermocline is greater than >0.5 and is sustained for the rest of the summer

for(i in 1:length(lake_years)){
  
  # day of year 81 is march 21st
  # the start of spring
  lake_year.cur = lake_years[i]
  temp.all.cur = temp.all.summer %>% filter(lake_year == lake_year.cur & doy >= 81)
  
  temp.all.cur = temp.all.cur %>% filter(!is.nan(thermo))
  
  strat.dates$year[i] = unique(temp.all.cur$year)
  strat.dates$lake[i] = unique(temp.all.cur$lake)
  
  
  location.of.0.5 = which(temp.all.cur$thermo == 0.5 | temp.all.cur$thermo == 0)
  
  # if there are no cases of 0.5
  if(length(location.of.0.5) == 0){
    strat.dates$strat.date[i] = temp.all.cur$doy[1]
  }
  if(length(location.of.0.5) > 0){
    strat.dates$strat.date[i] = temp.all.cur$doy[max(location.of.0.5) +1]
  }
  
}


# convert from doy to month and day without year
strat.dates <- strat.dates %>%
  mutate(date = as.Date(strat.date - 1, origin = paste0(1900, "-01-01")))

# years where there was mixing and the onset of stratification was identified prior to mixing
# in these years, thermocline is calculated as NA
mixed.years = c("AL_1986", "AL_1988", "AL_1989", "AL_1991", "AL_1993", "AL_2002", "AL_2004", "AL_2009",
                "AL_2011", "AL_2014", "TR_1994", "TR_2022", "FI_2014", "MO_2010", "MO_2008", "CR_2012", "CR_2013")

# at least remove years of Crystal mixing experiment
#mixed.years = c("CR_2012", "CR_2013")

strat.dates.filtered = strat.dates %>% mutate(lake_year = paste(lake, year, sep = "_")) %>% 
  filter(!(lake_year %in% mixed.years))

png("./figures/strat onset/onset of strat over time.png", res = 300, height = 6, width = 9, units = "in")

# plot the stratification dates
ggplot(strat.dates.filtered %>% filter(lake != "WI"), aes_string(x = "year", y = "date"))+
  geom_point()+
  geom_line(size = 1)+
  facet_wrap(~lake)+
  labs(title = "onset of stratification in LTER lakes")+
  theme_bw()+
  theme( strip.background = element_rect("steelblue2"))

dev.off()

# save the stratification dates
write.csv(strat.dates, "./formatted data/stratification onset dates.csv", row.names = FALSE)








