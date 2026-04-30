#GLUG - El Nino - compare phenology of 2024 to previous years at 11 sites
#April 2025
#Prepared by Jalene LaMontagne

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggbeeswarm)
library(tidyr)
library(broom)


options(scipen=999)

#Get the data into R
dat<-read.csv('./PHENOLOGY/phenocam/phenocam_transition_DOY_stats.csv', header=T)
names <- read.csv("./PHENOLOGY/phenocam/sitenames.csv", header=T)
str(dat)

# lets get our clean site names (lazy loop)
for (i in 1:nrow(names)) {
  dat$NAME[dat$site == names$ID[i]] <- names$NAME[i]
}

#make a year column
dat<-dat %>% mutate(year = as.numeric(str_sub(transition_10,-4,-1)))

#Set the focal year to 2024
focal.year <- "2024"

# and we need to make a new column to call out the focal year data 
# this step is probably unnecessary but may be useful for other plotting
dat$focal.year <- 0
dat$focal.year[dat$year == focal.year] <- 1

no.focal.year <- subset(dat, dat$focal.year != 1)
only.focal.year <- subset(dat, dat$focal.year == 1)

#subset data for graphing to only 2018-2023
no.focal.year_2018_to_2023<-subset(no.focal.year, year>="2018" & year<="2023")

#subset data to 2018-2024 for stats (could have probably merged this with graphing data, but didn't) 
dat_2018_to_2024<-subset(dat, year>="2018" & year<="2024")


# now to plot!
no.focal.year %>%
  arrange(latitude) %>%
  mutate(NAME = reorder(NAME, latitude)) %>%
  ggplot(aes(x = NAME, y = DOY_transition_10)) + 
  geom_beeswarm() +
  xlab("Site") +
  ylab("DOY_transition_10") +
  geom_quasirandom(data = only.focal.year, color = "red", cex = 2)


#Make a graph for each site. 2024 vs ALL OTHER YEARS
#Order by latitude
no.focal.year %>%
  arrange(latitude) %>%
  mutate(NAME = reorder(NAME, latitude)) %>%
  ggplot(aes(x = NAME, y = DOY_transition_10)) + 
  geom_boxplot(width = 0.5, outliers = FALSE) +
  geom_jitter(color="black", size=2, alpha=1,width = 0.05) +
  theme_classic() +
  xlab("Site (all comparison years)") +
  ylab("DOY_transition_10") +
  geom_quasirandom(data = only.focal.year, color = "red", cex = 3, shape = 17)

#Above graph, with adjusted colour ramp colours for years
no.focal.year %>%
  arrange(latitude) %>%
  mutate(NAME = reorder(NAME, latitude)) %>%
  ggplot(aes(x = NAME, y = DOY_transition_10)) + 
  geom_boxplot(width = 0.5, outliers = FALSE) +
  #geom_jitter(color="black", size=1, alpha=1,width = 0.05) +
  geom_jitter(aes(color=year), size=2, alpha=1,width = 0.05) +
 # scale_color_gradient(low = "gray10", 
 #                       high = "red", 
 #                       guide = "colourbar")+
  theme_classic() +
  xlab("Site (all comparison years)") +
  ylab("DOY_transition_10") +
  geom_quasirandom(data = only.focal.year, color = "red", cex = 3, shape = 17)

#Make a graph for each site. 2024 vs 2018-2023 ONLY
#Order by latitude
no.focal.year_2018_to_2023 %>%
  arrange(latitude) %>%
  mutate(NAME = reorder(NAME, latitude)) %>%
  ggplot(aes(x = NAME, y = DOY_transition_10)) + 
  geom_boxplot(width = 0.5, outliers = FALSE) +
  geom_jitter(aes(color=year), size=2, alpha=1,width = 0.05) +
  theme_classic() +
  xlab("Site (2018-2023 comparison years only)") +
  ylab("DOY_transition_10") +
  geom_quasirandom(data = only.focal.year, color = "red", cex = 3, shape = 17)

#Above graph, with adjusted colour ramp colours for years
no.focal.year_2018_to_2023 %>%
  arrange(latitude) %>%
  mutate(NAME = reorder(NAME, latitude)) %>%
  ggplot(aes(x = NAME, y = DOY_transition_10)) + 
  geom_boxplot(width = 0.5, outliers = FALSE) +
  #geom_jitter(color="black", size=1, alpha=1,width = 0.05) +
  geom_jitter(aes(color=year), size=2, alpha=1,width = 0.05) +
  # scale_color_gradient(low = "gray10", 
  #                       high = "red", 
  #                       guide = "colourbar")+
  theme_classic() +
  xlab("Site (2018-2023 comparison years only)") +
  ylab("DOY_transition_10") +
  geom_quasirandom(data = only.focal.year, color = "red", cex = 3, shape = 17)


#Stats time
#Single sample t-tests as a loop, comparing ALL PREVIOUS Years to the focal year
dat %>%
  group_by(NAME) %>%                       
  summarise(res = list(tidy(t.test(DOY_transition_10[focal.year==0], mu=DOY_transition_10[focal.year==1])))) %>%
  unnest()

#Single sample t-tests as a loop, comparing 2018-2023 to the focal year
dat_2018_to_2024 %>%
  group_by(NAME) %>%                       
  summarise(res = list(tidy(t.test(DOY_transition_10[focal.year==0], mu=DOY_transition_10[focal.year==1])))) %>%
  unnest()

