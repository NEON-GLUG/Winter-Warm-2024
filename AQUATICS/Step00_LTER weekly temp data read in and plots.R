### make individual weekly plots of temp for all LTER years using GLM package
# And Hilary Dugan's NTL LakeLoads package
# calculate the thermocline depth

#https://github.com/hdugan/NTLlakeloads

# install packages
# install.packages('remotes')

# remotes::install_github('usgs-r/glmtools')
# library(glmtools)
#
#remotes::install_github('usgs-r/glmtools')
#1remotes::install_github("GLEON/GLM3r")

#install.packages("remotes")   # if you don't have it
#remotes::install_github("hdugan/NTLlakeloads")


library(NTLlakeloads)
library(tidyverse)
library(lme4)
library(MuMIn)
library(rLakeAnalyzer)
library(ggridges)
library(ggbeeswarm)
library(metR)

#------------------------------------------------------------------------------#
#### plot weekly temperature with Hilary Dugan's ntlLakeLoads package ####

# Load NTL datasets
LTERtemp = loadLTERtemp() # Download NTL LTER data from EDI
#LTERnutrients = loadLTERnutrients() # Download NTL LTER data from EDI
#LTERions = loadLTERions() # Download NTL LTER data from EDI
#LTERsecchi = loadLTERsecchi() # Download NTL LTER data from EDI

# get unique lake_year combinations to loop through
lakes = unique(LTERtemp$lakeid)
years = unique(LTERtemp$year4)

lake_years = unique(paste(LTERtemp$lakeid, LTERtemp$year4, sep = "_"))


# make heatmap plots of temperature for all lake and year combinations from LTER
#pdf("./figures/GLM plots of weekly temperature/all lakes temp GLM.pdf", width = 6, height = 4)

max.depths = LTERtemp %>% group_by(lakeid) %>% summarize(max.depth = round(max(depth -1)))

write.csv(max.depths, "./formatted data/max depths LTER lakes.csv", row.names = FALSE)

#pdf("./figures/GLM plots of weekly temperature/all lakes temp GLM.pdf", width = 6, height = 4)
i = 1
j = 1
for(i in 1:length(lakes)){
  
  lake = lakes[i]
  depth = max.depths %>%
    filter(lakeid == lake)  %>% 
    pull(max.depth)
    
    
  df.temp = weeklyTempInterpolate(lakeAbr = lake, maxdepth = depth, dataset = LTERtemp)
  
  
  for(j in 1:length(years)){
    
    year = years[j]
    
    if(paste(lake, year, sep = "_") %in% lake_years){
      
   print(plotTimeseries.year(df.interpolated = df.temp$weeklyInterpolated,   
                          var = 'wtemp', chooseYear = year, binsize = 1, 
                          legend.title = 'Temp (°C)') +
        metR::scale_fill_divergent_discretised(high = 'red4', mid = '#e8e3a7', low = 'lightblue4', midpoint = 15) +
        labs(title = paste(lake, year))+
        geom_vline(xintercept = as.Date(paste(year, "-06-01", sep = "")), linetype = "dashed", color = "black", size = 1))

      
      
    }
  }
  
  
}


#dev.off()

#======================================================================================#
#### interpolated temp dataframe for all lakes ####

for(i in 1:length(lakes)){
  
  lake = lakes[i] # set current lake in the loop
  
  # get the maximum depth by lake
  depth = max.depths %>%
    filter(lakeid == lake)  %>% 
    pull(max.depth)
  
  
  df.temp = weeklyTempInterpolate(lakeAbr = lake, maxdepth = depth, dataset = LTERtemp)
  df.temp = df.temp$weeklyInterpolated
  df.temp$lake = lake
  #temp$year = year(date)
  
  if(i == 1){
    temp.all = df.temp
  }
  
  if(i >1){
    temp.all = rbind(temp.all, df.temp)
  }
  
}



#------------------------------------------------------------------------------#
######## CALCULATE THERMOCLINE DEPTH ########
# remove all NA values from water temp
temp.all = temp.all %>% filter(!is.na(var))

# calculate the thermocline for all using rLakeAnalyzer
# set the minimum density, Smin, to 0.1
temp.all= temp.all %>% group_by(date, lake) %>% 
  mutate(thermo = thermo.depth(wtr = var, depths = depth, Smin = 0.1, seasonal = TRUE, index = FALSE))

# create a year column and lake_year column
temp.all = temp.all %>%
  mutate(year = year(date), lake_year = paste(lake, year, sep = "_"))


# save the temp.all dataframe
write.csv(temp.all, "./formatted data/LTER weekly temperature/weekly temp with thermoclines.csv", row.names = FALSE)


# temp.interpolated = temp.interpolated %>% group_by(date, lake) %>% 
#   mutate(thermo = thermo.depth(wtr = var, depths = depth, seasonal = TRUE, index = FALSE, mixed.cutoff = 0.5))

# plot thermocline depth for each lake-year over time

# create year and lake_year columns to loop over
temp.all = temp.all %>% mutate(year = year(date), lake_year = paste(lake, year, sep = ""))

# get all of the lake_years present in the dataset
lake_years = unique(temp.all$lake_year)

# loop through and plot the thermocline depth of the lakes
pdf("./figures/thermocline/LTER thermocline rLakeAnalyzer.pdf", height = 6, width = 8)
for(i in 1:length(lake_years)){
  
  lake_year.cur = lake_years[i]
  temp.all.cur = temp.all %>% filter(lake_year == lake_year.cur)
  
 print( ggplot(temp.all.cur, aes(x = yday(date), y = thermo, color = as.factor(year(date))))+
    geom_point(color = "steelblue4")+
    geom_line(color = "steelblue4")+
    labs(title = lake_year.cur)+
    theme_classic())
    
}

dev.off()
