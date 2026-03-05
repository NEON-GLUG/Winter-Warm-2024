### Author: JMZ
### Last revised: 03-24-26
### Purpose: Code to acquire NEON data from all the core terrestrial and gradient sites, save it to the
### working directory
### Author: Zobitz (zobitz@augsburg.edu)

# install packages
#install.packages("neonSoilFlux")
#install.packages("neonUtilities")
#install.packages("tidyverse")

# Load libraries 
library(neonSoilFlux)
library(tidyverse)
library(neonUtilities)


### Identify directory to save files (set as the current working directory - you need to specify!)
save_dir <- getwd()
delete_month_files <- FALSE   # Option if you want to delete the individual site month flux files

### Helper functions:
## Determine which winter we are in, based on the day of year
winter_classify <- function(start_time) {
  
  # We have moved away from the ENSO rating of the years, as it appears that ENSO is not as reliable a predictor of of winter severity as envisioned. Instead, last year we developed the following list, and most groups are looking at results based on it (this was developed by Jonathan Tong based on domain average winter-early spring temperature and precipitation):
    
  # n (neutral): 1991, 1999, 2002, 2004, 2005, 2011, 2018  
  # cw (cold-wet): 1997, 2008, 2009, 2014, 2019, 2022  
  # d (cold-dry): 1994, 1996, 2003, 2015, 2001, 1993
  # wd (warm-dry): 1992, 1995, 2000, 2010, 2021, 2024
  # ww (warm-wet): 1998, 2006, 2007, 2012, 2013, 2016, 2017, 2020, 2023
  
  # This is for Dec-Mar and the year label reflects the January of that period (e.g., 1991 = Dec 1990-Mar 1991)

  
  year <- lubridate::year(start_time)
  month <- lubridate::month(start_time)

  case_when(
    (year %in% c(1991, 1999, 2002, 2004, 2005, 2011, 2018,2025) & month < 12) ~ "n",  # Jan - Nov of current year
    (year %in% c(1990, 1998, 2001, 2003, 2004, 2010, 2017,2024) & month == 12) ~ "n",  # Dec of previous year
    (year %in% c(1997, 2008, 2009, 2014, 2019, 2022) & month < 12) ~ "cw", # Jan - Nov of current year
    (year %in% c(1996, 2007, 2008, 2013, 2018, 2021) & month == 12) ~ "cw", # Dec of previous year
    (year %in% c(1994, 1996, 2003, 2015, 2001, 1993) & month < 12) ~ "d", # Jan - Nov of current year
    (year %in% c(1993, 1995, 2002, 2014, 2000, 1992) & month == 12) ~ "d", # Dec of previous year
    (year %in% c(1992, 1995, 2000, 2010, 2021, 2024) & month < 12) ~ "wd", # Jan - Nov of current year
    (year %in% c(1991, 1994, 1999, 2009, 2020, 2023) & month == 12) ~ "wd", # Dec of previous year
    (year %in% c(1998, 2006, 2007, 2012, 2013, 2016, 2017, 2020, 2023) & month < 12) ~ "ww", # Jan - Nov of current year
    (year %in% c(1997, 2005, 2006, 2011, 2012, 2015, 2016, 2019, 2022) & month == 12) ~ "ww", # Dec of previous year
  )
  
  
  
}

# Average fluxes across the day
flux_daily_agg <- function(input_fluxes, input_site) {
  
  #Previously, rather than averaging across all the soil plots, I selected the soil plot with the greatest uptime for 
  # use in the GLUG paper (STEI plot 1, TREE plot 5, and UNDE plot 4) 
  site_location <- tibble(site_name = c("STEI","TREE","UNDE"),
                          location = c("001","005","004")
  ) |>
    filter(site_name == input_site) |>
    pull(location)
  
  # Removed values <-1 and >50 micromol CO2 m-2 m-1 (data quality report threshold used by COSORE). Proportion of data removed reported in the topright to get a sense of the impact.
  
  
  input_fluxes |> 
    select(startDateTime,horizontalPosition,flux_compute) |>
    unnest(cols=c("flux_compute") ) |> 
      filter(!is.na(flux) & between(flux,1,50)) |>
      filter(horizontalPosition == site_location) |>
      mutate(date = floor_date(startDateTime,unit="day")) |>
      group_by(date) |> 
      summarize(soil_resp = mean(flux,na.rm = TRUE),
                soil_resp_err = mean(flux_err,na.rm=TRUE)) |>
      ungroup()
  
}



site_name <- c("UNDE","STEI","TREE")
# --> Create the dates vector You can shorten these to a single month if you want.
years <- c("2017","2018","2019","2020","2021","2022","2023","2024","2025")
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")


# Get the combination of all possible months / years / sites
places <- expand_grid(years,months) |>
  expand_grid(site_name)


# Start looping - sit back and wait. :-)
for (i in 1:length(places)) {
  
  
  # Name current month (you will need to adjust this on your computer)
  curr_month <- paste0(places$years[[i]],"-",places$months[[i]])
  curr_site_name <- places$site_name[[i]]
  
  
  # Process
  try(
    # NOTE: you will need to say y/n at several points here
    {
      
      start_time <- Sys.time()
      # env data
      out_env_data <- acquire_neon_data(
        site_name = curr_site_name,
        download_date = curr_month,
        provisional = TRUE
      )
      
      # flux data
      flux_input <- compute_neon_flux(
        input_site_env = out_env_data$site_data,
        input_site_megapit = out_env_data$site_megapit
      )
      
      # Now put this all together
      flux_vals <- flux_daily_agg(flux_input,curr_site_name) |>
        mutate(
          site = curr_site_name,
          classification = map_chr(date,winter_classify)
        ) |> 
        relocate(site)
      
      out_file <- paste0(save_dir,'/fluxes-',curr_site_name,'-',curr_month,'.Rda')
      
      save(flux_vals,file = out_file)
      
    }
    
  )
  
  
  
  
}


### Now start to aggregate up to make a single file

NEON_files <- list.files(save_dir,
                         full.names = TRUE,
                         pattern = 'fluxes')

out_results <- vector(mode="list",length=length(NEON_files))

for (i in 1:length(out_results)) {
  load(NEON_files[[i]])
  out_results[[i]] <- flux_vals
}

out_results_tot <- bind_rows(out_results)

csv_file_name <- paste0(save_dir,'/midwest_soil_flux.csv')

write_csv(out_results_tot,file=csv_file_name)

if(delete_month_files) {file.remove(NEON_files)}
# Remove the files for each month


