rm(list = ls())
graphics.off()
gc()

library(tidyverse)
library(rsoi)
library(dataRetrieval)
library(EflowStats)
library(mgcv)
library(patchwork)

# 1. Import ENSO indices ----
# oni <- download_oni() %>%
#   filter(ONI_month_window == "DJF") %>% 
#   mutate(preced_year = Year - 1,
#          years = paste(preced_year, Year, sep = " - ")) %>% 
#   rename(water_year = Year) %>% 
#   select(years, water_year, ONI_month_window, ONI, phase)
# 
# # 2. Identify USGS sites in region with stream flow data ----
# ## Downloaded on May 5, 2025
# sites1 <- whatWQPsites(
#   bBox = c(-95.0, 40.0, -92.0, 50.0),
#   providers = "NWIS",
#   characteristicName = "Stream flow",
#   legacy = TRUE
# )
# 
# sites2 <- whatWQPsites(
#   bBox = c(-92.0, 40.0, -89.0, 50.0),
#   providers = "NWIS",
#   characteristicName = "Stream flow",
#   legacy = TRUE
# )
# 
# sites3 <- whatWQPsites(
#   bBox = c(-89.0, 40.0, -86.0, 50.0),
#   providers = "NWIS",
#   characteristicName = "Stream flow",
#   legacy = TRUE
# )
# 
# sites4 <- whatWQPsites(
#   bBox = c(-86.0, 40.0, -83.0, 50.0),
#   providers = "NWIS",
#   characteristicName = "Stream flow",
#   legacy = TRUE
# )
# 
# sites5 <- whatWQPsites(
#   bBox = c(-83.0, 40.0, -80.0, 50.0),
#   providers = "NWIS",
#   characteristicName = "Stream flow",
#   legacy = TRUE
# )
# 
# # Bind all sites and only keep streams
# sites <- rbind(sites1, sites2, sites3, sites4, sites5) %>%
#   filter(MonitoringLocationTypeName == 'Stream') %>%
#   distinct() # 1,895 sites
# 
# rm(sites1,sites2,sites3,sites4,sites5)
# 
# # 3. Extract peak flow for each site ----
# ## 3a. isolate site numbers ----
# sites <- sites %>% 
#   separate(.,
#            col = MonitoringLocationIdentifier,
#            into = c('agency', 'site_no'),
#            sep = "-")
# 
# site_No <- unique(sites$site_no)
# 
# ## 3b. download peak flow for each site in 100 site chunks as not to overwhelm server ----
# ## Downloaded on May 5, 2025
# chunk_size  <-  100
# site_chunks <- split(site_No, ceiling(seq_along(site_No) / chunk_size))
# 
# qdat_list <- map(site_chunks, ~ 
#                    readNWISpeak(
#                      siteNumbers = .x,
#                      startDate   = "1949-10-01",
#                      endDate     = "2024-09-30"
#                    ) 
# )
# 
# ## 3c. bind all chunks together ----
# qdat <- bind_rows(qdat_list) %>% 
#   renameNWISColumns() %>% 
#   mutate(water_year = get_waterYear(peak_dt)) %>% 
#   select(site_no, water_year, peak_dt, peak_va, peak_cd) %>% 
#   filter(!is.na(peak_dt))
# 
# ## 3d. check if each water year has only one value ----
# multiples_check <- qdat %>% 
#   group_by(site_no, water_year) %>% 
#   summarise(Total = n())
# 
# max(multiples_check$Total)
# 
# # 4. Merge ONI with peak discharge ----
# peakQ <- left_join(qdat, oni, by = 'water_year') %>%
#   mutate(PeakFlow_cms = peak_va * 0.0283168,
#          PeakFlow_log = round(log(PeakFlow_cms), 3))

## 4a. export peak flow time series ----
# write.csv(peakQ, 'D:/School/MichiganTech/ENSO/Data/USGS_PeakFlow_ENSO_ONI.csv', row.names = FALSE)
peakQ <- read_csv('D:/School/MichiganTech/ENSO/Data/USGS_PeakFlow_ENSO_ONI.csv')

# 5. Generalized additive model (GAM) with random effects ----
peakQ <- peakQ %>% 
  mutate(site_no_factor = factor(site_no),
         dowy = get_waterYearDay(peak_dt))

model_flow <- mgcv::bam(PeakFlow_log ~ s(ONI) + s(site_no_factor, bs = 're'),
                        data = peakQ,
                        family = gaussian)

gam.check(model_flow)
summary(model_flow) # adjR2 = 0.876
plot(model_flow, pages = 1)

model_doy <- mgcv::bam(dowy ~ s(ONI) + s(site_no_factor, bs = 're'),
                       data = peakQ,
                       family = gaussian)

gam.check(model_doy)
summary(model_doy) # adjR2  0.0923
plot(model_doy, pages = 1)

## 5a. extract output of model to remake figure in ggplot2 ----
### Extract the smooth term components for s(ONI)
model_flow_output <- plot.gam(model_flow, pages = 1, seWithMean = TRUE)
model_doy_output <- plot.gam(model_doy, pages = 1, seWithMean = TRUE)

### Create a data frame with the values for ONI, Latitude, and the smooth term estimates
model_flow_output_data <- data.frame(
  ONI = model_flow_output[[1]]$x,
  oni_fit = model_flow_output[[1]]$fit,
  oni_se.fit = model_flow_output[[1]]$se
)

model_doy_output_data <- data.frame(
  ONI = model_doy_output[[1]]$x,
  oni_fit = model_doy_output[[1]]$fit,
  oni_se.fit = model_doy_output[[1]]$se
)

### Add confidence intervals
model_flow_output_data <- model_flow_output_data %>%
  mutate(oni_lower = oni_fit - 1.96 * oni_se.fit,
         oni_upper = oni_fit + 1.96 * oni_se.fit)

model_doy_output_data <- model_doy_output_data %>%
  mutate(oni_lower = oni_fit - 1.96 * oni_se.fit,
         oni_upper = oni_fit + 1.96 * oni_se.fit)

# 6. Figure: ONI impact on Peak Flow ----
# width = 800 height = 600
p1 <- ggplot(model_flow_output_data, aes(x = ONI, y = oni_fit)) +
    annotate('rect', xmin = 0.5, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
    annotate('rect', xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
    annotate('rect', xmin = -0.5, xmax = 0.5,
             ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
    # annotate('text', x = -1.25, y = -0.25, label = 'Cool Phase/La Niña', size = 5) +
    # annotate('text', x = 0, y = -0.25, label = 'Neutral Phase', size = 5) +
    # annotate('text', x = 1.75, y = -0.25, label = 'Warm Phase/El Niño', size = 5) +
    geom_line(color = "black") +
    geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
    labs(x = NULL,
         y = "Estimated Effect of s(ONI)",
         title = expression(R[adj.]^2~"="~0.88),
         subtitle = "Formula = log(PeakFlow) ~ s(oni) + s(site_no, bs = re)") +
    scale_x_continuous(breaks = seq(-2.0,2.5,0.5),
                       limits = c(-2.0,2.5),
                       expand = c(0,0)) +
    scale_y_continuous(breaks = seq(-0.25,0.25,0.05),
                       limits = c(-0.28,0.26),
                       expand = c(0,0),
                       labels = scales::label_number(accuracy = 0.01)) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black', size = 14),
          axis.text.x = element_blank(),
          axis.title = element_text(color = 'black', size = 14),
          plot.margin = margin(0,1,0.5,0, "lines"))

p2 <- ggplot(model_doy_output_data, aes(x = ONI, y = oni_fit)) +
  annotate('rect', xmin = 0.5, xmax = Inf,
           ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
  annotate('rect', xmin = -0.5, xmax = 0.5,
           ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
  # annotate('text', x = -1.25, y = -28, label = 'Cool Phase/La Niña', size = 5) +
  # annotate('text', x = 0, y = -28, label = 'Neutral Phase', size = 5) +
  # annotate('text', x = 1.75, y = -28, label = 'Warm Phase/El Niño', size = 5) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
  labs(x = NULL,
       y = NULL,
       title = expression(R[adj.]^2~"="~0.09),
       subtitle = "Formula = dowy ~ s(oni) + s(site_no, bs = re)") +
  scale_x_continuous(breaks = seq(-2.0,2.5,0.5),
                     limits = c(-2.0,2.5),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-30,20,10),
                     limits = c(-30,20),
                     expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_blank(),
        axis.title = element_text(color = 'black', size = 14),
        plot.margin = margin(0,1,0.5,0, "lines"))

# Lake Stratification Date ----
## data provided by Danny Szydlowski

### read in the ONI data ####
oni <- download_oni() %>%
  filter(ONI_month_window == "DJF") %>% 
  mutate(preced_year = Year - 1,
         years = paste(preced_year, Year, sep = " - ")) %>% 
  rename(water_year = Year) %>% 
  select(years, water_year, ONI_month_window, ONI, phase)

### read in the stratification onset data by year
strat = read.csv("D:/School/MichiganTech/ENSO/Data/stratification_onset_dates_v2_Oct2026.csv") %>% 
  rename(water_year = year) %>% 
  left_join(oni, by = "water_year")

strat_north <- strat %>% 
  mutate(lake = factor(lake)) %>% 
  filter(lake %in% c("AL", "BM", "CR", "SP", "TR"))

strat_south <- strat %>% 
  mutate(lake = factor(lake)) %>% 
  filter(lake %in% c("ME", "MO", "FI"))

model_north <- mgcv::bam(strat.date ~ s(ONI) + s(lake, bs = 're'),
                         data = strat_north,
                         family = gaussian)

model_south <- mgcv::bam(strat.date ~ s(ONI) + s(lake, bs = 're'),
                         data = strat_south,
                         family = gaussian)

gam.check(model_north)
gam.check(model_south)
summary(model_north) # adjR2 = 0.124
summary(model_south) # adjR2 = 0.329
plot(model_north, pages = 1)
plot(model_south, pages = 1)

## 5a. extract output of model to remake figure in ggplot2 ----
### Extract the smooth term components for s(ONI)
model_north_output <- plot.gam(model_north, pages = 1, seWithMean = TRUE)
model_south_output <- plot.gam(model_south, pages = 1, seWithMean = TRUE)

### Create a data frame with the values for ONI, Latitude, and the smooth term estimates
model_north_output_data <- data.frame(
  ONI = model_north_output[[1]]$x,
  oni_fit = model_north_output[[1]]$fit,
  oni_se.fit = model_north_output[[1]]$se
)

model_south_output_data <- data.frame(
  ONI = model_south_output[[1]]$x,
  oni_fit = model_south_output[[1]]$fit,
  oni_se.fit = model_south_output[[1]]$se
)

### Add confidence intervals
model_north_output_data <- model_north_output_data %>%
  mutate(oni_lower = oni_fit - 1.96 * oni_se.fit,
         oni_upper = oni_fit + 1.96 * oni_se.fit)

model_south_output_data <- model_south_output_data %>%
  mutate(oni_lower = oni_fit - 1.96 * oni_se.fit,
         oni_upper = oni_fit + 1.96 * oni_se.fit)

# 6. Figure: ONI impact on Peak Flow ----
# width = 800 height = 600
(p3 <- ggplot(model_north_output_data, aes(x = ONI, y = oni_fit)) +
  annotate('rect', xmin = 0.5, xmax = Inf,
           ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
  annotate('rect', xmin = -0.5, xmax = 0.5,
           ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
  # annotate('text', x = -1.25, y = -24, label = 'Cool Phase/La Niña', size = 5) +
  # annotate('text', x = 0, y = -24, label = 'Neutral Phase', size = 5) +
  # annotate('text', x = 1.75, y = -24, label = 'Warm Phase/El Niño', size = 5) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
  labs(x = "ONI Phase",
       y = "Estimated Effect of s(ONI)",
       title = bquote("Northern Lakes," ~ R[adj.]^2 ~ "=" ~ 0.13),
       subtitle = "Formula = StratDate ~ s(oni) + s(lake, bs = re)") +
  scale_x_continuous(breaks = seq(-2.0,2.5,0.5),
                     limits = c(-2.0,2.5),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-20,20,10),
                     limits = c(-25,20),
                     expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 14),
        axis.title = element_text(color = 'black', size = 14),
        plot.margin = margin(0.5,1,0,0, "lines")))

(p4 <- ggplot(model_south_output_data, aes(x = ONI, y = oni_fit)) +
  annotate('rect', xmin = 0.5, xmax = Inf,
           ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
  annotate('rect', xmin = -0.5, xmax = 0.5,
           ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
  # annotate('text', x = -1.25, y = -24, label = 'Cool Phase/La Niña', size = 5) +
  # annotate('text', x = 0, y = -24, label = 'Neutral Phase', size = 5) +
  # annotate('text', x = 1.75, y = -24, label = 'Warm Phase/El Niño', size = 5) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
  labs(x = "ONI Phase",
       y = NULL,
       title = bquote("Southern Lakes," ~ R[adj.]^2 ~ "=" ~ 0.33),
       subtitle = "Formula = StratDate ~ s(oni) + s(lake, bs = re)") +
  scale_x_continuous(breaks = seq(-2.0,2.5,0.5),
                     limits = c(-2.0,2.5),
                     expand = c(0,0)) +
    scale_y_continuous(breaks = seq(-20,20,10),
                       limits = c(-25,20),
                       expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 14),
        axis.title = element_text(color = 'black', size = 14),
        plot.margin = margin(0.5,1,0,0, "lines")))

# width = 900 height = 800
p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
