rm(list = ls())
dev.off()

library(tidyverse)
library(slider)
library(rsoi)
library(dataRetrieval)
library(lme4)
library(ggeffects)
library(mgcv)
library(patchwork)
library(ggpmisc)

# 1. Import ENSO indices ----
oni <- download_oni() %>%
  select(Date, ONI)
mei <- download_mei() %>%
  select(Date, MEI)

## 1a. Compare ONI and MEI ----
## width = 800, height = 600
mei %>%
  left_join(oni, by = 'Date') %>%
  ggplot(aes(x = MEI, y = ONI)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'longdash', linewidth = 1) +
  geom_point(shape = 21, fill = 'black', size = 2, alpha = 0.5) +
  stat_poly_line(se = FALSE) +
  stat_poly_eq(use_label(c("eq", "adj.R2", "p", "n")), size = 5) +
  labs(x = 'Multivariate ENSO Index Version 2',
       y = 'Oceanic Niño Index (ONI)') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 14, color = 'black'))

rm(mei, oni)
oni <- download_oni()

## 1b. Visualize ONI ----
## width = 800, height = 600
oni %>%
  ggplot(aes(x = Date, y = ONI)) +
  annotate('rect', xmin = min(oni$Date), xmax = max(oni$Date),
           ymin = 0.5, ymax = Inf, fill = 'red', alpha = 0.2) +
  annotate('rect', xmin = min(oni$Date), xmax = max(oni$Date),
           ymin = -Inf, ymax = -0.5, fill = 'blue', alpha = 0.2) +
  annotate('rect', xmin = min(oni$Date), xmax = max(oni$Date),
           ymin = -0.5, ymax = 0.5, fill = 'orange', alpha = 0.2) +
  geom_line() +
  scale_x_date(breaks = seq(as.Date("1950-01-01"),
                            as.Date("2024-12-31"),
                            "5 years"),
               date_labels = "%Y",
               minor_breaks = "1 year",
               expand = c(0,0)) +
  labs(x = NULL, y = 'Oceanic Niño Index (ONI)') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 14, color = 'black'))

# 2. Download daily mean discharge from USGS ----
## 2a. Which USGS sites have daily discharge in bounding area? ----
stat_Cd <- "00003" # statistics parameter code = mean
pcode_Q = '00060' # discharge
start_Date <- as.Date("1950-01-01")
end_Date <- as.Date("2024-06-30")

sites1 <- whatNWISsites(
  bBox = c(-95.0, 40.0, -92.0, 50.0),
  parameterCd = pcode_Q,
  hasDataTypeCd = "dv"
)

sites2 <- whatNWISsites(
  bBox = c(-92.0, 40.0, -89.0, 50.0),
  parameterCd = pcode_Q,
  hasDataTypeCd = "dv"
)

sites3 <- whatNWISsites(
  bBox = c(-89.0, 40.0, -86.0, 50.0),
  parameterCd = pcode_Q,
  hasDataTypeCd = "dv"
)

sites4 <- whatNWISsites(
  bBox = c(-86.0, 40.0, -83.0, 50.0),
  parameterCd = pcode_Q,
  hasDataTypeCd = "dv"
)

sites5 <- whatNWISsites(
  bBox = c(-83.0, 40.0, -80.0, 50.0),
  parameterCd = pcode_Q,
  hasDataTypeCd = "dv"
)

# Bind all sites and only keep streams
sites <- rbind(sites1, sites2, sites3, sites4, sites5) %>%
  filter(site_tp_cd == 'ST') %>%
  distinct() # 2,347 sites

rm(sites1,sites2,sites3,sites4,sites5)

## 2b. When were the discharge records collected? ----
site_No <- unique(sites$site_no)

dailyDataAvailable <- whatNWISdata(
  siteNumber = site_No,
  service = "dv",
  statCd = stat_Cd,
  parameterCd = pcode_Q
) # somehow have 2,357 sites (10 more than what site_No has)

sites_dataAvail <- dailyDataAvailable %>%
  filter(!end_date <= as.Date('1949-12-31'), # remove sites prior to MEI record
         !count_nu < 3650) # must have at least 10 years of data available

# Download daily mean discharge time series ----
# Initial download occurred on August 4, 2024
# site_No = sites_dataAvail$site_no # 1,368
# 
# qdat <- readNWISdv(siteNumbers = site_No,
#                    parameterCd = pcode_Q,
#                    startDate = start_Date,
#                    endDate = end_Date,
#                    statCd = stat_Cd)%>%
#   renameNWISColumns()
# 
# setwd("D:/School/MichiganTech/ENSO/Data")
# write.csv(qdat, 'USGS_Discharge_ENSO_ONI.csv', row.names = FALSE)

setwd("D:/School/MichiganTech/ENSO/Data")
qdat <- read_csv('USGS_Discharge_ENSO.csv', show_col_types = FALSE) %>%
  select(site_no, Date, Flow, Flow_cd)

# Remove poor quality data
table(qdat$Flow_cd)

qdat <- qdat %>%
  filter(!Flow_cd %in% c('A <', 'A >', 'P Bkw', 'P Eqp', 'P Ice'))

# Make the time series for each site complete
complete_dates <- function(data) {
  data %>%
    group_by(site_no) %>%
    complete(Date = seq(min(Date), max(Date), by = "day")) %>%
    ungroup()
}

qdat <- complete_dates(qdat)

# Only keep sites that have at least 10 years of data available
trueDataAvail <- qdat %>%
  group_by(site_no) %>%
  summarize(Total = n(),
            Total_nonNA = sum(!is.na(Flow)),
            Diff = Total - Total_nonNA)

goodSites <- trueDataAvail %>%
  filter(Total_nonNA >= 3650) # 1,291

qdat <- qdat %>%
  filter(site_no %in% goodSites$site_no)

# Extract peak discharge during each three month ONI phase ----
qdat <- qdat %>% arrange(site_no, Date)

peakQ <- qdat %>%
  group_by(site_no) %>%
  reframe(
    start_date = unique(floor_date(Date, "months")),
    interval   = as.interval(months(3) - days(1), start_date),
    Peak_Flow  = map_dbl(interval, \(x) max(keep(Flow, Date %within% x)))) %>%
  ungroup() %>%
  mutate(Peak_Flow = round(Peak_Flow, 1),
         Date = start_date %m+% months(1)) %>%
  select(!c(start_date, interval)) %>%
  relocate(site_no, Date, Peak_Flow)

# Merge ONI with peak discharge ----
peakQ <- left_join(peakQ, oni, by = 'Date') %>%
  mutate(PeakFlow_cms = Peak_Flow * 0.0283168,
         PeakFlow_log = round(log10(PeakFlow_cms + 1), 3))

# Peak Discharge Histograms ----
(p1 <- peakQ %>%
   drop_na() %>%
   ggplot(aes(PeakFlow_cms)) +
   geom_bar() +
   scale_x_binned(breaks = seq(0,16000, 2000),
                  limits = c(0,16000),
                  labels = scales::comma) +
   scale_y_continuous(transform = 'log10',
                      labels = scales::comma) +
   labs(y = 'Count',
        x = expression(Peak~Discharge~(m^-3~s^-1))) +
   theme_bw() +
   theme(axis.text.y = element_text(color = 'black', size = 14),
         axis.text.x = element_text(color = 'black', size = 14),
         axis.title = element_text(color = 'black', size = 14)))

(p2 <- peakQ %>%
    drop_na() %>%
    ggplot(aes(PeakFlow_log)) +
    geom_bar() +
    scale_x_binned(breaks = seq(0,4,0.5), limits = c(0,4)) +
    scale_y_continuous(labels = scales::comma) +
    labs(y = NULL,
         x = expression(log[10](Peak~Discharge~'+ 1'))) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black', size = 14),
          axis.title = element_text(color = 'black', size = 14)))

# width = 1400 height = 600
p1 + p2 + plot_layout(nrow = 1)

# Data Availability Figures ----
# width = 800 height = 600
peakQ %>%
  mutate(month = month(Date)) %>%
  transform(month_abb = factor(month.abb[month], levels = month.abb)) %>%
  group_by(month_abb, Year) %>%
  summarise(Total = n()) %>%
  ggplot(aes(x = month_abb, y = Year, fill = Total)) +
  geom_tile() +
  viridis::scale_fill_viridis(alpha = 0.8) +
  scale_y_continuous(breaks = seq(1950, 2020, 10),
                     minor_breaks = seq(1950, 2025, by = 5),
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       fill = 'Total Number\nof Sites') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, color = 'black'),
        legend.text = element_text(size = 14, color = 'black'),
        legend.title = element_text(size = 14, color = 'black'),
        panel.grid = element_line(color = 'gray60'))

peakQ %>%
  ggplot(aes(x = Date, y = site_no, group = site_no)) +
  geom_line(aes(color = !is.na(PeakFlow_cms)), na.rm = TRUE) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = NA)) +
  labs(x = NULL,
       y = 'Sites (n = 1,291)') +
  scale_x_date(breaks = seq(as.Date("1950-01-01"),
                            as.Date("2020-12-31"),
                            by = "10 years"),
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 14, color = 'black'),
        legend.position = "none")

# Linear mixed-effects model for log-Peak Flow and ONI with site as random effect ----
model1 <- lmer(PeakFlow_log ~ ONI + (1 + ONI | site_no),
               control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
               data = peakQ)

summary(model1)
# For large sample sizes, a t-value greater than approximately 1.96 indicates statistical significance at the 5% level (p < 0.05)
# The fixed effect of MEI on PeakFlow_log is statistically significant, as evidenced by the high t-value (10.81)

new_values <- seq(-2.5, 2.8, by = 0.1)
pred <- ggpredict(model1, terms = list(MEI = new_values))

cols = c("Warm Phase/El Nino" = 'red',
         "Neutral Phase" = 'gray',
         "Cool Phase/La Nina" = 'blue')

# width = 1000, height = 800
peakQ %>%
  drop_na() %>%
  ggplot() + 
  geom_point(aes(x = MEI, y = PeakFlow_log, colour = Phase),
             alpha = 0.1) + 
  geom_line(data = pred,
            aes(x = x, y = predicted)) + # slope
  geom_line(data = pred, 
            aes(x = x, y = predicted + std.error), 
            linetype = 'longdash') + # upper bound
  geom_line(data = pred, 
            aes(x = x, y = predicted - std.error), 
            linetype = 'longdash') + # lower bound
  # geom_ribbon(data = pred,
  #             aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error),
  #             fill = "gray20", alpha = 0.5) + # error band
  scale_color_manual(values = cols) +
  labs(x = 'MEI Phase',
       y = expression(log[10](Peak~Discharge~'+ 1')),
       title = 'Slope = 0.0141 ± 0.0013',
       subtitle = 'Formula = logPeakFlow ~ MEI + (1 + MEI | site_no)') +
  scale_x_continuous(breaks = seq(-2.5,2.5,0.5), limits = c(-2.5,2.9)) +
  scale_y_continuous(breaks = seq(0,4,0.5), limits = c(0,4.2)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 14),
        axis.title = element_text(color = 'black', size = 14),
        legend.title = element_blank(),
        legend.text = element_text(color = 'black', size = 14),
        legend.position = 'bottom') +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

# Generalized additive model (GAM) with random effects ----
sites <- sites %>%
  rename(lat = dec_lat_va,
         lon = dec_long_va) %>%
  select(site_no, station_nm, lat, lon)

peakQ <- left_join(peakQ, sites, by = 'site_no') %>%
  mutate(site_no2 = as.factor(site_no)) %>%
  relocate(site_no, site_no2, station_nm, lat, lon, Date, Year, Month, phase, ONI)

model2 <- bam(PeakFlow_log ~ s(ONI) + s(site_no2, bs = 're'),
              data = peakQ,
              family = gaussian)

summary(model2)
plot(model2)

model3 <- bam(PeakFlow_log ~ s(ONI) + s(lat) + s(site_no2, bs = 're'),
              data = peakQ,
              family = gaussian)

summary(model3)
plot(model3)

# Generalized additive mixed-effects model (GAMM) with random effects ----
model4 <- gamm(PeakFlow_log ~ s(ONI),
               random = list(site_no2 = ~1),
               data = peakQ)
summary(model4$gam)
plot(model4$gam)

model5 <- gamm(PeakFlow_log ~ s(ONI) + s(lat),
               random = list(site_no2 = ~1),
               data = peakQ)
summary(model5$gam)
plot(model5$gam)

# Extract output of model 2 to remake figure in ggplot2
# Extract the smooth term components for s(ONI)
model2_output <- plot.gam(model2, pages = 1, seWithMean = TRUE)

# Create a data frame with the values for ONI, Latitude, and the smooth term estimates
model2_output_data <- data.frame(
  ONI = model2_output[[1]]$x,
  oni_fit = model2_output[[1]]$fit,
  oni_se.fit = model2_output[[1]]$se
)

# Add confidence intervals
model2_output_data <- model2_output_data %>%
  mutate(oni_lower = oni_fit - 2 * oni_se.fit,
         oni_upper = oni_fit + 2 * oni_se.fit)

# width = 800 height = 600
(gam_oni <- ggplot(model2_output_data, aes(x = ONI, y = oni_fit)) +
    annotate('rect', xmin = 0.5, xmax = Inf,
             ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
    annotate('rect', xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
    annotate('rect', xmin = -0.5, xmax = 0.5,
             ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
    annotate('text', x = -2.0, y = -0.39, label = 'Cool Phase/La Nina', size = 5) +
    annotate('text', x = 0, y = -0.39, label = 'Neutral Phase', size = 5) +
    annotate('text', x = 2.5, y = -0.39, label = 'Warm Phase/El Nino', size = 5) +
    geom_line(color = "black") +
    geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
    labs(x = "ONI Phase",
         y = "Estimated Effect of s(ONI, 8.97)",
         title = expression(R[adj.]^2~"="~0.70),
         subtitle = "Formula = logPeakFlow ~ s(oni) + s(site_no, bs = re)") +
    scale_x_continuous(breaks = seq(-2,2.5,0.5), limits = c(-2.04,2.65)) +
    scale_y_continuous(breaks = seq(-0.12,0.12,0.03), limits = c(-0.125,0.133),
                       labels = scales::label_number(accuracy = 0.01)) +
    theme_bw() +
    theme(axis.text = element_text(color = 'black', size = 14),
          axis.title = element_text(color = 'black', size = 14)))

# ECDF ----
# width = 800 height = 600
cols = c("Warm Phase/El Nino" = 'red',
         "Neutral Phase" = 'gray',
         "Cool Phase/La Nina" = 'blue')

peakQ %>%
  drop_na() %>%
  ggplot(aes(x = PeakFlow_log, color = phase)) +
  stat_ecdf(geom = 'step', pad = F, linewidth = 1) +
  scale_color_manual(values = cols) +
  labs(color = 'ONI Phase',
       x = expression(log[10](Peak~Discharge~"+"~1)),
       y = 'Cumulative Distribution') +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,4,0.5), limits = c(0, 4.2)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = c(0.8,0.3),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  guides(color = guide_legend(reverse = TRUE))

sites_smaller <- sites %>%
  filter(site_no %in% peakQ$site_no) %>%
  select(site_no, station_nm, lat, lon)

write.csv(sites_smaller, 'USGS_Coordinates_ENSO_ONI.csv', row.names = FALSE)
