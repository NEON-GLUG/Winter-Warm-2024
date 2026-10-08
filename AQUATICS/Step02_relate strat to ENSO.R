##### fit a GAM to the stratification onset data to match Spencer's work on flow #######


library(tidyverse)
library(mgcv)
library(rsoi)


### read in the ONI data ####

oni <- download_oni() %>%
  filter(ONI_month_window == "DJF") %>% 
  mutate(preced_year = Year - 1,
         years = paste(preced_year, Year, sep = " - ")) %>% 
  rename(water_year = Year) %>% 
  select(years, water_year, ONI_month_window, ONI, phase)


### read in the stratification onset data by year
strat = read.csv("./formatted data/stratification onset dates.csv") %>% 
  rename(water_year = year) %>% 
  left_join(oni, by = "water_year")

ggplot(strat, aes(x = as.numeric(water_year), y = strat.date))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)

strat <- strat %>% 
  mutate(lake = factor(lake)) %>% 
  filter(lake %in% c("AL", "BM", "CR", "SP", "TR"))

model <- mgcv::bam(strat.date ~ s(ONI) + s(lake, bs = 're'),
                   data = strat,
                   family = gaussian)

summary(model)
plot(model)





## extract output of model to remake figure in ggplot2 ----
### Extract the smooth term components for s(ONI)
model_output <- plot.gam(model, pages = 1, seWithMean = TRUE)

### Create a data frame with the values for ONI, Latitude, and the smooth term estimates
model_output_data <- data.frame(
  ONI = model_output[[1]]$x,
  oni_fit = model_output[[1]]$fit,
  oni_se.fit = model_output[[1]]$se
)

### Add confidence intervals
model_output_data <- model_output_data %>%
  mutate(oni_lower = oni_fit  - oni_se.fit,
         oni_upper = oni_fit +  oni_se.fit)

# extract R2
r.sq = round(summary(model)$r.sq, 2)

# get the formula
edf <- summary(model)$s.table["s(ONI)", "edf"]

strat$pred <- predict(model)

strat = strat %>% 
  mutate(anomaly = strat.date - mean(strat.date, na.rm = TRUE))

# 6. Figure: ONI impact on Peak Flow ----
# width = 800 height = 600
ggplot(model_output_data, aes(x = ONI, y = oni_fit)) +
  annotate('rect', xmin = 0.5, xmax = Inf,
           ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
  annotate('rect', xmin = -0.5, xmax = 0.5,
           ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
  annotate('text', x = -1.25, y = -15, label = 'Cool Phase/La Niña', size = 5) +
  annotate('text', x = 0, y = -15, label = 'Neutral Phase', size = 5) +
  annotate('text', x = 1.75, y = -15, label = 'Warm Phase/El Niño', size = 5) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
  labs(x = "ONI Phase",
       y = sprintf("Estimated Effect of s(ONI, %.2f)", edf),
       title = bquote("Northern Lakes," ~ R[adj.]^2 ~ "=" ~ .(round(r.sq, 2))),
       subtitle = "Formula = stratification date ~ s(oni) + s(lake, bs = re)") +
  scale_x_continuous(breaks = seq(-2.0,2.5,0.5),
                     limits = c(-2.0,2.5),
                     expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-0.25,0.25,0.05),
  #                    limits = c(-0.28,0.26),
  #                    expand = c(0,0),
  #                    labels = scales::label_number(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 14),
        axis.title = element_text(color = 'black', size = 14),
        plot.margin = margin(0,1,0,0, "lines"))







##### Southern Lakes #####

### read in the ONI data ####

oni <- download_oni() %>%
  filter(ONI_month_window == "DJF") %>% 
  mutate(preced_year = Year - 1,
         years = paste(preced_year, Year, sep = " - ")) %>% 
  rename(water_year = Year) %>% 
  select(years, water_year, ONI_month_window, ONI, phase)


### read in the stratification onset data by year
strat = read.csv("./formatted data/stratification onset dates.csv") %>% 
  rename(water_year = year) %>% 
  left_join(oni, by = "water_year")

ggplot(strat, aes(x = as.numeric(water_year), y = strat.date))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)

strat <- strat %>% 
  mutate(lake = factor(lake)) %>% 
  filter(lake %in% c("ME", "MO", "FI"))

model <- mgcv::bam(strat.date ~ s(ONI) + s(lake, bs = 're'),
                   data = strat,
                   family = gaussian)

summary(model)
plot(model)


## extract output of model to remake figure in ggplot2 ----
### Extract the smooth term components for s(ONI)
model_output <- plot.gam(model, pages = 1, seWithMean = TRUE)

### Create a data frame with the values for ONI, Latitude, and the smooth term estimates
model_output_data <- data.frame(
  ONI = model_output[[1]]$x,
  oni_fit = model_output[[1]]$fit,
  oni_se.fit = model_output[[1]]$se
)

### Add confidence intervals
model_output_data <- model_output_data %>%
  mutate(oni_lower = oni_fit  - oni_se.fit,
         oni_upper = oni_fit +  oni_se.fit)

# extract R2
r.sq = round(summary(model)$r.sq, 2)

# get the formula
edf <- summary(model)$s.table["s(ONI)", "edf"]

strat$pred <- predict(model)

strat = strat %>% 
  mutate(anomaly = strat.date - mean(strat.date, na.rm = TRUE))

# 6. Figure: ONI impact on Peak Flow ----
# width = 800 height = 600
ggplot(model_output_data, aes(x = ONI, y = oni_fit)) +
  annotate('rect', xmin = 0.5, xmax = Inf,
           ymin = -Inf, ymax = Inf, fill = 'red', alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf, fill = 'blue', alpha = 0.2) +
  annotate('rect', xmin = -0.5, xmax = 0.5,
           ymin = -Inf, ymax = Inf, fill = 'orange', alpha = 0.2) +
  annotate('text', x = -1.25, y = -20, label = 'Cool Phase/La Niña', size = 5) +
  annotate('text', x = 0, y = -20, label = 'Neutral Phase', size = 5) +
  annotate('text', x = 1.75, y = -20, label = 'Warm Phase/El Niño', size = 5) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = oni_lower, ymax = oni_upper), alpha = 0.4) +
  labs(x = "ONI Phase",
       y = sprintf("Estimated Effect of s(ONI, %.2f)", edf),
       title = bquote("Southern Lakes," ~ R[adj.]^2 ~ "=" ~ .(round(r.sq, 2))),
       subtitle = "Formula = stratification date ~ s(oni) + s(lake, bs = re)") +
  scale_x_continuous(breaks = seq(-2.0,2.5,0.5),
                     limits = c(-2.0,2.5),
                     expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-0.25,0.25,0.05),
  #                    limits = c(-0.28,0.26),
  #                    expand = c(0,0),
  #                    labels = scales::label_number(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black', size = 14),
        axis.title = element_text(color = 'black', size = 14),
        plot.margin = margin(0,1,0,0, "lines"))


