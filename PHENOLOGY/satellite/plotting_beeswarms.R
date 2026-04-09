library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggbeeswarm)
library(dplyr)

setwd("C:/Users/kylad/Dropbox/GLUG_pheno_2024/greenup/")

#in.names <- read.csv("sitenames.csv")

df <- read.csv("MCD12Q2_Greenup_MidGreenup_2001_2024_20260116.csv")
df <- subset(df, df$Site_Code != "DOWN-ph")

clim <- read.csv("climate_year_types.csv")
names(clim) <- c("Year", "Climate")

phen_cols <- names(df)[grepl("Greenup", names(df), ignore.case = TRUE)]

stopifnot(length(phen_cols) > 0)

df_long <- df %>%
  select(Site_Code, all_of(phen_cols)) %>%
  pivot_longer(
    cols = -Site_Code,
    names_to = c("Metric", "Year"),
    names_pattern = "^X\\d{4}_\\d{2}_\\d{2}_(Greenup_1|MidGreenup_1)_(\\d{4})$",
    values_to = "DOY"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Metric = recode(
      Metric,
      "Greenup_1"    = "Greenup",
      "MidGreenup_1" = "MidGreenup"
    )
  )

df_long$date <- as.Date(df_long$DOY)
df_long$DOY <- as.integer(format(df_long$date, "%j"))
df_long <- left_join(df_long, clim, by = "Year", copy = FALSE)

df_long$Metric <- factor(df_long$Metric, levels = c("Greenup", "MidGreenup"))


# now to plot!
ggplot(df_long, aes(x = Site_Code, y = DOY, color = Site_Code)) +
  geom_beeswarm(size = 2, cex = 1) +   # adjust size/spacing
  facet_wrap(~ Metric, scales = "free_y") +  # one panel per metric
  theme_bw() +
  labs(
    y = "Day of Year (DOY)",
    x = "Site Code",
    title = "MODIS Phenology (Greenup vs MidGreenup)"
  ) +
  theme(
    legend.position = "none",   # optional: remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

df_long <- df_long %>%
  mutate(
    # Shape: triangle (17) for 2024, circle (16) for others
    Shape = ifelse(Year == 2024, 17, 16),
    # Color: red for 2024, blue gradient for other years
    Color = ifelse(Year == 2024, "2024", as.character(Year))
  )

# Convert Color to numeric for gradient, excluding 2024
df_long$Color_numeric <- ifelse(df_long$Year == 2024, NA, df_long$Year)

# Plot

ggplot(df_long, aes(x = Site_Code, y = DOY, color = Climate)) +
  geom_quasirandom(
    aes(shape = factor(Shape), size = factor(Shape))
  ) +
  scale_shape_manual(
    name = "Year group",
    values = c("16" = 16, "17" = 17),
    labels = c("Other years", "2024")
  ) +
  scale_size_manual(
    name = "Year group",
    values = c("16" = 2, "17" = 3.5),
    guide = "none"
  ) +
  scale_color_manual(values = c(
    "cold/dry" = "skyblue1",
    "cold/wet" = "dodgerblue3",
    "neutral" = "black",
    "warm/dry" = "red3",
    "warm/wet" = "orchid1"
  )) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(
    y = "Day of Year",
    x = "Site Code",
    color = "Climate"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  guides(
    shape = guide_legend(
      override.aes = list(
        size = c(2, 3.5)   # must match scale_size_manual values
      )
    )
  )
