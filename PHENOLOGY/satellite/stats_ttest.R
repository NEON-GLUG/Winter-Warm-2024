
#GLUG - El Nino - compare MODIS phenology of 2024 to previous years
#April 2026
#Prepared by Kyla Dahlin

df <- read.csv("./PHENOLOGY/satellite/MCD12Q2_Greenup_MidGreenup_2001_2024_20260116.csv")
df <- subset(df, df$Site_Code != "DOWN-ph")

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
df_long$focal.year <- 0

df_long$focal.year[df_long$Year == 2024] <- 1

df_mid <- subset(df_long, df_long$Metric == "MidGreenup")

df_gup <- subset(df_long, df_long$Metric == "Greenup")


#Stats time
#Single sample t-tests as a loop, comparing ALL PREVIOUS Years to the focal year
# for greenup
df_gup %>%
  group_by(Site_Code) %>%                       
  summarise(res = list(tidy(t.test(DOY[focal.year==0], 
                                   mu=DOY[focal.year==1])))) %>%
  unnest()

# for mid greenup
df_mid %>%
  group_by(Site_Code) %>%                       
  summarise(res = list(tidy(t.test(DOY[focal.year==0], 
                                   mu=DOY[focal.year==1])))) %>%
  unnest()
