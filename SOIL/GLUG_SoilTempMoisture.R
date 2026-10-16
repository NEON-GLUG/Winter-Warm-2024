# Analysis for GLUG manuscript

library(neonUtilities)
library(lubridate)

# # Specify sites of interest
# sites <- c("UNDE", "STEI", "TREE")
# 
# # save the plot
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/soilTempSwc.png"
# png(filename=filename, res=200, width = 10, height = 5, units = "in")
# 
# # Create a plotting window with 2 panels
# par(mfcol=c(4,6))
# 
# # Loop through each site
# for(i in 1:length(sites)){
#   
#   # Download soil temperature data
#   st <- loadByProduct(dpID="DP1.00041.001", include.provisional=T, site=sites[i], timeIndex=30, check.size=F)
#   # Remove rows below measurement level 2
#   st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$verticalPosition < 503), ]
#   
#   # Download soil water content data
#   swc <- loadByProduct(dpID="DP1.00094.001", include.provisional=T, site=sites[i], package="expanded", timeIndex=30, check.size=F)
#   # Remove rows below measurement level 1
#   swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$verticalPosition < 502), ]
#   
#   # # Download soil water content data
#   # co2 <- loadByProduct(dpID="DP1.00095.001", include.provisional=T, site=sites[i], package="basic", timeIndex=30, check.size=F)
#   # # Remove rows below measurement level 2
#   # co2$SCO2C_30_minute <- co2$SCO2C_30_minute[which(co2$SCO2C_30_minute$verticalPosition < 503), ]
#   
#   
#   
#   # Create a day of year column
#   st$ST_30_minute$doy <- yday(st$ST_30_minute$startDateTime)
#   
#   # Create date column
#   st$ST_30_minute$date <- substr(st$ST_30_minute$startDateTime, 1, 10)
#   
#   # Identify rows that passed QAQC for measurement level 1 for soil plot 1 (STEI), 2 (TREE), or 1 (UNDE). Those are plots with lots of good data
#   if(sites[i] == "UNDE"){plot <- "004"}else if(sites[i] == "TREE"){plot <- "002"}else if(sites[i] == "STEI"){plot <- "001"}
#   rowsTemp <- intersect(intersect(which(st$ST_30_minute$finalQF == 0), which(st$ST_30_minute$horizontalPosition == plot)), which(st$ST_30_minute$verticalPosition == "501"))
#   
#   # Calculate mean in soil temperature for each day in the time series
#   dateMeanTemp <- aggregate(st$ST_30_minute$soilTempMean[rowsTemp], by=list(st$ST_30_minute$date[rowsTemp]), FUN=mean)
#   colnames(dateMeanTemp) <- c("date", "soilTempDailyMean")
#   dateMeanTemp$date <- as.POSIXct(dateMeanTemp$date, tz="UTC")
#   
#   # Add day of year
#   dateMeanTemp$doy <- yday(dateMeanTemp$date)
#   
#   
#   # Calculate standard deviation in soil temperature for each day in the time series
#   dateSdTemp <- aggregate(st$ST_30_minute$soilTempMean[rowsTemp], by=list(st$ST_30_minute$date[rowsTemp]), FUN=sd)
#   colnames(dateSdTemp) <- c("date", "soilTempDailySd")
#   dateSdTemp$date <- as.POSIXct(dateSdTemp$date, tz="UTC")
#   
#   # Add day of year
#   dateSdTemp$doy <- yday(dateSdTemp$date)
#   
#   
#   
#   
#   # Remove soil moisture data calculated with the soil specific calibration to ensure consistent sensor calibration is used throughout the time series
#   neonCalStart <- min(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
#   neonCalEnd <- max(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
#   neanCalRows <- intersect(which(swc$SWS_30_minute$startDateTime > neonCalStart), which(swc$SWS_30_minute$startDateTime < neonCalEnd))
#   swc$SWS_30_minute$VSWCMean[neanCalRows] <- NA
#   
#   # Replace soil moisture <0.01 with NA as appears to represent a sensor spike
#   swc$SWS_30_minute$VSWCMean[which(swc$SWS_30_minute$VSWCMean < 0.01)] <- NA
#   
#   # Create a day of year column
#   swc$SWS_30_minute$doy <- yday(swc$SWS_30_minute$startDateTime)
#   
#   # Create date column
#   swc$SWS_30_minute$date <- substr(swc$SWS_30_minute$startDateTime, 1, 10)
#   
#   # Identify rows that passed QAQC for measurement level 1 for soil plot 1 (STEI), 2 (TREE), or 1 (UNDE). Those are plots with lots of good data
#   # if(sites[i] == "UNDE"){plot <- "001"}else if(sites[i] == "TREE"){plot <- "002"}else if(sites[i] == "STEI"){plot <- "001"}
#   rowsSwc <- intersect(intersect(which(swc$SWS_30_minute$VSWCFinalQF == 0), which(swc$SWS_30_minute$horizontalPosition == plot)), which(swc$SWS_30_minute$verticalPosition == "501"))
#   
#   # Calculate mean in soil temperature for each day in the time series
#   dateMeanSwc <- aggregate(swc$SWS_30_minute$VSWCMean[rowsSwc], by=list(swc$SWS_30_minute$date[rowsSwc]), FUN=mean, na.rm=T)
#   colnames(dateMeanSwc) <- c("date", "swcDailyMean")
#   dateMeanSwc$date <- as.POSIXct(dateMeanSwc$date, tz="UTC")
#   
#   # Add day of year
#   dateMeanSwc$doy <- yday(dateMeanSwc$date)
#   
#   
#   # Calculate standard deviation in soil temperature for each day in the time series
#   dateSdSwc <- aggregate(swc$SWS_30_minute$VSWCMean[rowsSwc], by=list(swc$SWS_30_minute$date[rowsSwc]), FUN=sd, na.rm=T)
#   colnames(dateSdSwc) <- c("date", "swcDailySd")
#   dateSdSwc$date <- as.POSIXct(dateSdSwc$date, tz="UTC")
#   
#   # Add day of year
#   dateSdSwc$doy <- yday(dateSdSwc$date)
#   
#   
#   
#   
#   
#   # # Create a day of year column
#   # co2$SCO2C_30_minute$doy <- yday(co2$SCO2C_30_minute$startDateTime)
#   # 
#   # # Create date column
#   # co2$SCO2C_30_minute$date <- substr(co2$SCO2C_30_minute$startDateTime, 1, 10)
#   # 
#   # # Identify rows that passed QAQC for measurement level 1 for soil plot 1 (STEI), 2 (TREE), or 1 (UNDE). Those are plots with lots of good data
#   # if(sites[i] == "UNDE"){plot <- "004"}else if(sites[i] == "TREE"){plot <- "002"}else if(sites[i] == "STEI"){plot <- "001"}
#   # rowsCo2 <- intersect(intersect(which(co2$SCO2C_30_minute$finalQF == 0), which(co2$SCO2C_30_minute$horizontalPosition == plot)), which(co2$SCO2C_30_minute$verticalPosition == "501"))
#   # 
#   # # Calculate mean in soil CO2 for each day in the time series
#   # dateMeanCo2 <- aggregate(co2$SCO2C_30_minute$soilCO2concentrationMean[rowsCo2], by=list(co2$SCO2C_30_minute$date[rowsCo2]), FUN=mean)
#   # colnames(dateMeanCo2) <- c("date", "co2DailyMean")
#   # dateMeanCo2$date <- as.POSIXct(dateMeanCo2$date, tz="UTC")
#   # 
#   # # Add day of year
#   # dateMeanCo2$doy <- yday(dateMeanCo2$date)
#   # 
#   # 
#   # # Calculate standard deviation in soil CO2 for each day in the time series
#   # dateSdCo2 <- aggregate(co2$SCO2C_30_minute$soilCO2concentrationMean[rowsCo2], by=list(co2$SCO2C_30_minute$date[rowsCo2]), FUN=sd)
#   # colnames(dateSdCo2) <- c("date", "co2DailySd")
#   # dateSdCo2$date <- as.POSIXct(dateSdCo2$date, tz="UTC")
#   # 
#   # # Add day of year
#   # dateSdCo2$doy <- yday(dateSdCo2$date)
#   
#   
#   
#   
#   
#   
#   
#   # Create mean soil temperature plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateMeanTemp$doy, dateMeanTemp$soilTempDailyMean, pch=NA, ylim=c(-5, max(dateMeanTemp$soilTempDailyMean)+5), xlab="", ylab="Ts daily mean (°C)", main=sites[i], cex=0.7)
#   lines(dateMeanTemp$doy[grep("2017", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2017", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2018", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2018", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2019", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2019", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2020", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2020", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2021", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2021", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2022", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2022", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2023", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2023", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[intersect(grep("2023", dateMeanTemp$date), which(dateMeanTemp$doy>300))], 
#         dateMeanTemp$soilTempDailyMean[intersect(grep("2023", dateMeanTemp$date), which(dateMeanTemp$doy>300))], col="blue", lwd=1)
#   lines(dateMeanTemp$doy[grep("2024", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2024", dateMeanTemp$date)], col="blue", lwd=1)
#   lines(aggregate(dateMeanTemp$soilTempDailyMean, by=list(dateMeanTemp$doy), FUN=median, na.rm=T), lwd=2)
#   legend("topleft", legend=c("Multi-year median", "23/24 ENSO winter"), lty=1, col=c("black", "blue"), lwd=c(2,1), bty="n", cex=0.7)
#   
#   # Create soil temperature SD plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateSdTemp$doy, dateSdTemp$soilTempDailySd, pch=NA, xlab="Day of year", ylab="Ts daily SD (°C)", cex=0.7)
#   lines(dateSdTemp$doy[grep("2017", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2017", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2018", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2018", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2019", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2019", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2020", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2020", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2021", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2021", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2022", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2022", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2023", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2023", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[intersect(grep("2023", dateSdTemp$date), which(dateSdTemp$doy>300))], 
#         dateSdTemp$soilTempDailySd[intersect(grep("2023", dateSdTemp$date), which(dateSdTemp$doy>300))], col="blue", lwd=1)
#   lines(dateSdTemp$doy[grep("2024", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2024", dateSdTemp$date)], col="blue", lwd=1)
#   lines(aggregate(dateSdTemp$soilTempDailySd, by=list(dateSdTemp$doy), FUN=median, na.rm=T), lwd=2)
#   
#   
#   
#   # Create mean soil moisture plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateMeanSwc$doy, dateMeanSwc$swcDailyMean, pch=NA, ylim=c(0, max(dateMeanSwc$swcDailyMean, na.rm=T)), xlab="", ylab="SWC daily mean (m3/m3)", cex=0.7)
#   lines(dateMeanSwc$doy[grep("2017", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2017", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2018", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2018", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2019", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2019", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2020", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2020", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2021", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2021", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2022", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2022", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2023", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2023", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[intersect(grep("2023", dateMeanSwc$date), which(dateMeanSwc$doy>300))], 
#         dateMeanSwc$swcDailyMean[intersect(grep("2023", dateMeanSwc$date), which(dateMeanSwc$doy>300))], col="blue", lwd=1)
#   lines(dateMeanSwc$doy[grep("2024", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2024", dateMeanSwc$date)], col="blue", lwd=1)
#   lines(aggregate(dateMeanSwc$swcDailyMean, by=list(dateMeanSwc$doy), FUN=median, na.rm=T), lwd=2)
#   # legend("topleft", legend=c("Multi-year median", "23/24 ENSO winter"), lty=1, col=c("black", "blue"), lwd=2, bty="n")
#   
#   # Create soil moisture SD plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateSdSwc$doy, dateSdSwc$swcDailySd, pch=NA, xlab="Day of year", ylab="SWC daily SD (m3/m3)", cex=0.7)
#   lines(dateSdSwc$doy[grep("2017", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2017", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2018", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2018", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2019", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2019", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2020", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2020", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2021", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2021", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2022", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2022", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2023", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2023", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[intersect(grep("2023", dateSdSwc$date), which(dateSdSwc$doy>300))], 
#         dateSdSwc$swcDailySd[intersect(grep("2023", dateSdSwc$date), which(dateSdSwc$doy>300))], col="blue", lwd=1)
#   lines(dateSdSwc$doy[grep("2024", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2024", dateSdSwc$date)], col="blue", lwd=1)
#   lines(aggregate(dateSdSwc$swcDailySd, by=list(dateSdSwc$doy), FUN=median, na.rm=T), lwd=2)
#   
#   
#   
#   # # Create mean soil CO2 plot
#   # par(mar = c(2.1, 4.1, 2.1, 0.5))
#   # plot(dateMeanCo2$doy, dateMeanCo2$co2DailyMean, pch=NA, ylim=c(0, max(dateMeanCo2$co2DailyMean)), xlab="", ylab="Daily soil CO2 mean (ppm)", cex=0.7)
#   # lines(dateMeanCo2$doy[grep("2017", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2017", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[grep("2018", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2018", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[grep("2019", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2019", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[grep("2020", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2020", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[grep("2021", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2021", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[grep("2022", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2022", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[grep("2023", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2023", dateMeanCo2$date)], col="grey")
#   # lines(dateMeanCo2$doy[intersect(grep("2023", dateMeanCo2$date), which(dateMeanCo2$doy>300))], 
#   #       dateMeanCo2$co2DailyMean[intersect(grep("2023", dateMeanCo2$date), which(dateMeanCo2$doy>300))], col="blue", lwd=1)
#   # lines(dateMeanCo2$doy[grep("2024", dateMeanCo2$date)], dateMeanCo2$co2DailyMean[grep("2024", dateMeanCo2$date)], col="blue", lwd=1)
#   # lines(aggregate(dateMeanCo2$co2DailyMean, by=list(dateMeanCo2$doy), FUN=median, na.rm=T), lwd=2)
#   # 
#   # # Create soil CO2 SD plot
#   # par(mar = c(4.1, 4.1, 2.1, 0.5))
#   # plot(dateSdCo2$doy, dateSdCo2$co2DailySd, pch=NA, xlab="Day of year", ylab="Daily soil CO2 SD (ppm)", cex=0.7)
#   # lines(dateSdCo2$doy[grep("2017", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2017", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[grep("2018", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2018", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[grep("2019", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2019", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[grep("2020", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2020", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[grep("2021", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2021", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[grep("2022", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2022", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[grep("2023", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2023", dateSdCo2$date)], col="grey")
#   # lines(dateSdCo2$doy[intersect(grep("2023", dateSdCo2$date), which(dateSdCo2$doy>300))], 
#   #       dateSdCo2$co2DailySd[intersect(grep("2023", dateSdCo2$date), which(dateSdCo2$doy>300))], col="blue", lwd=1)
#   # lines(dateSdCo2$doy[grep("2024", dateSdCo2$date)], dateSdCo2$co2DailySd[grep("2024", dateSdCo2$date)], col="blue", lwd=1)
#   # lines(aggregate(dateSdCo2$co2DailySd, by=list(dateSdCo2$doy), FUN=median, na.rm=T), lwd=2)
#   
#   
#   # Remove soil temp, soil moisture, and soil CO2 data
#   st <- NA
#   swc <- NA
#   # co2 <- NA
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### Get SCAN data
# # Hourly data manually downloaded from:
# # https://nwcc-apps.sc.egov.usda.gov/imap/#version=169&elements=&networks=SNTL,SCAN,SNTLT,OTHER&states=!&counties=!&hucs=&minElevation=&maxElevation=&elementSelectType=all&activeOnly=true&activeForecastPointsOnly=false&hucLabels=false&hucIdLabels=false&hucParameterLabels=false&stationLabels=&overlays=&hucOverlays=&basinOpacity=100&basinNoDataOpacity=100&basemapOpacity=100&maskOpacity=0&mode=stations&openSections=dataElement,parameter,date,basin,elements,location,networks&controlsOpen=true&popup=2003:WI:SCAN&popupMulti=&popupBasin=&base=esriNgwm&displayType=inventory&basinType=6&dataElement=STO&depth=-8&parameter=PCTPORAVG&frequency=DAILY&duration=I&customDuration=&dayPart=E&year=2018&month=11&day=1&monthPart=E&forecastPubMonth=6&forecastPubDay=1&forecastExceedance=50&useMixedPast=true&seqColor=1&divColor=3&scaleType=D&scaleMin=&scaleMax=&referencePeriodType=POR&referenceBegin=1981&referenceEnd=2010&minimumYears=20&hucAssociations=true&lat=42.533&lon=-82.536&zoom=6.5
# 
# 
# # Create vector of SCAN site IDs
# scanSites <- c("2003", "2196", "2004")
# 
# for(j in 1:length(scanSites)){
#   files <- list.files(paste0("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/SCAN data/", scanSites[j]), full.names = T)
#   
#   
#   # # Create a vector of SCAN data files
#   # files <- list.files("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/SCAN data/2003", full.names = T)
#   
#   # # Read in SCAN data
#   # library("data.table")    
#   # scan <- fread(files[1], select = c("Site Id", "Date", "Time", "SMS.I-1:-2 (pct)  (loam)", "STO.I-1:-2 (degC)"))
#   # for(i in 2:length(files)){
#   #   x <- fread(files[i], select = c("Site Id", "Date", "Time", "SMS.I-1:-2 (pct)  (loam)", "STO.I-1:-2 (degC)"))
#   #   scan <- rbind(scan, x)
#   # }
#   
#   
#   
#   # Read in SCAN data (start with file #12 because data was missing or formatted differently earlier)
#   scan <- read.csv(files[1], stringsAsFactors = F, skip=6)
#   for(i in 2:length(files)){
#     x <- read.csv(files[i], stringsAsFactors = F, skip=6)
#     scan <- rbind(scan, x)
#   }
#   
#   # Temperatures and soil moistures of around -100 represent errors in the SCAN data, so replace them with NA
#   scan$STO.I.1..2..degC.[which(scan$STO.I.1..2..degC. < -20)] <- NA
#   scan$SMS.I.1..2..pct....loam.[which(scan$SMS.I.1..2..pct....loam. < -20)] <- NA
#   
#   # Replace soil moisture with NA if the temperature was below 0 °C
#   scan$SMS.I.1..2..pct....loam.[which(scan$STO.I.1..2..degC. < 0.1)] <- NA
#   
#   # Convert % volumetric soil moisture data to m3/m3
#   scan$SMS.I.1..2..pct....loam. <- scan$SMS.I.1..2..pct....loam./100
#   
#   # Create a day of year column
#   scan$doy <- yday(scan$Date)
#   
#   # # Create date column
#   # scan$date <- as.Date(scan$Date)
#   
#   # # Identify rows that passed QAQC for measurement level 1 for soil plot 1 (STEI), 2 (TREE), or 1 (UNDE). Those are plots with lots of good data
#   # if(sites[i] == "UNDE"){plot <- "004"}else if(sites[i] == "TREE"){plot <- "002"}else if(sites[i] == "STEI"){plot <- "001"}
#   # rowsTemp <- intersect(intersect(which(st$ST_30_minute$finalQF == 0), which(st$ST_30_minute$horizontalPosition == plot)), which(st$ST_30_minute$verticalPosition == "501"))
#   
#   # Calculate mean soil temperature for each day in the time series
#   dateMeanTemp <- aggregate(scan$STO.I.1..2..degC., by=list(scan$Date), FUN=mean)
#   colnames(dateMeanTemp) <- c("date", "soilTempDailyMean")
#   dateMeanTemp$date <- as.POSIXct(dateMeanTemp$date, tz="UTC")
#   
#   # Add day of year
#   dateMeanTemp$doy <- yday(dateMeanTemp$date)
#   
#   
#   # Calculate standard deviation in soil temperature for each day in the time series
#   dateSdTemp <- aggregate(scan$STO.I.1..2..degC., by=list(scan$Date), FUN=sd)
#   colnames(dateSdTemp) <- c("date", "soilTempDailySd")
#   dateSdTemp$date <- as.POSIXct(dateSdTemp$date, tz="UTC")
#   
#   # Add day of year
#   dateSdTemp$doy <- yday(dateSdTemp$date)
#   
#   
#   
#   
#   
#   
#   
#   
#   # Calculate mean soil temperature for each day in the time series
#   dateMeanSwc <- aggregate(scan$SMS.I.1..2..pct....loam., by=list(scan$Date), FUN=mean)
#   colnames(dateMeanSwc) <- c("date", "swcDailyMean")
#   dateMeanSwc$date <- as.POSIXct(dateMeanSwc$date, tz="UTC")
#   
#   # Add day of year
#   dateMeanSwc$doy <- yday(dateMeanSwc$date)
#   
#   
#   # Calculate standard deviation in soil temperature for each day in the time series
#   dateSdSwc <- aggregate(scan$SMS.I.1..2..pct....loam., by=list(scan$Date), FUN=sd)
#   colnames(dateSdSwc) <- c("date", "swcDailySd")
#   dateSdSwc$date <- as.POSIXct(dateSdSwc$date, tz="UTC")
#   
#   # Add day of year
#   dateSdSwc$doy <- yday(dateSdSwc$date)
#   
#   
#   
#   
#   
#   
#   # plot(dateMeanTemp$date, dateMeanTemp$soilTempDailyMean)
#   # plot(dateMeanTemp$doy, dateMeanTemp$soilTempDailyMean)
#   # plot(dateSdTemp$date, dateSdTemp$soilTempDailySd)
#   # plot(dateSdTemp$doy, dateSdTemp$soilTempDailySd)
#   # 
#   # plot(dateMeanSwc$date, dateMeanSwc$swcDailyMean)
#   # plot(dateMeanSwc$doy, dateMeanSwc$swcDailyMean)
#   # plot(dateSdSwc$date, dateSdSwc$swcDailySd)
#   # plot(dateSdSwc$doy, dateSdSwc$swcDailySd)
#   # 
#   # plot(scan$STO.I.1..2..degC.)
#   # plot(scan$SMS.I.1..2..pct....loam.)
#   
#   
#   
#   # ENSO start and end dates from: https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
#   # Create mean soil temperature plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateMeanTemp$doy, dateMeanTemp$soilTempDailyMean, pch=NA, ylim=c(-5, max(dateMeanTemp$soilTempDailyMean, na.rm=T)+5), xlab="", ylab="Ts daily mean (°C)", cex=0.7, main=scanSites[j])
#   lines(dateMeanTemp$doy[grep("2009", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2009", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2010", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2010", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2011", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2011", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2012", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2012", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2013", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2013", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2014", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2014", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2015", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2015", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2016", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2016", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2017", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2017", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2018", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2018", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2019", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2019", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2020", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2020", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2021", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2021", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2022", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2022", dateMeanTemp$date)], col="grey")
#   lines(dateMeanTemp$doy[grep("2023", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2023", dateMeanTemp$date)], col="grey")
#   # # 2018-2019
#   # lines(dateMeanTemp$doy[intersect(grep("2018", dateMeanTemp$date), which(dateMeanTemp$doy>240))], 
#   #       dateMeanTemp$soilTempDailyMean[intersect(grep("2018", dateMeanTemp$date), which(dateMeanTemp$doy>240))], col="pink", lwd=1)
#   # lines(dateMeanTemp$doy[intersect(grep("2019", dateMeanTemp$date), which(dateMeanTemp$doy<180))], dateMeanTemp$soilTempDailyMean[intersect(grep("2019", dateMeanTemp$date), which(dateMeanTemp$doy<180))], col="pink", lwd=1)
#   # # 2014-2016
#   # lines(dateMeanTemp$doy[intersect(grep("2014", dateMeanTemp$date), which(dateMeanTemp$doy>270))], 
#   #       dateMeanTemp$soilTempDailyMean[intersect(grep("2014", dateMeanTemp$date), which(dateMeanTemp$doy>270))], col="red", lwd=1)
#   # lines(dateMeanTemp$doy[grep("2015", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2015", dateMeanTemp$date)], col="red")
#   # lines(dateMeanTemp$doy[intersect(grep("2016", dateMeanTemp$date), which(dateMeanTemp$doy<120))], dateMeanTemp$soilTempDailyMean[intersect(grep("2016", dateMeanTemp$date), which(dateMeanTemp$doy<120))], col="red", lwd=1)
#   
#   # 2023-2024
#   lines(dateMeanTemp$doy[intersect(grep("2023", dateMeanTemp$date), which(dateMeanTemp$doy>300))], 
#         dateMeanTemp$soilTempDailyMean[intersect(grep("2023", dateMeanTemp$date), which(dateMeanTemp$doy>300))], col="blue", lwd=1)
#   lines(dateMeanTemp$doy[grep("2024", dateMeanTemp$date)], dateMeanTemp$soilTempDailyMean[grep("2024", dateMeanTemp$date)], col="blue", lwd=1)
#   lines(aggregate(dateMeanTemp$soilTempDailyMean, by=list(dateMeanTemp$doy), FUN=median, na.rm=T), lwd=2)
#   legend("topleft", legend=c("Multi-year median", "23/24 ENSO winter"), lty=1, col=c("black", "blue"), lwd=c(2,1), bty="n", cex=0.7)
#   
#   
#   
#   # Create soil temperature SD plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateSdTemp$doy, dateSdTemp$soilTempDailySd, pch=NA, xlab="Day of year", ylab="Ts daily SD (°C)", cex=0.7)
#   lines(dateSdTemp$doy[grep("2010", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2010", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2011", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2011", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2012", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2012", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2013", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2013", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2014", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2014", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2015", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2015", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2016", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2016", dateSdTemp$date)], col="grey") 
#   lines(dateSdTemp$doy[grep("2017", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2017", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2018", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2018", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2019", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2019", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2020", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2020", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2021", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2021", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2022", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2022", dateSdTemp$date)], col="grey")
#   lines(dateSdTemp$doy[grep("2023", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2023", dateSdTemp$date)], col="grey")
#   # # 2018-2019
#   # lines(dateSdTemp$doy[intersect(grep("2018", dateSdTemp$date), which(dateSdTemp$doy>240))], 
#   #       dateSdTemp$soilTempDailySd[intersect(grep("2018", dateSdTemp$date), which(dateSdTemp$doy>240))], col="pink", lwd=1)
#   # lines(dateSdTemp$doy[intersect(grep("2019", dateSdTemp$date), which(dateSdTemp$doy<180))], dateSdTemp$soilTempDailySd[intersect(grep("2019", dateSdTemp$date), which(dateSdTemp$doy<180))], col="pink", lwd=1)
#   # # 2014-2016
#   # lines(dateSdTemp$doy[intersect(grep("2014", dateSdTemp$date), which(dateSdTemp$doy>270))], 
#   #       dateSdTemp$soilTempDailySd[intersect(grep("2014", dateSdTemp$date), which(dateSdTemp$doy>270))], col="red", lwd=1)
#   # lines(dateSdTemp$doy[grep("2015", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2015", dateSdTemp$date)], col="red")
#   # lines(dateSdTemp$doy[intersect(grep("2016", dateSdTemp$date), which(dateSdTemp$doy<120))], dateSdTemp$soilTempDailySd[intersect(grep("2016", dateSdTemp$date), which(dateSdTemp$doy<120))], col="red", lwd=1)
#   
#   # 2023-2024
#   lines(dateSdTemp$doy[intersect(grep("2023", dateSdTemp$date), which(dateSdTemp$doy>300))], 
#         dateSdTemp$soilTempDailySd[intersect(grep("2023", dateSdTemp$date), which(dateSdTemp$doy>300))], col="blue", lwd=1)
#   lines(dateSdTemp$doy[grep("2024", dateSdTemp$date)], dateSdTemp$soilTempDailySd[grep("2024", dateSdTemp$date)], col="blue", lwd=1)
#   lines(aggregate(dateSdTemp$soilTempDailySd, by=list(dateSdTemp$doy), FUN=median, na.rm=T), lwd=2)
#   
#   
#   
#   # Create mean soil moisture plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateMeanSwc$doy, dateMeanSwc$swcDailyMean, pch=NA, ylim=c(0, max(dateMeanSwc$swcDailyMean, na.rm=T)), xlab="", ylab="SWC daily mean (m3/m3)", cex=0.7)
#   lines(dateMeanSwc$doy[grep("2010", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2010", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2011", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2011", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2012", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2012", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2013", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2013", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2014", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2014", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2015", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2015", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2016", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2016", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2017", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2017", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2018", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2018", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2019", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2019", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2020", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2020", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2021", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2021", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2022", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2022", dateMeanSwc$date)], col="grey")
#   lines(dateMeanSwc$doy[grep("2023", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2023", dateMeanSwc$date)], col="grey")
#   # # 2018-2019
#   # lines(dateMeanSwc$doy[intersect(grep("2018", dateMeanSwc$date), which(dateMeanSwc$doy>240))], 
#   #       dateMeanSwc$swcDailyMean[intersect(grep("2018", dateMeanSwc$date), which(dateMeanSwc$doy>240))], col="pink", lwd=1)
#   # lines(dateMeanSwc$doy[intersect(grep("2019", dateMeanSwc$date), which(dateMeanSwc$doy<180))], dateMeanSwc$swcDailyMean[intersect(grep("2019", dateMeanSwc$date), which(dateMeanSwc$doy<180))], col="pink", lwd=1)
#   # # 2014-2016
#   # lines(dateMeanSwc$doy[intersect(grep("2014", dateMeanSwc$date), which(dateMeanSwc$doy>270))], 
#   #       dateMeanSwc$swcDailyMean[intersect(grep("2014", dateMeanSwc$date), which(dateMeanSwc$doy>270))], col="red", lwd=1)
#   # lines(dateMeanSwc$doy[grep("2015", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2015", dateMeanSwc$date)], col="red")
#   # lines(dateMeanSwc$doy[intersect(grep("2016", dateMeanSwc$date), which(dateMeanSwc$doy<120))], dateMeanSwc$swcDailyMean[intersect(grep("2016", dateMeanSwc$date), which(dateMeanSwc$doy<120))], col="red", lwd=1)
#   
#   # 2023-2024
#   lines(dateMeanSwc$doy[intersect(grep("2023", dateMeanSwc$date), which(dateMeanSwc$doy>300))], 
#         dateMeanSwc$swcDailyMean[intersect(grep("2023", dateMeanSwc$date), which(dateMeanSwc$doy>300))], col="blue", lwd=1)
#   lines(dateMeanSwc$doy[grep("2024", dateMeanSwc$date)], dateMeanSwc$swcDailyMean[grep("2024", dateMeanSwc$date)], col="blue", lwd=1)
#   lines(aggregate(dateMeanSwc$swcDailyMean, by=list(dateMeanSwc$doy), FUN=median, na.rm=T), lwd=2)
#   
#   
#   
#   
#   # Create soil moisture SD plot
#   par(mar = c(2.1, 4.1, 2.1, 0.5))
#   plot(dateSdSwc$doy, dateSdSwc$swcDailySd, pch=NA, xlab="Day of year", ylab="SWC daily SD (m3/m3)", cex=0.7)
#   lines(dateSdSwc$doy[grep("2010", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2010", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2011", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2011", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2012", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2012", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2013", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2013", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2014", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2014", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2015", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2015", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2016", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2016", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2017", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2017", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2018", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2018", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2019", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2019", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2020", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2020", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2021", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2021", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2022", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2022", dateSdSwc$date)], col="grey")
#   lines(dateSdSwc$doy[grep("2023", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2023", dateSdSwc$date)], col="grey")
#   # # 2018-2019
#   # lines(dateSdSwc$doy[intersect(grep("2018", dateSdSwc$date), which(dateSdSwc$doy>240))], 
#   #       dateSdSwc$swcDailySd[intersect(grep("2018", dateSdSwc$date), which(dateSdSwc$doy>240))], col="pink", lwd=1)
#   # lines(dateSdSwc$doy[intersect(grep("2019", dateSdSwc$date), which(dateSdSwc$doy<180))], dateSdSwc$swcDailySd[intersect(grep("2019", dateSdSwc$date), which(dateSdSwc$doy<180))], col="pink", lwd=1)
#   # # 2014-2016
#   # lines(dateSdSwc$doy[intersect(grep("2014", dateSdSwc$date), which(dateSdSwc$doy>270))], 
#   #       dateSdSwc$swcDailySd[intersect(grep("2014", dateSdSwc$date), which(dateSdSwc$doy>270))], col="red", lwd=1)
#   # lines(dateSdSwc$doy[grep("2015", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2015", dateSdSwc$date)], col="red")
#   # lines(dateSdSwc$doy[intersect(grep("2016", dateSdSwc$date), which(dateSdSwc$doy<120))], dateSdSwc$swcDailySd[intersect(grep("2016", dateSdSwc$date), which(dateSdSwc$doy<120))], col="red", lwd=1)
#   
#   # 2023-2024
#   lines(dateSdSwc$doy[intersect(grep("2023", dateSdSwc$date), which(dateSdSwc$doy>300))], 
#         dateSdSwc$swcDailySd[intersect(grep("2023", dateSdSwc$date), which(dateSdSwc$doy>300))], col="blue", lwd=1)
#   lines(dateSdSwc$doy[grep("2024", dateSdSwc$date)], dateSdSwc$swcDailySd[grep("2024", dateSdSwc$date)], col="blue", lwd=1)
#   lines(aggregate(dateSdSwc$swcDailySd, by=list(dateSdSwc$doy), FUN=median, na.rm=T), lwd=2)
#   
#   
#   
#   # Remove SCAN data
#   scan <- NA
#   
# }
# 
# dev.off()



# 
# # Remove soil temp, soil moisture, and soil CO2 data
# st <- NA
# swc <- NA






# # Read in El Nino NOAA ONI index
# oni <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/ONI_elNinoIndex.csv", stringsAsFactors = F)
# 
# # Add a column with the extremeness of the El Nino event (none 0: oni <0.5; mild 1: 0.5>= oni <=1; moderate 2: 1<=oni<2; strong 3: 2<=oni)
# oni$extremeness <- NA
# oni$extremeness[which(oni$oni < 0.5)] <- 0
# oni$extremeness[intersect(which(oni$oni >= 0.5), which(oni$oni < 1))] <- 1
# oni$extremeness[intersect(which(oni$oni >= 1), which(oni$oni < 2))] <- 2
# oni$extremeness[which(oni$oni >= 2)] <- 3
# 
# # Add year-month column (YYYY-MM)
# oni$yearMonth <- NA
# oni$yearMonth[which(oni$months == "DJF")] <- paste0(oni$year[which(oni$months == "DJF")], "-01")
# oni$yearMonth[which(oni$months == "JFM")] <- paste0(oni$year[which(oni$months == "JFM")], "-02")
# oni$yearMonth[which(oni$months == "FMA")] <- paste0(oni$year[which(oni$months == "FMA")], "-03")
# oni$yearMonth[which(oni$months == "MAM")] <- paste0(oni$year[which(oni$months == "MAM")], "-04")
# oni$yearMonth[which(oni$months == "AMJ")] <- paste0(oni$year[which(oni$months == "AMJ")], "-05")
# oni$yearMonth[which(oni$months == "MJJ")] <- paste0(oni$year[which(oni$months == "MJJ")], "-06")
# oni$yearMonth[which(oni$months == "JJA")] <- paste0(oni$year[which(oni$months == "JJA")], "-07")
# oni$yearMonth[which(oni$months == "JAS")] <- paste0(oni$year[which(oni$months == "JAS")], "-08")
# oni$yearMonth[which(oni$months == "ASO")] <- paste0(oni$year[which(oni$months == "ASO")], "-09")
# oni$yearMonth[which(oni$months == "SON")] <- paste0(oni$year[which(oni$months == "SON")], "-10")
# oni$yearMonth[which(oni$months == "OND")] <- paste0(oni$year[which(oni$months == "OND")], "-11")
# oni$yearMonth[which(oni$months == "NDJ")] <- paste0(oni$year[which(oni$months == "NDJ")], "-12")
# 





# Read in UMBS FASET data
umbsFaset <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/UMBS data/UMBS_Rs/compiled_Rs_FASET.csv", stringsAsFactors = F)

# Only keep control plot data
umbsFaset <- umbsFaset[grep("control", umbsFaset$site), ]

# Remove implausible temperature measurements
umbsFaset$temp[which(umbsFaset$temp < -40)] <- NA

# Convert moisture data to numeric
umbsFaset$moisture <- as.numeric(umbsFaset$moisture)

# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRows <- which(umbsFaset$efflux < -1)
suspectRows <- append(suspectRows, which(umbsFaset$efflux > 50))
proportionSuspectUmbsFaset <- length(suspectRows) / nrow(umbsFaset)
if(length(suspectRows) > 0){
  umbsFaset <- umbsFaset[-suspectRows ,]
}

# Add a date column (YYYY-MM-DD) to the data frame
umbsFaset$date <- as.Date(umbsFaset$doy, origin= paste0(umbsFaset$year, "-01-01") )

# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesUmbsFaset <- aggregate(umbsFaset, by=list(substr(umbsFaset$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesUmbsFaset <- data.frame(unique(substr(umbsFaset$date, 1, 10)))
# colnames(datesUmbsFaset) <- "date"
# datesUmbsFaset$efflux <- NA
# datesUmbsFaset$temp <- NA
# datesUmbsFaset$moisture <- NA
# for(i in 1:nrow(datesUmbsFaset)){
#   datesUmbsFaset$efflux[i] <- mean(umbsFaset$efflux[grep(datesUmbsFaset$date[i], umbsFaset$date)], na.rm=T)
# }

# remove unnecessary data
umbsFaset <- NA

# Add a DOY column
datesUmbsFaset$doy <- yday(datesUmbsFaset$date)

# Add data source
datesUmbsFaset$source <- "umbsFaset"

# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesUmbsFaset$date)
neutral <- append(neutral, grep("1999", datesUmbsFaset$date))
neutral <- append(neutral, grep("2002", datesUmbsFaset$date))
neutral <- append(neutral, grep("2004", datesUmbsFaset$date))
neutral <- append(neutral, grep("2005", datesUmbsFaset$date))
neutral <- append(neutral, grep("2011", datesUmbsFaset$date))
neutral <- append(neutral, grep("2018", datesUmbsFaset$date))

coldWet <- grep("1997", datesUmbsFaset$date)
coldWet <- append(coldWet, grep("2008", datesUmbsFaset$date))
coldWet <- append(coldWet, grep("2009", datesUmbsFaset$date))
coldWet <- append(coldWet, grep("2014", datesUmbsFaset$date))
coldWet <- append(coldWet, grep("2019", datesUmbsFaset$date))
coldWet <- append(coldWet, grep("2022", datesUmbsFaset$date))

coldDry <- grep("1993", datesUmbsFaset$date)
coldDry <- append(coldDry, grep("1994", datesUmbsFaset$date))
coldDry <- append(coldDry, grep("1996", datesUmbsFaset$date))
coldDry <- append(coldDry, grep("2001", datesUmbsFaset$date))
coldDry <- append(coldDry, grep("2003", datesUmbsFaset$date))
coldDry <- append(coldDry, grep("2015", datesUmbsFaset$date))

warmDry <- grep("1992", datesUmbsFaset$date)
warmDry <- append(warmDry, grep("1995", datesUmbsFaset$date))
warmDry <- append(warmDry, grep("2000", datesUmbsFaset$date))
warmDry <- append(warmDry, grep("2010", datesUmbsFaset$date))
warmDry <- append(warmDry, grep("2021", datesUmbsFaset$date))
warmDry <- append(warmDry, grep("2024", datesUmbsFaset$date))

warmWet <- grep("1998", datesUmbsFaset$date)
warmWet <- append(warmWet, grep("2006", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2007", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2012", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2013", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2016", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2017", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2020", datesUmbsFaset$date))
warmWet <- append(warmWet, grep("2023", datesUmbsFaset$date))

# Add preceding winter category
datesUmbsFaset$winterType <- NA
datesUmbsFaset$winterType[neutral] <- "neutral"
datesUmbsFaset$winterType[coldWet] <- "coldWet"
datesUmbsFaset$winterType[coldDry] <- "coldDry"
datesUmbsFaset$winterType[warmDry] <- "warmDry"
datesUmbsFaset$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesUmbsFaset)[grep("efflux", colnames(datesUmbsFaset))] <- "flux"
colnames(datesUmbsFaset)[grep("temp", colnames(datesUmbsFaset))] <- "temp"
colnames(datesUmbsFaset)[grep("moisture", colnames(datesUmbsFaset))] <- "swc"

# Convert soil moisture units from % vol to m3 m-3
datesUmbsFaset$swc <- datesUmbsFaset$swc/100


# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesUmbsFaset$date >= as.Date("1997-11-01")), which(datesUmbsFaset$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesUmbsFaset$date >= as.Date("2015-11-01")), which(datesUmbsFaset$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesUmbsFaset$date >= as.Date("2023-11-01")), which(datesUmbsFaset$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesUmbsFaset$date >= as.Date("1991-11-01")), which(datesUmbsFaset$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesUmbsFaset$date >= as.Date("1994-11-01")), which(datesUmbsFaset$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUmbsFaset$date >= as.Date("2002-11-01")), which(datesUmbsFaset$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUmbsFaset$date >= as.Date("2009-11-01")), which(datesUmbsFaset$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesUmbsFaset$date >= as.Date("2004-11-01")), which(datesUmbsFaset$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesUmbsFaset$date >= as.Date("2006-11-01")), which(datesUmbsFaset$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUmbsFaset$date >= as.Date("2014-11-01")), which(datesUmbsFaset$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUmbsFaset$date >= as.Date("2018-11-01")), which(datesUmbsFaset$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesUmbsFaset$date >= as.Date("1990-11-01")), which(datesUmbsFaset$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("1992-11-01")), which(datesUmbsFaset$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("1995-11-01")), which(datesUmbsFaset$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("1998-11-01")), which(datesUmbsFaset$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("2003-11-01")), which(datesUmbsFaset$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("2005-11-01")), which(datesUmbsFaset$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("2007-11-01")), which(datesUmbsFaset$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("2010-11-01")), which(datesUmbsFaset$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("2016-11-01")), which(datesUmbsFaset$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsFaset$date >= as.Date("2019-11-01")), which(datesUmbsFaset$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUmbsFaset.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUmbsFaset$doy[neutral], datesUmbsFaset$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="UMBS FASET: 2008-17 all control plots", xlim=c(0, 365), ylim=c(0,max(datesUmbsFaset$flux, na.rm=T)))
# points(datesUmbsFaset$doy[coldDry], datesUmbsFaset$flux[coldDry], col="lightblue")
# points(datesUmbsFaset$doy[coldWet], datesUmbsFaset$flux[coldWet], col="blue")
# points(datesUmbsFaset$doy[warmDry], datesUmbsFaset$flux[warmDry], col="red")
# points(datesUmbsFaset$doy[warmWet], datesUmbsFaset$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# legend("bottomleft", legend="Mixed deciduous-pine forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUmbsFaset_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUmbsFaset$doy[neutral], datesUmbsFaset$temp[neutral], col="grey", xlab="DOY", ylab="Soil tempature (°C)", main="UMBS FASET: 2008-17 XX cm", xlim=c(0, 365), ylim=c(min(datesUmbsFaset$temp, na.rm=T),max(datesUmbsFaset$temp, na.rm=T)))
# points(datesUmbsFaset$doy[coldDry], datesUmbsFaset$temp[coldDry], col="lightblue")
# points(datesUmbsFaset$doy[coldWet], datesUmbsFaset$temp[coldWet], col="blue")
# points(datesUmbsFaset$doy[warmDry], datesUmbsFaset$temp[warmDry], col="red")
# points(datesUmbsFaset$doy[warmWet], datesUmbsFaset$temp[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# legend("bottomleft", legend="Mixed deciduous-pine forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUmbsFaset_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUmbsFaset$doy[neutral], datesUmbsFaset$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="UMBS FASET: 2008-17 XX cm", xlim=c(0, 365), ylim=c(0,max(datesUmbsFaset$swc, na.rm=T)))
# points(datesUmbsFaset$doy[coldDry], datesUmbsFaset$swc[coldDry], col="lightblue")
# points(datesUmbsFaset$doy[coldWet], datesUmbsFaset$swc[coldWet], col="blue")
# points(datesUmbsFaset$doy[warmDry], datesUmbsFaset$swc[warmDry], col="red")
# points(datesUmbsFaset$doy[warmWet], datesUmbsFaset$swc[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# legend("bottomleft", legend="Mixed deciduous-pine forest", bty="n")
# dev.off()

# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(umbsFaset$date >= as.Date("1997-11-01")), which(umbsFaset$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(umbsFaset$date >= as.Date("2015-11-01")), which(umbsFaset$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(umbsFaset$date >= as.Date("2023-11-01")), which(umbsFaset$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(umbsFaset$date >= as.Date("1991-11-01")), which(umbsFaset$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(umbsFaset$date >= as.Date("1994-11-01")), which(umbsFaset$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(umbsFaset$date >= as.Date("2002-11-01")), which(umbsFaset$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(umbsFaset$date >= as.Date("2009-11-01")), which(umbsFaset$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(umbsFaset$date >= as.Date("2004-11-01")), which(umbsFaset$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(umbsFaset$date >= as.Date("2006-11-01")), which(umbsFaset$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(umbsFaset$date >= as.Date("2014-11-01")), which(umbsFaset$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(umbsFaset$date >= as.Date("2018-11-01")), which(umbsFaset$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(umbsFaset$date >= as.Date("1990-11-01")), which(umbsFaset$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("1992-11-01")), which(umbsFaset$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("1995-11-01")), which(umbsFaset$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("1998-11-01")), which(umbsFaset$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("2003-11-01")), which(umbsFaset$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("2005-11-01")), which(umbsFaset$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("2007-11-01")), which(umbsFaset$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("2010-11-01")), which(umbsFaset$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("2016-11-01")), which(umbsFaset$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsFaset$date >= as.Date("2019-11-01")), which(umbsFaset$date < as.Date("2023-11-01"))))
# 
# # # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# # umbsFaset$extremeness <- NA
# # for(i in 1:nrow(umbsFaset)){
# #   umbsFaset$extremeness[i] <- oni$extremeness[grep(substr(umbsFaset$date[i], 1, 7), oni$yearMonth)]
# # }
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # plot(umbsFaset$doy, umbsFaset$efflux, col="grey", xlab="DOY", ylab="Soil resp. (µmol C m-2 s-1)", main="UMBS FASET: 2008-17 all control plots", xlim=c(0,365))
# # points(umbsFaset$doy[which(umbsFaset$extremeness == 1)], umbsFaset$efflux[which(umbsFaset$extremeness == 1)], col="lightblue")
# # points(umbsFaset$doy[which(umbsFaset$extremeness == 2)], umbsFaset$efflux[which(umbsFaset$extremeness == 2)], col="blue")
# # points(umbsFaset$doy[which(umbsFaset$extremeness == 3)], umbsFaset$efflux[which(umbsFaset$extremeness == 3)], col="darkblue")
# # legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event during the last winter
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/umbsFaset.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(umbsFaset$doy[noneRows], umbsFaset$efflux[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="UMBS FASET: 2008-17 all control plots", xlim=c(0,365), ylim=c(0,max(umbsFaset$efflux)))
# points(umbsFaset$doy[weakRows], umbsFaset$efflux[weakRows], col="lightblue")
# points(umbsFaset$doy[moderateRows], umbsFaset$efflux[moderateRows], col="blue")
# points(umbsFaset$doy[strongRows], umbsFaset$efflux[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# dev.off()





# Read in UMBS FoRTE data
umbsForte <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/UMBS data/UMBS_Rs/compiled_FoRTE_Rs.csv", stringsAsFactors = F)

# Only keep control plot data
umbsForte <- umbsForte[which(umbsForte$Severity == 0), ]

# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRows <- which(umbsForte$soilCO2Efflux < -1)
suspectRows <- append(suspectRows, which(umbsForte$soilCO2Efflux > 50))
proportionSuspectUmbsForte <- length(suspectRows) / nrow(umbsForte)
if(length(suspectRows) > 0){
  umbsForte <- umbsForte[-suspectRows ,]
}

# Convert date to POSIXct
umbsForte$date <- as.POSIXct(umbsForte$date, format = "%m/%d/%y")

# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesUmbsForte <- aggregate(umbsForte, by=list(substr(umbsForte$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesUmbsForte <- data.frame(unique(substr(umbsForte$date, 1, 10)))
# colnames(datesUmbsForte) <- "date"
# datesUmbsForte$soilCO2Efflux <- NA
# for(i in 1:nrow(datesUmbsForte)){
#   datesUmbsForte$soilCO2Efflux[i] <- mean(umbsForte$soilCO2Efflux[grep(datesUmbsForte$date[i], umbsForte$date)], na.rm=T)
# }

# remove unnecessary data
umbsForte <- NA

# Add a DOY column
datesUmbsForte$doy <- yday(datesUmbsForte$date)

# Add data source
datesUmbsForte$source <- "umbsForte"


# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesUmbsForte$date)
neutral <- append(neutral, grep("1999", datesUmbsForte$date))
neutral <- append(neutral, grep("2002", datesUmbsForte$date))
neutral <- append(neutral, grep("2004", datesUmbsForte$date))
neutral <- append(neutral, grep("2005", datesUmbsForte$date))
neutral <- append(neutral, grep("2011", datesUmbsForte$date))
neutral <- append(neutral, grep("2018", datesUmbsForte$date))

coldWet <- grep("1997", datesUmbsForte$date)
coldWet <- append(coldWet, grep("2008", datesUmbsForte$date))
coldWet <- append(coldWet, grep("2009", datesUmbsForte$date))
coldWet <- append(coldWet, grep("2014", datesUmbsForte$date))
coldWet <- append(coldWet, grep("2019", datesUmbsForte$date))
coldWet <- append(coldWet, grep("2022", datesUmbsForte$date))

coldDry <- grep("1993", datesUmbsForte$date)
coldDry <- append(coldDry, grep("1994", datesUmbsForte$date))
coldDry <- append(coldDry, grep("1996", datesUmbsForte$date))
coldDry <- append(coldDry, grep("2001", datesUmbsForte$date))
coldDry <- append(coldDry, grep("2003", datesUmbsForte$date))
coldDry <- append(coldDry, grep("2015", datesUmbsForte$date))

warmDry <- grep("1992", datesUmbsForte$date)
warmDry <- append(warmDry, grep("1995", datesUmbsForte$date))
warmDry <- append(warmDry, grep("2000", datesUmbsForte$date))
warmDry <- append(warmDry, grep("2010", datesUmbsForte$date))
warmDry <- append(warmDry, grep("2021", datesUmbsForte$date))
warmDry <- append(warmDry, grep("2024", datesUmbsForte$date))

warmWet <- grep("1998", datesUmbsForte$date)
warmWet <- append(warmWet, grep("2006", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2007", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2012", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2013", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2016", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2017", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2020", datesUmbsForte$date))
warmWet <- append(warmWet, grep("2023", datesUmbsForte$date))

# Add preceding winter category
datesUmbsForte$winterType <- NA
datesUmbsForte$winterType[neutral] <- "neutral"
datesUmbsForte$winterType[coldWet] <- "coldWet"
datesUmbsForte$winterType[coldDry] <- "coldDry"
datesUmbsForte$winterType[warmDry] <- "warmDry"
datesUmbsForte$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesUmbsForte)[grep("soilCO2Efflux", colnames(datesUmbsForte))] <- "flux"
colnames(datesUmbsForte)[grep("soilTemp", colnames(datesUmbsForte))] <- "temp"
colnames(datesUmbsForte)[grep("VWC", colnames(datesUmbsForte))] <- "swc"

# Convert soil moisture units from % vol to m3 m-3
datesUmbsForte$swc <- datesUmbsForte$swc/100




# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesUmbsForte$date >= as.Date("1997-11-01")), which(datesUmbsForte$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesUmbsForte$date >= as.Date("2015-11-01")), which(datesUmbsForte$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesUmbsForte$date >= as.Date("2023-11-01")), which(datesUmbsForte$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesUmbsForte$date >= as.Date("1991-11-01")), which(datesUmbsForte$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesUmbsForte$date >= as.Date("1994-11-01")), which(datesUmbsForte$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUmbsForte$date >= as.Date("2002-11-01")), which(datesUmbsForte$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUmbsForte$date >= as.Date("2009-11-01")), which(datesUmbsForte$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesUmbsForte$date >= as.Date("2004-11-01")), which(datesUmbsForte$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesUmbsForte$date >= as.Date("2006-11-01")), which(datesUmbsForte$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUmbsForte$date >= as.Date("2014-11-01")), which(datesUmbsForte$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUmbsForte$date >= as.Date("2018-11-01")), which(datesUmbsForte$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesUmbsForte$date >= as.Date("1990-11-01")), which(datesUmbsForte$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("1992-11-01")), which(datesUmbsForte$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("1995-11-01")), which(datesUmbsForte$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("1998-11-01")), which(datesUmbsForte$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("2003-11-01")), which(datesUmbsForte$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("2005-11-01")), which(datesUmbsForte$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("2007-11-01")), which(datesUmbsForte$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("2010-11-01")), which(datesUmbsForte$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("2016-11-01")), which(datesUmbsForte$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUmbsForte$date >= as.Date("2019-11-01")), which(datesUmbsForte$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUmbsForte.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUmbsForte$doy[neutral], datesUmbsForte$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="UMBS FORTE: 2018-2023 all control plots", xlim=c(0, 365), ylim=c(0,max(datesUmbsForte$flux, na.rm=T)))
# points(datesUmbsForte$doy[coldDry], datesUmbsForte$flux[coldDry], col="lightblue")
# points(datesUmbsForte$doy[coldWet], datesUmbsForte$flux[coldWet], col="blue")
# points(datesUmbsForte$doy[warmDry], datesUmbsForte$flux[warmDry], col="red")
# points(datesUmbsForte$doy[warmWet], datesUmbsForte$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# legend("bottomleft", legend="Mixed deciduous-pine forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUmbsForte_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUmbsForte$doy[neutral], datesUmbsForte$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="UMBS FORTE: 2018-2023 XX cm", xlim=c(0, 365), ylim=c(min(datesUmbsForte$temp, na.rm=T),max(datesUmbsForte$temp, na.rm=T)))
# points(datesUmbsForte$doy[coldDry], datesUmbsForte$temp[coldDry], col="lightblue")
# points(datesUmbsForte$doy[coldWet], datesUmbsForte$temp[coldWet], col="blue")
# points(datesUmbsForte$doy[warmDry], datesUmbsForte$temp[warmDry], col="red")
# points(datesUmbsForte$doy[warmWet], datesUmbsForte$temp[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# legend("bottomleft", legend="Mixed deciduous-pine forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUmbsForte_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUmbsForte$doy[neutral], datesUmbsForte$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="UMBS FORTE: 2018-2023 XX cm", xlim=c(0, 365), ylim=c(0,max(datesUmbsForte$swc, na.rm=T)))
# points(datesUmbsForte$doy[coldDry], datesUmbsForte$swc[coldDry], col="lightblue")
# points(datesUmbsForte$doy[coldWet], datesUmbsForte$swc[coldWet], col="blue")
# points(datesUmbsForte$doy[warmDry], datesUmbsForte$swc[warmDry], col="red")
# points(datesUmbsForte$doy[warmDry], datesUmbsForte$swc[warmDry], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsFaset, digits=2)), bty="n")
# legend("bottomleft", legend="Mixed deciduous-pine forest", bty="n")
# dev.off()

# # Add a DOY column
# umbsForte$doy <- yday(umbsForte$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(umbsForte$date >= as.Date("1997-11-01")), which(umbsForte$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(umbsForte$date >= as.Date("2015-11-01")), which(umbsForte$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(umbsForte$date >= as.Date("2023-11-01")), which(umbsForte$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(umbsForte$date >= as.Date("1991-11-01")), which(umbsForte$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(umbsForte$date >= as.Date("1994-11-01")), which(umbsForte$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(umbsForte$date >= as.Date("2002-11-01")), which(umbsForte$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(umbsForte$date >= as.Date("2009-11-01")), which(umbsForte$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(umbsForte$date >= as.Date("2004-11-01")), which(umbsForte$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(umbsForte$date >= as.Date("2006-11-01")), which(umbsForte$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(umbsForte$date >= as.Date("2014-11-01")), which(umbsForte$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(umbsForte$date >= as.Date("2018-11-01")), which(umbsForte$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(umbsForte$date >= as.Date("1990-11-01")), which(umbsForte$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("1992-11-01")), which(umbsForte$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("1995-11-01")), which(umbsForte$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("1998-11-01")), which(umbsForte$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("2003-11-01")), which(umbsForte$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("2005-11-01")), which(umbsForte$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("2007-11-01")), which(umbsForte$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("2010-11-01")), which(umbsForte$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("2016-11-01")), which(umbsForte$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(umbsForte$date >= as.Date("2019-11-01")), which(umbsForte$date < as.Date("2023-11-01"))))
# 
# 
# # # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# # umbsForte$extremeness <- NA
# # for(i in 1:nrow(umbsForte)){
# #   umbsForte$extremeness[i] <- oni$extremeness[grep(substr(umbsForte$date[i], 1, 7), oni$yearMonth)]
# # }
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # plot(umbsForte$doy, umbsForte$soilCO2Efflux, col="grey", xlab="DOY", ylab="Soil resp. (µmol C m-2 s-1)", main="UMBS FoRTE: 2018-23 all undisturbed plots", xlim=c(0, 365))
# # points(umbsForte$doy[which(umbsForte$extremeness == 1)], umbsForte$soilCO2Efflux[which(umbsForte$extremeness == 1)], col="lightblue")
# # points(umbsForte$doy[which(umbsForte$extremeness == 2)], umbsForte$soilCO2Efflux[which(umbsForte$extremeness == 2)], col="blue")
# # points(umbsForte$doy[which(umbsForte$extremeness == 3)], umbsForte$soilCO2Efflux[which(umbsForte$extremeness == 3)], col="darkblue")
# # legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event the previous winter
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/umbsForte.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(umbsForte$doy[noneRows], umbsForte$soilCO2Efflux[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="UMBS FoRTE: 2018-23 all undisturbed plots", xlim=c(0, 365), ylim=c(0,max(umbsForte$soilCO2Efflux)))
# points(umbsForte$doy[weakRows], umbsForte$soilCO2Efflux[weakRows], col="lightblue")
# points(umbsForte$doy[moderateRows], umbsForte$soilCO2Efflux[moderateRows], col="blue")
# points(umbsForte$doy[strongRows], umbsForte$soilCO2Efflux[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectUmbsForte, digits=2)), bty="n")
# dev.off()







# Read in KBS data
kbs <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/KBS data/knb-lter-kbs.13.46/Trace+gas+on+the+forested+sites.csv", stringsAsFactors = F, skip = 27)

# Only keep control plot data
kbs <- kbs[which(kbs$fertilization_rate == 0), ]

# Convert kg C ha-1 d-1 to umol C m-2 s-1. 86,400 s = 1 d; 10,000 m2 = 1 ha; and 83,333,333 umol C = 1 kg C. Therefore, unit conversion is to multiply by 83,333,333 / (10,000 * 86,400).
kbs$CSR_FLUX_CO2 <- kbs$co2_c * (83333333 / (10000 * 86400))

# Identify different treatment rows
rowsTCF <- grep("TCF", kbs$treatment)
rowsTDF <- grep("TDF", kbs$treatment)
rowsTSF <- grep("TSF", kbs$treatment)

# Create data frames for each forest type
kbsTCF <- kbs[rowsTCF, ]
kbsTDF <- kbs[rowsTDF, ]
kbsTSF <- kbs[rowsTSF, ]

# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRowsTCF <- which(kbsTCF$CSR_FLUX_CO2 < -1)
suspectRowsTCF <- append(suspectRowsTCF, which(kbsTCF$CSR_FLUX_CO2 > 50))
proportionSuspectKbsTCF <- length(suspectRowsTCF) / nrow(kbsTCF)
if(length(suspectRowsTCF) > 0){
  kbsTCF <- kbsTCF[-suspectRowsTCF ,]
}

# Identify different treatment rows
rowsTDF <- grep("TDF", kbsTDF$treatment)
# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRowsTDF <- which(kbsTDF$CSR_FLUX_CO2 < -1)
suspectRowsTDF <- append(suspectRowsTDF, which(kbsTDF$CSR_FLUX_CO2 > 50))
proportionSuspectKbsTDF <- length(suspectRowsTDF) / nrow(kbsTDF)
if(length(suspectRowsTDF) > 0){
  kbsTDF <- kbsTDF[-suspectRowsTDF ,]
}

# Identify different treatment rows
rowsTSF <- grep("TSF", kbsTSF$treatment)
# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRowsTSF <- which(kbsTSF$CSR_FLUX_CO2 < -1)
suspectRowsTSF <- append(suspectRowsTSF, which(kbsTSF$CSR_FLUX_CO2 > 50))
proportionSuspectKbsTSF <- length(suspectRowsTSF) / nrow(kbsTSF)
if(length(suspectRowsTSF) > 0){
  kbsTSF <- kbsTSF[-suspectRowsTSF ,]
}

# Convert date to POSIXct
kbsTCF$date <- as.POSIXct(kbsTCF$sample_date, format = "%m/%d/%Y")
kbsTDF$date <- as.POSIXct(kbsTDF$sample_date, format = "%m/%d/%Y")
kbsTSF$date <- as.POSIXct(kbsTSF$sample_date, format = "%m/%d/%Y")









# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesKbsTCF <- aggregate(kbsTCF, by=list(substr(kbsTCF$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesKbsTCF <- data.frame(unique(substr(kbsTCF$date, 1, 10)))
# colnames(datesKbsTCF) <- "date"
# datesKbsTCF$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesKbsTCF)){
#   datesKbsTCF$CSR_FLUX_CO2[i] <- mean(kbsTCF$CSR_FLUX_CO2[grep(datesKbsTCF$date[i], kbsTCF$date)], na.rm=T)
# }

# remove unnecessary data
kbsTCF <- NA

# Add a DOY column
datesKbsTCF$doy <- yday(datesKbsTCF$date)

# Add data source
datesKbsTCF$source <- "kbsTCF"


# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesKbsTCF$date)
neutral <- append(neutral, grep("1999", datesKbsTCF$date))
neutral <- append(neutral, grep("2002", datesKbsTCF$date))
neutral <- append(neutral, grep("2004", datesKbsTCF$date))
neutral <- append(neutral, grep("2005", datesKbsTCF$date))
neutral <- append(neutral, grep("2011", datesKbsTCF$date))
neutral <- append(neutral, grep("2018", datesKbsTCF$date))

coldWet <- grep("1997", datesKbsTCF$date)
coldWet <- append(coldWet, grep("2008", datesKbsTCF$date))
coldWet <- append(coldWet, grep("2009", datesKbsTCF$date))
coldWet <- append(coldWet, grep("2014", datesKbsTCF$date))
coldWet <- append(coldWet, grep("2019", datesKbsTCF$date))
coldWet <- append(coldWet, grep("2022", datesKbsTCF$date))

coldDry <- grep("1993", datesKbsTCF$date)
coldDry <- append(coldDry, grep("1994", datesKbsTCF$date))
coldDry <- append(coldDry, grep("1996", datesKbsTCF$date))
coldDry <- append(coldDry, grep("2001", datesKbsTCF$date))
coldDry <- append(coldDry, grep("2003", datesKbsTCF$date))
coldDry <- append(coldDry, grep("2015", datesKbsTCF$date))

warmDry <- grep("1992", datesKbsTCF$date)
warmDry <- append(warmDry, grep("1995", datesKbsTCF$date))
warmDry <- append(warmDry, grep("2000", datesKbsTCF$date))
warmDry <- append(warmDry, grep("2010", datesKbsTCF$date))
warmDry <- append(warmDry, grep("2021", datesKbsTCF$date))
warmDry <- append(warmDry, grep("2024", datesKbsTCF$date))

warmWet <- grep("1998", datesKbsTCF$date)
warmWet <- append(warmWet, grep("2006", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2007", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2012", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2013", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2016", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2017", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2020", datesKbsTCF$date))
warmWet <- append(warmWet, grep("2023", datesKbsTCF$date))

# Add preceding winter category
datesKbsTCF$winterType <- NA
datesKbsTCF$winterType[neutral] <- "neutral"
datesKbsTCF$winterType[coldWet] <- "coldWet"
datesKbsTCF$winterType[coldDry] <- "coldDry"
datesKbsTCF$winterType[warmDry] <- "warmDry"
datesKbsTCF$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesKbsTCF)[grep("CSR_FLUX_CO2", colnames(datesKbsTCF))] <- "flux"
colnames(datesKbsTCF)[grep("CSR_T9", colnames(datesKbsTCF))] <- "temp"
colnames(datesKbsTCF)[grep("CSR_SM4", colnames(datesKbsTCF))] <- "swc"



# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesKbsTCF$date >= as.Date("1997-11-01")), which(datesKbsTCF$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesKbsTCF$date >= as.Date("2015-11-01")), which(datesKbsTCF$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesKbsTCF$date >= as.Date("2023-11-01")), which(datesKbsTCF$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesKbsTCF$date >= as.Date("1991-11-01")), which(datesKbsTCF$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTCF$date >= as.Date("1994-11-01")), which(datesKbsTCF$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTCF$date >= as.Date("2002-11-01")), which(datesKbsTCF$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTCF$date >= as.Date("2009-11-01")), which(datesKbsTCF$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesKbsTCF$date >= as.Date("2004-11-01")), which(datesKbsTCF$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesKbsTCF$date >= as.Date("2006-11-01")), which(datesKbsTCF$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesKbsTCF$date >= as.Date("2014-11-01")), which(datesKbsTCF$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesKbsTCF$date >= as.Date("2018-11-01")), which(datesKbsTCF$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesKbsTCF$date >= as.Date("1990-11-01")), which(datesKbsTCF$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("1992-11-01")), which(datesKbsTCF$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("1995-11-01")), which(datesKbsTCF$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("1998-11-01")), which(datesKbsTCF$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("2003-11-01")), which(datesKbsTCF$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("2005-11-01")), which(datesKbsTCF$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("2007-11-01")), which(datesKbsTCF$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("2010-11-01")), which(datesKbsTCF$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("2016-11-01")), which(datesKbsTCF$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTCF$date >= as.Date("2019-11-01")), which(datesKbsTCF$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesKbsTCF.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesKbsTCF$doy[neutral], datesKbsTCF$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="KBS TCF: 1993-1997 all unfertilized plots", xlim=c(0, 365), ylim=c(0,max(datesKbsTCF$flux, na.rm=T)))
# points(datesKbsTCF$doy[coldDry], datesKbsTCF$flux[coldDry], col="lightblue")
# points(datesKbsTCF$doy[coldWet], datesKbsTCF$flux[coldWet], col="blue")
# points(datesKbsTCF$doy[warmDry], datesKbsTCF$flux[warmDry], col="red")
# points(datesKbsTCF$doy[warmWet], datesKbsTCF$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectKbsTCF, digits=2)), bty="n")
# legend("bottomleft", legend="Coniferous forest", bty="n")
# dev.off()








# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesKbsTDF <- aggregate(kbsTDF, by=list(substr(kbsTDF$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesKbsTDF <- data.frame(unique(substr(kbsTDF$date, 1, 10)))
# colnames(datesKbsTDF) <- "date"
# datesKbsTDF$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesKbsTDF)){
#   datesKbsTDF$CSR_FLUX_CO2[i] <- mean(kbsTDF$CSR_FLUX_CO2[grep(datesKbsTDF$date[i], kbsTDF$date)], na.rm=T)
# }

# remove unnecessary data
kbsTDF <- NA

# Add a DOY column
datesKbsTDF$doy <- yday(datesKbsTDF$date)

# Add data source
datesKbsTDF$source <- "kbsTDF"


# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesKbsTDF$date)
neutral <- append(neutral, grep("1999", datesKbsTDF$date))
neutral <- append(neutral, grep("2002", datesKbsTDF$date))
neutral <- append(neutral, grep("2004", datesKbsTDF$date))
neutral <- append(neutral, grep("2005", datesKbsTDF$date))
neutral <- append(neutral, grep("2011", datesKbsTDF$date))
neutral <- append(neutral, grep("2018", datesKbsTDF$date))

coldWet <- grep("1997", datesKbsTDF$date)
coldWet <- append(coldWet, grep("2008", datesKbsTDF$date))
coldWet <- append(coldWet, grep("2009", datesKbsTDF$date))
coldWet <- append(coldWet, grep("2014", datesKbsTDF$date))
coldWet <- append(coldWet, grep("2019", datesKbsTDF$date))
coldWet <- append(coldWet, grep("2022", datesKbsTDF$date))

coldDry <- grep("1993", datesKbsTDF$date)
coldDry <- append(coldDry, grep("1994", datesKbsTDF$date))
coldDry <- append(coldDry, grep("1996", datesKbsTDF$date))
coldDry <- append(coldDry, grep("2001", datesKbsTDF$date))
coldDry <- append(coldDry, grep("2003", datesKbsTDF$date))
coldDry <- append(coldDry, grep("2015", datesKbsTDF$date))

warmDry <- grep("1992", datesKbsTDF$date)
warmDry <- append(warmDry, grep("1995", datesKbsTDF$date))
warmDry <- append(warmDry, grep("2000", datesKbsTDF$date))
warmDry <- append(warmDry, grep("2010", datesKbsTDF$date))
warmDry <- append(warmDry, grep("2021", datesKbsTDF$date))
warmDry <- append(warmDry, grep("2024", datesKbsTDF$date))

warmWet <- grep("1998", datesKbsTDF$date)
warmWet <- append(warmWet, grep("2006", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2007", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2012", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2013", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2016", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2017", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2020", datesKbsTDF$date))
warmWet <- append(warmWet, grep("2023", datesKbsTDF$date))

# Add preceding winter category
datesKbsTDF$winterType <- NA
datesKbsTDF$winterType[neutral] <- "neutral"
datesKbsTDF$winterType[coldWet] <- "coldWet"
datesKbsTDF$winterType[coldDry] <- "coldDry"
datesKbsTDF$winterType[warmDry] <- "warmDry"
datesKbsTDF$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesKbsTDF)[grep("CSR_FLUX_CO2", colnames(datesKbsTDF))] <- "flux"
colnames(datesKbsTDF)[grep("CSR_T9", colnames(datesKbsTDF))] <- "temp"
colnames(datesKbsTDF)[grep("CSR_SM4", colnames(datesKbsTDF))] <- "swc"




# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesKbsTDF$date >= as.Date("1997-11-01")), which(datesKbsTDF$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesKbsTDF$date >= as.Date("2015-11-01")), which(datesKbsTDF$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesKbsTDF$date >= as.Date("2023-11-01")), which(datesKbsTDF$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesKbsTDF$date >= as.Date("1991-11-01")), which(datesKbsTDF$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTDF$date >= as.Date("1994-11-01")), which(datesKbsTDF$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTDF$date >= as.Date("2002-11-01")), which(datesKbsTDF$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTDF$date >= as.Date("2009-11-01")), which(datesKbsTDF$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesKbsTDF$date >= as.Date("2004-11-01")), which(datesKbsTDF$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesKbsTDF$date >= as.Date("2006-11-01")), which(datesKbsTDF$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesKbsTDF$date >= as.Date("2014-11-01")), which(datesKbsTDF$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesKbsTDF$date >= as.Date("2018-11-01")), which(datesKbsTDF$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesKbsTDF$date >= as.Date("1990-11-01")), which(datesKbsTDF$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("1992-11-01")), which(datesKbsTDF$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("1995-11-01")), which(datesKbsTDF$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("1998-11-01")), which(datesKbsTDF$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("2003-11-01")), which(datesKbsTDF$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("2005-11-01")), which(datesKbsTDF$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("2007-11-01")), which(datesKbsTDF$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("2010-11-01")), which(datesKbsTDF$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("2016-11-01")), which(datesKbsTDF$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTDF$date >= as.Date("2019-11-01")), which(datesKbsTDF$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesKbsTDF.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesKbsTDF$doy[neutral], datesKbsTDF$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="KBS TDF: 1993-2007 all unfertilized plots", xlim=c(0, 365), ylim=c(0,max(datesKbsTDF$flux, na.rm=T)))
# points(datesKbsTDF$doy[coldDry], datesKbsTDF$flux[coldDry], col="lightblue")
# points(datesKbsTDF$doy[coldWet], datesKbsTDF$flux[coldWet], col="blue")
# points(datesKbsTDF$doy[warmDry], datesKbsTDF$flux[warmDry], col="red")
# points(datesKbsTDF$doy[warmWet], datesKbsTDF$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectKbsTDF, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()







# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesKbsTSF <- aggregate(kbsTSF, by=list(substr(kbsTSF$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesKbsTSF <- data.frame(unique(substr(kbsTSF$date, 1, 10)))
# colnames(datesKbsTSF) <- "date"
# datesKbsTSF$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesKbsTSF)){
#   datesKbsTSF$CSR_FLUX_CO2[i] <- mean(kbsTSF$CSR_FLUX_CO2[grep(datesKbsTSF$date[i], kbsTSF$date)], na.rm=T)
# }

# remove unnecessary data
kbsTSF <- NA

# Add a DOY column
datesKbsTSF$doy <- yday(datesKbsTSF$date)

# Add data source
datesKbsTSF$source <- "kbsTSF"


# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesKbsTSF$date)
neutral <- append(neutral, grep("1999", datesKbsTSF$date))
neutral <- append(neutral, grep("2002", datesKbsTSF$date))
neutral <- append(neutral, grep("2004", datesKbsTSF$date))
neutral <- append(neutral, grep("2005", datesKbsTSF$date))
neutral <- append(neutral, grep("2011", datesKbsTSF$date))
neutral <- append(neutral, grep("2018", datesKbsTSF$date))

coldWet <- grep("1997", datesKbsTSF$date)
coldWet <- append(coldWet, grep("2008", datesKbsTSF$date))
coldWet <- append(coldWet, grep("2009", datesKbsTSF$date))
coldWet <- append(coldWet, grep("2014", datesKbsTSF$date))
coldWet <- append(coldWet, grep("2019", datesKbsTSF$date))
coldWet <- append(coldWet, grep("2022", datesKbsTSF$date))

coldDry <- grep("1993", datesKbsTSF$date)
coldDry <- append(coldDry, grep("1994", datesKbsTSF$date))
coldDry <- append(coldDry, grep("1996", datesKbsTSF$date))
coldDry <- append(coldDry, grep("2001", datesKbsTSF$date))
coldDry <- append(coldDry, grep("2003", datesKbsTSF$date))
coldDry <- append(coldDry, grep("2015", datesKbsTSF$date))

warmDry <- grep("1992", datesKbsTSF$date)
warmDry <- append(warmDry, grep("1995", datesKbsTSF$date))
warmDry <- append(warmDry, grep("2000", datesKbsTSF$date))
warmDry <- append(warmDry, grep("2010", datesKbsTSF$date))
warmDry <- append(warmDry, grep("2021", datesKbsTSF$date))
warmDry <- append(warmDry, grep("2024", datesKbsTSF$date))

warmWet <- grep("1998", datesKbsTSF$date)
warmWet <- append(warmWet, grep("2006", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2007", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2012", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2013", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2016", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2017", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2020", datesKbsTSF$date))
warmWet <- append(warmWet, grep("2023", datesKbsTSF$date))

# Add preceding winter category
datesKbsTSF$winterType <- NA
datesKbsTSF$winterType[neutral] <- "neutral"
datesKbsTSF$winterType[coldWet] <- "coldWet"
datesKbsTSF$winterType[coldDry] <- "coldDry"
datesKbsTSF$winterType[warmDry] <- "warmDry"
datesKbsTSF$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesKbsTSF)[grep("CSR_FLUX_CO2", colnames(datesKbsTSF))] <- "flux"
colnames(datesKbsTSF)[grep("CSR_T9", colnames(datesKbsTSF))] <- "temp"
colnames(datesKbsTSF)[grep("CSR_SM4", colnames(datesKbsTSF))] <- "swc"




# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesKbsTSF$date >= as.Date("1997-11-01")), which(datesKbsTSF$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesKbsTSF$date >= as.Date("2015-11-01")), which(datesKbsTSF$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesKbsTSF$date >= as.Date("2023-11-01")), which(datesKbsTSF$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesKbsTSF$date >= as.Date("1991-11-01")), which(datesKbsTSF$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTSF$date >= as.Date("1994-11-01")), which(datesKbsTSF$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTSF$date >= as.Date("2002-11-01")), which(datesKbsTSF$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesKbsTSF$date >= as.Date("2009-11-01")), which(datesKbsTSF$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesKbsTSF$date >= as.Date("2004-11-01")), which(datesKbsTSF$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesKbsTSF$date >= as.Date("2006-11-01")), which(datesKbsTSF$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesKbsTSF$date >= as.Date("2014-11-01")), which(datesKbsTSF$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesKbsTSF$date >= as.Date("2018-11-01")), which(datesKbsTSF$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesKbsTSF$date >= as.Date("1990-11-01")), which(datesKbsTSF$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("1992-11-01")), which(datesKbsTSF$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("1995-11-01")), which(datesKbsTSF$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("1998-11-01")), which(datesKbsTSF$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("2003-11-01")), which(datesKbsTSF$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("2005-11-01")), which(datesKbsTSF$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("2007-11-01")), which(datesKbsTSF$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("2010-11-01")), which(datesKbsTSF$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("2016-11-01")), which(datesKbsTSF$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesKbsTSF$date >= as.Date("2019-11-01")), which(datesKbsTSF$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesKbsTSF.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesKbsTSF$doy[neutral], datesKbsTSF$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="KBS TSF: 1993-1997 all unfertilized plots", xlim=c(0, 365), ylim=c(0,max(datesKbsTSF$flux, na.rm=T)))
# points(datesKbsTSF$doy[coldDry], datesKbsTSF$flux[coldDry], col="lightblue")
# points(datesKbsTSF$doy[coldWet], datesKbsTSF$flux[coldWet], col="blue")
# points(datesKbsTSF$doy[warmDry], datesKbsTSF$flux[warmDry], col="red")
# points(datesKbsTSF$doy[warmWet], datesKbsTSF$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectKbsTSF, digits=2)), bty="n")
# legend("bottomleft", legend="Successional forest", bty="n")
# dev.off()



# 
# # Add a DOY column
# kbs$doy <- yday(kbs$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(kbs$date >= as.Date("1997-11-01")), which(kbs$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(kbs$date >= as.Date("2015-11-01")), which(kbs$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(kbs$date >= as.Date("2023-11-01")), which(kbs$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(kbs$date >= as.Date("1991-11-01")), which(kbs$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(kbs$date >= as.Date("1994-11-01")), which(kbs$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(kbs$date >= as.Date("2002-11-01")), which(kbs$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(kbs$date >= as.Date("2009-11-01")), which(kbs$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(kbs$date >= as.Date("2004-11-01")), which(kbs$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(kbs$date >= as.Date("2006-11-01")), which(kbs$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(kbs$date >= as.Date("2014-11-01")), which(kbs$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(kbs$date >= as.Date("2018-11-01")), which(kbs$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(kbs$date >= as.Date("1990-11-01")), which(kbs$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("1992-11-01")), which(kbs$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("1995-11-01")), which(kbs$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("1998-11-01")), which(kbs$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("2003-11-01")), which(kbs$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("2005-11-01")), which(kbs$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("2007-11-01")), which(kbs$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("2010-11-01")), which(kbs$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("2016-11-01")), which(kbs$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(kbs$date >= as.Date("2019-11-01")), which(kbs$date < as.Date("2023-11-01"))))
# 
# # # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# # kbs$extremeness <- NA
# # for(i in 1:nrow(kbs)){
# #   kbs$extremeness[i] <- oni$extremeness[grep(substr(kbs$date[i], 1, 7), oni$yearMonth)]
# # }
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # plot(kbs$doy, kbs$co2_c, col="grey", xlab="DOY", ylab="Soil resp. (kg C ha d-1)", main="KBS: 1993-2007 all unfertilized plots", xlim=c(0, 365))
# # points(kbs$doy[which(kbs$extremeness == 1)], kbs$co2_c[which(kbs$extremeness == 1)], col="lightblue")
# # points(kbs$doy[which(kbs$extremeness == 2)], kbs$co2_c[which(kbs$extremeness == 2)], col="blue")
# # points(kbs$doy[which(kbs$extremeness == 3)], kbs$co2_c[which(kbs$extremeness == 3)], col="darkblue")
# # legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/KBS.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(kbs$doy[noneRows], kbs$CSR_FLUX_CO2[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="KBS: 1993-2007 all unfertilized plots", xlim=c(0, 365), ylim=c(0,max(kbs$CSR_FLUX_CO2, na.rm=T)))
# points(kbs$doy[weakRows], kbs$CSR_FLUX_CO2[weakRows], col="lightblue")
# points(kbs$doy[moderateRows], kbs$CSR_FLUX_CO2[moderateRows], col="blue")
# points(kbs$doy[strongRows], kbs$CSR_FLUX_CO2[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectKbs, digits=2)), bty="n")
# dev.off()
# 








# Read in COSORE DESAI data
desai <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/COSORE data for GLUG/DESAI_data.csv", stringsAsFactors = F)

# Only keep control plot data (plot 4 was trenched)
desai <- desai[which(desai$CSR_PORT < 4), ]

# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRows <- which(desai$CSR_FLUX_CO2 < -1)
suspectRows <- append(suspectRows, which(desai$CSR_FLUX_CO2 > 50))
proportionSuspectDesai <- length(suspectRows) / nrow(desai)
if(length(suspectRows) > 0){
  desai <- desai[-suspectRows ,]
}

# Convert date to POSIXct
desai$date <- as.POSIXct(desai$CSR_TIMESTAMP_BEGIN, format = "%Y-%m-%d %H:%M:%S")

# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesDesai <- aggregate(desai, by=list(substr(desai$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesDesai <- data.frame(unique(substr(desai$date, 1, 10)))
# colnames(datesDesai) <- "date"
# datesDesai$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesDesai)){
#   datesDesai$CSR_FLUX_CO2[i] <- mean(desai$CSR_FLUX_CO2[grep(datesDesai$date[i], desai$date)], na.rm=T)
# }

# remove unnecessary data
desai <- NA

# Add a DOY column
datesDesai$doy <- yday(datesDesai$date)

# Add data source
datesDesai$source <- "desai"

# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesDesai$date)
neutral <- append(neutral, grep("1999", datesDesai$date))
neutral <- append(neutral, grep("2002", datesDesai$date))
neutral <- append(neutral, grep("2004", datesDesai$date))
neutral <- append(neutral, grep("2005", datesDesai$date))
neutral <- append(neutral, grep("2011", datesDesai$date))
neutral <- append(neutral, grep("2018", datesDesai$date))

coldWet <- grep("1997", datesDesai$date)
coldWet <- append(coldWet, grep("2008", datesDesai$date))
coldWet <- append(coldWet, grep("2009", datesDesai$date))
coldWet <- append(coldWet, grep("2014", datesDesai$date))
coldWet <- append(coldWet, grep("2019", datesDesai$date))
coldWet <- append(coldWet, grep("2022", datesDesai$date))

coldDry <- grep("1993", datesDesai$date)
coldDry <- append(coldDry, grep("1994", datesDesai$date))
coldDry <- append(coldDry, grep("1996", datesDesai$date))
coldDry <- append(coldDry, grep("2001", datesDesai$date))
coldDry <- append(coldDry, grep("2003", datesDesai$date))
coldDry <- append(coldDry, grep("2015", datesDesai$date))

warmDry <- grep("1992", datesDesai$date)
warmDry <- append(warmDry, grep("1995", datesDesai$date))
warmDry <- append(warmDry, grep("2000", datesDesai$date))
warmDry <- append(warmDry, grep("2010", datesDesai$date))
warmDry <- append(warmDry, grep("2021", datesDesai$date))
warmDry <- append(warmDry, grep("2024", datesDesai$date))

warmWet <- grep("1998", datesDesai$date)
warmWet <- append(warmWet, grep("2006", datesDesai$date))
warmWet <- append(warmWet, grep("2007", datesDesai$date))
warmWet <- append(warmWet, grep("2012", datesDesai$date))
warmWet <- append(warmWet, grep("2013", datesDesai$date))
warmWet <- append(warmWet, grep("2016", datesDesai$date))
warmWet <- append(warmWet, grep("2017", datesDesai$date))
warmWet <- append(warmWet, grep("2020", datesDesai$date))
warmWet <- append(warmWet, grep("2023", datesDesai$date))

# Add preceding winter category
datesDesai$winterType <- NA
datesDesai$winterType[neutral] <- "neutral"
datesDesai$winterType[coldWet] <- "coldWet"
datesDesai$winterType[coldDry] <- "coldDry"
datesDesai$winterType[warmDry] <- "warmDry"
datesDesai$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesDesai)[grep("CSR_FLUX_CO2", colnames(datesDesai))] <- "flux"
colnames(datesDesai)[grep("CSR_T9", colnames(datesDesai))] <- "temp"
colnames(datesDesai)[grep("CSR_SM4", colnames(datesDesai))] <- "swc"




# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesDesai$date >= as.Date("1997-11-01")), which(datesDesai$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesDesai$date >= as.Date("2015-11-01")), which(datesDesai$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesDesai$date >= as.Date("2023-11-01")), which(datesDesai$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesDesai$date >= as.Date("1991-11-01")), which(datesDesai$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesDesai$date >= as.Date("1994-11-01")), which(datesDesai$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesDesai$date >= as.Date("2002-11-01")), which(datesDesai$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesDesai$date >= as.Date("2009-11-01")), which(datesDesai$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesDesai$date >= as.Date("2004-11-01")), which(datesDesai$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesDesai$date >= as.Date("2006-11-01")), which(datesDesai$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesDesai$date >= as.Date("2014-11-01")), which(datesDesai$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesDesai$date >= as.Date("2018-11-01")), which(datesDesai$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesDesai$date >= as.Date("1990-11-01")), which(datesDesai$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("1992-11-01")), which(datesDesai$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("1995-11-01")), which(datesDesai$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("1998-11-01")), which(datesDesai$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("2003-11-01")), which(datesDesai$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("2005-11-01")), which(datesDesai$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("2007-11-01")), which(datesDesai$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("2010-11-01")), which(datesDesai$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("2016-11-01")), which(datesDesai$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesDesai$date >= as.Date("2019-11-01")), which(datesDesai$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesDesai.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesDesai$doy[neutral], datesDesai$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="DESAI: 2011-2012 all ports", xlim=c(0, 365), ylim=c(0,max(datesDesai$flux, na.rm=T)))
# points(datesDesai$doy[coldDry], datesDesai$flux[coldDry], col="lightblue")
# points(datesDesai$doy[coldWet], datesDesai$flux[coldWet], col="blue")
# points(datesDesai$doy[warmDry], datesDesai$flux[warmDry], col="red")
# points(datesDesai$doy[warmWet], datesDesai$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectDesai, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesDesai_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesDesai$doy[neutral], datesDesai$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="DESAI: 2011-2012 9 cm", xlim=c(0, 365), ylim=c(0,max(datesDesai$temp, na.rm=T)))
# points(datesDesai$doy[coldDry], datesDesai$temp[coldDry], col="lightblue")
# points(datesDesai$doy[coldWet], datesDesai$temp[coldWet], col="blue")
# points(datesDesai$doy[warmDry], datesDesai$temp[warmDry], col="red")
# points(datesDesai$doy[warmWet], datesDesai$temp[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectDesai, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesDesai_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesDesai$doy[neutral], datesDesai$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="DESAI: 2011-2012 4 cm", xlim=c(0, 365), ylim=c(0,max(datesDesai$swc, na.rm=T)))
# points(datesDesai$doy[coldDry], datesDesai$swc[coldDry], col="lightblue")
# points(datesDesai$doy[coldWet], datesDesai$swc[coldWet], col="blue")
# points(datesDesai$doy[warmDry], datesDesai$swc[warmDry], col="red")
# points(datesDesai$doy[warmWet], datesDesai$swc[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectDesai, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()

plot(as.POSIXct(datesDesai$date), datesDesai$CSR_FLUX_CO2, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="DESAI: 2011-2012 all ports", pch=".")

# # Add a DOY column
# desai$doy <- yday(desai$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(desai$date >= as.Date("1997-11-01")), which(desai$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(desai$date >= as.Date("2015-11-01")), which(desai$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(desai$date >= as.Date("2023-11-01")), which(desai$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(desai$date >= as.Date("1991-11-01")), which(desai$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(desai$date >= as.Date("1994-11-01")), which(desai$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(desai$date >= as.Date("2002-11-01")), which(desai$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(desai$date >= as.Date("2009-11-01")), which(desai$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(desai$date >= as.Date("2004-11-01")), which(desai$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(desai$date >= as.Date("2006-11-01")), which(desai$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(desai$date >= as.Date("2014-11-01")), which(desai$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(desai$date >= as.Date("2018-11-01")), which(desai$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(desai$date >= as.Date("1990-11-01")), which(desai$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("1992-11-01")), which(desai$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("1995-11-01")), which(desai$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("1998-11-01")), which(desai$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("2003-11-01")), which(desai$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("2005-11-01")), which(desai$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("2007-11-01")), which(desai$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("2010-11-01")), which(desai$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("2016-11-01")), which(desai$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(desai$date >= as.Date("2019-11-01")), which(desai$date < as.Date("2023-11-01"))))
# 
# # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# desai$extremeness <- NA
# for(i in 1:nrow(desai)){
#   desai$extremeness[i] <- oni$extremeness[grep(substr(desai$date[i], 1, 7), oni$yearMonth)]
# }

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# plot(desai$doy, desai$co2_c, col="grey", xlab="DOY", ylab="Soil resp. (kg C ha d-1)", main="KBS: 1993-2007 all unfertilized plots", xlim=c(0, 365))
# points(desai$doy[which(desai$extremeness == 1)], desai$co2_c[which(desai$extremeness == 1)], col="lightblue")
# points(desai$doy[which(desai$extremeness == 2)], desai$co2_c[which(desai$extremeness == 2)], col="blue")
# points(desai$doy[which(desai$extremeness == 3)], desai$co2_c[which(desai$extremeness == 3)], col="darkblue")
# legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# plot(desai$doy[noneRows], desai$CSR_FLUX_CO2[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="DESAI: 2011-2012 all ports", xlim=c(0, 365), ylim=c(0,max(desai$CSR_FLUX_CO2, na.rm=T)))
# points(desai$doy[weakRows], desai$CSR_FLUX_CO2[weakRows], col="lightblue")
# points(desai$doy[moderateRows], desai$CSR_FLUX_CO2[moderateRows], col="blue")
# points(desai$doy[strongRows], desai$CSR_FLUX_CO2[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# plot(desai$date, desai$CSR_FLUX_CO2, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="DESAI: 2011-2012 all ports", pch=".")








# Read in COSORE ARAIN_TPD data
arainTpd <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/COSORE data for GLUG/ARAIN_TPD_data.csv", stringsAsFactors = F)

# # Only keep control plot data
# kbs <- kbs[which(kbs$fertilization_rate == 0), ]

# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRows <- which(arainTpd$CSR_FLUX_CO2 < -1)
suspectRows <- append(suspectRows, which(arainTpd$CSR_FLUX_CO2 > 50))
proportionSuspectArainTpd <- length(suspectRows) / nrow(arainTpd)
if(length(suspectRows) > 0){
  arainTpd <- arainTpd[-suspectRows ,]
}

# Set soil temperature to NA during 2014 since the sensor appears to have got stuck at ~1 °C
arainTpd$CSR_T5[grep("2014", arainTpd$CSR_TIMESTAMP_BEGIN)] <- NA

# Set all soil moisture data to NA since the median soil moisture is 1 m3 m-3, which would represent being submerged solely in water (i.e., not soil)
arainTpd$CSR_SM5 <- NA

# Convert date to POSIXct
arainTpd$date <- as.POSIXct(arainTpd$CSR_TIMESTAMP_BEGIN, format = "%Y-%m-%d %H:%M:%S")

# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesArainTpd <- aggregate(arainTpd, by=list(substr(arainTpd$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesArainTpd <- data.frame(unique(substr(arainTpd$date, 1, 10)))
# colnames(datesArainTpd) <- "date"
# datesArainTpd$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesArainTpd)){
#   datesArainTpd$CSR_FLUX_CO2[i] <- mean(arainTpd$CSR_FLUX_CO2[grep(datesArainTpd$date[i], arainTpd$date)], na.rm=T)
# }

# remove unnecessary data
arainTpd <- NA

# Add a DOY column
datesArainTpd$doy <- yday(datesArainTpd$date)

# Add data source
datesArainTpd$source <- "arainTpd"

# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesArainTpd$date)
neutral <- append(neutral, grep("1999", datesArainTpd$date))
neutral <- append(neutral, grep("2002", datesArainTpd$date))
neutral <- append(neutral, grep("2004", datesArainTpd$date))
neutral <- append(neutral, grep("2005", datesArainTpd$date))
neutral <- append(neutral, grep("2011", datesArainTpd$date))
neutral <- append(neutral, grep("2018", datesArainTpd$date))

coldWet <- grep("1997", datesArainTpd$date)
coldWet <- append(coldWet, grep("2008", datesArainTpd$date))
coldWet <- append(coldWet, grep("2009", datesArainTpd$date))
coldWet <- append(coldWet, grep("2014", datesArainTpd$date))
coldWet <- append(coldWet, grep("2019", datesArainTpd$date))
coldWet <- append(coldWet, grep("2022", datesArainTpd$date))

coldDry <- grep("1993", datesArainTpd$date)
coldDry <- append(coldDry, grep("1994", datesArainTpd$date))
coldDry <- append(coldDry, grep("1996", datesArainTpd$date))
coldDry <- append(coldDry, grep("2001", datesArainTpd$date))
coldDry <- append(coldDry, grep("2003", datesArainTpd$date))
coldDry <- append(coldDry, grep("2015", datesArainTpd$date))

warmDry <- grep("1992", datesArainTpd$date)
warmDry <- append(warmDry, grep("1995", datesArainTpd$date))
warmDry <- append(warmDry, grep("2000", datesArainTpd$date))
warmDry <- append(warmDry, grep("2010", datesArainTpd$date))
warmDry <- append(warmDry, grep("2021", datesArainTpd$date))
warmDry <- append(warmDry, grep("2024", datesArainTpd$date))

warmWet <- grep("1998", datesArainTpd$date)
warmWet <- append(warmWet, grep("2006", datesArainTpd$date))
warmWet <- append(warmWet, grep("2007", datesArainTpd$date))
warmWet <- append(warmWet, grep("2012", datesArainTpd$date))
warmWet <- append(warmWet, grep("2013", datesArainTpd$date))
warmWet <- append(warmWet, grep("2016", datesArainTpd$date))
warmWet <- append(warmWet, grep("2017", datesArainTpd$date))
warmWet <- append(warmWet, grep("2020", datesArainTpd$date))
warmWet <- append(warmWet, grep("2023", datesArainTpd$date))

# Add preceding winter category
datesArainTpd$winterType <- NA
datesArainTpd$winterType[neutral] <- "neutral"
datesArainTpd$winterType[coldWet] <- "coldWet"
datesArainTpd$winterType[coldDry] <- "coldDry"
datesArainTpd$winterType[warmDry] <- "warmDry"
datesArainTpd$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesArainTpd)[grep("CSR_FLUX_CO2", colnames(datesArainTpd))] <- "flux"
colnames(datesArainTpd)[grep("CSR_T5", colnames(datesArainTpd))] <- "temp"
colnames(datesArainTpd)[grep("CSR_SM5", colnames(datesArainTpd))] <- "swc"




# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesArainTpd$date >= as.Date("1997-11-01")), which(datesArainTpd$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesArainTpd$date >= as.Date("2015-11-01")), which(datesArainTpd$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesArainTpd$date >= as.Date("2023-11-01")), which(datesArainTpd$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesArainTpd$date >= as.Date("1991-11-01")), which(datesArainTpd$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesArainTpd$date >= as.Date("1994-11-01")), which(datesArainTpd$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesArainTpd$date >= as.Date("2002-11-01")), which(datesArainTpd$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesArainTpd$date >= as.Date("2009-11-01")), which(datesArainTpd$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesArainTpd$date >= as.Date("2004-11-01")), which(datesArainTpd$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesArainTpd$date >= as.Date("2006-11-01")), which(datesArainTpd$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesArainTpd$date >= as.Date("2014-11-01")), which(datesArainTpd$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesArainTpd$date >= as.Date("2018-11-01")), which(datesArainTpd$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesArainTpd$date >= as.Date("1990-11-01")), which(datesArainTpd$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("1992-11-01")), which(datesArainTpd$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("1995-11-01")), which(datesArainTpd$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("1998-11-01")), which(datesArainTpd$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("2003-11-01")), which(datesArainTpd$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("2005-11-01")), which(datesArainTpd$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("2007-11-01")), which(datesArainTpd$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("2010-11-01")), which(datesArainTpd$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("2016-11-01")), which(datesArainTpd$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTpd$date >= as.Date("2019-11-01")), which(datesArainTpd$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTpd.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTpd$doy[neutral], datesArainTpd$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TPD: 2014-2019 all ports", xlim=c(0, 365), ylim=c(0,max(datesArainTpd$flux, na.rm=T)))
# points(datesArainTpd$doy[coldDry], datesArainTpd$flux[coldDry], col="lightblue")
# points(datesArainTpd$doy[coldWet], datesArainTpd$flux[coldWet], col="blue")
# points(datesArainTpd$doy[warmDry], datesArainTpd$flux[warmDry], col="red")
# points(datesArainTpd$doy[warmWet], datesArainTpd$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTpd, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTpd_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTpd$doy[neutral], datesArainTpd$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="ARAIN_TPD: 2014-2019 5 cm", xlim=c(0, 365), ylim=c(0,4))
# points(datesArainTpd$doy[coldDry], datesArainTpd$temp[coldDry], col="lightblue")
# points(datesArainTpd$doy[coldWet], datesArainTpd$temp[coldWet], col="blue")
# points(datesArainTpd$doy[warmDry], datesArainTpd$temp[warmDry], col="red")
# points(datesArainTpd$doy[warmWet], datesArainTpd$temp[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTpd, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTpd_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTpd$doy[neutral], datesArainTpd$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="ARAIN_TPD: 2014-2019 5 cm", xlim=c(0, 365), ylim=c(0,0.6))
# points(datesArainTpd$doy[coldDry], datesArainTpd$swc[coldDry], col="lightblue")
# points(datesArainTpd$doy[coldWet], datesArainTpd$swc[coldWet], col="blue")
# points(datesArainTpd$doy[warmDry], datesArainTpd$swc[warmDry], col="red")
# points(datesArainTpd$doy[warmWet], datesArainTpd$swc[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTpd, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()

plot(as.POSIXct(datesArainTpd$date), datesArainTpd$flux, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TPD: 2014-2019 all ports", pch=".")

# 
# # Convert date to POSIXct
# arainTpd$date <- as.POSIXct(arainTpd$CSR_TIMESTAMP_BEGIN, format = "%Y-%m-%d %H:%M:%S")
# 
# # Add a DOY column
# arainTpd$doy <- yday(arainTpd$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(arainTpd$date >= as.Date("1997-11-01")), which(arainTpd$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(arainTpd$date >= as.Date("2015-11-01")), which(arainTpd$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(arainTpd$date >= as.Date("2023-11-01")), which(arainTpd$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(arainTpd$date >= as.Date("1991-11-01")), which(arainTpd$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(arainTpd$date >= as.Date("1994-11-01")), which(arainTpd$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(arainTpd$date >= as.Date("2002-11-01")), which(arainTpd$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(arainTpd$date >= as.Date("2009-11-01")), which(arainTpd$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(arainTpd$date >= as.Date("2004-11-01")), which(arainTpd$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(arainTpd$date >= as.Date("2006-11-01")), which(arainTpd$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(arainTpd$date >= as.Date("2014-11-01")), which(arainTpd$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(arainTpd$date >= as.Date("2018-11-01")), which(arainTpd$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(arainTpd$date >= as.Date("1990-11-01")), which(arainTpd$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("1992-11-01")), which(arainTpd$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("1995-11-01")), which(arainTpd$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("1998-11-01")), which(arainTpd$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("2003-11-01")), which(arainTpd$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("2005-11-01")), which(arainTpd$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("2007-11-01")), which(arainTpd$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("2010-11-01")), which(arainTpd$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("2016-11-01")), which(arainTpd$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTpd$date >= as.Date("2019-11-01")), which(arainTpd$date < as.Date("2023-11-01"))))
# 
# # # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# # arainTpd$extremeness <- NA
# # for(i in 1:nrow(arainTpd)){
# #   arainTpd$extremeness[i] <- oni$extremeness[grep(substr(arainTpd$date[i], 1, 7), oni$yearMonth)]
# # }
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # plot(arainTpd$doy, arainTpd$co2_c, col="grey", xlab="DOY", ylab="Soil resp. (kg C ha d-1)", main="KBS: 1993-2007 all unfertilized plots", xlim=c(0, 365))
# # points(arainTpd$doy[which(arainTpd$extremeness == 1)], arainTpd$co2_c[which(arainTpd$extremeness == 1)], col="lightblue")
# # points(arainTpd$doy[which(arainTpd$extremeness == 2)], arainTpd$co2_c[which(arainTpd$extremeness == 2)], col="blue")
# # points(arainTpd$doy[which(arainTpd$extremeness == 3)], arainTpd$co2_c[which(arainTpd$extremeness == 3)], col="darkblue")
# # legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# plot(arainTpd$doy[noneRows], arainTpd$CSR_FLUX_CO2[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TPD: 2014-2019 all ports", xlim=c(0, 365), ylim=c(0,max(arainTpd$CSR_FLUX_CO2, na.rm=T)))
# points(arainTpd$doy[weakRows], arainTpd$CSR_FLUX_CO2[weakRows], col="lightblue")
# points(arainTpd$doy[moderateRows], arainTpd$CSR_FLUX_CO2[moderateRows], col="blue")
# points(arainTpd$doy[strongRows], arainTpd$CSR_FLUX_CO2[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# plot(arainTpd$date, arainTpd$CSR_FLUX_CO2, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TPD: 2014-2019 all ports", pch=".")









# Read in COSORE ARAIN_TP39 data
arainTp39 <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/COSORE data for GLUG/ARAIN_TP39_data.csv", stringsAsFactors = F)
# arainTp39 <- read.csv("H:/VirtualProfile/Documents/Meetings/2023/GLUG/FollowUpAnalyses/COSORE data for GLUG/ARAIN_TP39_data.csv", stringsAsFactors = F)

# Only keep control plot data (i.e., ports 2, 5, and 7; other ports were trenched, had drought, or no litter)
arainTp39 <- arainTp39[c(which(arainTp39$CSR_PORT == 2), which(arainTp39$CSR_PORT == 5), which(arainTp39$CSR_PORT == 7)), ]

# # Test out only keeping data prior to 2015. After the start of 2015 the soil respiration data became more erratic with both higher and lower rates than in previous years. Also a study by the PI does not include data from 2015 onward: https://www.mdpi.com/1999-4907/11/3/304
# arainTp39 <- arainTp39[which(substr(arainTp39$CSR_TIMESTAMP_BEGIN, 1, 4) < 2015), ]

# Only keeping data prior to March 2012, which is when ~30% of overstory trees were removed: https://www.mdpi.com/1999-4907/11/3/304.
arainTp39 <- arainTp39[which(substr(arainTp39$CSR_TIMESTAMP_BEGIN, 1, 4) < 2012), ]


# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRows <- which(arainTp39$CSR_FLUX_CO2 < -1)
suspectRows <- append(suspectRows, which(arainTp39$CSR_FLUX_CO2 > 50))
proportionSuspectArainTp39 <- length(suspectRows) / nrow(arainTp39)
if(length(suspectRows) > 0){
  arainTp39 <- arainTp39[-suspectRows ,]
}

# Convert date to POSIXct
arainTp39$date <- as.POSIXct(arainTp39$CSR_TIMESTAMP_BEGIN, format = "%Y-%m-%d %H:%M:%S")

# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesArainTp39 <- aggregate(arainTp39, by=list(substr(arainTp39$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesArainTp39 <- data.frame(unique(substr(arainTp39$date, 1, 10)))
# colnames(datesArainTp39) <- "date"
# datesArainTp39$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesArainTp39)){
#   datesArainTp39$CSR_FLUX_CO2[i] <- mean(arainTp39$CSR_FLUX_CO2[grep(datesArainTp39$date[i], arainTp39$date)], na.rm=T)
# }

# remove unnecessary data
arainTp39 <- NA

# Add a DOY column
datesArainTp39$doy <- yday(datesArainTp39$date)

# Add data source
datesArainTp39$source <- "arainTp39"

# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesArainTp39$date)
neutral <- append(neutral, grep("1999", datesArainTp39$date))
neutral <- append(neutral, grep("2002", datesArainTp39$date))
neutral <- append(neutral, grep("2004", datesArainTp39$date))
neutral <- append(neutral, grep("2005", datesArainTp39$date))
neutral <- append(neutral, grep("2011", datesArainTp39$date))
neutral <- append(neutral, grep("2018", datesArainTp39$date))

coldWet <- grep("1997", datesArainTp39$date)
coldWet <- append(coldWet, grep("2008", datesArainTp39$date))
coldWet <- append(coldWet, grep("2009", datesArainTp39$date))
coldWet <- append(coldWet, grep("2014", datesArainTp39$date))
coldWet <- append(coldWet, grep("2019", datesArainTp39$date))
coldWet <- append(coldWet, grep("2022", datesArainTp39$date))

coldDry <- grep("1993", datesArainTp39$date)
coldDry <- append(coldDry, grep("1994", datesArainTp39$date))
coldDry <- append(coldDry, grep("1996", datesArainTp39$date))
coldDry <- append(coldDry, grep("2001", datesArainTp39$date))
coldDry <- append(coldDry, grep("2003", datesArainTp39$date))
coldDry <- append(coldDry, grep("2015", datesArainTp39$date))

warmDry <- grep("1992", datesArainTp39$date)
warmDry <- append(warmDry, grep("1995", datesArainTp39$date))
warmDry <- append(warmDry, grep("2000", datesArainTp39$date))
warmDry <- append(warmDry, grep("2010", datesArainTp39$date))
warmDry <- append(warmDry, grep("2021", datesArainTp39$date))
warmDry <- append(warmDry, grep("2024", datesArainTp39$date))

warmWet <- grep("1998", datesArainTp39$date)
warmWet <- append(warmWet, grep("2006", datesArainTp39$date))
warmWet <- append(warmWet, grep("2007", datesArainTp39$date))
warmWet <- append(warmWet, grep("2012", datesArainTp39$date))
warmWet <- append(warmWet, grep("2013", datesArainTp39$date))
warmWet <- append(warmWet, grep("2016", datesArainTp39$date))
warmWet <- append(warmWet, grep("2017", datesArainTp39$date))
warmWet <- append(warmWet, grep("2020", datesArainTp39$date))
warmWet <- append(warmWet, grep("2023", datesArainTp39$date))

# Add preceding winter category
datesArainTp39$winterType <- NA
datesArainTp39$winterType[neutral] <- "neutral"
datesArainTp39$winterType[coldWet] <- "coldWet"
datesArainTp39$winterType[coldDry] <- "coldDry"
datesArainTp39$winterType[warmDry] <- "warmDry"
datesArainTp39$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesArainTp39)[grep("CSR_FLUX_CO2", colnames(datesArainTp39))] <- "flux"
colnames(datesArainTp39)[grep("CSR_T5", colnames(datesArainTp39))] <- "temp"
colnames(datesArainTp39)[grep("CSR_SM5", colnames(datesArainTp39))] <- "swc"





# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesArainTp39$date >= as.Date("1997-11-01")), which(datesArainTp39$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesArainTp39$date >= as.Date("2015-11-01")), which(datesArainTp39$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesArainTp39$date >= as.Date("2023-11-01")), which(datesArainTp39$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesArainTp39$date >= as.Date("1991-11-01")), which(datesArainTp39$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesArainTp39$date >= as.Date("1994-11-01")), which(datesArainTp39$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesArainTp39$date >= as.Date("2002-11-01")), which(datesArainTp39$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesArainTp39$date >= as.Date("2009-11-01")), which(datesArainTp39$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesArainTp39$date >= as.Date("2004-11-01")), which(datesArainTp39$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesArainTp39$date >= as.Date("2006-11-01")), which(datesArainTp39$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesArainTp39$date >= as.Date("2014-11-01")), which(datesArainTp39$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesArainTp39$date >= as.Date("2018-11-01")), which(datesArainTp39$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesArainTp39$date >= as.Date("1990-11-01")), which(datesArainTp39$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("1992-11-01")), which(datesArainTp39$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("1995-11-01")), which(datesArainTp39$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("1998-11-01")), which(datesArainTp39$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("2003-11-01")), which(datesArainTp39$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("2005-11-01")), which(datesArainTp39$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("2007-11-01")), which(datesArainTp39$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("2010-11-01")), which(datesArainTp39$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("2016-11-01")), which(datesArainTp39$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp39$date >= as.Date("2019-11-01")), which(datesArainTp39$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp39.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTp39$doy[neutral], datesArainTp39$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP39 2008-2011 control plots", xlim=c(0, 365), ylim=c(0,max(datesArainTp39$flux, na.rm=T)))
# points(datesArainTp39$doy[coldDry], datesArainTp39$flux[coldDry], col="lightblue")
# points(datesArainTp39$doy[coldWet], datesArainTp39$flux[coldWet], col="blue")
# points(datesArainTp39$doy[warmDry], datesArainTp39$flux[warmDry], col="red")
# points(datesArainTp39$doy[warmWet], datesArainTp39$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp39, digits=2)), bty="n")
# legend("bottomleft", legend="Evergreen forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp39_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTp39$doy[neutral], datesArainTp39$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="ARAIN_TP39 2008-2011 5 cm", xlim=c(0, 365), ylim=c(0,max(datesArainTp39$temp, na.rm=T)))
# points(datesArainTp39$doy[coldDry], datesArainTp39$temp[coldDry], col="lightblue")
# points(datesArainTp39$doy[coldWet], datesArainTp39$temp[coldWet], col="blue")
# points(datesArainTp39$doy[warmDry], datesArainTp39$temp[warmDry], col="red")
# points(datesArainTp39$doy[warmWet], datesArainTp39$temp[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp39, digits=2)), bty="n")
# legend("bottomleft", legend="Evergreen forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp39_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTp39$doy[neutral], datesArainTp39$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="ARAIN_TP39 2008-2011 5 cm", xlim=c(0, 365), ylim=c(0,max(datesArainTp39$swc, na.rm=T)))
# points(datesArainTp39$doy[coldDry], datesArainTp39$swc[coldDry], col="lightblue")
# points(datesArainTp39$doy[coldWet], datesArainTp39$swc[coldWet], col="blue")
# points(datesArainTp39$doy[warmDry], datesArainTp39$swc[warmDry], col="red")
# points(datesArainTp39$doy[warmWet], datesArainTp39$swc[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp39, digits=2)), bty="n")
# legend("bottomleft", legend="Evergreen forest", bty="n")
# dev.off()

plot(as.POSIXct(datesArainTp39$date), datesArainTp39$flux, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP39 2008-2018 all ports", pch=".")

# plot(as.POSIXct(arainTp39$date[which(arainTp39$CSR_PORT == 2)]), arainTp39$flux[which(arainTp39$CSR_PORT == 2)], col="red", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP39 2008-2018 all ports", pch=".")
# points(as.POSIXct(arainTp39$date[which(arainTp39$CSR_PORT == 5)]), arainTp39$flux[which(arainTp39$CSR_PORT == 5)], col="blue", pch=".")
# points(as.POSIXct(arainTp39$date[which(arainTp39$CSR_PORT == 7)]), arainTp39$flux[which(arainTp39$CSR_PORT == 7)], col="grey", pch=".")
# legend("topleft", legend=c("Port 2", "Port 5", "Port 7"), lty=1, col=c("red", "blue", "gray"), bty="n")

# # Add a DOY column
# arainTp39$doy <- yday(arainTp39$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(arainTp39$date >= as.Date("1997-11-01")), which(arainTp39$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(arainTp39$date >= as.Date("2015-11-01")), which(arainTp39$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(arainTp39$date >= as.Date("2023-11-01")), which(arainTp39$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(arainTp39$date >= as.Date("1991-11-01")), which(arainTp39$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(arainTp39$date >= as.Date("1994-11-01")), which(arainTp39$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(arainTp39$date >= as.Date("2002-11-01")), which(arainTp39$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(arainTp39$date >= as.Date("2009-11-01")), which(arainTp39$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(arainTp39$date >= as.Date("2004-11-01")), which(arainTp39$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(arainTp39$date >= as.Date("2006-11-01")), which(arainTp39$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(arainTp39$date >= as.Date("2014-11-01")), which(arainTp39$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(arainTp39$date >= as.Date("2018-11-01")), which(arainTp39$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(arainTp39$date >= as.Date("1990-11-01")), which(arainTp39$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("1992-11-01")), which(arainTp39$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("1995-11-01")), which(arainTp39$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("1998-11-01")), which(arainTp39$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("2003-11-01")), which(arainTp39$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("2005-11-01")), which(arainTp39$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("2007-11-01")), which(arainTp39$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("2010-11-01")), which(arainTp39$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("2016-11-01")), which(arainTp39$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp39$date >= as.Date("2019-11-01")), which(arainTp39$date < as.Date("2023-11-01"))))
# 
# # # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# # arainTp39$extremeness <- NA
# # for(i in 1:nrow(arainTp39)){
# #   arainTp39$extremeness[i] <- oni$extremeness[grep(substr(arainTp39$date[i], 1, 7), oni$yearMonth)]
# # }
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # plot(arainTp39$doy, arainTp39$co2_c, col="grey", xlab="DOY", ylab="Soil resp. (kg C ha d-1)", main="KBS: 1993-2007 all unfertilized plots", xlim=c(0, 365))
# # points(arainTp39$doy[which(arainTp39$extremeness == 1)], arainTp39$co2_c[which(arainTp39$extremeness == 1)], col="lightblue")
# # points(arainTp39$doy[which(arainTp39$extremeness == 2)], arainTp39$co2_c[which(arainTp39$extremeness == 2)], col="blue")
# # points(arainTp39$doy[which(arainTp39$extremeness == 3)], arainTp39$co2_c[which(arainTp39$extremeness == 3)], col="darkblue")
# # legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# plot(arainTp39$doy[noneRows], arainTp39$CSR_FLUX_CO2[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP39 2008-2018 all ports", xlim=c(0, 365), ylim=c(min(arainTp39$CSR_FLUX_CO2, na.rm=T) ,max(arainTp39$CSR_FLUX_CO2, na.rm=T)))
# points(arainTp39$doy[weakRows], arainTp39$CSR_FLUX_CO2[weakRows], col="lightblue")
# points(arainTp39$doy[moderateRows], arainTp39$CSR_FLUX_CO2[moderateRows], col="blue")
# points(arainTp39$doy[strongRows], arainTp39$CSR_FLUX_CO2[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# 
# 
# plot(arainTp39$date, arainTp39$CSR_FLUX_CO2, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP39 2008-2018 all ports", pch=".")







# Read in COSORE ARAIN_TP74 data
arainTp74 <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/COSORE data for GLUG/ARAIN_TP74_data.csv", stringsAsFactors = F)

# # Only keep control plot data
# kbs <- kbs[which(kbs$fertilization_rate == 0), ]

# Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
suspectRows <- which(arainTp74$CSR_FLUX_CO2 < -1)
suspectRows <- append(suspectRows, which(arainTp74$CSR_FLUX_CO2 > 50))
proportionSuspectArainTp74 <- length(suspectRows) / nrow(arainTp74)
if(length(suspectRows) > 0){
  arainTp74 <- arainTp74[-suspectRows ,]
}

# Change soil moisture values greater than 1 to NA since m3 m-3 units should not exceed 1
arainTp74$CSR_SM5[which(arainTp74$CSR_SM5 > 1)] <- NA

# Convert date to POSIXct
arainTp74$date <- as.POSIXct(arainTp74$CSR_TIMESTAMP_BEGIN, format = "%Y-%m-%d %H:%M:%S")

# Average across all soil respiration data collected on the same date (i.e., across all times and plots)
datesArainTp74 <- aggregate(arainTp74, by=list(substr(arainTp74$date, 1, 10)), FUN=mean, na.rm=T)

# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesArainTp74 <- data.frame(unique(substr(arainTp74$date, 1, 10)))
# colnames(datesArainTp74) <- "date"
# datesArainTp74$CSR_FLUX_CO2 <- NA
# for(i in 1:nrow(datesArainTp74)){
#   datesArainTp74$CSR_FLUX_CO2[i] <- mean(arainTp74$CSR_FLUX_CO2[grep(datesArainTp74$date[i], arainTp74$date)], na.rm=T)
# }

# remove unnecessary data
arainTp74 <- NA

# Add a DOY column
datesArainTp74$doy <- yday(datesArainTp74$date)

# Add data source
datesArainTp74$source <- "arainTp74"

# Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
neutral <- grep("1991", datesArainTp74$date)
neutral <- append(neutral, grep("1999", datesArainTp74$date))
neutral <- append(neutral, grep("2002", datesArainTp74$date))
neutral <- append(neutral, grep("2004", datesArainTp74$date))
neutral <- append(neutral, grep("2005", datesArainTp74$date))
neutral <- append(neutral, grep("2011", datesArainTp74$date))
neutral <- append(neutral, grep("2018", datesArainTp74$date))

coldWet <- grep("1997", datesArainTp74$date)
coldWet <- append(coldWet, grep("2008", datesArainTp74$date))
coldWet <- append(coldWet, grep("2009", datesArainTp74$date))
coldWet <- append(coldWet, grep("2014", datesArainTp74$date))
coldWet <- append(coldWet, grep("2019", datesArainTp74$date))
coldWet <- append(coldWet, grep("2022", datesArainTp74$date))

coldDry <- grep("1993", datesArainTp74$date)
coldDry <- append(coldDry, grep("1994", datesArainTp74$date))
coldDry <- append(coldDry, grep("1996", datesArainTp74$date))
coldDry <- append(coldDry, grep("2001", datesArainTp74$date))
coldDry <- append(coldDry, grep("2003", datesArainTp74$date))
coldDry <- append(coldDry, grep("2015", datesArainTp74$date))

warmDry <- grep("1992", datesArainTp74$date)
warmDry <- append(warmDry, grep("1995", datesArainTp74$date))
warmDry <- append(warmDry, grep("2000", datesArainTp74$date))
warmDry <- append(warmDry, grep("2010", datesArainTp74$date))
warmDry <- append(warmDry, grep("2021", datesArainTp74$date))
warmDry <- append(warmDry, grep("2024", datesArainTp74$date))

warmWet <- grep("1998", datesArainTp74$date)
warmWet <- append(warmWet, grep("2006", datesArainTp74$date))
warmWet <- append(warmWet, grep("2007", datesArainTp74$date))
warmWet <- append(warmWet, grep("2012", datesArainTp74$date))
warmWet <- append(warmWet, grep("2013", datesArainTp74$date))
warmWet <- append(warmWet, grep("2016", datesArainTp74$date))
warmWet <- append(warmWet, grep("2017", datesArainTp74$date))
warmWet <- append(warmWet, grep("2020", datesArainTp74$date))
warmWet <- append(warmWet, grep("2023", datesArainTp74$date))

# Add preceding winter category
datesArainTp74$winterType <- NA
datesArainTp74$winterType[neutral] <- "neutral"
datesArainTp74$winterType[coldWet] <- "coldWet"
datesArainTp74$winterType[coldDry] <- "coldDry"
datesArainTp74$winterType[warmDry] <- "warmDry"
datesArainTp74$winterType[warmWet] <- "warmWet"

# Standardize CO2 flux, soil temperature, and soil moisture column names
colnames(datesArainTp74)[grep("CSR_FLUX_CO2", colnames(datesArainTp74))] <- "flux"
colnames(datesArainTp74)[grep("CSR_T5", colnames(datesArainTp74))] <- "temp"
colnames(datesArainTp74)[grep("CSR_SM5", colnames(datesArainTp74))] <- "swc"


# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesArainTp74$date >= as.Date("1997-11-01")), which(datesArainTp74$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesArainTp74$date >= as.Date("2015-11-01")), which(datesArainTp74$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesArainTp74$date >= as.Date("2023-11-01")), which(datesArainTp74$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesArainTp74$date >= as.Date("1991-11-01")), which(datesArainTp74$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesArainTp74$date >= as.Date("1994-11-01")), which(datesArainTp74$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesArainTp74$date >= as.Date("2002-11-01")), which(datesArainTp74$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesArainTp74$date >= as.Date("2009-11-01")), which(datesArainTp74$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesArainTp74$date >= as.Date("2004-11-01")), which(datesArainTp74$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesArainTp74$date >= as.Date("2006-11-01")), which(datesArainTp74$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesArainTp74$date >= as.Date("2014-11-01")), which(datesArainTp74$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesArainTp74$date >= as.Date("2018-11-01")), which(datesArainTp74$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesArainTp74$date >= as.Date("1990-11-01")), which(datesArainTp74$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("1992-11-01")), which(datesArainTp74$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("1995-11-01")), which(datesArainTp74$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("1998-11-01")), which(datesArainTp74$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("2003-11-01")), which(datesArainTp74$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("2005-11-01")), which(datesArainTp74$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("2007-11-01")), which(datesArainTp74$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("2010-11-01")), which(datesArainTp74$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("2016-11-01")), which(datesArainTp74$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesArainTp74$date >= as.Date("2019-11-01")), which(datesArainTp74$date < as.Date("2023-11-01"))))

# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTp74$doy[neutral], datesArainTp74$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP74 2017-2019 all ports", xlim=c(0, 365), ylim=c(0,max(datesArainTp74$flux, na.rm=T)))
# points(datesArainTp74$doy[coldDry], datesArainTp74$flux[coldDry], col="lightblue")
# points(datesArainTp74$doy[coldWet], datesArainTp74$flux[coldWet], col="blue")
# points(datesArainTp74$doy[warmDry], datesArainTp74$flux[warmDry], col="red")
# points(datesArainTp74$doy[warmWet], datesArainTp74$flux[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Evergreen forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTp74$doy[neutral], datesArainTp74$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="ARAIN_TP74 2017-2019 5 cm", xlim=c(0, 365), ylim=c(0,max(datesArainTp74$temp, na.rm=T)))
# points(datesArainTp74$doy[coldDry], datesArainTp74$temp[coldDry], col="lightblue")
# points(datesArainTp74$doy[coldWet], datesArainTp74$temp[coldWet], col="blue")
# points(datesArainTp74$doy[warmDry], datesArainTp74$temp[warmDry], col="red")
# points(datesArainTp74$doy[warmWet], datesArainTp74$temp[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Evergreen forest", bty="n")
# dev.off()
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesArainTp74$doy[neutral], datesArainTp74$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="ARAIN_TP74 2017-2019 5 cm", xlim=c(0, 365), ylim=c(0,max(datesArainTp74$swc, na.rm=T)))
# points(datesArainTp74$doy[coldDry], datesArainTp74$swc[coldDry], col="lightblue")
# points(datesArainTp74$doy[coldWet], datesArainTp74$swc[coldWet], col="blue")
# points(datesArainTp74$doy[warmDry], datesArainTp74$swc[warmDry], col="red")
# points(datesArainTp74$doy[warmWet], datesArainTp74$swc[warmWet], col="purple")
# legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Evergreen forest", bty="n")
# dev.off()


plot(as.POSIXct(datesArainTp74$date), datesArainTp74$flux, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP74 2017-2019 all ports", pch=".")

# # Add a DOY column
# arainTp74$doy <- yday(arainTp74$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(arainTp74$date >= as.Date("1997-11-01")), which(arainTp74$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(arainTp74$date >= as.Date("2015-11-01")), which(arainTp74$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(arainTp74$date >= as.Date("2023-11-01")), which(arainTp74$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(arainTp74$date >= as.Date("1991-11-01")), which(arainTp74$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(arainTp74$date >= as.Date("1994-11-01")), which(arainTp74$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(arainTp74$date >= as.Date("2002-11-01")), which(arainTp74$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(arainTp74$date >= as.Date("2009-11-01")), which(arainTp74$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(arainTp74$date >= as.Date("2004-11-01")), which(arainTp74$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(arainTp74$date >= as.Date("2006-11-01")), which(arainTp74$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(arainTp74$date >= as.Date("2014-11-01")), which(arainTp74$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(arainTp74$date >= as.Date("2018-11-01")), which(arainTp74$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(arainTp74$date >= as.Date("1990-11-01")), which(arainTp74$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("1992-11-01")), which(arainTp74$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("1995-11-01")), which(arainTp74$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("1998-11-01")), which(arainTp74$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("2003-11-01")), which(arainTp74$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("2005-11-01")), which(arainTp74$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("2007-11-01")), which(arainTp74$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("2010-11-01")), which(arainTp74$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("2016-11-01")), which(arainTp74$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(arainTp74$date >= as.Date("2019-11-01")), which(arainTp74$date < as.Date("2023-11-01"))))
# 
# # # Add extremeness of El Nino event based on NOAA ONI to each soil respiration measurement
# # arainTp74$extremeness <- NA
# # for(i in 1:nrow(arainTp74)){
# #   arainTp74$extremeness[i] <- oni$extremeness[grep(substr(arainTp74$date[i], 1, 7), oni$yearMonth)]
# # }
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # plot(arainTp74$doy, arainTp74$co2_c, col="grey", xlab="DOY", ylab="Soil resp. (kg C ha d-1)", main="KBS: 1993-2007 all unfertilized plots", xlim=c(0, 365))
# # points(arainTp74$doy[which(arainTp74$extremeness == 1)], arainTp74$co2_c[which(arainTp74$extremeness == 1)], col="lightblue")
# # points(arainTp74$doy[which(arainTp74$extremeness == 2)], arainTp74$co2_c[which(arainTp74$extremeness == 2)], col="blue")
# # points(arainTp74$doy[which(arainTp74$extremeness == 3)], arainTp74$co2_c[which(arainTp74$extremeness == 3)], col="darkblue")
# # legend("topleft", legend=c("ONI <0.5", "ONI 0.5-0.9", "ONI 1-1.9", "ONI 2 or above"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# plot(arainTp74$doy[noneRows], arainTp74$CSR_FLUX_CO2[noneRows], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP74 2017-2019 all ports", xlim=c(0, 365), ylim=c(min(arainTp74$CSR_FLUX_CO2, na.rm=T) ,max(arainTp74$CSR_FLUX_CO2, na.rm=T)))
# points(arainTp74$doy[weakRows], arainTp74$CSR_FLUX_CO2[weakRows], col="lightblue")
# points(arainTp74$doy[moderateRows], arainTp74$CSR_FLUX_CO2[moderateRows], col="blue")
# points(arainTp74$doy[strongRows], arainTp74$CSR_FLUX_CO2[strongRows], col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=1)
# 
# 
# plot(arainTp74$date, arainTp74$CSR_FLUX_CO2, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="ARAIN_TP74 2017-2019 all ports", pch=".")



# # Download soil temperature data
# st <- loadByProduct(dpID="DP1.00041.001", include.provisional=T, site="STEI", timeIndex=30, check.size=F)
# # Only keep measurement level 2 (~6 cm)
# st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$verticalPosition == 502), ]
# # Only keep unflagged data
# st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$finalQF == 0), ]
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesSTEI <- aggregate(st$ST_30_minute, by=list(substr(st$ST_30_minute$startDateTime, 1, 10)), FUN=mean, na.rm=T)
# colnames(datesSTEI)[1] <- "date"
# 
# # Add a DOY column
# datesSTEI$doy <- yday(datesSTEI$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesSTEI$date >= as.Date("1997-11-01")), which(datesSTEI$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesSTEI$date >= as.Date("2015-11-01")), which(datesSTEI$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesSTEI$date >= as.Date("2023-11-01")), which(datesSTEI$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesSTEI$date >= as.Date("1991-11-01")), which(datesSTEI$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesSTEI$date >= as.Date("1994-11-01")), which(datesSTEI$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesSTEI$date >= as.Date("2002-11-01")), which(datesSTEI$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesSTEI$date >= as.Date("2009-11-01")), which(datesSTEI$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesSTEI$date >= as.Date("2004-11-01")), which(datesSTEI$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesSTEI$date >= as.Date("2006-11-01")), which(datesSTEI$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesSTEI$date >= as.Date("2014-11-01")), which(datesSTEI$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesSTEI$date >= as.Date("2018-11-01")), which(datesSTEI$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesSTEI$date >= as.Date("1990-11-01")), which(datesSTEI$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("1992-11-01")), which(datesSTEI$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("1995-11-01")), which(datesSTEI$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("1998-11-01")), which(datesSTEI$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("2003-11-01")), which(datesSTEI$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("2005-11-01")), which(datesSTEI$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("2007-11-01")), which(datesSTEI$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("2010-11-01")), which(datesSTEI$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("2016-11-01")), which(datesSTEI$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEI$date >= as.Date("2019-11-01")), which(datesSTEI$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesSTEI_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesSTEI$doy[noneRows], datesSTEI$soilTempMean[noneRows], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="STEI 2017-2024 6 cm", xlim=c(0, 365), ylim=c(min(datesSTEI$soilTempMean, na.rm=T),max(datesSTEI$soilTempMean, na.rm=T)))
# points(datesSTEI$doy[weakRows], datesSTEI$soilTempMean[weakRows], col="lightblue")
# points(datesSTEI$doy[moderateRows], datesSTEI$soilTempMean[moderateRows], col="blue")
# points(datesSTEI$doy[strongRows], datesSTEI$soilTempMean[strongRows], pch=16, col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=c(1,1,1,16))
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()






library(tidyverse)
# # Read in NEON flux data from STEI
# files <- list.files("C:/Users/eayres/OneDrive - National Ecological Observatory Network/ExternalProjects/GLUG Hydrophenology/Data", full.names = T, pattern="STEI")
# 
# # Unnest the flux data
# load(files[1])
# x <- out_fluxes |> unnest(cols=c(flux_compute)) # > select(-diffusivity) 
# # Loop through the remaining files
# for(i in 2:length(files)){
#   # Load the next file
#   load(file = files[i])
#   
#   # Unnest the flux data
#   y <- out_fluxes |> unnest(cols=c(flux_compute)) # > select(-diffusivity) 
#   
#   # Add the data from the new file to the previous data
#   x <- rbind(x,y)
#   
#   y <- NA
#   # y_shallowOnly <- NA
# }
# 
# # # Add a combined QF column
# x$comboQF <- x$soilCO2concentrationMeanQF + x$VSWCMeanQF + x$soilTempMeanQF + x$staPresMeanQF
# 
# p1Rows <- which(x$horizontalPosition == "001")
# dejongRows <- grep("dejong", x$method)
# bestRows <- which(x$comboQF == 0)
# 
# # Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
# suspectRows <- which(x$flux < -1)
# suspectRows <- append(suspectRows, which(x$flux > 50))
# proportionSuspectStei <- length(suspectRows) / nrow(x)
# if(length(suspectRows) > 0){
#   x <- x[-suspectRows ,]
# }
# 
# # Convert date to POSIXct
# x$date <- x$startDateTime
# 
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesStei <- aggregate(x[intersect(intersect(dejongRows, bestRows), p1Rows), ], by=list(substr(x$startDateTime[intersect(intersect(dejongRows, bestRows), p1Rows)], 1, 10)), FUN=mean, na.rm=T)
# 
# # Add a DOY column
# # datesStei$date <- datesStei$startDateTime
# datesStei$doy <- yday(datesStei$date)
# 
# # Add data source
# datesStei$source <- "stei"
# 
# # Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
# neutral <- grep("1991", datesStei$date)
# neutral <- append(neutral, grep("1999", datesStei$date))
# neutral <- append(neutral, grep("2002", datesStei$date))
# neutral <- append(neutral, grep("2004", datesStei$date))
# neutral <- append(neutral, grep("2005", datesStei$date))
# neutral <- append(neutral, grep("2011", datesStei$date))
# neutral <- append(neutral, grep("2018", datesStei$date))
# 
# coldWet <- grep("1997", datesStei$date)
# coldWet <- append(coldWet, grep("2008", datesStei$date))
# coldWet <- append(coldWet, grep("2009", datesStei$date))
# coldWet <- append(coldWet, grep("2014", datesStei$date))
# coldWet <- append(coldWet, grep("2019", datesStei$date))
# coldWet <- append(coldWet, grep("2022", datesStei$date))
# 
# coldDry <- grep("1993", datesStei$date)
# coldDry <- append(coldDry, grep("1994", datesStei$date))
# coldDry <- append(coldDry, grep("1996", datesStei$date))
# coldDry <- append(coldDry, grep("2001", datesStei$date))
# coldDry <- append(coldDry, grep("2003", datesStei$date))
# coldDry <- append(coldDry, grep("2015", datesStei$date))
# 
# warmDry <- grep("1992", datesStei$date)
# warmDry <- append(warmDry, grep("1995", datesStei$date))
# warmDry <- append(warmDry, grep("2000", datesStei$date))
# warmDry <- append(warmDry, grep("2010", datesStei$date))
# warmDry <- append(warmDry, grep("2021", datesStei$date))
# warmDry <- append(warmDry, grep("2024", datesStei$date))
# 
# warmWet <- grep("1998", datesStei$date)
# warmWet <- append(warmWet, grep("2006", datesStei$date))
# warmWet <- append(warmWet, grep("2007", datesStei$date))
# warmWet <- append(warmWet, grep("2012", datesStei$date))
# warmWet <- append(warmWet, grep("2013", datesStei$date))
# warmWet <- append(warmWet, grep("2016", datesStei$date))
# warmWet <- append(warmWet, grep("2017", datesStei$date))
# warmWet <- append(warmWet, grep("2020", datesStei$date))
# warmWet <- append(warmWet, grep("2023", datesStei$date))
# 
# # Add preceding winter category
# datesStei$winterType <- NA
# datesStei$winterType[neutral] <- "neutral"
# datesStei$winterType[coldWet] <- "coldWet"
# datesStei$winterType[coldDry] <- "coldDry"
# datesStei$winterType[warmDry] <- "warmDry"
# datesStei$winterType[warmWet] <- "warmWet"
# 
# # Standardize CO2 flux, soil temperature, and soil moisture column names
# colnames(datesStei)[which(colnames(datesStei) =="soilTempMean")] <- "temp"
# colnames(datesStei)[which(colnames(datesStei) =="VSWCMean")] <- "swc"
# 
# 
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesStei$doy[neutral], datesStei$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="STEI 2017-2024", xlim=c(0, 365), ylim=c(0,max(datesStei$flux, na.rm=T)))
# # points(datesStei$doy[coldDry], datesStei$flux[coldDry], col="lightblue")
# # points(datesStei$doy[coldWet], datesStei$flux[coldWet], col="blue")
# # points(datesStei$doy[warmDry], datesStei$flux[warmDry], col="red")
# # points(datesStei$doy[warmWet], datesStei$flux[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="XXXXXX", bty="n")
# # # dev.off()
# # 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_Temp.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesStei$doy[neutral], datesStei$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="STEI 2017-2024 X cm", xlim=c(0, 365), ylim=c(0,max(datesStei$temp, na.rm=T)))
# # points(datesStei$doy[coldDry], datesStei$temp[coldDry], col="lightblue")
# # points(datesStei$doy[coldWet], datesStei$temp[coldWet], col="blue")
# # points(datesStei$doy[warmDry], datesStei$temp[warmDry], col="red")
# # points(datesStei$doy[warmWet], datesStei$temp[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="Evergreen forest", bty="n")
# # # dev.off()
# # 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_SWC.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesStei$doy[neutral], datesStei$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="STEI 2017-2024 X cm", xlim=c(0, 365), ylim=c(0,max(datesStei$swc, na.rm=T)))
# # points(datesStei$doy[coldDry], datesStei$swc[coldDry], col="lightblue")
# # points(datesStei$doy[coldWet], datesStei$swc[coldWet], col="blue")
# # points(datesStei$doy[warmDry], datesStei$swc[warmDry], col="red")
# # points(datesStei$doy[warmWet], datesStei$swc[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="Evergreen forest", bty="n")
# # # dev.off()
# 
# 
# plot(as.POSIXct(datesStei$Group.1), datesStei$flux, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="STEI 2017-2024", pch=".")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Read in NEON flux data from TREE
# files <- list.files("C:/Users/eayres/OneDrive - National Ecological Observatory Network/ExternalProjects/GLUG Hydrophenology/Data", full.names = T, pattern="TREE")
# 
# # Unnest the flux data
# load(files[1])
# x <- out_fluxes |> unnest(cols=c(flux_compute)) # > select(-diffusivity) 
# # Loop through the remaining files
# for(i in 2:length(files)){
#   # Load the next file
#   load(file = files[i])
#   
#   # Unnest the flux data
#   y <- out_fluxes |> unnest(cols=c(flux_compute)) # > select(-diffusivity) 
#   
#   # Add the data from the new file to the previous data
#   x <- rbind(x,y)
#   
#   y <- NA
#   # y_shallowOnly <- NA
# }
# 
# # # Add a combined QF column
# x$comboQF <- x$soilCO2concentrationMeanQF + x$VSWCMeanQF + x$soilTempMeanQF + x$staPresMeanQF
# 
# p5Rows <- which(x$horizontalPosition == "005")
# dejongRows <- grep("dejong", x$method)
# bestRows <- which(x$comboQF == 0)
# 
# # Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
# suspectRows <- which(x$flux < -1)
# suspectRows <- append(suspectRows, which(x$flux > 50))
# proportionSuspectStei <- length(suspectRows) / nrow(x)
# if(length(suspectRows) > 0){
#   x <- x[-suspectRows ,]
# }
# 
# # Convert date to POSIXct
# x$date <- x$startDateTime
# 
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesTree <- aggregate(x[intersect(intersect(dejongRows, bestRows), p5Rows), ], by=list(substr(x$startDateTime[intersect(intersect(dejongRows, bestRows), p5Rows)], 1, 10)), FUN=mean, na.rm=T)
# 
# # Add a DOY column
# datesTree$doy <- yday(datesTree$date)
# 
# # Add data source
# datesTree$source <- "tree"
# 
# # Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
# neutral <- grep("1991", datesTree$date)
# neutral <- append(neutral, grep("1999", datesTree$date))
# neutral <- append(neutral, grep("2002", datesTree$date))
# neutral <- append(neutral, grep("2004", datesTree$date))
# neutral <- append(neutral, grep("2005", datesTree$date))
# neutral <- append(neutral, grep("2011", datesTree$date))
# neutral <- append(neutral, grep("2018", datesTree$date))
# 
# coldWet <- grep("1997", datesTree$date)
# coldWet <- append(coldWet, grep("2008", datesTree$date))
# coldWet <- append(coldWet, grep("2009", datesTree$date))
# coldWet <- append(coldWet, grep("2014", datesTree$date))
# coldWet <- append(coldWet, grep("2019", datesTree$date))
# coldWet <- append(coldWet, grep("2022", datesTree$date))
# 
# coldDry <- grep("1993", datesTree$date)
# coldDry <- append(coldDry, grep("1994", datesTree$date))
# coldDry <- append(coldDry, grep("1996", datesTree$date))
# coldDry <- append(coldDry, grep("2001", datesTree$date))
# coldDry <- append(coldDry, grep("2003", datesTree$date))
# coldDry <- append(coldDry, grep("2015", datesTree$date))
# 
# warmDry <- grep("1992", datesTree$date)
# warmDry <- append(warmDry, grep("1995", datesTree$date))
# warmDry <- append(warmDry, grep("2000", datesTree$date))
# warmDry <- append(warmDry, grep("2010", datesTree$date))
# warmDry <- append(warmDry, grep("2021", datesTree$date))
# warmDry <- append(warmDry, grep("2024", datesTree$date))
# 
# warmWet <- grep("1998", datesTree$date)
# warmWet <- append(warmWet, grep("2006", datesTree$date))
# warmWet <- append(warmWet, grep("2007", datesTree$date))
# warmWet <- append(warmWet, grep("2012", datesTree$date))
# warmWet <- append(warmWet, grep("2013", datesTree$date))
# warmWet <- append(warmWet, grep("2016", datesTree$date))
# warmWet <- append(warmWet, grep("2017", datesTree$date))
# warmWet <- append(warmWet, grep("2020", datesTree$date))
# warmWet <- append(warmWet, grep("2023", datesTree$date))
# 
# # Add preceding winter category
# datesTree$winterType <- NA
# datesTree$winterType[neutral] <- "neutral"
# datesTree$winterType[coldWet] <- "coldWet"
# datesTree$winterType[coldDry] <- "coldDry"
# datesTree$winterType[warmDry] <- "warmDry"
# datesTree$winterType[warmWet] <- "warmWet"
# 
# # Standardize CO2 flux, soil temperature, and soil moisture column names
# colnames(datesTree)[which(colnames(datesTree) =="soilTempMean")] <- "temp"
# colnames(datesTree)[which(colnames(datesTree) =="VSWCMean")] <- "swc"
# 
# 
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesTree$doy[neutral], datesTree$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="TREE 2017-2024", xlim=c(0, 365), ylim=c(0,max(datesTree$flux, na.rm=T)))
# # points(datesTree$doy[coldDry], datesTree$flux[coldDry], col="lightblue")
# # points(datesTree$doy[coldWet], datesTree$flux[coldWet], col="blue")
# # points(datesTree$doy[warmDry], datesTree$flux[warmDry], col="red")
# # points(datesTree$doy[warmWet], datesTree$flux[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="XXXXXX", bty="n")
# # # dev.off()
# # 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_Temp.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesTree$doy[neutral], datesTree$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="TREE 2017-2024 x cm", xlim=c(0, 365), ylim=c(0,max(datesTree$temp, na.rm=T)))
# # points(datesTree$doy[coldDry], datesTree$temp[coldDry], col="lightblue")
# # points(datesTree$doy[coldWet], datesTree$temp[coldWet], col="blue")
# # points(datesTree$doy[warmDry], datesTree$temp[warmDry], col="red")
# # points(datesTree$doy[warmWet], datesTree$temp[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="Evergreen forest", bty="n")
# # # dev.off()
# # 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_SWC.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesTree$doy[neutral], datesTree$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="TREE 2017-2024 x cm", xlim=c(0, 365), ylim=c(0,max(datesTree$swc, na.rm=T)))
# # points(datesTree$doy[coldDry], datesTree$swc[coldDry], col="lightblue")
# # points(datesTree$doy[coldWet], datesTree$swc[coldWet], col="blue")
# # points(datesTree$doy[warmDry], datesTree$swc[warmDry], col="red")
# # points(datesTree$doy[warmWet], datesTree$swc[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="Evergreen forest", bty="n")
# # # dev.off()
# 
# 
# plot(as.POSIXct(datesTree$Group.1), datesTree$flux, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="TREE 2017-2024", pch=".")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Read in NEON flux data from UNDE
# files <- list.files("C:/Users/eayres/OneDrive - National Ecological Observatory Network/ExternalProjects/GLUG Hydrophenology/Data", full.names = T, pattern="UNDE")
# 
# # Unnest the flux data
# load(files[1])
# x <- out_fluxes |> unnest(cols=c(flux_compute)) # > select(-diffusivity) 
# # Loop through the remaining files
# for(i in 2:length(files)){
#   # Load the next file
#   load(file = files[i])
#   
#   # Unnest the flux data
#   y <- out_fluxes |> unnest(cols=c(flux_compute)) # > select(-diffusivity) 
#   
#   # Add the data from the new file to the previous data
#   x <- rbind(x,y)
#   
#   y <- NA
#   # y_shallowOnly <- NA
# }
# 
# # # Add a combined QF column
# x$comboQF <- x$soilCO2concentrationMeanQF + x$VSWCMeanQF + x$soilTempMeanQF + x$staPresMeanQF
# 
# p4Rows <- which(x$horizontalPosition == "004")
# dejongRows <- grep("dejong", x$method)
# bestRows <- which(x$comboQF == 0)
# 
# # Determine the proportion of values where soil respiration was >50 or <-1 umol CO2 m2 s-1 (QA/QC thresholds used by COSORE in data submission reports) and remove those rows
# suspectRows <- which(x$flux < -1)
# suspectRows <- append(suspectRows, which(x$flux > 50))
# proportionSuspectStei <- length(suspectRows) / nrow(x)
# if(length(suspectRows) > 0){
#   x <- x[-suspectRows ,]
# }
# 
# # Convert date to POSIXct
# x$date <- x$startDateTime
# 
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesUnde <- aggregate(x[intersect(intersect(dejongRows, bestRows), p4Rows), ], by=list(substr(x$startDateTime[intersect(intersect(dejongRows, bestRows), p4Rows)], 1, 10)), FUN=mean, na.rm=T)
# 
# # Add a DOY column
# datesUnde$doy <- yday(datesUnde$date)
# 
# # Add data source
# datesUnde$source <- "unde"
# 
# # Identify dates (rows) that correspond to cold and wet, warm and wet, cold and dry, warm and dry, and neutral winter anomolies based on email from Ankur on 3 Feb 2025
# neutral <- grep("1991", datesUnde$date)
# neutral <- append(neutral, grep("1999", datesUnde$date))
# neutral <- append(neutral, grep("2002", datesUnde$date))
# neutral <- append(neutral, grep("2004", datesUnde$date))
# neutral <- append(neutral, grep("2005", datesUnde$date))
# neutral <- append(neutral, grep("2011", datesUnde$date))
# neutral <- append(neutral, grep("2018", datesUnde$date))
# 
# coldWet <- grep("1997", datesUnde$date)
# coldWet <- append(coldWet, grep("2008", datesUnde$date))
# coldWet <- append(coldWet, grep("2009", datesUnde$date))
# coldWet <- append(coldWet, grep("2014", datesUnde$date))
# coldWet <- append(coldWet, grep("2019", datesUnde$date))
# coldWet <- append(coldWet, grep("2022", datesUnde$date))
# 
# coldDry <- grep("1993", datesUnde$date)
# coldDry <- append(coldDry, grep("1994", datesUnde$date))
# coldDry <- append(coldDry, grep("1996", datesUnde$date))
# coldDry <- append(coldDry, grep("2001", datesUnde$date))
# coldDry <- append(coldDry, grep("2003", datesUnde$date))
# coldDry <- append(coldDry, grep("2015", datesUnde$date))
# 
# warmDry <- grep("1992", datesUnde$date)
# warmDry <- append(warmDry, grep("1995", datesUnde$date))
# warmDry <- append(warmDry, grep("2000", datesUnde$date))
# warmDry <- append(warmDry, grep("2010", datesUnde$date))
# warmDry <- append(warmDry, grep("2021", datesUnde$date))
# warmDry <- append(warmDry, grep("2024", datesUnde$date))
# 
# warmWet <- grep("1998", datesUnde$date)
# warmWet <- append(warmWet, grep("2006", datesUnde$date))
# warmWet <- append(warmWet, grep("2007", datesUnde$date))
# warmWet <- append(warmWet, grep("2012", datesUnde$date))
# warmWet <- append(warmWet, grep("2013", datesUnde$date))
# warmWet <- append(warmWet, grep("2016", datesUnde$date))
# warmWet <- append(warmWet, grep("2017", datesUnde$date))
# warmWet <- append(warmWet, grep("2020", datesUnde$date))
# warmWet <- append(warmWet, grep("2023", datesUnde$date))
# 
# # Add preceding winter category
# datesUnde$winterType <- NA
# datesUnde$winterType[neutral] <- "neutral"
# datesUnde$winterType[coldWet] <- "coldWet"
# datesUnde$winterType[coldDry] <- "coldDry"
# datesUnde$winterType[warmDry] <- "warmDry"
# datesUnde$winterType[warmWet] <- "warmWet"
# 
# # Standardize CO2 flux, soil temperature, and soil moisture column names
# colnames(datesUnde)[which(colnames(datesUnde) =="soilTempMean")] <- "temp"
# colnames(datesUnde)[which(colnames(datesUnde) =="VSWCMean")] <- "swc"
# 
# 
# 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesUnde$doy[neutral], datesUnde$flux[neutral], col="grey", xlab="DOY", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="UNDE 2017-2024", xlim=c(0, 365), ylim=c(0,max(datesUnde$flux, na.rm=T)))
# # points(datesUnde$doy[coldDry], datesUnde$flux[coldDry], col="lightblue")
# # points(datesUnde$doy[coldWet], datesUnde$flux[coldWet], col="blue")
# # points(datesUnde$doy[warmDry], datesUnde$flux[warmDry], col="red")
# # points(datesUnde$doy[warmWet], datesUnde$flux[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="XXXXXX", bty="n")
# # # dev.off()
# # 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_Temp.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesUnde$doy[neutral], datesUnde$temp[neutral], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="UNDE 2017-2024 x cm", xlim=c(0, 365), ylim=c(0,max(datesUnde$temp, na.rm=T)))
# # points(datesUnde$doy[coldDry], datesUnde$temp[coldDry], col="lightblue")
# # points(datesUnde$doy[coldWet], datesUnde$temp[coldWet], col="blue")
# # points(datesUnde$doy[warmDry], datesUnde$temp[warmDry], col="red")
# # points(datesUnde$doy[warmWet], datesUnde$temp[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="Evergreen forest", bty="n")
# # # dev.off()
# # 
# # # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# # # filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesArainTp74_SWC.png"
# # # png(filename=filename, res=300, width = 5, height = 5, units = "in")
# # plot(datesUnde$doy[neutral], datesUnde$swc[neutral], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="UNDE 2017-2024 x cm", xlim=c(0, 365), ylim=c(0,max(datesUnde$swc, na.rm=T)))
# # points(datesUnde$doy[coldDry], datesUnde$swc[coldDry], col="lightblue")
# # points(datesUnde$doy[coldWet], datesUnde$swc[coldWet], col="blue")
# # points(datesUnde$doy[warmDry], datesUnde$swc[warmDry], col="red")
# # points(datesUnde$doy[warmWet], datesUnde$swc[warmWet], col="purple")
# # legend("topleft", legend=c("Neutral", "Cold/dry", "Cold/wet", "Warm/dry", "Warm/wet"), bty="n", col=c("grey", "lightblue", "blue", "red", "purple"), pch=1)
# # # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# # legend("bottomleft", legend="Evergreen forest", bty="n")
# # # dev.off()
# 
# 
# plot(as.POSIXct(datesUnde$Group.1), datesUnde$flux, col="grey", xlab="", ylab="Soil resp. (µmol CO2 m-2 s-1)", main="UNDE 2017-2024", pch=".")
# 










library(neonUtilities)
library(lubridate)

# Read in reprocessed neonSoilFlux data from John
datesNeon <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/midwest_soil_flux.csv", stringsAsFactors = F)

# Exclude data that were not included in the NEON RELEASE-2026 data release since it hasn't been as thoroughly QA/QC'd yet
datesNeon <- datesNeon[-which(datesNeon$date >= "2025-07-01T00:00:00.000z"), ]

# Create a Group.1 column with date in it
datesNeon$Group.1 <- substr(datesNeon$date, 1, 10)

# Rename the soil_resp column flux
colnames(datesNeon)[grep("soil_resp", colnames(datesNeon))[1]] <- "flux"

# Set time to POSIXct
datesNeon$date <- as.POSIXct(datesNeon$date, tz="gmt")
# Add a DOY column
datesNeon$doy <- yday(datesNeon$date)

# Add winterType column
datesNeon$winterType <- NA
datesNeon$winterType[grep("n", datesNeon$classification)] <- "neutral"
datesNeon$winterType[grep("ww", datesNeon$classification)] <- "warmWet"
datesNeon$winterType[grep("cw", datesNeon$classification)] <- "coldWet"
datesNeon$winterType[grep("wd", datesNeon$classification)] <- "warmDry"

# Create columns for soil temperature and soil moisture
datesNeon$temp <- NA
datesNeon$swc <- NA


# Read in NEON calibrations for soil water content correction
neonCal <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Documents 1/GitHub/NEON-Soils-data/CodeToAssignCalCoeffsToSensors/SoilMoistureDepths_FromAs-built_DontOpenInExcel.csv", header=T, stringsAsFactors=F)
# Read in NEON bounded60 calibrations
neonBoulded60Cal <- read.csv("C:/Users/eayres/OneDrive - National Ecological Observatory Network/Documents 1/GitHub/NEON-Soils-data/CodeToAssignCalCoeffsToSensors/Bounded60_SoilMoistureDepths_FromAs-built_DontOpenInExcel.csv", header=T, stringsAsFactors=F)



# Loop through the NEON sites
for(j in c("STEI", "TREE", "UNDE")){
  
  # Download soil temperature data
  site <- j
  
  st <- loadByProduct(dpID="DP1.00041.001", site=site, startdate=min(substr(datesNeon$date, 1, 7)[grep(site, datesNeon$site)]), enddate="2025-06", release="RELEASE-2026", timeIndex=30, check.size=F)
  
  if(site=="STEI"){plot <- "001"}
  if(site=="TREE"){plot <- "005"}
  if(site=="UNDE"){plot <- "004"}
  
  # Only keep rows from the relevant soil plot, measurement level 2 (i.e., ~6 cm deep), and that were unflagged
  st <- st$ST_30_minute[intersect(intersect(which(st$ST_30_minute$horizontalPosition == plot), which(st$ST_30_minute$finalQF == 0)), which(st$ST_30_minute$verticalPosition == 502)), ]
  
  # Calculate mean daily soil temperature
  datesTemp <- aggregate(st, by=list(substr(st$startDateTime, 1, 10)), FUN="mean")
  
  # Identify the rows corresponding to the target site
  theseRows <- grep(site, datesNeon$site)
  
  # Add soil temperature to the flux data
  for(i in theseRows){
    # Skip if the date is not present in the temperature data (e.g., because it was flagged)
    if(length(grep(datesNeon$Group.1[i], datesTemp$Group.1)) > 0){
      datesNeon$temp[i] <- datesTemp$soilTempMean[grep(datesNeon$Group.1[i], datesTemp$Group.1)]
    }
  }
  st <- NA
  
  
  
  # Download soil water content data
  swc <- loadByProduct(dpID="DP1.00094.001", site=site, startdate=min(substr(datesNeon$date, 1, 7)[grep(site, datesNeon$site)]), enddate="2025-06", release="RELEASE-2026", timeIndex=30, check.size=F, package="expanded")
  
  if(site=="STEI"){plot <- "001"}
  if(site=="TREE"){plot <- "005"}
  if(site=="UNDE"){plot <- "004"}
  
  # Only keep rows from the relevant soil plot, measurement level 1 (i.e., ~6 cm deep)
  swc$SWS_30_minute <- swc$SWS_30_minute[intersect(which(swc$SWS_30_minute$horizontalPosition == plot), which(swc$SWS_30_minute$verticalPosition == 501)), ]
  
  
  
  
  
  
  list2env(swc, .GlobalEnv)
  swc <- NA
  
  # Remove ion content columns
  SWS_30_minute <- SWS_30_minute[, -grep("VSIC", colnames(SWS_30_minute))]
  
  # Change dates to posixct
  SWS_30_minute$startDateTime <- as.POSIXct(SWS_30_minute$startDateTime, tz="UTC")
  startYYYYMM <- substr(SWS_30_minute$startDateTime[1], 1, 7)
  

  # Identify rows calculated with the Sentek manufacturer calibration based on calDefaultQM and VSWCExpUncert
  # defaultCalRows <- intersect(which(SWS_30_minute$calDefaultQM > 0), which(SWS_30_minute$VSWCExpUncert > 0.21)) # filtering by which(SWS_30_minute$VSWCExpUncert > 0.21) was causing rows to be missed in recent parts of time series (after ~2020-2021)
  defaultCalRows <- which(SWS_30_minute$calDefaultQM > 0)
  # Add rows where defaults calibration was used (uncertainty > 0.21), but calibration coefficients were not reported in the cal file so the calibration flag did not work correctly
  startDateCalFlagP1 <- SWS_30_minute$startDateTime[intersect(which(SWS_30_minute$horizontalPosition == plot), defaultCalRows)][1]
  defaultCalRows <- append(defaultCalRows, intersect(intersect(which(SWS_30_minute$startDateTime < startDateCalFlagP1), which(SWS_30_minute$horizontalPosition == plot)), which(SWS_30_minute$VSWCExpUncert > 0.21)))

  
  # Calculate sensor scaled frequency for the default calibration rows (based on Sentek Calibration Manual). Multiply water content by 100 to get % vol units
  SWS_30_minute$sf <- NA
  SWS_30_minute$sf[defaultCalRows] <- (0.1957 * ((SWS_30_minute$VSWCMean[defaultCalRows]*100)^0.404)) + 0.02852
  
  
  # Identify rows calculated with the NEON calibration based on calDefaultQM and VSWCExpUncert
  neonCalRowsP1D1 <- intersect(intersect(intersect(which(SWS_30_minute$calNEONQM > 0), which(SWS_30_minute$VSWCExpUncert < 0.21))
                                         , which(SWS_30_minute$horizontalPosition == plot))
                               , which(SWS_30_minute$verticalPosition == "501"))

  
  # Create vector of plots and measurement levels where soil moisture will be recalculated
  neonCals <- ls()[grep("neonCalRowsP", ls())]
  
  
  # Calculate sensor scaled frequency for the neon calibration rows (based on neonCal data frame)
  for(i in 1:length(neonCals)){
    # Find the row in the calibration data frame corresponding to the sensor
    row <- intersect(intersect(grep(site, neonCal$site), which(neonCal$plot == substr(neonCals[i], 13, 13))), which(neonCal$measurementLevel == substr(neonCals[i], 15, 15)) )
    # Assign the calibration coefficients
    a <- neonCal$CVALA1[row]
    b <- neonCal$CVALA2[row]
    c <- neonCal$CVALA3[row]
    # Identify the rows where the calibration coefficients will be applied
    if(neonCals[i] == "neonCalRowsP1D1"){
      rows <- neonCalRowsP1D1
    }else if(neonCals[i] == "neonCalRowsP2D1"){
      rows <- neonCalRowsP2D1
    }else if(neonCals[i] == "neonCalRowsP3D1"){
      rows <- neonCalRowsP3D1
    }else if(neonCals[i] == "neonCalRowsP4D1"){
      rows <- neonCalRowsP4D1
    }else if(neonCals[i] == "neonCalRowsP5D1"){
      rows <- neonCalRowsP5D1
    }

    # Back-calculate scaled frequency using the calibration coefficients
    SWS_30_minute$sf[rows] <- (a * ((SWS_30_minute$VSWCMean[rows]*100)^b)) + c
    
    # prevent carryover to next loop
    row <- NA
    rows <- NA
  }
  
  
  # Calculate soil water content (cm3/cm3) using the neon bounded60 calibration coefficients
  SWS_30_minute$soilSpecVSWCMean <- NA
  for(i in 1:length(neonCals)){
    # Find the row in the calibration data frame corresponding to the sensor
    row <- intersect(intersect(grep(site, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == substr(neonCals[i], 13, 13))), which(neonBoulded60Cal$measurementLevel == substr(neonCals[i], 15, 15)) )
    # Assign the calibration coefficients
    aBounded <- neonBoulded60Cal$CVALA1[row]
    bBounded <- neonBoulded60Cal$CVALA2[row]
    cBounded <- neonBoulded60Cal$CVALA3[row]
    # Identify the rows where the calibration coefficients will be applied
    if(neonCals[i] == "neonCalRowsP1D1"){
      rows <- intersect(which(SWS_30_minute$horizontalPosition == plot), which(SWS_30_minute$verticalPosition == "501"))
    }

    # Apply the calibration
    SWS_30_minute$soilSpecVSWCMean[rows] <- (((SWS_30_minute$sf[rows] - cBounded) / aBounded)^(1/bBounded)) / 100
    
    # prevent carryover to next loop
    row <- NA
    rows <- NA
  }
  
  # Assign soilSpec water content final quality flag based on alpha and beta quality metrics, temp test, and science review flags
  # Use temperature test to inform flagging at all sites for measurement levels 1-3, but for deeper measurement levels only use temperature test at sites where >=35 cm soil temperature falls below 0.1 degrees C (based on 2016-April 2022 RELEASE 2023 soil temperature data)
  coldSites <- c("BARR", "HEAL", "TREE", "TOOL", "DEJU", "NOGP", "NIWO", "WOOD", "BONA", "DCFS", "CPER", "YELL", "RMNP", "MOAB", "STER", "BLAN", "ONAQ")
  if(length(grep(site, coldSites)) == 1){
    # Identify rows with higher alpha metric, but that pass all other tests (beta, temp test, science review)
    goodRows <- intersect(intersect(intersect(intersect(which(SWS_30_minute$VSWCAlphaQM < 30), 
                                                        which(SWS_30_minute$VSWCBetaQM < 20)), 
                                              c(which(SWS_30_minute$VSWCFinalQFSciRvw == 0), 
                                                which(is.na(SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=TRUE))), 
                                    which(SWS_30_minute$tempPassQM > 0)), 
                          which(SWS_30_minute$tempFailQM == 0))
  }else{
    goodRowsDeep <- intersect(intersect(intersect(which(SWS_30_minute$VSWCAlphaQM < 30), 
                                                  which(SWS_30_minute$VSWCBetaQM < 20)), 
                                        c(which(SWS_30_minute$VSWCFinalQFSciRvw == 0), 
                                          which(is.na(SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=T))), 
                              which(SWS_30_minute$verticalPosition >= 504))
    goodRowsShallow <- intersect(intersect(intersect(intersect(intersect(which(SWS_30_minute$VSWCAlphaQM < 30), 
                                                                         which(SWS_30_minute$VSWCBetaQM < 20)), 
                                                               c(which(SWS_30_minute$VSWCFinalQFSciRvw == 0), 
                                                                 which(is.na(SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=T))), 
                                                     which(SWS_30_minute$verticalPosition < 504)), 
                                           which(SWS_30_minute$tempPassQM > 0)), 
                                 which(SWS_30_minute$tempFailQM == 0))
    goodRows <- c(goodRowsShallow, goodRowsDeep)
  }
  SWS_30_minute$correctVSWCFinalQF <- 1
  SWS_30_minute$correctVSWCFinalQF[goodRows] <- 0
  
  # Flag data that was missed by automated QA/QC tests, but appears suspect based on visual inspection. Only TREE and STEI in D05.
  if(length(grep("TREE", SWS_30_minute$siteID[1])) == 1){
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d1rows <- grep("501", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p1rows, d1rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p1rows, d1rows), lowRows)] <- 1
    }
    
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d1rows <- grep("501", SWS_30_minute$verticalPosition)
    rows2019 <- grep("2019", SWS_30_minute$startDateTime)
    rows2020 <- grep("2020", SWS_30_minute$startDateTime)
    yearRows <- c(rows2019, rows2020)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.1)
    if(length(intersect(intersect(p1rows, d5rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(intersect(p1rows, d1rows), yearRows), lowRows)] <- 1
    }
    
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d2rows <- grep("502", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.13)
    if(length(intersect(intersect(p1rows, d2rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p1rows, d2rows), lowRows)] <- 1
    }
    
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d3rows <- grep("503", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.2)
    if(length(intersect(intersect(p1rows, d3rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p1rows, d3rows), lowRows)] <- 1
    }
    
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.01)
    if(length(intersect(intersect(p1rows, d4rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p1rows, d4rows), lowRows)] <- 1
    }
    
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d7rows <- grep("507", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.2)
    if(length(intersect(intersect(p1rows, d7rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p1rows, d7rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d2rows <- grep("502", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p2rows, d2rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d2rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.02)
    if(length(intersect(intersect(p2rows, d4rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d4rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d6rows <- grep("506", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.02)
    if(length(intersect(intersect(p2rows, d6rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d6rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d7rows <- grep("507", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.1)
    if(length(intersect(intersect(p2rows, d7rows), lowRows))){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d7rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d1rows <- grep("501", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d1rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d1rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d2rows <- grep("502", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d2rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d2rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d3rows <- grep("503", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d3rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d3rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d4rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d4rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d5rows <- grep("505", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d5rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d5rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d6rows <- grep("506", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d6rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d6rows), lowRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d7rows <- grep("507", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p3rows, d7rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d7rows), lowRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d1rows <- grep("501", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.02)
    if(length(intersect(intersect(p4rows, d1rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d1rows), lowRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d2rows <- grep("502", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.03)
    if(length(intersect(intersect(p4rows, d2rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d2rows), lowRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d3rows <- grep("503", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.03)
    if(length(intersect(intersect(p4rows, d3rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d3rows), lowRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.02)
    if(length(intersect(intersect(p4rows, d4rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d4rows), lowRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d6rows <- grep("506", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.03)
    if(length(intersect(intersect(p4rows, d6rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d6rows), lowRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d7rows <- grep("507", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p4rows, d7rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d7rows), lowRows)] <- 1
    }
    
    p5rows <- grep(5, SWS_30_minute$horizontalPosition)
    d5rows <- grep("505", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p5rows, d5rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p5rows, d5rows), lowRows)] <- 1
    }
    
    p5rows <- grep(5, SWS_30_minute$horizontalPosition)
    d6rows <- grep("506", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.06)
    if(length(intersect(intersect(p5rows, d6rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p5rows, d6rows), lowRows)] <- 1
    }
    
    p5rows <- grep(5, SWS_30_minute$horizontalPosition)
    d7rows <- grep("507", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p5rows, d7rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p5rows, d7rows), lowRows)] <- 1
    }
  }
  if(length(grep("STEI", SWS_30_minute$siteID[1])) == 1){
    p1rows <- grep(1, SWS_30_minute$horizontalPosition)
    d5rows <- grep("505", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.1)
    if(length(intersect(intersect(p1rows, d5rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p1rows, d5rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.05)
    if(length(intersect(intersect(p2rows, d4rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d4rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d5rows <- grep("505", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.15)
    if(length(intersect(intersect(p2rows, d5rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d5rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d7rows <- grep("507", SWS_30_minute$verticalPosition)
    lowRows <- which(SWS_30_minute$soilSpecVSWCMean < 0.2)
    if(length(intersect(intersect(p2rows, d7rows), lowRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d7rows), lowRows)] <- 1
    }
    
    p2rows <- grep(2, SWS_30_minute$horizontalPosition)
    d8rows <- grep("508", SWS_30_minute$verticalPosition)
    highRows <- which(SWS_30_minute$soilSpecVSWCMean > 0.6)
    if(length(intersect(intersect(p2rows, d8rows), highRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p2rows, d8rows), highRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    highRows <- which(SWS_30_minute$soilSpecVSWCMean > 0.02)
    if(length(intersect(intersect(p3rows, d4rows), highRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d4rows), highRows)] <- 1
    }
    
    p3rows <- grep(3, SWS_30_minute$horizontalPosition)
    d6rows <- grep("506", SWS_30_minute$verticalPosition)
    highRows <- which(SWS_30_minute$soilSpecVSWCMean > 0.2)
    if(length(intersect(intersect(p3rows, d6rows), highRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p3rows, d6rows), highRows)] <- 1
    }
    
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    d5rows <- grep("505", SWS_30_minute$verticalPosition)
    highRows <- which(SWS_30_minute$soilSpecVSWCMean > 0.02)
    if(length(intersect(intersect(p4rows, d5rows), highRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p4rows, d5rows), highRows)] <- 1
    }
    
    p5rows <- grep(5, SWS_30_minute$horizontalPosition)
    d4rows <- grep("504", SWS_30_minute$verticalPosition)
    highRows <- which(SWS_30_minute$soilSpecVSWCMean > 0.05)
    if(length(intersect(intersect(p5rows, d4rows), highRows)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(intersect(p5rows, d4rows), highRows)] <- 1
    }
    
    # Large discontinuity after gap, not consistent with other STEI soil plots
    p4rows <- grep(4, SWS_30_minute$horizontalPosition)
    rows2017 <- grep("2017", SWS_30_minute$startDateTime)
    rows2018 <- grep("2018", SWS_30_minute$startDateTime)
    pre2019 <- append(rows2017, rows2018)
    if(length(intersect(p4rows, pre2019)) > 0){
      SWS_30_minute$correctVSWCFinalQF[intersect(p4rows, pre2019)] <- 1
    }
  }
  
  
  
  # Re-specify goodRows following additional flagging
  goodRows <- which(SWS_30_minute$correctVSWCFinalQF == 0)
  
  
  # Assign measurement uncertainty to unflagged rows. NOTE this does not include changes in water content within the averaging period
  SWS_30_minute$soilSpecVSWCExpUncert <- NA
  goodP1D1 <- intersect(intersect(which(SWS_30_minute$horizontalPosition == plot), which(SWS_30_minute$verticalPosition == "501")), goodRows)
  SWS_30_minute$soilSpecVSWCExpUncert[goodP1D1] <- neonBoulded60Cal$U_CVALA3[intersect(intersect(grep(site, neonBoulded60Cal$site), which(neonBoulded60Cal$plot == 1)), which(neonBoulded60Cal$measurementLevel == 1) )] * 2 # Multiply combined uncertainty by 2 to get expeanded uncertainty (95% confidence)

  # Apply the manufacturer's default calibration (from: "CALIBRATION MANUAL For Sentek Soil Moisture Sensors, Version 2.0") and meansurement uncertainty (From: NEON.DOC.000007 NEON Algorithm Theoretical Basis Document (ATBD): TIS Soil Water Content and Water Salinity) 
  SWS_30_minute$manuVSWCMean <- (((SWS_30_minute$sf - 0.02852) / 0.1957)^(1/0.404)) / 100
  SWS_30_minute$manuVSWCExpUncert <- 0.107*2 # Multiply combined uncertainty by 2 to get expeanded uncertainty (95% confidence)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Calculate mean daily soil water content
  datesSwc <- aggregate(SWS_30_minute, by=list(substr(SWS_30_minute$startDateTime, 1, 10)), FUN="mean")
  
  # Identify the rows corresponding to the target site
  theseRows <- grep(site, datesNeon$site)
  
  # Add soil temperature to the flux data
  for(i in theseRows){
    # Skip if the date is not present in the temperature data (e.g., because it was flagged)
    if(length(grep(datesNeon$Group.1[i], datesSwc$Group.1)) > 0){
      # Skip if soil water content data was flagged
      if(SWS_30_minute$correctVSWCFinalQF[i] == 0){
        datesNeon$swc[i] <- datesSwc$soilSpecVSWCMean[grep(datesNeon$Group.1[i], datesSwc$Group.1)]
      }
    }
  }
}

# Change name of site column to source
colnames(datesNeon)[grep("site", colnames(datesNeon))] <- "source"






plot(datesNeon$date[grep("TREE", datesNeon$site)], datesNeon$flux[grep("TREE", datesNeon$site)], pch=".")
plot(datesNeon$date[grep("TREE", datesNeon$site)], datesNeon$temp[grep("TREE", datesNeon$site)], pch=".")
plot(datesNeon$date[grep("TREE", datesNeon$site)], datesNeon$swc[grep("TREE", datesNeon$site)], pch=".")


























# Create empty columns of soil temperature and moisture for the KBS data
datesKbsTCF$temp <- NA
datesKbsTCF$swc <- NA
datesKbsTDF$temp <- NA
datesKbsTDF$swc <- NA
datesKbsTSF$temp <- NA
datesKbsTSF$swc <- NA

# Combine flux data from all sites
allFlux <- rbind(datesUmbsFaset[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesUmbsForte[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesKbsTCF[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesKbsTDF[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesKbsTSF[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesDesai[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesNeon[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 # datesTree[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 # datesUnde[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesArainTp39[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesArainTp74[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")], 
                 datesArainTpd[, c("Group.1", "doy", "source", "winterType", "flux", "temp", "swc")])

# Remove rows where the flux is missing
allFlux <- allFlux[-is.na(allFlux$flux), ]
allFlux <- allFlux[-which(allFlux$flux=="NaN"), ]

# Calculate sine and cosine of DOY
allFlux$sin_doy <- sin(allFlux$doy * 2 * pi / 365)
allFlux$cos_doy <- cos(allFlux$doy * 2 * pi / 365)

# Add latitude and longitude
allFlux$latitude <- NA
allFlux$longitude <- NA
allFlux$latitude[grep("umbsFaset", allFlux$source)] <- 45.56
allFlux$latitude[grep("umbsForte", allFlux$source)] <- 45.56
allFlux$latitude[grep("kbsTCF", allFlux$source)] <- 42.4
allFlux$latitude[grep("kbsTDF", allFlux$source)] <- 42.4
allFlux$latitude[grep("kbsTSF", allFlux$source)] <- 42.4
allFlux$latitude[grep("desai", allFlux$source)] <- 45.8059
allFlux$latitude[grep("STEI", allFlux$source)] <- 45.50894
allFlux$latitude[grep("TREE", allFlux$source)] <- 45.49369
allFlux$latitude[grep("UNDE", allFlux$source)] <- 46.23391
allFlux$latitude[grep("arainTp39", allFlux$source)] <- 42.710161
allFlux$latitude[grep("arainTp74", allFlux$source)] <- 42.70681111
allFlux$latitude[grep("arainTpd", allFlux$source)] <- 42.635328

allFlux$longitude[grep("umbsFaset", allFlux$source)] <- -84.67
allFlux$longitude[grep("umbsForte", allFlux$source)] <- -84.67
allFlux$longitude[grep("kbsTCF", allFlux$source)] <- -85.4
allFlux$longitude[grep("kbsTDF", allFlux$source)] <- -85.4
allFlux$longitude[grep("kbsTSF", allFlux$source)] <- -85.4
allFlux$longitude[grep("desai", allFlux$source)] <- -90.0799
allFlux$longitude[grep("STEI", allFlux$source)] <- -89.58637
allFlux$longitude[grep("TREE", allFlux$source)] <- -89.58571
allFlux$longitude[grep("UNDE", allFlux$source)] <- -89.537254
allFlux$longitude[grep("arainTp39", allFlux$source)] <- -80.357376
allFlux$longitude[grep("arainTp74", allFlux$source)] <- -80.34831389
allFlux$longitude[grep("arainTpd", allFlux$source)] <- -80.557731


# Add vegetation type
allFlux$vegType <- NA
allFlux$vegType[grep("umbsFaset", allFlux$source)] <- "Mixed deciduous-pine forest"
allFlux$vegType[grep("umbsForte", allFlux$source)] <- "Mixed deciduous-pine forest"
allFlux$vegType[grep("kbsTCF", allFlux$source)] <- "Evergreen forest"
allFlux$vegType[grep("kbsTDF", allFlux$source)] <- "Deciduous forest"
allFlux$vegType[grep("kbsTSF", allFlux$source)] <- "Successional forests"
allFlux$vegType[grep("desai", allFlux$source)] <- "Deciduous forest"
allFlux$vegType[grep("STEI", allFlux$source)] <- "Deciduous forest"
allFlux$vegType[grep("TREE", allFlux$source)] <- "Deciduous forest"
allFlux$vegType[grep("UNDE", allFlux$source)] <- "Deciduous forest"
allFlux$vegType[grep("arainTp39", allFlux$source)] <- "Evergreen forest"
allFlux$vegType[grep("arainTp74", allFlux$source)] <- "Evergreen forest"
allFlux$vegType[grep("arainTpd", allFlux$source)] <- "Deciduous forest"


# reg <- lm(flux ~ sin_doy + cos_doy + winterType + temp + swc + source + longitude + latitude + vegType, data=allFlux)
# summary(reg)

# Create a short name for winter types to better fit on the figures (cd: cold-dry; cw: cold-wet, n: neutral; wd: warm-dry; and ww: warm-wet)
allFlux$winterTypeShort <- as.character(allFlux$winterType)
allFlux$winterTypeShort[grep("coldDry", allFlux$winterType)] <- "cd"
allFlux$winterTypeShort[grep("coldWet", allFlux$winterType)] <- "cw"
allFlux$winterTypeShort[grep("neutral", allFlux$winterType)] <- "n"
allFlux$winterTypeShort[grep("warmDry", allFlux$winterType)] <- "wd"
allFlux$winterTypeShort[grep("warmWet", allFlux$winterType)] <- "ww"

# Set winter type as a factor
allFlux$winterType <- as.factor(allFlux$winterType)
allFlux$winterTypeShort <- as.factor(allFlux$winterTypeShort)
library(emmeans)
library(multcomp)

# Save the data
write.csv(allFlux, "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/allFluxData.csv", row.names = F)



# Create a figure showing impact of different winter types on soil respiration, temperature, and moisture
filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/winterTypeImpact.png"
png(filename=filename, res=200, width = 6, height = 10, units = "in")
par(oma = c(0, 0, 3, 0))
par(mfcol=c(12, 3))
par(mar=c(2, 5, 1.5, 0.1))
# # FLUX
# for(i in 1:length(unique(allFlux$source))){
#   # Identify the row for the site
#   theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
# 
#   # Create an empty plot
#   boxplot(-100, type="n", xlab="", ylab="Flux (µmol m-2 s-1)", xlim=c(0.5, 5.5), ylim=c(0.1, 50), log="y", main=unique(allFlux$source)[i])
#   
#   # Try each site, but keep going if there is no data
#   tryCatch({
#     
#     # Add data to plot
#     boxplot(allFlux$flux[theseRows] ~ allFlux$winterTypeShort[theseRows], add=T)
#     # Create the regression
#     reg <- lm(flux ~ sin_doy + cos_doy + winterTypeShort, data=allFlux[theseRows,])
#     summary(reg)
#     # Perform pairwise comparisons if winter type was significant, P<0.05
#     if(min(summary(reg)$coefficients[grep("winter", rownames(summary(reg)$coefficients)), "Pr(>|t|)"]) < 0.05){
#       print(unique(allFlux$source)[i])
#       print(emmeans(reg, pairwise ~ winterTypeShort))
#       
#       # Add letters to denote significant differences
#       if(unique(allFlux$source)[i] == "stei"){
#         text(c(1.7,3.7,4.7), 30, "a")
#         text(c(2.7), 30, "b")}
#       if(unique(allFlux$source)[i] == "tree"){
#         text(c(1.7), 30, "a")
#         text(c(2.7), 30, "b")
#         text(c(3.7), 30, "c")
#         text(c(4.7), 30, "ac")}
#       if(unique(allFlux$source)[i] == "unde"){
#         text(c(1.7), 30, "a")
#         text(c(2.7), 30, "b")
#         text(c(3.7), 30, "c")
#         text(c(4.7), 30, "d")}
#       if(unique(allFlux$source)[i] == "arainTp39"){
#         text(c(1.7), 30, "a")
#         text(c(2.7), 30, "b")
#         text(c(3.7), 30, "c")}
#       if(unique(allFlux$source)[i] == "arainTpd"){
#         text(c(0.7), 30, "a")
#         text(c(1.7), 30, "b")
#         text(c(2.7), 30, "b")
#         text(c(4.7), 30, "c")}
#       
#     }
# 
#     
#     
#     
#   }, error=function(e){})
# }  
#   
# 
# 
# # TEMP
# for(i in 1:length(unique(allFlux$source))){
#   # Identify the row for the site
#   theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
#   
#   # Create an emtpy plot
#   boxplot(-100, type="n", xlab="", ylab="Temp. (°C)", xlim=c(0.5, 5.5), ylim=c(0, 30), main=unique(allFlux$source)[i])
#   
#   # Add outer margin title
#   if(unique(allFlux$source)[i] == "umbsFaset"){mtext("---------------------------- DOY 0-365 ----------------------------", side=3, line=2)}
#   
#   # Try each site, but keep going if there is no data
#   tryCatch({
#     
#     # Add data to plot
#     boxplot(allFlux$temp[theseRows] ~ allFlux$winterTypeShort[theseRows], add=T)
#     # Create the regression
#     reg <- lm(temp ~ sin_doy + cos_doy + winterTypeShort, data=allFlux[theseRows,])
#     summary(reg)
#     # Perform pairwise comparisons if winter type was significant, P<0.05
#     if(min(summary(reg)$coefficients[grep("winter", rownames(summary(reg)$coefficients)), "Pr(>|t|)"]) < 0.05){
#       print(unique(allFlux$source)[i])
#       print(emmeans(reg, pairwise ~ winterTypeShort))
#       
#       if(unique(allFlux$source)[i] == "desai"){
#         text(c(2.7), 25, "a")
#         text(c(4.7), 25, "b")}
#       if(unique(allFlux$source)[i] == "stei"){
#         text(c(1.7), 25, "a")
#         text(c(2.7), 25, "bc")
#         text(c(3.7), 25, "b")
#         text(c(4.7), 25, "c")}
#       if(unique(allFlux$source)[i] == "tree"){
#         text(c(1.7), 25, "a")
#         text(c(2.7), 25, "b")
#         text(c(3.7), 25, "c")
#         text(c(4.7), 25, "b")}
#       if(unique(allFlux$source)[i] == "unde"){
#         text(c(1.7), 25, "a")
#         text(c(2.7), 25, "bc")
#         text(c(3.7), 25, "b")
#         text(c(4.7), 25, "ac")}
#       if(unique(allFlux$source)[i] == "arainTp39"){
#         text(c(1.7), 25, "a")
#         text(c(2.7), 25, "b")
#         text(c(3.7), 25, "c")}
#       if(unique(allFlux$source)[i] == "arainTpd"){
#         text(c(0.7), 25, "a")
#         text(c(1.7), 25, "b")
#         text(c(2.7), 25, "c")
#         text(c(4.7), 25, "d")}
#       
#     }
#   }, error=function(e){})
# }  
# 
# 
# # SWC
# for(i in 1:length(unique(allFlux$source))){
#   # Identify the row for the site
#   theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
#   
#   # Create an emtpy plot
#   boxplot(-100, type="n", xlab="", ylab="SWC (m3/m3)", xlim=c(0.5, 5.5), ylim=c(0, 0.6), main=unique(allFlux$source)[i])
#   
#   # Try each site, but keep going if there is no data
#   tryCatch({
#     
#     # Add data to plot
#     boxplot(allFlux$swc[theseRows] ~ allFlux$winterTypeShort[theseRows], add=T)
#     # Create the regression
#     reg <- lm(swc ~ sin_doy + cos_doy + winterTypeShort, data=allFlux[theseRows,])
#     summary(reg)
#     # Perform pairwise comparisons if winter type was significant, P<0.05
#     if(min(summary(reg)$coefficients[grep("winter", rownames(summary(reg)$coefficients)), "Pr(>|t|)"]) < 0.05){
#       print(unique(allFlux$source)[i])
#       print(emmeans(reg, pairwise ~ winterTypeShort))
#       
#       if(unique(allFlux$source)[i] == "umbsForte"){
#         text(c(1.7), 0.5, "abc")
#         text(c(2.7), 0.5, "ac")
#         text(c(3.7), 0.5, "b")
#         text(c(4.7), 0.5, "c")}
#       if(unique(allFlux$source)[i] == "desai"){
#         text(c(2.7), 0.5, "a")
#         text(c(4.7), 0.5, "b")}
#       if(unique(allFlux$source)[i] == "stei"){
#         text(c(1.7), 0.5, "ab")
#         text(c(2.7), 0.5, "c")
#         text(c(3.7), 0.5, "bd")
#         text(c(4.7), 0.5, "d")}
#       if(unique(allFlux$source)[i] == "tree"){
#         text(c(1.7), 0.5, "a")
#         text(c(2.7), 0.5, "b")
#         text(c(3.7), 0.5, "c")
#         text(c(4.7), 0.5, "d")}
#       if(unique(allFlux$source)[i] == "unde"){
#         text(c(1.7), 0.5, "a")
#         text(c(2.7), 0.5, "b")
#         text(c(3.7), 0.5, "a")
#         text(c(4.7), 0.5, "c")}
#       if(unique(allFlux$source)[i] == "arainTp39"){
#         text(c(1.7), 0.5, "a")
#         text(c(2.7), 0.5, "b")
#         text(c(3.7), 0.5, "b")}
# 
#     }
#   }, error=function(e){})
# }  







# Define early season rows
doyEarlySeasonrows <- intersect(which(allFlux$doy > 80), which(allFlux$doy <= 140))

# FLUX, early season
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- intersect(grep(unique(allFlux$source)[i], allFlux$source), doyEarlySeasonrows)
  
  # Create an emtpy plot
  boxplot(-100, type="n", xlab="", ylab="Flux \n(µmol m-2 s-1)", xlim=c(0.5, 5.5), ylim=c(0.1, 50), log="y", main=unique(allFlux$source)[i])
  
  # Try each site, but keep going if there is no data
  tryCatch({
    
    boxplot(allFlux$flux[theseRows] ~ allFlux$winterTypeShort[theseRows], add=T)
    # Perform pairwise comparisons if winter type was significant, P<0.05
    # Create the regression
    reg <- lm(flux ~ sin_doy + cos_doy + winterTypeShort, data=allFlux[theseRows,])
    summary(reg)
    # Add data to plot
    if(min(summary(reg)$coefficients[grep("winter", rownames(summary(reg)$coefficients)), "Pr(>|t|)"]) < 0.05){
      print(unique(allFlux$source)[i])
      print(emmeans(reg, pairwise ~ winterTypeShort))
      
      # Add letters to denote significant differences
      if(unique(allFlux$source)[i] == "STEI"){
        text(c(1.7), 30, "a")
        text(c(2.7), 30, "b")
        text(c(3.7), 30, "c")
        text(c(4.7), 30, "c")}
      if(unique(allFlux$source)[i] == "TREE"){
        text(c(1.7), 30, "a")
        text(c(2.7), 30, "b")
        text(c(3.7), 30, "c")
        text(c(4.7), 30, "c")}
      if(unique(allFlux$source)[i] == "UNDE"){
        text(c(1.7), 30, "a")
        text(c(2.7), 30, "b")
        text(c(3.7), 30, "c")
        text(c(4.7), 30, "ac")}
      if(unique(allFlux$source)[i] == "arainTp39"){
        text(c(1.7), 30, "a")
        text(c(2.7), 30, "b")
        text(c(3.7), 30, "c")}
      if(unique(allFlux$source)[i] == "arainTpd"){
        text(c(0.7), 30, "a")
        text(c(1.7), 30, "b")
        text(c(2.7), 30, "a")
        text(c(4.7), 30, "c")}
      
    }
    
  }, error=function(e){})
}  


# TEMP, early season
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- intersect(grep(unique(allFlux$source)[i], allFlux$source), doyEarlySeasonrows)
  
  # Create an emtpy plot
  boxplot(-100, type="n", xlab="", ylab="Temp. (°C)", xlim=c(0.5, 5.5), ylim=c(0, 20), main=unique(allFlux$source)[i])
  
  # Add outer margin title
  if(unique(allFlux$source)[i] == "umbsFaset"){mtext("---------------------------- DOY: 80-140 ----------------------------", side=3, line=2)}

  # Try each site, but keep going if there is no data
  tryCatch({
    
    # Add data to plot
    boxplot(allFlux$temp[theseRows] ~ allFlux$winterTypeShort[theseRows], add=T)
    # Create the regression
    reg <- lm(temp ~ sin_doy + cos_doy + winterTypeShort, data=allFlux[theseRows,])
    summary(reg)
    # Perform pairwise comparisons if winter type was significant, P<0.05
    if(min(summary(reg)$coefficients[grep("winter", rownames(summary(reg)$coefficients)), "Pr(>|t|)"]) < 0.05){
      print(unique(allFlux$source)[i])
      print(emmeans(reg, pairwise ~ winterTypeShort))
      
      if(unique(allFlux$source)[i] == "STEI"){
        text(c(1.7), 18, "a")
        text(c(2.7), 18, "b")
        text(c(3.7), 18, "c")
        text(c(4.7), 18, "c")}
      if(unique(allFlux$source)[i] == "TREE"){
        text(c(1.7), 18, "a")
        text(c(2.7), 18, "b")
        text(c(3.7), 18, "c")
        text(c(4.7), 18, "b")}
      if(unique(allFlux$source)[i] == "UNDE"){
        text(c(1.7), 18, "a")
        text(c(2.7), 18, "a")
        text(c(3.7), 18, "b")
        text(c(4.7), 18, "a")}
      if(unique(allFlux$source)[i] == "arainTp39"){
        text(c(1.7), 18, "a")
        text(c(2.7), 18, "b")
        text(c(3.7), 18, "c")}
      if(unique(allFlux$source)[i] == "arainTpd"){
        text(c(0.7), 18, "a")
        text(c(1.7), 18, "a")
        text(c(2.7), 18, "b")
        text(c(4.7), 18, "b")}
      
    }
  }, error=function(e){})
}  


# SWC, early season
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- intersect(grep(unique(allFlux$source)[i], allFlux$source), doyEarlySeasonrows)
  
  
  
  # Create an emtpy plot
  boxplot(-100, type="n", xlab="", ylab="SWC (m3/m3)", xlim=c(0.5, 5.5), ylim=c(0, 0.6), main=unique(allFlux$source)[i])
  
  # Try each site, but keep going if there is no data
  tryCatch({
    
    # Add data to plot
    boxplot(allFlux$swc[theseRows] ~ allFlux$winterTypeShort[theseRows], add=T)
    # Create the regression
    reg <- lm(swc ~ sin_doy + cos_doy + winterTypeShort, data=allFlux[theseRows,])
    summary(reg)
    # Perform pairwise comparisons if winter type was significant, P<0.05
    if(min(summary(reg)$coefficients[grep("winter", rownames(summary(reg)$coefficients)), "Pr(>|t|)"]) < 0.05){
      print(unique(allFlux$source)[i])
      print(emmeans(reg, pairwise ~ winterTypeShort))
      
      if(unique(allFlux$source)[i] == "STEI"){
        text(c(1.7), 0.5, "a")
        text(c(2.7), 0.5, "a")
        text(c(3.7), 0.5, "b")
        text(c(4.7), 0.5, "a")}
      if(unique(allFlux$source)[i] == "TREE"){
        text(c(1.7), 0.5, "a")
        text(c(2.7), 0.5, "b")
        text(c(3.7), 0.5, "b")
        text(c(4.7), 0.5, "a")}
      if(unique(allFlux$source)[i] == "UNDE"){
        text(c(1.7), 0.5, "a")
        text(c(2.7), 0.5, "b")
        text(c(3.7), 0.5, "c")
        text(c(4.7), 0.5, "b")}
      if(unique(allFlux$source)[i] == "arainTp39"){
        text(c(1.7), 0.5, "a")
        text(c(2.7), 0.5, "ab")
        text(c(3.7), 0.5, "b")}
      
    }
  }, error=function(e){})
}  
dev.off()







# Temp, early growing season
# reg <- lm(temp ~ sin_doy + cos_doy + winterType, data=allFlux[intersect(grep("umbsFaset", allFlux$source), doyEarlySeasonrows), ])
# summary(reg)
# boxplot(allFlux$temp[intersect(grep("umbsFaset",allFlux$source), doyEarlySeasonrows)] ~ allFlux$winterType[intersect(grep("umbsFaset",allFlux$source), doyEarlySeasonrows)])
# emmeans(reg, pairwise ~ winterType)

# reg <- lm(temp ~ sin_doy + cos_doy + winterType, data=allFlux[intersect(grep("umbsForte", allFlux$source), doyEarlySeasonrows), ])
# summary(reg)
# boxplot(allFlux$temp[intersect(grep("umbsForte",allFlux$source), doyEarlySeasonrows)] ~ allFlux$winterType[intersect(grep("umbsForte",allFlux$source), doyEarlySeasonrows)])
# emmeans(reg, pairwise ~ winterType)

# reg <- lm(temp ~ sin_doy + cos_doy + winterType, data=allFlux[intersect(grep("kbsTCF", allFlux$source), doyEarlySeasonrows), ])
# summary(reg)
# boxplot(allFlux$temp[intersect(grep("kbsTCF",allFlux$source), doyEarlySeasonrows)] ~ allFlux$winterType[intersect(grep("kbsTCF",allFlux$source), doyEarlySeasonrows)])
# emmeans(reg, pairwise ~ winterType)
# 
# reg <- lm(temp ~ sin_doy + cos_doy + winterType, data=allFlux[intersect(grep("kbsTDF", allFlux$source), doyEarlySeasonrows), ])
# summary(reg)
# boxplot(allFlux$temp[intersect(grep("kbsTDF",allFlux$source), doyEarlySeasonrows)] ~ allFlux$winterType[intersect(grep("kbsTDF",allFlux$source), doyEarlySeasonrows)])
# emmeans(reg, pairwise ~ winterType)
# 
# reg <- lm(temp ~ sin_doy + cos_doy + winterType, data=allFlux[intersect(grep("kbsTSF", allFlux$source), doyEarlySeasonrows), ])
# summary(reg)
# boxplot(allFlux$temp[intersect(grep("kbsTSF",allFlux$source), doyEarlySeasonrows)] ~ allFlux$winterType[intersect(grep("kbsTSF",allFlux$source), doyEarlySeasonrows)])
# emmeans(reg, pairwise ~ winterType)

# reg <- lm(temp ~ sin_doy + cos_doy + winterType, data=allFlux[intersect(grep("desai", allFlux$source), doyEarlySeasonrows), ])
# summary(reg)
# boxplot(allFlux$temp[intersect(grep("desai",allFlux$source), doyEarlySeasonrows)] ~ allFlux$winterType[intersect(grep("desai",allFlux$source), doyEarlySeasonrows)])
# emmeans(reg, pairwise ~ winterType)





reg <- lm(temp ~ sin_doy + cos_doy + winterType + swc, data=allFlux[grep("stei", allFlux$source),])
summary(reg)
boxplot(allFlux$temp[intersect(grep("arainTp39", allFlux$source), intersect(which(allFlux$doy > 91), which(allFlux$doy <= 182)))] ~ allFlux$winterType[intersect(grep("arainTp39", allFlux$source), intersect(which(allFlux$doy > 91), which(allFlux$doy <= 182)))])



plot(allFlux$doy, allFlux$flux, pch=".")
plot(allFlux$cos_doy, allFlux$flux, pch=".")
plot(allFlux$sin_doy, allFlux$flux, pch=".")
plot(allFlux$temp, allFlux$flux, pch=".")
plot(allFlux$swc, allFlux$flux, pch=".")
plot(allFlux$swc[which(allFlux$swc < 0.6)], allFlux$flux[which(allFlux$swc < 0.6)], pch=".")
boxplot(allFlux$flux ~ allFlux$winterType)
boxplot(allFlux$flux ~ allFlux$winterType + substr(allFlux$Group.1, 6, 7), col=c("lightblue", "blue", "grey", "red", "purple"))

boxplot(allFlux$flux[which(allFlux$doy <= 91)] ~ allFlux$winterType[which(allFlux$doy <= 91)])
boxplot(allFlux$flux[intersect(which(allFlux$doy > 91), which(allFlux$doy <= 182))] ~ allFlux$winterType[intersect(which(allFlux$doy > 91), which(allFlux$doy <= 182))])
boxplot(allFlux$flux[intersect(which(allFlux$doy > 182), which(allFlux$doy <= 274))] ~ allFlux$winterType[intersect(which(allFlux$doy > 182), which(allFlux$doy <= 274))])
boxplot(allFlux$flux[which(allFlux$doy > 274)] ~ allFlux$winterType[which(allFlux$doy > 274)])

plot(allFlux$doy[which(allFlux$winterType=="neutral")], allFlux$flux[which(allFlux$winterType=="neutral")], col="grey")
points(allFlux$doy[which(allFlux$winterType=="coldDry")], allFlux$flux[which(allFlux$winterType=="coldDry")], col="lightblue")
points(allFlux$doy[which(allFlux$winterType=="coldWet")], allFlux$flux[which(allFlux$winterType=="coldWet")], col="blue")
points(allFlux$doy[which(allFlux$winterType=="warmDry")], allFlux$flux[which(allFlux$winterType=="warmDry")], col="red")
points(allFlux$doy[which(allFlux$winterType=="warmWet")], allFlux$flux[which(allFlux$winterType=="warmWet")], col="purple")



# Plot temperature-soil respiration relationship for each site
filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/tempFluxRelationship.jpg"
jpeg(filename=filename, res=200, width = 4, height = 4, units = "in")
# Create and empty plot
plot(allFlux$temp, allFlux$flux, type="n", ylim=c(0, 12), xlim=c(-1, 23), xlab="Soil temperature (°C)", ylab="Flux (µmol m-2 s-1)")
# Create color palette
cols <- rainbow(length(unique(allFlux$source)))
mad_n <- NA
mad_cd <- NA
mad_cw <- NA
mad_wd <- NA
mad_ww <- NA
# Loop through each site
for(i in c(1:2,6:12)){
  # Identify site rows
  theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
  
  # Skip if no valid temperature data
  if(sum(!is.na(allFlux$temp[theseRows])) != 0){
    # Add the points for a given site
    points(allFlux$temp[theseRows], allFlux$flux[theseRows], pch=".", col=cols[i])
    
    # Create a data frame for the site
    x <- allFlux[theseRows, ]
    row.names(x) <- 1:nrow(x)
    # Create a regression
    reg <- lm(log(x$flux) ~ x$temp)
    # Move the temperature data and Add an ordered sequence of temp data
    x$tempMeasured <- x$temp
    x$temp <- seq(min(x$temp, na.rm=T), max(x$temp, na.rm=T), by=(max(x$temp, na.rm=T) - min(x$temp, na.rm=T))/(nrow(x)-1))
    # Calculate predicted flux based on ordered temp data and then plot it
    x$predFlux <- predict(reg, newdata = x)
    lines(x$temp, exp(x$predFlux), col=cols[i], lwd=1)
    

        
    # Find the row that has the closest temperature to the median temperature for each winter type during the early season
    doyEarlySeasonrows_x <- intersect(which(x$doy > 80), which(x$doy <= 140))
    medianTempRow_n <- which.min(abs(x$temp - median(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("n", x$winterTypeShort))], na.rm=T)))
    medianTempRow_cd <- which.min(abs(x$temp - median(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("cd", x$winterTypeShort))], na.rm=T)))
    medianTempRow_cw <- which.min(abs(x$temp - median(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("cw", x$winterTypeShort))], na.rm=T)))
    medianTempRow_wd <- which.min(abs(x$temp - median(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("wd", x$winterTypeShort))], na.rm=T)))
    medianTempRow_ww <- which.min(abs(x$temp - median(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("ww", x$winterTypeShort))], na.rm=T)))
    
    # Add median absolute deviation error bars
    mad_n <- mad(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("n", x$winterTypeShort))])
    mad_cd <- mad(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("cd", x$winterTypeShort))])
    mad_cw <- mad(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("cw", x$winterTypeShort))])
    mad_wd <- mad(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("wd", x$winterTypeShort))])
    mad_ww <- mad(x$tempMeasured[intersect(doyEarlySeasonrows_x, grep("ww", x$winterTypeShort))])
    # if(!is.na(mad_n)){
    #   arrows(x$temp[medianTempRow_n]-mad_n, exp(x$predFlux[medianTempRow_n]), 
    #          x$temp[medianTempRow_n]+mad_n, exp(x$predFlux[medianTempRow_n]),
    #          code = 3, angle = 90, length = 0.02)
    # }
    # if(!is.na(mad_cd)){
    #   arrows(x$temp[medianTempRow_cd]-mad_cd, exp(x$predFlux[medianTempRow_cd]), 
    #          x$temp[medianTempRow_cd]+mad_cd, exp(x$predFlux[medianTempRow_cd]),
    #          code = 3, angle = 90, length = 0.02)
    # }
    # if(!is.na(mad_cw)){
    #   arrows(x$temp[medianTempRow_cw]-mad_cw, exp(x$predFlux[medianTempRow_cw]), 
    #          x$temp[medianTempRow_cw]+mad_cw, exp(x$predFlux[medianTempRow_cw]),
    #          code = 3, angle = 90, length = 0.02)
    # }
    # if(!is.na(mad_wd)){
    #   arrows(x$temp[medianTempRow_wd]-mad_wd, exp(x$predFlux[medianTempRow_wd]), 
    #          x$temp[medianTempRow_wd]+mad_wd, exp(x$predFlux[medianTempRow_wd]),
    #          code = 3, angle = 90, length = 0.02)
    # }
    # if(!is.na(mad_ww)){
    #   arrows(x$temp[medianTempRow_ww]-mad_ww, exp(x$predFlux[medianTempRow_ww]), 
    #          x$temp[medianTempRow_ww]+mad_ww, exp(x$predFlux[medianTempRow_ww]),
    #          code = 3, angle = 90, length = 0.02)
    # }
    mad_n <- NA
    mad_cd <- NA
    mad_cw <- NA
    mad_wd <- NA
    mad_ww <- NA
    
    # Add a point showing the median temperature for each winter type during the early season
    points(x$temp[medianTempRow_n], exp(x$predFlux[medianTempRow_n]), col="black", pch=16, cex=1)
    points(x$temp[medianTempRow_cd], exp(x$predFlux[medianTempRow_cd]), col="lightblue", pch=16, cex=1)
    points(x$temp[medianTempRow_cw], exp(x$predFlux[medianTempRow_cw]), col="blue", pch=16, cex=1)
    points(x$temp[medianTempRow_wd], exp(x$predFlux[medianTempRow_wd]), col="red", pch=16, cex=1)
    points(x$temp[medianTempRow_ww], exp(x$predFlux[medianTempRow_ww]), col="purple", pch=16, cex=1)
    # Add outer black line around each point to increase visibility
    points(x$temp[medianTempRow_n], exp(x$predFlux[medianTempRow_n]), cex=1)
    points(x$temp[medianTempRow_cd], exp(x$predFlux[medianTempRow_cd]), cex=1)
    points(x$temp[medianTempRow_cw], exp(x$predFlux[medianTempRow_cw]), cex=1)
    points(x$temp[medianTempRow_wd], exp(x$predFlux[medianTempRow_wd]), cex=1)
    points(x$temp[medianTempRow_ww], exp(x$predFlux[medianTempRow_ww]), cex=1)
  }
}
legend("topleft", legend=unique(allFlux$source)[c(1:2,6:12)], col=cols[c(1:2,6:12)], lty=1, bty="n", cex=0.6)
legend("top", legend=c("Cold-dry", "Cold-wet", "Neutral", "Warm-dry", "Warm-wet"), col=c("lightblue", "blue", "black", "red", "purple"), pch=16, bty="n", cex=0.6)
dev.off()





# Calculate the mean flux for a given DOY within a 30-day window at each site
allFlux$runningMeanFlux <- NA
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
  
  # Calculate the mean flux
  for(j in 1:length(allFlux$doy[theseRows])){
    allFlux$runningMeanFlux[theseRows][j] <- mean(allFlux$flux[intersect(intersect(which(allFlux$doy >= (allFlux$doy[theseRows][j]-15)), 
                                                                                   which(allFlux$doy < (allFlux$doy[theseRows][j]+15))), theseRows)])
  }
}


# Calculate the flux anomaly
allFlux$fluxAnomaly <- allFlux$flux - allFlux$runningMeanFlux



doy <- NA
fluxNeutral <- NA
fluxColdDry <- NA
fluxColdWet <- NA
fluxWarmDry <- NA
fluxWarmWet <- NA
# Plot soil respiration, temperature, and soil moisture versus DOY for each site
filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/allFluxBySite.png"
png(filename=filename, res=200, width = 10, height = 10, units = "in")
par(mfcol=c(6, 6))
par(mar=c(2, 4, 1.5, 0.5))
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
  
  
  # Soil CO2 efflux
  plot(allFlux$doy[theseRows], allFlux$flux[theseRows], type="n", xlim=c(0,365), ylim=c(0,max(allFlux$flux[theseRows], na.rm=T)), main=unique(allFlux$source)[i], ylab="Flux (µmol m-2 s-1)", xlab="Day of year", cex=0.6)
  points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # Add running mean
  for(j in 16:350){
    doy[j] <- j
    fluxNeutral[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))])
    fluxColdDry[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))])
    fluxColdWet[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))])
    fluxWarmDry[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))])
    fluxWarmWet[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))])
    
  }
  lines(doy, fluxNeutral, lwd=2, col="black")
  lines(doy, fluxColdDry, lwd=2, col="lightblue")
  lines(doy, fluxColdWet, lwd=2, col="blue")
  lines(doy, fluxWarmDry, lwd=2, col="red")
  lines(doy, fluxWarmWet, lwd=2, col="purple")
  if(i == 1){
    legend("topleft", legend=c("Warm/wet", "Warm/dry", "Neutral", "Cold/wet", "Cold/dry"), bty="n", col=c("purple", "red", "black", "blue", "lightblue"), lwd=2, lty=1, cex=0.6)
    
  }
  
  
  
  # # Soil CO2 efflux anomaly
  # plot(allFlux$doy[theseRows], allFlux$fluxAnomaly[theseRows], type="n", xlim=c(0,365), ylim=c(min(allFlux$fluxAnomaly[theseRows], na.rm=T),max(allFlux$fluxAnomaly[theseRows], na.rm=T)), main=unique(allFlux$source)[i], ylab="Flux Anom. (µmol m-2 s-1)", xlab="Day of year")
  # points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # # Add running mean
  # for(j in 16:350){
  #   doy[j] <- j
  #   fluxNeutral[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))])
  #   fluxColdDry[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))])
  #   fluxColdWet[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))])
  #   fluxWarmDry[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))])
  #   fluxWarmWet[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))])
  #   
  # }
  # lines(doy, fluxNeutral, lwd=2, col="black")
  # lines(doy, fluxColdDry, lwd=2, col="lightblue")
  # lines(doy, fluxColdWet, lwd=2, col="blue")
  # lines(doy, fluxWarmDry, lwd=2, col="red")
  # lines(doy, fluxWarmWet, lwd=2, col="purple")
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # Soil temp
  plot(allFlux$doy[theseRows], allFlux$temp[theseRows], type="n", xlim=c(0,365), ylim=c(-3,22), main=unique(allFlux$source)[i], ylab="Temp. (°C)", xlab="Day of year")
  points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # Add running mean
  for(j in 16:350){
    doy[j] <- j
    fluxNeutral[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))])
    fluxColdDry[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))])
    fluxColdWet[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))])
    fluxWarmDry[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))])
    fluxWarmWet[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))])
    
  }
  lines(doy, fluxNeutral, lwd=2, col="black")
  lines(doy, fluxColdDry, lwd=2, col="lightblue")
  lines(doy, fluxColdWet, lwd=2, col="blue")
  lines(doy, fluxWarmDry, lwd=2, col="red")
  lines(doy, fluxWarmWet, lwd=2, col="purple")
  
  
  # Soil water content
  plot(allFlux$doy[theseRows], allFlux$swc[theseRows], type="n", xlim=c(0,365), ylim=c(0,0.5), main=unique(allFlux$source)[i], ylab="SWC (m3/m3)", xlab="Day of year")
  points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # Add running mean
  for(j in 16:350){
    doy[j] <- j
    fluxNeutral[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))], na.rm=T)
    fluxColdDry[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))], na.rm=T)
    fluxColdWet[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))], na.rm=T)
    fluxWarmDry[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))], na.rm=T)
    fluxWarmWet[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))], na.rm=T)
    
  }
  lines(doy, fluxNeutral, lwd=2, col="black")
  lines(doy, fluxColdDry, lwd=2, col="lightblue")
  lines(doy, fluxColdWet, lwd=2, col="blue")
  lines(doy, fluxWarmDry, lwd=2, col="red")
  lines(doy, fluxWarmWet, lwd=2, col="purple")
  
}
dev.off()
par(mfcol=c(1, 1))







# Plot soil respiration, temperature, and soil moisture versus DOY for each site
filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/allFluxBySiteDOY80-180.png"
png(filename=filename, res=200, width = 10, height = 10, units = "in")
par(mfcol=c(6, 6))
par(mar=c(2, 4, 1.5, 0.5))
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
  
  
  # Soil CO2 efflux
  plot(allFlux$doy[theseRows], allFlux$flux[theseRows], type="n", xlim=c(80,180), ylim=c(0,max(allFlux$flux[theseRows], na.rm=T)), main=unique(allFlux$source)[i], ylab="Flux (µmol m-2 s-1)", xlab="Day of year", cex=0.6)
  points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$flux[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # Add running mean
  for(j in 16:350){
    doy[j] <- j
    fluxNeutral[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))])
    fluxColdDry[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))])
    fluxColdWet[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))])
    fluxWarmDry[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))])
    fluxWarmWet[j] <- mean(allFlux$flux[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))])
    
  }
  lines(doy, fluxNeutral, lwd=2, col="black")
  lines(doy, fluxColdDry, lwd=2, col="lightblue")
  lines(doy, fluxColdWet, lwd=2, col="blue")
  lines(doy, fluxWarmDry, lwd=2, col="red")
  lines(doy, fluxWarmWet, lwd=2, col="purple")
  if(i == 1){
    legend("topleft", legend=c("Warm/wet", "Warm/dry", "Neutral", "Cold/wet", "Cold/dry"), bty="n", col=c("purple", "red", "black", "blue", "lightblue"), lwd=2, lty=1, cex=0.6)
    
  }
  
  
  
  # # Soil CO2 efflux anomaly
  # plot(allFlux$doy[theseRows], allFlux$fluxAnomaly[theseRows], type="n", xlim=c(80,180), ylim=c(min(allFlux$fluxAnomaly[theseRows], na.rm=T),max(allFlux$fluxAnomaly[theseRows], na.rm=T)), main=unique(allFlux$source)[i], ylab="Flux Anom. (µmol m-2 s-1)", xlab="Day of year")
  # points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  # points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$fluxAnomaly[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # # Add running mean
  # for(j in 16:350){
  #   doy[j] <- j
  #   fluxNeutral[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))])
  #   fluxColdDry[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))])
  #   fluxColdWet[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))])
  #   fluxWarmDry[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))])
  #   fluxWarmWet[j] <- mean(allFlux$fluxAnomaly[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))])
  #   
  # }
  # lines(doy, fluxNeutral, lwd=2, col="black")
  # lines(doy, fluxColdDry, lwd=2, col="lightblue")
  # lines(doy, fluxColdWet, lwd=2, col="blue")
  # lines(doy, fluxWarmDry, lwd=2, col="red")
  # lines(doy, fluxWarmWet, lwd=2, col="purple")
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # Soil temp
  plot(allFlux$doy[theseRows], allFlux$temp[theseRows], type="n", xlim=c(80,180), ylim=c(-3,22), main=unique(allFlux$source)[i], ylab="Temp. (°C)", xlab="Day of year")
  points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$temp[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # Add running mean
  for(j in 16:350){
    doy[j] <- j
    fluxNeutral[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))])
    fluxColdDry[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))])
    fluxColdWet[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))])
    fluxWarmDry[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))])
    fluxWarmWet[j] <- mean(allFlux$temp[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))])
    
  }
  lines(doy, fluxNeutral, lwd=2, col="black")
  lines(doy, fluxColdDry, lwd=2, col="lightblue")
  lines(doy, fluxColdWet, lwd=2, col="blue")
  lines(doy, fluxWarmDry, lwd=2, col="red")
  lines(doy, fluxWarmWet, lwd=2, col="purple")
  
  
  # Soil water content
  plot(allFlux$doy[theseRows], allFlux$swc[theseRows], type="n", xlim=c(80,180), ylim=c(0,0.5), main=unique(allFlux$source)[i], ylab="SWC (m3/m3)", xlab="Day of year")
  points(allFlux$doy[intersect(theseRows, grep("neutral", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("neutral", allFlux$winterType))], col=rgb(0,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldDry", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("coldDry", allFlux$winterType))], col=rgb(0,0.5,0.5,0.3))
  points(allFlux$doy[intersect(theseRows, grep("coldWet", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("coldWet", allFlux$winterType))], col=rgb(0,0,1,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmDry", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("warmDry", allFlux$winterType))], col=rgb(1,0,0,0.3))
  points(allFlux$doy[intersect(theseRows, grep("warmWet", allFlux$winterType))], allFlux$swc[intersect(theseRows, grep("warmWet", allFlux$winterType))], col=rgb(0.5,0,0.5,0.3))
  # Add running mean
  for(j in 16:350){
    doy[j] <- j
    fluxNeutral[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("neutral", allFlux$winterType))], na.rm=T)
    fluxColdDry[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldDry", allFlux$winterType))], na.rm=T)
    fluxColdWet[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("coldWet", allFlux$winterType))], na.rm=T)
    fluxWarmDry[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmDry", allFlux$winterType))], na.rm=T)
    fluxWarmWet[j] <- mean(allFlux$swc[intersect(intersect(intersect(which(allFlux$doy >= (j-15)), which(allFlux$doy < (j+15))), theseRows), grep("warmWet", allFlux$winterType))], na.rm=T)
    
  }
  lines(doy, fluxNeutral, lwd=2, col="black")
  lines(doy, fluxColdDry, lwd=2, col="lightblue")
  lines(doy, fluxColdWet, lwd=2, col="blue")
  lines(doy, fluxWarmDry, lwd=2, col="red")
  lines(doy, fluxWarmWet, lwd=2, col="purple")
  
}
dev.off()
par(mfcol=c(1, 1))


# Loop through the sites to detemine the available date ranges
for(i in 1:length(unique(allFlux$source))){
  # Identify the row for the site
  theseRows <- grep(unique(allFlux$source)[i], allFlux$source)
  
  # Print the start and end year for each sites' data
  print(unique(allFlux$source)[i])
  print(min(substr(allFlux$Group.1[theseRows],1,4)))
  print(max(substr(allFlux$Group.1[theseRows],1,4)))
  print("")
}



# # Download soil water content data
# swc <- loadByProduct(dpID="DP1.00094.001", include.provisional=T, site="STEI", package="expanded", timeIndex=30, check.size=F)
# # Only keep measurement level 1 (~6 cm)
# swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$verticalPosition == 501), ]
# # Only keep unflagged data
# swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$VSWCFinalQF == 0), ]
# 
# 
# # Remove soil moisture data calculated with the soil specific calibration to ensure consistent sensor calibration is used throughout the time series
# neonCalStart <- min(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
# neonCalEnd <- max(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
# neanCalRows <- intersect(which(swc$SWS_30_minute$startDateTime > neonCalStart), which(swc$SWS_30_minute$startDateTime < neonCalEnd))
# swc$SWS_30_minute$VSWCMean[neanCalRows] <- NA
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesSTEIswc <- aggregate(swc$SWS_30_minute, by=list(substr(swc$SWS_30_minute$startDateTime, 1, 10)), FUN=mean, na.rm=T)
# colnames(datesSTEIswc)[1] <- "date"
# 
# # Add a DOY column
# datesSTEIswc$doy <- yday(datesSTEIswc$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesSTEIswc$date >= as.Date("1997-11-01")), which(datesSTEIswc$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesSTEIswc$date >= as.Date("2015-11-01")), which(datesSTEIswc$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesSTEIswc$date >= as.Date("2023-11-01")), which(datesSTEIswc$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesSTEIswc$date >= as.Date("1991-11-01")), which(datesSTEIswc$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesSTEIswc$date >= as.Date("1994-11-01")), which(datesSTEIswc$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesSTEIswc$date >= as.Date("2002-11-01")), which(datesSTEIswc$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesSTEIswc$date >= as.Date("2009-11-01")), which(datesSTEIswc$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesSTEIswc$date >= as.Date("2004-11-01")), which(datesSTEIswc$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesSTEIswc$date >= as.Date("2006-11-01")), which(datesSTEIswc$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesSTEIswc$date >= as.Date("2014-11-01")), which(datesSTEIswc$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesSTEIswc$date >= as.Date("2018-11-01")), which(datesSTEIswc$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesSTEIswc$date >= as.Date("1990-11-01")), which(datesSTEIswc$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("1992-11-01")), which(datesSTEIswc$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("1995-11-01")), which(datesSTEIswc$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("1998-11-01")), which(datesSTEIswc$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("2003-11-01")), which(datesSTEIswc$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("2005-11-01")), which(datesSTEIswc$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("2007-11-01")), which(datesSTEIswc$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("2010-11-01")), which(datesSTEIswc$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("2016-11-01")), which(datesSTEIswc$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesSTEIswc$date >= as.Date("2019-11-01")), which(datesSTEIswc$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesSTEI_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesSTEIswc$doy[noneRows], datesSTEIswc$VSWCMean[noneRows], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="STEI 2017-2024 6 cm", xlim=c(0, 365), ylim=c(0,max(datesSTEIswc$VSWCMean, na.rm=T)))
# points(datesSTEIswc$doy[weakRows], datesSTEIswc$VSWCMean[weakRows], col="lightblue")
# points(datesSTEIswc$doy[moderateRows], datesSTEIswc$VSWCMean[moderateRows], col="blue")
# points(datesSTEIswc$doy[strongRows], datesSTEIswc$VSWCMean[strongRows], pch=16, col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=c(1,1,1,16))
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Download soil temperature data
# st <- loadByProduct(dpID="DP1.00041.001", include.provisional=T, site="UNDE", timeIndex=30, check.size=F)
# # Only keep measurement level 2 (~6 cm)
# st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$verticalPosition == 502), ]
# # Only keep unflagged data
# st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$finalQF == 0), ]
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesUNDE <- aggregate(st$ST_30_minute, by=list(substr(st$ST_30_minute$startDateTime, 1, 10)), FUN=mean, na.rm=T)
# colnames(datesUNDE)[1] <- "date"
# 
# # Add a DOY column
# datesUNDE$doy <- yday(datesUNDE$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesUNDE$date >= as.Date("1997-11-01")), which(datesUNDE$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesUNDE$date >= as.Date("2015-11-01")), which(datesUNDE$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesUNDE$date >= as.Date("2023-11-01")), which(datesUNDE$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesUNDE$date >= as.Date("1991-11-01")), which(datesUNDE$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesUNDE$date >= as.Date("1994-11-01")), which(datesUNDE$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUNDE$date >= as.Date("2002-11-01")), which(datesUNDE$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUNDE$date >= as.Date("2009-11-01")), which(datesUNDE$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesUNDE$date >= as.Date("2004-11-01")), which(datesUNDE$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesUNDE$date >= as.Date("2006-11-01")), which(datesUNDE$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUNDE$date >= as.Date("2014-11-01")), which(datesUNDE$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUNDE$date >= as.Date("2018-11-01")), which(datesUNDE$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesUNDE$date >= as.Date("1990-11-01")), which(datesUNDE$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("1992-11-01")), which(datesUNDE$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("1995-11-01")), which(datesUNDE$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("1998-11-01")), which(datesUNDE$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("2003-11-01")), which(datesUNDE$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("2005-11-01")), which(datesUNDE$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("2007-11-01")), which(datesUNDE$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("2010-11-01")), which(datesUNDE$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("2016-11-01")), which(datesUNDE$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDE$date >= as.Date("2019-11-01")), which(datesUNDE$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUNDE_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUNDE$doy[noneRows], datesUNDE$soilTempMean[noneRows], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="UNDE 2017-2024 6 cm", xlim=c(0, 365), ylim=c(min(datesUNDE$soilTempMean, na.rm=T),max(datesUNDE$soilTempMean, na.rm=T)))
# points(datesUNDE$doy[weakRows], datesUNDE$soilTempMean[weakRows], col="lightblue")
# points(datesUNDE$doy[moderateRows], datesUNDE$soilTempMean[moderateRows], col="blue")
# points(datesUNDE$doy[strongRows], datesUNDE$soilTempMean[strongRows], pch=16, col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=c(1,1,1,16))
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# 
# 
# # Download soil water content data
# swc <- loadByProduct(dpID="DP1.00094.001", include.provisional=T, site="UNDE", package="expanded", timeIndex=30, check.size=F)
# # Only keep measurement level 1 (~6 cm)
# swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$verticalPosition == 501), ]
# # Only keep unflagged data
# swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$VSWCFinalQF == 0), ]
# 
# 
# # Remove soil moisture data calculated with the soil specific calibration to ensure consistent sensor calibration is used throughout the time series
# neonCalStart <- min(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
# neonCalEnd <- max(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
# neanCalRows <- intersect(which(swc$SWS_30_minute$startDateTime > neonCalStart), which(swc$SWS_30_minute$startDateTime < neonCalEnd))
# swc$SWS_30_minute$VSWCMean[neanCalRows] <- NA
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesUNDEswc <- aggregate(swc$SWS_30_minute, by=list(substr(swc$SWS_30_minute$startDateTime, 1, 10)), FUN=mean, na.rm=T)
# colnames(datesUNDEswc)[1] <- "date"
# 
# # Add a DOY column
# datesUNDEswc$doy <- yday(datesUNDEswc$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesUNDEswc$date >= as.Date("1997-11-01")), which(datesUNDEswc$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesUNDEswc$date >= as.Date("2015-11-01")), which(datesUNDEswc$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesUNDEswc$date >= as.Date("2023-11-01")), which(datesUNDEswc$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesUNDEswc$date >= as.Date("1991-11-01")), which(datesUNDEswc$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesUNDEswc$date >= as.Date("1994-11-01")), which(datesUNDEswc$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUNDEswc$date >= as.Date("2002-11-01")), which(datesUNDEswc$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesUNDEswc$date >= as.Date("2009-11-01")), which(datesUNDEswc$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesUNDEswc$date >= as.Date("2004-11-01")), which(datesUNDEswc$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesUNDEswc$date >= as.Date("2006-11-01")), which(datesUNDEswc$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUNDEswc$date >= as.Date("2014-11-01")), which(datesUNDEswc$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesUNDEswc$date >= as.Date("2018-11-01")), which(datesUNDEswc$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesUNDEswc$date >= as.Date("1990-11-01")), which(datesUNDEswc$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("1992-11-01")), which(datesUNDEswc$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("1995-11-01")), which(datesUNDEswc$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("1998-11-01")), which(datesUNDEswc$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("2003-11-01")), which(datesUNDEswc$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("2005-11-01")), which(datesUNDEswc$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("2007-11-01")), which(datesUNDEswc$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("2010-11-01")), which(datesUNDEswc$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("2016-11-01")), which(datesUNDEswc$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesUNDEswc$date >= as.Date("2019-11-01")), which(datesUNDEswc$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesUNDE_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesUNDEswc$doy[noneRows], datesUNDEswc$VSWCMean[noneRows], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="UNDE 2017-2024 6 cm", xlim=c(0, 365), ylim=c(0,max(datesUNDEswc$VSWCMean, na.rm=T)))
# points(datesUNDEswc$doy[weakRows], datesUNDEswc$VSWCMean[weakRows], col="lightblue")
# points(datesUNDEswc$doy[moderateRows], datesUNDEswc$VSWCMean[moderateRows], col="blue")
# points(datesUNDEswc$doy[strongRows], datesUNDEswc$VSWCMean[strongRows], pch=16, col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=c(1,1,1,16))
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Download soil temperature data
# st <- loadByProduct(dpID="DP1.00041.001", include.provisional=T, site="TREE", timeIndex=30, check.size=F)
# # Only keep measurement level 2 (~6 cm)
# st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$verticalPosition == 502), ]
# # Only keep unflagged data
# st$ST_30_minute <- st$ST_30_minute[which(st$ST_30_minute$finalQF == 0), ]
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesTREE <- aggregate(st$ST_30_minute, by=list(substr(st$ST_30_minute$startDateTime, 1, 10)), FUN=mean, na.rm=T)
# colnames(datesTREE)[1] <- "date"
# 
# # Add a DOY column
# datesTREE$doy <- yday(datesTREE$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesTREE$date >= as.Date("1997-11-01")), which(datesTREE$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesTREE$date >= as.Date("2015-11-01")), which(datesTREE$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesTREE$date >= as.Date("2023-11-01")), which(datesTREE$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesTREE$date >= as.Date("1991-11-01")), which(datesTREE$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesTREE$date >= as.Date("1994-11-01")), which(datesTREE$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesTREE$date >= as.Date("2002-11-01")), which(datesTREE$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesTREE$date >= as.Date("2009-11-01")), which(datesTREE$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesTREE$date >= as.Date("2004-11-01")), which(datesTREE$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesTREE$date >= as.Date("2006-11-01")), which(datesTREE$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesTREE$date >= as.Date("2014-11-01")), which(datesTREE$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesTREE$date >= as.Date("2018-11-01")), which(datesTREE$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesTREE$date >= as.Date("1990-11-01")), which(datesTREE$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("1992-11-01")), which(datesTREE$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("1995-11-01")), which(datesTREE$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("1998-11-01")), which(datesTREE$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("2003-11-01")), which(datesTREE$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("2005-11-01")), which(datesTREE$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("2007-11-01")), which(datesTREE$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("2010-11-01")), which(datesTREE$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("2016-11-01")), which(datesTREE$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREE$date >= as.Date("2019-11-01")), which(datesTREE$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesTREE_Temp.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesTREE$doy[noneRows], datesTREE$soilTempMean[noneRows], col="grey", xlab="DOY", ylab="Soil temperature (°C)", main="TREE 2017-2024 6 cm", xlim=c(0, 365), ylim=c(min(datesTREE$soilTempMean, na.rm=T),max(datesTREE$soilTempMean, na.rm=T)))
# points(datesTREE$doy[weakRows], datesTREE$soilTempMean[weakRows], col="lightblue")
# points(datesTREE$doy[moderateRows], datesTREE$soilTempMean[moderateRows], col="blue")
# points(datesTREE$doy[strongRows], datesTREE$soilTempMean[strongRows], pch=16, col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=c(1,1,1,16))
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()
# 
# 
# 
# # Download soil water content data
# swc <- loadByProduct(dpID="DP1.00094.001", include.provisional=T, site="TREE", package="expanded", timeIndex=30, check.size=F)
# # Only keep measurement level 1 (~6 cm)
# swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$verticalPosition == 501), ]
# # Only keep unflagged data
# swc$SWS_30_minute <- swc$SWS_30_minute[which(swc$SWS_30_minute$VSWCFinalQF == 0), ]
# 
# 
# # Remove soil moisture data calculated with the soil specific calibration to ensure consistent sensor calibration is used throughout the time series
# neonCalStart <- min(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
# neonCalEnd <- max(swc$SWS_30_minute$startDateTime[which(swc$SWS_30_minute$calNEONQM>0)])
# neanCalRows <- intersect(which(swc$SWS_30_minute$startDateTime > neonCalStart), which(swc$SWS_30_minute$startDateTime < neonCalEnd))
# swc$SWS_30_minute$VSWCMean[neanCalRows] <- NA
# 
# # Average across all soil respiration data collected on the same date (i.e., across all times and plots)
# datesTREEswc <- aggregate(swc$SWS_30_minute, by=list(substr(swc$SWS_30_minute$startDateTime, 1, 10)), FUN=mean, na.rm=T)
# colnames(datesTREEswc)[1] <- "date"
# 
# # Add a DOY column
# datesTREEswc$doy <- yday(datesTREEswc$date)
# 
# # Identify dates (rows) that correspond to no, weak, moderate, or strong ENSO based on email from Ankur
# # Strong
# strongRows <- intersect(which(datesTREEswc$date >= as.Date("1997-11-01")), which(datesTREEswc$date < as.Date("1998-11-01")))
# strongRows <- append(strongRows, intersect(which(datesTREEswc$date >= as.Date("2015-11-01")), which(datesTREEswc$date < as.Date("2016-11-01"))))
# strongRows <- append(strongRows, intersect(which(datesTREEswc$date >= as.Date("2023-11-01")), which(datesTREEswc$date < as.Date("2024-11-01"))))
# 
# # Moderate
# moderateRows <- intersect(which(datesTREEswc$date >= as.Date("1991-11-01")), which(datesTREEswc$date < as.Date("1992-11-01")))
# moderateRows <- append(moderateRows, intersect(which(datesTREEswc$date >= as.Date("1994-11-01")), which(datesTREEswc$date < as.Date("1995-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesTREEswc$date >= as.Date("2002-11-01")), which(datesTREEswc$date < as.Date("2003-11-01"))))
# moderateRows <- append(moderateRows, intersect(which(datesTREEswc$date >= as.Date("2009-11-01")), which(datesTREEswc$date < as.Date("2010-11-01"))))
# 
# # Weak
# weakRows <- intersect(which(datesTREEswc$date >= as.Date("2004-11-01")), which(datesTREEswc$date < as.Date("2005-11-01")))
# weakRows <- append(weakRows, intersect(which(datesTREEswc$date >= as.Date("2006-11-01")), which(datesTREEswc$date < as.Date("2007-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesTREEswc$date >= as.Date("2014-11-01")), which(datesTREEswc$date < as.Date("2015-11-01"))))
# weakRows <- append(weakRows, intersect(which(datesTREEswc$date >= as.Date("2018-11-01")), which(datesTREEswc$date < as.Date("2019-11-01"))))
# 
# # None (Neutral or La Nina)
# noneRows <- intersect(which(datesTREEswc$date >= as.Date("1990-11-01")), which(datesTREEswc$date < as.Date("1991-11-01")))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("1992-11-01")), which(datesTREEswc$date < as.Date("1994-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("1995-11-01")), which(datesTREEswc$date < as.Date("1997-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("1998-11-01")), which(datesTREEswc$date < as.Date("2002-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("2003-11-01")), which(datesTREEswc$date < as.Date("2004-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("2005-11-01")), which(datesTREEswc$date < as.Date("2006-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("2007-11-01")), which(datesTREEswc$date < as.Date("2009-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("2010-11-01")), which(datesTREEswc$date < as.Date("2014-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("2016-11-01")), which(datesTREEswc$date < as.Date("2018-11-01"))))
# noneRows <- append(noneRows, intersect(which(datesTREEswc$date >= as.Date("2019-11-01")), which(datesTREEswc$date < as.Date("2023-11-01"))))
# 
# # Plot soil respiration rate versus day of year. Color points based on extremeness of El Nino event that month
# filename <- "C:/Users/eayres/OneDrive - National Ecological Observatory Network/Meetings/2023/GLUG/FollowUpAnalyses/datesTREE_SWC.png"
# png(filename=filename, res=300, width = 5, height = 5, units = "in")
# plot(datesTREEswc$doy[noneRows], datesTREEswc$VSWCMean[noneRows], col="grey", xlab="DOY", ylab="Soil moisture (m3 m-3)", main="TREE 2017-2024 6 cm", xlim=c(0, 365), ylim=c(0,max(datesTREEswc$VSWCMean, na.rm=T)))
# points(datesTREEswc$doy[weakRows], datesTREEswc$VSWCMean[weakRows], col="lightblue")
# points(datesTREEswc$doy[moderateRows], datesTREEswc$VSWCMean[moderateRows], col="blue")
# points(datesTREEswc$doy[strongRows], datesTREEswc$VSWCMean[strongRows], pch=16, col="darkblue")
# legend("topleft", legend=c("None", "Weak", "Moderate", "Strong"), bty="n", col=c("grey", "lightblue", "blue", "darkblue"), pch=c(1,1,1,16))
# # legend("topright", legend=paste0("Proportion removed: ", round(proportionSuspectArainTp74, digits=2)), bty="n")
# legend("bottomleft", legend="Deciduous forest", bty="n")
# dev.off()

