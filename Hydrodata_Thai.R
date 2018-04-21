#setwd("F:/thailand_Ruddy_Hydro")

require("zoo")
require("EcoHydRology")
raw <- read.csv("2014_waterlevel_raw_data.csv")
summary(raw)

# change colum names and save new data as Data.csv
colnames(raw) <- c("Date", "Time", "Pressure_cm")
write.csv(raw, file = "Data.csv")

# Data clean
## in general do we have missing value? how many?
Data <- read.csv("Data.csv")
sum(is.na(Data$Pressure_cm))
summary(Data)

## do we have missing day?
date <- as.Date(Data$Date, format = '%d.%m.%Y')
summary(unique(date)) 
day1 <- unique(date)[1]
final_day <- unique(date)[length(unique(date))]
isTRUE(length(unique(date)) == final_day - day1 +1)

## do we have missing time on specific days? if yes, which day, how many?
time <- as.POSIXct(paste(Data$Date, Data$Time),format="%d.%m.%Y %H:%M:%S")

x <- difftime(time[1:(length(time)-1)] , time[2:length(time)])
unique(x)

##where is the missing water level data
z <- as.integer(x)
time_char <- as.character(time)
df <- cbind(time_char[2:length(time_char)],z)
df <- as.data.frame(df)
colnames(df) <- c("Date_time", "diffT")
df[which(df$diffT != -10), ]

##there were around 26 water level data missing from 2014-07-13 10:26:49 to 2014-07013 14:51:14 colum 10049, 10050
##checking rainfall data: no big rainfall from 2014-07-10 to 2014-07-13
##linear interpolation missing value
NewRow.time <- as.POSIXct("2014-07-13 10:26:49") + seq(from = 10*60, to = 260*60, by = 10*60)
NewRow.time <- substr(as.character(NewRow.time),12,19)
NewRow <- cbind(rep(NA, 26), rep("13.07.2014",26), NewRow.time, rep(NA, 26))
colnames(NewRow)<- names(Data)
Data_inser <- rbind(Data[1:10049,],NewRow, Data[10050:nrow(Data),])
# install.packages("zoo")
Data_inser$Pressure_cm <- na.approx(Data_inser$Pressure_cm)
write.table(Data_inser, file = "CleanData.csv", sep = ",", row.names = F)


Data <- read.csv("CleanData.csv")

####################### Data Processing ################################
Data$Pressure_m <- Data$Pressure_cm/100
colnames(Data) <- c("X", "Date", "Time", "WaterLevel_cm","WaterLevel_m")
Data$Dis_m3_h <- (Data$WaterLevel_m)^2.31*136500   

#10min total discharge calculation
x1 <- Data$Dis_m3_h[1:(length(Data$Dis_m3_h)-1)]
x2 <- Data$Dis_m3_h[2:length(Data$Dis_m3_h)]
x3 <- (x1 + x2)*10/60/2
Data$Dis_10min_m3 <- c(x3, NA)

#Daily total discharge
Dis_day <- tapply(Data$Dis_10min_m3, as.Date(Data$Date, "%d.%m.%Y"), sum)
Dis_day <- c(Dis_day, rep(NA, (nrow(Data)-length(Dis_day))))
Data$Dis_day_m3 <- Dis_day

#Daily average discharge (m3/s) and save
Data$Dis_day_m3_s <- Data$Dis_day_m3 / 24/3600
write.table(Data, file = "Daily_Dis.csv", sep = ",", row.names = F)

Data <- read.csv("Daily_Dis.csv")
#combined dataframe for baseflow seperation
#Colum name "Date"  "rainfall"  "Dis_day_m3_s" 
Dis_day_m3_s <- Data$Dis_day_m3_s[!is.na(Data$Dis_day_m3_s)]
Dis_day_m3_s <- Dis_day_m3_s[2:length(Dis_day_m3_s)] #remove first value to correspond to rainfall date
Date <- as.Date("2014-05-05") + seq(from = 1, to = 127, by = 1) - 1
rainfall_data <- read.csv("2014_rainfall_data.csv")
colnames(rainfall_data) <- c("Date", "Rainfall_mm")
rainfall_date <- as.Date(as.character(rainfall_data$Date), "%Y/%m/%d")
x <- rainfall_date[1:(length(rainfall_date)-1)] - rainfall_date[2:length(rainfall_date)]
unique(x)
rainfall <- c(rainfall_data$Rainfall_mm, rep(NA, 2))
DailyDis_comb <- data.frame(cbind(as.character(Date), rainfall, round(Dis_day_m3_s, 4)))
colnames(DailyDis_comb) <- c("Date", "Rainfall", "Dis_m3_s")
write.table(DailyDis_comb, file = "DailyDis_comb.csv", sep = ",", row.names = F)

#Plot daily discharge change in time seriers for check
Date <- as.Date(DailyDis_comb$Date)
Daily_Dis <- as.numeric((as.character((DailyDis_comb$Dis_m3_s))))
plot_DailyDis <- plot(Date, Daily_Dis, type = 'l')

#Base flow seperation by package EcoHydRology
Data <- read.csv("DailyDis_comb.csv")
#hydrograph(input = Data, precip = Data[,2], streamflow = Data[,3])
# if above step fail, check class of column, most probably because the first column is not Date class
Data$Date <- as.Date(Data$Date)

# Baseflow seperation
bfs <- BaseflowSeparation(Data$Dis_m3_s,0.9,1)
tiff(file="hydrograph.tiff", width=12, height=8, units="in", compression="lzw", res=150)
hydrograph(input = Data, precip = Data[,2], streamflow = Data[,3], streamflow2 = bfs[,1])
dev.off()
Data$Baseflow <- bfs[,1]
Data$Peakflow <- bfs[,2]
write.table(Data, file = "Hydrography_Thai_2014.csv", sep = ",", row.names = FALSE)
tiff(file="Peakflow.tiff", width=12, height=8, units="in", compression="lzw", res=150)
plot_peakflow <- plot(Data$Peakflow, type = 'l')
dev.off()

