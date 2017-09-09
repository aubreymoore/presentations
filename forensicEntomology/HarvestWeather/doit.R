#
# This R script processes data from a Davis Instruments weather station
# deployed near dead pigs used in Joey Lopez's forensic entomology
# experiment
#
########################################################################

# Read raw data.  
# Skip the 2 header lines and provide new column names.
# Prevent date and time columns (1:2) from being converted to factors

raw = read.delim("Harvest weather data.txt", header=F, skip=2, as.is=1:2)
names(raw) = c("xDate","xTime","TempOut","HiTemp","LowTemp","OutHum","DewPt",
  "WindSpeed","WindDir","WindRun","HiSpeed","HiDir","WindChill","HeatIndex",
  "THWIndex","THSWIndex","Bar","Rain","RainRate","SolarRad","SolarEnergy",
  "HiSolarRad.","UVIndex","UVDose","HiUV","HeatDD","CoolDD","InTemp",
  "InHum","InDew","InHeat","InEMC","InAirDensity","ET","WindSamp","WindTx",
  "ISSRecept","ArcInt")

# Put date and time in standard format

t = character(0)
for (i in 1:nrow(raw)){
  t[i] = paste( raw$xDate[i], " ", raw$xTime[i], "m", sep="")
}
t = strptime(t, "%m/%d/%y %I:%M %p")
raw = cbind(t, raw[,3:ncol(raw)])

# Subset raw data to extract the rows and columns we want

start = strptime("2010-09-09 12:00", "%Y-%m-%d %H:%M")
stop = strptime("2010-10-07 23:59", "%Y-%m-%d %H:%M")
data = subset(raw, t >= start & t <= stop, select=c(t,TempOut,OutHum,WindSpeed,Rain,SolarRad))

# Add column which is post mortem interval (PMI) in days

PMI = round((data$t - start)/(60*60*24),1)
data = cbind(data,PMI)

# Convert temperature from F to C

data$TempOut = round((5.0/9.0) * (data$TempOut - 32.0), 1)

# Convert rainfall from inches to mm

data$Rain = round(25.4 * data$Rain, 0)

# Save 10 minute data as a file

write.csv(data, "TenMinuteData.csv")

# Generate daily summary and save as a file

MeanTemp = round(tapply(data$TempOut, as.character(trunc(data$t, "days")), mean), 1)
MinTemp = tapply(data$TempOut, as.character(trunc(data$t, "days")), min)
MaxTemp = tapply(data$TempOut, as.character(trunc(data$t, "days")), max)
MeanRH = round(tapply(data$OutHum, as.character(trunc(data$t, "days")), mean), 1)
MinRH = tapply(data$OutHum, as.character(trunc(data$t, "days")), min)
MaxRH = tapply(data$OutHum, as.character(trunc(data$t, "days")), max)
MeanWind = round(tapply(data$WindSpeed, as.character(trunc(data$t, "days")), mean), 3)
Rain = tapply(data$Rain, as.character(trunc(data$t, "days")), sum)
SolarRad = tapply(data$SolarRad, as.character(trunc(data$t, "days")), sum)
daily.data = cbind(MeanTemp, MinTemp, MaxTemp, MeanRH, MinRH, MaxRH, MeanWind, Rain, SolarRad)
daily.data
write.csv(daily.data,"DailyData.csv")

# Make a vector for verticle line at each midnight

vlines = trunc(data$t, "days") # strip hours, minutes, seconds
vlines = unique(as.character(vlines)) # find unique values
vlines = as.POSIXct(vlines) # convert back to datetime

# Generate plot

par(mfrow=c(4,1), mar=c(2.5,4,1,2))
plot(TempOut~PMI, data, type="l", xlab="", ylab="temperature (C)")
abline(v=vlines, col="lightgray")
plot(OutHum~PMI, data, type="l", xlab="", ylab= "relative humidity (%)")
abline(v=vlines, col="lightgray")
plot(Rain~PMI, data, type="h", xlab="", ylab="rainfall (?)")
abline(v=vlines, col="lightgray")
plot(SolarRad~PMI, data, type="l", ylab="solar radiation (?)")
abline(v=vlines, col="lightgray")
