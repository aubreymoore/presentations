#
# This R script processes data from a Davis Instruments weather station
# deployed near dead pigs used in Joey Lopez's forensic entomology
# experiment
#
########################################################################

# Clean up workspace by removing all objects
# We don't want to trip on anything

rm(list=ls())

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

PMI = as.numeric(round((data$t - start)/(60*60*24),4))
data = cbind(data,PMI)

summary(data)

# Convert temperature from F to C

data$TempOut = round((5.0/9.0) * (data$TempOut - 32.0), 4)

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

# Generate plot

png(filename="weatherData.png", width=800, height=600)
# op = par(mfrow=c(4,1), mar=c(2.5,4,2,2))
op = par(mfrow=c(4,1))
plot(TempOut~PMI, data, type="l", xlab="", ylab="temperature (C)")
plot(OutHum~PMI, data, type="l", xlab="", ylab= "relative humidity (%)")
plot(Rain~PMI, data, type="h", xlab="", ylab="rainfall (?)")
plot(SolarRad~PMI, data, type="l", ylab="solar radiation (?)",
  xlab="post mortem interval (days)")
par(op)
dev.off()

#
# This R script processes temperature measurements from an infrared
# thermometer measuring pig skin temperature, maggot mass temperature,
# and adjacent soil temperature. Air temperature comes from a Davis 
# Instruments weather station set up nearby.
#
########################################################################

# Get ten minute data for air temperature values

tmdata = read.csv("TenMinuteData.csv", as.is=2)
tmdata$t = as.POSIXct(strptime(tmdata$t, "%Y-%m-%d %H:%M:%s"))

# Get IR thermometer measurements for skin, maggot mass, and soil surface

data = read.csv("LaserTemps2.csv", as.is=1)
data$t = as.POSIXct(strptime(data$t, "%Y-%m-%d %H:%M"))
start = min(data$t)
stop = max(data$t)

# Add post mortem inteval column to tmdata and data

PMI = as.numeric(round((data$t - start)/(60*60*24),4))
data = cbind(data, PMI)

# Plot data for each pig

  # Set line thickness for plots

op = par(lwd=2)
for (i in 1:3){
  png(filename=paste("pig",i,".png",sep=""), width=800, height=600)
  pig = data[data$pig==i,]

  # Plot air temp. then all data for Pig i

  plot(TempOut~PMI,tmdata, type="l", ylim=c(22,62), col="blue",
    main=paste("Pig", i), xlab="post mortem interval (days)", ylab="temperature(C)", lwd=2)
  points(skin~PMI, pig, type="b", col="black", pch=1, lwd=2)
  points(maggots~PMI, pig, type="b", col="green", pch=2, lwd=2)
  points(soil~PMI, pig, type="b", col="red", pch=5, lwd=2)
  legend("topleft", c("skin","maggot mass","soil", "air"), 
    col=c("black","green","red","blue"), pch=c(1,2,5,NA), lwd=2, bg="lightgray");
  dev.off()
}
par(op)

# Plot weight loss for pig 1

gross.mass = c(20,20,20,20,18,17,16,14,10.5,9.5,8,8,8,7.5,8,7,7,7,
  8,8,7.5,7.5,7.5,8,7,7,7,7,7,6.25,6.5,7,6.5,7.5,7,7.8,7.2,6.2,6.1,6.1,6,5.8,6.3)
net.mass = gross.mass-5
percent.mass = 100 * (net.mass / net.mass[1])
png(filename="pigMass.png", width=800, height=600)
plot(data[data$pig==1,"PMI"], percent.mass, type="b", lwd=2, 
  xlab="post mortem interval (days)", ylab="mass remaining (%)")
dev.off()

library(ggplot2)
pig1 = data[data$pig==1,]
p = qplot(PMI,skin,data=pig1,
  ylab="skin temperature (°C)",xlab="post mortem interval (days)",
  geom=c("point","line"))
p + scale_x_continuous(breaks=c(0,5,10,15,20,25), minor_breaks=0:28)


