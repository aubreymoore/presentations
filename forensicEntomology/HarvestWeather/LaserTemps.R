#
# This R script processes temperature measurements from an infrared
# thermometer measuring pig skin temperature, maggot mass temperature,
# and adjacent soil temperature. Air temperature comes from a Davis 
# Instruments weather station set up nearby.
#
########################################################################

# Clean up workspace by removing all objects
# We don't want to trip on anything

rm(list=ls())

# Get ten minute data for air temperature values

tmdata = read.csv("TenMinuteData.csv", as.is=2)
tmdata$t = as.POSIXct(strptime(tmdata$t, "%Y-%m-%d %H:%M:%s"))

# Get IR thermometer measurements for skin, maggot mass, and soil surface

data = read.csv("LaserTemps2.csv", as.is=1)
data$t = as.POSIXct(strptime(data$t, "%Y-%m-%d %H:%M"))
start = min(data$t)
stop = max(data$t)

# Add post mortem inteval column to tmdata and data

PMI = round((data$t - start)*(60*60*24),1)
data = cbind(data, PMI)
PMI = round((tmdata$t - start)*(60*60*24),1)
tmdata = cbind(tmdata, PMI)

# Plot data for each pig

  # Set line thickness for plots

  par(lwd=2)

  # Make a vector for verticle line at each midnight

  vlines = trunc(tmdata$t, "days") # strip hours, minutes, seconds
  vlines = unique(as.character(vlines)) # find unique values
  vlines = as.POSIXct(vlines) # convert back to datetime

for (i in 1:3){
  pig = data[data$pig==i,]

  # Plot air temp. then all data for Pig i

  plot(TempOut~PMI,tmdata, type="l", ylim=c(22,62), col="blue",
    main=paste("Pig", i), xlab="time", ylab="temperature(C)")
  points(skin~PMI, pig, type="b", col="black", pch=1)
  points(maggots~PMI, pig, type="b", col="green", pch=2)
  points(soil~PMI, pig, type="b", col="red", pch=5)
  abline(v=vlines, col="lightgray")
  legend("topleft", c("air","skin","maggots","soil"), 
    col=c("blue","black","green","red"), pch=c(NA,1,2,5), lwd=2, bg="lightgray");
}
