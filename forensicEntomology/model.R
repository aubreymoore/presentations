# Simple model

x = c(0, 12, 25, 100)
y = c(0, 0, 0.1, (100-12)*0.1/13)
plot(x, y, type='b', ylab='developmental rate', xlab='temperature', lwd=3, 
  xlim=c(-5,50), ylim=c(0,0.3), cex.lab=1.5, col='blue',
  main="Simple model for Chrysomya sp.")
abline(v=x, col='grey')
text(x[1], y[1], "lower\nlethal\ntemp", pos=3, cex=1.5, offset=3.5)
text(x[2], y[2], "lower\ndevelopmental\nthreshold", pos=3, cex=1.5, offset=3.5)
text(x[3], y[3], "Egg to Adult\n10 days at 25C", pos=3, cex=1.5, offset=3.5)

# Better model

x = c(0, 12, 25, 30, 49)
y = c(0, 0, 0.1, 0, 0)
plot(x, y, type='b', ylab='developmental rate', xlab='temperature', lwd=3, 
  xlim=c(-5,50), ylim=c(0,0.13), cex.lab=1.5, col='blue',
  main="Better model for Chrysomya sp.")
abline(v=x, col='grey')
text(x[1], y[1], "lower\nlethal\ntemp", pos=3, cex=1.5, offset=5)
text(x[2], y[2], "lower\ndevelopmental\nthreshold", pos=3, cex=1.5, offset=5)
text(x[3], y[3], "Egg to Adult\n10 days at 25C", pos=3, cex=1.5, offset=0.5)
text(x[4], y[4], "upper\ndevelopmental\nthreshold", pos=3, cex=1.5, offset=5)
text(x[5], y[5], "upper\nlethal\ntemp", pos=3, cex=1.5, offset=5)




 
x = c(0, 12, 25, 30, 49)
y = c(0, 0, 0.1, 0, 0)
plot(x, y, type='b', ylab='developmental rate', xlab='temperature', lwd=2)
