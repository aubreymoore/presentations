library(solaR)

BTd = fBTd(mode = 'serie', start = '15-10-2011', end = '15-10-2011')

Guam.SolD = fSolD(lat=13.0, BTd)
Guam.SolI = fSolI(Guam.SolD, sample='min', keep.night=F)
summary(Guam.SolI)
p = xyplot(Guam.SolI$Bo0, col='red')
#print(p)

Honolulu.SolD = fSolD(lat=23.0, BTd)
Honolulu.SolI = fSolI(Honolulu.SolD, sample='min', keep.night=F)
summary(Honolulu.SolI)
p = p + xyplot(Honolulu.SolI$Bo0, col='blue')
print(p)

