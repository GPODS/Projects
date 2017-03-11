# SÈRIES TEMPORALS
# 20-10-2016

#
# EXEMPLES MODELS ARMA
#

# Sèrie 1
#
# Índex quadrimestral de producció industrial a USA, 
# amb base 100 el 1985, amb les dades desestacionalitzades,
# des de 1960.1 fins a 1991.4
#
# Font: Franses P.H. (1998) Time Series models for bussiness
# and economic forecasting, Cambridge Univ. Press.

ipi<-ts(read.table("ipi.dat"),start=1960,frequency=12)
plot.ts(ipi,col=4)

par(mfrow=c(2,1))   
acf(ipi,ylim=c(-1,1))
pacf(ipi,ylim=c(-1,1))

# transformació de les dades

# tendencia lineal
trend<-lm(ipi~c(1:length(ipi)))
ipi_lt<-ts(residuals(trend),start=1960,frequency=12)
par(mfrow=c(1,1))       
plot.ts(ipi_lt,col=1)

par(mfrow=c(2,1))   
acf(ipi_lt,ylim=c(-1,1))
pacf(ipi_lt,ylim=c(-1,1))

# Sèrie 2
#
# Gross National Product (GNP) USA trimestral 
# des de 1947.1 fins a 1991.1 
# dades desestacionalitzades
# 
# Font: Shumway R. (2000) Time series Analysis and Its Applications

gnp<-ts(read.table("gnp1947.dat"),start=1947,frequency=4)
par(mfrow=c(1,1))   

plot.ts(gnp,col=c(4))

par(mfrow=c(2,1))   
acf(gnp,ylim=c(-1,1))
pacf(gnp,ylim=c(-1,1))

# tendencia lineal

trend<-lm(gnp~c(1:length(gnp)))
gnp_lt<-ts(residuals(trend),start=1947,frequency=1)
par(mfrow=c(1,1))   
plot.ts(gnp_lt)

par(mfrow=c(2,1))   
acf(gnp_lt,ylim=c(-1,1))
pacf(gnp_lt,ylim=c(-1,1))

# diferència

dgnp=diff(gnp)
par(mfrow=c(1,1))   
ts.plot(dgnp,col=4)

par(mfrow=c(2,1))   
acf(dgnp,ylim=c(-1,1))
pacf(dgnp,ylim=c(-1,1))

# Sèrie 3
#
# Morts en accidents als USA 
# dades mensuals des de 1973.1 fins a 1978.12

accid<-ts(read.table("accidents.dat"),start=1973,frequency=12)
par(mfrow=c(1,1))
plot.ts(accid,col=c(4))
d12accid=diff(accid,12)
plot.ts(d12accid,col=c(4))

par(mfrow=c(2,1))
acf(d12accid,ylim=c(-1,1))
pacf(d12accid,ylim=c(-1,1))


# EXERCICIS

# Sèrie A
#

serie<-ts(read.table("serie_a.dat"),start=1,frequency=4)
par(mfrow=c(1,1))
plot.ts(serie,col=c(4))

par(mfrow=c(2,1))
acf(serie,ylim=c(-1,1))
pacf(serie,ylim=c(-1,1))



# Sèrie B
#

serie<-ts(read.table("serie_b.dat"),start=1,frequency=4)


# Sèrie C
#

serie<-ts(read.table("serie_c.dat"),start=1,frequency=4)



