install.packages("quantmod")
install.packages("lattice")
install.packages("timeSeries")
install.packages("rugarch")

library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(gdata)

getSymbols("^GSPC", from="2014-01-01")
spReturns = diff(log(Cl(GSPC)))
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0

windowLength = 500
foreLength = length(spReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)

for (d in 0:foreLength) { 
    spReturnsOffset = spReturns[(1+d):(windowLength+d)]  
    
    final.aic <- Inf
         final.order <- c(0,0,0)
         for (p in 0:5) for (q in 0:5) {
             if ( p == 0 && q == 0) {
                 next
             }
     
             arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q)),
                                  error=function( err ) FALSE,
                                  warning=function( err ) FALSE )
    
             if( !is.logical( arimaFit ) ) {
                 current.aic <- AIC(arimaFit)
                 if (current.aic < final.aic) {
                     final.aic <- current.aic
                     final.order <- c(p, 0, q)
                     final.arima <- arima(spReturnsOffset, order=final.order)
                 }
             } else {
                 next
             }
         }
    
         
         spec = ugarchspec(
                 variance.model=list(garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
                 distribution.model="sged")
     
     fit = tryCatch(
       ugarchfit(
         spec, spReturnsOffset, solver = 'hybrid'
       ), error=function(e) e, warning=function(w) w
     )         
     
     if(is(fit, "warning")) {
       forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")
       print(paste(index(spReturnsOffset[windowLength]), 1, sep=","))
     } else {
       fore = ugarchforecast(fit, n.ahead=1)
       ind = fore@forecast$seriesFor
       forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
       print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")) 
     }
} 

write.csv(forecasts, file="forecasts_test2.csv", row.names=FALSE)

forecasts = open("forecasts_test.csv", "r").readlines()

old_value = 1
new_list = []
for f in forecasts[1:]:
  strpf = f.replace('"','').strip()
new_str = "%s,%s\n" % (strpf, old_value)
newspl = new_str.strip().split(",")
final_str = "%s,%s\n" % (newspl[0], newspl[2])
final_str = final_str.replace('"','')
old_value = f.strip().split(',')[1]
new_list.append(final_str)

out = open("forecasts_new.csv", "w")
for n in new_list:
  out.write(n)

spArimaGarch = as.xts( 
   read.zoo(
     file="forecasts_test.csv", format="%Y-%m-%d", header=F, sep=",",skip = 2
   )
 )


#----
spArimaGarch <- read.csv(file="forecasts_test.csv")
spArimaGarch[,1] <- as.Date(spArimaGarch[,1])
#----


spIntersect = merge( spArimaGarch[,1], spReturns, all=F )
spArimaGarchReturns = spIntersect[,1] * spIntersect[,2]

spArimaGarchCurve = log( cumprod( 1 + spArimaGarchReturns ) )
spBuyHoldCurve = log( cumprod( 1 + spIntersect[,2] ) )
spCombinedCurve = merge( spArimaGarchCurve, spBuyHoldCurve, all=F )

 xyplot( 
   spCombinedCurve,
   superpose=T,
   col=c("darkred", "darkblue"),
   lwd=2,
   key=list( 
     text=list(
       c("ARIMA+GARCH", "Buy & Hold")
     ),
     lines=list(
     lwd=2, col=c("darkred", "darkblue")
     )
   )
 )
