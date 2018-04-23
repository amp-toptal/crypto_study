

fetchHistorical <- function(currency.1, currency.2, start.date="2014-01-01", frequency.months = 2){
  
  unix.start <- as.numeric(as.POSIXct(start.date, format="%Y-%m-%d"))
  current.time <- as.numeric(as.POSIXct(Sys.time()))
   # 30 days x 24 hours x 60 minutes x 60 seconds
  seconds.per.month <- 30*24*60*60
  
  unix.start <- unix.start + frequency.months*seconds.per.month
  
  output.data <- NULL
  while (unix.start < current.time){
    print(unix.start)
    output.data <- rbind(output.data, getHistoricalData(currency.1, currency.2, to.timestamp = unix.start))
    Sys.sleep(5)
    unix.start <- unix.start + frequency.months*seconds.per.month
  }
  
  output.data <- output.data[!(duplicated(output.data[,'Time Stamp'])),]
  
  return(output.data)
}


btc.data <- fetchHistorical("BTC","USD", start.date = "2014-01-01", frequency.months = 2)
setwd("/Users/aaronpickering/Desktop/miscellaneous/crypto_outliers/")
write.csv(btc.data, file="BTC_Historical.csv", row.names=FALSE)

eth.data <- fetchHistorical("ETH","USD", start.date = "2014-01-01", frequency.months = 2)
write.csv(eth.data, file="ETH_Historical.csv", row.names=FALSE)

