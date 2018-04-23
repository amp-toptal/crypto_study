library("httr")
library("jsonlite")
library("quantmod")
library("mgcv")

# Fetch historical hourly data from cryptocompare
getHistoricalData <- function(currency.1, currency.2, to.timestamp=NULL){
  
  if (is.null(to.timestamp)){
    link <- paste("https://min-api.cryptocompare.com/data/histohour?fsym=",currency.1,"&tsym=", currency.2, "&limit=2000&e=CCCAGG", sep="")
    response <- GET(link)
    
    response.content <- content(response)
  } else {
    link <- paste("https://min-api.cryptocompare.com/data/histohour?fsym=",currency.1,"&tsym=", currency.2, "&limit=2000&e=CCCAGG","&toTs=",to.timestamp, sep="")
    response <- GET(link)
    
    response.content <- content(response)
  }

  
  # Extract relevant data and convert to a data frame
  dat <- response.content$Data
  
  output.data <- matrix(NA, nrow=length(dat), ncol=7)
  colnames(output.data) <- c("Time Stamp","Close","High","Low","Open","Volume From","Volume To")
  for (i in 1:length(dat)){
    time.stamp <- as.POSIXct(dat[[i]]$time, origin="1970-01-01")
    output.data[i,'Time Stamp'] <- as.character(time.stamp)
    output.data[i,'Close'] <- dat[[i]]$close
    output.data[i,'High'] <- dat[[i]]$high
    output.data[i,'Low'] <- dat[[i]]$low
    output.data[i,'Open'] <- dat[[i]]$open
    output.data[i,'Volume From'] <- dat[[i]]$volumefrom
    output.data[i,'Volume To'] <- dat[[i]]$volumeto
  }
  
  return(output.data)
  
}

btc.data <- getHistoricalData("BTC", "USD")
write.csv(btc.data, file="BTC_historical_march_10.csv", row.names = FALSE)
eth.data <- getHistoricalData("ETH", "USD")
write.csv(eth.data, file="ETH_historical_march_10.csv", row.names = FALSE)


btc.data <- as.data.frame(btc.data)

getDate <- function(x){
  date.stamp <- as.Date(strsplit(as.character(x)," ")[[1]][1], origin="1970-01-01")
  return(date.stamp)
}

getHour <- function(x){
  hour <- as.numeric(strsplit(strsplit(as.character(x)," ")[[1]][2],":")[[1]][1])
  return(hour)
}

getWeekday <- function(x){
  weekday <- weekdays(as.Date(as.character(x), origin="1970-01-01"))
  return(weekday)
}



#  -------------  Do the analysis for BTC
btc.data <- as.data.frame(btc.data)
btc.data$date.stamp <- as.Date(unlist(lapply(as.character(btc.data$Time.Stamp), getDate)), origin="1970-01-01")
btc.data$hour <- unlist(lapply(btc.data$`Time Stamp`, getHour))
btc.data$weekday <- unlist(lapply(btc.data$`Time Stamp`, getWeekday))

plot(as.numeric(as.character(btc.data$Close)), type="lines")

btc.data$price.detrended <- as.numeric(as.character(btc.data$Close))/probabilisticSmoothing(as.numeric(as.character(btc.data$Close)), trials = 2000, lag.u = 0, lag.sd = 24) - 1
btc.data$weekday <- as.factor(btc.data$weekday)

summary(lm(price.detrended ~ weekday, data=btc.data))


# Aggregate onto a daily level first 

btc.data.daily <- subset(btc.data, btc.data$hour == "12")
plot(as.numeric(as.character(btc.data.daily$Close)), type="lines")

btc.data.daily$price.detrended <- as.numeric(as.character(btc.data.daily$Close))/probabilisticSmoothing(as.numeric(as.character(btc.data.daily$Close)), trials = 2000, lag.u = 0, lag.sd = 4) - 1
btc.data.daily$weekday <- as.factor(btc.data.daily$weekday)

# Plot the detrended value
plot(btc.data.daily$price.detrended, type="lines")

# Plot the Weekday vs the Price
plot(btc.data.daily$weekday, btc.data.daily$price.detrended)

# Regression of day vs Price
lm.fit <- lm(price.detrended ~ weekday, data=btc.data.daily)

summary(lm.fit)
# Result - no significant evidence of day of the week affecting the price, however there 
#is a slight hint that saturdays tend to have higher prices


# Now look at the day/time combination
btc.data$weekday.hour <- paste(btc.data$weekday,"_",btc.data$hour, sep="")

first.value <- btc.data[1,'weekday.hour']
counter <- 1
btc.data$weekday.hour.num <- NA
for (i in 1:nrow(btc.data)){
  if (btc.data[i,'weekday.hour'] == first.value){
    counter <- 1
    btc.data[i,'weekday.hour.num'] <- counter
  } else {
    btc.data[i,'weekday.hour.num'] <- counter
  }
  counter <- counter + 1
}

btc.data$weekday.hour.num <- as.numeric(as.factor(btc.data$weekday.hour))

cyclical.vars <- cyclicalTransformation(btc.data$weekday.hour.num, 168)
btc.data <- cbind(btc.data, cyclical.vars)


# Linear model fit

lm.fit <- lm(price.detrended ~ x + y, data=btc.data)
plot(btc.data$price.detrended, type="lines")
lines(predict(lm.fit), col="red")

# Non linear fit

gam.fit <- mgcv::gam(price.detrended ~ s(x, k=4) + s(y, k=4), data=btc.data, family="gaussian")
plot(btc.data$price.detrended, type="lines")
lines(predict(gam.fit), col="red")

plot(btc.data$weekday.hour.num, btc.data$price.detrended)
points(btc.data$weekday.hour.num, predict(gam.fit), col="red")

gam.check(gam.fit)
plot(gam.fit)

# -------------  Do the analysis for ETH

eth.data <- as.data.frame(eth.data)
eth.data$date.stamp <- as.Date(unlist(lapply(eth.data$`Time Stamp`, getDate)), origin="1970-01-01")
eth.data$hour <- unlist(lapply(eth.data$`Time Stamp`, getHour))
eth.data$weekday <- unlist(lapply(eth.data$`Time Stamp`, getWeekday))

plot(as.numeric(as.character(eth.data$Close)), type="lines")

eth.data$price.detrended <- as.numeric(as.character(eth.data$Close))/probabilisticSmoothing(as.numeric(as.character(eth.data$Close)), trials = 2000, lag.u = 0, lag.sd = 24) - 1
eth.data$weekday <- as.factor(eth.data$weekday)

summary(lm(price.detrended ~ weekday, data=eth.data))


# Aggregate onto a daily level first 
eth.data.daily <- subset(eth.data, eth.data$hour == "12")
plot(as.numeric(as.character(eth.data.daily$Close)), type="lines")

eth.data.daily$price.detrended <- as.numeric(as.character(eth.data.daily$Close))/probabilisticSmoothing(as.numeric(as.character(eth.data.daily$Close)), trials = 2000, lag.u = 0, lag.sd = 4) - 1
eth.data.daily$weekday <- as.factor(eth.data.daily$weekday)

# Plot the detrended value
plot(eth.data.daily$price.detrended, type="lines")

# Plot the Weekday vs the Price
plot(eth.data.daily$weekday, eth.data.daily$price.detrended)

# Regression of day vs Price
lm.fit <- lm(price.detrended ~ weekday, data=eth.data.daily)

summary(lm.fit)
# Result - no significant evidence of day of the week affecting the price, however there 
#is a slight hint that saturdays tend to have higher prices


# Now look at the day/time combination
eth.data$weekday.hour <- paste(eth.data$weekday,"_",eth.data$hour, sep="")

first.value <- eth.data[1,'weekday.hour']
counter <- 1
eth.data$weekday.hour.num <- NA
for (i in 1:nrow(eth.data)){
  if (eth.data[i,'weekday.hour'] == first.value){
    counter <- 1
    eth.data[i,'weekday.hour.num'] <- counter
  } else {
    eth.data[i,'weekday.hour.num'] <- counter
  }
  counter <- counter + 1
}

cyclical.vars <- cyclicalTransformation(eth.data$weekday.hour.num, 168)
eth.data <- cbind(eth.data, cyclical.vars)


# Linear model fit
lm.fit <- lm(price.detrended ~ x + y, data=eth.data)
plot(eth.data$price.detrended, type="lines")
lines(predict(lm.fit), col="red")

# Non linear fit
gam.fit <- mgcv::gam(price.detrended ~ s(x, k=4) + s(y, k=4), data=eth.data, family="gaussian")
plot(eth.data$price.detrended, type="lines")
lines(predict(gam.fit), col="red")

plot(eth.data$weekday.hour.num, eth.data$price.detrended)
points(eth.data$weekday.hour.num, predict(gam.fit), col="red")

gam.check(gam.fit)
plot(gam.fit)



# Summary 
# We can see from the linear model fits, thats weekday does not appear to have a significant impact on the price. 
# However both linear models suggest an increase in price on Saturdays. While not statistically significant to a 5% level p vaue,
# this would definitey be worth keeping an eye on.

# Finally with regard to hours in the day on specific weekdays, there appears to be a very slight trend heading into Saturday
# This could however be due to noise in the current sample, I would hesitate to draw strong conclusions


# Buy on wednesday at 4pm and sell on saturday at 4pm

money <- 1
owned.coins <- 0
for (i in 1:nrow(btc.data)){
  if (btc.data[i,'weekday.hour'] == "Wednesday_16"){
    owned.coins <- money/as.numeric(as.character(btc.data[i,'Open']))
    abline(v=i, col="red")
  }
  
  if (btc.data[i,'weekday.hour'] == "Saturday_16"){
    money <- owned.coins*as.numeric(as.character(btc.data[i,'Open']))
    print(money)
    owned.coins <- 0
    abline(v=i, col="green")
  }
}

money <- 1
owned.coins <- 0
for (i in 1:nrow(eth.data)){
  if (eth.data[i,'weekday.hour'] == "Wednesday_16"){
    owned.coins <- money/as.numeric(as.character(eth.data[i,'Open']))
    abline(v=i, col="red")
  }
  
  if (eth.data[i,'weekday.hour'] == "Saturday_16"){
    money <- owned.coins*as.numeric(as.character(eth.data[i,'Open']))
    print(money)
    owned.coins <- 0
    abline(v=i, col="green")
  }
}



