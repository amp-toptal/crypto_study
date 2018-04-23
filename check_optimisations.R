optimisation <- readRDS("/Users/aaronpickering/Desktop/miscellaneous/crypto_outliers/optimisation.rds")

btc.data <- read.csv("/Users/aaronpickering/Desktop/miscellaneous/crypto_outliers/BTC_historical.csv")

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
btc.data$hour <- unlist(lapply(btc.data$`Time.Stamp`, getHour))
btc.data$weekday <- unlist(lapply(btc.data$`Time.Stamp`, getWeekday))
btc.data$hour <- fillNAs(btc.data$hour, fill.with = "0")

optimisation.grid <- createGrid(btc.data)



which.max(optimisation$`2015`)
which.max(optimisation$`2016`)
which.max(optimisation$`2017`)

optimisation.grid[5592,]
optimisation.grid[6574,]
optimisation.grid[20501,]

best.2015 <- findTopN(optimisation$`2015`, n = 100, optimisation.grid, index.or.info = "info")
best.2016 <- findTopN(optimisation$`2016`, n = 100, optimisation.grid, index.or.info = "info")
best.2017 <- findTopN(optimisation$`2017`, n = 5000, optimisation.grid, index.or.info = "info")

write.csv(best.2015, file="./top_100_2015.csv", row.names = FALSE)
write.csv(best.2016, file="./top_100_2016.csv", row.names = FALSE)
write.csv(best.2017, file="./top_100_2017.csv", row.names = FALSE)

createGrid <- function(input.data){
  
  all.combos <- unique(paste(input.data$weekday, "_", input.data$hour, sep=""))
  grid <- expand.grid(all.combos, all.combos)
  grid$is.after <- TRUE
  
  grid <- subset(grid, grid$Var1 != grid$Var2)
  
  buy.day <- unlist(lapply(grid[,1], function(x){strsplit(as.character(x),"_")[[1]][1]}))
  buy.time <- unlist(lapply(grid[,1], function(x){strsplit(as.character(x),"_")[[1]][2]}))
  sell.day <- unlist(lapply(grid[,2], function(x){strsplit(as.character(x),"_")[[1]][1]}))
  sell.time <- unlist(lapply(grid[,2], function(x){strsplit(as.character(x),"_")[[1]][2]}))
  
  output.grid <- cbind(buy.day, buy.time, sell.day, sell.time)
  
  return(output.grid)
}

findTopN <- function(optimisation.input, n=20, optimisation.grid, index.or.info = "info"){
  
  
  x <- cbind(optimisation.input, 1:length(optimisation.input))
  x <- x[order(-x[,1]),]
  
  top.n.indices <- x[1:n, 2]
  top.strategies <- optimisation.grid[top.n.indices,]
  
  if (index.or.info == "info"){
    return(top.strategies)
  } else {
    return(top.n.indices)
  }
}
  