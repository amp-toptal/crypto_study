setwd("/Users/aaronpickering/Desktop/miscellaneous/crypto_outliers/")
source("./support_functions.R")

btc.data <- read.csv("BTC_Historical.csv", header = TRUE)


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



singleSimulation("Wednesday","14","Saturday","14", input.data = btc.data)

optimisation.grid <- createGrid(btc.data)

# Split into optimisation and Cross Validation period
training <- btc.data[1:25000,]
test <- btc.data[25001:36557,]

optimise.2015 <- btc.data[9320:18078,]
optimise.2016 <- btc.data[18079:26861,]
optimise.2017 <- btc.data[26862:35620,]
test.set <- btc.data[35261:nrow(btc.data),]

# Simulate all combinations
all.results.2015 <- rep(NA,nrow(optimise.2015))
all.results.2016 <- rep(NA,nrow(optimise.2016))
all.results.2017 <- rep(NA,nrow(optimise.2017))
for (i in 1:nrow(optimisation.grid)){
  print(i)
  info <- optimisation.grid[i,]
  info <- as.character(info)
  result <- singleSimulation(info[1], info[2], info[3], info[4], optimise.2015)
  all.results.2015[i] <- result$`Final Return`
  result <- singleSimulation(info[1], info[2], info[3], info[4], optimise.2016)
  all.results.2016[i] <- result$`Final Return`
  result <- singleSimulation(info[1], info[2], info[3], info[4], optimise.2017)
  all.results.2017[i] <- result$`Final Return`
}

output <- list("2015"=all.results.2015, "2016"=all.results.2016, "2017"=all.results.2017)
saveRDS(output, file="./optimisation.rds")

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

singleSimulation <- function(buy.day, buy.time, sell.day, sell.time, input.data){
  input.data$hour <- as.character(input.data$hour)
  input.data$weekday <- as.character(input.data$weekday)
  
  first.buy.time <- min(intersect(which(input.data[,'weekday'] == buy.day), which(input.data[,'hour'] == buy.time)))
  input.data <- input.data[first.buy.time:nrow(input.data),]
  
  money <- 1
  owned.coins <- 0
  results <- vector(mode="numeric",length=ceiling(nrow(input.data)/168))
  results[1:length(results)] <- NA
  for (i in 1:nrow(input.data)){
    if (input.data[i,'weekday'] == buy.day && input.data[i,'hour'] == buy.time){
      owned.coins <- money/as.numeric(as.character(input.data[i,'Open']))
      #abline(v=i, col="red")
    }
    
    if (input.data[i,'weekday'] == sell.day && input.data[i,'hour'] == sell.time){
      money <- owned.coins*as.numeric(as.character(input.data[i,'Open']))
      results[length(na.omit(results)) + 1] <- money
      owned.coins <- 0
      #abline(v=i, col="green")
    }
  }
  return(list("Progress Returns"=results, "Final Return"= money))
}
