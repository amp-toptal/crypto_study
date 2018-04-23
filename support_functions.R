arimaLag <- function(input.vector, arima.lags){
  require(forecast)
  require(foreach)
  require(parallel)
  require(doParallel)
  
  # Register clusters
  cl <- makeCluster(7)
  registerDoParallel(cl)
  
  leading.nas <- length(rep(1, length(input.vector))[is.na(input.vector)])
  output <- input.vector
  input.vector <- na.omit(input.vector)
  
  result <- foreach(prediction.window=arima.lags, .export=c("forecast", "auto.arima")) %do% {
    vector.copy <- c(rep(NA, prediction.window), input.vector)
    for (i in 1:length(input.vector)){
      print(i)
      start <- (i - 200)
      if (start < 1) start <- 1
      aa.fit <- auto.arima(input.vector[start:i])
      arima.pred <- as.vector(forecast(aa.fit, h=prediction.window)$mean)
      vector.copy[prediction.window + i] <- arima.pred[prediction.window]
    }
    vector.copy
  }
  doParallel::stopImplicitCluster()
  arima.output <- do.call("cbind", result)

  colnames(arima.output) <- paste("arima_lag_",arima.lags, sep="")
  
  return(arima.output)
}


cyclicalTransformation <- function(input.vector, num.levels){
  
  x <- sin(input.vector*2*pi/num.levels)
  y <- cos(input.vector*2*pi/num.levels)
  
  output.data <- cbind(x, y)
  return(output.data)
  
}


probabilisticSmoothing <- function (time.series, trials, lag.u, lag.sd, fill.nas = TRUE, plot.ts = TRUE) {
  smoothing <- round(rnorm(trials, lag.u, lag.sd), 0)
  all.x <- time.series
  for (i in 1:trials) {
    if (smoothing[i] < 0) {
      all.x <- cbind(all.x, Next(time.series, k = abs(smoothing[i])))
    }
    else if (smoothing[i] == 0) {
      all.x <- cbind(all.x, time.series)
    }
    else {
      all.x <- cbind(all.x, Lag(time.series, k = abs(smoothing[i])))
    }
  }
  naOmitMean <- function(row) {
    mean(na.omit(row))
  }
  if (fill.nas == TRUE) {
    probabilistic.smooth <- apply(all.x, 1, naOmitMean)
  }
  else {
    probabilistic.smooth <- rowMeans(all.x)
  }
  if (plot.ts == TRUE) {
    plot(time.series, type = "lines")
    lines(probabilistic.smooth, col = "red")
  }
  return(probabilistic.smooth)
}



standardize <- function (x) 
{
  maximum <- max(na.omit(x))
  minimum <- min(na.omit(x))
  x.sd <- x
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      x.sd[i] <- (x[i] - minimum)/(maximum - minimum)
    }
  }
  return(x.sd)
}

fillInf <- function (vector, fill.with = NA) {
  fill <- function(x, f) {
    if (is.infinite(x)) {
      return(fill.with)
    }
    else {
      return(x)
    }
  }
  return(sapply(vector, fill, f = fill.with))
}

fillNAs <- function (vector, fill.with = 0) {
  fill <- function(x, f) {
    if (is.na(x)) {
      return(fill.with)
    }
    else {
      return(x)
    }
  }
  return(sapply(vector, fill, f = fill.with))
}


