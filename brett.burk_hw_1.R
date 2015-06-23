#' Name: Brett Burk
#' HA: 2.1, 2.3, 2.4
#' KJ: 3.2, 4.4

library(ggplot2)
library(fma)
library(zoo)
library(mlbench)
library(caret)

question_2.1a <- function() {
  plot(dole)
  doleLambda  <- BoxCox.lambda(dole)
  plot(BoxCox(dole, doleLambda), xlab = 'Date', 
       ylab = paste('Unemployment benefits with Lambda =', round(doleLambda, digits = 2)))
}

question_2.1b <- function() {
  plot(usdeaths)
  seasonplot(usdeaths, ylab = 'Accidental deaths in the US', year.labels = TRUE, 
             year.labels.left = TRUE, col = 1:6, pch = 19, main= 'Accidental US Deaths')
}

question_2.1c <- function() {
  plot(bricksq)
  plot(rollmean(bricksq, 4), main = 'Rolling Average for Last 12 Months of Data', 
       ylab = 'Average brick production in the past year')
  monthplot(bricksq)
}

question_2.3a <- function() {
  plot(ibmclose)
  plot(rollmean(ibmclose, 10))
  hist(ibmclose, breaks = seq(min(ibmclose), 
                              max(ibmclose) + 10, 10))
}

question_2.3b <- function() {
  trainIbm <- ibmclose[1:300]
  testIbm <- ibmclose[301:369]
}

question_2.3c <- function() {
  trainIbm <- ibmclose[1:300]
  testIbm <- ibmclose[301:369]
  ibmMean <- meanf(trainIbm, h = 10)
  ibmDrift <- rwf(trainIbm, h = 10, drift = TRUE)
  ibmNaive <- naive(trainIbm, h = 10)
  print('Mean')
  print(accuracy(ibmMean, testIbm))
  print('Drift')
  print(accuracy(ibmDrift, testIbm))
  print('Naive')
  print(accuracy(ibmNaive, testIbm))
}

question_2.4a <- function() {
  plot(hsales2)
  seasonplot(hsales2)
  monthplot(hsales2)
}

question_2.4b <- function() {
  salesTrain <- window(hsales2, end = 1994 - 0.1)
  salesTest <- window(hsales2, start = 1994 - 0.1)
}

question_2.4c <- function() {
  salesTrain <- window(hsales2, end = 1994 - 0.1)
  salesTest <- window(hsales2, start = 1994 - 0.1)
  salesMean <- meanf(salesTrain, h = 10)
  salesDrift <- rwf(salesTrain, h = 10, drift = TRUE)
  salesNaive <- naive(salesTrain, h = 10)
  salesSNaive <- snaive(salesTrain, h = 10)
  print('Mean')
  print(accuracy(salesMean, salesTest))
  print('Drift')
  print(accuracy(salesDrift, salesTest))
  print('Naive')
  print(accuracy(salesNaive, salesTest))
  print('Seasonal Naive')
  print(accuracy(salesSNaive, salesTest))
}

question_3.2a <- function() {
  data(Soybean)
  for(name in names(Soybean)){
    print(name)
    print(table(Soybean[name]))
    print('------------------------------')
  }
}

question_3.2b <- function() {
  missingData <- Soybean[!(complete.cases(Soybean)),]
  table(missingData$Class)[table(missingData$Class) >= 1]
}

question_3.2c <- function() {
  mungedData <- Soybean[complete.cases(Soybean),]
}

question_4.4a <- function() {
  data(oil)
  propTable <- table(oilType)/length(oilType)
  simData <- propTable
  for(i in 1:100){
    sampleItems <- sample(1:length(oilType), 60)
    sampledOil <- oilType[sampleItems]
    sampTable <- table(sampledOil)/length(sampledOil)
    simData <- rbind(simData, propTable - sampTable)
  }
  print(colMeans(simData))
}

question_4.4b <- function() {
  propTable <- table(oilType)/length(oilType)
  dp <- createDataPartition(oilType, p = 0.59)
  dpOil <- oilType[dp[[1]]]
  dpTable <- table(dpOil)/length(dpOil)
  print(propTable - dpTable)
}

question_4.4d <- function() {
  toPlot <- c(0)
  for(i in 1:length(oilType)){
    for(j in 1:i){
      currTest <- binom.test(j, i)$conf.int
      toPlot[length(toPlot)+1] <- currTest[2] - currTest[1]
    }
  }
  plot(toPlot)
}

