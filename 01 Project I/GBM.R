#source("GBM.R")

EstimateReturnParameters <- function(DataSet, StartDate = "1900-01-01", EndDate = Sys.Date(), WorkingDays = 252)
{
  Closings <- DataSet[DataSet$Data >= StartDate & DataSet$Data <= EndDate,][,5]
  Returns <- diff(Closings)/Closings[-length(Closings)]
  return(c(WorkingDays*mean(Returns), sqrt(WorkingDays)*sd(Returns)))
}

RateOfProfits <- function(DataSet, StartDate = "1900-01-01", EndDate = Sys.Date())
{
  Closings <- DataSet[DataSet$Data >= StartDate & DataSet$Data <= EndDate,][,5]
  return(diff(Closings)/Closings[-length(Closings)])
}

RateOfProfitsSimple <- function(DataSet)
{
  return(diff(DataSet)/DataSet[-length(DataSet)])
}

Covar <- function(DataSet1, DataSet2, StartDate = "1900-01-01", EndDate = Sys.Date())
{
  Closings1 <- DataSet1[DataSet1$Data >= StartDate & DataSet1$Data <= EndDate,][,5]
  Closings2 <- DataSet2[DataSet2$Data >= StartDate & DataSet2$Data <= EndDate,][,5]
  Returns1 <- diff(Closings1)/Closings1[-length(Closings1)]
  Returns2 <- diff(Closings2)/Closings2[-length(Closings2)]
  
  Covaration <-cov(Returns1, Returns2, method="pearson")
}

GenerateBrownianMotion <- function (Drift, Volatility, StartValue, Length = 252)
{
  StNormVal <- rnorm(Length)
  dt <- 1/Length
  Out <- 1:Length
  Out[1] <- StartValue
  
  for(i in 2:Length) 
  {
    Out[i] <- Out[i-1]*exp((Drift-0.5*Volatility^2)*dt+Volatility*sqrt(dt)*StNormVal[i])
  }
  
  return(Out)
}

GenerateBrownianMotionCor <- function (Drift1, Drift2, Volatility1, Volatility2, Cov, StartValue1, StartValue2, Length = 252)
{
  StNormVal <- rmvnorm(Length, c(0,0), matrix(c(1, Cov, Cov, 1), nrow = 2))
  dt <- 1/Length
  Out1 <- Out2 <- 1:Length
  Out1[1] <- StartValue1
  Out2[1] <- StartValue2
  
  for(i in 2:Length) 
  {
    Out1[i] <- Out1[i-1]*exp((Drift1-0.5*Volatility1^2)*dt+Volatility1*sqrt(dt)*StNormVal[i,1])
    Out2[i] <- Out2[i-1]*exp((Drift2-0.5*Volatility2^2)*dt+Volatility2*sqrt(dt)*StNormVal[i,2])
  }
  
  return(cbind(Out1, Out2))
}

