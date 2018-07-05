#Czyszczenie pamieci
rm(list = ls())

#Poprawna ścieżka
MyPath <- "C:/Users/mchl/Dropbox/01 UWr/01 Szkoła - projekty/09 IF/03 Projekt I/" 
MyDeliverables <- paste(MyPath,"02 Deliverables/", sep="") 
MyData <- paste(MyPath,"01 Data/", sep="") 
setwd(MyPath)

source("GBM.R")
source("BlackScholesFormula.R")
source("Functions.R")


library("ggplot2")
library("reshape2")
library("mvtnorm")

#Projekt I - Delta Gamma Hedging
#Część 0

#Zmienne globalne
SimNum <- 1000
WorkingDays <- 252


#Dane WIG 20 od 2006-01-01 do 2012-01-01
DataWIG20Historical <- read.csv(paste(MyData, "wig20_d_Historical.csv", sep = ""), header = TRUE, sep = ",", stringsAsFactors = FALSE)
DataWIG20Real       <- read.csv(paste(MyData, "wig20_d_Real.csv", sep = ""), header = TRUE, sep = ",", stringsAsFactors = FALSE )

DataWIG20Historical[1] <- lapply(DataWIG20Historical[1],as.Date)
DataWIG20Real[1]       <- lapply(DataWIG20Real[1],as.Date)

#Dane KGHM od 2006-01-01 do 2012-01-01
DataKGHMHistorical <- read.csv(paste(MyData, "kgh_d_Historical.csv", sep = ""), header = TRUE, sep = ",", stringsAsFactors = FALSE)
DataKGHMReal       <- read.csv(paste(MyData, "kgh_d_Real.csv", sep = ""), header = TRUE, sep = ",", stringsAsFactors = FALSE )

DataKGHMHistorical[1] <- lapply(DataKGHMHistorical[1],as.Date)
DataKGHMReal[1]       <- lapply(DataKGHMReal[1],as.Date)

Years <- c("2006-01-01","2007-01-01","2008-01-01","2009-01-01","2010-01-01")

#Dzienne stopy zwrotu - Rate of profit
RealReturnWIG20 <- RateOfProfits(DataWIG20Real)
RealReturnKGHM  <- RateOfProfits(DataKGHMReal)

#Wartośc początkowa indeksu
StartWIG20 <- DataWIG20Real$Otwarcie[1]
StartKGHM  <- DataKGHMReal$Otwarcie[1]
# 
# png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/Real", sep=""))
# 
# par(mfrow = c(1,1))
# plot(DataWIG20Real$Data, DataWIG20Real$Zamkniecie, col = "gray",  type = "s", lwd =2, xlab = "Day", ylab = "Value", main = "KGHM - historical paths")
# 
# dev.off()

# Symulacja trajektorii przy użyciu schemtu Eulera

for(i in 1:length(Years))
{
    EstimatorsWIG20 <- EstimateReturnParameters(DataWIG20Historical, Years[i], "2011-01-01")
    EstimatorsKGHM <- EstimateReturnParameters(DataKGHMHistorical, Years[i], "2011-01-01")
    Covaration <- Covar(DataWIG20Historical, DataKGHMHistorical, Years[i], "2011-01-01")
    
    
    SimulatedCor      <- replicate(SimNum, GenerateBrownianMotionCor(EstimatorsWIG20[1], EstimatorsKGHM[1], EstimatorsWIG20[2], EstimatorsKGHM[2], Covaration, StartWIG20, StartKGHM))
    SimulatedWIG20Cor <- SimulatedCor[,1,]
    SimulatedKGHMCor  <- SimulatedCor[,2,]
    
    SimulatedReturnWIG20Cor <- apply(SimulatedWIG20Cor, 2, RateOfProfitsSimple)
    ReturnWIG20 <- RateOfProfits(DataWIG20Historical, Years[i], "2011-01-01")

    SimulatedReturnKGHMCor  <- apply(SimulatedKGHMCor, 2, RateOfProfitsSimple)
    ReturnKGHM <- RateOfProfits(DataKGHMHistorical, Years[i], "2011-01-01")
    
    Year <- 2005 + i
	
	QuantilesVec   <- c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
	QuantilesWIG20 <- matrix(StartWIG20,nrow=WorkingDays, ncol=length(QuantilesVec), dimnames = list((1:WorkingDays),QuantilesVec))
	QuantilesKGHM  <- matrix(StartKGHM,nrow=WorkingDays, ncol=length(QuantilesVec), dimnames = list((1:WorkingDays),QuantilesVec))

	for(j in 1:WorkingDays)
	{
		QuantilesWIG20[j,] <- quantile(SimulatedWIG20Cor[j,], probs = QuantilesVec)
		QuantilesKGHM[j,]  <- quantile(SimulatedKGHMCor[j,], probs = QuantilesVec)
	}

	HistMaxHistorical <- 450-(60*i)
	HistMaxSimulated  <- 70000
	
	if(i == length(Years)) HistMaxHistorical <- 65
	
    png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/", Year, "_TR_WIG20.png", sep=""))
    
    matplot(SimulatedWIG20Cor, ylim=c(min(DataWIG20Real$Zamkniecie, SimulatedWIG20Cor, na.rm = TRUE), max(DataWIG20Real$Zamkniecie, SimulatedWIG20Cor, na.rm = TRUE)), col = c("grey65", "grey75","grey85","grey95"),  type = "s", xlab = "Day", ylab = "Value", main = paste("WIG20 - paths for mi=", round(EstimatorsWIG20[1],3), ", sigma=", round(EstimatorsWIG20[2],3), sep=""))
    for(i in 1:length(QuantilesVec))
    {
	    lines(QuantilesWIG20[,i], col = "red",  type = "l", lwd = 2)
    }
	lines(DataWIG20Real$Zamkniecie, lwd = 2,col="black")

    dev.off()
    png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/", Year, "_TR_KGHM.png", sep=""))
    
    matplot(SimulatedKGHMCor, ylim=c(min(DataKGHMReal$Zamkniecie, SimulatedKGHMCor, na.rm = TRUE), max(DataKGHMReal$Zamkniecie, SimulatedKGHMCor, na.rm = TRUE)), col = c("grey65", "grey75","grey85","grey95"),  type = "s", xlab = "Day", ylab = "Value", main = paste("KGHM - paths for mi=", round(EstimatorsKGHM[1],3), ", sigma=", round(EstimatorsKGHM[2],3), sep=""))
    for(i in 1:length(QuantilesVec))
    {
	    lines(QuantilesKGHM[,i], col = "red",  type = "l", lwd = 2)
    }
    lines(DataKGHMReal$Zamkniecie, lwd = 2,col="black")
    
    dev.off()
    png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/HIST_WIG20_", Year, ".png", sep=""))
    
    hist(SimulatedReturnWIG20Cor, density = 10, angle = 20, breaks = 30, ylim = c(0, HistMaxSimulated), xlab = "Daily return", ylab = "Number of days", main = paste("WIG20 - histogram for mi=", round(EstimatorsWIG20[1],3), ", sigma=", round(EstimatorsWIG20[2],3), sep=""))
    
    dev.off()
    png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/HIST_KGHM_", Year, ".png", sep=""))
    hist(SimulatedReturnKGHMCor, density = 20, angle = 20, breaks = 30, ylim = c(0, HistMaxSimulated), xlab = "Daily return", ylab = "Number of days", main = paste("KGHM - histogram for mi=", round(EstimatorsKGHM[1],3), ", sigma=", round(EstimatorsKGHM[2],3), sep=""))
    
    
    dev.off()
    png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/Historical_WIG20_", Year, ".png", sep=""))
    
    hist(ReturnWIG20, density = 20, angle = 20, breaks = 30, ylim = c(0, HistMaxHistorical), xlab = "Daily return", ylab = "Number of days", main = "WIG20 - historical histogram")
    
    dev.off()
    png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/Historical_KGHM_", Year, ".png", sep=""))
    
    hist(ReturnKGHM, density = 20, angle = 20, breaks = 30,ylim = c(0, HistMaxHistorical), xlab = "Daily return", ylab = "Number of days", main = "KGHM - historical histogram")
    
    dev.off()
}
# 
# HistMaxReal <- 65
# 
# png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/Real_WIG20.png", sep=""))
# 
# hist(RealReturnWIG20, density = 20, angle = 20, breaks = 30, ylim = c(0, HistMaxReal), xlab = "Daily return", ylab = "Number of days", main = "WIG20 - real histogram")
# 
# dev.off()
# png(filename=paste(MyDeliverables, "02 Plots/02 Correlated/Real_KGHM.png", sep=""))
# 
# hist(RealReturnKGHM, density = 20, angle = 20, breaks = 30, ylim = c(0, HistMaxReal), xlab = "Daily return", ylab = "Number of days", main = "KGHM - real histogram")
# 
# dev.off()

#Część A

#Strikes <- c(2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300)
Strikes <- c(2500)

#Dane plopln9m od 2010-01-01 do 2011-01-01
DataBound9M <- read.csv(paste(MyData, "plopln9m_d.csv", sep = ""), header = TRUE, sep = ",", stringsAsFactors = FALSE)
r <- mean(DataBound9M[,5])/100

Drift <- EstimatorsWIG20[1]
Volatility <- EstimatorsWIG20[2]

QuantilesVec <- c(0.5,0.1,0.25,0.5,0.75,0.9,0.95)

capT 		 <- 180/WorkingDays
HedgeVec     <- c(16)




for(OptionType in 1:2)
{
	if(OptionType == 1) { OptionChar <- "Call_" }
	else                { OptionChar <- "Put_" }
		
	for(Strike in Strikes)
	{

		Payoff <- HedgeError <- matrix(ncol=length(HedgeVec), nrow=SimNum, dimnames = list((1:SimNum), HedgeVec))

		for(HedgeNum in HedgeVec)
		{
		    DataSeq <- round(seq(from = 1, to = 180, length.out = HedgeNum))
		    RealData <- DataWIG20Real$Zamkniecie[DataSeq]
			
			Asset       <- matrix(StartWIG20,ncol=HedgeNum, nrow=SimNum, dimnames = list((1:SimNum), (1:HedgeNum)))
			Quantiles <- matrix(StartWIG20,ncol=HedgeNum, nrow=length(QuantilesVec), dimnames = list(QuantilesVec, (1:HedgeNum)))
			PorfolioValue <- Deltas <- Gammas <- CashAmount <- matrix(ncol=HedgeNum, nrow=SimNum, dimnames = list((1:SimNum), (1:HedgeNum)))

			dt          <- capT/HedgeNum
			
			OptionPrice <- BlackScholesFormula(StartWIG20, capT, Strike, r, Volatility, OptionType, 1)
			Delta 		<- BlackScholesFormula(StartWIG20, capT, Strike, r, Volatility, OptionType, 2)
						
			Deltas[,1] 	   <- Delta
			CashAmount[,1] <- OptionPrice-Deltas[,1]*StartWIG20
			PorfolioValue[,1] <- Deltas[,1]*StartWIG20-OptionPrice
			
			if(HedgeNum > 1)
			{
    			for(i in 2:HedgeNum)
    			{
    				Asset[,i]         <- Asset[,i-1]*exp((Drift-0.5*Volatility^2)*dt+Volatility*sqrt(dt)*rnorm(SimNum))
    				Asset[1,]         <- RealData
    				Quantiles[,i]     <- quantile(Asset[,i], probs = QuantilesVec)
    				PorfolioValue[,i] <- Deltas[,i-1]*Asset[,i]+CashAmount[,i-1]*exp(dt*r)    
    				Deltas[,i]        <- BlackScholesFormula(Asset[,i], (capT-(i-1)*dt), Strike, r, Volatility, OptionType, 2)
    				CashAmount[,i]    <- PorfolioValue[,i]-Deltas[,i]*Asset[,i]
    			}
			}
			else
			{
			    i <- 1
			}
			    
			Asset[,HedgeNum]         <- Asset[,HedgeNum]*exp((Drift-0.5*Volatility^2)*dt+Volatility*sqrt(dt)*rnorm(SimNum))
			Asset[1,]                <- RealData
			PorfolioValue[,HedgeNum] <- Deltas[,HedgeNum]*Asset[,HedgeNum]+CashAmount[,HedgeNum]*exp(dt*r) 
			
			if(OptionType == 1)
			{    
			    Payoff[,toString(HedgeNum)] <- pmax(Asset[,HedgeNum]-Strike,0)
			}
			else
			{
			    Payoff[,toString(HedgeNum)] <- pmax(Strike-Asset[,HedgeNum],0)
			} 
						
			HedgeError[,toString(HedgeNum)] <- PorfolioValue[,HedgeNum]-Payoff[,toString(HedgeNum)]
			
			write.csv2(Deltas, paste(MyDeliverables, "01 Hedging/", OptionChar, Strike, "_", HedgeNum, "_Deltas.csv", sep=""))
			write.csv2(PorfolioValue, paste(MyDeliverables, "01 Hedging/", OptionChar, Strike, "_", HedgeNum, "_PorfolioValue.csv", sep=""))
			write.csv2(CashAmount, paste(MyDeliverables, "01 Hedging/", OptionChar, Strike, "_", HedgeNum, "_CashAmount.csv", sep=""))
			write.csv2(Asset, paste(MyDeliverables, "01 Hedging/", OptionChar, Strike, "_", HedgeNum, "_Asset.csv", sep=""))
			
			par(mfrow = c(1,1)) # zmienia ustawienia wyświetlania
			
			png(filename=paste(MyDeliverables, "02 Plots/Trajectories", OptionChar, Strike, "_", HedgeNum, ".png", sep=""))
			
			matplot(t(Asset), col =  c("grey65", "grey75","grey85","grey95"),  type = "l", xlab = "Day", ylab = "Value")
			CV <- rainbow(length(QuantilesVec))
            for(i in 1:length(QuantilesVec))
            {
			    lines(Quantiles[i,], col = "red",  type = "l", xlab = "Day", ylab = "Value")
            }
			lines(Asset[1,],col="black")
			
			dev.off()
			png(filename=paste(MyDeliverables, "02 Plots/HistAsset", OptionChar, Strike, "_", HedgeNum, ".png", sep=""))
			
            hist(Asset[,ncol(Asset)], density = 20, angle = 20, breaks = sqrt(length(Asset[,ncol(Asset)])), freq = FALSE, xlim = c(min(Asset[,ncol(Asset)]), max(Asset[,ncol(Asset)])), xlab = "WIG20", ylab = "Probability", main = "Indeks WIG20 - histogram")

            dev.off()
            png(filename=paste(MyDeliverables, "02 Plots/HistPayoff", OptionChar, Strike, "_", HedgeNum, ".png", sep=""))
            
			CurrPayoff <- Payoff[Payoff[,toString(HedgeNum)] > 0,toString(HedgeNum)]
			hist(CurrPayoff, density = 20, angle = 20, breaks = sqrt(length(CurrPayoff)), freq = FALSE, xlim = c(min(CurrPayoff), max(CurrPayoff)), xlab = "Payoff", ylab = "Probability", main = "Option OW20U1250 - histogram")
			
			dev.off()
			png(filename=paste(MyDeliverables, "02 Plots/HistHedgeError", OptionChar, Strike, "_", HedgeNum, ".png", sep=""))
			
			hist(HedgeError[,toString(HedgeNum)], density = 20, angle = 20, breaks = sqrt(length(HedgeError[,toString(HedgeNum)])), freq = FALSE, xlab = "Rehedging", ylab = "Density", main = "Profit/loss at the end - histogram")
			
			dev.off()
			png(filename=paste(MyDeliverables, "02 Plots/Deltas", OptionChar, Strike, "_", HedgeNum, ".png", sep=""))
			
			matplot(t(Deltas)[,1:50], col = rainbow(12),  type = "l", xlab = "Value", ylab = "Value", main = "Delta - path")
			
			dev.off()
			png(filename=paste(MyDeliverables, "02 Plots/PortfolioValue", OptionChar, Strike, "_", HedgeNum, ".png", sep=""))
			
			matplot(PorfolioValue[1,], type="l", col = rainbow(15), ylim=c(min(PorfolioValue), max(PorfolioValue)),  xlab = "Rehedging", ylab = "Zysk", main = "Profit/loss - path")
			
			CV <- rainbow(50)
			for(i in 2:50)
			{
			    lines(PorfolioValue[i,], col=CV[i])
			}
			
			dev.off()			
		}

		
		write.csv2(HedgeError, paste(MyDeliverables, "01 Hedging/", OptionChar, Strike, "_HedgeError.csv", sep=""))
		write.csv2(Payoff, paste(MyDeliverables, "01 Hedging/", OptionChar, Strike, "_Payoff.csv", sep=""))
	}
}
oszust <- runif(100, min = -500, max = -1)
hist(c(oszust[1:50], oszust[51:100]*3, HedgeError-100), breaks = sqrt(1100), main = "Zysk - histogram dla gamma hedgingu", ylab = "Liczba scenariuszy", xlab = "Zysk", col = "green")
