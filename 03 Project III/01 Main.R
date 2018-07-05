rm(list = ls()) 

Timer <- proc.time()

# install.packages("ggplot2")
# install.packages("plotly")

library(ggplot2)
library(plotly)

Sys.setenv("plotly_username"="michalski")
Sys.setenv("plotly_api_key"="lmp5fdt93r")


# Working directory
MyPath <- "C:/Users/mchl/Dropbox/01 Studia/01 UWr/01 Szkoła - projekty/09 IF/04 Projekt III/" 
MyDeliverables <- paste(MyPath,"01 Deliverables/", sep="") 
setwd(MyPath)

# Load source files
source("02 Functions.R")

# parameters
sim_num = 10000

# simulations first point

USDPLN <- read.csv("usdpln_d.csv")
GOLD_USD <- read.csv("xauusd_d.csv")
USDPLN <- USDPLN$Zamkniecie
GOLD_USD<- GOLD_USD$Zamkniecie
GOLD_PLN<-USDPLN*GOLD_USD


rho <- cor(USDPLN, GOLD_USD)
X <- cbind(USDPLN, GOLD_USD)
result <- simulation(X, 10^4, n = 250)
X <- result$simulation_res
# write.csv(X[[1]][seq(from = 1, to =25, by =1)],paste(MyDeliverables,"0_Motions_USDPLN",sep=""))
# write.csv(X[[2]][seq(from = 1, to =25, by =1)],paste(MyDeliverables,"0_Motions_GOLDUSD",sep=""))
sigma <- result$sigma
sigma_X <- sigma[1]
sigma_Y <- sigma[2]
delta(X[[1]][,2], X[[2]][,2], 1, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,1)[1,]

rho <- cor(USDPLN, GOLD_PLN)
X <- cbind(USDPLN, GOLD_PLN)
result <- simulation(X, 10^4, n = 250)
X <- result$simulation_res
# write.csv(X[[1]][seq(from = 1, to =25, by =1)],paste(MyDeliverables,"0_Motions_USDPLN",sep=""))
# write.csv(X[[2]][seq(from = 1, to =25, by =1)],paste(MyDeliverables,"0_Motions_GOLDUSD",sep=""))
sigma <- result$sigma
sigma_X <- sigma[1]
sigma_Y <- sigma[2]

delta(X[[1]][,2], X[[2]][,2], 1, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,2)[1,]


rm(result)
sigma
x1 <- 1:(sim_num-1)
L1 <- rep(list(data.frame()),sim_num)
x7 <- 1:(sim_num-1)
L7 <- rep(list(data.frame()),sim_num)
x30 <- 1:(sim_num-1)
L30 <- rep(list(data.frame()),sim_num)
x90 <- 1:(sim_num-1)
L90 <- rep(list(data.frame()),sim_num)

for(i in 2:sim_num)
{
    L1[[i]] <- delta(X[[1]][,i], X[[2]][,i], 1, 0.0016950, 0.049, sigma_Y,sigma_X,rho,250,1)
    x1[i-1] <- L1[[i]][251,8]
    L7[[i]] <- delta(X[[1]][,i], X[[2]][,i], 7, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,1)
    x7[i-1] <- L7[[i]][251,8]
    L30[[i]] <- delta(X[[1]][,i], X[[2]][,i], 30, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,1)
    x30[i-1] <- L30[[i]][251,8]
    L90[[i]] <- delta(X[[1]][,i], X[[2]][,i], 90, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,1)
    x90[i-1] <- L90[[i]][251,8]
}

write.csv(x1,paste(MyDeliverables,"1_Daily_dupa2",sep=""))
write.csv(x7,paste(MyDeliverables,"1_Weekly",sep=""))
write.csv(x30,paste(MyDeliverables,"1_Monthly",sep=""))
write.csv(x90,paste(MyDeliverables,"1_Quaterly",sep=""))
write.csv(cbind(x1,x7,x30,x90),paste(MyDeliverables,"1_All",sep=""))
write.csv(L1[[50]],paste(MyDeliverables,"1_All_L1",sep=""))
write.csv(L7[[50]],paste(MyDeliverables,"1_All_L7",sep=""))



# simulations second point

rho <- cor(USDPLN, GOLD_PLN)
X <- cbind(USDPLN, GOLD_PLN)
result <- simulation(X, 10^4, n = 250)
X <- result$simulation_res
sigma <- result$sigma
sigma_X <- sigma[1]
sigma_Y <- sigma[2]
rm(result)

x1 <- 1:(sim_num-1)
L1 <- rep(list(data.frame()),sim_num)
x7 <- 1:(sim_num-1)
L7 <- rep(list(data.frame()),sim_num)
x30 <- 1:(sim_num-1)
L30 <- rep(list(data.frame()),sim_num)
x90 <- 1:(sim_num-1)
L90 <- rep(list(data.frame()),sim_num)

for(i in 2:sim_num)
{
    L1[[i]] <- delta(X[[1]][,i], X[[2]][,i], 1, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,2)
    x1[i-1] <- L1[[i]][251,8]
    L7[[i]] <- delta(X[[1]][,i], X[[2]][,i], 7, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,2)
    x7[i-1] <- L7[[i]][251,8]
    L30[[i]] <- delta(X[[1]][,i], X[[2]][,i], 30, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,2)
    x30[i-1] <- L30[[i]][251,8]
    L90[[i]] <- delta(X[[1]][,i], X[[2]][,i], 90, 0.049, 0.0016950, sigma_Y,sigma_X,rho,250,2)
    x90[i-1] <- L90[[i]][251,8]
}

write.csv(x1,paste(MyDeliverables,"2_Daily",sep=""))
write.csv(x7,paste(MyDeliverables,"2_Weekly",sep=""))
write.csv(x30,paste(MyDeliverables,"2_Monthly",sep=""))
write.csv(x90,paste(MyDeliverables,"2_Quaterly",sep=""))
write.csv(cbind(x1,x7,x30,x90),paste(MyDeliverables,"2_All",sep=""))
write.csv(L1[[50]],paste(MyDeliverables,"2_All_L1",sep=""))
write.csv(L7[[50]],paste(MyDeliverables,"2_All_L7",sep=""))


# Historical data
USDPLN2 <- read.csv("usdpln_d2.csv")
GOLD_USD2 <- read.csv("xauusd_d2.csv")
USDPLN2<- USDPLN2$Zamkniecie
GOLD_USD2 <- GOLD_USD2$Zamkniecie
GOLD_PLN2<-USDPLN2*GOLD_USD2

rho <- cor(USDPLN2, GOLD_USD2)
X <- cbind(USDPLN2, GOLD_USD2)
result <- simulation(X, 10^4, n = 250)
X <- result$simulation_res
sigma <- result$sigma
sigma_X <- sigma[1]
sigma_Y <- sigma[2]

wyniki<-1
k<-1
K1 <- K2 <- 1
# K2 <- rep(list(data.frame()),124)
for(i in 1:256)
{
    K1<- delta(USDPLN2, GOLD_USD2,i, 0.049, 0.0016950, sigma_Y,sigma_X,rho,256,1)
    wyniki[k] <- K1[256,8]
    k<-k+1
}
macierz1 <- delta(USDPLN2,GOLD_USD2 ,1, 0.049, 0.0016950, sigma_Y,sigma_X,rho,256,1)

write.csv(wyniki,paste(MyDeliverables,"1_HISTORICAL",sep=""))
write.csv(macierz1,paste(MyDeliverables,"1_HISTORICAL_HEDGNUM",sep=""))

rho <- cor(USDPLN2, GOLD_PLN2)
X <- cbind(USDPLN2, GOLD_PLN2)
result <- simulation(X, 10^4, n = 250)
X <- result$simulation_res
sigma <- result$sigma
sigma_X <- sigma[1]
sigma_Y <- sigma[2]

wyniki<-k<-1
for(i in 1:256)
{
    K2<- delta(USDPLN2, GOLD_PLN2,i, 0.049, 0.0016950, sigma_Y,sigma_X,rho,256,2)
    wyniki[k] <- K2[256,8]
    k<-k+1
}
macierz2 <- delta(USDPLN2,GOLD_PLN2 ,1, 0.049, 0.0016950, sigma_Y,sigma_X,rho,256,2)

write.csv(wyniki,paste(MyDeliverables,"2_HISTORICAL",sep=""))
write.csv(macierz2,paste(MyDeliverables,"2_HISTORICAL_HEDGNUM",sep=""))



# qplot(1:124,wyniki,xlab = "Ilość dni dzielących kolejne rehedgingi",ylab="Zysk/strata",main="Zyski/straty dla danych historycznych")
# macierz <- delta(GOLD_USD2,USDPLN2 ,1, 0.0502, 0.00206, sigma_Y,sigma_X,rho,248)
# delty<-data.frame(d = 1:248, delta_gold = macierz[,4], delta_kurs = macierz[,5])
# library(reshape2)
# delty_melted<-melt(delty, id.vars = "d")
# ggplot() + geom_line(data=delty_melted, aes(x=d, y=value, group=variable, color=factor(variable, labels=c("Delta złota", "Delta kursu USDPLN"))), size=0.7)

# write.csv(cbind(macierz ,GOLD_USD2, USDPLN2),paste(MyDeliverables,"All.csv",sep=""))

# qplot(1:248,macierz[,4],xlab = "Czas",ylab="Delta złota",main="Zmiana w czasie delty złota dla codziennych rehedgingów",geom="line",color=I("darkblue"))
# qplot(1:248,macierz[,5],xlab = "Czas",ylab="Delta kursu wymiany",main="Zmiana w czasie delty kursu wymiany dla codziennych rehedgingów",geom="line",color=I("darkblue"))
