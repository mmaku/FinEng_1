# Second point

USDPLN <- read.csv("usdpln_d.csv")
GOLD_USD <- read.csv("xauusd_d.csv")
USDPLN <- USDPLN$Zamkniecie
GOLD_USD<- GOLD_USD$Zamkniecie
GOLD_PLN<-USDPLN*GOLD_USD


rho <- cor(USDPLN, GOLD_PLN)
X <- cbind(USDPLN, GOLD_PLN)
result <- simulation(X, 10^4, n = 250)
X <- result$simulation_res
sigma <- result$sigma
sigma_X <- sigma[1]
sigma_Y <- sigma[2]

T<-0.5
r_PLN<-0.049
r_f<-0.0016950
sigma_X<-0.196
sigma_Y<-0.263
w<-exp(-T*(r_PLN-r_f-sigma_X^2+rho*sigma_Y*sigma_X))
V0<-100*w

# rPLN change
delta_r_PLN<-c(0.02,0.015,0.01,0.005,0,-0.005,-0.01,-0.015,-0.02)
r_PLN_wek<-r_PLN+delta_r_PLN
r_PLN_proc<-delta_r_PLN/r_PLN*100
deltaVrPLN<-100*exp((-1)*T*(r_PLN_wek-r_f-sigma_X^2+rho*sigma_Y*sigma_X))-V0

VrPLNproc<-deltaVrPLN/V0*100
# qplot(r_PLN_proc,VrPLNproc,size=1,xlab = "rPLN change [%]",ylab="Price change [%]",main="Price volatility due to rPLN change")+ theme(legend.position = "none")
write.csv(cbind(r_PLN_proc,VrPLNproc),paste(MyDeliverables,"2_rPLN",sep=""))

# rf change - same as in rPLN

# sigma_X change
delta_sigma_X<-c(0.1,0.05,0.02,0.01,0,-0.01,-0.02,-0.05,-0.1)
sigma_X_wek<-sigma_X+delta_sigma_X
sigma_X_proc<-delta_sigma_X/sigma_X*100
deltasigmaX<-100*exp((-1)*T*(r_PLN-r_f-sigma_X_wek^2+rho*sigma_Y*sigma_X_wek))-V0
VsigmaXproc<-deltasigmaX/V0*100
# qplot(sigma_X_proc,VsigmaXproc,size=1,xlab = "sigmaX change [%]",ylab="Price change [%]",main="Price volatility due to sigmaX change")+ theme(legend.position = "none")
write.csv(cbind(sigma_X_proc,VsigmaXproc),paste(MyDeliverables,"2_sigma_X",sep=""))


# sigma_Y change
delta_sigma_Y<-c(0.1,0.05,0.02,0.01,0,-0.01,-0.02,-0.05,-0.1)
sigma_Y_wek<-sigma_Y+delta_sigma_Y
sigma_Y_proc<-delta_sigma_Y/sigma_Y*100
deltasigmaY<-100*exp((-1)*T*(r_PLN-r_f-sigma_X^2+rho*sigma_Y_wek*sigma_X))-V0
VsigmaYproc<-deltasigmaY/V0*100
# qplot(sigma_Y_proc,VsigmaYproc,size=1,xlab = "sigmaY change [%]",ylab="Price change [%]",main="Price volatility due to sigmaY change")+ theme(legend.position = "none")
write.csv(cbind(sigma_Y_proc,VsigmaYproc),paste(MyDeliverables,"2_sigma_Y",sep=""))


# rho change
delta_rho<-c(0.2,0.1,0.05,0,-0.05,-0.1,-0.2,-0.3,-0.4)
rho_wek<-rho+delta_rho
rho_proc<-delta_rho/rho*100
deltarho<-100*exp((-1)*T*(r_PLN-r_f-sigma_X^2+rho_wek*sigma_Y*sigma_X))-V0
Vrhoproc<-deltarho/V0*100
# qplot(rho_proc,Vrhoproc,size=1,xlab = "Correlation change [%]",ylab="Price change [%]",main="Price volatility due to correlation change")+ theme(legend.position = "none")
write.csv(cbind(rho_proc,Vrhoproc),paste(MyDeliverables,"2_rho",sep=""))

# heatmatp
sigmaX_seq<-seq(from = sigma_X*2/3, to = sigma_X*3/2, length.out = 100)
sigmaY_seq<-seq(from = sigma_Y*2/3, to = sigma_Y*3/2, length.out = 100)

# gold_BEG <- tail(GOLD_PLN, n=1)
# USD_BEG <- tail(USDPLN, n=1)
# 
# sigmaX_seq<-seq(from = sigma_X*2/3, to = sigma_X*3/2, length.out = 100)
# sigmaY_seq<-seq(from = sigma_Y*2/3, to = sigma_Y*3/2, length.out = 100)

macierz <- matrix(0, nrow = 100, ncol =100, dimnames = list(sigmaX_seq, sigmaY_seq))

for(i in 1:100)
{
    for(j in 1:100)
    {
        macierz[i,j] <- (100)*exp(-(r_PLN-r_f-sigmaX_seq[i]^2+rho*sigmaY_seq[j]*sigmaX_seq[i])) 
    }
}
write.csv(macierz,paste(MyDeliverables,"2_MACsigma",sep=""))

plot <- plot_ly(z = macierz, x = sigmaY_seq, y = sigmaX_seq, type = "surface")
plotly_POST(plot, filename = "2_MACsigma", world_readable=TRUE)

