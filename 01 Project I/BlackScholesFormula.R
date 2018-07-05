#source("BlackScholesFormula.R")

#BlackScholesFormula(S(t), T-t, Strike, r, Sigma, OptionType=1, GreekType=1)
# optype: 1~call, 2~put, 3~binary call, 4~binary put
# greektype: 1~price, 2~delta, 3~gamma

BlackScholesFormula  <- function(Spot, TimeToMaturity, Strike, r, Sigma, OptionType=1, GreekType=1)
{ 
		
	d1 <- (log(Spot/Strike)+(r+0.5*Sigma^2)*TimeToMaturity)/(Sigma*sqrt(TimeToMaturity))
	d2 <- d1-Sigma*sqrt(TimeToMaturity)

	if (OptionType==1 && GreekType==1) result <- Spot*pnorm(d1)-Strike*exp(-r*TimeToMaturity)*pnorm(d2)
	if (OptionType==2 && GreekType==1) result <- Spot*pnorm(d1)-Strike*exp(-r*TimeToMaturity)*pnorm(d2)-Spot+Strike*exp(-r*TimeToMaturity)
	if (OptionType==3 && GreekType==1) result <- exp(-r*TimeToMaturity)*pnorm(d2)
	if (OptionType==1 && GreekType==2) result <- pnorm(d1)
	if (OptionType==2 && GreekType==2) result <- pnorm(d1)-1
	if (OptionType==3 && GreekType==2) result <- exp(-r*TimeToMaturity)*dnorm(d2)/(Spot*Sigma*sqrt(TimeToMaturity))
	if (OptionType==3 && GreekType==3) result <- -exp(-r*TimeToMaturity)*dnorm(d2)*d1/(Spot^2*Sigma^2*TimeToMaturity)
	if (OptionType!=3 && GreekType==3) result <- dnorm(d1)/(Spot*Sigma*sqrt(TimeToMaturity))

	BlackScholesFormula<-result
}

BlackScholesImpVol  <- function (obsprice,Spot,TimeToMaturity,Strike,r, q=0, OptionType=1)
{ 
	difference<- function(sigBS, obsprice,Spot,TimeToMaturity,Strike,r,q,OptionType)
    {
		BlackScholesFormula (Spot,TimeToMaturity,Strike,r,sigBS, OptionType,1)-obsprice
    }

    uniroot(difference, c(0,3),obsprice=obsprice,Spot=Spot,TimeToMaturity=TimeToMaturity,Strike=Strike,r=r,OptionType=OptionType)$root
}
