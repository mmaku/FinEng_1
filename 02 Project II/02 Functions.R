# source("02 Functions.R")
Payoff <- function(S_ = 100, P_ = 100, T_ = "Call")
{
    if(T_ == "Call") 
    {
        out <- max(0, P_-S_)       # call option
    } else 
    {
        out <- max(0, S_-P_)       # put option
    }
    
    return(out)
}

CreatAssetMatrix <- function(Strike_ = 2500,
                             Boundary_ = FALSE,
                             BoundaryLevel_ = 3*Strike_,
                             Type_ = "Call",
                             DyvidendLevel_ = 100,
                             DyvidentTime_ = 50,
                             I_ = 100,
                             K_ = 100)
{
    if(Boundary_ && Type_ == "Call")
    {
        # BoundaryLevel_ <- BoundaryLevel_ + 
        AssetStep <- BoundaryLevel_/I_ 
        AssetPattern <- seq(from = 0, to = BoundaryLevel_, length.out = I+1)
    } else if(Boundary_ && Type_ == "Put")
    {
        AssetStep <- (3*Strike_ - BoundaryLevel_)/I_ 
        AssetPattern <- seq(from = BoundaryLevel_, to = 3*Strike_, length.out = I+1)
        
    } else 
    {
        AssetStep <- 3*Strike_/I_
        AssetPattern <- seq(from = 0, to = 3*Strike_, length.out = I+1)
    }
    
    out <- matrix(AssetPattern, nrow = I_+1, ncol = K_+1, byrow = FALSE, dimnames = list(c(0:I_),c(0:K_)))
    
    for(i in DyvidentTime_:K_+1)
    {
        out[,i] <- out[,i] + DyvidendLevel_
    }   
    
    return(out)
}

CreatePriceMatrix <- function(Strike_ = 2500,
                              Boundary_ = FALSE,
                              BoundaryLevel_ = 3*Strike_,
                              Type_ = "Call",
                              AssetMatrix_,
                              AssetStep_,
                              TimeStep_,
                              Volatility_ = c(0.2,0.3),
                              r_ = 0.05,
                              AmericanType_ = FALSE)
{
    I <- nrow(AssetMatrix_)-1
    K <- ncol(AssetMatrix_)-1
    
    out <- matrix(0, nrow = I+1, ncol = K+1, dimnames = list(c(0:I),c(0:K)))
    
    for(i in 0:I) 
    {
        out[i+1, 1] <- Payoff(S_ = Strike_, P_ = AssetMatrix_[i+1, 1], T_ = Type_)
    }
    
    if(Boundary == TRUE) 
    {
        if(Type == "Call") 
        {
            out[I+1,] <- 0
        } 
        else             # Put option
        {    
            out[1,] <- 0
        }
    }
    
    for(k in 1:K)          # loop over all columns
    {
        AssetPattern <- AssetMatrix_[,k]
        
        for(i in 2:I) 
        {
            G <- (out[i+1,k] - 2*out[i,k] + out[i-1,k])/AssetStep_^2
            
            # Check volatility
            if(G > 0) 
            {
                Volatility <- Volatility_[2]
            } else {
                Volatility <- Volatility_[1]
            }
            
            out[i,k+1] <- out[i,k]*(2*AssetStep_^2-Volatility^2*2*TimeStep_*AssetPattern[i]^2-r*2*TimeStep_*AssetStep_^2) + 
                          out[i+1,k]*(Volatility^2*AssetPattern[i]^2*TimeStep_+r_*AssetPattern[i]*TimeStep_*AssetStep_) + 
                          out[i-1,k]*(Volatility^2*AssetPattern[i]^2*TimeStep_-r_*AssetPattern[i]*TimeStep_*AssetStep_) 
            
            out[i,k+1] <- out[i,k+1]/(2*AssetStep_^2)
        }
        
        if(Type_ == "Call") 
        {
            out[1,k+1] <- 2*out[2, k+1] - out[3,k+1]
        }
        
        if(!Boundary_ || Type_ == "Put") 
        {
            out[I+1,k+1] <- 2*out[I, k+1] - out[I-1,k+1]
        }
        
        if(AmericanType_) 
        {
            for(i in 1:I)
            {
                out[i,k+1] <- max(out[i,k+1],Payoff(S_ = Strike_, P_ = AssetMatrix_[i,k+1], T_ = Type_))
                if(Type_ == "Call" && Boundary_ && AssetMatrix_[i,k+1] >= BoundaryLevel_) 
                {
                    out[i,k+1] <- 0
                }
                if(Type_ == "Put" && Boundary_ && AssetMatrix_[i,k+1] <= BoundaryLevel_) 
                {
                    out[i,k+1] <- 0
                }
            }
        }
    }
    
    return(out)
}

CreateGammaMatrix <- function(ValueMatrix_,
                              AssetStep_)
{
    I <- nrow(ValueMatrix_)-1
    K <- ncol(ValueMatrix_)-1
    
    out <- matrix(0, nrow = I+1, ncol = K+1, dimnames = list(c(0:I),c(0:K)))
    
    for(k in 1:K) 
    {        
        for(i in 2:I) 
        {
            out[i,k] <- (ValueMatrix_[i+1,k] - 2*ValueMatrix_[i,k] + ValueMatrix_[i-1,k])/AssetStep_^2
        }
    }
    
    return(out)
}

CreateDeltaMatrix <- function(ValueMatrix_,
                              AssetStep_)
{
    I <- nrow(ValueMatrix_)-1
    K <- ncol(ValueMatrix_)-1
    
    out <- matrix(0, nrow = I+1, ncol = K+1, dimnames = list(c(0:I),c(0:K)))
    
    for(k in 2:K-1) 
    {        
        for(i in 3:I-1) 
        {
            out[i,k] <- (ValueMatrix_[i+1,k] - ValueMatrix_[i-1,k])/(2*AssetStep_)
        }
    }
    
    return(out)
}


CreateDeltaSignMatrix <- function(ValueMatrix_,
                              AssetStep_)
{
    I <- nrow(ValueMatrix_)-1
    K <- ncol(ValueMatrix_)-1
    
    out <- matrix(0, nrow = I+1, ncol = K+1, dimnames = list(c(0:I),c(0:K)))
    
    for(k in 2:K-1) 
    {        
        for(i in 3:I-1) 
        {
            Sign <- (ValueMatrix_[i+1,k] - ValueMatrix_[i-1,k])/(2*AssetStep_)
            if(Sign < 0)
            {
                out[i,k] <- -1
            } else {
                out[i,k] <- 1
            }
        }
    }
    
    return(out)
}


BlackScholesFormula <- function(Spot, TimeToMaturity, Strike, r, Sigma, OptionType=1, GreekType=1)
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

# ListApprox <- function(Value1_, Value2_)
# {
#     out <- which.min(abs(Value1_ - Value2_)) 
#     return(out)
# }
 
                                                   