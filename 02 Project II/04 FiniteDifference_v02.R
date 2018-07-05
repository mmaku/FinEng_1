rm(list = ls()) 

# install.packages("ggplot2")
library(ggplot2)
library(reshape2)

# Option'Asset parameters
Strike <- 2400
o <- 0.05
r <- 0.05
TimeHorizon <- 1
Type <- "Call"          # Call or Put

Boundary <- FALSE
BoundaryLevel <- 3000

# Finite diferences parameters

I <- 100
TimeStep <- 0.9/(o^2*I^2)
K <- floor(TimeHorizon/TimeStep)+1
TimeStep <- TimeHorizon/K

Value <- Delta <- Gamma <- Theta <- matrix(0, nrow = I+1, ncol = K+1)       # R indexes from 1 not from 0

Asset <- matrix(AssetPattern, nrow = I+1, ncol = K+1, byrow = FALSE) 
# payoff initialization
for(i in 0:I) 
{
    if(Type=="Call") 
    {
        Value[i+1, 1] <- max(0, Asset[i+1,1]-Strike)       # call option
    } else 
    {
        Value[i+1, 1] <- max(0, Strike-Asset[i+1,1])       # put option
    }   
}

rownames(Value) <- AssetPattern
# colnames(Value) < - round(seq(from = 0, to = TimeHorizon, length.out = K+1), digits = 2)

# Asset = 0 row
for(k in 1:K) {
    Value[1, k+1] <- Value[1, k]*(1-r*TimeStep)
}


# Boundary 
I0 <- 1
if(Boundary==TRUE) 
{
    if(Type=="Call") 
    {
        Value[I+1,] <- 0
    } 
    else             # Put option
    {    
        Value[1,] <- 0
    }
}

for(k in 1:K)          # loop over all columns
{
    for(i in 2:I) 
    {
        Value[i, k+1] <- Value[i,k]*(2*AssetStep^2-o^2*2*TimeStep*AssetPattern[i]^2-r) + 
            Value[i+1,k]*(o^2*AssetPattern[i]^2*TimeStep+r*AssetPattern[i]*TimeStep*AssetStep) + 
            Value[i-1, k]*(o^2*AssetPattern[i]^2*TimeStep-r*AssetPattern[i]*TimeStep*AssetStep) 
        Value[i, k+1] <- Value[i, k+1]/(2*AssetStep^2)
    }
    if(Boundary==FALSE || Type=="Put") 
    {
        # infinity condition
        Value[I+1,k+1] <- 2*Value[I+1, k+1] - Value[I,k+1]
    }
}


ggplot(melt(t(Value)), aes(x=Var1, y=Var2, fill=value)) + geom_raster() + ylab("Asset Value") + xlab("Time")
