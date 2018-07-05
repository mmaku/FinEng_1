rm(list = ls()) 

# install.packages("ggplot2")
library(ggplot2)
library(reshape2)

# Option's parameters
E <- 2400
o <- 0.2
r <- 0.05
T_ex <- 0.5
type <- "Call"          # Call or Put

boundary <- FALSE
B <- 2600

# Finite diferences parameters
I <- 100
dS <- 2*E/I
dt <- 0.9/(o^2*I^2)
K <- floor(T_ex/dt)+1
dt <- T_ex/K


V <- matrix(0, nrow=I+1, K+1)       # R indexes from 1 not from 0
S <- numeric(I+1)

# payoff initialization
for(i in 0:I) {
    S[i+1] <- dS*i    
    if(type=="Call") {
        V[i+1, 1] <- max(0, S[i+1]-E)       # call option
    } else {
        V[i+1, 1] <- max(0, E-S[i+1])       # put option
    }   
}
rownames(V) <- S

# S=0 row
for(k in 1:K) {
        V[1, k+1] <- V[1, k]*(1-r*dt)
}


# boundary 
I0 <- 1
if(boundary==TRUE) {
    if(type=="Call") {
        I <- which(S == B) - 1
        V[as.character(B),] <- 0
        V <- V[1:(I+1), ]
        I0 <- 1
    } else {                # Put option
        I0 <- which(S == B)
        V[as.character(B),] <- 0
        V <- V[I0:(I+1), ]
    }
}

for(k in 1:K) {         # loop over all columns
    for(i in 2:(I-I0+1)) {
        V[i, k+1] <- V[i,k]*(2*dS^2-o^2*2*dt*S[i]^2-r) + 
                     V[i+1,k]*(o^2*S[i]^2*dt+r*S[i]*dt*dS) + 
                      V[i-1, k]*(o^2*S[i]^2*dt-r*S[i]*dt*dS) 
        V[i, k+1] <- V[i, k+1]/(2*dS^2)
    }
    if(boundary==FALSE || type=="Put") {
        # infinity condition
        V[(I-I0+1)+1,k+1] <- 2*V[I-I0+1, k+1] - V[(I-I0+1)-1,k+1]
    }
}

ggplot(melt(t(V)), aes(x=Var1, y=Var2, fill=value)) + geom_raster() + ylab("Asset Value") + xlab("Time")

