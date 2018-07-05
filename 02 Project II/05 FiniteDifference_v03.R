# Option's parameters
E <- 2400
o <- 0.2
o_min <- 0.2
o_max <- 0.3
r <- 0.05
T_ex <- 1 
type <- "Call"          # Call or Put
D <- 30                  # divident

boundary <- FALSE
B <- 2600

# Finite diferences parameters
I <- 30
dS <- 2*E/I
dt <- 0.9/(o^2*I^2)
K <- floor(T_ex/dt)+1
# even K to have divident payment on the grid
if(K %% 2 != 0) K <- K+1
divident_time <- K/2
dt <- T_ex/K

if( boundary == TRUE) {
    if(type=="Call") {
        dS <- B/I
    } else {            # put option
        dS <- (2*E-B)/I
    }
}

V <- matrix(0, nrow=I+1, K+1)       # R indexes from 1 not from 0
G <- matrix(0, nrow=I+1, K+1)       # matrix for gamma
delta <- matrix(0, nrow=I+1, K+1)   # matrix for delta
S <- numeric(I+1)

# payoff initialization
for(i in 0:I) {
    # to ensure that grid starts/ends with boundary
    if(boundary==TRUE && type=="Put") {
        S[i+1] <- dS*i + B
    } else {
        S[i+1] <- dS*i    
    }
    if(type=="Call") {
        V[i+1, 1] <- max(0, S[i+1]-E)       # call option
    } else {
        V[i+1, 1] <- max(0, E-S[i+1])       # put option
    }   
}

if(boundary == TRUE) {
    if(type=="Call") {
        V[I+1,1] <- 0
    } else {            # put option
        V[1,1] <- 0
    }
}

rownames(V) <- S
rownames(G) <- S
rownames(delta) <- S

# S=0 row
if(boundary==FALSE || type="Call") {
    for(k in 1:K) {
        V[1, k+1] <- V[1, k]*(1-r*dt)
    }
}

# boundary 
# I0 <- 1
# if(boundary==TRUE) {
#     if(type=="Call") {        
#         I <- which(S == B) - 1
#         V[as.character(B),] <- 0
#         V <- V[1:(I+1), ]
#         I0 <- 1
#     } else {                # Put option
#         I0 <- which(S == B)
#         V[as.character(B),] <- 0
#         V <- V[I0:(I+1), ]
#     }
# }

for(k in 1:K) {         # loop over all columns
    for(i in 2:I) {
        
        # calculate gamma
        G[i,k] <- (V[i+1,k] - 2*V[i,k] + V[i-1,k])/dS^2
        
        # calculate delta
        delta[i,k] <- (V[i+1,k] - V[i-1,k])/(2*dS)
        
        # use proper volatility
        if(G[i,k] > 0) {
            o <- o_max
        } else {
            o <- o_min
        }
        # divident treatment
        s <- S[i]
        if(k >= divident_time) {
            s <- s + D             # note that we go back in time therefore +D
        }
        # calculate option value
        V[i, k+1] <- V[i,k]*(2*dS^2-o^2*2*dt*s^2-r) + 
                     V[i+1,k]*(o^2*s^2*dt+r*s*dt*dS) + 
                     V[i-1, k]*(o^2*s^2*dt-r*s*dt*dS) 
        V[i, k+1] <- V[i, k+1]/(2*dS^2)   
    }
    if(boundary==FALSE || type=="Put") {
        # infinity condition
        V[I+1,k+1] <- 2*V[I, k+1] - V[I-1,k+1]
    }
}



