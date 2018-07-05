rm(list = ls()) 

# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("RSEIS")

library(ggplot2)
library(plotly)
library(RSEIS)

# set plotly connection!!!

# Working directory
MyPath <- "C:/Users/mchl/Dropbox/01 UWr/01 Szkoła - projekty/09 IF/03 Projekt II/" 
MyDeliverables <- paste(MyPath,"01 Deliverables/", sep="") 
setwd(MyPath)

# Load source files
source("02 Functions.R")

# Option's parameters
Strike <- 2400
o <- 0.25
Volatility <- c(0.2,0.3)
r <- 0.05
T_ex <- 0.5
Type <- "Call"          # Call or Put

Boundary <- TRUE
BoundaryLevel <- 3000

AmericanType <- FALSE

# Finite diferences parameters
I <- 250

if(Boundary && Type == "Call")
{
    AssetStep <- BoundaryLevel/I 
} else if(Boundary && Type == "Put")
{
    AssetStep <- (3*Strike - BoundaryLevel)/I

} else 
{
    AssetStep <- 3*Strike/I
}

TimeStep <- 0.5/(o^2*I^2)
K <- floor(T_ex/TimeStep)+1
if(K %% 2 != 0) K <- K+1

TimeStep <- T_ex/K

# Divident parameters
Dyvidend <- TRUE
DyvidendLevel <- 300
DyvidentTime <- K/2

Asset <- CreatAssetMatrix(Strike_ = Strike,
                          Boundary_ = Boundary,
                          BoundaryLevel_ = BoundaryLevel,
                          Type_ = Type,
                          Dyvidend_ = Dyvidend,
                          DyvidendLevel_ = DyvidendLevel,
                          DyvidentTime_ = DyvidentTime,
                          I_ = I,
                          K_ = K)
    

Price <- CreatePriceMatrix(Strike_ = Strike,
                           Boundary_ = Boundary,
                           Type_ = Type,
                           AssetMatrix_ = Asset,
                           AssetStep_ = AssetStep,
                           TimeStep_ = TimeStep,
                           Volatility_ = Volatility,
                           r_ = r,
                           AmericanType_ = AmericanType)                                       

Gamma <- CreateGammaMatrix(ValueMatrix_ = Price,
                           AssetStep_ = AssetStep)

Delta <- CreateDeltaMatrix(ValueMatrix_ = Price,
                           AssetStep_ = AssetStep)


Asset <- mirror.matrix(Asset)
Price <- mirror.matrix(Price)
Gamma <- mirror.matrix(Gamma)
Delta <- mirror.matrix(Delta)

Date <- as.character(seq(as.Date("2015/1/1"), as.Date("2015/6/30"),length.out = K+1))
StartAsset <- as.integer(Asset[,1])

plot <- plot_ly(z = Price,
                x = Date,
                y = StartAsset,
                type = "surface",
                colorbar = list(title = "Value", len = 0.9, thickness = 15)
                )


# plot
plotly_POST(plot, filename = "r-docs/TEST", world_readable=TRUE)
