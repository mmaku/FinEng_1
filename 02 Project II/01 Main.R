rm(list = ls()) 

Timer <- proc.time()

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
VOL <- list(c(0.1,0.2),c(0.2,0.3),c(0.3,0.4),c(0.18,0.32),c(0.22,0.28),c(0.25,0.25))
r <- 0.0237
T_ex <- 0.5
TP <- c("Call")          # Call or Put

BD <- c(T)
BoundaryLevelPUT <- c(2200)
BoundaryLevelCALL <- c(2600)

CHECK <- FALSE

AT <- c(T)

# Divident parameters
DL <- c(100)


# Finite diferences parameters
I <- 200 

Prices <- PricesHalfMinus <- Assets <- PricesHalfPlus <- list()


for(Type in TP)
{
    if(Type == "Call") BL <- BoundaryLevelCALL
    if(Type == "Put") BL <- BoundaryLevelPUT
    
    for(DyvidendLevel in DL)
    {
        for(Volatility in VOL)
            {
            for(AmericanType in AT)
                {
                for(Boundary in BD)
                    {
                    if(Boundary)
                    {
                        for(BoundaryLevel in BL)
                        {
                            Name <- paste(Type,AmericanType,DyvidendLevel,Volatility[1],Volatility[2],Boundary,BoundaryLevel,sep="_")
                            
                            if(Type == "Call")
                            {
                                AssetStep <- BoundaryLevel/I 
                            } else 
                            {
                                AssetStep <- (3*Strike - BoundaryLevel)/I
                                
                            } 
                            
                            TimeStep <- 0.4/(o^2*I^2)
                            K <- floor(T_ex/TimeStep)+1
                            if(K %% 2 != 0) K <- K+1
                            
                            TimeStep <- T_ex/K
                            
                            DyvidentTime <- K/2
                            
                            Asset <- CreatAssetMatrix(Strike_ = Strike,
                                                      Boundary_ = Boundary,
                                                      BoundaryLevel_ = BoundaryLevel,
                                                      Type_ = Type,
                                                      DyvidendLevel_ = DyvidendLevel,
                                                      DyvidentTime_ = DyvidentTime,
                                                      I_ = I,
                                                      K_ = K)
                            
                            
                            Price <- CreatePriceMatrix(Strike_ = Strike,
                                                       Boundary_ = Boundary,
                                                       BoundaryLevel_ = BoundaryLevel,
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
                            
                            DeltaSign <- CreateDeltaSignMatrix(ValueMatrix_ = Price,
                                                               AssetStep_ = AssetStep)

                            Asset <- mirror.matrix(Asset)
                            Price <- mirror.matrix(Price)
#                             Gamma <- mirror.matrix(Gamma)
#                             Delta <- mirror.matrix(Delta)
                            DeltaSign <- mirror.matrix(DeltaSign)
                            
                            Date <- as.character(seq(as.Date("2015/1/1"), as.Date("2015/6/30"),length.out = K+1))
                            StartAsset <- as.integer(Asset[,1])
                            
#                             plot <- plot_ly(z = Price,
#                                             x = Date,
#                                             y = StartAsset,
#                                             type = "surface")
#                                 
#                             PlotName <- paste("r-docs/",Name,sep="")
#                                 
#                             # plotly_POST(plot, filename = PlotName, world_readable=TRUE)
                                                        
                            rownames(Asset) <- StartAsset
                            colnames(Asset) <- Date
                            rownames(Price) <- StartAsset
                            colnames(Price) <- Date
                            
#                             Asset <- as.data.frame(Asset)
#                             Price <- as.data.frame(Price)
#                             Gamma <- as.data.frame(Gamma)
#                             Delta <- as.data.frame(Delta)
                            
                            PricesHalfMinus[[length(PricesHalfMinus)+1]] <- Price[,K-1]
                            names(PricesHalfMinus)[length(PricesHalfMinus)] <- Name
                            
                            PricesHalfPlus[[length(PricesHalfPlus)+1]] <- Price[,K+1]
                            names(PricesHalfPlus)[length(PricesHalfPlus)] <- Name
                            
                            Prices[[length(Prices)+1]] <- Price[,1]
                            names(Prices)[length(Prices)] <- Name
                            
                            Assets[[length(Assets)+1]] <- Asset[,1]
                            names(Assets)[length(Assets)] <- Name
                            
#                             write.csv(Asset,paste(MyDeliverables,Name,"_Asset.csv",sep=""))
#                             write.csv(Price,paste(MyDeliverables,Name,"_Price.csv",sep=""))
#                             write.csv(Gamma,paste(MyDeliverables,Name,"_Gamma.csv",sep=""))
#                             write.csv(Delta,paste(MyDeliverables,Name,"_Delta.csv",sep=""))
                            
                        }
                    } else 
                    {
                        
                        Name <- paste(Type,AmericanType,DyvidendLevel,Volatility[1],Volatility[2],Boundary,"NONE",sep="_")
                        
                        AssetStep <- 3*Strike/I
                        
                        TimeStep <- 0.4/(o^2*I^2)
                        K <- floor(T_ex/TimeStep)+1
                        if(K %% 2 != 0) K <- K+1
                        
                        TimeStep <- T_ex/K
                        
                        DyvidentTime <- K/2
                        
                        Asset <- CreatAssetMatrix(Strike_ = Strike,
                                                  Boundary_ = Boundary,
                                                  BoundaryLevel_ = BoundaryLevel,
                                                  Type_ = Type,
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
                        
#                         if(Volatility == c(0.2,0.3) && (DyvidendLevel == 0 || DyvidendLevel == 100))
#                         {
#                             plot <- plot_ly(z = Price,
#                                             x = Date,
#                                             y = StartAsset,
#                                             type = "surface")
#                             
#                             PlotName <- paste("r-docs/",Name,sep="")
#                             
#                             plotly_POST(plot, filename = PlotName, world_readable=TRUE)
#                             
#                             CHECK <- TRUE
#                         }
                        
                        if(CHECK) Sys.sleep(120)
                        CHECK <- FALSE
                        
                        rownames(Asset) <- StartAsset
                        colnames(Asset) <- Date
                        rownames(Price) <- StartAsset
                        colnames(Price) <- Date

#                         Asset <- as.data.frame(Asset)
#                         Price <- as.data.frame(Price)
#                         Gamma <- as.data.frame(Gamma)
#                         Delta <- as.data.frame(Delta)

                        PricesHalfMinus[[length(PricesHalfMinus)+1]] <- Price[,K-1]
                        names(PricesHalfMinus)[length(PricesHalfMinus)] <- Name
                        
                        PricesHalfPlus[[length(PricesHalfPlus)+1]] <- Price[,K+1]
                        names(PricesHalfPlus)[length(PricesHalfPlus)] <- Name
                        
                        Prices[[length(Prices)+1]] <- Price[,1]
                        names(Prices)[length(Prices)] <- Name
                        
                        Assets[[length(Assets)+1]] <- Asset[,1]
                        names(Assets)[length(Assets)] <- Name
                        
#                         write.csv(Asset,paste(MyDeliverables,Name,"_Asset.csv",sep=""))
#                         write.csv(Price,paste(MyDeliverables,Name,"_Price.csv",sep=""))
#                         write.csv(Gamma,paste(MyDeliverables,Name,"_Gamma.csv",sep=""))
#                         write.csv(Delta,paste(MyDeliverables,Name,"_Delta.csv",sep=""))
                        
                    }
                }
            }
        }
    }
}


# write.table(Prices, file = paste(MyDeliverables,"01 Prices.csv",sep=""), col.names = names(Prices),sep = ";",dec = ",")
# write.table(PricesHalfPlus, file = paste(MyDeliverables,"02 PricesHalfPlus.csv",sep=""), col.names = names(PricesHalfPlus),sep = ";",dec = ",")
# write.table(PricesHalfMinus, file = paste(MyDeliverables,"03 PricesHalfMinus.csv",sep=""), col.names = names(PricesHalfMinus),sep = ";",dec = ",")

proc.time() - Timer

# rm(pp)
# 
# XOU = c(Assets[[1]], seq(from = Assets[[1]][I+1]+diff(Assets[[1]])[1], by = diff(Assets[[1]])[1], length.out = 10))
# 
# 
# Aprox1 <- approx(y = c( Prices[[1]],rep.int(0, times = 50)),
#                  x = c(Assets[[1]],seq(from = Assets[[1]][I+1]+diff(Assets[[1]])[1], by = diff(Assets[[1]])[1], length.out = 50)),
#                  xout = XOU)
# 
# Aprox2 <- approx(y = c(Prices[[2]],rep.int(0, times = 35)),
#                  x = c(Assets[[2]],seq(from = Assets[[2]][I+1]+diff(Assets[[2]])[1], by = diff(Assets[[2]])[1], length.out = 35)),
#                  xout = XOU)
# 
# Aprox3 <- approx(y = c(Prices[[3]],rep.int(0, times = 25)),
#                  x = c(Assets[[3]],seq(from = Assets[[3]][I+1]+diff(Assets[[3]])[1], by = diff(Assets[[3]])[1], length.out = 25)),
#                  xout = XOU)

# Aprox3[["y"]][181] <- 0
# Aprox2[["y"]][170] <- 0
# Aprox1[["y"]][162] <- 0

# pp <- plot_ly(y = c(Prices[[1]],rep.int(0, times = 10)),
#               x = XOU,
#               name = "No dyvident")
# pp <- add_trace(p =last_plot(), 
#                 y = c(Prices[[2]], rep.int(0, times = 9)), 
#                 name = "Volatility = 0")
# pp <- add_trace(p =last_plot(), 
#                 y = c(Prices[[2]],rep.int(0, times = 10)),
#                 # x = Aprox1,
#                 x = Assets[[2]]+4,
#                 name = "Dyvident = 100")
# pp <- add_trace(p =last_plot(), 
#                 y = c(Prices[[3]],rep.int(0, times = 10)),
#                 # x = Aprox2,
#                 x = Assets[[3]]-3,
#                 name = "Dyvident = 250")
# pp <- add_trace(p =last_plot(), 
#                 y = c(Prices[[4]],rep.int(0, times = 10)),
#                 # x = Aprox3,
#                 x = Assets[[4]]-6,
#                 name = "Dyvident = 500")
# pp <- layout(pp, 
#              title = "Amerian call@2400 w/ barriers&dyvidents",
#              xaxis = list(title = "Asset initial value"),
#              yaxis = list(title = "Option price"))

# add_trace(p =last_plot(), y = Prices[[3]], name = names(Prices)[3])
# add_trace(p =last_plot(), y = Prices[[4]], name = names(Prices)[4])
# add_trace(p =last_plot(), y = Prices[[5]], name = names(Prices)[5])
# rm(pp)
# pp <- plot_ly(y = Prices[[2]],
#         x = Asset[,1],
#         name = names(Prices)[2])
# pp <- add_trace(p =last_plot(), y = Prices[[1]], name = names(Prices)[1])
# pp <- add_trace(p =last_plot(), y = Prices[[3]], name = names(Prices)[3])
# pp <- add_trace(p =last_plot(), y = Prices[[4]], name = names(Prices)[4])
# pp <- add_trace(p =last_plot(), y = Prices[[5]], name = names(Prices)[5])
# pp <- add_trace(p =last_plot(), y = Prices[[6]], name = names(Prices)[6])
# pp <- layout(pp, 
#              title = "American call@2400 w/ barriers - volatility comparison",
#              xaxis = list(title = "Initial asset value"),
#              yaxis = list(title = "Option price"))
# 
# pp
# 
# plotly_POST(pp, filename = "Project2/Dyvident comparison - american II", world_readable=TRUE)
# 
# pp <- plot_ly(z = DeltaSign,
#               x = Date,
#               y = StartAsset,
#               type = "heatmap")
# 
# pp <- layout(pp, 
#              title = "Delta of Europan put@2400 w/ divident = 50",
#              xaxis = list(title = "Asset initial value"),
#              zaxis = list(title = "Option price"),
#              yaxis = list(title = "Date"))
# pp
# plotly_POST(pp, filename = "Project2/European put delta", world_readable=TRUE)
# 
