# source("02 Functions.R")

return_vector <- function (X, type = "log")
{
	n <- length(X)
	
	if (type =="daily")
	{
		X <- (X-X[-1])/X[-1]
	} else if (type == "log") {
		X <- log(X[-1]/X)
	}
	
	return (X[1:n-1])
}
0
B_M_transform <- function(X)
{
	len <- length(X)
	
	odd_positions <- seq(from = 1, to = len, by = 2) #Z_1
	even_positions <- seq(from = 2, to = len, by = 2) #Z_2
	
	tmp <- X
		
	X[odd_positions] <- sqrt(-2*log(tmp[odd_positions]))*cos(2*pi*tmp[even_positions])
	X[even_positions] <- sqrt(-2*log(tmp[odd_positions]))*sin(2*pi*tmp[even_positions])
	remove(tmp)
		
	return (X[1:len])
}

simulation <- function (quotation, sim_amount, type = "log", n = 250)
{
	options(warn = -1)
	
	if (class(quotation) == "numeric")
	{ 
    	R <- return_vector(X = quotation, type = type) 
    	
    	num_of_companies <- 1 
    	sigma <- sd(R)*sqrt(250) 
    	mi <- mean(R)*250
    	X <- BM(sim_amount = sim_amount,P_0 = quotation[length(quotation)],mi = mi,sigma = sigma, n = n, m = num_of_companies, chol = sigma)
	} else
	{
	    names <- names(quotation[1,])
	
    	R <- apply(quotation, 2, return_vector, type = type) 
    	num_of_companies <- length(quotation[1,]) 
    	sigma <- apply(R, 2, sd)*sqrt(n)
    	
    	mi <- apply(R, 2, mean)*n
    	q_cor <- cor(R)
    	q_cov <- cov(R) 
    	q_chol <- t(chol(q_cov))
    	
    	X <- BM(sim_amount = sim_amount, P_0 = quotation[length(quotation[,1]),1:num_of_companies],mi = mi, sigma = sigma, n = n, m = num_of_companies, chol = q_cor[1,2])
    	names(X) <- names
	}
    
	options(warn = 0)
	
	return (list(drift = mi, sigma = sigma, daily_returns = R, time_period = n, simulation_res = X))
}

BM <- function (sim_amount, P_0, mi, sigma, n, m, chol)
{
	Z <- matrix(runif(n = sim_amount*n*m,min = 0, max = 1), n, sim_amount*m)
	Z <- apply(Z, 2, B_M_transform)
	dt <- 1/250
	
	if (m == 1)
	{
	    const_matrix <- exp(dt*(mi-sigma^2/2)+Z*sigma*sqrt(dt))
	} else
	{
	    for (i in seq(from = 1, to = sim_amount*m-m+1, by = m))
	    {
		    Z[,i+m-1] <- Z[,i]*chol + sqrt(1-chol^2)*Z[,i+m-1]
	    }
	    
	const_matrix <- exp(dt*(mi-sigma^2/2) + Z*sigma*sqrt(dt))
	}
	
	simulation_matrix <- rbind(rep(P_0, sim_amount*m), const_matrix)  
	B.t <- apply(simulation_matrix, 2, cumprod)
	company_list <- rep(list(data.frame()),m)
	time <- seq(from = 0, by = 1, to = n)
	
	for(i in 1:m)
	{
	    company_list[[i]] <- data.frame(cbind(time,B.t[,seq(from = i, by = m, to = sim_amount*m)]))
	}
	
	return (company_list)
}

delta <- function(X, Y, dt, r_PLN, r_f, sigma_Y, sigma_X, rho, n = 250, calib_system = 1)
{
# parametry opcji
	up <- 100
	s_gold_zero <- 1586.95
	
	T <- length(Y)
	T <- T-1
	t <- 0:T
	r_PLN <- r_PLN/n
	r_f <- r_f/n
	sigma_Y <- sigma_Y/sqrt(n)
	sigma_X <- sigma_X/sqrt(n)
	
	k <- seq(from = 0 , by = dt , to = T)

	if(calib_system == 1)
	{
		const <- (up/s_gold_zero)*exp(-(r_PLN-r_f+rho*sigma_Y*sigma_X)*(T-t))			# tutaj pierwsza kropka
		delta_X <- (-1)*Y[k+1]/X[k+1]*const[k+1]
		delta_X <- rep(delta_X, each = dt , length = (T+1))
		delta_Y <- 1/X[k+1]*const[k+1]
		delta_Y <- rep(delta_Y, each = dt, length = (T+1))
		value <-  Y[k+1]*const[k+1]

	} else if(calib_system == 2)
	{
		const <- (up/s_gold_zero)*exp(-(r_PLN-r_f-sigma_X^2+rho*sigma_Y*sigma_X)*(T-t)) # tutaj druga kropka
		delta_X <- (-1)*Y[k+1]/(X[k+1])^2*const[k+1]
		delta_X <- rep(delta_X, each = dt , length = (T+1))
		delta_Y <- 1/X[k+1]*const[k+1]
		delta_Y <- rep(delta_Y, each = dt, length = (T+1))
		value <- Y[k+1]/X[k+1]*const[k+1]

	} else {
		const <- (up/s_gold_zero)*exp(-(r_PLN+r_f-sigma_X^2+rho*sigma_Y*sigma_X)*(T-t)) # tutaj oryginał
		delta_X <- (-1)*Y[k+1]*const[k+1]/(X[k+1])^2
		delta_X <- rep(delta_X, each = dt , length = (T+1))
		delta_Y <- const[k+1]/X[k+1]
		delta_Y <- rep(delta_Y, each = dt, length = (T+1))
		value <- Y[k+1]/X[k+1]*const[k+1]
	}
	
	value <- rep(value, each = dt , length = (T+1))
	
	gold_dt <- (delta_Y-c(0,delta_Y[1:(T)])) 
	buy_gold_dt <- rep(0, length = length(gold_dt))
	
	if(calib_system == 1)
	{
		buy_gold_dt <- (gold_dt-r_f*gold_dt)*Y*X
	} else if(calib_system == 2)
	{
		buy_gold_dt <- (gold_dt-r_PLN*gold_dt)*Y
	} else {
		buy_gold_dt <- (gold_dt-r_PLN*gold_dt)*Y
	}
	
	dolar_dt <- (delta_X*exp(-r_f) - c(0,delta_X[1:(T)]))
	buy_dolar_dt <- dolar_dt*X
	
	total_cost <- rep(0,(T+1))
	total_cost[1] <- buy_gold_dt[1] + buy_dolar_dt[1] - value[1]
	
	for(i in 2:T)
	{
		total_cost[i] <- total_cost[i-1]*exp(r_PLN)+buy_gold_dt[i] + buy_dolar_dt[i]
	}
	
	if(calib_system == 1)
	{
	    total_cost[T+1] <- total_cost[T]*exp(r_PLN)+value[T+1]-delta_Y[T+1]*Y[T+1]*X[T+1] - delta_X[T+1]*X[T+1]
	} else if(calib_system == 2)
	{
	    total_cost[T+1] <- total_cost[T]*exp(r_PLN)+value[T+1]-delta_Y[T+1]*Y[T+1] - delta_X[T+1]*X[T+1]
	} else {
	    total_cost[T+1] <- total_cost[T]*exp(r_PLN)+value[T+1]-delta_Y[T+1]*Y[T+1] - delta_X[T+1]*X[T+1]
	}
	
	delta_table <- data.frame(cbind(Y,X, value, delta_Y,delta_X,buy_gold_dt, buy_dolar_dt,total_cost))
	
	return(delta_table)
}
