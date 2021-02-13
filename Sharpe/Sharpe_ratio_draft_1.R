
library(BatchGetSymbols)


# I will feed the program all the 500 stocks from sp500 to get the highest possible sharpe ratio everyday.
# only 3 datas for now
first.date <- Sys.Date()-365
last.date <- Sys.Date()
df.SP500 <- GetSP500Stocks()
df.SP500
tickers <- df.SP500$Tickers[1:100]
tickers
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)


library(ggplot2)
View(l.out)
length(tickers)
print(tickers)
lenght(l.out$df.tickers$price.adjusted[MMM])
l.out$df.tickers$price.adjusted
tickers <- unique(l.out$df.tickers$ticker)
for (i in tickers){
  boolll = l.out$df.tickers$ticker == i
  
  DAT <-l.out$df.tickers$price.adjusted[boolll]
  print(length(DAT))
  if (length(isTRUE(boolll))== 252){
    
    
  }
}


# We only need the adjusted daily price for this ratio so, removing everything else with this function



price_adj <- function(tickers){
  price_adjusted = NULL
  tick <- NULL
  for (i in tickers){
    
    
    bool = l.out$df.tickers$ticker == i
    data = l.out$df.tickers$price.adjusted[bool]
    
    
    if (length(data)==252){
      price_adjusted <- cbind(price_adjusted, tickers = data)
      tick <- append(tick, i)
  
    }
    
  }
  colnames(price_adjusted) <- tick
  return(price_adjusted)
}

price_adjusted <- price_adj(tickers)
View(price_adjusted)


# We calculate the daily expected return by calculating the percentage change in price everyday

daily_return <- function(df){
  x <- ncol(df)
  y <- nrow(df)
  e <- NULL
  for (i in (1:x)){
    d <- NULL
    for (j in (1:y)){
      
      a <- df[j,i]
      if (j != y){
        b <- df[(j+1),i]
        c <- (b - a)/a
        d <- append(d, c)}
  
    }
    e <- cbind(e, tickets = d)
    }
  colnames(e) <- colnames(df)
  return(e)
}

daily_return(price_adjusted)
price_adjusted[1,3]
exp_return <- daily_return(price_adjusted)
View(exp_return)


# we get the average of the year-round % change in price to get the average daily return

R_Eturn<-avg_dailyreturn(exp_return)
View(R_Eturn)

avg_dailyreturn <- function(df){
  b <- NULL
  c <- ncol(df)
  for (i in 1:c){
    a <- sum(df[,i])/nrow(df)
    b <- cbind(b, tickets = a)
    }
  
  colnames(b) <- colnames(df)
  return(b)
}

# calculating the individual standard deviation of each of the stocks (this will be the risk for our data)

stan_D <- function(df){
  b <- NULL
  for (i in 1:ncol(df)){
    a<- sd(df[,i])
    b <- cbind(b, data = a)
  }
  colnames(b) <- colnames(df)
  return(b)
}
sd<- stan_D(exp_return)

View(sd)

for (i in tickers){
  print(a[,i])
}

cor(exp_return[,1],exp_return[,2])
# to get a combination of all different corelation in the portfolio
install.packages("gtools")
library(gtools)
tickers
nrow(combinations(length(tickers),2,tickers))
comb <- combinations(3,2,tickers)
comb
for (i in 1:nrow(comb)){
  names_try <- comb[i,1]
}
expand.grid(tickers,tickers)

# Gives the corelation for any ammount of data in the data frame
corelation <- function(df){
  comb <- combinations(ncol(df),2, colnames(df))
  e <- NULL
  names_cor <- NULL
  for (i in 1:nrow(comb)){
    b <- exp_return[,comb[i,1]]
    c <- exp_return[,comb[i,2]]
    d <- cor(b,c)
    e <- cbind(e,i = d)
    names_cor <- append(names_cor, paste(comb[i,1],comb[i,2]))
    
  }
  colnames(e) <- names_cor
  return(e)
  
}
exp_return[,"ZM"]
coor_value <- corelation(price_adjusted)
View(coor_value)
i <- c("ZM","PLUG")
# So far

# need to max weight for the  
install.packages("lpSolve")
library(lpSolve)
install.packages("lpSolveAPI")
library(lpSolveAPI)









sharpe_ratio <- function(weight, st_dev, cor_ll, R_eturn, rf = 0){
  comb <- combinations(ncol(st_dev), 3, colnames(st_dev))
  e <- NULL
  f <- NULL
  ngy <- NULL
  names_cor <- NULL
 
  names_shrp <- NULL
  for (i in 1:nrow(comb)){
    sd_a <- st_dev[,comb[i,1]]
    sd_b <- st_dev[,comb[i,2]]
    sd_c <- st_dev[,comb[i,3]]
    weight_a <- weight[1]
    weight_b <- weight[2]
    weight_c <- weight[3]
    P_ab <- cor_ll[,paste(comb[i,1],comb[i,2])]
    P_ac <- cor_ll[,paste(comb[i,1],comb[i,3])]
    P_bc <- cor_ll[,paste(comb[i,2],comb[i,3])]
    var_portfolio <- (weight_a*weight_a*sd_a*sd_a)+(weight_b*weight_b*sd_b*sd_b)+(weight_c*weight_c*sd_c*sd_c)+(2*weight_a*weight_b*sd_a*sd_b*P_ab)+(2*weight_a*weight_c*sd_c*sd_a*P_ac)+(2*weight_b*weight_c*sd_c*sd_b*P_bc)
    R_eturn_a <- R_eturn[,comb[i,1]]
    R_eturn_b <- R_eturn[,comb[i,2]]
    R_eturn_c <- R_eturn[,comb[i,3]]
    
    portfolio_return <- (R_eturn_a * weight_a)+ (R_eturn_b * weight_b)+(R_eturn_c*weight_c)
    Final_ratio <- (portfolio_return-rf)/sqrt(var_portfolio)
    e <- paste(comb[i,1],comb[i,2],comb[i,3])
    ngy <- cbind(ngy,i = Final_ratio)
    names_cor <- append(names_cor, e)
  
  }
  colnames(ngy) <- names_cor
  return(ngy)
}

sharpe_ratio <- function(weight, st_dev, cor_ll, R_eturn, rf = 0){
  comb <- combinations(ncol(st_dev), 3, colnames(st_dev))
  e <- NULL
  f <- NULL
  ngy <- NULL
  names_cor <- NULL
  
  names_shr <- NULL
  for (i in 1:nrow(comb)){
    sd_a <- st_dev[,comb[i,1]]
    sd_b <- st_dev[,comb[i,2]]
    sd_c <- st_dev[,comb[i,3]]
    weight_a <- weight[1]
    weight_b <- weight[2]
    weight_c <- weight[3]
    P_ab <- cor_ll[,paste(comb[i,1],comb[i,2])]
    P_ac <- cor_ll[,paste(comb[i,1],comb[i,3])]
    P_bc <- cor_ll[,paste(comb[i,2],comb[i,3])]
    var_portfolio <- (weight_a*weight_a*sd_a*sd_a)+(weight_b*weight_b*sd_b*sd_b)+(weight_c*weight_c*sd_c*sd_c)+(2*weight_a*weight_b*sd_a*sd_b*P_ab)+(2*weight_a*weight_c*sd_c*sd_a*P_ac)+(2*weight_b*weight_c*sd_c*sd_b*P_bc)
    R_eturn_a <- R_eturn[,comb[i,1]]
    R_eturn_b <- R_eturn[,comb[i,2]]
    R_eturn_c <- R_eturn[,comb[i,3]]
    
    portfolio_return <- (R_eturn_a * weight_a)+ (R_eturn_b * weight_b)+(R_eturn_c*weight_c)
    Final_ratio <- (portfolio_return-rf)/sqrt(var_portfolio)
    e <- paste(comb[i,1],comb[i,2],comb[i,3])
    ngy <- cbind(ngy,i = Final_ratio)
    names_cor <- append(names_cor, e)
    
  }
  colnames(ngy) <- names_cor
  return(ngy)
}
FINALTRY <- sharpe_ratiotry(weight, sd, coor_value, R_Eturn)
View(FINALTRY)
sharpe_ratiotry <- function(weight, st_dev, cor_ll, R_eturn, rf = 0){
  comb <- combinations(ncol(st_dev), 3, colnames(st_dev))
  e <- NULL
  f <- NULL
  ngy <- NULL
  names_cor <- NULL
  
  names_shr <- NULL
  for (i in 1:nrow(comb)){
    sd_a <- st_dev[,comb[i,1]]
    sd_b <- st_dev[,comb[i,2]]
    sd_c <- st_dev[,comb[i,3]]
    weight_a <- weight[1]
    weight_b <- weight[2]
    weight_c <- weight[3]
    P_ab <- cor_ll[,paste(comb[i,1],comb[i,2])]
    P_ac <- cor_ll[,paste(comb[i,1],comb[i,3])]
    P_bc <- cor_ll[,paste(comb[i,2],comb[i,3])]
    var_portfolio <- (weight_a*weight_a*sd_a*sd_a)+(weight_b*weight_b*sd_b*sd_b)+(weight_c*weight_c*sd_c*sd_c)+(2*weight_a*weight_b*sd_a*sd_b*P_ab)+(2*weight_a*weight_c*sd_c*sd_a*P_ac)+(2*weight_b*weight_c*sd_c*sd_b*P_bc)
    R_eturn_a <- R_eturn[,comb[i,1]]
    R_eturn_b <- R_eturn[,comb[i,2]]
    R_eturn_c <- R_eturn[,comb[i,3]]
    
    portfolio_return <- (R_eturn_a * weight_a)+ (R_eturn_b * weight_b)+(R_eturn_c*weight_c)
    Final_ratio <- (portfolio_return-rf)/sqrt(var_portfolio)
    e <- paste(comb[i,1],comb[i,2],comb[i,3])
    ngy <- append(ngy, Final_ratio)
    names_cor <- append(names_cor, e)
    
  }
  row.names(ngy) <- names_cor
  return(ngy)
}
