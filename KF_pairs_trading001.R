# Kalman Filter implementation of pairs trading strategy
# Author: Matthew Benning
# Other scripts used are in Python
# Main reason for this script: Google colab py plots are :(
# Matrix algebra is also more straight forward in R - needed for Kalman

rm(list=ls())
library(readxl)

#############################################################################

######## Kalman Filter##################

############################################################################
# First read in the preprocessed data
dat<- as.data.frame(read_excel("jse_data_ready.xlsx"))
dat<- removeNA(dat)

dat_2 <- as.data.frame(dat[,-1])

# Make the row names a date 
rownames(dat_2) <- dat[,1]
jse_data <- dat_2
jse_data <- as.timeSeries(jse_data)
save(jse_data,file = "~/Desktop/Honours Research Project/JSE_Data.RData")

# Normalize the data _ Assumption of Kalman inputs
jse_data <- scale(jse_data)

# No. of assets looked at 
# Plot the asset paths
ncol(jse_data)
plot(jse_data,plot.type='s',ylab='Normalized Price (ZAC)',xlab='Date')

# Get the dates column in case we need for later
dates_index <- dat[,1]

##################################################
# Get the tickers to be used in our portfolio 
asset_1<- rbind('NED.JO','PSG.JO','PIK.JO','FFA.JO','NPN.JO')
asset_2 <- rbind('VKE.JO','SOL.JO','SPP.JO','SNT.JO','RDF.JO')
portfolio <- cbind(asset_1,asset_2)
colnames(portfolio) <-c('Asset 1','Asset 2')

#################################################

# Now set up a function that iteratively calculates the dynamic hedge ratio
# using the  Kalman filter

# Equtions based on ... and code adapted K. Longmore
# We run multiple tests with different delta values for the foramtion period
Kalman_dynamic_hedge <-function(tick1,tick2,jse_data=jse_data,delta=0.0005){
  
  # Set up the two assets
  x <- as.data.frame(jse_data[,tick2])
  y <- as.matrix(jse_data[,tick1])
  
  # add a column of ones to the x asset 
  x$int <- rep(1, nrow(x))
  x <- as.matrix(x)
  
  # Set up 
  Vw <- delta/(1-delta)*diag(2)
  Ve <- 0.001
  R <- matrix(rep(0, 4), nrow=2)
  P <- matrix(rep(0, 4), nrow=2)
  
  # Holder vectors 
  hedge_ratio<- matrix(rep(0, nrow(y)*2), ncol=2)
  y_hat <- matrix(0, nrow(y),ncol=1)
  error <- matrix(0, nrow(y),ncol=1)
  Q <- matrix(0, nrow(y),ncol=1)
  spread <-matrix(0,nrow(y),ncol=1)
  
  # Loop through all days in look back period
  for(i in 1:nrow(y)) {
    if(i > 1) {
      hedge_ratio[i, ] <- hedge_ratio[i-1, ] 
      R <- P + Vw 
    }
    # Prediction
    y_hat[i] <- x[i, ] %*% hedge_ratio[i, ] 
    
    # Variance of prediciton
    Q[i] <- t(x[i, ])%*%(R%*% x[i, ]) + Ve 
    
    # Error between pred and observed
    error[i] <- y[i] - y_hat[i]
    K <- R %*% x[i, ] / Q[i] # Kalman gain
    
    # Spread update
    hedge_ratio[i, ] <- hedge_ratio[i, ] + K*error[i]
    P = R - (K%*% x[i, ] %*% R)
    
  }
  
  # Only want Beta from our lin reg not intercept
  hedge_ratio <- hedge_ratio[,1]
  # Make sure that all the vectors and dfs produced have dates
  hedge_ratio<-as.data.frame(hedge_ratio)
  rownames(hedge_ratio) <- dates_index
  hedge_ratio <- as.timeSeries(hedge_ratio)
  
  error<-as.data.frame(error)
  rownames(error) <- dates_index
  error <- as.timeSeries(error)
  
  sdv <- sqrt(Q)
  
  sdv<-as.data.frame(sdv)
  rownames(sdv) <- dates_index
  sdv <- as.timeSeries(sdv)
  
  # With the calculated hedge ratios, calculate the spread
   #spread[i] <- x[i,1]-y[i,1]*hedge_ratio[i,1]

  # Now we want to plot how the hedge ratio as it changes over time 
  plot(hedge_ratio[2:nrow(hedge_ratio)], type='l', xlab='Date',
       ylab=expression(paste('Hedge Ratio - ',beta)),lwd=1.5)
  legend(legend=paste(tick1,'|',tick2),'bottomright',cex=0.75)
  
  # Now plot the spread over time with the trad signals
  # Trading signals on the spread
  # Burn in of 10 days
  signals <- cbind(error, sdv, -sdv)
  colnames(signals) <- c("Error between estimate and observed", "Std", "-Std")
  plot(error[11:nrow(error),], type='l',
       xlab='Date', ylab ='Error',col='black',lwd=1.5)
  lines(1*sdv,col='red',lwd=2)
  lines(-1*sdv,col='red',lwd=2)
  legend(legend='1 Standard Deviation','topleft',lty=1,col='red',cex = 0.50)
  legend(legend=paste(tick1,'|',tick2),'bottomright',cex=0.5)
  
  results <- cbind(hedge_ratio,error,sdv,y_hat)
  colnames(results) <- c('Hedge_Ratio','Error','Std Dev','Yhat')
  return(results)
}

#########################################################################
# Set up data splits for formation and trading periods
# This is done accordingly with the Pyscript, which uses 70/30 split for
# cointegration test and the LSTM forecast
# Train from 2010/01/04 to 2016/12/30
# Trading period from 2017/01/03 to 2019/12/31

split <- round(0.80*nrow(jse_data),0)-1
data_train <- jse_data[1:split,]
data_test <- jse_data[(split+1):nrow(jse_data),]

dat_2_train <- as.data.frame(dat_2[1:split,])
dat_2_test <- as.data.frame(dat_2[(split+1):nrow(jse_data),])

dates_index <- rownames(jse_data)

############################################################################
# Now create a function to capture the market signals ie. whether we are long
# short etc.

##############################################################################

market_signals_Kalman <- function(tick1,tick2,jse_data=jse_data,dat=dat_2){
  jse_data = jse_data

  # Again set up the two assets
  x <- as.data.frame(jse_data[,tick2])
  y <- as.matrix(jse_data[,tick1])
  
  # Run the dynamic track 
  results <- Kalman_dynamic_hedge(tick1=tick1,tick2=tick2,jse_data=jse_data)
  
  # Set up a matrix to hold the signals alongside assets
  long<- rep(0,nrow(x))
  short <- rep(0,nrow(x))
  exit<- rep(0,nrow(x))
  long_market <-rep(0,nrow(x))
  short_market <-rep(0,nrow(x))
  pairs_signals <- cbind(y,x,results$Error,results$`Std Dev`,long,short,exit,
                         long_market,short_market)
  colnames(pairs_signals) <-c('Asset1','Asset2','Error','Std','long','short'
                              ,'Exit','long_market','short_market')
  
  # Now check the error score against the trade threshold and map them
  # We use -0.005 and 0.005 as the exit strat because we only have discrete
  # intraday data so cannot see the exact point at which it hits 0
  for ( i in 1:nrow(x)){
    if (pairs_signals[i,'Error']>= 1*pairs_signals[i,'Std']){
      pairs_signals[i,'long']=1
    } else if (pairs_signals[i,'Error']<= -1*pairs_signals[i,'Std']){
      pairs_signals[i,'short']=1
    } else if (pairs_signals[i,'Error'] < 0.005 & pairs_signals[i,'Error'] >- 0.005){
      pairs_signals[i,'Exit']=1
    }
  }
  
  sum(pairs_signals[,'long'])
  sum(pairs_signals[,'short'])
  
  # Now that we now timing for entry or exit, we need to make it reflective of 
  # if we stay in a position and dont exit 
  lm = 0
  sm = 0
  
  # Also add a lag of 1 day as we dont want to introduce lookback bias
  # Burn in number of days = 2
  for (i in 3:nrow(x)){
    
    if (pairs_signals[i,'long']==1){
      lm=1
    } 
    if (pairs_signals[i,'short']==1){
      sm=1
    } 
    if (pairs_signals[i,'Exit']==1){
      lm=0
      sm=0
    } 
    # Make sure we arent in any short or long positions at the same time
    # Since its daily we cant check the exact cross-over point 
    if (pairs_signals[i-1,'short_market']==1 & pairs_signals[i-1,'long']==1){
      sm=0
    }
    
    if (pairs_signals[i-1,'long_market']==1 & pairs_signals[i-1,'short']==1){
      lm=0
    }
    
    
    pairs_signals[i,'long_market'] = lm
    pairs_signals[i,'short_market'] = sm
  }
  
  # Plot our long short positions and find the value of them 
  long_x<- matrix(0,nrow(x),ncol=1)
  long_y<- matrix(0,nrow(x),ncol=1)
  short_x <-matrix(0,nrow(x),ncol=1)
  short_y<- matrix(0,nrow(x),ncol=1)
  
  for( i in 1:nrow(x)){

    # Long spread
    short_x[i]<- jse_data[i,tick2]*pairs_signals[i,'long_market']
    long_y[i] <- jse_data[i,tick1]*pairs_signals[i,'long_market']
      
    # Short spread
    short_y[i]<- jse_data[i,tick1]*pairs_signals[i,'short_market']
    long_x[i] <- jse_data[i,tick2]*pairs_signals[i,'short_market']
      
  
  }
  
  # Temporary just so that we dont plot all the 0's
  short_y[short_y==0]=NA
  short_x[short_x==0]=NA
  long_y[long_y==0]=NA
  long_x[long_x==0]=NA
  
  long_x<- as.timeSeries(long_x)
  long_y<- as.timeSeries(long_y)
  short_x <-as.timeSeries(short_x)
  short_y<- as.timeSeries(short_y)
  
  rownames(long_x)<-dates_index
  rownames(long_y)<-dates_index
  rownames(short_x)<-dates_index
  rownames(short_y)<-dates_index
  
  
  plot(jse_data[,tick1],type='l',col='blue',xlab='Date',
       ylab='Normalized Price (ZAC)',lwd=1.5,
       at='chic')
  points(short_y,pch=16,col='red',cex=0.5)
  points(long_y, pch=17,col='green',cex=0.5)
  legend('topleft',legend=c('In the position long','In position short'),
         col=c('green','red'), 
         pch = c(17,16),cex=0.75)
  legend(legend=paste(tick1),'bottomright',cex=0.75)
  
  plot(jse_data[,tick2],type='l',col='blue',xlab='Date',ylab='Normalized Price (ZAC)',
       lwd=1.5,at='chic')
  points(short_x, pch=16, col='red', cex=0.5)
  points(long_x,pch=17,col='green',cex=0.5)
  legend('topleft',legend=c('In position long','In position short'),
         col=c('green','red'), 
         pch = c(17,16),cex=0.75)
  legend(legend=paste(tick2),'bottomleft',cex=0.75,col=c('blue','darkorange'),
         lty=1)
  
  # Reverse
  short_x[is.na(short_x)]=0
  short_y[is.na(short_y)]=0
  long_y[is.na(long_y)]=0
  long_x[is.na(long_x)]=0
  
  long_x<- as.vector(long_x)
  long_y<- as.vector(long_y)
  short_x <-as.vector(short_x)
  short_y<- as.vector(short_y)
  
  # Get the total number of days in each position
  print(days_short <- sum(pairs_signals[,'short_market']))
  print(days_long <- sum(pairs_signals[,'long_market']))
  
  ret <- cbind(pairs_signals,short_x,long_x,short_y,long_y)
  return(ret)
}

##############################################################################
###### Backtest Portfolio ####################################################
##############################################################################

pair_backtest <- function(tick1,tick2,jse_data=jse_data,dat=dat_2,delta){
  
  jse_data=jse_data
  # Calculate the daily returns using log prices of original data
  log_prices <- as.matrix(log(dat))
  daily_returns <- diff((log_prices))
  daily_returns <- daily_returns[,c(tick1,tick2)]
  
  market_pos_x <-matrix(0,nrow=nrow(daily_returns),1)
  market_pos_y <-matrix(0,nrow=nrow(daily_returns),1)
  # Get the market position for both assets 
  #in one vector where -1 = short and 1 = long
  results <- market_signals_Kalman(tick1,tick2,jse_data = jse_data)
  market_pos_x = as.data.frame(results$long_x-results$short_x)
  
  market_pos_y = as.data.frame(results$long_y-results$short_y)
  
  # Make the signals 1 and -1s
  market_pos_x[market_pos_x> 0] <- 1 
  market_pos_y[market_pos_y> 0] <- 1 
  market_pos_x[market_pos_x< 0] <- -1
  market_pos_y[market_pos_y< 0] <- -1
  
  
  market_pos=cbind(market_pos_y,market_pos_x)
  pair_returns = (market_pos_x*daily_returns[,2]+market_pos_y*daily_returns[,1])
  
  # Index them proeprly as before
  pair_returns<-as.data.frame(pair_returns)
  rownames(pair_returns) <- dates_index
  pair_returns <- as.timeSeries(pair_returns)
  pair_returns[pair_returns==0]<-NA
  pair_returns <-removeNA(pair_returns)
  pair_returns <- as.timeSeries(pair_returns)
  # Now we use the Performance Analytics package to return some portfolio stats
  ret_cum <-as.vector(Return.cumulative(pair_returns))
  ret_annual <-as.vector( Return.annualized(pair_returns))
  Std <- as.vector(StdDev.annualized(pair_returns))
  #ASrat <- as.vector(AdjustedSharpeRatio(pair_returns))
  MDD <- as.vector(maxDrawdown(pair_returns))
  pair_summary <-cbind(ret_cum,ret_annual,MDD,Std)
  
  # Cum return and MDD plots
  charts.PerformanceSummary(pair_returns,main='',plot.type='h')
  
  
  return(pair_summary)
  
}

############################################################################
# ########### Portfolio Performance #######################################
# Run all 5 pairs and find the entire portfolio performance 
##########################################################################

# Backtest for formation period
train_returns =NULL
test_returns=NULL

train_std =NULL
test_std=NULL

train_MDD =NULL
test_MDD=NULL

dates_index <- rownames(dat_2_train)

# Formation period
for (i in 1:nrow(portfolio)){
  result = pair_backtest(tick1=portfolio[i,1], 
          tick2=portfolio[i,2],jse_data=data_train,dat=dat_2_train)
  
  train_returns[i]=result[1]
  train_std[i] = result[4]
  train_MDD[i] = result[3]
  
}

train_returns<-mean(train_returns)
train_std<-mean(train_std)
train_MDD <-mean(train_MDD)

dates_index <- rownames(dat_2_test)

# Now find best delta param 
delta_tries <- c(0.0001,0.005,0.001,0.05,0.01)

# Can also loop through different thresholds 
# but from just changing it manually we see that we should either use 2stds
# or 1. So we stick with one

# Loop through differnt delta values
for ( i in 1:length(delta_tries)){
  pair_backtest(tick1=portfolio[i,1], 
                tick2=portfolio[i,2],jse_data=data_train,dat=dat_2_train,
                delta=delta_tries[i],thresh=std_dev[i])
}

# Formation period
for (i in 1:nrow(portfolio)){
  
  for (j in length(delta_tries)){
    result = pair_backtest(tick1=portfolio[i,1], 
                           tick2=portfolio[i,2],jse_data=data_train,
                           dat=dat_2_train)
    
    train_returns[i]=result[1]
    train_std[i] = result[4]
    train_MDD[i] = result[3]
    
  }
  print(mean(result))
  
}

# We find best delta = 0.0005
# Best trade threshold = 1std

####################################################################
#### Backtest trading period #######################################
###################################################################

# Trading period 
dates_index <- rownames(dat_2_test)

for (i in 1:nrow(portfolio)){
  result = pair_backtest(tick1=portfolio[i,1], 
                         tick2=portfolio[i,2],jse_data=data_test,dat=dat_2_test)
  
  test_returns[i]=result[1]
  test_std[i] = result[4]
  test_MDD[i] = result[3]
  
}

test_returns <- mean(test_returns)
test_std<- mean(test_std)
test_MDD <- mean(test_MDD)

############################  End of script  #################################