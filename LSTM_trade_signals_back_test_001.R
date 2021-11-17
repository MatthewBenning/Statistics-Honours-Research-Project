# From Python we import the LSTM forecasts
# We then use a similar approach to Kalman Filter to create signals 
# and back test
# Author: Matthew Benning
rm(list=ls())
###########################################################################
# Set up our portfolio again
asset_1<- rbind('NED.JO','PSG.JO','PIK.JO','FFA.JO','NPN.JO')
asset_2 <- rbind('VKE.JO','SOL.JO','SPP.JO','SNT.JO','RDF.JO')
portfolio <- cbind(asset_1,asset_2)
colnames(portfolio) <-c('Asset 1','Asset 2')

###########################################################################
########## Bring in the 7 day forecast from colab ########################
# This is the forecast for the trading period 

spread1<- read.csv("yhat_pair1.csv")
spread2<- read.csv("yhat_pair2.csv")
spread3<- read.csv("yhat_pair3.csv")
spread4<- read.csv("yhat_pair4.csv")
spread5<- read.csv("yhat_pair5.csv")

# We did a 7 day forecast, so we now average over the 7 days
spread1 <- as.vector(rowMeans(spread1[,-1]))
spread2 <- as.vector(rowMeans(spread2[,-1]))
spread3 <- as.vector(rowMeans(spread3[,-1]))
spread4 <- as.vector(rowMeans(spread4[,-1]))
spread5 <- as.vector(rowMeans(spread5[,-1]))
spread_forecast <- cbind(spread1,spread2,spread3,spread4,spread5)
load("~/Desktop/Honours Research Project/spreads.RData")
spreads_actual<- spreads

##########################################################################
# Split the original spreads back into training and testing 
split <- round(0.80*nrow(spreads_actual),0)-1
data_train <- spreads_actual[1:split,]
spread_actual<- spreads_actual[(split+1):nrow(spreads_actual),]
spread_forecast<-scale(spread_forecast)
spread_actual <- scale(spread_actual)
rownames(spread_actual)<-rownames(spread_forecast)
#########################################################################
# Plot all the forecast against actual
for ( i in 1:5){
  
  plot(spread_forecast[,i],type='l',xlab='Days',ylab='Scaled Spread',col='red'
       ,lwd=2,ylim=c(-5,5))
  lines(spread_actual[,i],type='l',col='blue',lwd=2)
  legend(legend=c('Actual Spread','LSTM Forecast Spread'),'bottomleft',
         col=c('blue','red'),lty=1,cex=0.5)
  legend('topright',legend=c(paste(portfolio[i,1],portfolio[i,2])),cex=0.5)
  
}
spread_actual<- scale(spread_actual[1:494,])
#spread_difference<-spread_actual-spread_forecast

########################################################################
# Read in the JSE prices so we can have them for actual values
# Get only the trading period though
load("~/Desktop/Honours Research Project/JSE_Data.RData")
jse_data <- jse_data[(nrow(jse_data)-nrow(spread_forecast)+1):nrow(jse_data),]
# Check that it is correct
nrow(jse_data)
dates_index <- rownames(jse_data)
###########################################################
#### Now set up the trading signals
##########################################################

market_signals_LSTM<- 
  function(tick1,tick2,jse_data=jse_data,spread_forecast=spread_forecast){
  jse_data = jse_data
  
  # Again set up the two assets
  x <- as.data.frame(jse_data[,tick2])
  y <- as.matrix(jse_data[,tick1])
  
  # Set up a matrix to hold the signals alongside assets
  long<- rep(0,nrow(x))
  short <- rep(0,nrow(x))
  exit<- rep(0,nrow(x))
  long_market <-rep(0,nrow(x))
  short_market <-rep(0,nrow(x))
  pairs_signals <- cbind(y,x,spread_forecast,long,short,exit,
                         long_market,short_market)
  colnames(pairs_signals) <-c('Asset1','Asset2','Spread','Std','long','short'
                              ,'Exit','long_market','short_market')
  
  # Now check the error score against the trade threshold and map them
  # We use -0.005 and 0.005 as the exit strat because we only have discrete
  # intraday data so cannot see the exact point at which it hits 0
  for ( i in 1:nrow(x)){
    if (pairs_signals[i,'Spread']>= 1){
      pairs_signals[i,'long']=1
    } else if (pairs_signals[i,'Spread']<= -1){
      pairs_signals[i,'short']=1
    } else if (pairs_signals[i,'Spread'] < 0.005 & pairs_signals[i,'Spread'] >- 0.005){
      pairs_signals[i,'Spread']=1
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
    
    if (pairs_signals[i-1,'long']==1){
      lm=1
    } 
    if (pairs_signals[i-1,'short']==1){
      sm=1
    } 
    if (pairs_signals[i-1,'Spread']==1){
      lm=0
      sm=0
    } 
    # Make sure we arent in any short or long positions at the same time
    # Since its daily we cant check the exact cross-over point 
    if (pairs_signals[i-2,'short_market']==1 & pairs_signals[i-1,'long']==1){
      sm=0
    }
    
    if (pairs_signals[i-2,'long_market']==1 & pairs_signals[i-1,'short']==1){
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
       ylab='Price (ZAC)',lwd=1.5,
       at='chic')
  points(short_y,pch=16,col='red',cex=0.5)
  points(long_y, pch=17,col='green',cex=0.5)
  legend('topleft',legend=c('In the position long','In position short'),
         col=c('green','red'), 
         pch = c(17,16),cex=0.5)
  legend(legend=paste(tick1),'bottomright',cex=0.5)
  
  plot(jse_data[,tick2],type='l',col='blue',xlab='Date',ylab='Price (ZAC)',
       lwd=1.5,at='chic')
  points(short_x, pch=16, col='red', cex=0.5)
  points(long_x,pch=17,col='green',cex=0.5)
  legend('topleft',legend=c('In position long','In position short'),
         col=c('green','red',cex=0.5), 
         pch = c(17,16),cex=0.75)
  legend(legend=paste(tick2),'bottomleft',cex=0.5,col=c('blue','darkorange'),
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

###################################################################
##### Set up backtest ##############################

pair_backtest_LSTM<- function(tick1,tick2,jse_data=jse_data,spread_forecast=spread_forecast){
  
  # Calculate the daily returns using log prices of original data
  log_prices <- as.matrix(log(jse_data))
  daily_returns <- diff((log_prices))
  daily_returns <- daily_returns[,c(tick1,tick2)]
  
  market_pos_x <-matrix(0,nrow=nrow(daily_returns),1)
  market_pos_y <-matrix(0,nrow=nrow(daily_returns),1)
  # Get the market position for both assets 
  #in one vector where -1 = short and 1 = long
  results <- market_signals_LSTM(tick1,tick2,jse_data = jse_data,spread_forecast
                                 = spread_forecast)
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
  charts.PerformanceSummary(pair_returns,main='',type='h')
  
  
  return(pair_summary)
  
}


###################################################################
##### Backtest the trading period ###############################

# Holders 
test_returns=NULL
test_std=NULL
test_MDD=NULL

# Loop through all 5 pairs
for (i in 1:nrow(portfolio)){
  result = pair_backtest_LSTM(tick1=portfolio[i,1], 
                         tick2=portfolio[i,2],jse_data=jse_data,spread_forecast 
                         = spread_forecast)
  
  test_returns[i]=result[1]
  test_std[i] = result[4]
  test_MDD[i] = result[3]
  
}

test_returns <- mean(test_returns)
test_std<- mean(test_std)
test_MDD <- mean(test_MDD)

print(test_returns)
print(test_std)
print(test_MDD)

################### End of script #####################################
