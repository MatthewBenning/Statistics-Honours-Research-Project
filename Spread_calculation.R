# LSTM for forecasting the spread 
# Author: Matthew Benning
# Other scripts used are in Python
# Training is run on python on colab GPU 
rm(list=ls())
load("~/Desktop/Honours Research Project/JSE_Data.RData")

##########################################################################
# Split the data into training and testing 
split <- round(0.70*nrow(jse_data),0)-1
data_train <- jse_data[1:split,]
data_test <- jse_data[(split+1):nrow(jse_data),]

#########################################################################
# Get the tickers to be used in our portfolio 
asset_1<- rbind('NED.JO','ANG.JO','LBH.JO','ABG.JO','RDF.JO')
asset_2 <- rbind('NRP.JO','GFI.JO','SNT.JO','DSY.JO','SLM.JO')
portfolio <- cbind(asset_1,asset_2)
colnames(portfolio) <-c('Asset 1','Asset 2')

#########################################################################
# Calcualte the static hedge ratio and corresponding spread 
# for the training period,
# this will be used to calculate our spread in the forecast

static_hedge_ratio_spread<- function(tick1,tick2,data_train=data_train,data_test=data_test){
  # Set up the two assets
  # Traiining period 
  x_train<- data_train[,tick2]
  y_train<- data_train[,tick1]
  
  # Testing period 
  x_test<- data_test[,tick2]
  y_test<- data_test[,tick1]
  
  spread_train = matrix(0,ncol=1,nrow=nrow(x_train))
  spread_test = matrix(0,ncol=1,nrow=nrow(x_test))
  # Run OLS
  mod <- lm(y_train~x_train)
  static_hedge_ratio <- mod$coefficient[2]
  spread_train= y_train-static_hedge_ratio*x_train
  spread_test = y_test-static_hedge_ratio*x_test
  spread = rbind(spread_train,spread_test)
  
  return(rbind(spread_train,spread_test))
}


run1 <- static_hedge_ratio_spread(tick1='PIK.JO',tick2='SPP.JO',data_train=data_train,data_test=data_test)
# Obtain spreads for all five pairs 
spreads = matrix(ncol=5,nrow=nrow(jse_data))
for (i in 1:nrow(portfolio)){
  spreads[,i] = static_hedge_ratio_spread(tick1=portfolio[i,1],
  tick2=portfolio[i,2],data_train = data_train,data_test = data_test)
}



portfolio[1,1]
tick1=portfolio[1,1]
tick2=portfolio[1,2]

static_hedge_ratio_spread(tick1='PIK.JO',tick2='SPP.JO')
