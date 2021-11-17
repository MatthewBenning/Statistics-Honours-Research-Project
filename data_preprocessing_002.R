# Script file for JSE top 100 fetching and data cleaning
# Author: Matthew Benning
# Version 1.2

##############################################################################
library(readxl)
library(writexl)
library(BatchGetSymbols)
library(reshape)
library(tidyr)
library(timeSeries)
library(xts)
library(xlsx)

jse <- read_excel('jse_indicators.xlsx',col_names=FALSE)
jse <- jse$...1

# Get the indices to use 
for (i in 1:100){
  jse[i]= paste(jse[i],'.JO',sep='')
  
}


# set dates
first.date <- '2010-01-01'
last.date <- '2020-01-01'
freq.data <- 'daily'
# set tickers

# Now make use of the batch get function to fetch data
# Batch get also automatically checks the liquidty and NA values
l.out <- BatchGetSymbols(tickers = jse, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
jse_data <- as.data.frame(l.out$df.tickers)
jse_data <- jse_data[,6:8]
jse_data_new <- spread(jse_data, key = 'ticker', value = c('price.adjusted'))

# Remove those that were not listed for a period of time
a <- which(colnames(jse_data_new) == 'MSP.JO' )
b <- which(colnames(jse_data_new) == 'RBP.JO' )
c <- which(colnames(jse_data_new) == 'SOLBE1.JO' )

unlisted <- c(a,b,c)

jse_cleaned <- jse_data_new[,-unlisted]

# Total number of assets on which to do the cointegration test
ncol(jse_cleaned)

# Save the tickers used 
tickers_used <- as.vector(colnames(jse_cleaned))

# Save the data and the tickers used
write.csv(tickers_used,'tickers_used_final.csv')
write.csv(jse_cleaned,'jse_data_ready.csv')

# Save as an excel to, in case
write.xlsx(tickers_used,'tickers_used_final.xlsx')
write.xlsx(jse_cleaned,'jse_data_ready.xlsx')

# Some initital plots
jse_cleaned$ref.date <- as.Date(jse_cleaned$ref.date) 
jse_tS <- as.timeSeries(xts(jse_cleaned[,-1],jse_cleaned$ref.date))
plot(jse_tS[,1:10], plot.type='s')
legend('topleft',legend=tickers_used[2:11], )
jse_cleaned[,2:]
