# Setting current working directory
# Add your working directory here
setwd("~/Desktop/Fintech-Constructing-a-Alpha-Model")

# Required libraries
library("TTR");
library(quantmod);

# common function
get_year <- function(x) strsplit(x,'-')[[1]][1]
get_year_month <- function(x) paste(strsplit(x,'-')[[1]][1],strsplit(x,'-')[[1]][2],sep='-')

# Define date range for data 
start.date <- "2011-01-01"; end.date <- "2021-12-31"

################################################################################
#################################### Data ######################################
################################################################################
# 1. Daily Stock Data           (done)
# 2. Fundamental Data
# 3. Sentiment Data
# 4. Economic Data
# 5. S&P500 index data          (done)
# 6. Technical indicator data 

d

# Fundamental Data and Financial Ratios
funddata.AAPL <- read.csv( "Data/FundamentalData/appl_fin_ratios.csv", header = TRUE)
funddata.AAPL_new <- t(funddata.AAPL)
funddata.AMZN <- read.csv( "Data/FundamentalData/appl_fin_ratios.csv", header = TRUE)
funddata.AMZN_new <- t(funddata.AMZN)
funddata.TSLA <- read.csv( "Data/FundamentalData/appl_fin_ratios.csv", header = TRUE)
funddata.TSLA_new <- t(funddata.TSLA)

# Economic Data
# Crude oil data
oil.prices <- read.csv( "Data/EconomicData/crude-oil-prices.csv", header = TRUE)
# removing un-necessary columns
oil.prices = oil.prices[,-1]
oil.prices = oil.prices[,-1]
names(oil.prices) <- c('Year','Oil Price')

# Unemployment rate dataset
getSymbols("UNRATE",src = "FRED")
unrate<-UNRATE["2011-01-01/2021-12-31"]
unrate = as.data.frame(unrate)
unrate['Month'] = rownames(unrate)
unrate['Month'] = apply(unrate['Month'],1,get_year_month)

# GDP data
GDP <- read.csv( "Data/EconomicData/GDP-Data.csv", header = TRUE)
GDP['GDP_%Change'] <-  Delt(GDP$GDP)
GDP['Year'] = apply(GDP['DATE'],1,get_year)
GDP['Quarter'] = quarters(as.Date(GDP$DATE))
GDP['Quarter'] = paste(GDP$Year,GDP$Quarter,sep="-")


################################################################################
############################## Data Manipulation ###############################
################################################################################

# Data Manipulation and Organization
# 1. Get log returns of all stocks in the portfolio
# 2. Organize fundamental data , join data frames to match index (date)
# 3. Perform sentiment analysis and merge sentiment scores in the dataframe
# 4. Merge economic data
# 5. Merge SP500 daily change percentage column (y)

# Calculating daily returns % of each stock in the portfolio
AMZN.ret <- Delt(data.AMZN$AMZN.Adjusted)
AAPL.ret <- Delt(data.AAPL$AAPL.Adjusted)
TSLA.ret <- Delt(data.TSLA$TSLA.Adjusted)

# Renaming columns
names(AMZN.ret) <- paste("AMZN.ret")
names(AAPL.ret) <- paste("AAPL.ret")
names(TSLA.ret) <- paste("TSLA.ret")


# Calculating S&P500 daily returns
data.GSPC <- getSymbols("^GSPC",from="2010-12-31",to="2020-12-31",auto.assign=FALSE)
GSPC.ret <- Delt(data.GSPC$GSPC.Adjusted)
names(GSPC.ret) <- paste("GSPC.ret")

# main_df for each stock
main_df_AAPL = cbind(AAPL.ret,GSPC.ret)
main_df_AMZN = cbind(AMZN.ret,GSPC.ret)
main_df_TSLA = cbind(TSLA.ret,GSPC.ret)

# adding the year month and quarter columns
main_df_AAPL = as.data.frame(main_df_AAPL)
main_df_AMZN = as.data.frame(main_df_AMZN)
main_df_TSLA = as.data.frame(main_df_TSLA)
main_df_AAPL <- na.omit(main_df_AAPL)
main_df_AMZN <- na.omit(main_df_AMZN)
main_df_TSLA <- na.omit(main_df_TSLA)
main_df_AAPL['Year'] = rownames(main_df_AAPL)
main_df_AMZN['Year'] = rownames(main_df_AMZN)
main_df_TSLA['Year'] = rownames(main_df_TSLA)
main_df_AAPL['Quarter'] = quarters(as.Date(main_df_AAPL$Year))
main_df_AAPL['Quarter'] = quarters(as.Date(main_df_AMZN$Year))
main_df_AAPL['Quarter'] = quarters(as.Date(main_df_TSLA$Year))
main_df_AAPL['Year'] = apply(main_df_AAPL['Year'],1,get_year)
main_df_AMZN['Year'] = apply(main_df_AMZN['Year'],1,get_year)
main_df_TSLA['Year'] = apply(main_df_TSLA['Year'],1,get_year)
main_df_AAPL['Month'] = rownames(main_df_AAPL)
main_df_AMZN['Month'] = rownames(main_df_AMZN)
main_df_TSLA['Month'] = rownames(main_df_TSLA)
main_df_AAPL['Month'] = apply(main_df_AAPL['Month'],1,get_year_month)
main_df_AMZN['Month'] = apply(main_df_AMZN['Month'],1,get_year_month)
main_df_TSLA['Month'] = apply(main_df_TSLA['Month'],1,get_year_month)
main_df_AAPL['Quarter'] = paste(main_df_AAPL$Year,main_df_AAPL$Quarter,sep="-")
main_df_AMZN['Quarter'] = paste(main_df_AMZN$Year,main_df_AMZN$Quarter,sep="-")
main_df_TSLA['Quarter'] = paste(main_df_TSLA$Year,main_df_TSLA$Quarter,sep="-")

# Financial ratios
finratio.AAPL <- funddata.AAPL_new[,c(2,22,47,30)]
finratio.AMZN <- funddata.AMZN_new[,c(2,22,47,30)]
finratio.TSLA <- funddata.TSLA_new[,c(2,22,47,30)]

# renaming columns and fixing the fundamental data
# AAPL
finratio.AAPL <- as.data.frame(finratio.AAPL)
names(finratio.AAPL) <- finratio.AAPL[1,] 
finratio.AAPL <- finratio.AAPL[-1,]
removex <- function(x) strsplit(x,'X')[[1]][2]
rownames(finratio.AAPL) <- apply(as.array(rownames(finratio.AAPL)),1,removex)
colnames(finratio.AAPL) <- c('currentRatioAAPL',   'debtEquityRatioAAPL', 'priceEarningsRatioAAPL', 'inventoryTurnoverAAPL')
finratio.AAPL['Year'] = rownames(finratio.AAPL)
finratio.AAPL['Year'] = apply(finratio.AAPL['Year'],1,get_year)

# AMZN
finratio.AMZN <- as.data.frame(finratio.AMZN)
names(finratio.AMZN) <- finratio.AMZN[1,]
finratio.AMZN <- finratio.AMZN[-1,]
removex <- function(x) strsplit(x,'X')[[1]][2]
rownames(finratio.AMZN) <- apply(as.array(rownames(finratio.AMZN)),1,removex)
colnames(finratio.AMZN) <- c('currentRatioAMZN',   'debtEquityRatioAMZN', 'priceEarningsRatioAMZN', 'inventoryTurnoverAMZN')
finratio.AMZN['Year'] = as.array(rownames(finratio.AMZN))
finratio.AMZN['Year'] = apply(finratio.AMZN['Year'],1,get_year)

# TSLA
finratio.TSLA <- as.data.frame(finratio.TSLA)
names(finratio.TSLA) <- finratio.TSLA[1,]
finratio.TSLA <- finratio.TSLA[-1,]
removex <- function(x) strsplit(x,'X')[[1]][2]
rownames(finratio.TSLA) <- apply(as.array(rownames(finratio.TSLA)),1,removex)
colnames(finratio.TSLA) <- c('currentRatioTSLA',   'debtEquityRatioTSLA', 'priceEarningsRatioTSLA', 'inventoryTurnoverTSLA')
finratio.TSLA['Year'] = as.array(rownames(finratio.TSLA))
finratio.TSLA['Year'] = apply(finratio.TSLA['Year'],1,get_year)


# merging all data frames
# merging fundamental data
df_merge_AAPL <- merge(main_df_AAPL, finratio.AAPL, by = "Year")
df_merge_AMZN <- merge(main_df_AMZN, finratio.AMZN, by = "Year",all.x = TRUE)
df_merge_TSLA <- merge(main_df_TSLA, finratio.TSLA, by = "Year",all.x = TRUE)
# merging economic data
# merging oil data
df_merge_AAPL <- merge(df_merge_AAPL,oil.prices, by = "Year",all.x = TRUE)
df_merge_AMZN <- merge(df_merge_AMZN,oil.prices, by = "Year",all.x = TRUE)
df_merge_TSLA <- merge(df_merge_TSLA,oil.prices, by = "Year",all.x = TRUE)
# merging unemployment rate
df_merge_AAPL <- merge(df_merge_AAPL,unrate, by = "Month",all.x = TRUE)
df_merge_AMZN <- merge(df_merge_AMZN,unrate, by = "Month",all.x = TRUE)
df_merge_TSLA <- merge(df_merge_TSLA,unrate, by = "Month",all.x = TRUE)
# merging percent change in GDP
df_merge_AAPL <- merge(df_merge_AAPL,GDP, by = "Quarter",all.x = TRUE)
df_merge_AMZN <- merge(df_merge_AMZN,GDP, by = "Quarter",all.x = TRUE)
df_merge_TSLA <- merge(df_merge_TSLA,GDP, by = "Quarter",all.x = TRUE)

# adding dates on index
rownames(df_merge_AAPL) <- rownames(main_df_AAPL)
rownames(df_merge_AMZN) <- rownames(main_df_AMZN)
rownames(df_merge_TSLA) <- rownames(main_df_TSLA)


# removing unnecessary columns
df_merge_AAPL = within(df_merge_AAPL, rm("Year.x","Year.y","Month","Quarter","GDP","DATE"))
df_merge_AMZN = within(df_merge_AMZN, rm("Year.x","Year.y","Month","Quarter","GDP","DATE"))
df_merge_TSLA = within(df_merge_TSLA, rm("Year.x","Year.y","Month","Quarter","GDP","DATE"))


#write.csv(df_merge, 'df.merge.csv')
# Portfolio daily returns , assuming the portfolio stocks are weighted equally
#PORTFOLIO.ret =  0.33*AMZN.ret + 0.33*AAPL.ret + 0.34*TSLA.ret
#names(PORTFOLIO.ret) <- paste("PORFOLIO.ret")
##### plots
##### scatter plot between portfolio and sp500
#apple_returns = as.vector(c(AAPL.logret$AAPL.Adjusted))
#sp500_returns = as.vector(c(AAPL.logret$AAPL.Adjusted))
# adding scatter plot
#plot(apple_returns, sp500_returns, main="Scatterplot Example",
#     xlab="SP500 returns ", ylab="APP returns ", pch=19)


# for each stock            (x)--->
#index , stock.returns(y)* , (fun1, fun2, fun3, fun4)*, [year,eco1, eco2, eco3], (ta1, ta2, ta3), [sp500]*, (sentimentdata)
#() - different
#[] - same



################################################################################
########################### Adding more factors ################################
################################################################################


# Add additional indicators (optional for now)
# 1. Technical indicators,
# 2. Momentum indicators
# 3. ....

# Simple moving average 
sma20.AAPL<-SMA(data.AAPL$AAPL.Adjusted,n=20) 
sma20.AMZN<-SMA(data.AMZN$AMZN.Adjusted,n=20)
sma20.TSLA<-SMA(data.TSLA$TSLA.Adjusted,n=20)

# MACD
macd.AAPL<-MACD(data.AAPL$AAPL.Adjusted, nFast = 12, nSlow = 26, nSig = 9, maType = SMA)
macd.AMZN<-MACD(data.AAPL$AAPL.Adjusted, nFast = 12, nSlow = 26, nSig = 9, maType = SMA)
macd.TSLA<-MACD(data.AAPL$AAPL.Adjusted, nFast = 12, nSlow = 26, nSig = 9, maType = SMA)

# Merging TA indicators
# Combine indicators with stock data 
# AAPL
ta_AAPL <- as.data.frame(merge(sma20.AAPL, macd.AAPL))
ta_AAPL['date'] <- rownames(ta_AAPL)
df_merge_AAPL['date'] <- rownames(df_merge_AAPL)
df_merge_AAPL = data.frame(merge(df_merge_AAPL,ta_AAPL, by = "date",all.x = TRUE))
df_merge_AAPL['stock'] = 'AAPL'

# AMZN
ta_AMZN <- as.data.frame(merge(sma20.AMZN, macd.AMZN))
ta_AMZN['date'] <- rownames(ta_AMZN)
df_merge_AMZN['date'] <- rownames(df_merge_AMZN)
df_merge_AMZN = data.frame(merge(df_merge_AMZN,ta_AMZN, by = "date",all.x = TRUE))
df_merge_AAPL['stock'] = 'AMZN'

# TSLA
ta_TSLA <- as.data.frame(merge(sma20.TSLA, macd.TSLA))
ta_TSLA['date'] <- rownames(ta_TSLA)
df_merge_TSLA['date'] <- rownames(df_merge_TSLA)
df_merge_TSLA = data.frame(merge(df_merge_TSLA,ta_TSLA, by = "date",all.x = TRUE))
df_merge_AAPL['stock'] = 'TSLA'

panel_data = rbind(df_merge_AAPL,df_merge_AMZN,df_merge_TSLA)

# Factor Model
# 1. Run linear regression on factors
# 2. Run lm.summary() and select the significant factors
# 3. Drop factor which are not significant from the data frame

# tentative code
# what about panel data
# portfolio_factor_analysis <-lm(FF.data$exret ~ RmxRf + SMB + HML, data=FF.data)



# Execute the alpha model and report performance
# 1. Implement a strategy? - what is the premise, what indicators / factors to consider
# 2. Generate signals
# 3. Take positions
# 4. Calculate metrics : P&L, underwater plot, sharpe ratio, alpha, Beta, max drawdowns etc...  
# 2. Open to suggestions on this one (ill check the lectures)

# Performance ANalytics
#port.ret <-WGT.2asset %*% mat.ret  
library(PerformanceAnalytics)
Return.annualized(returns$AMZN.ret)
maxDrawdown(returns$AMZN.ret)
#charts.PerformanceSummary(returns$AMZN.ret,methods="GaussianES")
SharpeRatio.annualized(returns$AMZN.ret)
#SharpeRatio(returns$AMZN.ret)
VaR(returns$AMZN.ret, 0.05,method="gaussian")
KellyRatio(returns$AMZN.ret)

InformationRatio(returns$AMZN.ret,returns$IBM.ret,scale=252) #the 2nd argument is the benchmark
chart.VaRSensitivity(returns$AMZN.ret) #chart includes VaR, ES
chart.VaRSensitivity(returns$IBM.ret)
charts.PerformanceSummary(returns$AMZN.ret)
#charts.PerformanceSummary(returns$AMZN.ret,methods="GaussianES") #other methods GaussianVaR, HistoricalVaR


