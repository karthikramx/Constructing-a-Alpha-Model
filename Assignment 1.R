# Setting current working directory
# Add your working directory here
setwd("~/Desktop/Fintech-Constructing-a-Alpha-Model")



################################################################################
#################################### Data ######################################
################################################################################
# 1. Daily Stock Data   (done)
# 2. Fundamental Data
# 3. Sentiment Data
# 4. Economic Data
# 5. S&P500 index data  (done)


library(quantmod);
# Storing daily OHLCV data of the stock portfolio
data.AMZN <- getSymbols("AMZN",from="2010-12-31",to="2020-12-31",auto.assign=FALSE)
data.AAPL <- getSymbols("AAPL",from="2010-12-31",to="2020-12-31",auto.assign=FALSE)
data.TSLA <- getSymbols("TSLA",from="2010-12-31",to="2020-12-31",auto.assign=FALSE)

# Calculating daily returns % of each stock in the portfolio
AMZN.ret <- Delt(data.AMZN$AMZN.Adjusted)
AAPL.ret <- Delt(data.AAPL$AAPL.Adjusted)
TSLA.ret <- Delt(data.TSLA$TSLA.Adjusted)

# Renaming columns
names(AMZN.ret) <- paste("AMZN.ret")
names(AAPL.ret) <- paste("AAPL.ret")
names(TSLA.ret) <- paste("TSLA.ret")

# Portfolio daily returns , assuming the portfolio stocks are weighted equally
PORTFOLIO.ret =  0.33*AMZN.ret + 0.33*AAPL.ret + 0.34*TSLA.ret
names(PORTFOLIO.ret) <- paste("PORFOLIO.ret")

# Calculating S&P500 daily returns
data.GSPC <- getSymbols("^GSPC",from="2010-12-31",to="2020-12-31",auto.assign=FALSE)
GSPC.ret <- Delt(data.GSPC$GSPC.Adjusted)
names(GSPC.ret) <- paste("GSPC.ret")

# main_df
main_df = cbind(PORTFOLIO.ret,GSPC.ret)
# adding the year column

main_df = as.data.frame(main_df)
main_df['Year'] = rownames(main_df)
get_year <- function(x) strsplit(x,'-')[[1]][1]
main_df['Year'] = apply(main_df['Year'],1,get_year)
main_df <- na.omit(main_df)


# Fundamental Data and Financial Ratios
funddata.AAPL <- read.csv( "Data/FundamentalData/appl_fin_ratios.csv", header = TRUE)
funddata.AAPL_new <- t(funddata.AAPL)
funddata.AMZN <- read.csv( "Data/FundamentalData/appl_fin_ratios.csv", header = TRUE)
funddata.AMZN_new <- t(funddata.AMZN)
funddata.TSLA <- read.csv( "Data/FundamentalData/appl_fin_ratios.csv", header = TRUE)
funddata.TSLA_new <- t(funddata.TSLA)

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
df_merge <- merge(main_df, finratio.AAPL, by = "Year",all.x = TRUE)
df_merge <- merge(df_merge, finratio.AMZN, by = "Year",all.x = TRUE)
df_merge <- merge(df_merge, finratio.TSLA, by = "Year",all.x = TRUE)

write.csv(df_merge, 'df.merge.csv')






################################################################################
############################## Data Manipulation ###############################
################################################################################

# Data Manipulation and Organization
# 1. Get log returns of all stocks in the portfolio
# 2. Organize fundamental data , join data frames to match index (date)
# 3. Perform sentiment analysis and merge sentiment scores in the dataframe
# 4. Merge economic data
# 5. Merge SP500 daily change percentage column (y)


################################################################################
########################### Adding more factors ################################
################################################################################



# Add additional indicators (optional for now)
# 1. Technical indicators,
# 2. Momentum indicators
# 3. ....


# Factor Model
# 1. Run linear regression on factors
# 2. Run lm.summary() and select the significant factors
# 3. Drop factor which are not significant from the data frame

# Execute the alpha model and report performance
# 1. Implement a strategy? - what is the premise, what indicators / factors to consider
# 2. Generate signals
# 3. Take positions
# 4. Calculate metrics : P&L, underwater plot, sharpe ratio, alpha, Beta, max drawdowns etc...  
# 2. Open to suggestions on this one (ill check the lectures)
