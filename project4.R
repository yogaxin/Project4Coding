
library(quantmod)

# 定义我们要获取数据的股票代码和日期
symbols <- c("BTC-USD", "ETH-USD", "LTC-USD","XRP-USD","ADA-USD","TSLA", "AAPL", "AMZN", "MSFT", "PG")
start_date <- as.Date("2020-05-10")
end_date <- as.Date("2023-05-10")

# 初始化一个xts对象用于存储所有的收盘价格
all_close_prices <- xts()

# 遍历所有的股票代码
for(symbol in symbols){
  # 获取股票数据
  stock_data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  # 提取收盘价格
  close_prices <- Cl(stock_data)
  # 添加到我们的xts对象中
  all_close_prices <- merge(all_close_prices, close_prices)
}

# Rename Columns
names(all_close_prices) <- c("BTC-USD", "ETH-USD", "LTC-USD","XRP-USD","ADA-USD","TSLA", "AAPL", "AMZN", "MSFT", "PG")

# 将结果存储到文件中
write.zoo(all_close_prices, file = "G:/BA资料/FM/IA/4/Close Prices.csv", sep = ",", index.name = "Date")
#=========================================================================================================
#Pearson Correlation
setwd("G:\\BA资料\\FM\\IA\\P4")
close.prices = read.csv("Close Prices.csv", header = TRUE) #importing the data
close.prices <- na.omit(close.prices)
# create a correlation matrix
cor_matrix <- cor(close.prices[, c("BTC.USD", "ETH.USD", "LTC.USD","XRP.USD","ADA.USD","TSLA", "AAPL", "AMZN", "MSFT", "PG")])

# print the correlation matrix
print(cor_matrix)

# convert the matrix to a dataframe
cor_df <- as.data.frame(cor_matrix)

# load the knitr package
library(knitr)

# create a table
kable(cor_df, digits = 3)
# write the dataframe to a .csv file
write.csv(cor_df, file = "correlation_table.csv")

# define a function to apply cor.test to each pair of variables
cor_test <- function(x, y) cor.test(x, y, method = "spearman")$p.value
library(dplyr)
# Remove rows with NA or Inf values
clean_close_prices <- close.prices %>% 
  filter_all(all_vars(!is.na(.) & !is.infinite(.)))
# Remove the Date column
clean_close_prices <- clean_close_prices[, -which(names(clean_close_prices) %in% "Date")]

# Initialize an empty matrix to hold the p-values
p_values <- matrix(nrow = ncol(clean_close_prices), ncol = ncol(clean_close_prices))

str(clean_close_prices)

# Loop over each pair of columns
for(i in 1:ncol(clean_close_prices)) {
  for(j in i:ncol(clean_close_prices)) {
    # Calculate the Spearman correlation and extract the p-value
    test <- cor.test(clean_close_prices[,i], clean_close_prices[,j], method = "spearman")
    p_values[i,j] <- test$p.value
    p_values[j,i] <- test$p.value
  }
}

# Convert the matrix to a data frame for easier viewing
p_values <- as.data.frame(p_values)

# Set the row and column names
rownames(p_values) <- colnames(clean_close_prices)
colnames(p_values) <- colnames(clean_close_prices)

print(p_values)
# create a table
kable(p_values, digits = 3)
# write the dataframe to a .csv file
write.csv(p_values, file = "p_values_table.csv")

#=====================================================================================================
##volitilities
library(DescTools)

# Initialize an empty list to store return data for each asset
asset_returns <- list()

# Loop over each column in the clean_close_prices dataframe
for (asset in colnames(clean_close_prices)) {
  
  # Calculate log returns
  data.xts <- clean_close_prices[[asset]]
  return <- diff(log(data.xts))
  
  # Store return data in the list
  asset_returns[[asset]] <- return
  
  # Calculate and print estimates of variability
  
  # Mean absolute deviation
  mean_absolute_deviation <- MeanAD(return, na.rm = TRUE)
  cat("\nMean Absolute Deviation for", asset, ":", mean_absolute_deviation)
  
  # Variance
  variance <- var(return, na.rm = TRUE)
  cat("\nVariance for", asset, ":", variance)
  
  # Standard deviation
  standard_deviation <- sd(return, na.rm = TRUE)
  cat("\nStandard Deviation for", asset, ":", standard_deviation)
  
  # Median Absolute Deviation
  median_absolute_deviation <- mad(return, na.rm = TRUE)
  cat("\nMedian Absolute Deviation for", asset, ":", median_absolute_deviation)
}


# Install the package if you haven't already
if (!require(openxlsx)) {
  install.packages("openxlsx")
}

# Load the required package
library(openxlsx)

# Create a data frame
volatility_df <- data.frame(
  Asset = c("BTC.USD", "ETH.USD", "LTC.USD", "XRP.USD", "ADA.USD", "TSLA", "AAPL", "AMZN", "MSFT", "PG"),
  Volatility = c(0.04239522, 0.05741449, 0.0596459, 0.07257317, 0.06622882, 0.04094615, 0.01995675, 0.02403013, 0.0185065, 0.01123113)
)

# Order the data frame by Volatility in descending order
volatility_df <- volatility_df[order(-volatility_df$Volatility), ]

# Transpose the data frame for horizontal output
volatility_df_t <- t(volatility_df)

# Write to an Excel file
write.csv(volatility_df_t, "volatility_data.csv")


#==================================================================================================
## Causality based on DAG

library(quantmod)
library(dplyr)

library(pcalg)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")
install.packages("pcalg")
library(pcalg)
library(graph)
library(ggm)
close.prices
data = cbind(close.prices$BTC.USD, close.prices$ETH.USD, close.prices$LTC.USD, close.prices$XRP.USD, 
             close.prices$ADA.USD, close.prices$TSLA, close.prices$AAPL, close.prices$AMZN,
             close.prices$MSFT,close.prices$PG)
colnames(data) = c("BTC.USD",  "ETH.USD", 
                   "LTC.USD", "XRP.USD",
                   "ADA.USD", "TSLA", "AAPL", "AMZN",
                   "MSFT",  "PG")
head(data)

# Estimate the DAG:
suffStat = list(C = cor(data), n = nrow(data))
dag = pc(suffStat, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data))
summary(dag)
# Plot the estimated DAG:
library(Rgraphviz)
plot(dag)
#===================================================================================================
#statistical Model
# Subset the data to only include the relevant columns
subset_data = close.prices[,c("BTC.USD", "ETH.USD", "LTC.USD", "XRP.USD", "ADA.USD", "TSLA", "AAPL", "AMZN", "MSFT",  "PG")]

# Conduct PCA
pca_model = prcomp(subset_data, scale. = TRUE)

# Print a summary of the PCA
summary(pca_model)
plot(pca_model)
#===================================================================================================
#economic model
# List of all assets
assets = c("BTC.USD", "ETH.USD", "LTC.USD", "XRP.USD", "ADA.USD", "TSLA", "AAPL", "AMZN", "MSFT", "PG")

# Initialize a list to store the VAR models
VAR_models = list()

# Loop over all assets
for (asset in assets) {
  # Subset the data to only include the current asset
  subset_data = close.prices[,asset]
  
  # Ensure the data is in a time-series format
  asset_time_series = ts(subset_data)
  
  # Fit a VAR model to the data
  VAR_models[[asset]] = VAR(asset_time_series, lag.max = 1)  # Here 'lag.max' is the maximum number of lags to be considered for the lag order selection, adjust as necessary
}

# Print a summary of the VAR models
for (asset in assets) {
  print(paste("Summary for", asset))
  print(summary(VAR_models[[asset]]))
}

#====================================================================================================
equityFunds = close.prices
# Example : Factor analysis of equity funds

# VARIMAX rotation
fa_vari = factanal(equityFunds[,c(2:9)],3,rotation="varimax") #factor model with rotation


print(fa_vari,cutoff=0.1,sort=T)
print(fa_vari,cutoff=0.1)
sum(fa_vari$loadings[,1]^2)
B=fa_vari$loadings[,] # factor loadings



# Going into details

library (psych) # the library for factor analysis
library (GPArotation) # to estimate oblimin rotation
equities = equityFunds[,2:9]
describe(equities) # general description of the data 

##Assessing the Factorability of the Data
#Bartlett's Test of Sphericity
cortest.bartlett(equities)
#KMO
KMO(equities)


##Determining the Number of Factors to Extract
# scree plot
scree(equities)

#Parallel Analysis
fa.parallel (equities) #


# estimation factor model
factor.model <- fa(equities, nfactors = 3, fm="ols", max.iter = 100, rotate = "oblimin")
# we estimate a factor model with 3 factors, estimated by means of OLS, 100 is the number of iterations or attempts to use when identifying the "best"" solution
# rotate - we apply oblimin rotation, allowing factors to correlate.


# make it visual
fa.diagram(factor.model) # 
# we can see that factor 1 is the common factor for GOld and mining equities
# Factor 2 affects energy, Latam and water equities
# and Factor 3 affects India and China
# it means that the respective groups of the equities have something in common. 

# Communality 
factor.model$communality

#Eeigenvalues
factor.model$e.values

#Percentage of Variance Accounted For
100*factor.model$e.values/length(factor.model$e.values)


print(factor.model$loadings, cutoff=0, digits=3)
print(factor.model$Structure, cutoff=0, digits=3)

#=======================================================================================================================================================================
# Load required packages
library(quantmod)
library(tidyquant)
library(dplyr)

# Define the ticker symbols and the date range
symbols <- c("BTC-USD", "ETH-USD", "LTC-USD","XRP-USD","ADA-USD","TSLA", "AAPL", "AMZN", "MSFT", "PG")
start_date <- as.Date("2020-05-10")
end_date <- as.Date("2023-05-10")

# GBM Discretization parameters
time_step = 1/252  # Assuming 252 trading days per year
num_simulations = 1000
forecast_period = 5  # 5 trading days for next week

# Function to simulate stock prices
simulate_stock_prices = function(current_price, daily_return, daily_volatility, time_step, num_simulations, forecast_period) {
  random_daily_returns = matrix(rnorm(num_simulations * forecast_period, mean = daily_return * time_step, sd = daily_volatility * sqrt(time_step)), nrow = num_simulations)
  price_paths = current_price * t(apply(1 + random_daily_returns, 1, cumprod))
  return(price_paths)
}

# Loop over all symbols
for (symbol in symbols) {
  # Download stock prices
  prices = tq_get(symbol, from = start_date, to = end_date)
  
  # Calculate log returns using adjusted prices
  daily_returns = prices %>%
    dplyr::select(date, adjusted) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, type = "log")
  
  # Calculate return and volatility
  daily_return = mean(daily_returns$monthly.returns)
  daily_volatility = sd(daily_returns$monthly.returns)
  
  # Get the most recent adjusted closing price
  current_price = tail(prices$adjusted, 1)
  
  # Simulate stock prices for next week
  simulated_prices = simulate_stock_prices(current_price, daily_return, daily_volatility, time_step, num_simulations, forecast_period)
  
  # Calculate the expected stock price for the end of next week
  expected_future_price = mean(simulated_prices[, forecast_period])
  
  # Print the expected future price
  print(paste(symbol, expected_future_price))
}





