library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyverse)

rm(list=ls())

set.seed(11)


############# TICKERS OF ASSETS ###############


#tick <- c('VDE', 'VCR', 'VOX', 'VFH', 'VWOB', 'VGT', 'VAW', 'VNQ')
#tick <- c('VDE', 'VCR', 'VOX', 'VFH', 'VWOB', 'VGT', 'VAW', 'VNQ', 'VV', 'VB', 'VGIT')
#tick <- c('ACN', 'AMZN', 'COST', 'F', 'GILD', 'JPM', 'KO', 'LUV', 'MA', 'MSFT', 'PFE', 'TSLA')
# MAYBE USE GROWTH INDICES

#tick <- c('MGK', 'VIOG', 'VBK','VGIT', 'VWOB')
tick <- c('MGK', 'EWW', 'SPAB', 'VV', 'VB','VGK')


# TODO: Add description of each asset

##############################################
### "TRAINING DATA"
price_data <- tq_get(tick,
                     from = '2018-01-01',
                     to = as.character(Sys.Date()),
                     get = 'stock.prices')

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

market_prices <- tq_get('^GSPC',
                        from = '2018-01-01',
                        to = as.character(Sys.Date()),
                        get = 'stock.prices')

# THIS GETS DATA INTO WIDER FORMAT

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

price_data_xts <- price_data  %>%
  select(c("date","symbol","adjusted")) %>%
  group_by(symbol) %>%
  spread(symbol, value = adjusted) %>%
  tk_xts()

market_price_xts <- market_prices  %>%
  select(c("date","symbol","adjusted")) %>%
  group_by(symbol) %>%
  spread(symbol, value = adjusted) %>%
  tk_xts()


##### GET THE STOCK PRICES IN LONG FORMAT AND ALSO 1 SERIES NORMALIZED TO 100
price_data_asdf <- data.frame(date=index(price_data_xts), coredata(price_data_xts))
market_price_asdf <- data.frame(date=index(market_price_xts), coredata(market_price_xts))



#### "TESTING DATA" #####

price_data_test <- tq_get(tick,
                     from = '2022-05-02',
                     to = '2023-05-01',
                     get = 'stock.prices')

log_ret_tidy_test <- price_data_test %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

# THIS GETS DATA INTO WIDER FORMAT

log_ret_xts_test <- log_ret_tidy_test %>%
  spread(symbol, value = ret) %>%
  tk_xts()

### CALCULATE MEAN RETURNS OF ASSETS ##########
# DAILY:
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

# ANNUALIZED:
mean_ret_an <- (mean_ret + 1)^252 - 1
print(round(mean_ret_an, 5))

################################

cov_mat <- cov(log_ret_xts) * 252

#create random weights
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

port_returns <- (sum(wts * mean_ret) + 1)^252 - 1
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Since Risk free rate is 0% 

sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)

# Calculate the random weights
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk



#### CREATE PORTFOLIOS FOR SIMULATION ############

num_port <- 50000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)


############ SIMULATE PORTFOLIOS ###############

for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)


min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

# to_prices <- function(x) {
#   return(exp(cumsum(x)) * 100)
# }
# 
# 
# port_ret <- data.frame(log_ret_xts %*% head(t(portfolio_values),length(tick)))
# port_ret <- port_ret %>% mutate_all(list(port_price = to_prices))



############### THE FOLLOWING IS EXTREMELY SLOW, SHOULD FIND ANOTHER WAY TO DO IT. ##################

# port_prices0 <-  data.frame(log_ret_xts %*% head(t(portfolio_values),length(tick)))
# 
# port_prices <- port_ret %>% mutate_all(list(port_price = to_prices))
# 
# port_prices_mean <- summarize_all(port_prices,mean)
# 
# port_prices_last <- tail(port_prices,1)
# 
# # TO FIND PORTFOLIO WITH GREATEST LAST VALUE
# colnames(port_prices_last)[max.col(port_prices_last,ties.method="first")]
# 

test_portoflios <- dplyr::filter(portfolio_values,MGK>=0.10 & SPAB>=0.70)

# -> min-var with 50,000 portfolios looks good
# 0.04101116 0.1151937 0.7172913 0.01380145 0.03724624 0.07545614 0.03490042 0.07278385 0.4795078 

# MIN-VAR PORT USING SHRINKAGE:
# some_port <- data.frame('EWW'= 0.0,
#                         'MGK' = 0.00209,
#                         'SPAB'= 0.9112,
#                         'VB' = 0,
#                         'VGK'=0,
#                         'VV'=.08671)

# MIN-VAR PORT USING SHRINKAGE WITH L2 REGULARIZATION FOR DIVERSIFICATION:
some_port <- data.frame('EWW'= 0.08512,
                        'MGK' = 0.16784,
                        'SPAB'= 0.3276,
                        'VB' = 0.1297,
                        'VGK'=0.13254,
                        'VV'=0.1572)

p <- min_var %>%
  gather(-c("Return","Risk","SharpeRatio"), key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

p <- max_sr %>%
  gather(-c("Return","Risk","SharpeRatio"), key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)


### EFFICIENT FRONTIER
# p <- portfolio_values %>%
#   ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
#   geom_point() +
#   theme_classic() +
#   scale_y_continuous(labels = scales::percent) +
#   scale_x_continuous(labels = scales::percent) +
#   labs(x = 'Annualized Risk',
#        y = 'Annualized Returns',
#        title = "Portfolio Optimization & Efficient Frontier") +
#   geom_point(aes(x = Risk,
#                  y = Return), data = min_var, color = 'red') +
#   geom_point(aes(x = Risk,
#                  y = Return), data = max_sr, color = 'red')
#   # annotate('text', x = 0.20, y = 0.42, label = "Tangency Portfolio") +
#   # annotate('text', x = 0.18, y = 0.01, label = "Minimum variance portfolio") +
#   # annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01,
#   #          yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
#   # annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405,
#   #          yend = 0.365, color = 'red', arrow = arrow(type = "open"))
# 
# 
# ggplotly(p)


############ TESTING FOR NEW DATES ############

from <- as.character(Sys.Date()-360)
to <- as.character(Sys.Date())

test_data <- log_ret_xts_test[index(log_ret_xts_test) >= from &
      +                 index(log_ret_xts_test) < to, tick]

test_data_prices <- price_data_xts[index(price_data_xts) >= from &
                                +                 index(price_data_xts) < to, tick]

test_data_prices_norm <- data.frame(lapply(as.data.frame(test_data_prices[,1:length(tick)]), function(X) 100*X/X[1]))
test_data_prices_norm <- data.frame(date=index(test_data_prices), coredata(test_data_prices_norm))

test_port_ret_minvar <- test_data[,1:(length(tick))] %*% head(t(min_var),length(tick))

test_port_ret_maxsr <- test_data[,1:(length(tick))] %*% head(t(max_sr),length(tick))

test_ret_some_port <- test_data[,1:(length(tick))] %*% head(t(some_port),length(tick))

#### NORMALIZING TO TRACK EVOLUTION SHOULD BE DONE STARTING FROM TESTING DATE!!!!!!!
# to normalize divide all rows by first row and multiply by 100

basetest <- nrow(price_data_asdf)-nrow(test_data_prices)+1

price_data_norm <- data.frame(lapply(price_data_asdf[,2:length(tick)], function(X) 100*X/X[basetest]))
price_data_norm <- data.frame(date=index(price_data_xts), coredata(price_data_norm))
market_price_norm <- market_price_asdf %>% mutate(X.GSPC = 100*X.GSPC/X.GSPC[basetest])
#market_price_norm <- data.frame(date=index(market_price_xts), coredata(market_price_norm))

write_csv(price_data_asdf,'stock_prices2.csv')

test_evol <- data.frame(dates = index(test_data_prices),
                        port_tan = data.matrix(test_data_prices_norm[,2:(length(tick)+1)]) %*% head(t(max_sr),length(tick)),
                        #port_price_min = exp(cumsum(test_port_ret_minvar)) * 100,
                        #port_price_min = test_data_prices[,1:(length(tick))] %*% head(t(min_var),length(tick)),
                        port_price_min = data.matrix(test_data_prices_norm[,2:(length(tick)+1)]) %*% head(t(min_var),length(tick)),
                        #some_port = exp(cumsum(test_ret_some_port)) * 100,
                        some_port = data.matrix(test_data_prices_norm[,2:(length(tick)+1)]) %*% head(t(some_port),length(tick)),
                        market_price = tail(market_price_norm$X.GSPC,nrow(test_data_prices)))

#test_evol_diff <- test_evol %>% mutate(min_diff = port_price_min - market_price, port_diff = port_price - market_price)

#test_market_evol <- data.frame(dates = index(test_data), market_price = exp(cumsum( tail(log_market_prices_xts$`^GSPC`,nrow(test_maxsr_evol)) )) * 100)
test_evol_long <- test_evol %>% pivot_longer(-"dates")

### GRAPH EVOLUTION OF DIFFERENT PORTFOLIIOS VS MARKET (S&P 500)

ggplot(test_evol_long)+
  geom_line(aes(x = dates, y = value, color = name, group = name),linewidth = 0.7)+
  # geom_line(aes(x = dates, y = value, color = name, group = name))+
  # geom_line(aes(x = dates, y = value, color = name, group = name))+
  # geom_text(aes(x = tail(dates, 1), y = tail(value, 1), label = round(tail(value, 1), 2), group = tail(name,1)), 
  #           hjust = 1, vjust = -1)+
  theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(1.6,"cm"),legend.text = element_text(colour="black", size = 13),legend.title = element_blank())+
  #scale_color_manual(labels = c("Portfolio", "S&P"), values = c("blue", "red",'green'), name="") +
  theme_bw() +
  theme(legend.key.height=unit(1,"cm"),legend.key.width=unit(1,"cm"),legend.text = element_text(colour="black", size = 12),legend.title = element_blank())+
  theme(strip.text.x = element_text(size=13,face="bold"), strip.text.y = element_text(size=13,face="bold"))+
  theme(axis.text= element_text(size = 13), axis.title= element_text(size = 13),title= element_text(size = 14))+
  ggtitle("Portfolio vs Market")


