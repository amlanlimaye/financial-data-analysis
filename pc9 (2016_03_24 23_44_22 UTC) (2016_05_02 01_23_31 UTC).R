stocks_data <- read.csv("BAC.txt")

colnames(stocks_data) <- c("date", "time", 
                           "open", "high", "low", "close", "number.of.shares")

# Opening stock prices
open_data <- dcast(stocks_data, formula = time~date, value.var = c("open"))

# Closing stock prices
close_data <- dcast(stocks_data, formula = time~date, value.var = c("close"))

# Volume of shares
volume_data <- dcast(stocks_data, formula = time~date, value.var = c("number.of.shares"))

# One minute returns
required_data_1 <- ((close_data - open_data) / open_data) * volume_data
required_data_1$time <- open_data$time

# replacing all NAs with 0
required_data_1[is.na(required_data_1)] <- 0

# Cumulative intraday returns
required_data_2 <- apply(X = required_data_1, MARGIN = 2, FUN = cumsum)
required_data_2 <- data.frame(required_data_2)
required_data_2$time <- open_data$time

# Think of other visualizations for data
# Do everything above for S&P Data

# Converting date and time to proper data classes
stocks_data$date <- as.Date(stocks_data$date, "%m/%d/%Y")
stocks_data$time <- as.ts(stocks_data$time)

# Plot opening and closing prices for each day
# Splitting by date and getting opening time for each day
open_price_for_each_date <- ddply(stocks_data, .(date), summarize, min(time), 
                                  .progress = "text")

# Doing a join with stocks data to get the respective opening price for each date
open_price_for_each_date <- merge(open_price_for_each_date, stocks_data, all.x = TRUE,
                                  by.x = c("date", "..1"),
                                  by.y = c("date", "time"))

open_price_for_each_date <- open_price_for_each_date[,-c(4:7)]

# Doing the same as above for closing time
close_price_for_each_date <- ddply(stocks_data, .(date), summarize, max(time), 
                                   .progress = "text")

close_price_for_each_date <- merge(close_price_for_each_date, stocks_data, all.x = TRUE,
                                  by.x = c("date", "..1"),
                                  by.y = c("date", "time"))

close_price_for_each_date <- close_price_for_each_date[,-c(3,4,5,7)]

# Lets see the difference between the stock prices on 
# 9th April 1997 (the first day of data collection) and 
# 2nd April 2007 (the last day of data collection)
        
plot(stocks_data$date, stocks_data$open, type="l")

sp_data <- read.csv("SP.TXT")
