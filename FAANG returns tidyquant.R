#Quantmod data viz

library(quantmod)
library(tidyquant)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)


df_curr<-tq_get("USD/EUR", get = "alphavantage", av_fun = "FX_DAILY", outputsize = "full")
timestamp<-df_curr$timestamp
closeprice<-df_curr$close

plot(x=timestamp,y=1/closeprice,ylab='EUR/USD')


##get FAANG stock prices 2020
symbols<-c('FB','AMZN','AAPL','NFLX','GOOGL')

df_faang<-
    tq_get(symbols, get = "stock.prices", from = "2020-01-01")

#FAANG monthly returns
FAANG_returns_monthly <- df_faang %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "monthly.returns")

FAANG_returns_monthly %>%
  ggplot(aes(x = month(date), y = monthly.returns, fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "FAANG: Monthly Returns 2020",
       subtitle = "",
       y = "Returns", x = "Month", color = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
       scale_fill_tq() +
       theme_tq()



#Microsoft linear regression by month 8-2018 to 6-2020
df_mfst<-tq_get('MSFT',get='stock.prices',from = '2018-08-01')

mfst_returns_monthly <- df_mfst %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "monthly.returns")


mfst_returns_monthly %>% 
  ggplot(aes(x=date, y=monthly.returns, fill=symbol)) +
  geom_point(size = 2,show.legend = FALSE) +
  geom_line(size = 1, color = palette_dark()[[2]]) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_smooth(method = "lm",show.legend = FALSE,se=TRUE) +
  labs(title = "MSFT: Monthly Return Fit with Smooth Linear Model",
       x = "Year", y = "Monthly Returns", color = "")

lmod <- lm(monthly.returns ~ 0 + month(date),mfst_returns_monthly)
lmod

modelfit <- auto.arima(df_mfst$adjusted, lambda='auto')
summary(modelfit)
mfst_forecast <- forecast(modelfit, h=30)
plot(forecast(modelfit, h=30),ylab='Stock Price',xlab = 'Periods')
