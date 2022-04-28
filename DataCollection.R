# Questions:
# 1) Which and how many cryptos should we include?
# 2) How far back should we go? (maybe start in 2018/2019)

# Load the necessary packages
pacman::p_load(tidyverse, tidyquant, TTR)

# Load the data using tidyquant
BTC = tq_get("BTC-USD", from = '2019-01-01')
ETH = tq_get("ETH-USD", from = '2019-01-01')
ADA = tq_get("ADA-USD", from = '2019-01-01')
DOGE = tq_get("DOGE-USD", from = '2019-01-01')
LTC = tq_get("LTC-USD", from = '2019-01-01')

# Exploratory visualizations
# BTC
BTC$ma7 = rollmeanr(BTC$adjusted, k = 7, fill = NA)
BTC$ma200 = rollmeanr(BTC$adjusted, k = 200, fill = NA)

BTC$RSI = RSI(BTC$adjusted)

ggplot() +
  geom_line(aes(x = date, y = adjusted), data = BTC) +
  geom_line(aes(x = date, y = ma7, color = "blue"), data = BTC) +
  geom_line(aes(x = date, y = ma200, color = "red"), data = BTC) +
  theme_tq() +
  labs(x = "Date",
       y = "Adjusted Price",
       title = "Bitcoin Adjusted Prices over Time") +
  scale_color_discrete(name = "Moving Average",
                      labels = c("7-Day", "200-Day")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_line(aes(x = date, y = RSI), data = BTC) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  theme_tq() +
  labs(x = "Date",
       y = "RSI",
       title = "Bitcoin Relative Strength Index over Time") +
  theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------
# ETH
ETH$ma7 = rollmeanr(ETH$adjusted, k = 7, fill = NA)
ETH$ma50 = rollmeanr(ETH$adjusted, k = 50, fill = NA)

ETH$RSI = RSI(ETH$adjusted)

ggplot() +
  geom_line(aes(x = date, y = adjusted), data = ETH) +
  geom_line(aes(x = date, y = ma7, color = "blue"), data = ETH) +
  geom_line(aes(x = date, y = ma50, color = "red"), data = ETH) +
  theme_tq() +
  labs(x = "Date",
       y = "Adjusted Price",
       title = "Ethereum Adjusted Prices over Time") +
  scale_color_discrete(name = "Moving Average",
                       labels = c("7-Day", "50-Day")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_line(aes(x = date, y = RSI), data = ETH) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  theme_tq() +
  labs(x = "Date",
       y = "RSI",
       title = "Ethereum Relative Strength Index over Time") +
  theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------
# ADA
ADA$ma7 = rollmeanr(ADA$adjusted, k = 7, fill = NA)
ADA$ma200 = rollmeanr(ADA$adjusted, k = 200, fill = NA)

ADA$RSI = RSI(ADA$adjusted)

ggplot() +
  geom_line(aes(x = date, y = adjusted), data = ADA) +
  geom_line(aes(x = date, y = ma7, color = "blue"), data = ADA) +
  geom_line(aes(x = date, y = ma200, color = "red"), data = ADA) +
  theme_tq() +
  labs(x = "Date",
       y = "Adjusted Price",
       title = "Cardano Adjusted Prices over Time") +
  scale_color_discrete(name = "Moving Average",
                       labels = c("7-Day", "200-Day")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_line(aes(x = date, y = RSI), data = ADA) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  theme_tq() +
  labs(x = "Date",
       y = "RSI",
       title = "Cardano Relative Strength Index over Time") +
  theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------
# DOGE
DOGE$ma7 = rollmeanr(DOGE$adjusted, k = 7, fill = NA)
DOGE$ma200 = rollmeanr(DOGE$adjusted, k = 200, fill = NA)

DOGE$RSI = RSI(DOGE$adjusted)

ggplot() +
  geom_line(aes(x = date, y = adjusted), data = DOGE) +
  geom_line(aes(x = date, y = ma7, color = "blue"), data = DOGE) +
  geom_line(aes(x = date, y = ma200, color = "red"), data = DOGE) +
  theme_tq() +
  labs(x = "Date",
       y = "Adjusted Price",
       title = "Dogecoin Adjusted Prices over Time") +
  scale_color_discrete(name = "Moving Average",
                       labels = c("7-Day", "200-Day")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_line(aes(x = date, y = RSI), data = DOGE) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  theme_tq() +
  labs(x = "Date",
       y = "RSI",
       title = "Dogecoin Relative Strength Index over Time") +
  theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------
# LTC
LTC$ma7 = rollmeanr(LTC$adjusted, k = 7, fill = NA)
LTC$ma200 = rollmeanr(LTC$adjusted, k = 200, fill = NA)

LTC$RSI = RSI(LTC$adjusted)

ggplot() +
  geom_line(aes(x = date, y = adjusted), data = LTC) +
  geom_line(aes(x = date, y = ma7, color = "blue"), data = LTC) +
  geom_line(aes(x = date, y = ma200, color = "red"), data = LTC) +
  theme_tq() +
  labs(x = "Date",
       y = "Adjusted Price",
       title = "Litecoin Adjusted Prices over Time") +
  scale_color_discrete(name = "Moving Average",
                       labels = c("7-Day", "200-Day")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_line(aes(x = date, y = RSI), data = LTC) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  theme_tq() +
  labs(x = "Date",
       y = "RSI",
       title = "Litecoin Relative Strength Index over Time") +
  theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------
# Write csv files
write.csv(BTC, "BTC.csv")
write.csv(ETH, "ETH.csv")
write.csv(ADA, "ADA.csv")
write.csv(DOGE, "DOGE.csv")
write.csv(LTC, "LTC.csv")

#---------------------------------------------------------
crypto = read.csv("Crypto_groupA1.csv")

crypto$asset_id = as.factor(crypto$asset_id)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

ggplot() +
  geom_line(aes(x = time, y = galaxy_score,
                group = asset_id, color = asset_id),
            data = crypto) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red") +
  theme_tq() +
  scale_colour_manual(values = cbPalette, name = "Crypto",
                      labels = c("BTC", "ETH", "LTC", "ADA", "DOGE")) +
  labs(x = "Day", y = "Galaxy Score",
       title = "Galaxy Score over Time by Crypto") +
  theme(plot.title = element_text(hjust = 0.5))
