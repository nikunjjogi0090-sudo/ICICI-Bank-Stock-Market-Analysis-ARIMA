# 📊 ICICI Bank Stock Market Data Analysis using ARIMA Model

A complete financial data analysis and forecasting project on **ICICI Bank Stock Market Data** using **R Programming** and the **ARIMA Time Series Forecasting Model**.

This project covers:
- Financial Data Acquisition
- Data Cleaning & Preprocessing
- Data Visualization
- Technical Indicators
- Time Series Analysis
- ARIMA Forecasting
- Residual Diagnostics
- Stock Market Trend Analysis

---

## 🚀 Project Overview

This project analyzes **ICICI Bank (ICICIBANK.NS)** stock market performance using historical data from Yahoo Finance.

The analysis includes:
- Price trend analysis
- Volume analysis
- Volatility analysis
- Technical indicators
- Seasonal decomposition
- Stationarity testing
- ARIMA forecasting for future stock prices

The project is developed entirely in **R Programming** using financial and visualization libraries.

---

## 📂 Project File

### `ICICI_STOCK_MARKET_DATA_ANALYSIS_USE_OF_ARIMA_MODEL.R`

This R script contains:
- Stock data extraction from Yahoo Finance
- Data preprocessing and cleaning
- Financial data visualization
- Moving average calculations
- Bollinger Bands analysis
- Time series decomposition
- ADF stationarity testing
- ACF & PACF analysis
- ARIMA model forecasting
- Residual diagnostics

---

## 📈 Features

### ✅ Financial Data Acquisition
- Download historical stock data
- Save data as CSV & Excel
- Read and process datasets

### ✅ Data Cleaning
- Handle missing values
- Remove duplicate records
- Generate derived financial metrics

### ✅ Technical Indicators
- Daily Returns
- Log Returns
- Price Range
- SMA 50
- SMA 200
- Bollinger Bands

### ✅ Data Visualization
- Closing Price Line Chart
- Monthly Volume Bar Plot
- OHLC Candlestick Chart
- Returns Distribution Histogram
- Yearly Average Price Chart
- Moving Average Visualization
- Bollinger Bands Visualization

### ✅ Time Series Analysis
- Trend Analysis
- Seasonal Decomposition
- Stationarity Testing
- ACF & PACF Analysis

### ✅ ARIMA Forecasting
- Automatic ARIMA model selection
- 12-Month Stock Price Forecast
- Forecast Confidence Intervals
- Residual Diagnostics

---

## 🛠️ Technologies Used

- R Programming
- quantmod
- ggplot2
- tidyverse
- forecast
- tseries
- TTR
- PerformanceAnalytics
- xts
- zoo
- writexl
- readxl

---

## 📦 Installation

Install required packages in R:

```r
install.packages(c(
  "quantmod",
  "tidyverse",
  "ggplot2",
  "dplyr",
  "lubridate",
  "tseries",
  "forecast",
  "TTR",
  "PerformanceAnalytics",
  "xts",
  "zoo",
  "writexl",
  "readxl",
  "scales"
))
