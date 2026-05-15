
# ============================================================================
# ICICI BANK - COMPLETE FINANCIAL ANALYSIS PROJECT
# Part 1: Data Acquisition, Visualization, Time Series Analysis
# ============================================================================

# ---- Install & Load Packages ----
packages <- c("quantmod","tidyverse","ggplot2","dplyr","lubridate",
              "tseries","forecast","TTR","PerformanceAnalytics",
              "xts","zoo","writexl","readxl","scales")

for(pkg in packages){
  if(!require(pkg, character.only=TRUE)){
    install.packages(pkg, dependencies=TRUE)
    library(pkg, character.only=TRUE)
  }
}

# ============================================================================
# PART 1A: FINANCIAL DATA ACQUISITION & HANDLING
# ============================================================================

cat("\n========== PART 1A: DATA ACQUISITION ==========\n")

# --- 1. Download ICICI Bank data from Yahoo Finance API (last 10 years) ---
start_date <- Sys.Date() - (10*365)
end_date   <- Sys.Date()

getSymbols("ICICIBANK.NS", src="yahoo", from=start_date, to=end_date, auto.assign=TRUE)
icici_xts <- ICICIBANK.NS
cat("Data downloaded from Yahoo Finance API\n")
cat("Date range:", as.character(start(icici_xts)), "to", as.character(end(icici_xts)), "\n")
cat("Total observations:", nrow(icici_xts), "\n")

# --- 2. Convert to data frame for easy handling ---
icici_df <- data.frame(Date=index(icici_xts), coredata(icici_xts))
colnames(icici_df) <- c("Date","Open","High","Low","Close","Volume","Adjusted")

# --- 3. Save as CSV ---
write.csv(icici_df, "ICICI_Bank_Data.csv", row.names=FALSE)
cat("Saved as CSV: ICICI_Bank_Data.csv\n")

# --- 4. Save as Excel ---
writexl::write_xlsx(icici_df, "ICICI_Bank_Data.xlsx")
cat("Saved as Excel: ICICI_Bank_Data.xlsx\n")

# --- 5. Read back from CSV ---
icici_csv <- read.csv("ICICI_Bank_Data.csv")
icici_csv$Date <- as.Date(icici_csv$Date)
cat("Read back from CSV: ", nrow(icici_csv), " rows\n")

# --- 6. Read back from Excel ---
icici_excel <- readxl::read_xlsx("ICICI_Bank_Data.xlsx")
icici_excel$Date <- as.Date(icici_excel$Date)
cat("Read back from Excel: ", nrow(icici_excel), " rows\n")

# --- 7. Data Cleaning ---
cat("\n--- Data Cleaning ---\n")
cat("Missing values per column:\n")
print(colSums(is.na(icici_df)))

# Remove rows with NA
icici_clean <- na.omit(icici_df)
cat("Rows after cleaning:", nrow(icici_clean), "\n")

# Check for duplicates
cat("Duplicate rows:", sum(duplicated(icici_clean$Date)), "\n")
icici_clean <- icici_clean[!duplicated(icici_clean$Date),]

# Add derived columns
icici_clean <- icici_clean %>%
  mutate(
    Daily_Return = (Close - lag(Close)) / lag(Close) * 100,
    Log_Return   = log(Close / lag(Close)) * 100,
    Price_Range  = High - Low,
    Year         = year(Date),
    Month        = month(Date, label=TRUE),
    Day          = wday(Date, label=TRUE)
  )

# Summary statistics
cat("\n--- Summary Statistics ---\n")
print(summary(icici_clean[,c("Open","High","Low","Close","Volume","Daily_Return")]))

# ============================================================================
# PART 1B: DATA VISUALIZATION (ggplot2)
# ============================================================================

cat("\n========== PART 1B: DATA VISUALIZATION ==========\n")

# --- Plot 1: Closing Price Line Chart ---
p1 <- ggplot(icici_clean, aes(x=Date, y=Close)) +
  geom_line(color="#1E88E5", linewidth=0.5) +
  labs(title="ICICI Bank - Closing Price (10 Years)",
       x="Date", y="Price (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p1)
ggsave("01_closing_price.png", p1, width=12, height=6, dpi=150)
cat("Saved: 01_closing_price.png\n")
cat("\n>> EXPLANATION (Plot 1 - Closing Price Line Chart):\n")
cat("   This line chart shows the daily closing price of ICICI Bank over the last 10 years.\n")
cat("   It helps visualize the overall long-term price trend - uptrend, downtrend, or sideways.\n")
cat("   Steep slopes indicate strong growth, sharp drops indicate corrections or crashes.\n\n")

# --- Plot 2: Volume Bar Plot ---
icici_monthly_vol <- icici_clean %>%
  mutate(YM=floor_date(Date,"month")) %>%
  group_by(YM) %>%
  summarise(Avg_Volume=mean(Volume, na.rm=TRUE))

p2 <- ggplot(icici_monthly_vol, aes(x=YM, y=Avg_Volume)) +
  geom_bar(stat="identity", fill="#43A047", alpha=0.7) +
  labs(title="ICICI Bank - Monthly Average Trading Volume",
       x="Date", y="Volume") +
  scale_y_continuous(labels=comma) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p2)
ggsave("02_volume_barplot.png", p2, width=12, height=6, dpi=150)
cat("Saved: 02_volume_barplot.png\n")
cat("\n>> EXPLANATION (Plot 2 - Monthly Average Volume Bar Plot):\n")
cat("   High volume bars indicate strong market interest during news or earnings.\n")
cat("   Volume confirms price trends - a price rise with high volume is more reliable.\n\n")

# --- Plot 3: Candlestick-style OHLC Chart (last 60 days) ---
last60 <- tail(icici_clean, 60)
last60$Color <- ifelse(last60$Close >= last60$Open, "green", "red")

p3 <- ggplot(last60) +
  geom_segment(aes(x=Date, xend=Date, y=Low, yend=High, color=Color), linewidth=0.4) +
  geom_segment(aes(x=Date, xend=Date, y=Open, yend=Close, color=Color), linewidth=2) +
  scale_color_identity() +
  labs(title="ICICI Bank - OHLC Chart (Last 60 Trading Days)",
       x="Date", y="Price (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p3)
ggsave("03_ohlc_chart.png", p3, width=12, height=6, dpi=150)
cat("Saved: 03_ohlc_chart.png\n")
cat("\n>> EXPLANATION (Plot 3 - OHLC Candlestick Chart):\n")
cat("   GREEN = bullish days (Close > Open). RED = bearish days (Close < Open).\n")
cat("   Thin wicks show High-Low range; thick body shows Open-Close range.\n\n")

# --- Plot 4: Daily Returns Distribution ---
p4 <- ggplot(icici_clean, aes(x=Daily_Return)) +
  geom_histogram(bins=80, fill="#7B1FA2", alpha=0.7, color="white") +
  geom_vline(xintercept=0, linetype="dashed", color="red") +
  labs(title="ICICI Bank - Daily Returns Distribution",
       x="Daily Return (%)", y="Frequency") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p4)
ggsave("04_returns_distribution.png", p4, width=10, height=6, dpi=150)
cat("Saved: 04_returns_distribution.png\n")
cat("\n>> EXPLANATION (Plot 4 - Daily Returns Distribution):\n")
cat("   Bell-shaped distribution centered near 0 with fat tails indicates occasional large moves.\n")
cat("   Wider distribution = higher volatility and risk.\n\n")

# --- Plot 5: Yearly Average Close Price ---
yearly_avg <- icici_clean %>% group_by(Year) %>%
  summarise(Avg_Close=mean(Close, na.rm=TRUE))

p5 <- ggplot(yearly_avg, aes(x=factor(Year), y=Avg_Close)) +
  geom_bar(stat="identity", fill="#FF7043", alpha=0.8) +
  geom_text(aes(label=round(Avg_Close,0)), vjust=-0.5, size=3) +
  labs(title="ICICI Bank - Yearly Average Closing Price",
       x="Year", y="Avg Close (INR)") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p5)
ggsave("05_yearly_avg_price.png", p5, width=10, height=6, dpi=150)
cat("Saved: 05_yearly_avg_price.png\n")
cat("\n>> EXPLANATION (Plot 5 - Yearly Average Closing Price):\n")
cat("   Rising bars show consistent growth. Dips highlight poor years (e.g., COVID-19).\n\n")

# --- Plot 6: Moving Averages ---
icici_clean <- icici_clean %>%
  arrange(Date) %>%
  mutate(
    SMA_50  = SMA(Close, n=50),
    SMA_200 = SMA(Close, n=200)
  )

p6 <- ggplot(icici_clean, aes(x=Date)) +
  geom_line(aes(y=Close, color="Close"), linewidth=0.4) +
  geom_line(aes(y=SMA_50, color="SMA 50"), linewidth=0.6) +
  geom_line(aes(y=SMA_200, color="SMA 200"), linewidth=0.6) +
  scale_color_manual(values=c("Close"="grey50","SMA 50"="#1E88E5","SMA 200"="#E53935")) +
  labs(title="ICICI Bank - Price with 50 & 200 Day Moving Averages",
       x="Date", y="Price (INR)", color="Legend") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p6)
ggsave("06_moving_averages.png", p6, width=12, height=6, dpi=150)
cat("Saved: 06_moving_averages.png\n")
cat("\n>> EXPLANATION (Plot 6 - Moving Averages):\n")
cat("   GOLDEN CROSS: SMA-50 above SMA-200 = bullish. DEATH CROSS: opposite = bearish.\n\n")

# --- Plot 7: Bollinger Bands (last 1 year) ---
icici_1yr <- tail(icici_clean, 252)
bb <- BBands(icici_1yr$Close, n=20, sd=2)
icici_1yr$BB_Up   <- bb[,"up"]
icici_1yr$BB_Mid  <- bb[,"mavg"]
icici_1yr$BB_Down <- bb[,"dn"]

p7 <- ggplot(icici_1yr, aes(x=Date)) +
  geom_ribbon(aes(ymin=BB_Down, ymax=BB_Up), fill="#90CAF9", alpha=0.3) +
  geom_line(aes(y=Close, color="Close"), linewidth=0.5) +
  geom_line(aes(y=BB_Mid, color="Middle Band"), linetype="dashed") +
  scale_color_manual(values=c("Close"="black","Middle Band"="blue")) +
  labs(title="ICICI Bank - Bollinger Bands (Last 1 Year)",
       x="Date", y="Price (INR)", color="") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14))
print(p7)
ggsave("07_bollinger_bands.png", p7, width=12, height=6, dpi=150)
cat("Saved: 07_bollinger_bands.png\n")
cat("\n>> EXPLANATION (Plot 7 - Bollinger Bands):\n")
cat("   Upper band = overbought, Lower band = oversold. Narrow bands = squeeze before big move.\n\n")

# ============================================================================
# PART 1C: BASIC TIME SERIES ANALYSIS
# ============================================================================

cat("\n========== PART 1C: TIME SERIES ANALYSIS ==========\n")

# --- Create monthly time series ---
monthly_data <- icici_clean %>%
  mutate(YM=floor_date(Date, "month")) %>%
  group_by(YM) %>%
  summarise(Avg_Close=mean(Close, na.rm=TRUE)) %>%
  arrange(YM)

ts_monthly <- ts(monthly_data$Avg_Close,
                 start=c(year(min(monthly_data$YM)), month(min(monthly_data$YM))),
                 frequency=12)

# --- Trend Analysis ---
cat("\n--- Trend Analysis ---\n")
png("08_trend_analysis.png", width=1200, height=600, res=150)
plot(ts_monthly, main="ICICI Bank - Monthly Avg Close Price Trend",
     ylab="Price (INR)", xlab="Year", col="blue", lwd=1.5)
trend_line <- lm(ts_monthly ~ time(ts_monthly))
abline(trend_line, col="red", lwd=2, lty=2)
legend("topleft", c("Price","Trend"), col=c("blue","red"), lty=c(1,2), lwd=2)
dev.off()
# Display in RStudio
plot(ts_monthly, main="ICICI Bank - Monthly Avg Close Price Trend",
     ylab="Price (INR)", xlab="Year", col="blue", lwd=1.5)
abline(trend_line, col="red", lwd=2, lty=2)
legend("topleft", c("Price","Trend"), col=c("blue","red"), lty=c(1,2), lwd=2)
cat("Saved: 08_trend_analysis.png\n")
cat("Trend slope:", round(coef(trend_line)[2], 2), "per year\n")
cat("\n>> EXPLANATION (Plot 8 - Trend Analysis):\n")
cat("   Red trend line shows average price increase per year. Positive slope = upward trajectory.\n")
cat("   Deviations show overvalued (above) or undervalued (below) periods.\n\n")

# --- Seasonal Decomposition ---
cat("\n--- Seasonal Decomposition ---\n")
decomp <- decompose(ts_monthly, type="multiplicative")

png("09_seasonal_decomposition.png", width=1200, height=800, res=150)
plot(decomp, col="blue")
title(main="ICICI Bank - Seasonal Decomposition (Multiplicative)")
dev.off()
plot(decomp, col="blue")
title(main="ICICI Bank - Seasonal Decomposition (Multiplicative)")
cat("Saved: 09_seasonal_decomposition.png\n")
cat("\n>> EXPLANATION (Plot 9 - Seasonal Decomposition):\n")
cat("   Splits series into: Observed, Trend, Seasonal, and Random components.\n\n")

# --- Stationarity Tests ---
cat("\n--- Stationarity Tests ---\n")
adf_result <- adf.test(na.omit(ts_monthly))
cat("ADF Test p-value:", adf_result$p.value, "\n")
cat("Stationary?", ifelse(adf_result$p.value < 0.05, "Yes", "No - differencing needed"), "\n")

ts_diff <- diff(ts_monthly)
adf_diff <- adf.test(na.omit(ts_diff))
cat("ADF after differencing p-value:", adf_diff$p.value, "\n")

# --- ACF and PACF ---
png("10_acf_pacf.png", width=1200, height=600, res=150)
par(mfrow=c(1,2))
acf(na.omit(ts_diff), main="ACF - Differenced Series", lag.max=36)
pacf(na.omit(ts_diff), main="PACF - Differenced Series", lag.max=36)
dev.off()
par(mfrow=c(1,2))
acf(na.omit(ts_diff), main="ACF - Differenced Series", lag.max=36)
pacf(na.omit(ts_diff), main="PACF - Differenced Series", lag.max=36)
par(mfrow=c(1,1))
cat("Saved: 10_acf_pacf.png\n")
cat("\n>> EXPLANATION (Plot 10 - ACF and PACF):\n")
cat("   ACF: correlation with past values. PACF: partial correlation at each lag.\n")
cat("   Used to determine ARIMA order (p, q). Bars beyond blue lines are significant.\n\n")

# --- ARIMA Model ---
cat("\n--- ARIMA Modeling ---\n")
arima_auto <- auto.arima(ts_monthly, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
cat("Best ARIMA model:\n")
print(summary(arima_auto))

fc <- forecast(arima_auto, h=12)

png("11_arima_forecast.png", width=1200, height=600, res=150)
plot(fc, main="ICICI Bank - ARIMA Forecast (Next 12 Months)",
     xlab="Year", ylab="Price (INR)", col="blue", lwd=1.5)
dev.off()
plot(fc, main="ICICI Bank - ARIMA Forecast (Next 12 Months)",
     xlab="Year", ylab="Price (INR)", col="blue", lwd=1.5)
cat("Saved: 11_arima_forecast.png\n")
cat("\n>> EXPLANATION (Plot 11 - ARIMA Forecast):\n")
cat("   Dark shaded = 80% CI, Light shaded = 95% CI. Widening = increasing uncertainty.\n\n")

# Residual diagnostics
png("12_residual_diagnostics.png", width=1200, height=800, res=150)
checkresiduals(arima_auto)
dev.off()
checkresiduals(arima_auto)
cat("Saved: 12_residual_diagnostics.png\n")
cat("\n>> EXPLANATION (Plot 12 - Residual Diagnostics):\n")
cat("   Good model: random residuals, no ACF spikes, normal histogram.\n\n")

cat("\n========== PART 1 COMPLETED ==========\n")
cat("Now run ICICI_Bank_Analysis_Part2.R for Algorithmic Trading\n")
