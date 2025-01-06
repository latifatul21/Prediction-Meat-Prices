library (forecast)  #BoxCox Arima auto.arima function is in forecast package
library (MASS)      #boxcox function is in MASS package
#library (FitAR)     #LjungBoxTest function is in FitAR package
library (tsoutliers) #tso function is in tsoutliers package
library (lmtest)    #coeftest function is in lmtest package
library (stargazer) #stargazer function is in stargazer package
library (TSA)       #arimax function is in TSA package
library(tseries)
library(readxl)
setwd("C:/Users/Latifatul Khumairoh/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 7/ADS/Tugas Bu Irhamah")
df <- read_xlsx("harga daging Jakarta.xlsx")
df_ts <- ts(df$Daging)
df_ts

#Time Series Plot
plot(df_ts, xlab="Waktu", ylab="IDR", col="blue", type="l",
     main="Plot Harga Daging Jakarta")
abline(v=67, col=1, lty=2)


####PEMODELAN ARIMA PRE INTERVENSI######
# Pisahkan data pre-intervensi dari baris 1 hingga 66
df_pre <- df_ts[1:66]
# Plot time series untuk pre-intervensi
plot(df_pre, type="l", col="blue", xlab="Tanggal", ylab="Harga Daging", 
     main="Plot Harga Daging Jakarta (Pre-Intervensi)")

#Estimasi model
arima210<-arima(df_pre,  order=c(2,1,0), include.mean = TRUE,method="ML") 
coeftest(arima210)

#Diagnosis Model ARIMA
#Ljung-Box Test for Nt  #Sudah white noise
Box.test(arima210$residuals, lag=round(length(df_pre)/5,0),
         type = "Ljung-Box", fitdf = 1)

#Kolmogorov-Smirnov Test  #Sudah berdistribusi normal
ks.test(arima210$residuals, "pnorm",
        mean(arima210$residuals),
        sd(arima210$residuals))

# Forecast dari model ARIMA
forecast_arima210 <- forecast(df_pre,model=arima210, h=66)

# Hitung MAPE
mape_arima210 <- mean(abs((df_pre - forecast_arima210$mean) / df_pre)) * 100

# Tampilkan MAPE
print(paste("MAPE untuk ARIMA (2,1,0):", mape_arima210))
print('Ditemukan bahwa 210 model terbaik')

#Analisis Intervensi
#Identifikasi Orde Interval
pre_forecast <-forecast(df_pre,model=arima210, h = 21)
pre_forecast
plot(pre_forecast)

#Identification intervention order with plot of model residuals
error_idintv <-rep(0,86)
error_idintv[1:65] <- arima210$residuals
error_idintv[66:length(df_ts)] <- df_ts[66:length(df_ts)] - pre_forecast$mean
plot(error_idintv, type="h", xlab="Waktu (T)", ylab = "Residual", xaxt = "n")
abline(h=c(-3*sd(arima210$residuals), 3*sd(arima210$residuals)),col="blue", lty=2)
abline(v = 67, col = "red", lty = 3, lwd = 1.5)
text(67, 200, "T=67",cex = .8, pos = 2)
axis(1, at=c(0,10,20,30,40,50,60,70,80,90,100), 
     labels=c("T-60","T-50","T-40","T-30","T-20" ,"T-10", "T", "T+10", "T+20","T+30", "T+40"))
error_idintv
# orde dugaannya b=0 karena intervensi terjadi di titik T, s=1 karena juml intervensinya cuma 1, r=1/2 karena plot residualnya membentuk pola eksponensial

##MEMODELKAN ARIMA DENGAN INTERVENSI
# Waktu intervensi
T <- 67  # Intervensi terjadi pada pengamatan ke-67

# model (b=0, r=0, s=1)
intervensir0s1 <- ifelse(seq_along(df_ts) >= T, (seq_along(df_ts) - T + 1)^1, 0)

# Gabungkan model ARIMA dengan variabel intervensi
model_arima_intervensi_r0s1 <- Arima(df_ts, order = c(2, 1, 0), xreg = intervensir0s1)

# Diagnostik model
coeftest(model_arima_intervensi_r0s1)

# Plot residual model
aic_arima_intervensir0s1 <- AIC(model_arima_intervensi_r0s1);aic_arima_intervensir0s1

# model (b=0, r=0, s=2)
intervensir0s2 <- ifelse(seq_along(df_ts) >= T, (seq_along(df_ts) - T + 1)^2, 0)

# Gabungkan model ARIMA dengan variabel intervensi
model_arima_intervensi_r0s2 <- Arima(df_ts, order = c(2, 1, 0), xreg = intervensir0s2)

# Diagnostik model
coeftest(model_arima_intervensi_r0s2)

# Plot residual model
aic_arima_intervensir0s2 <- AIC(model_arima_intervensi_r0s2);aic_arima_intervensir0s2

# model (b=0, r=0, s=3)
intervensir0s3 <- ifelse(seq_along(df_ts) >= T, (seq_along(df_ts) - T + 1)^3, 0)

# Gabungkan model ARIMA dengan variabel intervensi
model_arima_intervensi_r0s3 <- Arima(df_ts, order = c(2, 1, 0), xreg = intervensir0s3)

# Diagnostik model
coeftest(model_arima_intervensi_r0s3)

# Plot residual model
aic_arima_intervensir0s3 <- AIC(model_arima_intervensi_r0s3);aic_arima_intervensir0s3

# model (b=0, r=0, s=4)
intervensir0s4 <- ifelse(seq_along(df_ts) >= T, (seq_along(df_ts) - T + 1)^4, 0)

# Gabungkan model ARIMA dengan variabel intervensi
model_arima_intervensi_r0s4 <- Arima(df_ts, order = c(2, 1, 0), xreg = intervensir0s4)

# Diagnostik model
coeftest(model_arima_intervensi_r0s4)

# Plot residual model
aic_arima_intervensir0s4 <- AIC(model_arima_intervensi_r0s4);aic_arima_intervensir0s4

# model (b=0, r=0, s=5)
intervensir0s5 <- ifelse(seq_along(df_ts) >= T, (seq_along(df_ts) - T + 1)^5, 0)

# Gabungkan model ARIMA dengan variabel intervensi
model_arima_intervensi_r0s5 <- Arima(df_ts, order = c(2, 1, 0), xreg = intervensir0s5)

# Diagnostik model
coeftest(model_arima_intervensi_r0s5)

# Plot residual model
aic_arima_intervensir0s5 <- AIC(model_arima_intervensi_r0s5);aic_arima_intervensir0s5

#Ljung-Box Test for Nt  #Sudah white noise
Box.test(model_arima_intervensi_r0s1$residuals, lag=round(length(df_ts)/5,0),
         type = "Ljung-Box", fitdf = 1)

ks.test(model_arima_intervensi_r0s1$residuals, "pnorm",
        mean(model_arima_intervensi_r0s1$residuals),
        sd(model_arima_intervensi_r0s1$residuals))

#Future Value of xreg 
xreg_ar = forecast(auto.arima(intervensir0s1), h=5)$mean
#Forecasting 
fore <- forecast(model_arima_intervensi_r0s1, xreg = xreg_ar)
fore


dev.new()  # Open a new graphics window
plot(forecast(model_arima_intervensi_r0s1, xreg = xreg_ar), main = NA)

# Plot the actual data
plot(df_ts, xlab = "Waktu", ylab = "IDR", col = "blue", type = "l", 
     main = "Plot Harga Daging Jakarta", ylim = range(c(df_ts, 
                                                        forecast(model_arima_intervensi_r0s1, xreg = xreg_ar)$mean)))

# Add the fitted values from the ARIMA model
lines(fitted(model_arima_intervensi_r0s1), col = "red", lwd = 2)  # Add fitted values in red

# Forecasting values
h <- 10  # Set the forecasting horizon
forecasted_values <- forecast(model_arima_intervensi_r0s1, xreg = xreg_ar)

# Add forecasted values (in green) with a dashed line
lines(forecasted_values$mean, col = "green", lwd = 2, lty = 2)  

# Optionally, add a legend to identify the lines
legend("topleft", legend = c("Actual Data", "Fitted Values", "Forecast"),
       col = c("blue", "red", "green"), lty = c(1, 1, 2), lwd = 2)
forecasted_values

# Ambil fitted values dari model
fitted_values <- fitted(model_arima_intervensi_r0s1)

# Konversi ke data frame
fitted_df <- data.frame(Fitted_Values = as.numeric(fitted_values))

# Tampilkan data frame
print(fitted_df)
actual_values=df$Daging
predicted_values=fitted_df$Fitted_Values
mape <- mean(abs((actual_values - predicted_values) / actual_values), na.rm = TRUE) * 100
print(mape)

library(writexl)

# Save the data frame to an Excel file
write_xlsx(fitted_df, "fitted_values.xlsx")
