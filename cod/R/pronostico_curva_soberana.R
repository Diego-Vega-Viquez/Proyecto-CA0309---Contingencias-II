library(readxl)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(car)
library(stats)
library(dplyr)
library(forecast)
library(tseries)

soberana <- read_excel('../../data/curva_soberana.xlsx', sheet = 'curva')
colnames(soberana) <- c('dias', 'tasa', 'fecha')

# Soberana mensual (Promedio Mensual)

soberana_promedio_mes <- soberana %>%
  mutate(anio_mes = floor_date(fecha, "month")) %>%
  group_by(anio_mes) %>%
  summarise(tasa = mean(tasa, na.rm = TRUE))

view(soberana_promedio_mes)

ts_soberana_promedio_mes <- ts(soberana_promedio_mes$tasa, start = c(2025, 1), frequency = 12)

autoplot(ts_soberana_promedio_mes) + 
  labs(title = "Serie temporal promedio mensual de tasas soberanas",
       x = "Fecha", y = "Tasa")

## Estacionariedad

### Prueba ADF
adf.test(ts_soberana_promedio_mes)

## ARIMA

modelo_arima_1 <- auto.arima(ts_soberana_promedio_mes, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(modelo_arima_1)

checkresiduals(modelo_arima_1)

forecast_arima <- forecast(modelo_arima_1, h = 50 * 12)

autoplot(forecast_arima) + 
  labs(title = "Pronóstico ARIMA de Tasas Soberanas a 50 Años (Mensual)",
       x = "Fecha", y = "Tasa proyectada")

## SARIMA

modelo_sarima_1 <- auto.arima(ts_soberana_promedio_mes, seasonal = TRUE)
summary(modelo_sarima_1)
checkresiduals(modelo_sarima_1)

forecast_sarima_1 <- forecast(modelo_sarima_1, h = 50 * 12)

autoplot(forecast_sarima_1) +
  labs(title = "Pronóstico SARIMA de Tasas Soberanas a 50 Años (Mensual)",
       x = "Fecha", y = "Tasa proyectada")

## ETS

modelo_ets_1 <- ets(ts_soberana_promedio_mes)
summary(modelo_ets_1)
checkresiduals(modelo_ets_1)

forecast_ets_1 <- forecast(modelo_ets_1, h = 50 * 12)

autoplot(forecast_ets_1) + 
  labs(title = "Pronóstico ETS de Tasas Soberanas a 50 Años",
       x = "Fecha", y = "Tasa proyectada")


# Soberana mensual (Último Valor del Mes)

soberana_final_mes <- soberana %>%
  mutate(anio_mes = ceiling_date(fecha, "month")) %>% 
  group_by(anio_mes) %>%
  summarise(tasa = last(tasa))

#view(soberana_final_mes)

ts_soberana_final_mes <- ts(soberana_final_mes$tasa, start = c(2025, 1), frequency = 12)

autoplot(ts_soberana_final_mes) + 
  labs(title = "Serie temporal final de cada mes de tasas soberanas",
       x = "Fecha", y = "Tasa")

### Prueba ADF
adf.test(ts_soberana_final_mes)

## ARIMA

modelo_arima_2 <- auto.arima(ts_soberana_final_mes, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(modelo_arima_2)

checkresiduals(modelo_arima_2)

forecast_arima_2 <- forecast(modelo_arima_2, h = 50 * 12)

autoplot(forecast_arima_2) + 
  labs(title = "Pronóstico ARIMA de Tasas Soberanas a 50 Años (Mensual)",
       x = "Fecha", y = "Tasa proyectada")

## SARIMA

modelo_sarima_2<- auto.arima(ts_soberana_final_mes, seasonal = TRUE, D = 1, max.P = 2, max.Q = 2)

summary(modelo_sarima_2)
checkresiduals(modelo_sarima_2)

forecast_sarima_2 <- forecast(modelo_sarima_2, h = 50 * 12)

autoplot(forecast_sarima_2) +
  labs(title = "Pronóstico SARIMA de Tasas Soberanas a 50 Años (Mensual)",
       x = "Fecha", y = "Tasa proyectada")

## ETS

modelo_ets_2 <- ets(ts_soberana_final_mes)
summary(modelo_ets_2)
checkresiduals(modelo_ets_2)

forecast_ets_2 <- forecast(modelo_ets_2, h = 50 * 12)

autoplot(forecast_ets_2) + 
  labs(title = "Pronóstico ETS de Tasas Soberanas a 50 Años",
       x = "Fecha", y = "Tasa proyectada")

ts_soberana_log <- diff(log(ts_soberana_final_mes))
modelo_sarima_log <- auto.arima(ts_soberana_log, seasonal = TRUE)
summary(modelo_sarima_log)
checkresiduals(modelo_sarima_log)

forecast_sarima_log <- forecast(modelo_sarima_log, h = 50 * 12)

autoplot(forecast_sarima_log)

forecast_simple <- data.frame(
  Fecha = time(forecast_sarima_log$mean),  # Los tiempos de las proyecciones
  Tasa_Proyectada = forecast_sarima_log$mean,  # Las proyecciones
  IC_Lower_80 = forecast_sarima_log$lower[,1],  # Límite inferior del IC 80%
  IC_Upper_80 = forecast_sarima_log$upper[,1],  # Límite superior del IC 80%
  IC_Lower_95 = forecast_sarima_log$lower[,2],  # Límite inferior del IC 95%
  IC_Upper_95 = forecast_sarima_log$upper[,2]   # Límite superior del IC 95%
)


view(forecast_simple)

real <- tail(soberana_final_mes,1)$tasa

forecast_sarima_log_df$`Point Forecast` <- real*exp(cumsum(forecast_sarima_log_df$`Point Forecast`))

colnames(forecast_sarima_log_df) <- c('tasa', 'lo80', 'hi80', 'lo95', 'hi95')

autoplot(forecast_sarima_log_df$tasa)

###############################################

# Ajuste del modelo Holt-Winters (ETS con estacionalidad)
frequency(ts_soberana_final_mes)
modelo_hw <- ets(ts_soberana_final_mes, model = "AAA")
summary(modelo_hw)
checkresiduals(modelo_hw)

forecast_hw <- forecast(modelo_hw, h = 50 * 12)  # Pronóstico a 50 años (600 meses)

# Graficar
autoplot(forecast_hw) + 
  labs(title = "Pronóstico Holt-Winters (ETS) con Estacionalidad", x = "Fecha", y = "Tasa proyectada")

#############################################

soberana <- soberana %>% mutate(fecha = as.Date(fecha))

soberana_final_mes <- soberana %>%
  mutate(anio_mes = floor_date(fecha, "month")) %>% 
  group_by(anio_mes) %>% 
  summarise(
    dias = last(dias),
    tasa = last(tasa),
    fecha = last(fecha)
  ) %>% select(-anio_mes)

view(soberana_final_mes)


####

datos <- soberana_final_mes

datos$tasa <- as.numeric(datos$tasa)

serie_temporal <- ts(datos$tasa, frequency=12, start=c(2025, 1))

plot(serie_temporal, main="Serie Temporal de la Tasa", xlab="Fecha", ylab="Tasa")

acf(serie_temporal, main="ACF de la Tasa")
pacf(serie_temporal, main="PACF de la Tasa")

adf.test(serie_temporal)

arima <- auto.arima(serie_temporal)
summary(arima)

checkresiduals(arima)

predicciones <- forecast(arima, h=120)
plot(predicciones, main="Pronóstico de la Tasa", xlab="Fecha", ylab="Tasa")

accuracy(predicciones)

modelo_hw <- ets(serie_temporal)
summary(modelo_hw)
checkresiduals(modelo_hw)

predicciones_hw <- forecast(modelo_hw, h=120)
plot(predicciones_hw)

modelo_arimax <- auto.arima(serie_temporal, xreg=datos$dias)
summary(modelo_arimax)
checkresiduals(modelo_arimax)

fechas <- read_excel('../../data/curva_soberana.xlsx', sheet = 'proyeccion')
fechas <- tail(fechas, 25974)
fechas <- fechas %>%
  group_by(Month = floor_date(Fecha, "month")) %>%
  filter(Fecha == max(Fecha)) %>%
  ungroup()

fechas <- tail(fechas, 853)

predicciones_arimax <- forecast(modelo_arimax, xreg=fechas$`Plazo en días`, h=12)
plot(predicciones_arimax)

df_predicciones <- data.frame(
  fecha = fechas$Fecha,
  Tasa = predicciones_arimax$mean,  # Los valores predichos
  Lower_80 = predicciones_arimax$lower[,1],  # Límite inferior del intervalo de confianza al 80%
  Upper_80 = predicciones_arimax$upper[,1],  # Límite superior del intervalo de confianza al 80%
  Lower_95 = predicciones_arimax$lower[,2],  # Límite inferior del intervalo de confianza al 95%
  Upper_95 = predicciones_arimax$upper[,2]   # Límite superior del intervalo de confianza al 95%
)

#view(df_predicciones)

library('writexl')
write_xlsx(df_predicciones, '../../data/pronostico_curva_soberana.xlsx')
