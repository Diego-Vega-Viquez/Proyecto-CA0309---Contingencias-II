library(readxl)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(car)
library(stats)
library(dplyr)
library(forecast)
library(tseries)
library(randomForest)
library(Metrics)
library(caret)

data <- read_excel('../../data/IPC Banco central.xlsx', sheet = 'Series')
fecha_pronostico <- read_excel('../../data/IPC Banco central.xlsx', sheet = 'Pronostico')

data$Fecha <- as.Date(data$Fecha)
fecha_pronostico$fecha <- as.Date(fecha_pronostico$fecha)

ts_ipc <- ts(data$`IPC, variación mensual (%)`, frequency = 12, start = c(1976,2))

adf.test(ts_ipc)
kpss.test(ts_ipc)
pp.test(ts_ipc)

autoplot(ts_ipc) + ggtitle("Inflación mensual (variación del IPC)")

## Arima

arima <- auto.arima(ts_ipc)
summary(arima)
checkresiduals(arima)

pronostico_arima <- forecast(arima, h = 338)

autoplot(pronostico_arima) + ggtitle("Pronóstico ARIMA de inflación mensual")

## ETS

ets <- ets(ts_ipc)
summary(ets)
checkresiduals(ets)

pronostico_ets <- forecast(ets, h = 338)
autoplot(pronostico_ets) + ggtitle("Pronóstico ETS de inflación mensual")

## SARIMA

sarima <- auto.arima(ts_ipc, seasonal = TRUE)
summary(sarima)
checkresiduals(sarima)

pronostico_sarima <- forecast(sarima, h = 338)
autoplot(pronostico_sarima) + ggtitle("Pronóstico SARIMA de inflación mensual")

## Comparación

accuracy(arima)
accuracy(ets)
accuracy(sarima)

data.frame(
  Modelo = c("ARIMA", "ETS", "SARIMA"),
  AICc = c(arima$aicc, ets$aicc, sarima$aicc)
)

autoplot(ts_ipc, series="Observado") +
  autolayer(pronostico_arima$mean, series="ARIMA", color="blue") +
  autolayer(pronostico_ets$mean, series="ETS", color="black") +
  autolayer(pronostico_sarima$mean, series="SARIMA", color="darkgreen") +
  ggtitle("Comparación de pronósticos de inflación mensual") +
  ylab("Variación mensual (%)") +
  theme_minimal()

## Otras opciones (BoxCox)

lambda <- BoxCox.lambda(ts_ipc)
ts_ipc_bc <- BoxCox(ts_ipc, lambda)

modelo <- auto.arima(ts_ipc_bc)
summary(modelo)
checkresiduals(modelo)

forecast_bc <- forecast(modelo, h = 338)
forecast_final <- InvBoxCox(forecast_bc$mean, lambda)
plot(forecast_final)

## Otras opciones (SARIMA MEJORADO)

sarima_mejor <- auto.arima(ts_ipc,
                           seasonal = TRUE,
                           stepwise = FALSE,
                           approximation = FALSE,
                           max.p = 5, max.q = 5, max.P = 2, max.Q = 2
)
summary(sarima_mejor)
checkresiduals(sarima_mejor)

## Otras Opciones (Random Forest)

# Crear los lags
lags <- 24
df <- data.frame(inflacion = data$`IPC, variación mensual (%)`)
for (i in 1:lags) df[paste0("lag_", i)] <- dplyr::lag(df$inflacion, i)
df <- na.omit(df)

# Agregar tendencia y mes
# Dividir datos
train <- head(df, -120)
test <- tail(df, 120)

# Configurar validación cruzada temporal
ctrl <- trainControl(
  method = "timeslice",
  initialWindow = nrow(train) - 120,
  horizon = 12,
  fixedWindow = TRUE,
  verboseIter = TRUE
)

# Definir rejilla de hiperparámetros
grid <- expand.grid(
  mtry = c(2, 4, 6, 8),
  splitrule = "variance",
  min.node.size = c(5, 10, 15)
)

# Entrenar modelo
set.seed(123)
rf_tuned <- train(
  inflacion ~ ., data = train,
  method = "ranger",              # usa la versión optimizada de random forest
  trControl = ctrl,
  tuneGrid = grid,
  importance = "impurity",
  num.trees = 500
)

# Resultados
print(rf_tuned)
plot(rf_tuned)
best_model <- rf_tuned$finalModel

pred_train <- predict(rf_tuned, newdata = train)
pred_test <- predict(rf_tuned, newdata = test)

# Métricas
metrics <- function(obs, pred){
  data.frame(
    RMSE = rmse(obs, pred),
    MAE = mae(obs, pred),
    R2 = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
  )
}

met_train <- metrics(train$inflacion, pred_train)
met_test <- metrics(test$inflacion, pred_test)

cat("\n📊 Métricas de entrenamiento:\n")
print(met_train)
cat("\n📊 Métricas de prueba:\n")
print(met_test)

# --- Gráficos ---
df_plot <- data.frame(
  Observado = c(train$inflacion, test$inflacion),
  Predicho = c(pred_train, pred_test),
  Tipo = c(rep("Entrenamiento", nrow(train)), rep("Prueba", nrow(test))),
  Tiempo = 1:(nrow(train) + nrow(test))
)

# 1️⃣ Entrenamiento
g1 <- ggplot(df_plot[df_plot$Tipo == "Entrenamiento", ], aes(x = Tiempo)) +
  geom_line(aes(y = Observado, color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicho")) +
  ggtitle("Random Forest (Tuning) - Entrenamiento") +
  ylab("Inflación mensual (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "blue"))

# 2️⃣ Prueba
g2 <- ggplot(df_plot[df_plot$Tipo == "Prueba", ], aes(x = Tiempo)) +
  geom_line(aes(y = Observado, color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicho")) +
  ggtitle("Random Forest (Tuning) - Prueba") +
  ylab("Inflación mensual (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "red"))

# 3️⃣ Combinado
g3 <- ggplot(df_plot, aes(x = Tiempo)) +
  geom_line(aes(y = Observado, color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicho")) +
  facet_wrap(~Tipo, scales = "free_x", ncol = 1) +
  ggtitle("Random Forest (Tuning) - Inflación mensual (Train + Test)") +
  ylab("Inflación mensual (%)") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "green"))

print(g1)
print(g2)
print(g3)
