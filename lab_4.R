library(fable)
library(forecast)
library(fpp2)
library(fpp)
library(ggplot2)
library(astsa)
library(tseries)


# Завдання 1
plot(wmurders)
frequency(wmurders)

adf.test(wmurders)  # Тест Дікі-Фуллера на стаціонарність

# Приведення ряду до стаціонарного вигляду
wmurders_diff <- diff(wmurders)
adf.test(wmurders_diff) # ряд стаціонарний, тому d = 1

acf(wmurders_diff) # p = 2
pacf(wmurders_diff) # q = 1

model_wmurders <- arima(wmurders, order = c(2, 1, 1), include.mean = TRUE)  # замініть p, d, q на знайдені значення
checkresiduals(model_wmurders)  # Перевірка залишків на нормальність і автокореляцію

fc_wmurders <- forecast(model_wmurders, h = 3)
print(fc_wmurders)

plot(fc_wmurders)

auto_wmurders <- auto.arima(wmurders)
checkresiduals(auto_wmurders)
fc_auto_wmurders <- forecast(auto_wmurders, h = 3)
plot(fc_auto_wmurders)

summary(auto_wmurders)
summary(model_wmurders)
accuracy(model_wmurders)


# Завдання 2
plot(austa)
frequency(austa)

fit_austa <- auto.arima(austa)
checkresiduals(fit_austa)
fc_austa <- forecast(fit_austa, h = 10)
plot(fc_austa)
summary(fit_austa)

# b
# Модель ARIMA(0,1,1) без дрейфу
model_arima_011 <- Arima(austa, order = c(0, 1, 1), include.drift = FALSE)
summary(model_arima_011)

fc_arima_011 <- forecast(model_arima_011, h = 10)
print(fc_arima_011)
plot(fc_arima_011)

# Модель ARIMA(0,1,0) без дрейфу (видалено MA)
model_arima_010 <- Arima(austa, order = c(0, 1, 0), include.drift = FALSE)
summary(model_arima_010)

fc_arima_010 <- forecast(model_arima_010, h = 10)
print(fc_arima_010)
plot(fc_arima_010)

# c 
# Модель ARIMA(2,1,3) з дрейфом
model_arima_213 <- Arima(austa, order = c(2, 1, 3), include.drift = TRUE)
summary(model_arima_213)

fc_arima_213 <- forecast(model_arima_213, h = 10)
print(fc_arima_213)
plot(fc_arima_213)

# Модель ARIMA(2,1,3) без константи
model_arima_213_no_const <- Arima(austa, order = c(2, 1, 3), include.drift = FALSE)
summary(model_arima_213_no_const)

fc_arima_213_no_const <- forecast(model_arima_213_no_const, h = 10)
print(fc_arima_213_no_const)
plot(fc_arima_213_no_const)

# d
# Модель ARIMA(0,0,1) з константою
model_arima_001_const <- Arima(austa, order = c(0, 0, 1), include.mean = TRUE)
summary(model_arima_001_const)

fс_arima_001_const <- forecast(model_arima_001_const, h = 10)
print(fс_arima_001_const)
plot(fс_arima_001_const)

# Модель ARIMA(0,0,0) з константою (видалено MA)
model_arima_000_const <- Arima(austa, order = c(0, 0, 0), include.mean = TRUE)
summary(model_arima_000_const)

fс_arima_000_const <- forecast(model_arima_000_const, h = 10)
print(fс_arima_000_const)
plot(fс_arima_000_const)

# e
# Модель ARIMA(0,2,1) без константи
model_arima_021 <- Arima(austa, order = c(0, 2, 1), include.mean = FALSE)
summary(model_arima_021)

fс_arima_021 <- forecast(model_arima_021, h = 10)
print(fс_arima_021)
plot(fс_arima_021)



# Завдання 3
# а 
autoplot(usgdp)
str(usgdp)

lambda <- BoxCox.lambda(usgdp)
lambda 

# b
model_auto_lambda <- auto.arima(usgdp, lambda = lambda)
summary(model_auto_lambda)

#c
Acf(usgdp)
Pacf(usgdp)

model_1 <- auto.arima(usgdp)
summary(model_1)

# Модель ARIMA(1,1,0)
model_2 <- Arima(usgdp, order = c(1, 1, 0))
summary(model_2)

# Модель ARIMA(1,1,1)
model_111 <- Arima(usgdp, order = c(1, 1, 1), lambda = lambda)
summary(model_111)

# Модель ARIMA(2,1,2)
model_212 <- Arima(usgdp, order = c(2, 1, 2), lambda = lambda)
summary(model_212)

# d
checkresiduals(model_auto_lambda)

#e
fc_usgdp <- forecast(model_auto_lambda, h = 24)
print(fc_usgdp)
plot(fc_usgdp)
accuracy(fc_usgdp)

# Модель ETS без перетворення
model_ets <- ets(usgdp)
summary(model_ets)

forecast_ets <- forecast(model_ets, h = 24)
print(forecast_ets)
plot(forecast_ets)


# Завдання 4
str(austourists)
autoplot(austourists)
frequency(austourists)
ggseasonplot(austourists)
ggsubseriesplot(austourists)
adf.test(austourists)

ggAcf(austourists)
ggPacf(austourists)

austourists_diff <- diff(austourists)
ggAcf(austourists_diff)
ggPacf(austourists_diff)

model_austourists <- Arima(austourists, order=c(1,1,1), seasonal=c(1,1,1))
summary(model_austourists)

fc_austourists <- forecast(model_austourists, h = 20)
plot(fc_austourists)


auto_model_austourists <- auto.arima(austourists, seasonal = TRUE)
summary(auto_model_austourists)

fc_auto_austourists <- forecast(auto_model_austourists, h = 20)
plot(fc_auto_austourists)


# Завдання 5

str(usmelec)
plot(usmelec)
frequency(usmelec)
ggseasonplot(usmelec)
ggsubseriesplot(usmelec)

# Обчислюємо та графічно представляємо 12-місячне ковзне середнє
moving_average <- ma(usmelec, order = 12)
plot(usmelec, main = "Часовий ряд і 12-місячне ковзне середнє", ylab = "Виробництво (млрд кВт/год)")
lines(moving_average, col = "red", lwd = 2)

# Чи потребують дані перетворення?
lambda <- BoxCox.lambda(usmelec)
lambda
usmelec_transformed <- BoxCox(usmelec, lambda=lambda)
plot(usmelec_transformed, main="Box-Cox перетворений ряд", ylab="Перетворене виробництво")

usmelec_diff_bc <- diff(usmelec_transformed, lag = 12)
usmelec_diff_bc <- diff(usmelec_diff_bc, differences = 1)

# Перевірка стаціонарності
adf_test <- adf.test(usmelec_diff_bc)
print(usmelec_diff_bc)

# Підбір параметрів моделі
ggAcf(usmelec_diff_bc)
ggPacf(usmelec_diff_bc)

model_usmelec <- Arima(usmelec_diff_bc, order=c(2,0,1), seasonal=c(2,0,1))
summary(model_usmelec)
checkresiduals(model_usmelec)

model_2_usmelec <- Arima(usmelec_diff_bc, order=c(0,0,2), seasonal=c(0,0,2))
summary(model_2_usmelec)
checkresiduals(model_2_usmelec)

model_3_usmelec <- Arima(usmelec_diff_bc, order=c(2,0,2), seasonal=c(2,0,2))
summary(model_3_usmelec)
checkresiduals(model_3_usmelec)

auto_model_usmelec <- auto.arima(usmelec_diff_bc, seasonal = TRUE)
summary(auto_model_usmelec)
checkresiduals(auto_model_usmelec)


#Найкраща модель 
fc_usmelec <- forecast(auto_model_usmelec, h = 180)
plot(fc_usmelec)



# Завдання 6
plot(mcopper)
str(mcopper)
frequency(mcopper)
ggseasonplot(mcopper)
ggsubseriesplot(mcopper)

adf.test(mcopper)

lambda_mcopper <- BoxCox.lambda(mcopper)
mcopper_transformed <- BoxCox(mcopper, lambda = lambda_mcopper)
plot(mcopper_transformed)
adf.test(mcopper_transformed)

# Autoarima
auto_model_mcopper <- auto.arima(mcopper_transformed, seasonal = TRUE)
summary(auto_model_mcopper)


# Приклад моделі ARIMA з конкретними параметрами
model_arima_custom <- arima(mcopper_transformed, order = c(1, 1, 1))  # Замінити на ваші параметри
summary(model_arima_custom)


# Модель SARIMA
model_sarima <- arima(mcopper_transformed, order = c(1, 1, 1), seasonal = c(1, 1, 1))
summary(model_sarima)

# найкраща модель
checkresiduals(auto_model_mcopper)
fc_mcopper <- forecast(auto_model_mcopper, h = 36)
plot(fc_mcopper)

# модель ets
model_ets <- ets(mcopper)
forecast_ets <- forecast(model_ets, h = 12)
plot(forecast_ets)

checkresiduals(model_ets)
summary(model_ets)


# Завдання 7
plot(auscafe)
frequency(auscafe)
ggseasonplot(auscafe)


plot(diff(auscafe))

lambda_auscafe <- BoxCox.lambda(diff(auscafe))
lambda_auscafe

adf.test(auscafe)
adf.test(diff(auscafe)) #ряд стаціонарний

auscafe_diff <- diff(auscafe)
ggAcf(auscafe)
ggPacf(auscafe)

model_1_auscafe <-Arima(auscafe, order = c(1, 1, 0), seasonal = c(1, 1, 0))
summary(model_1_auscafe)

model_2_auscafe <-Arima(auscafe, order = c(2, 1, 2), seasonal = c(2, 1, 2))
summary(model_2_auscafe)

model_3_auscafe <-Arima(auscafe, order = c(2, 1, 0), seasonal = c(2, 1, 0))
summary(model_3_auscafe)

model_auto_auscafe <- auto.arima(auscafe, seasonal = TRUE)
summary(model_auto_auscafe)


# Найкраща модель
checkresiduals(model_2_auscafe)

fc_auscafe <- forecast(model_2_auscafe, h = 24)
plot(fc_auscafe)


# Модель ets
model_ets_auscafe <- ets(auscafe)
fc_ets_auscafe <- forecast(model_ets_auscafe, h = 24)
plot(fc_ets_auscafe)

checkresiduals(fc_ets_auscafe)
summary(model_ets_auscafe)
