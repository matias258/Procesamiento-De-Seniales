############################ Forecasting parte 1 - Clasico ############################

# Ejemplo ST estacionaria
library(tseries)
library(forecast)


N = 256
tiempo = 0:(N-1)

xn <- rnorm(N)
plot(tiempo, xn, type ='l')


# --- Prueba Augmented Dickey-Fuller (ADF) ---
# Hipótesis nula (H0): La serie tiene una raíz unitaria (no estacionaria)
# Resultado en imagen: p-value = 0.01
# Interpretación: Como p < 0.05, rechazamos H0; la serie es estacionaria.
print(adf.test(xn))

# --- Prueba KPSS ---
# Hipótesis nula (H0): La serie es estacionaria
# Resultado en imagen: p-value = 0.1
# Interpretación: Como p > 0.05, no podemos rechazar H0; confirma estacionariedad.
print(kpss.test(xn))

# Conclusión final:
# Ambas pruebas coinciden: La serie temporal es estacionaria.



### Otra prueba con señal sinoidal de 10 ciclos
x <- 5*sin(10*2*pi*tiempo/N) + rnorm(N)
plot(tiempo, x, type ='l')

## visualmente vemos que puede tratarse de una ST estacionaria, verifiquemos:

# --- Prueba Augmented Dickey-Fuller (ADF) ---
# Hipótesis nula (H0): La serie tiene una raíz unitaria (no estacionaria)
# Resultado en imagen: Dickey-Fuller = -11.894, p-value = 0.01
# Interpretación: p < 0.05, se rechaza H0. La serie es estacionaria.
print(adf.test(x))

# --- Prueba KPSS ---
# Hipótesis nula (H0): La serie es estacionaria
# Resultado en imagen: KPSS Level = 0.04288, p-value = 0.1
# Interpretación: p > 0.05, no se rechaza H0. Se confirma la estacionariedad.
print(kpss.test(x))

# Conclusión: 
# Ambas pruebas son consistentes y sugieren que la serie es estacionaria.


### Ejemplo ST no estacionaria (suma acumulada)
e <- rnorm(N)
xcumsum <- cumsum(e)
plot(tiempo, xcumsum, type = 'l')

# los cambios rapidos en los piquitos -> alta frecuencia
# los cambios lentos en los piquitos -> baja frecuencia
# Podemos ver que la tendencia es de baja frecuencia en este caso
# Entonces lo mas optimo seria aplicar un pasa altos


# --- Prueba Augmented Dickey-Fuller (ADF) ---
# Hipótesis nula (H0): La serie tiene una raíz unitaria (no estacionaria)
# Resultado en imagen: p-value = 0.3581
# Interpretación: Como p > 0.05, no rechazamos H0; la serie NO es estacionaria.
print(adf.test(xcumsum))

# --- Prueba KPSS ---
# Hipótesis nula (H0): La serie es estacionaria
# Resultado en imagen: p-value = 0.01
# Interpretación: Como p < 0.05, rechazamos H0; confirma que la serie NO es estacionaria.
print(kpss.test(xcumsum))

# Conclusión final:
# Ambas pruebas coinciden: La serie temporal es no estacionaria.


### ----- Metodo para hacer estacionaria una no estacionaria -------
# --- Diferenciación para eliminar la tendencia ---
# La función diff() calcula la diferencia entre valores consecutivos (x[t] - x[t-1])
xcumsum.diff <- diff(xcumsum)

# Ajuste del vector de tiempo:
# Al diferenciar, perdemos una observación (la primera), por lo que debemos 
# ajustar el vector de tiempo para que coincida con la longitud de la nueva serie.
tiempo2 <- tiempo[1:length(xcumsum.diff)]

# Graficar la serie diferenciada
# Al eliminar la tendencia con diff(), la serie resultante suele ser estacionaria.
plot(tiempo2, xcumsum.diff, type='l', main="Serie diferenciada (estacionaria)", xlab="Tiempo", ylab="Valores")


# --- Validación de estacionariedad tras aplicar diff() ---

# 1. Prueba Augmented Dickey-Fuller (ADF)
# Hipótesis nula (H0): La serie tiene una raíz unitaria (no estacionaria)
# Resultado: p-value = 0.01
# Interpretación: p < 0.05, se rechaza H0. La serie diferenciada es estacionaria.
print(adf.test(xcumsum.diff))

# 2. Prueba KPSS
# Hipótesis nula (H0): La serie es estacionaria
# Resultado: p-value = 0.1
# Interpretación: p > 0.05, no se rechaza H0. Se confirma la estacionariedad.
print(kpss.test(xcumsum.diff))

# Conclusión: 
# Tras la diferenciación, 'xcumsum.diff' cumple con los criterios de estacionariedad.



# --- Ejemplo ST Indefinido ----

# Parámetros base
m <- 0.3          # Pendiente de la tendencia
b0 <- 10          # Intersección (valor inicial)
N <- length(tiempo) # Asumiendo que 'tiempo' ya está definido

# Generación de la serie 'xtrend'
# Componentes:
# 1. Dos señales sinusoidales (frecuencias 10 y 30)
# 2. Ruido blanco gaussiano: rnorm(N)
# 3. Tendencia lineal: m * tiempo + b0
xtrend <- sin(10 * 2 * pi * tiempo / N) + 
  sin(30 * 2 * pi * tiempo / N) + 
  rnorm(N) + 
  (m * tiempo) + 
  b0

# Graficar la serie
plot(tiempo, xtrend, type = 'l', 
     main = "Serie Temporal Indefinida", 
     xlab = "Tiempo", ylab = "Valor")

# --- Análisis de estacionariedad: Conflicto de resultados ---

# 1. Prueba Augmented Dickey-Fuller (ADF)
# Hipótesis nula (H0): La serie tiene una raíz unitaria (no estacionaria)
# Resultado: p-value = 0.01
# Interpretación: Como p < 0.05, sugiere RECHAZAR H0 (la serie sería estacionaria).
print(adf.test(xtrend))

# 2. Prueba KPSS
# Hipótesis nula (H0): La serie es estacionaria
# Resultado: p-value = 0.01
# Interpretación: Como p < 0.05, sugiere RECHAZAR H0 (la serie NO es estacionaria).
print(kpss.test(xtrend))

# Conclusion: Es indefinida

### Detrended ###
# 1. Ajustar un modelo de regresión lineal simple: xtrend = beta0 + beta1 * tiempo
lm.xtrend <- lm(xtrend ~ tiempo)

# 2. Ver el resumen del modelo para verificar la significancia de los coeficientes
print(summary(lm.xtrend))

# 3. Remover la tendencia calculada:
# Restamos a la serie original los valores predichos por la regresión: (Intercepto + Pendiente * tiempo)
xtrend.d <- xtrend - (lm.xtrend$coefficients[1] + lm.xtrend$coefficients[2] * tiempo)

# 4. Graficar la serie resultante (ahora sin tendencia)
plot(tiempo, xtrend.d, type = 'l', 
     main = "Serie sin tendencia (Residuos)", 
     xlab = "Tiempo", ylab = "Valores desestacionalizados")

# --- Validación final de la serie desestacionalizada ---

# 1. Prueba ADF sobre los residuos (xtrend.d)
# Hipótesis nula (H0): Raíz unitaria (no estacionaria)
# Resultado: p-value = 0.01
# Interpretación: p < 0.05, se rechaza H0. Es estacionaria.
print(adf.test(xtrend.d))

# 2. Prueba KPSS sobre los residuos (xtrend.d)
# Hipótesis nula (H0): La serie es estacionaria
# Resultado: p-value = 0.1
# Interpretación: p > 0.05, no se rechaza H0. Se confirma la estacionariedad.
print(kpss.test(xtrend.d))

# Conclusión:
# Tras remover la tendencia determinista, la serie es ahora claramente estacionaria.


#### ------------- Forecasting Lineal ------------------- ###

# Parámetros de la serie
N <- 250
tiempo <- 0:(N-1)

m <- 0.3
b0 <- 10

# Generación de la serie 'x'
# Componentes:
# 1. Dos componentes sinusoidales de diferente amplitud y frecuencia
# 2. Ruido blanco gaussiano con desviación estándar de 10
# 3. Tendencia lineal (m * tiempo + b0)
x <- 15 * sin(2 * pi * tiempo / N) + 
  5 * sin(5 * 2 * pi * tiempo / N) + 
  rnorm(N, sd = 10) + 
  (m * tiempo) + 
  b0

# Convertir a objeto de serie temporal (ts)
x <- ts(x)

# Graficar
plot(tiempo, x, type = 'l', 
     main = "Serie Temporal Sintética con Tendencia y Oscilaciones", 
     xlab = "Tiempo", ylab = "Valor")


# 1. Ajustar el modelo de regresión lineal de serie temporal
# tslm detecta automáticamente el componente 'trend'
fit.tslm <- tslm(x ~ trend)

# 2. Resumen del modelo con interpretación de coeficientes
# (Intercept): Valor estimado de 'x' cuando el tiempo es 0.
# trend: Pendiente de la tendencia (incremento promedio de 'x' por unidad de tiempo).
# Pr(>|t|): Si este valor es < 0.05, el coeficiente es estadísticamente significativo.
summary(fit.tslm)

# 3. Pronóstico a 20 pasos (h=20) con intervalos de confianza
f <- forecast(fit.tslm, h=20, level=c(80, 95))

# 4. Visualización
plot(f, ylab="x", xlab="tiempo")

# Agregar la línea de ajuste (valores predichos por el modelo)
lines(fitted(fit.tslm), col="blue")

# --- Interpretación técnica de los Coeficientes (Resumen) ---
# Según tu imagen:
# (Intercept) = 22.6260  -> Significa que el nivel base de la serie es ~22.6
# trend       = 0.1958   -> Significa que cada paso de tiempo, la serie sube 0.1958 unidades.
# Ambos tienen Pr(>|t|) < 2e-16, lo que indica que son altamente significativos.


# --- Análisis de Residuos ---

# 1. Definir una ventana de graficación de 1 fila y 2 columnas
par(mfrow=c(1,2))

# 2. Extraer los residuos del modelo tslm y convertirlos a serie de tiempo
res <- ts(resid(fit.tslm))

# 3. Graficar los residuos para ver si tienen media cero y varianza constante
plot.ts(res, ylab="res (x)")
abline(h=0, col="red") # Línea de referencia en cero

# 4. Analizar la función de autocorrelación (ACF)
# Si los residuos son ruido blanco, las barras en el gráfico ACF deberían 
# caer rápidamente dentro de las bandas de significancia (líneas punteadas).
Acf(res)
# nos esta diciendo que esa serie temporal tiene componentes estocasticas pero tmb muchas periodicidades.
# Esas periodicidades se supone que van a seguir estando en la prediccion, osea en el futuro,


# Prueba de Durbin-Watson para detectar autocorrelación
# Si el p-value es muy pequeño (como 1.719e-08), indica que hay 
# autocorrelación significativa en los residuos (el modelo no es completo).
library(lmtest)
print(dwtest(fit.tslm, alt="two.sided"))

#  Configurar ventana gráfica (1 fila, 1 columna)
par(mfrow=c(1,1))

#  Histograma de los residuos
# breaks='FD' utiliza la regla de Freedman-Diaconis para elegir el número de bins
bins <- hist(res, breaks='FD', xlab='Residuos', main='Histograma de residuos')

#  Ajuste de curva normal teórica sobre el histograma
# Crea un vector de -40 a 40 para definir el eje x de la campana de Gauss
xx <- -40:40

# Dibuja la curva normal basada en la media (0) y la desviación estándar de los residuos
# El factor 1300 es un escalador para ajustar la altura de la curva al histograma
lines(xx, 1300 * dnorm(xx, 0, sd(res)), col=2)




# --- Introducción a la Descomposición Clásica de Series Temporales ---
# La descomposición clásica permite separar una serie temporal (ST) en tres 
# componentes fundamentales:
# 1. Componente Estacional: Patrones que se repiten en intervalos fijos.
# 2. Componente de Tendencia: La dirección a largo plazo de los datos.
# 3. Componente Irregular (Residuos): El "ruido" o variaciones aleatorias.
# El método utiliza medias móviles para suavizar fluctuaciones y puede 
# aplicarse de forma aditiva (suma de componentes) o multiplicativa 
# (producto de componentes).

# --- Código de ejecución ---
library(fpp2)

# Cargar el dataset de ejemplo 'elecequip' (equipo eléctrico)
data(elecequip)

# Visualizar la serie original
plot(elecequip, main="Serie original: Elecequip")

# Aplicar la descomposición clásica de tipo aditivo
fit.decomp <- decompose(elecequip, type='additive')

# Visualizar los componentes extraídos (Tendencia, Estacionalidad, Aleatorio)
plot(fit.decomp)




# --- Forecasting STL (Seasonal and Trend decomposition using Loess) ---
# La descomposición STL separa una serie temporal en sus componentes:
# 1. Estacional: Patrones repetitivos.
# 2. Tendencia: Dirección a largo plazo.
# 3. Irregular (resto): Componente aleatorio o ruido.
# Utiliza el algoritmo 'loess' para realizar ajustes locales en ventanas.

# --- Código de ejecución ---

# Cargar el dataset 'elecequip' y visualizarlo
data(elecequip)
plot(elecequip)

# Aplicar la descomposición STL
# t.window: ventana para la tendencia
# s.window: ventana para la estacionalidad ("periodic" para repetir el patrón)
# robust=TRUE: para ignorar valores atípicos
fit.stl <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit.stl)

# Remover la componente estacional para obtener datos ajustados estacionalmente
eeadj <- seasadj(fit.stl)

# Realizar un pronóstico (forecast) usando el método 'naive' sobre los datos ajustados
plot(naive(eeadj), xlab="index", 
     main="Forecasting de datos con estacionalidad removida")

# Realizar el forecasting completo (incluyendo estacionalidad)
fcast <- forecast(fit.stl, method="naive")
plot(fcast, ylab="index", 
     main="Forecasting completo")



# --- Introducción a Modelos ARMA ---
# Un modelo ARMA modela una serie temporal como la respuesta al impulso de un filtro 
# con componentes autoregresivos (AR) y/o de media móvil (MA).
# - Puro AR: Depende de p valores pasados.
# - Puro MA: Depende de q errores pasados.
# - ARMA(p, q): Combinación de ambos.

# --- Comparativa: FFT vs Modelos AR ---
# La FFT (spec.pgram) analiza la potencia de las frecuencias, mientras que 
# los modelos AR (spec.ar) estiman el espectro mediante procesos autorregresivos.

# Generación de señal sintética para comparar
N = 256
tiempo = 0:(N-1)
x <- sin(10*2*pi*tiempo/N) + sin(30*2*pi*tiempo/N) + rnorm(N)

# Visualización y análisis espectral
par(mfrow=c(2,2)) # Configura gráfico en 4 paneles

# 1. Gráfico de la serie original
plot(tiempo, x, type='l', main="Serie original")

# 2. Análisis mediante Periodograma (FFT)
x.fft <- spec.pgram(x, plot=TRUE, taper=0, log='no', main="FFT (Periodograma)")

# 3. Estimación mediante modelo AR de orden 8
x.ar8 <- spec.ar(x, plot=TRUE, log='no', order=8, main="Modelo AR(8)")

# 4. Estimación mediante modelo AR de orden 6
x.ar6 <- spec.ar(x, plot=TRUE, log='no', order=6, main="Modelo AR(6)")


# --- Comparativa de espectros AR con diferentes órdenes ---
# Se configuran 4 paneles para comparar el ajuste de memoria (lags)
par(mfrow=c(2,2))

# calculamos los coeficientes ar1, ar2, etc..
x.ar4 <- spec.ar(x, plot=TRUE, log='no', order=4)
x.ar3 <- spec.ar(x, plot=TRUE, log='no', order=3)
x.ar2 <- spec.ar(x, plot=TRUE, log='no', order=2)
x.ar1 <- spec.ar(x, plot=TRUE, log='no', order=1)
# Vemos que a medida que vamos bajando el orden, esta empeorando (modeliza peor)
# Osea que el orden del filtro es muy importante. Es critico encontrar el orden optimo


### Sumamos una tendencia ###

# Parámetros definidos en la imagen
m <- 0.1
b0 <- 10

# Aplicamos la tendencia a la señal x
x <- x + m * tiempo + b0

# Configuramos la ventana de gráficos en 2x2
par(mfrow=c(2,2))

# 1. Graficamos la serie temporal resultante
plot(tiempo, x, type='l', main="Serie Temporal con Tendencia")

# 2. Periodograma (FFT) sin destrendar
x.fft <- spec.pgram(x, plot=TRUE, taper=0, log='no', detrend=FALSE)

# 3. Modelo Autorregresivo de orden 8
x.ar8 <- spec.ar(x, plot=TRUE, log='no', order=8)

# 4. Modelo Autorregresivo de orden 6
x.ar6 <- spec.ar(x, plot=TRUE, log='no', order=6)

# Calculamos la diferencia de la serie x para eliminar la tendencia
xdif <- diff(x)

# Ajustamos la longitud del vector tiempo para que coincida con la serie diferenciada
# (nota: diff reduce la longitud de la serie en 1)
length(tiempo) <- length(xdif)

# Configuramos la ventana de gráficos en 2x2
par(mfrow=c(2,2))

# 1. Graficamos la serie diferenciada
plot(tiempo, xdif, type='l', main="Serie Diferenciada")

# 2. Periodograma (FFT) de la serie diferenciada
x.fft <- spec.pgram(xdif, plot=TRUE, taper=0, log='no', detrend=FALSE)

# 3. Modelo Autorregresivo de orden 8 aplicado a la serie diferenciada
x.ar8 <- spec.ar(xdif, plot=TRUE, log='no', order=8)

# 4. Modelo Autorregresivo de orden 6 aplicado a la serie diferenciada
x.ar6 <- spec.ar(xdif, plot=TRUE, log='no', order=6)


# Efecto del uso de diff()
# Asegúrate de tener definido un valor para N, por ejemplo: N <- 100

# Creamos un vector de ceros y asignamos un valor de 1 en la posición 5
x1 <- rep(0, N + 1)
x1[5] <- 1

# Calculamos la diferencia
x1.diff <- diff(x1)

# Configuramos la ventana de gráficos en 2x2
par(mfrow=c(2,2))

# 1. Graficamos el impulso original
plot(x1, type='l', main="Señal original (Impulso)")

# 2. Graficamos el efecto del diff (se convierte en un par de impulsos opuestos)
plot(x1.diff, type='l', main="Serie diferenciada")

# 3. Graficamos el espectro de la señal original
plot(Mod(fft(x1)), type='l', main="Espectro (FFT) original")

# 4. Graficamos el espectro tras aplicar diff
plot(Mod(fft(x1.diff)), type='l', main="Espectro (FFT) tras diff")


# Podemos ver que usar diff tiene un efecto. Hyay que ver si lo usamos o no.
# Cualquier procesamiento genera consecuencias


### Forecasting auto ARIMA
library(fpp2)
# (Explicar brevemente que es el forecasting ARIMA)

data(uschange)
plot(uschange[,1])

# Ajuste del modelo
fit.arima <- auto.arima(uschange[,1], seasonal=FALSE)

# Impresión de resultados con los comentarios de la salida estándar
print(fit.arima)
# Series: uschange[, 1] 
# ARIMA(0,0,3) with non-zero mean 
# 
# Coefficients:
#          ma1     ma2     ma3  intercept
#       0.2542  0.2260  0.2695     0.7562
# s.e.  0.0767  0.0779  0.0692     0.0844
# 
# sigma^2 estimated as 0.3953:  log likelihood = -154.73
# AIC=319.46   AICc=319.84   BIC=334.96

# Graficamos el pronóstico
plot(forecast(fit.arima, h=10), include=80)


### Forecasting ARIMA
par(mfrow=c(1,2))

# Graficamos la Función de Autocorrelación y Autocorrelación Parcial
# Esto ayuda a determinar visualmente los órdenes p y q
Acf(uschange[,1], main="")
Pacf(uschange[,1], main="")

# Ajustamos un modelo ARIMA(0,0,3) manualmente
fit.arima2 <- Arima(uschange[,1], order=c(0,0,3))

# Imprimimos los resultados del modelo
print(fit.arima2)
# Series: uschange[, 1] 
# ARIMA(0,0,3) with non-zero mean 
# 
# Coefficients:
#          ma1     ma2     ma3  intercept
#       0.2542  0.2260  0.2695     0.7562
# s.e.  0.0767  0.0779  0.0692     0.0844
# 
# sigma^2 estimated as 0.3953:  log likelihood = -154.73
# AIC=319.46   AICc=319.84   BIC=334.96

# Graficamos el pronóstico
plot(forecast(fit.arima2))


### Forecasting ARIMA puramente regresivo

# Ajustamos un modelo ARIMA(3,0,0) manualmente
fit.arima3 <- Arima(uschange[,1], order=c(3,0,0))

# Imprimimos los resultados del modelo
print(fit.arima3)
# Series: uschange[, 1] 
# ARIMA(3,0,0) with non-zero mean 
# 
# Coefficients:
#          ar1     ar2     ar3  intercept
#       0.2366  0.1603  0.1909     0.7533
# s.e.  0.0763  0.0774  0.0759     0.1153
# 
# sigma^2 estimated as 0.3921:  log likelihood = -154.08
# AIC=318.16   AICc=318.54   BIC=333.66

# Graficamos el pronóstico
plot(forecast(fit.arima3))


### FOrecasting ARIMA (ARMA)

# Ajustamos un modelo ARIMA(3,0,3) combinando componentes AR y MA
fit.arima4 <- Arima(uschange[,1], order=c(3,0,3))

# Imprimimos los resultados del modelo
print(fit.arima4)
# Series: uschange[, 1] 
# ARIMA(3,0,3) with non-zero mean 
# 
# Coefficients:
#             ar1     ar2     ar3     ma1     ma2     ma3 intercept
#          0.5487  0.4810 -0.4132 -0.2950 -0.4055  0.4796    0.7545
# s.e.     0.3340  0.2159  0.2513  0.3181  0.2074  0.1650    0.0968
# 
# sigma^2 estimated as 0.3939:  log likelihood = -154.73
# AIC=321.92   AICc=322.84   BIC=346.71

# Graficamos el pronóstico
plot(forecast(fit.arima4))


# Ajustamos un modelo ARIMA(3,1,3)
# Este modelo incluye diferenciación (d=1) para tratar la no estacionariedad
fit.arima5 <- Arima(uschange[,1], order=c(3,1,3))

# Imprimimos los resultados del modelo
print(fit.arima5)
# Series: uschange[, 1] 
# ARIMA(3,1,3) 
# 
# Coefficients:
#             ar1     ar2     ar3     ma1     ma2     ma3
#          -0.2747  0.4921  0.2266 -0.4611 -0.7180  0.1791
# s.e.      0.2610  0.2169  0.1046  0.2650  0.3289  0.2025
# 
# sigma^2 estimated as 0.3996:  log likelihood = -155.33
# AIC=324.66   AICc=325.39   BIC=346.32

# Graficamos el pronóstico
plot(forecast(fit.arima5))