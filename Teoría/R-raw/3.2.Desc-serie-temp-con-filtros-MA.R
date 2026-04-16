# Definición del directorio y carga de datos
dir = '' # Dejá esto vacío si el archivo está en tu carpeta de trabajo
dato = read.csv(paste(dir, 'TS_1.csv', sep=""))
print(names(dato))
# [1] "X" "t" "a0" "a1" "a2" "rn" "a"

# Configuración de gráficos 2x2
op <- par(mfrow = c(2, 2))
plot(dato$t, dato$a0, type='l', xlim=c(0,260), xlab='dia', ylab='a0')
plot(dato$t, dato$a1, type='l', xlim=c(0,260), xlab='dia', ylab='a1')
plot(dato$t, dato$a2, type='l', xlim=c(0,260), xlab='dia', ylab='a2')
plot(dato$t, dato$rn, type='l', xlim=c(0,260), xlab='dia', ylab='rn')

# Configuración de gráfico único
op <- par(mfrow = c(1, 1))
plot(dato$t, dato$a, type='l', xlim=c(0,260), xlab='dia', ylab='a')




print(names(dato))
# Debería mostrar: "X" "t" "a0" "a1" "a2" "rn" "a"

# Configuración de la ventana gráfica: 2 filas y 2 columnas
op <- par(mfrow = c(2, 2))

# Gráficos de las componentes individuales
plot(dato$t, dato$a0, type='l', xlim=c(0,260), xlab='dia', ylab='a0')
plot(dato$t, dato$a1, type='l', xlim=c(0,260), xlab='dia', ylab='a1')
plot(dato$t, dato$a2, type='l', xlim=c(0,260), xlab='dia', ylab='a2')
plot(dato$t, dato$rn, type='l', xlim=c(0,260), xlab='dia', ylab='rn')

# Configuración de la ventana gráfica: 1 fila y 1 columna
op <- par(mfrow = c(1, 1))

# Gráfico de la serie temporal completa (señal original)
plot(dato$t, dato$a, type='l', xlim=c(0,260), xlab='dia', ylab='a')




# Aplicación de filtros de Media Móvil (MA)
# MA7: Filtro de ventana 7 (suavizado leve)
outMA7 <- filter(dato$a, rep(1/7, 7), circular = TRUE)
difMA7 <- dato$a - outMA7

# MA21: Filtro de ventana 21 (suavizado más fuerte)
outMA21 <- filter(dato$a, rep(1/21, 21), circular = TRUE)
difMA21 <- outMA7 - outMA21

# Configuración de gráficos 2x2
op <- par(mfrow = c(2, 2))

# Visualización de resultados
plot(dato$t, outMA7, type='l', xlab='dia', ylab='sal MA7')
plot(dato$t, difMA7, type='l', xlab='dia', ylab='dif MA7')
plot(dato$t, outMA21, type='l', xlab='dia', ylab='sal MA21')
plot(dato$t, difMA21, type='l', xlab='dia', ylab='dif MA21')


# Configuración para un solo gráfico
op <- par(mfrow = c(1, 1))

# Gráfico base con la salida del filtro MA7
plot(dato$t, outMA7, type='l', ylim=c(-10, 70), xlab='dia', ylab='sal - dif MA7 y MA21', col='black')

# Superposición de las demás componentes
lines(dato$t, difMA7, col='blue')    # Ruido / Alta frecuencia
lines(dato$t, outMA21, col='red')   # Tendencia / Baja frecuencia
lines(dato$t, difMA21, col='green') # Estacionalidad / Frecuencia intermedia

####################################################################################
# pasamos del dominio del tiempo al dominio de la frecuencia usando la FFT (Fast Fourier Transform).

# Transformada de Fourier (FFT) y cálculo del Módulo (Magnitud)
fft.a <- Mod(fft(dato$a))
fft.outMA7 <- Mod(fft(outMA7))
fft.difMA7 <- Mod(fft(difMA7))
fft.outMA21 <- Mod(fft(outMA21))
fft.difMA21 <- Mod(fft(difMA21))

# Configuración del gráfico único
op <- par(mfrow = c(1, 1))

# Gráfico del espectro de la señal original
plot(fft.a, type='l', col='black', ylim=c(0,800), xlim=c(0,128), lty=2)

# Superposición de los espectros filtrados (Análisis espectral)
lines(fft.outMA7, col='black')
lines(fft.difMA7, col='blue')
lines(fft.outMA21, col='red')
lines(fft.difMA21, col='green')

