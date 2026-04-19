 # Filtros FIR e IIR

################################# FIR ##############################

# Hagamos un ejemplo de FIR de Hanning, 
#  es decir un filtro con distintos pesos
N = 128
fir3 = rep(0,N)
fir3[1] = 1/4
fir3[2] = 1/2
fir3[3] = 1/4

ma3 = rep(0,N)
ma3[1:3] = 1/3

fft.fir3 = Mod(fft(fir3))
fft.ma3 = Mod(fft(ma3))
plot(fft.ma3, type = 'l', main = 'fft fir3 y ma3')
lines(fft.fir3, col = 'red')

# Agregar Leyenda por favor #


# Ejemplo de FIR5

fir5 = rep(0, N)
fir5[1] = 0.05
fir5[2] = 0.15
fir5[3] = 0.6
fir5[4] = 0.15
fir5[5] = 0.05

# Configurar el filtro de media móvil (MA)
ma5 = rep(0, N)
ma5[1:5] = 1/5

# Calcular la magnitud de la FFT
fft.fir5 = Mod(fft(fir5))
fft.ma5 = Mod(fft(ma5))

# Graficar los resultados
plot(fft.ma5, type="l", main="fft fir5 y ma5", ylab="Magnitud")
lines(fft.fir5, col="red")

# Agregar Leyenda por favor #


# Configurar el filtro FIR% de Hamming (ejemplo de 5 puntos)
fir5h = rep(0, N)
fir5h[1] = -3/35
fir5h[2] = 12/35
fir5h[3] = 17/35
fir5h[4] = 12/35
fir5h[5] = -3/35

# Configurar el filtro de media móvil (MA)
ma5 = rep(0, N)
ma5[1:5] = 1/5

# Calcular la magnitud de la FFT
fft.fir5h = Mod(fft(fir5h))
fft.ma5 = Mod(fft(ma5))

# Graficar los resultados
plot(fft.ma5, type="l", main="fft fir5h y ma5")
lines(fft.fir5h, col="red")

#### Ejemplo de Derivadas de 2 y 3 muestras

# Configurar el filtro de derivada de 2 muestras
der2 = rep(0, N)
der2[1] = 0.5
der2[2] = -0.5

# Configurar el filtro de derivada de 3 muestras
der3 = rep(0, N)
der3[1] = 0.5
der3[3] = -0.5

# Calcular la magnitud de la FFT
fft.der2 = Mod(fft(der2))
fft.der3 = Mod(fft(der3))

# Graficar los resultados
plot(fft.der2, type="l", main="fft der2 y der3")
lines(fft.der3, col="red")



# Configurar el filtro de derivada de 4 muestras
der4 = rep(0, N)
der4[1] = 2/10
der4[2] = 1/10
der4[0] = 0 
der4[4] = -1/10
der4[5] = -2/10

# Calcular la magnitud de la FFT
fft.der2 = Mod(fft(der2))
fft.der4 = Mod(fft(der4))

# Graficar los resultados
plot(fft.der2, type="l", main="fft der2 y der4")
lines(fft.der4, col="red")



################################# IIR ##############################

# Configurar señal de entrada x
x[2] = 1

# Configurar el filtro IIR de 2 coeficientes
iir2 = rep(0, N)
a = 0.5
b = -0.5

# Bucle para implementar la diferencia del filtro IIR2
for(n in 2:N)
{
  iir2[n] = a * x[n] - b * iir2[n-1]
}

# Calcular la magnitud de la FFT y graficar
fft.iir2 = Mod(fft(iir2))
plot(fft.iir2, type="l", main="fft iir2")

# Obs: notemos que es un filtro pasabajos



# Ejemplo IIR2b
iir2b = rep(0, N)
a = 0.3   
b = -0.7
        
# Bucle para implementar la diferencia del filtro IIR2b
for(n in 2:N)
{
  iir2b[n] = a * x[n] - b * iir2b[n-1]
}

# Calcular la magnitud de la FFT
fft.iir2b = Mod(fft(iir2b))

# Graficar comparando con el resultado anterior
plot(fft.iir2, type="l", main="fft iir2 y iir2b", ylim = c(0, 1))
lines(fft.iir2b, col="red")


# Ejemplo derivada con IIR
iir2d = rep(0, N)
a = 0.5
b = 0.5

# Bucle para implementar la diferencia del filtro IIR de derivada
for(n in 2:N)
{
  iir2d[n] = a * x[n] - b * iir2d[n-1]
}

# Calcular la magnitud de la FFT
fft.iir2d = Mod(fft(iir2d))

# Graficar comparando con el resultado de la derivada de 2 muestras (der2)
plot(fft.der2, type="l", main="fft der2 y iir2d", ylim = c(0, 1))
lines(fft.iir2d, col="red")

iir2d3 = rep(0, N)
a = 0.3
b = 0.7

# Bucle para implementar la diferencia del filtro IIR d3
for(n in 2:N)
{
  iir2d3[n] = a * x[n] - b * iir2d3[n-1]
}

# Calcular la magnitud de la FFT
fft.iir2d3 = Mod(fft(iir2d3))

# Graficar comparando los resultados
# Asume que fft.iir2, fft.iir2d y fft.iir2d3 ya han sido calculados
plot(fft.iir2, type="l", main="fft iir2, irr2d y irr2d3", ylim = c(0, 1))
lines(fft.iir2d, col="red")
lines(fft.iir2d3, col="blue")



######################## Splicacion de FIR e IIR #################

dato = read.csv("TS_1.csv")
op = par(mfrow = c(1, 1))
plot(dato$t, dato$a, type="l", xlim=c(0, 260), xlab="dia", ylab="a")

# notamos que hat componente de un ciclo entre los dias

# Le aplicamos un filtro FIR de 3 muestras a los datos
der.a = filter(dato$a, c(0.5, 0, -0.5), circular=TRUE)

# Configuramos el diseño de la gráfica
op = par(mfrow = c(1, 1))

# Graficar la serie original
plot(dato$t, dato$a, type="l", xlim=c(0, 260), ylim=c(-10, 70), xlab="t", ylab="a")


lines(der.a, col="red")

# recordar que este es un filtro pasa altos. Es decir que quedan las altas frecuencias, lo que vemos en rojo


# Veamos como queda en el dominio de la frecuencia
# Calcular la magnitud de la FFT para la señal original y para la señal derivada
fft.der.a = Mod(fft(der.a))
fft.a = Mod(fft(dato$a))

# Graficar comparando el espectro de la señal original y la derivada
plot(fft.a, type="l", xlim = c(0, 100), ylim = c(0, 1000))
lines(fft.der.a, col="red")


# Aplicar el filtro FIR de Hanning a los datos (Filtro Pasabajos)
# Los coeficientes c(0.25, 0.5, 0.25) implementan una suavización Hanning
h3.a = filter(dato$a, c(0.25, 0.5, 0.25), circular=TRUE)

# Configurar el diseño de la gráfica
op = par(mfrow = c(1, 1))

# Graficar la serie original
plot(dato$t, dato$a, type="l", xlim=c(0, 260), ylim=c(0, 70), xlab="dia", ylab="a")

# Añadir la línea del filtro Hanning aplicado en rojo
lines(h3.a, col="red")

# Ob: Nos filtro las altas frecuencias

# Y en el dominio de la frecuencia nos queda asi:
# Calcular la magnitud de la FFT para la señal suavizada con Hanning y la señal original
fft.h3.a = Mod(fft(h3.a))
fft.a = Mod(fft(dato$a))

# Graficar comparando el espectro de la señal original y la suavizada
plot(fft.a, type="l", xlim = c(0, 100), ylim = c(0, 1000))
lines(fft.h3.a, col="red")
