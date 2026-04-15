# Generacion de una linea de tiempo y series temporales

N = 256
tiempo = 0:(N-1)

ciclos = 2
amplitud = 1
seno2 = amplitud*sin(ciclos*2*pi*tiempo/N)

ciclos = 10
amplitud = 1
seno10 = amplitud*sin(ciclos*2*pi*tiempo/N)

ciclos = 20
amplitud = 2
seno20 = amplitud*sin(ciclos*2*pi*tiempo/N)

niveldc = rep(1, N)
nivelruido = rnorm(N, mean = 0, sd = 0.5)

plot(tiempo, seno20, type ='l')
lines(tiempo, seno2)
lines(tiempo, seno10)
lines(tiempo, niveldc)
lines(tiempo, nivelruido)


# Combinamos las componentes
seno21020dcruido = seno2+ seno10 + seno20 + niveldc + nivelruido
fft.seno21020dcruido = fft(seno21020dcruido)

op <- par(mfrow = c(1,2))
plot(tiempo, seno21020dcruido, type = 'l')
plot(Mod(fft.seno21020dcruido), type = 'l')

# Obs: el ruido son los picos irregulares (Lo que esta mas en negrita)
#      piquito solo -> frecuencias altas


# En el dominio de la freq, vemos que todo lo del medio (que esta re bajo) es ruido.
# El pico que esta del lado izq, que no esta del lado der, es la freq 0.
# Ya podemos decir que hay una componente de freq 0, osea constante
# Vemos 3 picos -> son 3 tonos puros (con los 3 negativos), son las 3 sinoidales.
# Toda esta info se puede usar para diseñar el filtro de acuerdo a nuestra necesidad


# filtrado en el dominio de la frqcuencia de la serie temporal combinada
# en este caso vamos a usar un filtro pasa-altos
filtroDF = rep(1, N)
banda.filtro = 15 # Seria la frecuencia de filtro (fc = 15; -fc = -15)
filtroDF[1:banda.filtro] = 0
filtroDF[(N-2-banda.filtro):N] = 0
fft.seno21020dcruidoDF = filtroDF * fft.seno21020dcruido # la señal filtrada

op <- par(mfrow = c(1,2))
plot(tiempo, filtroDF, type = 'l')
plot(Mod(fft.seno21020dcruidoDF), type = 'l')

# Por ejemplo en este caso filtramos todo el ruido (lo del medio), quedandonos con los picos de los costados





# Transformada inversa para volver al dominio del tiempo

seno21020dcruidoDF = Re(fft(fft.seno21020dcruidoDF, inverse = TRUE)/N)
plot(tiempo, seno21020dcruidoDF, type = 'l')
lines(tiempo, seno20, col='red')

# Vemos que saco el ruido de baja freq y las componentes de baja freq.
# Fitlro a la señal poniendo de manifiesto lo que nosotros queriamos.



