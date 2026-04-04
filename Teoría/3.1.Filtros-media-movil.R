## Generacion de una linea de tiempo y una serie temporal + ruido uniforme

N = 256
tiempo = 0:(N-1)

ciclos = 2
amplitud = 1
seno2 = amplitud*sin(ciclos*2*pi*tiempo/N)

nivelruidounif = runif(N, min=-0.5, max=0.5)
op <- par(mfrow = c(1,2))
plot(tiempo, nivelruidounif, type = 'l')
hist(nivelruidounif)


# La señar sinoidal y el ruido
op <- par(mfrow = c(1,1))
plot(tiempo, seno2, type = 'l')
lines(tiempo, nivelruidounif)

# Generamos una señal combinada
seno2ruidounif = seno2 + nivelruidounif
plot(tiempo, seno2ruidounif, type = 'l')


# Filtrado MA de 3 a 25 taps
seno2ruidounif.MA3 = filter(seno2ruidounif, rep(1/3, 3), circular = TRUE)
lines(tiempo, seno2ruidounif.MA3, col = 'red')

seno2ruidounif.MA7 = filter(seno2ruidounif, rep(1/7, 7), circular = TRUE)
lines(tiempo, seno2ruidounif.MA7, col = 'blue')

seno2ruidounif.MA15 = filter(seno2ruidounif, rep(1/15, 15), circular = TRUE)
lines(tiempo, seno2ruidounif.MA15, col = 'green')

seno2ruidounif.MA25 = filter(seno2ruidounif, rep(1/25, 25), circular = TRUE)
lines(tiempo, seno2ruidounif.MA25, col = 'yellow')
# Nos muestra la tendencia de una señal


# Filtrado MA de 3 a 25 taps en el dominio de la frecuencia
# Calculamos entonces la transformada de Fourier
fft.seno2ruidounif = fft(seno2ruidounif)
fft.seno2ruidounif.MA3 = fft(seno2ruidounif.MA3)
fft.seno2ruidounif.MA7 = fft(seno2ruidounif.MA7)
fft.seno2ruidounif.MA15 = fft(seno2ruidounif.MA15)f
fft.seno2ruidounif.MA25 = fft(seno2ruidounif.MA25)

plot(tiempo, Mod(fft.seno2ruidounif), type = 'l')
lines(tiempo, Mod(fft.seno2ruidounif.MA3), col = 'red')
lines(tiempo, Mod(fft.seno2ruidounif.MA7), col = 'blue')
lines(tiempo, Mod(fft.seno2ruidounif.MA15), col = 'green')
lines(tiempo, Mod(fft.seno2ruidounif.MA25), col = 'yellow')
# Vemos que la señal original tiene componentes de ruido a lo largo de
# todo el espectro
# En freq 2 tiene el impulso que coresponde al seno sinoidal de 2 ciclos

# A medida que sube nuestro MA, desaparece cada vez mas el ruido 


# Respuesta en frecuencia de los MA
ma3 = rep(0,N)
ma3[1:3] = 1/3

ma7 = rep(0,N)
ma7[1:7] = 1/7

ma15 = rep(0,N)
ma15[1:15] = 1/15

ma25 = rep(0,N)
ma25[1:25] = 1/25



fft.ma3 = fft(ma3)
fft.ma7 = fft(ma7)
fft.ma15 = fft(ma15)
fft.ma25 = fft(ma25)

plot(tiempo, Mod(fft.ma3), type='l', color = 'red', xlim = c(0, N/2))
lines(tiempo, Mod(fft.ma7), col = 'blue')
lines(tiempo, Mod(fft.ma15), col = 'green')
lines(tiempo, Mod(fft.ma25), col = 'yellow')
abline(h=0.7, lty = 2) # freq de corte
# Vemos como a medida que se va haciendo mas ancho el pulso,
# las freq de corte bajan
# Efecto pasabajos (buscar): deja pasar las bajas frecuencias 




