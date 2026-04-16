##### Generacion de Pulsos ######
N = 512
tiempo = 0:(N-1)

# 1 impulso
p1 = rep(0, N)
p1[1] = 1
plot(tiempo, p1, type ='l')

# 2 impulsos
p2 = rep(0, N)
p2[1:2] = 1
plot(tiempo, p2, type ='l')

# 5 impulsos
p5 = rep(0, N)
p5[1:5] = 1
plot(tiempo, p5, type ='l')

# 10 impulsos
p10 = rep(0, N)
p10[1:10] = 1
plot(tiempo, p10, type ='l')

# 20 impulsos
p20 = rep(0, N)
p20[1:20] = 1
plot(tiempo, p20, type ='l')

# 50 impulsos
p50 = rep(0, N)
p50[1:50] = 1
plot(tiempo, p50, type ='l')


# La transf de fourier de p1
fft.p1 = fft(p1)
plot(Mod(fft.p1), type = 'l')
# Conclusion: Un impulso en un dominio del tiempo = todas las freq en un
# valor constante
# Osea un impulso contiene todas las frecuencias

# Hagamos fft para el resto
fft.p2 = fft(p2)
plot(Mod(fft.p2), type = 'l')

fft.p5 = fft(p5)
plot(Mod(fft.p5), type = 'l')

fft.p10 = fft(p10)
plot(Mod(fft.p10), type = 'l')

fft.p20 = fft(p20)
plot(Mod(fft.p20), type = 'l')

fft.p50 = fft(p50)
plot(Mod(fft.p50), type = 'l')


### FFT de Pulsos ###
plot(Mod(fft.p2), type = 'l', ylim = c(0,50), xlim = c(0,64))
lines(Mod(fft.p5))
lines(Mod(fft.p10))
lines(Mod(fft.p20))
lines(Mod(fft.p50))

### Normalizamos el FFT de Pulsos para que tengan la misma amplitud ###
plot(Mod(fft.p2)/max(Mod(fft.p2)), type = 'l', ylim = c(0,1), xlim = c(0,64))
lines(Mod(fft.p5)/max(Mod(fft.p5)))
lines(Mod(fft.p10)/max(Mod(fft.p10)))
lines(Mod(fft.p10)/max(Mod(fft.p20)))
lines(Mod(fft.p50)/max(Mod(fft.p50)))
abline(h = 0.7, lty = 2)
# A medida que el corte se acerca a 0, la frecuencia de corte es mayor.
# En nuestro caso, podemos ver que en la interseccion de la linea punteada
# Y el 1er corte, obtenemos fC50 (frecuencia de corte del p50)



### Calibrar el eje de frecuencias ###
DeltaT = 0.001 # segundos
FrecuenciaMuestreo = 1/DeltaT # Hertz
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo * tiempo

plot(Frecuencia, Mod(fft.p2)/max(Mod(fft.p2)), type = 'l', ylim = c(0,1),
      xlim = c(0,250), ylab = 'Mod FFT p2 a p50 (normalizado)', xlab = 'Frecuencia (Hz)')
lines(Frecuencia, Mod(fft.p5)/max(Mod(fft.p5)))
lines(Frecuencia, Mod(fft.p10)/max(Mod(fft.p10)))
lines(Frecuencia, Mod(fft.p20)/max(Mod(fft.p20)))
lines(Frecuencia, Mod(fft.p50)/max(Mod(fft.p50)))
abline(h=0.7, lty = 2)
  
  