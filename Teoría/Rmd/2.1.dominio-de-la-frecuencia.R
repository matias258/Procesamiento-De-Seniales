######################### Generacion de senoidales #########################


N = 512
tiempo = 0:(N-1)


# senoidales
ciclos = 1
amplitud = 1
seno1= amplitud*sin(ciclos*2*pi*tiempo/N)
plot(tiempo, seno1, type='l')

ciclos = 2
amplitud = 1
seno2 = amplitud*sin(ciclos*2*pi*tiempo/N)
plot(tiempo, seno2, type='l')

ciclos = 10
amplitud = 2
seno10= amplitud*sin(ciclos*2*pi*tiempo/N)
plot(tiempo, seno10, type='l')

ciclos = 20
amplitud = 0.5
seno20= amplitud*sin(ciclos*2*pi*tiempo/N)
plot(tiempo, seno20, type='l')

# nivel de continua. Osea repite N veces el numero 1
niveldc = rep(1, N)
plot(tiempo, niveldc, type= 'l')


# Sumamos todas las señales que estuvimos generando
seno121020dc = seno1 + seno2 + seno10 + seno20 + niveldc
plot(tiempo, seno121020dc, type='l')


# Fast Fourier Transformation (fft)
# Pasamos del dominio del tiempo a la frecuencia
# devuelve nums real + imagnaria en notacion cartesiana (a+bi)
fft.seno1 = fft(seno1)
print(fft.seno1)
print(Mod(fft.seno1)) # devuelve el modulo


# Ya no hay tiempo, tenemos la amplitud de la señal (y) en la frecuencia (x)
plot(Mod(fft.seno1), type='l')
# frecuencias negativas: <- 0
# frecuencias positivas: 0 ->
# Es decir que los valores del -1 al 1 estan realmente en el "medio" 
# entre frecuencia negariva y positiva

# La parte real de la tf de la tf inversa del seno1
seno1a = Re(fft(fft.seno1, inverse = TRUE) / N)
# Volvemos del dominio de la frecuencia al tiempo

plot(tiempo, seno1a, type ='l')



# Vamos a hacer los mismo con seno10
fft.seno10 = fft(seno10)
print(fft.seno10)
print(Mod(fft.seno10))

# Vemos que ahora tenemos la componente 10 y -10, se corrió
plot(Mod(fft.seno10), type = 'l')

fft.seno10 = fft(seno10)
lines(Mod(fft.seno10), col = 'red')


# Vamos a superponerlo con el seno20
fft.seno20 = fft(seno20)
lines(Mod(fft.seno20), col = 'blue')


# Vamos a superponerlo con el niveldc
niveldc = fft(niveldc)
lines(Mod(niveldc), col = 'green')

# Ahora con la completa
fft.seno121020dc = fft(seno121020dc)
plot(Mod(fft.seno121020dc), type = 'l')

# La ttf esta expresando la contribucion de cada tono (color). 




###### Remover la componente de frecuencia cero (DC) ###########

fft.seno121020dc[1] = 0         # pisamos la freq 0 (la primera linea) con un 0  
plot(Mod(fft.seno121020dc), type = 'l')

# Antitransformada de fourier de la anterior
seno121020dc2 = fft(fft.seno121020dc, inverse = TRUE) / N
plot(tiempo, Re(seno121020dc2), type = 'l')

# Le superponemos la señal original a la antitransformada
plot(tiempo, seno121020dc, type = 'l', ylim = c(-4, 5))
lines(tiempo, seno121020dc2)
abline(h = 1, lty = 2)
abline(h = 0, lty = 2)
# Se corrio 1 para abajo porque el nivel de DC es 0 (le filtramos el nivel de continua)




