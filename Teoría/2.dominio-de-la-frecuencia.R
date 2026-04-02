######################### Generacion de senoidales #########################


N = length(tiempo)
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
# devuelve nums real + imagnaria en notacion cartesiana (a+bi)
fft.seno1 = fft(seno1)
print(fft.seno1)
print(Mod(fft.seno1)) # devuelve el modulo


# Ya no hay tiempo, tenemos la amplitud de la señal (y) en la frecuencia (x)
plot(Mod(fft.seno1), type='l')










