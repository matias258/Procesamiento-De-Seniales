# Convolucion en el DT

N = 256
tiempo = 0:(N - 1)

# Convolucion de dos series de tiempo en el DT
# f1 pulso
f1 = rep(0, N)
f1[100:200] = 1 

# f2 exponencial decreciente
f2 = 1 * exp(-0.03 * tiempo)

# Configuración de gráficos y visualización
op <- par(mfrow = c(1, 2))
plot(tiempo, f1, type = 'l')
plot(tiempo, f2, type = 'l')


# algoritmo de convolucion en el dominio del tiempo #################
f3 = rep(NA, N)

for(i in 1:N)
{
  suma = 0
  j = 1
  while(j <= i)
  {
    suma = suma + f1[j] * f2[i - j + 1]
    j = j + 1
  }
  f3[i] = suma
  print(i)
}

plot(tiempo, f3, type = 'l', col = 'red')

# convolucion en el DF ################################
fft.f1 = fft(f1)
fft.f2 = fft(f2)

op <- par(mfrow = c(1, 2))
plot(Mod(fft.f1), type = 'l')
plot(Mod(fft.f2), type = 'l')

fft.f3 = fft.f1 * fft.f2
plot(Mod(fft.f3), type = 'l')

# Transformada de Fourier Inversa
f3b = Re(fft(fft.f3, inverse = TRUE) / N)
plot(tiempo, f3b, type = 'l')
lines(tiempo, f3, col = 'red')

# Convolucion con convolve
f1f2 = convolve(f1, f2, conj = FALSE)
plot(tiempo, f1f2, type = 'l')
# Interamente el Convolve sta usando la Transformada de Fourier


# Correlacion cruzada con correlacion
f1f2 = convolve(f1,f2,conj = TRUE)
plot(tiempo, f1f2, type = 'l')



# Correlación cruzada de dos pulsos
#p5d5: pulso de 5 muestras con un delay de 5 muestras
p5d5 = rep(0, N)
p5d5[5:10] = 1

p5d25 = rep(0, N)
p5d25[25:30] = 1

plot(tiempo, p5d5, type = 'l', ylab = 'p5d5 & p5d25')
lines(tiempo, p5d25, col = 'red')

# Cross correlation function
ccf(p5d5, p5d25, lag.max = 50)


