################## Procesamiento Tiempo-Frecuencia ########################
library(wavethresh)

N = 512
tiempo = 0:(N-1)

# Señales sinoidales
sA = sin(4*2*pi*tiempo/N)
sB = sin(24*2*pi*tiempo/N)

# La primera mitad de sA va a estar en 0
sA[1:(N/2)] = 0
# La segunda mitad de sB va a estar en 0
sB[(N/2):N] = 0


plot(tiempo, sA, type = 'l', ylab = 'sA')
plot(tiempo, sB, type = 'l', ylab = 'sB')


# si nosotros aplicamos la transf de fourier aca, vaa funcionar
# Pero no nos va a decir en que momento empezo a desarrollarse la sinoidal, en el caso de sA
# O en que momento cesó, en el caso de sB
# Solo nos va a dar los valores de las sinoidales
# Con la transf de fourier se pierde la nocion del tiempo

#En ete caso, la señal combinada seria la siguiente:
sAB = sA + sB
plot(tiempo, sAB, type = 'l', ylab = 'sAB')
# Notemos que la señal es estacionaria (decir por que)

# Analsis tiempo-frecuencia: serie de tiempo combinada
# Usamos Wavelet Decomposition de 4 muestras
# Usamos un algoritmo piramidal de Mallot, termina cuando tenemos una sola muestra

# sA
wd.sA = wd(sA, filter.number = 4, family = 'DaubExPhase')
plot(wd.sA)
# Vemos como en la resolucion/banda 2, tiene una cierta magnitud en la señal que esta en la segunda mitad. Corrsppnode a baja frecuencia
# A medida que la resolucion va bajando, son frecuencias mas bajas

# sB
wd.sB = wd(sB, filter.number = 4, family = 'DaubExPhase')
plot(wd.sB)
# Vemos que resolucion alta es la primera mitad
# Y en la 2da mitad va apsando frecuencias mas bajas

# Combinada
wd.sAB = wd(sAB, filter.number = 4, family = 'DaubExPhase')
plot(wd.sAB)
# Vemos en la primera mitad valores en la resolucion 5 y 6, alta freucneica
# Y en la otra mitad tenemos valores en la resolucion 3 y 2, de baja frecuencia
# Se corresponde con la combinacion de sA y sB

# Entonces esta descomposicion nos da una idea temporal y frecuencial
# Sabemos en que momento del tiempo se desarrolla cada una
# Perdemos resolucion espectral pero ganamos algo de resolucion temporal

# Veamos paso a paso el algoritmo
# Vemos el acceso de la señal que pasa a la proxima etapa (C)
# Y vemos (D), el acceso de la señal Detalle
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 8)) 
plot.ts(accessD(wd.sAB, level = 8))
# Nivel 8, cuando empieza todo y D el primer Detalle

# Vemos lo mismo para los sigueintes, la siguiente etapa (C)
# Y el 2do Detalle (D)
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 7)) 
plot.ts(accessD(wd.sAB, level = 7))


# Ya en el Detalle que sigue, aparece desdibujada la frecuencia
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 6)) 
plot.ts(accessD(wd.sAB, level = 6))



# En el nivel 5 sigue desdibujandose la alta frecuencia
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 5)) 
plot.ts(accessD(wd.sAB, level = 5))


# En la 4 desaparece la alta frecuencia y empieza a aparecer la baja frecuencia
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 4)) 
plot.ts(accessD(wd.sAB, level = 4))



# Esta baja frecuencia aparece mucho mejor en la 3
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 3)) 
plot.ts(accessD(wd.sAB, level = 3))



# En el nivel 2 ya son muy pocas muestras,y ya vamos hacia la componente constante
par(mfrow = c(1,2))
plot.ts(accessC(wd.sAB, level = 2)) 
plot.ts(accessD(wd.sAB, level = 2))




