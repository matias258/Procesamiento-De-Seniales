################ Teorema de Parseval ################

N = 512
tiempo = 0:(N-1)

s2 = sin(2*2*pi*tiempo/N)

E.dt = sum(s2*s2)
print(paste('ENergia DT = ', E.dt))

fft.s2 = fft(s2)
E.df = sum(Mod(fft.s2)*Mod(fft.s2))/N
print(paste('Energia DF = ', E.df))


