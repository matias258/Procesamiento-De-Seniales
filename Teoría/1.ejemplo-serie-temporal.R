tiempo = seq(from = 0, to = 10, by = 0.1)
N = length(tiempo)

x = rnorm(N, mean=2, sd = 0.5)
print(paste('media =', mean(x)))
print(paste('DE =', sd(x)))

# Linea de tiempo
plot(tiempo, x, type = 'l')
abline(h = mean(x), lty=2)
abline(h = mean(x) + 2*sd(x), lty=3)
abline(h = mean(x) - 2*sd(x), lty=3)

# histogram
hx = hist(x)
abline(v = mean(x), lty = 2)
abline(v = mean(x) + 2*sd(x), lty=3)
abline(v = mean(x) - 2*sd(x), lty=3)

# boxplot
boxplot(x)
abline(h = mean(x), lty=2)
abline(h = mean(x) + 2*sd(x), lty=3)
abline(h = mean(x) - 2*sd(x), lty=3)

# distribucion normal
max.hist = max(hx$counts)
mu = mean(x)
sigma = sd(x)
ejex = seq(from = -min(x), to = max(x), by = 0.01)
fx.normal = (1/sqrt(2*pi*sigma))*exp((ejex-mu)*(ejex-mu)/(-2*sigma*sigma))
fx.normal = (max.hist/max(fx.normal))*fx.normal
plot(ejex, fx.normal, main = 'Distribucion normal', type = 'l')

# histograma + distribucion normal
hx = hist(x)
abline(v = mean(x), lty = 2)
abline(v = mean(x) + 2*sd(x), lty=3)
abline(v = mean(x) - 2*sd(x), lty=3)
lines(ejex, fx.normal, col = 'red')

# Guardar y leer una serie temporal
x.df = data.frame(t = tiempo, x = x)
dir = 'C:/Users/Matias/Desktop/procesamiento-de-señales/'
write.csv(x.df, paste(dir, 'xt.csv', sep = ''))
x.df2 = read.csv(paste(dir, 'xt.csv', sep = ''))

plot(x.df2$t, x.df2$x, type='l')




