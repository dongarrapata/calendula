library(lubridate)
library(ggplot2)
fecha <- as_datetime(Sys.Date())
N <- 365
fechas <- rep(fecha, N)+(ddays()*(1:N))
primeros <- which(day(fechas)==1)
quinces <- which(day(fechas)==15)
ultimos <- primeros[which(primeros != 1)] - 1
movimientos <- rep(0, N)
saldo <- rep(0, N)
renta <- 6000
comida <- 2000
gimnasio <- 1865
nutriologa <- 1500
transporte <- 1000
gastoFijoMensual <- renta + comida + gimnasio + nutriologa + transporte
movimientos[primeros] <- -gastoFijoMensual
movimientos[quinces] <- 17500
movimientos[ultimos] <- 17500
for (i in 2:N) {
  saldo[i] <- saldo[i-1] + movimientos[i]
}
df <- data.frame(fecha = fechas, movimientos = movimientos, saldo = saldo)
plot <- ggplot(df, aes(x = fechas, y = saldo)) + geom_line()
df$saldo[N]
