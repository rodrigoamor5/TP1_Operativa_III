#Libraries
library(tidyverse)
library(lubridate)
library(stringi)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(fitdistrplus)
library(fitdistrplus)
library(MASS)

# Clean memory
rm(list=ls())
# Read data in csv format
df = read.csv("~/FIUBA/1C2023/Operativa III/TP1/Codigo/TP1_datalimpia.csv", header = T, sep = ";", dec = ".")
head(df)

# Basic scatter plot
ggplot(df, aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point()

# Calculate the z-scores for x and y columns
df$z_x <- abs(scale(df$Tiempo_de_Carga_.min.))
df$z_y <- abs(scale(df$Tiempo_de_Entrega_.min.))

# Remove the rows where z-score is greater than 3
df1 <- df[df$z_x < 3 & df$z_y < 3,]

# Basic scatter plot
p <- ggplot(df1, aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point()
print(p)

# with marginal histogram
ggMarginal(p, type="histogram")

# marginal density
ggMarginal(p, type="density")

#For each city 
ggplot(filter(df1, Destino != "NA"), aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min.))  + geom_point() + facet_wrap(~Destino)
#For each day 
ggplot(filter(df1, Dia_Semana != "NA"), aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point() + facet_wrap(~Dia_Semana)
#For each day and city
ggplot(filter(df1, Dia_Semana != "NA", Destino != "NA"), aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point() + facet_grid(Destino~Dia_Semana)
#For each hour 
ggplot(filter(df1, Hora_Ingreso_Pedido != "NA"), aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point() + facet_wrap(~Hora_Ingreso_Pedido)
#For each hour and city
ggplot(filter(df1, Hora_Ingreso_Pedido != "NA", Destino != "NA"), aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point() + facet_grid(Destino~Hora_Ingreso_Pedido)

#For each hour and day
ggplot(filter(df1, Hora_Ingreso_Pedido != "NA", Dia_Semana != "NA"), aes(x=Tiempo_de_Carga_.min., y=Tiempo_de_Entrega_.min., color = Destino))  + geom_point() + facet_grid(Dia_Semana~Hora_Ingreso_Pedido)

##CONCLUSIÓN: no existe relación entre el tiempo de carga y el tiempo de descarga

#Ahora comienzo a analizar el tiempo de descarga y ver si varía por ciudad, dia, hora o tn pedida

#Ciudad
# Basic scatter plot
ggplot(filter(df, Destino != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Destino))  + geom_point()
# Remove the rows where values are way too high or way too low
df2 <- df[df$Tiempo_de_Entrega_.min. < 100 & df$Tiempo_de_Entrega_.min. > 10,]
ggplot(filter(df2, Destino != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Destino))  + geom_point()

#Dia
ggplot(filter(df, Dia_Semana != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Dia_Semana))  + geom_point()
# Remove outliers
df3 <- df[df$Tiempo_de_Entrega_.min. < 100 & df$Tiempo_de_Entrega_.min. > 10,]
ggplot(filter(df3, Dia_Semana != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Dia_Semana))  + geom_point()

#Hora
ggplot(filter(df, Hora_Ingreso_Pedido != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Hora_Ingreso_Pedido))  + geom_point()
# Remove ouliers 
df4 <- df[df$Tiempo_de_Entrega_.min. < 100 & df$Tiempo_de_Entrega_.min. > 10,]
ggplot(filter(df4, Hora_Ingreso_Pedido != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Hora_Ingreso_Pedido))  + geom_point()

#Tn
ggplot(filter(df, Toneladas_Pedidas != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Toneladas_Pedidas))  + geom_point()
# Remove outliers
df5 <- df[df$Tiempo_de_Entrega_.min. < 100 & df$Tiempo_de_Entrega_.min. > 10 & df$Tons_OK == "True",]
ggplot(filter(df5, Toneladas_Pedidas != "NA"), aes(x=Tiempo_de_Entrega_.min., y=Toneladas_Pedidas))  + geom_point()

##CONCLUSIÓN: el tiempo de entrega es el mismo para todos los pedidos

#Busco la distribución del tiempo de entrega

ggplot(df5, aes(x=Tiempo_de_Entrega_.min.)) + geom_histogram()
hist(df5$Tiempo_de_Entrega_.min.)
entrega <- df5$Tiempo_de_Entrega_.min.

#Use the descdist() function to obtain a list of candidate distributions to fit the data:
entrega = as.vector(entrega);
entrega = entrega[(entrega < 1000) & (entrega > 0)]
entrega = entrega[!is.na(entrega)]
is.finite(entrega)
is.vector(entrega)

hist(entrega)

fitdist_results <- descdist(entrega, discrete = TRUE )

fitdist_results

fit <- fitdist(entrega, distr = 'pois',method = 'mle')
fit <- fitdist(entrega, distr = 'pois',method = 'mme')
fit <- fitdist(entrega, distr = 'pois',method = 'mse')

fit

fitr <- fitdistr(entrega, densfun = 'poisson')

fitr$loglik
  