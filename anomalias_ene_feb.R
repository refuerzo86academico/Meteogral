
#El archivo t media EF.RData contiene un array de tipo lista con la
#temperatura media del mes de Enero y Febrero desde 1982 a 2012 para 5 
#estaciones argentinas (usar la sentencia load(“t media EF.RData”) para abrirlo). 

#La primera columna del array contiene el nombre de cada estacion, la segunda 
#contiene la serie para el mes de enero y la tercera para el mes de febrero. 

#Cada fila del array esta asociada a una estacion:

rm(list=ls())
dir1<- "C:/Users/cpu/Desktop/ARCHIVOS/Repositorios/Meteogral"
setwd(dir1)
getwd()
load("t_media_EF.RData")

#Osea tengo 5 estaciones, 30 años, 2 meses= 300 datos

colnames(estaciones)<-c("Estaciones", "Enero", "Febrero")
rownames(estaciones)<-c("1", "2", "3", "4", "5" )

#a) Para cada estacion: Obtener la anomalıa mensual maxima de enero y el anio
#en que se alcanzo. Almacenarlo en un data frame donde la primer variable 
#sea el nombre de cada estacion, la segunda el anio de la anomalia y la tercera
#dicho valor.

#Genero los vectores vacios que voy a necesitar para el ciclo
nombre<-c()
anomalias.max.anio<-c()
anomalias.max.valor<-c()
a<-0

for (i in 1:5) {
  estacion<-sapply(estaciones[i,2:3], mean)       #Pido la media para las columnas 2 y 3 (ene y feb), en todas las filas (estaciones)
  anomalia.enero<-(estaciones[[i,2]]-estacion[1]) #Me quedo con la columna 1 (ene), escribiendo estacion[1]
  posicion<-which.max(abs(anomalia.enero))        #Busco el maximo de anomalia de enero
  anomalias.max.anio[i]<-1981+posicion            #Busco el anio de esa anomalia max
  nombre[i]<-estaciones[[i,1]]                    #Voy llenando los nombres de las estaciones
  a<-anomalias.max.valor[i]<-(anomalia.enero[posicion]) #Busco el valor de la posicion del max
  print(paste("Para la estacion",nombre[i],"la anomalia mensual maxima de enero se alcanzo en el anio",anomalias.max.anio[i],"con un valor de",a))
}
est<-data.frame(nombre,anomalias.max.anio,anomalias.max.valor)
est

#b) Para cada estacion: Calcular la media y el desvıo estandard para el mes de
#febrero. Imprimir luego un cartel que seale para cada estacion en que los anios la
#temperatura estuvo por encima de la media mas un desvıo standar.

media<-sapply(estaciones[,3], mean) #Me quedo en la columna 3 que corresponde a feb
desvio<-sapply(estaciones[,3], sd)

anio <- 0

#Con este ciclo le pido imprimirme en oracion los resultado

for (i in 1:5){ #las 5 estaciones
  for (j in 1:31) { #los datos de febrero
    if (estaciones[[i,3]][j]>(media[i]+desvio[i])) {
      anio <- 1981+j #desde 1981 en adelante (1982,1983,etc)
      print(paste("Para la estacion",estaciones[i,1],"la temperatura por encima de la media mas un desvio estandar se alcanzo en el anio",anio))
    }
  }
}
