rm(list=ls())
dir1<- "C:/Users/cpu/Desktop/ARCHIVOS/Repositorios/Meteogral"
setwd(dir1)
getwd()
temperatura<-scan("datos_tmedia_SABE_2010.txt")
temperatura

#Me devuelve 365 datos datos diarios

#a) Dado que se trata de temperaturas medias diarias para la Ciudad de Buenos
#Aires, valores superiores a 40C son, muy probablemente, erroneos de acuerdo 
#con el comportamiento climatogico de esta variable.

#Diseniar y programar un algoritmo que identifique la posicion dentro 
#de la serie de los dıas donde el valor de la temperatura media supera
#los 40C, pero que ademas muestreel valor del dıa anterior y del dıa siguiente.

#Obtener la cantidad total de elementos erroneos.

#Armo un ciclo que recorra la cantidad de datos del archivo cargado. Si el valor
#es mayor a 40 grados entonces entra al FOR, el cual va a imprimir por pantalla
#el valor redondeado de ese día, el del siguiente y el anterior.

for (i in 1:365){
  if (temperatura[i]>40){
    print(paste("En el dia",i,"hay una T media de",temperatura[i],"°C"))
    print(paste("En el dia anterior a",i,"hay una T media de",(temperatura[i-1]),"°C"))
    print(paste("En el dia siguiente a",i,"hay una T media de",(temperatura[i+1]),"°C"))
  }
}

#Luego la cantidad de elementos erroneos seran

print(paste("Cantidad de datos erroneos:", length(which(temperatura>40))))

#b) Calcular el valor maximo y el mınimo de la serie teniendo en cuenta los
#puntos erroneos y sin tenerlos en cuenta. 

#Calcular la cantidad total de datos faltantes. Remover los elementos faltantes 
#y los erroneos de la serie de temperatura.

#Si tengo en cuenta los datos erroneos puedo aplicar a todo los datos completos
#la funcion max y min. Luego muestro el resultado

maximo<-max(temperatura)
minimo<-min(temperatura)

#Printeo redondeando el valor
print(paste("El valor maximo de temperatura corresponde a:",maximo,digits = 1,
            "°C el valor minimo corresponde a:",minimo,"°C"))


#Si no tengo en cuenta los datos erroneos, tengo que cambiar los faltantes y
#los mayores a 40

temperatura2<-replace(temperatura,temperatura==-999,NA) #Reemplazo aquellos que son =999 con NA
temperatura2<-replace(temperatura2,temperatura>40,NA) #Reemplazo aquellos que son >40 con NA
maximo2<-max(temperatura2,na.rm=T) #Aca saco el max diciendo que son TRUE los NA
minimo2<-min(temperatura2,na.rm=T) #Aca saco el min diciendo que son TRUE los NA

#Printeo redondeando el valor
print(paste("El valor maximo de temperatura corresponde a:",maximo2,
            "°C el valor minimo corresponde a:",minimo2,"°C"))

#Ahora me devuelve valores mas coherentes

#c) Ordenar la serie de menor a mayor y calcular su mediana.

#Ordeno con la funcion sort
temperatura3<-sort(temperatura2)

#Utilizo la funcion mediana de R
mediana3<-print(median(temperatura3))

#d) Generar una serie de medias semanales a partir de los datos medios diarios.

#Genero una matriz con los datos de temperatura2,para luego usar apply.
#Esta matriz tiene 7 filas (cada dia de la semana), y se necesita que haya 365 datos.
#Pero van a sobrar porque 365:7 no es entero. El mas proximo es 7x53=371

temperatura4<-matrix(temperatura2,7,53)

#Lleno de NA los lugares que sobran 371-365=6

for(i in 366:length(temperatura4)){ #Me muevo desde 366 hasta el final
  temperatura4[i]<-NA
}

#Aplico la media a la matriz, dejando fijo la dimension 2 y tomando validos los NA
media<-print(apply(temperatura4,2,mean,na.rm=T))

#Devuelve el valor medio para las 53 semanas

#e) Dividir el rango de la variable en N intervalos de igual longitud y calcular
#el numero de elementos de la serie que cae dentro de cada intervalo. Repetir 
#el ejercicio utilizando la funcion intrınseca “hist”.

#Primero veo el rango que contiene todos los datos usando la función range()
#en var3 (vector sin datos erróneos (+40) o faltantes (-999))

range(temperatura3,na.rm = TRUE) #Uso temperatura 3 que son los que estan ordenados

#El rango va de 5,4 a 30,28, elijo el rango de 5 a 35 y los divido de a 5
#Propongo los contadores y el ciclo con FOR que guarda los intervalos (i)
#uso i_n de intervalos e i el contador inicial

i=0
i1=0
i2=0
i3=0
i4=0
i5=0
i6=0

for(i in 1:length(temperatura3)){
  if (temperatura3[i]>=5 & temperatura3[i]<10){
    i1=i1+1
  }else if (temperatura3[i]>=10 & temperatura3[i]<15){
    i2=i2+1
  }else if (temperatura3[i]>=15 & temperatura3[i]<20){
    i3=i3+1
  }else if (temperatura3[i]>=20 & temperatura3[i]<25){
    i4=i4+1
  }else if (temperatura3[i]>=25 & temperatura3[i]<30){
    i5=i5+1
  }else if (temperatura3[i]>=30 & temperatura3[i]<35){
    i6=i6+1
  }
}
{print(paste0("Cantidad de valores entre 5°C y 10°C: ",i1))
  print(paste0("Cantidad de valores entre 10°C y 15°C: ",i2))
  print(paste0("Cantidad de valores entre 15°C y 20°C: ",i3))
  print(paste0("Cantidad de valores entre 20°C y 25°C: ",i4))
  print(paste0("Cantidad de valores entre 25°C y 30°C: ",i5))
  print(paste0("Cantidad de valores entre 30°C y 35°C: ",i6))
  print(paste0("Valores totales: ",i1+i2+i3+i4+i5+i6))
}

#Uso la funcion intrinseca hist genera un histograma, y grafica.

hist(temperatura3,main="Temperatura media",xlab="Temperatura",plot=T)

#Puedo observar que arma mas cantidad de intervalos que los propuestos

#############################################

#a) En base al programa anterior, desarrollar una funcion que reciba una serie
#de longitud N y entregue como resultado su media, valor maximo, valor mınimo 
#y desviacion estandar. Evaluar el desempeno de esta funcion con la serie 
#utilizada en el ejercicio anterior.

rm(list=ls())

#Se arma la función que calcula todos los datos

funcion_resumen <- function(serie) {
  media<-mean(serie)
  minimo<-min(serie)
  maximo<-max(serie)
  desvio<-sd(serie)
  return(list(media, minimo, maximo, desvio))
}

#Se piden los N valores para probar el programa, con la función scan

serie<-scan()
funcion_resumen(serie)

#Utilizo los datos del ejercicio 7, las temperaturas para probar el programa con
#mas volumen de datos

funcion_resumen(temperatura)

#b) Armar una funcion a la que se le ingrese una serie de longitud N y una 
#cantidad de intervalos I,y que calcule un histograma usando I intervalos
#iguales que abarquen la totalidad del rango de la serie.

funcion_histograma<-function(datos,intervalos){
  maximo<-max(datos,na.rm=T)
  minimo<-min(datos,na.rm=T)
  ancho<-(maximo-minimo)/intervalos
  grafico<-hist(datos,intervalos,plot = T)
  return(grafico)
}

funcion_histograma(temperatura3,6)