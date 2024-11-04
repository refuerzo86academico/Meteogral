#Hacer un programa que le pida al observador: el NOMBRE de cada estación meteorológica,
#la ALTURA sobre el nivel del mar (en metros) y que luego imprima un mensaje que
#clasifique la estación según su altitud, a saber:
#- Cercanas a nivel del mar: estaciones ubicadas a una altitud menor a 200m
#- Terreno elevado: estaciones ubicadas entre una altura de 200m y 1000m
#- Zona montañosa: estaciones ubicadas entre una altura de 1000m y 4000m
#- Difícil acceso: estaciones ubicadas a una altura mayor a 4000m


rm(list=ls()) 

#pido los datos por pantalla
estaciones<- c()
alturas <- c()
for (i in 1:5){
  estacion<- readline("ingrese nombre de la estación")
  altura <- as.numeric(readline("ingrese altura de la estación en metros"))
  #verifico la entrada de altura
  repeat{
    if (altura <0 | is.na(altura)){
      msj1<- "vuelva a ingresar la altura"
      print(msj1)
      altura <- as.numeric(readline("ingrese altura de la estación en metros"))
      alturas[i]<- altura
    }else{
      msj1<-"puede continuar"
      print(msj1)
      alturas[i]<- altura
      break
    }
  }
  estaciones[i]<-estacion
}

#armo el if con buenas practicas

for (j in 1:5){
  
  if (alturas[j]>=0 & alturas[j] <200){
    msj<- paste("la estacion", estaciones[j] , "que se encuentra a", alturas[j], "m se clasifica como zona cercana al nivel del mar")
    print(msj)
  }  else if (alturas[j]>=200 & alturas[j] <1000) {
    msj<- paste("la estacion", estaciones[j] , "que se encuentra a", alturas[j], "m se clasifica como zona de terreno elevado")
    print(msj)
  }  else if (alturas[j]>=1000 & alturas[j] <4000) {
    msj<- paste("la estacion", estaciones[j] , "que se encuentra a", alturas[j], "m se clasifica como zona montañosa")
    print(msj)
  }  else if (alturas[j]>=4000) {
    msj<- paste("la estacion", estaciones[j] , "que se encuentra a", alturas[j], "m se clasifica como zona de dificil acceso")
    print(msj)
  } else {
    msj<- "puede que haya ingresado mal los datos de altura"
    print(msj)
    
  }
}
