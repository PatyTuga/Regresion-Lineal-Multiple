#__2.__ Genere un archivo rlm.R, de tal forma que le permita responder las siguientes preguntas:
# __2.1__ Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.

library(readxl)
data1 <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE,na = "")
str(data1)
View(data1)
names(data1)
summary(data1)
n1<-dim(data1)
n1

data2 <- read_excel("poblacion2.xlsx",sheet = 1,col_names = TRUE,na = "")
str(data2)
View(data2)
names(data2)
summary(data2)
n2<-dim(data2)
n1
#__2.2__ Una los archivos leídos en un mismo objeto llamado poblacion.
# Union de bases de datos
poblacion <- merge(data1, data2, by = "identificador", suffixes = c("","")) 
str(poblacion)
View(poblacion)
names(poblacion)
summary(poblacion)

#  __2.3__ Cree un código que identifique la clase de cada variable y genere diagramas 
#de cajas para variables continuas y diagramas de barras para variables discretas


tipos <- numeric(ncol(poblacion))
clase <- numeric(ncol(poblacion))
for (j in 1:ncol(poblacion)){
  tipos[j] <- typeof(poblacion[,j])
  clase[j] <- class(poblacion[,j])
}
tipos
clase

library(ggplot2)

for(j in 2:dim(poblacion)[2])
{ 
  if(is.numeric(poblacion[,j])==TRUE)
    { 
    print(names(poblacion)[j])
    boxplot(poblacion[,j],col="lightcoral") 
    }

  else 
    {
    #if(is.factor(poblacion[,j])==TRUE){
      print(names(poblacion)[j])
    barplot(table(poblacion[,j]),col="lightblue")
    }
}

#__2.4__ Cree un código que calcule automáticamente el mínimo, media, máximo, desviación estándar, 
#primer cuartil de cada variable numérica y la frecuencia en el caso de va-riables categóricas.

for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    
    print(names(poblacion)[j])
    print(class(poblacion[,j]))
    
    print(max(poblacion[,j]))
    print(min(poblacion[,j]))
    print(mean(poblacion[,j]))
    print(sd(poblacion[,j]))
    print(quantile(poblacion[,j],probs=seq(0,1,0.25),na.rm = FALSE))
  }else {
    
    print(names(poblacion)[j])
    print(class(poblacion[,j]))
    print(table(poblacion[,j])/dim(poblacion)[1])
  }
  
}

#__2.5__ Calcule la correlación entre la variable dependiente poblacion y cada
#una de las variables explicativas (numéricas).

for(i in 3:dim(poblacion)[2]){
  if(is.numeric(poblacion[,i])==TRUE){
    correlacion<-cor(poblacion[,2],poblacion[,i])
    print(names(poblacion)[i])
    print(correlacion)
  }
}

# __2.6__ Considere la variable categórica serv.bas.compl con una confiabilidad del 90%, 
#¿ Puede asumirse que la media de la variable poblacion en el grupo serv.bas.compl: SI es distinta a la media del grupo serv.bas.compl: NO?. 
#Utilice la función: t.test
#Primero vamos a modificar la data, para transformar a "factor las columnas region y serv.bas..." para poder usar el test t(student)
servicios<-factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("si","no"))
region<-factor(poblacion[,"region"],levels=c("A","B"),labels=c("a","b"))
poblacion_fac<-data.frame(poblacion[,1:7],region,servicios)

#Luego hacemos un grafico de cajas para observar como se comporta la "poblacion" en los dos grupos
plot(poblacion ~ servicios , data = poblacion_fac, col="lightgreen") 
#Y finalmente realizamos la prueba de hipotesis para la diferencia de medias
t.test(poblacion ~ servicios , data = poblacion_fac, conf.level=0.9)

# __2.7__ Considerando los cálculos anteriores genere el modelo de regresión lineal múltiple que mejor se ajuste a los datos.
#Interprete los coeficientes obtenidos.

regresion<-lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen,data=poblacion)
summary(regresion)
#-----------------------------------------------------------------------------------------------------
clase<-sapply(poblacion,class)#a cada elemento de esta lista (a cada columna)aplique la función clase
# es un vector caracter
datanum<-poblacion[,clase=="numeric"]
str(datanum)
View(datanum)
datanum1<-datanum[-1]
View(datanum1)

#trabaja con todas las variables
reg0<-lm(poblacion~.,datanum1)
summary(reg0)

#seleciona el modelo que mejor se ajusta
reg<-step(reg0,direction = "backward")
#------------------------------------------------------------------------------------------------------

#__2.8__ Interprete el $R^2$.
summary(regresion)["r.squared"]


#__2.9__ Analice la significancia de la regresión y de cada uno de los parámetros individuales.
anova<-aov(regresion)
summary(anova)

#almacenamos los residuos en un vector
residuos<-1:40  
for(i in 1:40){
  residuos[i]<-summary(regresion)[["residuals"]][i]
}
#grafica poblacion vs residuos
plot(poblacion[,"poblacion"],residuos, col="yellow4")
#histograma de residuos
hist(residuos,col="orange")
#Comparando con la distribucion normal teorica
qqnorm(residuos, col="blue")
qqline(residuos,col="red")

