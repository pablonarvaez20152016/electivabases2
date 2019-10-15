#read data set
df <- read.csv("./datos/adult.data", header=FALSE)

columnas<-c('edad','clase_de_trabajo','cantidad_personas','educacion',
            'anios_de_estudio','estado_civil','ocupacion','relaciones','raza',
            'sexo','ganancia_capital','perdida_de_capital','horas_por_semana','pais','ganancia')


colnumericas<-c('edad','cantidad_personas','anios_de_estudio','ganancia_capital','perdida_de_capital','horas_por_semana')

colnames(df)<-columnas
######################################################################
dim(df) # Número de filas y de columnas del conjunto de datos.
(table(df$ganancia))

#datos faltantes
anyNA(df)
complete.cases(df)
#armar tablas table 
table(complete.cases(df))

#Con la función summary() podemos obtener los estadísticos descriptivos básicos para todas las variables (columnas) de nuestra matriz de datos.
View(summary(df))
######################################################################
mean(df$edad)  #media
median(df$edad) #mediana
quantile(df$edad) #quartiles
var(df$edad) #varianzas
#####################################################################
library(ggplot2)
hist(df$edad)  # Histograma
#ggplot(df, aes(x=edad)) + geom_histogram()
barplot(table(df$ganancia))
pie(table(df$ganancia))

boxplot(df$edad)
boxplot(df$horas_por_semana)

p <- ggplot(df, aes(x=ganancia, y=horas_por_semana)) + 
  geom_boxplot()
p
#detectar outlier
outlier<-function(a){

  me<-median(a)
  
  q1<-quantile(a, c(0.25, 0.5, 0.75), type = 1)[1]
  q2<-quantile(a, c(0.25, 0.5, 0.75), type = 1)[2]
  q3<-quantile(a, c(0.25, 0.5, 0.75), type = 1)[3]
  di<-q3-q1
  
  bigSup<-q3+(1.5*di)
  bigInf<-q1-(1.5*di)
  
  outSup<-q3+(3*di)
  outInf<-q1-(3*di)
  
  return(cbind(bigInf,bigSup,outInf,outSup))
}
outlierDetec=outlier(df$edad)

######################################################
#multivariado
#Una manera gráfica de abordar las relaciones entre variables es utilizando una matriz de scatterplots:
View(cov(df[colnumericas]))
View(cor(df[colnumericas]))

#estandarizar por variable   #zi=(x-mean(x))/sd(x)
df_standar=scaled.dat <- scale(df[colnumericas]) 
View(cov(df_standar))
########################################################
#dummies
library(dummies)
library(kableExtra)


df_cat<-df[,!(names(df) %in% colnumericas)]
df_dummies<-dummy.data.frame(df)
df_genero<-dummy.data.frame(df,names = 'sexo')
#######################################################
library(corrgram)

corrgram(df[colnumericas], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="dispersión")
  
