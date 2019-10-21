#read data set
df <- read.csv2("./datos/fifa.csv", row.names=1)
######################################################################
df<-df[,!(names(df) %in% c('id','player_api_id','player_name','player_fifa_api_id','birthday','player_api_id.1'))]
dim(df) # Número de filas y de columnas del conjunto de datos.
#datos faltantes
anyNA(df)
complete.cases(df)
#armar tablas table 
table(complete.cases(df))
sapply(df, function(x) sum(is.na(x)))
colSums(is.na(df))
colSums(is.na(df))
#omitir filas faltantes
df_1 <- na.omit(df)

#Imputación con la media
library(mice)
imputed_data <- mice(df[,names(df) %in% colnames(df)],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print=F)
complete.data <- mice::complete(imputed_data)
colSums(is.na(complete.data))

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
ggplot(df, aes(x=edad)) + geom_histogram()


boxplot(df$edad)
boxplot(df$finishing)

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
library(dummies)
df_dummies<-dummy.data.frame(complete.data)


View(cov(df_dummies))
View(cor(df_dummies))

#estandarizar por variable   #zi=(x-mean(x))/sd(x)
df_standar=scaled.dat <- scale(df[colnumericas]) 
View(cov(df_standar))

########################################################
library(corrgram)

corrgram(df_dummies, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="dispersión")

