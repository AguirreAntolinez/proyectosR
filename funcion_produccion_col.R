#Import libraries
library(readxl)
library(plotly)
library(listviewer)
#Import dataset
df<-read_excel("PIB enfoque de los ingresos.xlsx")
View(PIB_enfoque_de_los_ingresos)
#Linealizar las variables
attach(df)
lnsalario<-log(`Remuneración de los asalariados`)
lncapital<-log(`Excedente de explotación bruto e ingreso mixto bruto`)
lnproduccion<-log(Produccion)
dflineal<-data.frame(lnsalario,lncapital,lnproduccion)
#Estimar los coeficientes de la función
attach(dflineal)
cdmodel<-lm(lnproduccion~lncapital+lnsalario)
summary(cdmodel)
#Graficar 
trabajo<-runif(10000, min=0, max=10000)
capital<-runif(10000, min=0, max=10000)
dfcd<-data.frame(trabajo,capital)
dfcd$y<-exp(0.655144)*dfcd$trabajo^0.372561*dfcd$capital^ 0.627860 #Los coeficientes salen de la regresión
attach(dfcd)
schema(jsonedit = interactive())
plot_ly(x=~capital, y=~trabajo, z= ~y, type='mesh3d')
