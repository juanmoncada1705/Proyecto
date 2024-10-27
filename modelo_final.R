#Modelo de prediccion de precio de computadores.
#Predicción del precio de los computadores según la disposición del hardware 
install.packages("dplyr")
install.packages("ggplot1")
install.packages("car")
install.packages("lmtest")
install.packages("nortest")
install.packages("Metrics")


library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(Metrics)

options(max.print=100000000)
#Análisis exploratorio

datos<-datos%>%rename(Ram= `RAM (GB)`,Espacio= `Storage (GB)`,Precio= `Price ($)`, Ssize=`Screen Size (inches)`,
                      Peso=`Weight (kg)`, Bateria= `Battery Life (hours)`, Garantia = `Warranty (years)`, Grafica = `Graphics Card`, SO = `Operating System`)


datos$Ram<-as.factor(datos$Ram)
datos$Ssize<-as.factor(datos$Ssize)
datos$Garantia<-as.factor(datos$Garantia)
datos$Espacio<-as.factor(datos$Espacio)

hardware<-datos[,-c(2)]


options(max.print=100000)

### MODELO ###

m21<- lm(Precio~Ram+Processor+SO+Garantia+Bateria,data=hardware)
summary(m21)

#2 Multicolinealidad
vif(m21)

#3 Analisis residual y Calcular medidas de influencia

ad.test(m21$residuals)
shapiro.test(m21$residuals) #Normalidad
bptest(m21) #Homocedasticidad
dwtest(m21)#Independencia

influencia21 <- influence.measures(m21)

cooks_distances <- influencia21$infmat[, "cook.d"]
cooks_distances
umbral <- 4 / length(cooks_distances)
umbral
indices_influencia <- which(cooks_distances > umbral)

hardware_sin_influencia <- hardware[-indices_influencia, ]
print(hardware_sin_influencia)

if (length(indices_influencia) > 0) {
  hardware_sin_influencia <- hardware[-indices_influencia, ]
} else {
  hardware_sin_influencia <- hardware
}

#4 Seleccion de variables
modf2 <- step(m21, trace = T, direction = "both")
modf2
summary(modf2)

#5 Validacion del modelo
set.seed(123)

sample <- sample.int(n = nrow(datos), size = floor(0.80*nrow(datos)), replace = F)
train <- datos[sample, ] #División de datos entre entrenamiento y pruebas
test <- datos[-sample,]

#Modelo de entrenamiento; Para comprobar el modelo
modelotreaning <- lm(formula = Precio~Ram+Processor+SO+Garantia+Bateria, data = train)
summary(modelotreaning)

#Se revisan en el both step o back no en el de entrenamiento 

#Modelo de prueba: Para evaluar el rendimiento del modelo  
modelotesting <- lm(formula =Precio~Ram+Processor+SO+Garantia+Bateria, data = test)
prediccion <- predict.lm(modelotesting, data = test[, c("Ram","Garantia","Bateria","SO","Processor")])
summary(prediccion)
summary(modelotesting)


plot(test$Precio, prediccion)
abline(modelotesting)

modelotesting <- lm(formula = Precio ~ Ram+Processor+SO+Garantia+Bateria , data = test)
prediccion <- predict.lm(modelotesting, data = test)


library(Metrics)
metricas <- c(mae(test$Precio, prediccion),
              mape(test$Precio, prediccion),
              mse(test$Precio, prediccion),
              rmse(test$Precio, prediccion),
              AIC(modelotesting),
              BIC(modelotesting),
              summary(modelotesting)$r.squared)

names(metricas) <- c("Mae", "Mape", "Mse", "Rmse", "AIc", "BIC", "R^2")
metricas
























###########################
m22<- lm(Precio~Ram*Processor+Bateria+Ssize*Espacio,data=hardware)
summary(m22)


#Puntos de influencia

residuales22 <- rstandard(m22)
residuales22

plot(datos$Precio, residuales22)
abline(h=0)

 #Pruebas estadisticas

influencia2 <- influence.measures(m22)


medidas_influencia_numericas2 <- influencia2$infmat

medidas_influencia_df2 <- data.frame(medidas_influencia_numericas2)


medidas_influencia_df2

############################



#Prueba de residuales

ad.test(m22$residuals)
shapiro.test(m22$residuals) #Normalidad
bptest(m22) #Homocedasticidad
dwtest(m22) #Independencia

influencia1<-influence.measures(m1)


#Prueba de AIC (Stepwise) para checkear la significancia de las variables
#Modelo completo
modelo_completo <- lm(Precio ~ ., data = hardware)
vif(modelo_completo)
modelo_reducido <- lm(Precio ~ Ram+Garantia+Ssize , data = hardware)

AIC(modelo_completo)
AIC(m3)

BIC(modelo_completo)
BIC(modelo_reducido)


residuales<-rstandard(modelo_completo)
plot(datos$Precio,residuales)
abline(h=0)

ggplot(modelo_completo, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados", y = "Residuos") +
  theme_minimal()

influence.measures(modelo_completo)
