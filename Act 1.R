setwd("~/Desktop/Act 1 Análisis/")
library(plyr)
library(tidyverse)
library("ggpubr")
library("robustbase")

#Importamos el dataset
covidData <- read.csv(file = 'owid-covid-data.csv', header = TRUE, sep = ",")
head(covidData)

#Selecionamos los datos agrupados por país para realizar el estudio
t <- covidData %>%
  group_by(iso_code) %>% 
  summarise(human_development_index = max(human_development_index),
            handwashing_facilities = max(handwashing_facilities))

#Obtenemos los valores de estadística descriptiva de los datos que vamos a estudiar
summary(t)

#Mostramos los datos en una gráfica
plot(t$human_development_index, t$handwashing_facilities)

#Aplicamos el algoritmo de correlación de pearson sobre los datos
print(cor.test(t$handwashing_facilities, t$human_development_index, method = "pearson"))

ggscatter(t, x = "handwashing_facilities", y = "human_development_index", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Handwashing facilities", ylab = "Human development")

shapiro.test(t$handwashing_facilities)
ggqqplot(t$handwashing_facilities, ylab = "Handwashing facilities")

shapiro.test(t$human_development_index)
ggqqplot(t$human_development_index, ylab = "Human Development")

#Aplicamos un análisis de regresión lineal simple sobre los datos
simpleregression = lm(handwashing_facilities ~ human_development_index, data = t)
summary(simpleregression) 

#Volvemos ha hacer plot, ahora con la linea de regresión
plot(t$human_development_index, t$handwashing_facilities)
abline(simpleregression, col = 2, lwd = 3)

#Extraer los coeficientes del modelo y niveles de confianza
coef(simpleregression)
confint(simpleregression, level = 0.99)

ltsReg(t$human_development_index, t$handwashing_facilities, intercept = TRUE)
plot(ltsReg(t$human_development_index, t$handwashing_facilities, intercept = TRUE))

# A parte
# 
##############################################################
t2 <- covidData %>% 
  group_by(iso_code) %>% 
  summarise(total_deaths_per_million = max(total_deaths_per_million, na.rm = TRUE), 
            diabetes_prevalence = max(diabetes_prevalence, na.rm = TRUE),
            cardiovasc_death_rate = max(cardiovasc_death_rate, na.rm = TRUE),
            smokers = max(smokers, na.rm = TRUE))

plot(t1)

multipleregression = lm(human_development_index ~ gdp_per_capita + diabetes_prevalence + cardiovasc_death_rate + smokers, data = t)
summary(multipleregression) 







