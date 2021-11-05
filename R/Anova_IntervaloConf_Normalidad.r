# Ejercicio 4.2

#a) De la base de datos: 
#  https://vincentarelbundock.github.io/Rdatasets/articles/data.html 
#Seleccione los datos denominados “Toxicity of Nitrofen in Aquatic Systems” y 
#estudie la variable “Total” que cuenta el número total de crías vivas de una 
#especie de zooplancton bajo diferentes concentraciones (variable “conc”) de un 
#herbicida (nitrofen). Describa los datos con un diagrama de cajas y bigotes.


data <- read.csv("nitrogen.csv")
head(data)
print(data)
#data_total <- data$total
#data_total


conc <- data$conc
concentraciones <- factor(conc, levels = c (0, 80, 160, 235, 310), labels = c("ninguna", "baja", "media", "alta", "muy alta"))


xtabs( ~concentraciones, data)#muestra cuantas personas hay en cada grupo (n)
str(data)
aggregate( total ~ concentraciones, data, mean )#media de cada nivel
# aggregate( total ~ concentraciones, data, sd )#desviaciÃ³n de cada nivel

suppressWarnings({
  library(ggplot2)
  ggplot(data, aes(x = concentraciones, y = total, fill = concentraciones)) +
    geom_boxplot() +
    geom_jitter(shape = 15,
                color = "steelblue",
                position = position_jitter(0.21)) +
    theme_classic()
})    


# b)	Realice el test de ANOVA para explicar si alguno de los tratamientos 
# es significativamente diferente a la media del control.

anovaunfactor<-aov(formula=total~concentraciones,data=data) 
summary(anovaunfactor)


# c)	Compute los intervalos de confianza para las diferencias de medias. 
#Determine y justifique cuáles son significativamente diferentes.

#Intervalos de confianza

interv.total = lm(total ~ concentraciones, data)
confint(interv.total)

pairwise.t.test(data$total, concentraciones,p.adjust.method = "bonferroni")#

# d)	Verifique normalidad y el comportamiento de los residuos de manera 
# similar a la parte teórica. Justifique si es válido utilizar el método de 
# Análisis de Varianza.


par(mfrow=c(1,2))

plot(anovaunfactor, 5)

plot(anovaunfactor, 1)


#ValidaciÃ³n de Normalidad
#qqplot


plot(anovaunfactor, 2)
# Se extraen los residuos
aov_residuos <- residuals(object = anovaunfactor )
shapiro.test(x = aov_residuos )#Shapiro-Wilk test
