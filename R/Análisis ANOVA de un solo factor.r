# Ejemplo de Análsis de ANOVA de un solo factor

# De la base de datos: 
# https://vincentarelbundock.github.io/Rdatasets/articles/data.html 

#Descripción de los datos mediante diagrama de cajas y bigotes.

data <- read.csv("nitrogen.csv")
head(data)
print(data)


conc <- data$conc
concentraciones <- factor(conc, levels = c (0, 80, 160, 235, 310), labels = c("ninguna", "baja", "media", "alta", "muy alta"))


xtabs( ~concentraciones, data)#muestra cuantas personas hay en cada grupo (n)
str(data)
aggregate( total ~ concentraciones, data, mean )#media de cada nivel

suppressWarnings({
  library(ggplot2)
  ggplot(data, aes(x = concentraciones, y = total, fill = concentraciones)) +
    geom_boxplot() +
    geom_jitter(shape = 15,
                color = "steelblue",
                position = position_jitter(0.21)) +
    theme_classic()
})    


# Se	realiza el test de ANOVA para explicar si alguno de los tratamientos 
# es significativamente diferente a la media del control.

anovaunfactor<-aov(formula=total~concentraciones,data=data) 
summary(anovaunfactor)


# Se computan los intervalos de confianza para las diferencias de medias. 
# Se determina cuales son significativamente diferentes.

#Intervalos de confianza

interv.total = lm(total ~ concentraciones, data)
confint(interv.total)

pairwise.t.test(data$total, concentraciones,p.adjust.method = "bonferroni")#

# Se	verifica normalidad y el comportamiento de los residuos con el objetivo 
# de justificar si es válido utilizar el método de Análisis de Varianza.


par(mfrow=c(1,2))

plot(anovaunfactor, 5)

plot(anovaunfactor, 1)

# Validación de Normalidad

plot(anovaunfactor, 2)

# Se extraen los residuos
aov_residuos <- residuals(object = anovaunfactor )
shapiro.test(x = aov_residuos )#Shapiro-Wilk test
