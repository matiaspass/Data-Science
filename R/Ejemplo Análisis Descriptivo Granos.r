# Lo primero es descargar el archivo csv del directorio

arroz <- read.csv("cabecera.csv")
head(arroz)
summary(arroz)

library(ggplot2) #Importo la librería ggplot2

# Creamos el histograma
ggplot(data=arroz, aes(Humedad.Ingreso)) +
  geom_histogram(binwidth = 0.7, col='black', fill='green', alpha=0.4) +
  ggtitle('Arroz Procesado')# dibujamos el histograma

# Gráfico de Cajas
ggplot(arroz, aes(x=Variedad, y=Humedad.Ingreso, colour = factor(Variedad))) + geom_boxplot() + geom_jitter() + ggtitle("Humedad Ingresos Vs Variedad") + xlab('Variedad') + ylab('Humedad de Ingreso')

# Gráfico qqplots por variedad
ggplot(arroz, aes(sample = Humedad.Ingreso, colour = factor(Variedad))) + facet_wrap(~Variedad) + stat_qq() + stat_qq_line() + ggtitle("Variedad vs Humedad Ingreso") + xlab('Variedad') + ylab('Humedead Ingreso')



