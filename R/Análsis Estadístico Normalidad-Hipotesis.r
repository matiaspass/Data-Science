# Cargamos el archivo de lectura

IC109 <- read.csv("IC_109_L.csv")
head(IC109)
summary(IC109)

#Importo la libreria ggplot2

library(ggplot2) 

# Grafico para chequear normalidad y calculo de test de Shapiro

ggplot(IC109, aes(sample = Humedad.Ingreso, colour = "IC109")) + stat_qq() + stat_qq_line() + ggtitle("Variedad vs Humedad Ingreso") + xlab('Variedad') + ylab('Humedead Ingreso')

IC109_humed_ing = cbind(IC109$Humedad.Ingreso)
head(IC109_humed_ing)
shapiro.test(IC109_humed_ing)


# a) Se definen las hipótesis y el estadístico de prueba
# H0: u = u0 = 19
# H1: u > 19

mu0<-19 #Media de la H0

# Aplicando la función que tiene R para este calculo:
t.test(IC109_humed_ing, y = NULL,
       alternative = "greater",
       mu = 19)

#Gráficos

xLims <- c(-4, 4)
right <- seq(t0, xLims[2],   length.out=100)
yH0r  <- dt(right,length(IC109_humed_ing)-1)
curve(dt(x,length(IC109_humed_ing)-1), xlim=xLims, lwd=2, col="red", xlab="x", ylab="fdp",
      main="DistribuciÃ³n t-student de H0", ylim=c(-0.05, 0.4), xaxs="i")
polygon(c(right, rev(right)), c(yH0r, numeric(length(right))), border=NA,
        density=5, lty=2, lwd=2, angle=45, col="darkgray")       
abline(v=t0, lty=2, lwd=3, col="black")
text(t0+.15, -0.02,expression(t['0']),  cex=1.)
text(t0+.7, 0.05,  'p-valor',  cex=1.3)
alpha <- 0.05
crit <- qt(1-alpha, length(IC109_humed_ing)-1)
right2 <- seq(crit, xLims[2],   length.out=100)
yH0r2  <- dt(right2,length(IC109_humed_ing)-1)
polygon(c(right2, rev(right2)), c(yH0r2, numeric(length(right2))), border=NA,
        col=rgb(1, 0.3, 0.3, 0.6))
text(crit+.3, 0.025,expression(alpha),  cex=1.3)






