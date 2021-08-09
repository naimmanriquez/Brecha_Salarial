### Analisis de la ENIGH 2020 - Poblacion e ingresos
## Naim Manriquez

## Librerias

# Instalamos

install.packages("sjmisc")

install.packages("sjlabelled")

install.packages("tidyverse")

install.packages("haven")

install.packages("survey")

install.packages("dplyr")

install.packages("ggplot2")

install.packages("pacman")

install.packages("extrafont")

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2, extrafont)

loadfonts(device = "win", quiet = TRUE) 

## Cargamos bases de datos

poblacion <- read_csv("poblacion.csv")

ingresos <- read_csv("ingresos.csv")

## Union de bases de datos - funcion merge

pobingreso <- merge(poblacion, ingresos, join="leftouter", split=TRUE)

names(pobingreso)

## Algunos estadisticos

summary(pobingreso$ing_tri)

## Distribucion del ingreso

qplot(log(ing_tri), data=pobingreso, geom="density", alpha=I(.5), 
      main="Distribución del ingreso trimestral", xlab="Logaritmo", 
      ylab="Density")

## Y entre hombres y mujeres

qplot(log(ing_tri), data=pobingreso, geom="density", fill=factor(sexo), alpha=I(.5), 
      main="Distribución de los ingresos corrientes", xlab="Logaritmo", 
      ylab="Density")

qplot(log(ing_tri), data=pobingreso, geom="histogram", fill=factor(sexo), alpha=I(.5), 
      main="Distribución del ingreso trimestral por género", xlab="Logaritmo", 
      ylab="Frecuencia") +
  theme_bw()

## Y entre indigenas

## Filtramos solo los indigenas

poblacion_ind <- filter(pobingreso, hablaind == 1)


qplot(log(ing_tri), data=poblacion_ind, geom="density", fill=factor(sexo), alpha=I(.5), 
      main="Distribución de los ingresos poblacion indigena", xlab="Logaritmo", 
      ylab="Density") +
  theme_bw()

qplot(log(ing_tri), data=poblacion_ind, geom="histogram", fill=factor(sexo), alpha=I(.5), 
      main="Distribución del ingreso trimestral poblacion indigena", xlab="Logaritmo", 
      ylab="Frecuencia") +
  theme_bw()


#### Generamos graficos de caja y bigote

options(scipen = 999)

datos_ingreso <- filter(pobingreso, ing_tri >= 5000 & ing_tri <= 70000)

datos_ingreso$sexo <- factor(datos_ingreso$sexo,
                           labels = c("Masculino", "Femenino"))

datos_ingreso$hablaind <- factor(datos_ingreso$hablaind,
                             labels = c("Indigena", "No indigena"))


boxplot(datos_ingreso$ing_tri ~ datos_ingreso$sexo, col=terrain.colors(4)) 
  

boxplot(ing_tri ~ hablaind, data = datos_ingreso, col = "white")


ggplot(datos_ingreso, aes(x = sexo, y = ing_tri)) +
  geom_boxplot(colour = "black", fill = "#56B4E9") +
  scale_y_continuous(name = "Ingreso trimestral por persona",
                     breaks = seq(0, 70000, 10000),
                     limits=c(0, 70000)) +
  scale_x_discrete(name = "género") +
  labs(title = "Distribución del ingreso por persona en México",  #Título principal
       subtitle = "Según datos de la ENIGH 2020", 
       caption = "github \n @naimmanriquez") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, family="Candara"),
        text=element_text(size = 16, family="Candara"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))



ggplot(datos_ingreso, aes(x = hablaind, y = ing_tri)) +
  geom_boxplot(colour = "black", fill = "#56B4E9") +
  scale_y_continuous(name = "Ingreso trimestral por persona",
                     breaks = seq(0, 70000, 10000),
                     limits=c(0, 70000)) +
  scale_x_discrete(name = "Población indigena y no indigena") +
  labs(title = "Distribución del ingreso en población indigena",  #Título principal
       subtitle = "Según datos de la ENIGH 2020", 
       caption = "github \n @naimmanriquez") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, family="Microsoft PhagsPa"),
        text=element_text(size = 16, family="Microsoft PhagsPa"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))

library(RColorBrewer)

ggplot(datos_ingreso, aes(x = sexo, y = ing_tri, fill = hablaind)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Ingreso\ntrimestral por persona",
                     breaks = seq(0, 70000, 10000),
                     limits=c(0, 70000)) +
  scale_x_discrete(name = "género") +
  ggtitle("Distribución del ingreso trimestral por persona") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")


## Creando variable dicotomica por discapacidad

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_camin <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_ver <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_brazo <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_apren <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_oir <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_vest <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_habla <= 3, 1, 0)

datos_ingreso$Disc_A <- ifelse(datos_ingreso$disc_acti <= 3, 1, 0)

# Renombramos

datos_ingreso$Disc_A <- factor(datos_ingreso$Disc_A,
                                 labels = c("Persona sin discapacidad", "Persona con discapacidad"))

## Grafica


ggplot(datos_ingreso, aes(x = sexo, y = ing_tri, fill = Disc_A)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Ingreso\ntrimestral por persona",
                     breaks = seq(0, 70000, 10000),
                     limits=c(0, 70000)) +
  scale_x_discrete(name = "género") +
  ggtitle("Distribución del ingreso trimestral por persona") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")



