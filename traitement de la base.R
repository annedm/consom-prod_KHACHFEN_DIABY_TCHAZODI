library(tidyverse)
data1 <-  read.csv("C:\\Users\\Goldenshop.ma\\Desktop\\R advanced projects\\Projet Shiny\\data1.csv", sep = ";", encoding = "UTF-8")
data_sup <- read.csv("C:\\Users\\Goldenshop.ma\\Desktop\\R advanced projects\\Projet Shiny\\data2.csv", sep = ";", encoding = "UTF-8")
data_inf <- read.csv("C:\\Users\\Goldenshop.ma\\Desktop\\R advanced projects\\Projet Shiny\\data3.csv", sep = ";", encoding = "UTF-8")

data1 <- read.csv("D:/Projet Shiny/data1.csv", header=T, sep=";", encoding = "UTF-8")
data_sup <- read.csv("D:/Projet Shiny/data2.csv", header=T, sep=";", encoding = "UTF-8")
data_inf <- read.csv("D:/Projet Shiny/data3.csv", header=T, sep=";", encoding = "UTF-8")
### Trairement de donnnees:
library(lubridate)
data1$Horodate <- as.POSIXlt(data1$Horodate, tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data_inf$Horodate <-as.POSIXct(data_inf$Horodate  ,tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data_sup$Horodate <- as.POSIXlt(data_sup$Horodate , tz="UTC", "%Y-%m-%dT%H:%M:%OS")




data1 <- data1 %>% rename(data1$Region == "Région" ,  Filiere.de.production = Filière.de.production ,Total.energie.injectee..Wh.=Total.énergie.injectée..Wh.)
data_inf <- data_inf %>% rename(Region = Région, Total.energie.soutiree..Wh. = Total.énergie.soutirée..Wh. )
data_sup <- data_sup %>% rename(Region= Région , Secteur.activite = Secteur.activité, Total.energie.soutiree..Wh. = Total.énergie.soutirée..Wh.)

data1 <- na.omit(data1)
data_inf <- na.omit(data_inf)
data_sup <-na.omit(data_sup)


########### wath to mega wath

data1$Total.energie.injectee..Wh. <- data1$Total.energie.injectee..Wh./1000000
data_sup$Total.energie.soutiree..Wh. <- data_sup$Total.energie.soutiree..Wh./1000000
data_inf$Total.energie.soutiree..Wh. <- data_inf$Total.energie.soutiree..Wh./1000000




library(reshape2)

#tableau pour inferieur à 36
CT_inf <-c("Nb.points.soutirage","Total.Energie.soutire..Wh.","Courbe.Moyenne.n1..Wh.")
data_inf_verticale <- melt(data_inf, id.vars = c("Horodate", "Region", "Profil", "Plage.de.puissance.souscrite"), measure.vars = CT_inf)
View(data_inf_verticale)


categorie = unique(data_inf_verticale$variable )
filliere_prod = data11$Filiere.de.production

#tableau pour production 
data1$Horodate <- as.POSIXct(data1$Horodate, tz="UTC", "%Y-%m-%dT%H:%M:%OS")
CT_prod <-c("Total.Energie.injecte..Wh.","Nb.points.injection","Courbe.Moyenne.n1..Wh.")
data_11_verticale <- melt(data1, id.vars = c("Horodate", "Region", "Plage.de.puissance.injection", "Filiere.de.production"), measure.vars = CT_prod)

data_sup$Horodate <- as.POSIXct(data_sup$Horodate , tz="UTC", "%Y-%m-%dT%H:%M:%OS")
CT_sup <-c("Nb.points.soutirage","Total.Energie.soutire..Wh.","Courbe.Moyenne.n1..Wh.")
data_sup_verticale <- melt(data_sup, id.vars = c("Horodate", "Region", "Profil", "Plage.de.puissance.souscrite", "Secteur.activite"), measure.vars = CT_sup)
