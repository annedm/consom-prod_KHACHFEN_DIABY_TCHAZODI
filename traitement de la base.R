
library(tidyverse)


data1 <-  read.csv("C:\\Users\\Goldenshop.ma\\Desktop\\R advanced projects\\Projet Shiny\\data1.csv", sep = ";", encoding = "UTF-8")
data_sup <- read.csv("C:\\Users\\Goldenshop.ma\\Desktop\\R advanced projects\\Projet Shiny\\data2.csv", sep = ";", encoding = "UTF-8")
data_inf <- read.csv("C:\\Users\\Goldenshop.ma\\Desktop\\R advanced projects\\Projet Shiny\\data3.csv", sep = ";", encoding = "UTF-8")

data1 <- na.omit(data1)
data_sup <- na.omit(data_sup)
data_inf <- na.omit(data_inf)

### Trairement de donnnees:
library(lubridate)
data1$Horodate <- as.POSIXlt(data1$Horodate, tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data_inf$Horodate <-as.POSIXlt(data_inf$Horodate  ,tz="UTC", "%Y-%m-%dT%H:%M:%OS")
data_sup$Horodate <- as.POSIXlt(data_sup$Horodate , tz="UTC", "%Y-%m-%dT%H:%M:%OS")

 
########### wath to mega wath

data1$Total.energie.injecte..Wh. <- data1$Total.Energie.injecte..Wh./1000000
data_sup$Total.Energie.soutire..Wh. <- data_sup$Total.Energie.soutire..Wh./1000000
data_inf$Total.Energie.soutire..Wh. <- data_inf$Total.Energie.soutire..Wh./1000000


 

liste_reg_nf <-  unique(data_inf$Region)
liste_reg_sup <-unique(data_sup$Region)
liste_reg_prod <-unique(data1$Region)
liste_reg_nf <- c("National", liste_reg_nf)
liste_reg_sup <- c("National", liste_reg_sup)
liste_reg_prod <-c("National", liste_reg_prod)

liste_sec <- unique(data_sup$Secteur.activite)
liste_fil <- unique(data1$Filiere.de.production)



##don <- data_inf %>% filter(Horodate <= as.POSIXct("2021-09-30") & Horodate >= as.POSIXct("2021-06-01") & Plage.de.puissance.souscrite == "P0: Total <= 36 kVA"  & Profil == "ENT3 (+ ENT4 + ENT5)" )
##don <- don %>% group_by(Horodate, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh..= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )

##ggplot(data=don) + aes(x=as.POSIXct(Horodate), y=Total.Energie.soutire..Wh. , fill = Profil) +geom_stream(type = "ridge")

