# ADM mettre les imports en haut de script, par ordre alphabetique
library(lubridate)
library(tidyverse)
library(reshape2)

##ADM 
## - JAMAIS DE CHEMINS EN DUR!!!!!!!!!
## - fread ca va beaucoup plus 
## - attention, les horodates sont en heure locale francaise pas en utc!
## - pour ne pas dupliquer le code on fait une fonction


## TODO: mettre les donnees dans un repertoire data/,
## garder les noms des fichiers d'origine et rediger un readme qui dit ou et comment les 
## trouver (site endis etc...)

#' Formattage d'un fichier csv
#'
#' @param filename nom du fichier csv
#'
#' @return dataframe avec les espaces remplaces par des points, les horodates en posixt
#' @export
#'
#' @examples
prepare_df <- function(filename){
  
  
  prepared_df <-  data.table::fread(filename, encoding = "UTF-8") %>%
    # mettre des . dans les titres
    rename_all(list(gsub), pattern = ' ', replacement = '.') %>%
    # passer les horodates au bon format
    mutate(Horodate = lubridate::ymd_hms(Horodate, tz = "Europe/Paris"))
  
    # wath to mega wath IL FAUT CHANGER LE NOM!!!!
    var_wh <- colnames(prepared_df)[
      which(stringr::str_detect(colnames(prepared_df), 'Wh'))]
  
    prepared_df <- prepared_df  %>%
      mutate_at(var_wh, list(function(x){x/1e6})) %>%
      rename_at(var_wh, 
                list(function(x){gsub(x, pattern ="Wh", replacement = "MWh")
        }))
 

  ## ADM: pourquoi virer les NA?????? c est mieux de les garder!!!!!
  #prepared_df  <- na.omit(prepared_df) 
  
  prepared_df
}

data1 <- prepare_df("data1.csv")
data_sup <- prepare_df("data2.csv")
data_inf <- prepare_df("data3.csv")



#tableau pour inferieur à 36
CT_inf <-c("Nb.points.soutirage","Total.Energie.soutire.(MWh)","Courbe.Moyenne.n1.(MWh)")
data_inf_verticale <- melt(data_inf, id.vars = c("Horodate", "Region", "Profil", "Plage.de.puissance.souscrite"), measure.vars = CT_inf)

# View(data_inf_verticale)

## TODO/ passer en dplyr
categorie = unique(data_inf_verticale$variable )
#filliere_prod = data11$Filiere.de.production

#tableau pour production 
CT_prod <-c("Total.Energie.injecte.(MWh)","Nb.points.injection","Courbe.Moyenne.n1.(MWh)")
data_11_verticale <- melt(data1, id.vars = c("Horodate", "Region", "Plage.de.puissance.injection", "Filiere.de.production"), measure.vars = CT_prod)

CT_sup <-c("Nb.points.soutirage","Total.Energie.soutire.(MWh)","Courbe.Moyenne.n1.(MWh)")
data_sup_verticale <- melt(data_sup, id.vars = c("Horodate", "Region", "Profil", "Plage.de.puissance.souscrite", "Secteur.activite"), measure.vars = CT_sup)

