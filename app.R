#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggstream)
library(lubridate)
library(ggstream)



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




shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Analyse des consommations et productions regionales au pas demi horaire",
      titleWidth = 700
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Consommation < 36 kVA", tabName = "consommationinf36", icon = icon("dashboard")),
        menuItem("Consommation > 36 kVA", tabName = "consommationsup36", icon = icon("dashboard")),
        menuItem("Production", tabName = "production", icon = icon("chart-line"))
      )
    ),
    dashboardBody( 
      
      tabItems(
        tabItem(
          "consommationinf36",column(width = 4,
          selectInput(inputId = "total_energy_sout_inf", label = "Total Energie soutiree (MWh)", choices= c("Nb points de soutirage","Total Energie soutiree (MWh)", "Courbes Moyennes (Wh)")),
          selectInput(inputId = "nat_reg_inf", label = "Selectionner le national ou une region", choices=liste_reg_nf),
          selectInput(inputId = "Profil_inf", label = "Profil", choices= unique(data_inf$Profil)),
          selectInput(inputId = "plage_puissance_inf", label = "Plage de puissance souscrite", choices=unique(data_inf$Plage.de.puissance.souscrite)),
          dateRangeInput(
            inputId = "dates_inf",
            label = h3("Date"),
            start = "2021-06-01",
            end = "2021-09-30"), 
            checkboxGroupInput(inputId = "pas_inf", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire")),column(width=8,
          
          box(
            title = "Evolution de la consommation < 36 KVA",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            plotOutput("inf")
          ), valueBox(value=textOutput(""), subtitle = "La somme de consommation (< 36Kva)"), actionButton("buttoninf", "Telecharger la base de donnnees "))
        ),
        tabItem(
          "consommationsup36",column(width = 4,
          selectInput(inputId = "total_energy_sout_sup", label = "Total énergie soutirée (MWh)", choices= c("Nb points de soutirage","Total énergie soutiree (MWh)", "Courbes Moyennes (Wh)")),
                      selectInput(inputId = "nat_reg_sup", label = "Selectionner le national ou une région", choices= liste_reg_sup),
                      selectInput(inputId = "secteur_activite_sup", label = "Secteur d'activité", choices= liste_sec),
                      selectInput(inputId = "profil_sup", label = "Profil", choices= unique(data_sup$Profil)),
                      selectInput(inputId = "plage_puissance_sup", label = "Plage de puissance souscrite", choices= unique(data_sup$Plage.de.puissance.souscrite)),
                      dateRangeInput(
                        inputId = "dates_sup",
                        label = h3("Date"),
                        start = "2021-06-01",
                        end = "2021-09-30"), checkboxGroupInput(inputId = "pas_sup", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire")),column(width = 8,
                      
                      box(
                        title = "Evolution de la consommation > 36 KVA",
                        status = "info",
                        solidHeader = TRUE,
                        width = 12,
                        plotOutput("sup")
                      ), valueBox(value=textOutput(""), subtitle = "La somme de consommation (> 36Kva)"), actionButton("buttonsup", "Telecharger la base de donnnees "))
        ),
        tabItem("production",column(width = 4,
                selectInput(inputId = "total_energy-sout_prod", label = "Total energie injectee (MWh)", choices= c("Nb points d'injection","Total énergie injecte (MWh)", "Courbes Moyennes (Wh)")),
                            selectInput(inputId = "nat_reg_prod", label = "Sélectionner le national ou une région", choices=liste_reg_prod),
                            selectInput(inputId = "filiere_prod", label = "Filiere", choices= liste_fil),
                            selectInput(inputId = "plage_puissance_prod", label = "Plage de puissance d'injection", choices= unique(data1$Plage.de.puissance.injection)),
                            dateRangeInput(
                            inputId = "dates_prod",
                            label = h3("Date"),
                            start = "2021-06-01",
                            end = "2021-09-30"), checkboxGroupInput(inputId = "pas_prod", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire")),column(width = 8,
                      
                            box(
                            title = "Evolution de la production",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            plotOutput('prod')
                                        ) , valueBox(value=textOutput("prod_quantite"), subtitle = "La somme de la quantite produite "), actionButton("buttonprod", "Telecharger la base de donnnees ")
                             )     
                
            
        )
    )),
    title = "Analyse des consommations et productions régionales au pas demi horaire",
    skin = "black"
  ),
  server = function(input, output) {
    ### donnees 
    ## donnes de production 
    donnes_prod <- reactive({
   
     if (input$nat_reg_prod == "National")
     {
       don <- data1 %>% filter(Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) & Plage.de.puissance.injection == input$plage_puissance_prod  & Filiere.de.production== input$filiere_prod)
       don <- don %>% group_by(Horodate, Filiere.de.production) %>% summarise(Total.Energie.injecte..Wh. = sum(Total.Energie.injecte..Wh., na.rm=T) ,Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh. , na.rm =T), Nb.points.injection = sum(Nb.points.injection , na.rm =T))
       if (input$pas_prod == "pas quotidien")
       {
         don$day <- format(don$Horodate, '%Y-%m-%d')
         don <-  don %>% group_by(day, Horodate, Filiere.de.production) %>% summarise(Total.Energie.injecte..Wh. = sum(Total.Energie.injecte..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.injection = sum(Nb.points.injection , na.rm=T) )
       }
       }
      else {
       don <- data1 %>%  filter(Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) & Plage.de.puissance.injection == input$plage_puissance_prod & Region == input$nat_reg_prod & Filiere.de.production == input$filiere_prod )
      } 
      don
    })
    #### donnees de consommation inf a 36
    donnes_inf <- reactive({
      
      if (input$nat_reg_ == "National")
      {
        don_inf <- data_inf %>% filter(Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1]) & Plage.de.puissance.souscrite == input$plage_puissance_inf  & Profil == input$Profil_inf )
        don_inf <- don %>% group_by(Horodate, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh..= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        
        if (input$pas_inf == "pas quotidien")
        {
          don_inf$day <- format(don$Horodate, '%Y-%m-%d')
          don_inf <-  don %>% group_by(day, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
      }
      else {
        don_inf <- data_inf %>%  filter(Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1]) & Plage.de.puissance.souscrite == input$plage_puissance_inf & Region == input$nat_reg_inf & Profil == input$Profil_inf )
        if (input$pas_inf == "pas quotidien")
        {
          don_inf$day <- format(don$Horodate, '%Y-%m-%d')
          don_inf<-  don %>% group_by(day, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
        } 
      don_inf
      })
    ##### donnes de product sup a 36
    donnes_sup <- reactive({
      
      if (input$nat_reg_ == "National")
      {
        don_sup <- data_sup %>% filter(Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1]) & Plage.de.puissance.souscrite == input$plage_puissance_sup & Profil ==input$Profil_sup)
        don_sup <- don %>% group_by(Horodate) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        if (input$pas_sup == "pas quotidien")
        {
          don_sup$day <- format(don$Horodate, '%Y-%m-%d')
          don_sup <-  don %>% group_by(day) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
        
        }
      else {
        don_sup <- data_sup %>%  filter(Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1]) & Plage.de.puissance.souscrite == input$plage_puissance_sup & Region == input$nat_reg_sup & Profil == input$Profil_sup )
        if (input$pas_sup == "pas quotidien")
        {
          don_sup$day <- format(don$Horodate, '%Y-%m-%d')
          don_sup <-  don %>% group_by(day) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
         } 
      don_sup
    })
    
   
   
    # boutton download
    data <- eventReactive(input$buttoninf, {
      df <- data.frame(donnes_inf)
      write.csv(df, file ="C:\\Users\\Goldenshop.ma\\Documents\\data_inf.csv")
    })
  
    data2 <- eventReactive(input$buttonsup, {
      df <- data.frame(donnes_sup)
      write.csv(df, "C:\\Users\\Goldenshop.ma\\Documents\\data_sup.csv")
    })
    
    data2 <- eventReactive(input$buttonprod, {
      df <- data.frame(donnes_prod)
      write.csv(df, "C:\\Users\\Goldenshop.ma\\Documents\\data_prod.csv")
    })
   ### courbes
    construit_inf <- reactive({
      
      if (input$total_energy_sout_inf == "Total Energie soutiree (MWh)")
      {
        k <-ggplot(data=donnes_inf(), aes(x=as.POSIXct(Horodate), y=Total.Energie.soutire..Wh. , fill = Profil)) +geom_stream(type = "ridge") 
      }
      else
      {
        if (input$total_energy_sout_inf == "Nb points de soutirage")
        {
         k <- ggplot(data=donnes_inf(), aes(x=as.POSIXct(Horodate), y=Nb.points.soutirage , fill = Profil)) +geom_stream(type = "ridge") 
          
        }
        else {
         k<- ggplot(data=donnes_inf(),aes(x=as.POSIXct(Horodate), y=Courbe.Moyenne.n1..Wh. , fill = Profil)) +geom_stream(type = "ridge") 
          
        }
      }
      k
      })
    
    output$inf <- renderPlot({plot(construit_inf())})
    
    construit_sup <- reactive({
      if (input$total_energy_sout_sup == "Total Energie soutiree (MWh)")
      {
        ggplot(data=donnes_sup()) + aes(x=as.POSIXct(Horodate), y=Total.Energie.soutire..Wh. , fill = Profil) +geom_stream(type = "ridge") 
      }
      else
      {
        if (input$total_energy_sout_sup == "Nb points de soutirage")
        {
          ggplot(data=donnes_sup()) + aes(x=as.POSIXct(Horodate), y=Nb.points.soutirage , fill = Profil) +geom_stream(type = "ridge") 
         
        }
        else {
          ggplot(data=donnes_sup()) + aes(x=as.POSIXct(Horodate), y=Courbe.Moyenne.n1..Wh. , fill = Profil) +geom_stream(type = "ridge") 
          
        }
      }
      
      })
    
    output$sup <- renderPlot({plot(construit_sup())})
    
    construit_prod <- reactive({
      if (input$total_energy_sout_prod == "Total Energie soutiree (MWh)")
      {
        ggplot(data=donnes_prod()) + aes(x=as.POSIXct(Horodate), y=Total.Energie.injecte..Wh , fill = Filiere.de.production) +geom_stream(type = "ridge") 
      }
      else
      {
        if (input$total_energy_sout_prod == "Nb points de soutirage")
        {
          ggplot(data=donnes_prod()) + aes(x=as.POSIXct(Horodate), y=Nb.points.injection , fill = Filiere.de.production) +geom_stream(type = "ridge") 
          
        }
        else {
          ggplot(data=donnes_prod()) + aes(x=as.POSIXct(Horodate), y=Courbe.Moyenne.n1..Wh. , fill = Filiere.de.production) +geom_stream(type = "ridge") 
          
        }
      }
      
    })
    
    output$prod <- renderPlot({plot(construit_prod())})
    
  
    
    
    
    
  }
)