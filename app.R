library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggstream)
library(lubridate)
library(ggstream)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Analyse des consommations et productions regionales au pas demi horaire",
      titleWidth = 700),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Consommation < 36 kVA", tabName = "consommationinf36", icon = icon("dashboard")),
        menuItem("Consommation > 36 kVA", tabName = "consommationsup36", icon = icon("dashboard")),
        menuItem("Production", tabName = "production", icon = icon("chart-line")))
    ),
    dashboardBody(
      strong("Les donn?es publi?es donnent une vision de la consommation d'?lectricit? au pas 1/2 h des points de soutirage ??? 36kVA. La liste d?roulante permet de s?lectionner l'agr?gat souhait? : total de l'?nergie consomm?e, courbes de charges moyennes ou nombre de points de soutirage."),
      tabItems(
        tabItem("consommationinf36",align="center",
                column(width = 6, align="center",
                       box(
                         title = "Filtres",
                         em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) s?lectionnables par un bouton."),
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,align="center",
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "total_energy-sout_inf", label = " ", choices= c("Nb points d'injection","Total ?nergie inject?e (MWh)", "Courbes Moyennes (Wh)"))),
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "nat_reg_inf", label = "S?lectionner le national ou une r?gion", choices=liste_reg_nf)),
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "Profil_inf", label = "Profil", choices= unique(data_inf$Profil))),
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_inf", label = "Plage de puissance souscrite", choices=unique(data_inf$Plage.de.puissance.souscrite))),
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(
                                  inputId = "dates_inf",
                                  label = "P?riode",
                                  start = "2021-06-01",
                                  end = "2021-09-30")), 
                         column(10,align="center",style=list("padding-right: 3px;"),
                                checkboxGroupInput(inputId = "pas", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire"))
                       )),
                column(width=6,
                       box(
                         title = "Evolution du volume de ventes",
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,
                         plotOutput("inf")
                       ), 
                       valueBox(value=textOutput(""), subtitle = "La somme de consommation (< 36Kva)"), actionButton("button", "Telecharger la base de donnnees "))),
        tabItem("consommationsup36",align="center",
                column(width = 6, align="center",
                       box(
                         title = "Filtres",
                         em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) s?lectionnables par un bouton."),
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,align="center",
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "total_energy_sout_sup", label = " ", choices= c("Nb points de soutirage","Total ?nergie soutiree (MWh)", "Courbes Moyennes (Wh)"))),
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "nat_reg_sup", label = "Selectionner le national ou une r?gion", choices= liste_reg_sup)),
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "secteur_activite_sup", label = "Secteur d'activit?", choices= liste_sec)),
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "profil_sup", label = "Profil", choices= unique(data_sup$Profil))),
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_sup", label = "Plage de puissance souscrite", choices= unique(data_sup$Plage.de.puissance.souscrite))),
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(
                                  inputId = "dates_sup",
                                  label = "P?riode",
                                  start = "2021-06-01",
                                  end = "2021-09-30")), 
                         column(10,align="center",style=list("padding-right: 3px;"),
                                checkboxGroupInput(inputId = "pas_sup", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire"))
                       )),
                
                column(width = 6,
                       
                       box(
                         title = "Evolution de la consommation > 36 KVA",
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,
                         plotOutput("sup")
                       ), valueBox(value=textOutput(""), subtitle = "La somme de consommation (> 36Kva)"), actionButton("buttonsup", "Telecharger la base de donnnees "))
        ),
        tabItem("production",align="center",
                column(width = 6,align="center",
                       box(
                         title = "Filtres",
                         em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) s?lectionnables par un bouton."),
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,align="center",
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "total_energy-sout_prod", label = " ", choices= c("Nb points d'injection","Total ?nergie injecte (MWh)", "Courbes Moyennes (Wh)"))),
                         column(10,align="center",style=list("padding-right: 3px;"),          
                                selectInput(inputId = "nat_reg_prod", label = "S?lectionner le national ou une r?gion", choices=liste_reg_prod)),
                         column(5,align="center",style=list("padding-right: 3px;"),           
                                selectInput(inputId = "filiere_prod", label = "Filiere", choices= liste_fil)),
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_prod", label = "Plage de puissance d'injection", choices= unique(data1$Plage.de.puissance.injection))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(
                                  inputId = "dates_prod",
                                  label = "P?riode",
                                  start = "2021-06-01",
                                  end = "2021-09-30")),
                         column(10,align="center",style=list("padding-right: 3px;"),   
                                checkboxGroupInput(inputId = "pas_prod", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire"))
                       )),
                column(width = 6,
                       
                       box(
                         title = "Evolution de la production",
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,
                         plotOutput('prod')
                       ),  valueBox(value=textOutput("prod_quantite"), subtitle = "La somme de la quantite produite "), actionButton("buttonprod", "Telecharger la base de donnnees ")
                )     
                
                
        )
      )),
    title = "Analyse des consommations et productions r?gionales au pas demi horaire",
    skin = "black"
  ),
  
  
  server = function(input, output, session) {
    ### donnees 
    ## donnes de production 
    donnes_prod <- reactive({
      
      if (input$nat_reg_prod == "National"){
        don <- data1 %>% filter(Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) & Plage.de.puissance.injection == input$plage_puissance_prod  & Filiere.de.production== input$filiere_prod)
        don <- don %>% group_by(Horodate,Filiere.de.production) %>% summarise(Total.Energie.injecte..Wh. = sum(Total.Energie.injecte..Wh., na.rm=T) ,Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh. , na.rm =T), Nb.points.injection = sum(Nb.points.injection , na.rm =T))
        if (input$pas_prod == "pas quotidien")
        {
          don$day <- format(don$Horodate, '%Y-%m-%d')
          don <-  don %>% group_by(day, Horodate,Filiere.de.production) %>% summarise(Total.Energie.injecte..Wh. = sum(Total.Energie.injecte..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.injection = sum(Nb.points.injection , na.rm=T) )
        }}
      else {
        don <- data1 %>%  filter(Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) & Plage.de.puissance.injection == input$plage_puissance_prod & Region == input$nat_reg_prod & Filiere.de.production == input$filiere_prod )
      } 
      don
    })
    #### donnees de consommation inf a 36
    donnes_inf <- reactive({
      
      if (input$nat_reg_ == "National")
      {
        don <- data_inf %>% filter(Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1]) & Plage.de.puissance.souscrite == input$plage_puissance_inf  & Profil == input$Profil_inf )
        don <- don %>% group_by(Horodate, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh..= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        
        if (input$pas_inf == "pas quotidien")
        {
          don$day <- format(don$Horodate, '%Y-%m-%d')
          don <-  don %>% group_by(day, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
      }
      else {
        don <- data_inf %>%  filter(Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1]) & Plage.de.puissance.souscrite == input$plage_puissance_inf & Region == input$nat_reg_inf & Profil == input$Profil_inf )
        if (input$pas_inf == "pas quotidien")
        {
          don$day <- format(don$Horodate, '%Y-%m-%d')
          don <-  don %>% group_by(day, Profil) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
      } 
      don
    })
    ##### donnes de product sup a 36
    donnes_sup <- reactive({
      
      if (input$nat_reg_ == "National")
      {
        don <- data_sup %>% filter(Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1]) & Plage.de.puissance.souscrite == input$plage_puissance_sup & Profil ==input$Profil_sup)
        don <- don %>% group_by(Horodate) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        if (input$pas_sup == "pas quotidien")
        {
          don$day <- format(don$Horodate, '%Y-%m-%d')
          don <-  don %>% group_by(day) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
        
      }
      else {
        don <- data_sup %>%  filter(Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1]) & Plage.de.puissance.souscrite == input$plage_puissance_sup & Region == input$nat_reg_sup & Profil == input$Profil_sup )
        if (input$pas_sup == "pas quotidien")
        {
          don$day <- format(don$Horodate, '%Y-%m-%d')
          don <-  don %>% group_by(day) %>% summarise(Total.Energie.soutire..Wh. = sum(Total.Energie.soutire..Wh. , na.rm=T), Courbe.Moyenne.n1..Wh.= sum(Courbe.Moyenne.n1..Wh., na.rm=T), Nb.points.soutirage = sum(Nb.points.soutirage , na.rm=T) )
        }
      } 
      don
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
        k <-ggplot(data=donnes_inf()) + aes(x=as.POSIXct(Horodate), y=Total.Energie.soutire..Wh. , fill = Profil) +geom_stream(type = "ridge") 
      }
      else
      {
        if (input$total_energy_sout_inf == "Nb points de soutirage")
        {
          k <- ggplot(data=donnes_inf()) + aes(x=as.POSIXct(Horodate), y=Nb.points.soutirage , fill = Profil) +geom_stream(type = "ridge") 
          
        }
        else {
          k<- ggplot(data=donnes_inf()) + aes(x=as.POSIXct(Horodate), y=Courbe.Moyenne.n1..Wh. , fill = Profil) +geom_stream(type = "ridge") 
          
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
    
    ### Text
    
    
    
    
    
  }
)
