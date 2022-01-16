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
      h1("Agr?gats segment?s de consommation et production ?lectriques au pas 1/2 h"),
      strong("Les donn?es publi?es donnent une vision de la consommation d'?lectricit? au pas 1/2 h des points de soutirage < 36kVA. La liste d?roulante permet de s?lectionner l'agr?gat souhait? : total de l'?nergie consomm?e, courbes de charges moyennes ou nombre de points de soutirage."),
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
                                selectInput(inputId = "cat_inf", label = "Selectionner une categorie : ", choices= unique(data_inf_verticale$variable), selected ='Nb.points.soutirage')),
                         
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "Region_inf", label = "S?lectionner le national ou une r?gion", choices= c(unique(data_inf_verticale$Region),"National", selected ='National') )),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "Profil_inf", label = "Profil", choices= c(unique(data_inf_verticale$Profil),"Tous les profils", selected ='Tous les profils' ))),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_inf", label = "Plage de puissance souscrite", choices=unique(data_inf_verticale$Plage.de.puissance.souscrite))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(inputId = "dates_inf", label = "P?riode", start = "2021-06-01",end = "2021-09-30")), 
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                
                                checkboxGroupInput(inputId = "pas", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire")
                         )
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
                                selectInput(inputId = "cat_sup", label = " ", choices= unique(data_sup_verticale$variable) )),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "nat_reg_sup", label = "Selectionner le national ou une r?gion", choices=c(unique(data_sup_verticale$Region), "National"))),
                         
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "secteur_activite_sup", label = "Secteur d'activit?", choices=c(unique(data_sup_verticale$Secteur.activite), "Tous les secteurs")  )),
                         
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "profil_sup", label = "Profil", choices= c( unique(data_sup_verticale$Profil), "Tous les profils"))),
                         
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_sup", label = "Plage de puissance souscrite", choices= unique(data_sup_verticale$Plage.de.puissance.souscrite))),
                         
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
                                selectInput(inputId = "cat_prod", label = " ", choices= unique(data_11_verticale$variable))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),          
                                selectInput(inputId = "nat_reg_prod", label = "S?lectionner le national ou une r?gion", choices= c(unique(data_11_verticale$Region), "National"), selected ='National' )),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),           
                                selectInput(inputId = "filiere_prod", label = "Filiere", choices= c(unique(data_11_verticale$Filiere.de.production), "Toutes les filieres" ), selected = "Toutes les filieres" )),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_prod", label = "Plage de puissance d'injection", choices= unique(data_11_verticale$Plage.de.puissance.injection))),
                         
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
    
    ### courbes
    
    construit_inf <- reactive({
      if (input$Region_inf == "National")
      {
        if (input$Profil_inf == "Tous les profils"){
          new_data <- data_inf_verticale  %>% filter( variable %in% input$cat_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1])) %>% group_by(Horodate, Profil, variable) %>% summarise(value = sum(value))
          
          ggplot(data=new_data) +
            aes(x=as.POSIXct(Horodate), y=value , fill = Profil) +
            geom_stream(type = "ridge") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey"))
        } 
        else {
          new_data <- data_inf_verticale  %>% filter(Profil %in% input$Profil_inf, variable %in% input$cat_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1])) %>% group_by(Horodate, Profil, variable) %>% summarise(value = sum(value))
          ggplot(data=new_data) +
            aes(x=as.POSIXct(Horodate), y=value , fill = Profil) +
            geom_stream(type = "ridge") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey"))
        }
      }
      else {
        if (input$Profil_inf == "Tous les profils"){
          new_data <- data_inf_verticale  %>% filter( variable %in% input$cat_inf, Region == input$Region_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1])) %>% group_by(Horodate, Profil, variable) %>% summarise(value = sum(value))
          
          ggplot(data=new_data) +
            aes(x=as.POSIXct(Horodate), y=value , fill = Profil) +
            geom_stream(type = "ridge") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey"))
        }
        else {
          
          ggplot(data=data_inf_verticale %>% filter(Profil %in% input$Profil_inf, variable %in% input$cat_inf, Region == input$Region_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1]))) +
            aes(x=as.POSIXct(Horodate), y=value , fill = Profil) +
            geom_stream(type = "ridge") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey"))
        }
      }
      
    })
    output$inf <- renderPlot({plot(construit_inf())})
    
    
    construit_sup <- reactive({
      if (input$nat_reg_sup == "National")
      {
        new_data1 <- data_sup_verticale  %>% filter(Secteur.activite == input$secteur_activite_sup,Profil %in% input$profil_sup, variable %in% input$cat_sup, Plage.de.puissance.souscrite == input$plage_puissance_sup, Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1])) %>% group_by(Horodate,  variable) %>% summarise(value = sum(value))
        
        
        ggplot(data = new_data1)+
          aes(x=as.POSIXct(Horodate), y=value , fill = Secteur.activite) +
          theme(plot.title = element_text(hjust = 0.5))+
          geom_stream(type = "ridge") + 
          ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
          ylab(" ")+
          xlab("Temps (au pas jours)") + 
          theme(
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            axis.line.x = element_line(color = "grey")
          )  
      }
      else
      {
        if ( input$profil_sup == "Tous les profils")
        {
          
          
          new_data1 <- data_sup_verticale %>% filter(  variable == input$cat_sup, Region == input$nat_reg_sup, Plage.de.puissance.souscrite == input$plage_puissance_sup, Secteur.activite == input$secteur_activite_sup, Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1]) ) %>% group_by(Horodate, Profil, variable) %>% summarise(value = sum(value))
          ggplot(data = new_data1)+
            aes(x=as.POSIXct(Horodate), y=value , fill = Secteur.activite) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps (au pas jours)") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey")
            ) 
        }
        else 
        {
          new_data1 <- data_sup_verticale %>% filter(Profil %in% input$profil_sup, variable == input$cat_sup, Region == input$nat_reg_sup, Plage.de.puissance.souscrite == input$plage_puissance_sup, Secteur.activite == input$secteur_activite_sup, Horodate <= as.POSIXct(input$dates_sup[2]) & Horodate >= as.POSIXct(input$dates_sup[1]) )
          ggplot(data = new_data1)+
            aes(x=as.POSIXct(Horodate), y=value , fill = Secteur.activite) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps (au pas jours)") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey")
            )
        }
      }
      
    })
    
    
    
    output$sup <- renderPlot({plot(construit_sup())})   
    
    construit_prod <- reactive({
      if (input$nat_reg_prod == "National")
      {
        
        if (input$filiere_prod == "Toutes les filieres")
        {
          new_data3 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) ) %>% group_by(Horodate,Filiere.de.production, variable) %>% summarise(value = sum(value))
          ggplot(data= new_data3)+aes(x=as.POSIXct(Horodate), y=value , fill = Filiere.de.production) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey")
            ) 
        }
        else {
          new_data3 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod, Filiere.de.production == input$filiere_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) ) %>% group_by(Horodate,Filiere.de.production, variable) %>% summarise(value = sum(value))
          ggplot(data= new_data3)+aes(x=as.POSIXct(Horodate), y=value , fill = Filiere.de.production) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey")
            ) 
        }
      }
      else {
        if (input$filiere_prod == "Toutes les filieres")
        {
          new_data3 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod,Region == input$nat_reg_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) ) %>% group_by(Horodate,Filiere.de.production , variable) %>% summarise(value = sum(value))
          ggplot(data= new_data3)+aes(x=as.POSIXct(Horodate), y=value , fill = Filiere.de.production) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey")
            )  
        }
        else {
          
          ggplot(data_11_verticale %>% filter(variable %in% input$cat_prod, Filiere.de.production == input$filiere_prod, Region == input$nat_reg_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) )) +
            
            aes(x=as.POSIXct(Horodate), y=value , fill = Filiere.de.production) +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_stream(type = "ridge") + 
            ggtitle("Evolution de la quantit? d'?nergie soutir?e moyenne (MWh)")+
            ylab(" ")+
            xlab("Temps ") + 
            theme(
              plot.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color = "grey")
            ) 
        }
      }
    })
    
    
    output$prod <- renderPlot({plot(construit_prod())}) 
    
    ### Telechargement des donnees
    data <- eventReactive(input$buttoninf, {
      if (input$Region_inf == "National")
      {
        if (input$Profil_inf == "Tous les profils"){
          new_data <- data_inf_verticale  %>% filter( variable %in% input$cat_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1])) %>% group_by(Horodate, Profil, variable) %>% summarise(value = sum(value))
        }
        else
        {
          new_data <- data_inf_verticale  %>% filter(Profil %in% input$Profil_inf, variable %in% input$cat_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1])) %>% group_by(Horodate, Profil, variable) %>% summarise(value = sum(value))
        }
      }
      else 
      {
        new_data <- data_inf_verticale %>% filter(Profil %in% input$Profil_inf, variable %in% input$cat_inf, Region == input$Region_inf, Plage.de.puissance.souscrite == input$plage_puissance_inf, Horodate <= as.POSIXct(input$dates_inf[2]) & Horodate >= as.POSIXct(input$dates_inf[1]))
      }
      df <- data.frame(new_data)
      write.csv(df, file ="C:\\Users\\Goldenshop.ma\\Documents\\data_inf.csv")
    })
    
    data2 <- eventReactive(input$buttonprod, {
      if (input$nat_reg_prod == "National")
      {
        
        if (input$filiere_prod == "Toutes les filieres")
        {
          new_data3 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) ) %>% group_by(Horodate,Filiere.de.production, variable) %>% summarise(value = sum(value))
          
          
        }
        else {
          new_data3 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod, Filiere.de.production == input$filiere_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) ) %>% group_by(Horodate,Filiere.de.production, variable) %>% summarise(value = sum(value))
          
        }
      }
      else {
        if (input$filiere_prod == "Toutes les filieres")
        {
          new_data3 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod,Region == input$nat_reg_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) ) %>% group_by(Horodate,Filiere.de.production , variable) %>% summarise(value = sum(value))
          
        }
        else {
          
          new_data3 <- data_11_verticale %>% filter(variable %in% input$cat_prod, Filiere.de.production == input$filiere_prod, Region == input$nat_reg_prod, Plage.de.puissance.injection == input$plage_puissance_prod, Horodate <= as.POSIXct(input$dates_prod[2]) & Horodate >= as.POSIXct(input$dates_prod[1]) )
          
          
        }
      }
      df <- data.frame(new_data3)
      write.csv(df, "C:\\Users\\Goldenshop.ma\\Documents\\data_prod.csv")
    })
    
    data2 <- eventReactive(input$buttonsup, {
      df <- data.frame(donnes_prod)
      write.csv(df, "C:\\Users\\Goldenshop.ma\\Documents\\data_sup.csv")
    })
    
    
  }
)
