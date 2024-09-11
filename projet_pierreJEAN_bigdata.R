
library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(dplyr)
library(ggmap)
library(jsonlite)
library(sf)
library(RColorBrewer)



donnees <- read.csv("lic-data-2021.csv", stringsAsFactors = FALSE, sep = ";")

donnees_federation_aviron <- donnees %>% filter(Fédération == "FF d'Aviron")


regions_shapefile <- st_read(dsn = "regions-20180101.shp", quiet = TRUE)


contours_departements <- st_read("contour-des-departements.geojson", quiet = TRUE)

mygeocode <- function(adresses) {
  nominatim_osm <- function(address = NULL) {
    if (suppressWarnings(is.null(address))) return(data.frame(lon = NA, lat = NA))
    tryCatch({
      d <- jsonlite::fromJSON(
        paste0("https://nominatim.openstreetmap.org/search?q=", 
               URLencode(address), 
               "&format=json&addressdetails=0&limit=1")
      )
      if (length(d) == 0) return(data.frame(lon = NA, lat = NA))
      return(data.frame(lon = as.numeric(d$lon[1]), lat = as.numeric(d$lat[1])))
    }, error = function(c) return(data.frame(lon = NA, lat = NA)))
  }
  
  tableau <- do.call(rbind, lapply(adresses, nominatim_osm))
  return(tableau)
}


if(!all(!is.na(donnees_federation_aviron$Longitude) & !is.na(donnees_federation_aviron$Latitude))) {
  coordonnees <- mygeocode(donnees_federation_aviron$Commune)
  

  donnees_federation_aviron$Longitude <- coordonnees$lon
  donnees_federation_aviron$Latitude <- coordonnees$lat
}

nouv_noms <- c(
  "Code_Commune", "Commune", "Code_QPV", "Nom_QPV", "Departement", "Region", 
  "Statut_geo", "Code", "Federation", "F_1_4_ans", "F_5_9_ans", "F_10_14_ans", 
  "F_15_19_ans", "F_20_24_ans", "F_25_29_ans", "F_30_34_ans", "F_35_39_ans", 
  "F_40_44_ans", "F_45_49_ans", "F_50_54_ans", "F_55_59_ans", "F_60_64_ans", 
  "F_65_69_ans", "F_70_74_ans", "F_75_79_ans", "F_80_99_ans", "F_NR", 
  "H_1_4_ans", "H_5_9_ans", "H_10_14_ans", "H_15_19_ans", "H_20_24_ans", 
  "H_25_29_ans", "H_30_34_ans", "H_35_39_ans", "H_40_44_ans", "H_45_49_ans", 
  "H_50_54_ans", "H_55_59_ans", "H_60_64_ans", "H_65_69_ans", "H_70_74_ans", 
  "H_75_79_ans", "H_80_99_ans", "H_NR", "NR_NR", "Total", "Longitude", "Latitude"
)
colnames(donnees_federation_aviron) <- nouv_noms


donnees_federation_aviron$Total_homme <- rowSums(donnees_federation_aviron[, grep("^H_", colnames(donnees_federation_aviron))], na.rm = TRUE)
donnees_federation_aviron$Total_femme <- rowSums(donnees_federation_aviron[, grep("^F_", colnames(donnees_federation_aviron))], na.rm = TRUE)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Statistiques des Licenciés d'Aviron en France"),
  
  tabsetPanel(
    tabPanel("Nombre de licenciés d'aviron en France",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Region", "Choisir une région :", choices = unique(donnees_federation_aviron$Region)),
                 selectInput("Departement", "Choisir un département :", choices = NULL),
                 selectInput("Commune", "Choisir une commune :", choices = NULL)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Licenciés", 
                            h4("Nombre de licenciés d'aviron :"),
                            verbatimTextOutput("licencies_output")
                   )
                 )
               )
             )),
    
    tabPanel("Statistiques descriptives",
             tabsetPanel(
               tabPanel("Graphique H/F",
                        plotlyOutput("genre_plot")),
               
               tabPanel("Comparaison Hommes/Femmes", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("region", "Choisir une région :", choices = unique(donnees_federation_aviron$Region))
                          ),
                          mainPanel(
                            plotlyOutput("bar_chart")
                          )
                        )),
               
               tabPanel("Licenciés par département",
                        fluidPage(
                          titlePanel("Graphique interactif des licenciés par département"),
                          sliderInput("range", "Sélectionnez une plage de valeurs :", min = 0, max = 3000, value = c(0, 3000), step = 100),
                          plotlyOutput("licencies_plot")
                        )
               )
             )),
    
    tabPanel("Cartes des licenciés d'aviron",
             tabsetPanel(
               tabPanel("Par Commune", leafletOutput("ma_carte_commune")),
               tabPanel("Par Région", leafletOutput("ma_carte_region")),
               tabPanel("Par Département", leafletOutput("ma_carte_departement")),
               tabPanel("Carte H/F", leafletOutput("ma_carte_region_clickH_F"))
             )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$Region, {
    req(input$Region)
    dept_choices <- unique(donnees_federation_aviron$Departement[donnees_federation_aviron$Region == input$Region])
    updateSelectInput(session, "Departement", choices = dept_choices, selected = NULL)
  })
  
  observeEvent(input$Departement, {
    req(input$Departement)
    commune_choices <- unique(donnees_federation_aviron$Commune[donnees_federation_aviron$Departement == input$Departement])
    updateSelectInput(session, "Commune", choices = commune_choices, selected = NULL)
  })
  
  output$licencies_output <- renderPrint({
    req(input$Commune)
    licencies <- donnees_federation_aviron$Total[donnees_federation_aviron$Commune == input$Commune]
    paste("Nombre de licenciés d'aviron à", input$Commune, ":", licencies)
  })
  
  output$genre_plot <- renderPlotly({
    df_total <- data.frame(
      Categorie = c("Hommes", "Femmes"),
      Total = c(sum(donnees_federation_aviron$Total_homme, na.rm = TRUE),
                sum(donnees_federation_aviron$Total_femme, na.rm = TRUE))
    )
    
    plot_ly(df_total, labels = ~Categorie, values = ~Total, type = "pie") %>%
      layout(title = "Répartition des licences par genre",
             showlegend = TRUE,
             legend = list(title = list(text = "Catégorie"), font = list(size = 12)),
             titlefont = list(size = 18, hjust = 0.5))
  })
  
  filtered_data <- reactive({
    req(input$region)
    subset(donnees_federation_aviron, Region == input$region)
  })
  
  output$bar_chart <- renderPlotly({
    region_data <- filtered_data()
    
    total_hommes <- sum(region_data$Total_homme, na.rm = TRUE)
    total_femmes <- sum(region_data$Total_femme, na.rm = TRUE)
    
    plot_ly(x = c("Hommes", "Femmes"), 
            y = c(total_hommes, total_femmes), 
            type = 'bar', 
            marker = list(color = c("blue", "pink")),
            name = 'Licenciés') %>%
      layout(title = paste("Comparaison des licenciés hommes et femmes dans la région", input$region),
             xaxis = list(title = "Genre"),
             yaxis = list(title = "Nombre de licenciés"),
             barmode = 'group')
  })
  
  output$licencies_plot <- renderPlotly({
    filtered_data <- donnees_federation_aviron[donnees_federation_aviron$Total >= input$range[1] & 
                                                 donnees_federation_aviron$Total <= input$range[2], ]
    
    plot_ly(data = filtered_data, x = ~Departement, y = ~Total, type = 'bar', 
            marker = list(color = 'steelblue')) %>%
      layout(title = "Répartition des licenciés par département",
             xaxis = list(title = "Département"),
             yaxis = list(title = "Nombre de licenciés"),
             bargap = 0.2) %>%
      config(scrollZoom = TRUE)
  })
  
  output$ma_carte_commune <- renderLeaflet({
    leaflet(data = donnees_federation_aviron) %>%
      addTiles() %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
      addMarkers(
        ~Longitude, ~Latitude, 
        popup = ~paste("Commune : ", Commune, "<br>",
                       "Nombre de licenciés : ", Total),
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$ma_carte_region <- renderLeaflet({
    regions_data <- regions_shapefile %>%
      left_join(donnees_federation_aviron %>% 
                  group_by(Region) %>% 
                  summarise(Total = sum(Total, na.rm = TRUE)),
                by = c("nom" = "Region"))
    
    pal <- colorQuantile("YlGnBu", domain = regions_data$Total, n = 5)
    
    leaflet(regions_data) %>%
      addTiles() %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal(Total),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = ~paste("Région : ", nom, "<br>",
                       "Nombre de licenciés : ", Total),
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Total, opacity = 0.7, title = "Nombre de licenciés",
                position = "bottomright")
  })
  
  output$ma_carte_departement <- renderLeaflet({
    departements_data <- contours_departements %>%
      left_join(donnees_federation_aviron %>% 
                  group_by(Departement) %>% 
                  summarise(Total = sum(Total, na.rm = TRUE)),
                by = c("code" = "Departement"))
    
    pal <- colorQuantile("Reds", domain = departements_data$Total, n = 5)
    
    leaflet(departements_data) %>%
      addTiles() %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(Total),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = ~paste("Département : ", nom, "<br>",
                       "Nombre de licenciés : ", Total),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Total, opacity = 0.7, title = "Nombre de licenciés",
                position = "bottomright")
  })
  
  output$ma_carte_region_clickH_F <- renderLeaflet({
    leaflet(regions_shapefile) %>%
      addTiles() %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 5) %>%
      addPolygons(
        fillColor = "lightblue",
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        layerId = ~code_insee,
        popup = ~paste("Région : ", nom, "<br>",
                       "Nombre de licenciés : ", 
                       sum(donnees_federation_aviron$Total[donnees_federation_aviron$Region == nom], na.rm = TRUE))
      )
  })
  observeEvent(input$ma_carte_region_clickH_F_shape_click, {
    event <- input$ma_carte_region_clickH_F_shape_click
    if (!is.null(event$id)) {
      region_name <- regions_shapefile$nom[regions_shapefile$code_insee == event$id]
      
      region_total_homme <- sum(donnees_federation_aviron$Total_homme[donnees_federation_aviron$Region == region_name], na.rm = TRUE)
      region_total_femme <- sum(donnees_federation_aviron$Total_femme[donnees_federation_aviron$Region == region_name], na.rm = TRUE)
      
      couleur <- ifelse(region_total_homme < region_total_femme, "pink", "blue")
      
      showModal(modalDialog(
        title = "Région cliquée",
        paste("Vous avez cliqué sur la région :", region_name),
        paste("Nombre d'hommes dans cette région :", region_total_homme),
        paste("Nombre de femmes dans cette région :", region_total_femme)
      ))
      
      leafletProxy("ma_carte_region_clickH_F") %>%
        clearShapes() %>%
        addPolygons(data = regions_shapefile,
                    fillColor = ~ifelse(nom == region_name, couleur, "lightblue"),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    layerId = ~code_insee,
                    popup = ~paste("Région : ", nom, "<br>",
                                   "Nombre de licenciés : ", 
                                   sum(donnees_federation_aviron$Total[donnees_federation_aviron$Region == nom], na.rm = TRUE))
        )
    }
  })
}

shinyApp(ui = ui, server = server)
