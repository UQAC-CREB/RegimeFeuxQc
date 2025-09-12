# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

# --- Données ---
zones_sf   <- st_read("./Data/zones_styled.geojson", quiet = TRUE)
qc_contour <- readRDS("./Data/Province_contourSimp_wgs84.rds")

# Centroides pour l’étiquette permanente au centre des polygones
zone_centroids <- st_point_on_surface(zones_sf) %>% select(ZONE_ID)

ui <- fluidPage(
  titlePanel(NULL),
  tags$style(HTML("
    .header-title {
      background-color: #2C3E50;
      color: white;
      padding: 20px;
      font-size: 22px;
      font-weight: bold;
      text-align: left;
      text-transform: uppercase;
      border-radius: 0px;
      margin-bottom: 20px;
      box-shadow: 2px 2px 8px rgba(0,0,0,0.2);
    }
    .box-style {
      background-color: #f9f9f9;
      border: 1px solid #ccc;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
      height: 700px;
      overflow-y: auto;
    }
    .popup-feux {
      font-family: Arial, sans-serif;
      line-height: 1.3;
      color: black;          /* Texte par défaut noir */
    }
    .popup-feux .titre {
      font-weight: bold; 
      font-size: 16px; 
      margin-bottom: 6px; 
      color: black;          /* Titre aussi en noir */
    }
    .popup-feux .ligne { 
      margin: 2px 0; 
      color: black;          /* Forcer la ligne en noir */
    }
    .popup-feux .label { 
      font-weight: bold; 
      font-size: 15px;
      color: black;          /* Forcer les labels en noir */
    }
  ")),
  div("RÉGIMES DE FEUX AU QUÉBEC", class = "header-title"),
  fluidRow(
    column(
      3,
      div(
        class = "box-style",
        htmlOutput("info_text", height = "60px"),
        tags$hr(style = "border-top: 1px solid #aaa; margin-top: 20px; margin-bottom: 20px;"),
        plotOutput("barplot", height = "540px")
      )
    ),
    column(
      9,
      div(
        class = "box-style",
        leafletOutput("map", height = "640px")
      )
    )
  )
)

server <- function(input, output, session) {
  zone_bounds   <- st_bbox(zones_sf)
  selected_zone <- reactiveVal(NULL)
  
  # Carte de base
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(
        lng = mean(c(zone_bounds$xmin, zone_bounds$xmax)),
        lat = mean(c(zone_bounds$ymin, zone_bounds$ymax)),
        zoom = 6
      ) %>%
      addPolygons(
        data = qc_contour,
        color = "#000000",
        weight = 1,
        fill = FALSE
      ) %>%
      addPolygons(
        data = zones_sf,
        layerId = ~ZONE_ID,                  # IMPORTANT pour capter le clic
        fillColor = ~Couleur_hex,
        fillOpacity = 0.6,
        color = "black",
        weight = 1,
        label = ~ZONE_ID,
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textOnly = TRUE,
          direction = "center"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          bringToFront = TRUE
        )
      ) %>%
      addLabelOnlyMarkers(
        data = zone_centroids,
        label = ~ZONE_ID,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "font-weight" = "bold",
            "font-size"   = "20px"
          )
        )
      )
  })
  
  # Gestion du clic : met à jour le panneau et affiche la popup
  observeEvent(input$map_shape_click, {
    # S’assurer qu’on clique un polygone de zones (avec un id)
    zone_id <- input$map_shape_click$id
    if (is.null(zone_id)) return()
    
    selected <- zones_sf %>% filter(ZONE_ID == zone_id)
    if (nrow(selected) == 0) return()
    
    selected_zone(selected)
    
    # Contenu HTML de la popup
    popup_html <- sprintf(
      '<div class="popup-feux">
         <div class="titre">Régime de feux</div>
         <div class="ligne"><span class="label">Zone :</span> %s</div>
         <div class="ligne"><span class="label">Cycle de feu :</span> %s ans</div>
       </div>',
      htmltools::htmlEscape(selected$ZONE_ID[1]),
      htmltools::htmlEscape(selected$CYCLE_FEU[1])
    )
    
    # Afficher la popup au point cliqué
    leafletProxy("map") %>%
      clearPopups() %>%  # retire l’ancienne popup si présente
      addPopups(
        lng = input$map_shape_click$lng,
        lat = input$map_shape_click$lat,
        popup = popup_html,
        options = popupOptions(closeButton = TRUE, autoPan = TRUE)
      )
  })
  
  # Panneau de gauche : texte
  output$info_text <- renderUI({
    z <- selected_zone()
    if (is.null(z)) {
      HTML("<b>Zone sélectionnée :</b><br><b>Cycle de feu :</b>")
    } else {
      HTML(paste0(
        "<b>Zone sélectionnée :</b> ", z$ZONE_ID,
        "<br><b>Cycle de feu :</b> ", z$CYCLE_FEU, " ans"
      ))
    }
  })
  
  # Panneau de gauche : barplot
  output$barplot <- renderPlot({
    if (is.null(selected_zone())) {
      ggplot(
        data.frame(x = "Aucune zone sélectionnée", y = 0),
        aes(x = x, y = y)
      ) +
        geom_bar(stat = "identity", fill = "gray80") +
        labs(
          x = "Zone",
          y = "Superficie (km² x 1000)",
          title = "Sélectionnez une zone"
        ) +
        ylim(0, 100) +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x  = element_text(size = 12),
          axis.text.y  = element_text(size = 12),
          plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
        )
    } else {
      z <- selected_zone()
      ggplot(z, aes(x = ZONE_ID, y = SUPERFICIE / 1000)) +
        geom_bar(stat = "identity", fill = z$Couleur_hex) +
        labs(
          x = "Zone",
          y = "Superficie (km² x 1000)"
        ) +
        ylim(0, 100) +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text.x  = element_text(size = 12),
          axis.text.y  = element_text(size = 12)
        )
    }
  })
}

shinyApp(ui, server)
