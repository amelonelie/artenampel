#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

#später nochmal überprüfen ob wir wirklich alle brauchen
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(readxl)
library(leaflet)
library(jsonlite)
library(tidyr)
library(httr)
library(rgbif)
library(leaflet)
# Funktion zum Laden der Excel-Datei
load_excel_data <- function(filepath = "/Users/amelonelie/Documents/Programme/GitHub/artenampel/ameliestry/rote_liste_saeugetiere_2005.xlsx") {
    tryCatch({
        data <- read_excel(filepath)
        # Bereinige Spaltennamen
        colnames(data) <- c("wissenschaftlicher_name", "deutscher_name", 
                            "gefaehrdung", "tiergruppe", "tiergruppe_deutsch")
        return(data)
    }, error = function(e) {
        message("Excel-Datei nicht gefunden. Bitte Pfad anpassen.")
        return(NULL)
    })
}

# Lade lokale Artendaten
arten_data <- load_excel_data()

kategorien <- data.frame(
    code = c("EX","RE", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE"),
    name = c("Ausgestorben", "Regional ausgestorben","In der Natur ausgestorben", "Vom Aussterben bedroht",
             "Stark gefährdet", "Gefährdet", "Potenziell gefährdet", 
             "Nicht gefährdet", "Unzureichende Datenlage", "Nicht bewertet"),
    farbe = c("#000000", "#3D1244","#9C2007", "#D81E05", "#F37324", "#F8CC1B", #die können eigentlich raus
              "#72B043", "#4D8126", "#D1D1C6", "#DDDDDD"),
    stringsAsFactors = FALSE
)
reihenfolge <- c("LC", "NT", "VU", "EN", "CR", "RE", "EX")

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
        
    ),
    # Header mit Logo
    div(class = "main-header",
        tags$img(src = "logo.png", height = "200px", style = "margin-bottom: 15px;"),
        div(class = "subtitle", "Interaktives Dashboard für gefährdete Arten")
    ),
    tabPanel("Lokale Artendaten",
             fluidRow(style = "margin-top: 20px;",
                      column(3, uiOutput("stat_gesamt_ui")),
                      column(3, uiOutput("stat_gefaehrdet_ui")),
                      column(3, uiOutput("stat_kritisch_ui")),
                      column(3, uiOutput("stat_prozent_ui"))
             ),
             fluidRow(
                 column(6, uiOutput("overview_status_ui")),
                 column(6, uiOutput("overview_tiergruppe_ui"))
             ),
             hr(),
             fluidRow(
                 column(3, 
                        selectInput("Tiergruppe", "Wähle eine Tiergruppe aus",
                                    choices = unique(arten_data$tiergruppe_deutsch)

                        ),
                        selectInput("Art", "Wähle eine Art aus",
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = FALSE)
                 ),
                 column(6, 
                        uiOutput("art_info_ui"),
                        uiOutput("art_image_ui"), 
                        uiOutput("art_map_ui")
                 column(3, 
                        uiOutput("art_info_ui")),
                 column(3,
                        uiOutput("art_image_ui")
                 )
             ),
             hr(),
             
             fluidRow(
                 column(12, uiOutput("arten_tabelle_ui"))
             )
    ),
    
)

# Function to get image URL from Wikimedia Commons
get_wikimedia_image <- function(scientific_name) {
    tryCatch({
        # URL encode the scientific name
        encoded_name <- URLencode(scientific_name)
        
        # First, get the page title from Wikipedia
        wiki_url <- paste0(
            "https://en.wikipedia.org/w/api.php?",
            "action=query&format=json&prop=pageimages&piprop=original&",
            "titles=", encoded_name
        )
        
        wiki_response <- httr::GET(wiki_url)
        wiki_data <- httr::content(wiki_response, "parsed")
        
        # Extract image URL from response
        pages <- wiki_data$query$pages
        page <- pages[[1]]
        
        if (!is.null(page$original$source)) {
            return(page$original$source)
        }
        
        # If no image found, try Wikimedia Commons search
        commons_url <- paste0(
            "https://commons.wikimedia.org/w/api.php?",
            "action=query&format=json&prop=imageinfo&iiprop=url&",
            "generator=search&gsrnamespace=6&gsrsearch=", encoded_name,
            "&gsrlimit=1"
        )
        
        commons_response <- httr::GET(commons_url)
        commons_data <- httr::content(commons_response, "parsed")
        
        if (!is.null(commons_data$query$pages)) {
            page <- commons_data$query$pages[[1]]
            if (!is.null(page$imageinfo[[1]]$url)) {
                return(page$imageinfo[[1]]$url)
            }
        }
        
        return(NULL)
    }, error = function(e) {
        return(NULL)
    })
}

get_gbif_occurrences <- function(scientific_name, limit = 500) {
    tryCatch({
        all_occurrences <- NULL
        countries <- c("AT", "DE", "CH")
        limit_per_country <- ceiling(limit / 3)
        
        for (country in countries) {
            occ_data <- occ_search(
                scientificName = scientific_name,
                hasCoordinate = TRUE,
                limit = limit_per_country,
                country = country
            )
            
            if (!is.null(occ_data$data) && nrow(occ_data$data) > 0) {
                country_data <- occ_data$data %>%
                    select(scientificName, decimalLatitude, decimalLongitude, 
                           eventDate, basisOfRecord, countryCode) %>%
                    filter(!is.na(decimalLatitude), !is.na(decimalLongitude))
                
                all_occurrences <- bind_rows(all_occurrences, country_data)
            }
        }
        
        return(all_occurrences)
        
    }, error = function(e) {
        message("GBIF Error: ", e$message)
        return(NULL)
    })
}
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Lokale Daten - Statistiken
    output$stat_gesamt_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "stat-box no-data",
                div(class = "stat-number no-data", "?"),
                div(class = "stat-label", "Erfasste Arten"))
        } else {
            div(class = "stat-box",
                div(class = "stat-number", nrow(arten_data)),
                div(class = "stat-label", "Erfasste Arten"))
        }
    })
    
    output$stat_gefaehrdet_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "stat-box no-data",
                div(class = "stat-number no-data", "?"),
                div(class = "stat-label", "Gefährdete Arten"))
        } else {
            gefaehrdet <- arten_data %>% filter(gefaehrdung %in% c("CR", "EN", "VU")) %>% nrow()
            div(class = "stat-box",
                div(class = "stat-number", gefaehrdet),
                div(class = "stat-label", "Gefährdete Arten"))
        }
    })
    
    output$stat_kritisch_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "stat-box no-data",
                div(class = "stat-number no-data", "?"),
                div(class = "stat-label", "Kritisch bedroht"))
        } else {
            kritisch <- arten_data %>% filter(gefaehrdung == "CR") %>% nrow()
            div(class = "stat-box",
                div(class = "stat-number", kritisch),
                div(class = "stat-label", "Kritisch bedroht"))
        }
    })
    
    output$stat_prozent_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "stat-box no-data",
                div(class = "stat-number no-data", "?"),
                div(class = "stat-label", "% Gefährdet"))
        } else {
            total <- nrow(arten_data)
            gefaehrdet <- arten_data %>% filter(gefaehrdung %in% c("CR", "EN", "VU")) %>% nrow()
            prozent <- round(gefaehrdet / total * 100)
            div(class = "stat-box",
                div(class = "stat-number", paste0(prozent, "%")),
                div(class = "stat-label", "% Gefährdet"))
        }
    })
    
    # Lokale Daten - Plots
    output$overview_status_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "info-box no-data",
                h3("Gefährdungsstatus nach Kategorie"),
                div(class = "no-data-overlay",
                    "⚠ Keine lokalen Artendaten verfügbar", br(),
                    "Bitte Excel-Datei laden"))
        } else {
            div(class = "info-box",
                h3("Gefährdungsstatus nach Kategorie"),
                plotlyOutput("overview_status_plot", height = "350px"))
        }
    })

    
    
    arten_count <- arten_data %>%
        filter(gefaehrdung %in% reihenfolge) %>%
        group_by(gefaehrdung) %>%
        summarise(Anzahl = n(), .groups = "drop") %>%
        complete(gefaehrdung = reihenfolge, fill = list(Anzahl = 0)) %>%
        left_join(kategorien %>% select(code, farbe, name), by = c("gefaehrdung" = "code")) %>%
        mutate(gefaehrdung = factor(gefaehrdung, levels = reihenfolge))
    
    # Plot
    barplotgefaehrdung<- ggplot(arten_count, aes(x = gefaehrdung, y = Anzahl, fill = farbe, text = name)) +
        geom_bar(stat = "identity") +
        scale_fill_identity() +
        labs(
            x = "Gefährdungskategorie",
            y = "Anzahl der Arten",
            title = "Anzahl der Arten pro Gefährdungskategorie"
        ) +
        theme_minimal() +
        theme(
            text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
  
   output$overview_status_plot <- renderPlotly({ggplotly(barplotgefaehrdung, tooltip = c("x", "y", "text"))})
    

    output$overview_tiergruppe_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "info-box no-data",
                h3("Verteilung nach Tiergruppen"),
                div(class = "no-data-overlay",
                    "⚠ Keine lokalen Artendaten verfügbar", br(),
                    "Bitte Excel-Datei laden"))
        } else {
            div(class = "info-box",
                h3("Verteilung nach Tiergruppen"),
                plotlyOutput("overview_tiergruppe_plot", height = "350px"))
        }
    })
    
    output$overview_tiergruppe_plot <- renderPlotly({
        req(arten_data)
        
        daten <- arten_data %>%
            group_by(tiergruppe_deutsch) %>%
            summarise(anzahl = n(), .groups = 'drop')
        
        plot_ly(daten, labels = ~tiergruppe_deutsch, values = ~anzahl, 
                type = 'pie',
                marker = list(colors = c('#DA2A1D', '#F37324', '#F8CC1B', '#72B043', '#80B0D5', '#0061AB')),
                textinfo = 'label+percent') %>%
            layout(showlegend = TRUE)
    })
    
    
    
    
    # Dynamische Aktualisierung der Art-Auswahl basierend auf der ausgewählten Tiergruppe
    observe({
        req(input$Tiergruppe)
        arten_in_tiergruppe <- arten_data %>%
            filter(tiergruppe_deutsch == input$Tiergruppe) %>%
            pull(deutscher_name)
        
        updateSelectInput(session, "Art",
                          choices = arten_in_tiergruppe,
                          selected = "")
    })
    
    # Art-Informationen
    output$art_info_ui <- renderUI({
        if (is.null(arten_data) || is.null(input$Art) || input$Art == "") {
            div(class = "info-box no-data",
                h3("Artinformationen"),
                div(class = "no-data-overlay",
                    "Keine Art ausgewählt"))
        } else {
            art_info <- arten_data %>%
                filter(deutscher_name == input$Art) %>%
                left_join(kategorien, by = c("gefaehrdung" = "code"))
            
            div(class = "info-box",
                h3("Artinformationen"),
                tags$strong("Deutscher Name: "), art_info$deutscher_name, br(),
                tags$strong("Wissenschaftlicher Name: "), art_info$wissenschaftlicher_name, br(),
                tags$strong("Tiergruppe: "), art_info$tiergruppe_deutsch, br(),
                tags$strong("Gefährdungsstatus: "), art_info$name
            )
        }
    })
    
    # Art-Bild von Wikimedia
    output$art_image_ui <- renderUI({
        if (is.null(arten_data) || is.null(input$Art) || input$Art == "") {
            return(NULL)
        }
        
        art_info <- arten_data %>%
            filter(deutscher_name == input$Art)
        
        if (nrow(art_info) == 0) {
            return(NULL)
        }
        
        wissenschaftlicher_name <- art_info$wissenschaftlicher_name
        
        # Get image URL
        image_url <- get_wikimedia_image(wissenschaftlicher_name)
        
        if (!is.null(image_url)) {
            div(class = "info-box species-image",
                h3("Artenfoto"),
                tags$img(src = image_url, 
                         alt = wissenschaftlicher_name,
                         style = "max-width: 100%; height: auto; border-radius: 8px;"),
                tags$p(style = "font-size: 0.8em; color: #666; margin-top: 10px;",
                       "Bild: Wikimedia Commons")
            )
        } else {
            div(class = "info-box no-data",
                h3("Artenfoto"),
                div(class = "no-data-overlay",
                    "Kein Bild verfügbar"))
        }
    })
    # Art-Verbreitungskarte von GBIF
    output$art_map_ui <- renderUI({
        if (is.null(arten_data) || is.null(input$Art) || input$Art == "") {
            return(NULL)
        }
        
        art_info <- arten_data %>%
            filter(deutscher_name == input$Art)
        
        if (nrow(art_info) == 0) {
            return(NULL)
        }
        
        div(class = "info-box species-map",
            h3("Verbreitungskarte"),
            leafletOutput(paste0("map_", gsub(" ", "_", input$Art)), 
                          height = "400px"),
            tags$p(style = "font-size: 0.8em; color: #666; margin-top: 10px;",
                   "Daten: GBIF (Global Biodiversity Information Facility)")
        )
    })
    
    # Art-Verbreitungskarte von GBIF
    observe({
        if (is.null(arten_data) || is.null(input$Art) || input$Art == "") {
            return(NULL)
        }
        
        art_info <- arten_data %>%
            filter(deutscher_name == input$Art)
        
        if (nrow(art_info) == 0) {
            return(NULL)
        }
        
        wissenschaftlicher_name <- art_info$wissenschaftlicher_name
        map_id <- paste0("map_", gsub(" ", "_", input$Art))
        
        # Get GBIF occurrences
        occurrences <- get_gbif_occurrences(wissenschaftlicher_name)
        
        output[[map_id]] <- renderLeaflet({
            if (is.null(occurrences) || nrow(occurrences) == 0) {
                # Show empty map centered on DACH region
                leaflet() %>%
                    addTiles() %>%
                    setView(lng = 10.5, lat = 47.5, zoom = 6) %>%  # Centered on DACH region
                    addControl(
                        html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
                        Keine Verbreitungsdaten verfügbar</div>",
                        position = "topright"
                    )
            } else {
                # Create map with occurrence points
                leaflet(occurrences) %>%
                    addTiles() %>%
                    addCircleMarkers(
                        lng = ~decimalLongitude,
                        lat = ~decimalLatitude,
                        radius = 5,
                        color = "#e74c3c",
                        fillColor = "#e74c3c",
                        fillOpacity = 0.6,
                        stroke = TRUE,
                        weight = 1,
                        clusterOptions = markerClusterOptions(),
                        popup = ~paste0(
                            "<strong>", scientificName, "</strong><br>",
                            "Land: ", ifelse(!is.na(countryCode), countryCode, "Unbekannt"), "<br>",
                            "Datum: ", ifelse(!is.na(eventDate), eventDate, "Unbekannt"), "<br>",
                            "Typ: ", basisOfRecord
                        )
                    ) %>%
                    addControl(
                        html = paste0("<div style='background: white; padding: 10px; border-radius: 5px;'>
                              <strong>", nrow(occurrences), " Fundorte (AT, DE, CH)</strong></div>"),
                        position = "topright"
                    )
            }
        })
    })
    # Artentabelle
    output$arten_tabelle_ui <- renderUI({
        if (is.null(arten_data)) {
            div(class = "info-box no-data",
                h3("Artenliste"),
                div(class = "no-data-overlay",
                    "⚠ Keine lokalen Artendaten verfügbar", br(),
                    "Bitte Excel-Datei 'arten.xlsx' im Arbeitsverzeichnis platzieren"))
        } else {
            div(class = "info-box",
                h3("Artenliste"),
                DT::dataTableOutput("arten_tabelle"))
        }
    })
    
    output$arten_tabelle <- DT::renderDataTable({
        req(arten_data)
        
        daten <- arten_data %>%
            left_join(kategorien, by = c("gefaehrdung" = "code")) %>%
            select(deutscher_name, wissenschaftlicher_name, name, tiergruppe_deutsch) %>%
            rename("Deutscher Name" = deutscher_name,
                   "Wissenschaftlicher Name" = wissenschaftlicher_name,
                   "Gefährdungsstatus" = name,
                   "Tiergruppe" = tiergruppe_deutsch)
        
        datatable(daten, options = list(
            pageLength = 15,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')
        ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
