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

# Funktion zum Laden der Excel-Datei
load_excel_data <- function(filepath = "C:/Users/oscha/Documents/GitHub/artenampel/ameliestry/rote_liste_saeugetiere_2005.xlsx") {
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
             fluidRow(
                 column(6, 
                        selectInput("Tiergruppe", "Wähle eine Tiergruppe aus",
                                    choices = unique(arten_data$tiergruppe_deutsch)

                        ),
                        selectInput("Art", "Wähle eine Art aus",
                                    choices = NULL,
                                    selected = NULL,
                                    multiple = FALSE)
                 ),
                 column(6, uiOutput("art_info_ui"))
             ),
             fluidRow(
                 column(12, uiOutput("arten_tabelle_ui"))
             )
    ),
    
)

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
