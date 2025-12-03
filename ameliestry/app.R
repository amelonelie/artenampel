library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(readxl)
library(leaflet)
library(jsonlite)

# Funktion zum Laden der Excel-Datei
load_excel_data <- function(filepath = "/Users/amelonelie/Downloads/rote_liste_saeugetiere_2005.xlsx") {
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

# Lade Our World in Data - Red List Index
load_red_list_data <- function() {
  tryCatch({
    df <- read.csv("https://ourworldindata.org/grapher/red-list-index.csv?v=1&csvType=full&useColumnShortNames=true")
    metadata <- fromJSON("https://ourworldindata.org/grapher/red-list-index.metadata.json?v=1&csvType=full&useColumnShortNames=true")
    list(data = df, metadata = metadata)
  }, error = function(e) {
    message("Red List Index Daten konnten nicht geladen werden.")
    return(NULL)
  })
}

# Lade Our World in Data - Threatened Species
load_threatened_species_data <- function() {
  tryCatch({
    df <- read.csv("https://ourworldindata.org/grapher/number-species-threatened.csv?v=1&csvType=full&useColumnShortNames=true")
    metadata <- fromJSON("https://ourworldindata.org/grapher/number-species-threatened.metadata.json?v=1&csvType=full&useColumnShortNames=true")
    list(data = df, metadata = metadata)
  }, error = function(e) {
    message("Threatened Species Daten konnten nicht geladen werden.")
    return(NULL)
  })
}

red_list_data <- load_red_list_data()
threatened_species_data <- load_threatened_species_data()

# Gefährdungskategorien
kategorien <- data.frame(
  code = c("EX","RE", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE"),
  name = c("Ausgestorben", "Regional ausgestorben","In der Natur ausgestorben", "Vom Aussterben bedroht",
           "Stark gefährdet", "Gefährdet", "Potenziell gefährdet", 
           "Nicht gefährdet", "Unzureichende Datenlage", "Nicht bewertet"),
  farbe = c("#000000", "#3D1244","#3D1244", "#D81E05", "#FC7F3F", "#F9E814", 
            "#CCE226", "#60C659", "#D1D1C6", "#DDDDDD"),
  stringsAsFactors = FALSE
)

# UI Definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            body {
                font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
                background-color: #f5f5f5;
            }
            .main-header {
                background: linear-gradient(135deg, #DA2A1D 0%, #b02218 100%);
                color: white;
                padding: 30px 20px;
                margin: -15px -15px 20px -15px;
                box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                text-align: center;
            }
            .logo-placeholder {
                width: 100px;
                height: 100px;
                background-color: rgba(255,255,255,0.15);
                border: 3px dashed white;
                border-radius: 12px;
                display: inline-block;
                margin-bottom: 15px;
                line-height: 100px;
                text-align: center;
                font-size: 14px;
                font-weight: bold;
            }
            h1 {
                margin: 10px 0 5px 0;
                font-size: 3em;
                font-weight: 700;
                text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
            }
            .subtitle {
                font-size: 1.2em;
                opacity: 0.95;
                margin-top: 5px;
            }
            .info-box {
                background: white;
                border-radius: 12px;
                padding: 20px;
                margin: 10px 0;
                box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                border-left: 5px solid #DA2A1D;
                transition: transform 0.2s, box-shadow 0.2s;
            }
            .info-box:hover {
                transform: translateY(-2px);
                box-shadow: 0 4px 16px rgba(0,0,0,0.12);
            }
            .info-box.no-data {
                border-left-color: #999;
                background-color: #f9f9f9;
                opacity: 0.7;
            }
            .info-box h3 {
                color: #DA2A1D;
                margin-top: 0;
                font-size: 1.3em;
            }
            .info-box.no-data h3 {
                color: #999;
            }
            .stat-box {
                background: white;
                border-radius: 12px;
                padding: 25px;
                text-align: center;
                box-shadow: 0 2px 12px rgba(0,0,0,0.08);
                transition: transform 0.2s;
                height: 140px;
                display: flex;
                flex-direction: column;
                justify-content: center;
            }
            .stat-box:hover {
                transform: translateY(-4px);
                box-shadow: 0 6px 20px rgba(0,0,0,0.15);
            }
            .stat-box.no-data {
                background-color: #f9f9f9;
                opacity: 0.7;
            }
            .stat-number {
                font-size: 2.8em;
                font-weight: bold;
                color: #DA2A1D;
                line-height: 1;
                margin-bottom: 8px;
            }
            .stat-number.no-data {
                color: #999;
            }
            .stat-label {
                color: #666;
                font-size: 1em;
                font-weight: 500;
            }
            .no-data-overlay {
                text-align: center;
                padding: 40px;
                color: #999;
                font-size: 1.1em;
            }
            .well {
                background-color: #f8f9fa;
                border: 1px solid #e9ecef;
                border-radius: 8px;
            }
        "))
  ),
  
  # Header mit Logo
  div(class = "main-header",
      tags$img(src = "logo.png", height = "100px", style = "margin-bottom: 15px;"),
      h1("Artenampel"),
      div(class = "subtitle", "Interaktives Dashboard für gefährdete Arten")
  ),
  
  # Tab-basiertes Layout
  tabsetPanel(id = "main_tabs",
              
              # Tab 1: Übersicht Lokale Daten
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
                         column(12, uiOutput("arten_tabelle_ui"))
                       )
              ),
              
              # Tab 2: Globale Trends - Red List Index
              tabPanel("Red List Index",
                       fluidRow(style = "margin-top: 20px;",
                                column(12, uiOutput("red_list_info_ui"))
                       ),
                       fluidRow(
                         column(12, uiOutput("red_list_plot_ui"))
                       ),
                       fluidRow(
                         column(6, uiOutput("red_list_regions_ui")),
                         column(6, uiOutput("red_list_recent_ui"))
                       )
              ),
              
              # Tab 3: Bedrohte Arten Weltweit
              tabPanel("Bedrohte Arten Weltweit",
                       fluidRow(style = "margin-top: 20px;",
                                column(12, uiOutput("threatened_info_ui"))
                       ),
                       fluidRow(
                         column(12, uiOutput("threatened_trend_ui"))
                       ),
                       fluidRow(
                         column(6, uiOutput("threatened_by_country_ui")),
                         column(6, uiOutput("threatened_recent_ui"))
                       )
              ),
              
              # Tab 4: Detailanalyse
              tabPanel("Detailanalyse",
                       fluidRow(style = "margin-top: 20px;",
                                column(3, uiOutput("filter_panel_ui")),
                                column(9, uiOutput("detail_plots_ui"))
                       )
              ),
              
              # Tab 5: Info & Quellen
              tabPanel("Info & Datenquellen",
                       fluidRow(style = "margin-top: 20px;",
                                column(12,
                                       div(class = "info-box",
                                           h3("Über das Artenampel-Dashboard"),
                                           p("Dieses Dashboard visualisiert Daten über gefährdete Arten aus verschiedenen Quellen."),
                                           
                                           h4("Datenquellen:"),
                                           tags$ul(
                                             tags$li(strong("Lokale Artendaten:"), " Excel-Datei mit österreichischen Arten"),
                                             tags$li(strong("Our World in Data - Red List Index:"), 
                                                     tags$a(href = "https://ourworldindata.org/grapher/red-list-index", 
                                                            "https://ourworldindata.org/grapher/red-list-index", 
                                                            target = "_blank")),
                                             tags$li(strong("Our World in Data - Threatened Species:"), 
                                                     tags$a(href = "https://ourworldindata.org/grapher/number-species-threatened", 
                                                            "https://ourworldindata.org/grapher/number-species-threatened", 
                                                            target = "_blank"))
                                           ),
                                           
                                           h4("Gefährdungskategorien (IUCN Red List):"),
                                           tags$ul(
                                             tags$li(strong("EX - Ausgestorben:"), " Keine lebenden Individuen mehr vorhanden"),
                                             tags$li(strong("EW - In der Natur ausgestorben:"), " Nur noch in Gefangenschaft"),
                                             tags$li(strong("CR - Vom Aussterben bedroht:"), " Extrem hohes Aussterberisiko"),
                                             tags$li(strong("EN - Stark gefährdet:"), " Sehr hohes Aussterberisiko"),
                                             tags$li(strong("VU - Gefährdet:"), " Hohes Aussterberisiko"),
                                             tags$li(strong("NT - Potenziell gefährdet:"), " Nahe der Gefährdung"),
                                             tags$li(strong("LC - Nicht gefährdet:"), " Geringes Aussterberisiko"),
                                             tags$li(strong("DD - Unzureichende Datenlage:"), " Zu wenig Information"),
                                             tags$li(strong("NE - Nicht bewertet:"), " Noch nicht evaluiert")
                                           ),
                                           
                                           h4("Hinweise zu grau markierten Bereichen:"),
                                           p("Grau dargestellte Visualisierungen bedeuten, dass für diesen Bereich noch keine echten Datenquellen vorhanden sind oder die Daten nicht geladen werden konnten.")
                                       )
                                )
                       )
              )
  )
)

# Server Logic
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
  
  output$overview_status_plot <- renderPlotly({
    req(arten_data)
    
    daten <- arten_data %>%
      group_by(gefaehrdung) %>%
      summarise(anzahl = n(), .groups = 'drop') %>%
      left_join(kategorien, by = c("gefaehrdung" = "code"))
    
    plot_ly(daten, x = ~name, y = ~anzahl, type = 'bar',
            marker = list(color = ~farbe),
            text = ~paste(anzahl, "Arten"),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Anzahl Arten"),
             showlegend = FALSE)
  })
  
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
            marker = list(colors = c('#DA2A1D', '#FC7F3F', '#F9E814', '#60C659')),
            textinfo = 'label+percent') %>%
      layout(showlegend = TRUE)
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
  
  # Red List Index
  output$red_list_info_ui <- renderUI({
    if (is.null(red_list_data)) {
      div(class = "info-box no-data",
          h3("Red List Index - Information"),
          div(class = "no-data-overlay",
              "⚠ Red List Index Daten konnten nicht geladen werden", br(),
              "Bitte Internetverbindung prüfen"))
    } else {
      div(class = "info-box",
          h3("Red List Index - Globale Entwicklung"),
          p("Der Red List Index misst Veränderungen im Aussterberisiko von Arten über die Zeit. Ein Wert von 1.0 bedeutet, dass alle Arten als 'Least Concern' eingestuft sind. Ein Wert von 0 bedeutet, dass alle Arten ausgestorben sind."))
    }
  })
  
  output$red_list_plot_ui <- renderUI({
    if (is.null(red_list_data)) {
      div(class = "info-box no-data",
          h3("Red List Index über Zeit"),
          div(class = "no-data-overlay", "⚠ Keine Daten verfügbar"))
    } else {
      div(class = "info-box",
          h3("Red List Index über Zeit"),
          plotlyOutput("red_list_plot", height = "400px"))
    }
  })
  
  output$red_list_plot <- renderPlotly({
    req(red_list_data)
    
    world_data <- red_list_data$data %>%
      filter(Entity == "World") %>%
      arrange(Year)
    
    plot_ly(world_data, x = ~Year, y = ~get(names(world_data)[4]),
            type = 'scatter', mode = 'lines+markers',
            fill = 'tozeroy',
            fillcolor = 'rgba(218, 42, 29, 0.2)',
            line = list(color = '#DA2A1D', width = 3),
            marker = list(size = 6)) %>%
      layout(xaxis = list(title = "Jahr"),
             yaxis = list(title = "Red List Index", range = c(0, 1)),
             hovermode = 'x unified')
  })
  
  output$red_list_regions_ui <- renderUI({
    if (is.null(red_list_data)) {
      div(class = "info-box no-data",
          h3("Vergleich nach Regionen"),
          div(class = "no-data-overlay", "⚠ Keine Daten verfügbar"))
    } else {
      div(class = "info-box",
          h3("Vergleich nach Regionen (neueste Daten)"),
          plotlyOutput("red_list_regions_plot", height = "350px"))
    }
  })
  
  output$red_list_regions_plot <- renderPlotly({
    req(red_list_data)
    
    latest_data <- red_list_data$data %>%
      group_by(Entity) %>%
      filter(Year == max(Year)) %>%
      ungroup() %>%
      filter(!Entity %in% c("World")) %>%
      arrange(desc(get(names(red_list_data$data)[4]))) %>%
      head(15)
    
    plot_ly(latest_data, 
            y = ~reorder(Entity, get(names(latest_data)[4])), 
            x = ~get(names(latest_data)[4]),
            type = 'bar', orientation = 'h',
            marker = list(color = '#DA2A1D')) %>%
      layout(yaxis = list(title = ""),
             xaxis = list(title = "Red List Index"))
  })
  
  output$red_list_recent_ui <- renderUI({
    if (is.null(red_list_data)) {
      div(class = "info-box no-data",
          h3("Aktuelle Entwicklung"),
          div(class = "no-data-overlay", "⚠ Keine Daten verfügbar"))
    } else {
      div(class = "info-box",
          h3("Entwicklung der letzten 10 Jahre"),
          plotlyOutput("red_list_recent_plot", height = "350px"))
    }
  })
  
  output$red_list_recent_plot <- renderPlotly({
    req(red_list_data)
    
    recent_years <- red_list_data$data %>%
      filter(Year >= max(Year) - 10)
    
    top_entities <- recent_years %>%
      group_by(Entity) %>%
      summarise(avg_val = mean(get(names(recent_years)[4]), na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(avg_val)) %>%
      head(6) %>%
      pull(Entity)
    
    plot_data <- recent_years %>%
      filter(Entity %in% top_entities)
    
    plot_ly(plot_data, x = ~Year, y = ~get(names(plot_data)[4]), 
            color = ~Entity, type = 'scatter', mode = 'lines+markers',
            colors = c('#DA2A1D', '#FC7F3F', '#F9E814', '#60C659', '#3D1244', '#999')) %>%
      layout(xaxis = list(title = "Jahr"),
             yaxis = list(title = "Red List Index"),
             hovermode = 'x unified')
  })
  
  # Threatened Species
  output$threatened_info_ui <- renderUI({
    if (is.null(threatened_species_data)) {
      div(class = "info-box no-data",
          h3("Bedrohte Arten - Information"),
          div(class = "no-data-overlay",
              "⚠ Daten zu bedrohten Arten konnten nicht geladen werden", br(),
              "Bitte Internetverbindung prüfen"))
    } else {
      div(class = "info-box",
          h3("Anzahl bedrohter Arten weltweit"),
          p("Diese Visualisierung zeigt die Entwicklung der Anzahl bedrohter Arten über die Zeit."))
    }
  })
  
  output$threatened_trend_ui <- renderUI({
    if (is.null(threatened_species_data)) {
      div(class = "info-box no-data",
          h3("Entwicklung bedrohter Arten"),
          div(class = "no-data-overlay", "⚠ Keine Daten verfügbar"))
    } else {
      div(class = "info-box",
          h3("Entwicklung bedrohter Arten über Zeit"),
          plotlyOutput("threatened_trend_plot", height = "400px"))
    }
  })
  
  output$threatened_trend_plot <- renderPlotly({
    req(threatened_species_data)
    
    world_data <- threatened_species_data$data %>%
      filter(Entity == "World") %>%
      arrange(Year)
    
    plot_ly(world_data, x = ~Year, y = ~get(names(world_data)[4]),
            type = 'scatter', mode = 'lines+markers',
            fill = 'tozeroy',
            fillcolor = 'rgba(218, 42, 29, 0.2)',
            line = list(color = '#DA2A1D', width = 3),
            marker = list(size = 6)) %>%
      layout(xaxis = list(title = "Jahr"),
             yaxis = list(title = "Anzahl bedrohter Arten"),
             hovermode = 'x unified')
  })
  
  output$threatened_by_country_ui <- renderUI({
    if (is.null(threatened_species_data)) {
      div(class = "info-box no-data",
          h3("Top Länder"),
          div(class = "no-data-overlay", "⚠ Keine Daten verfügbar"))
    } else {
      div(class = "info-box",
          h3("Top 15 Länder (neueste Daten)"),
          plotlyOutput("threatened_by_country_plot", height = "400px"))
    }
  })
  
  output$threatened_by_country_plot <- renderPlotly({
    req(threatened_species_data)
    
    latest_data <- threatened_species_data$data %>%
      group_by(Entity) %>%
      filter(Year == max(Year)) %>%
      ungroup() %>%
      filter(!Entity %in% c("World")) %>%
      arrange(desc(get(names(threatened_species_data$data)[4]))) %>%
      head(15)
    
    plot_ly(latest_data, 
            y = ~reorder(Entity, get(names(latest_data)[4])), 
            x = ~get(names(latest_data)[4]),
            type = 'bar', orientation = 'h',
            marker = list(color = '#DA2A1D')) %>%
      layout(yaxis = list(title = ""),
             xaxis = list(title = "Anzahl bedrohter Arten"))
  })
  
  output$threatened_recent_ui <- renderUI({
    if (is.null(threatened_species_data)) {
      div(class = "info-box no-data",
          h3("Regionale Entwicklung"),
          div(class = "no-data-overlay", "⚠ Keine Daten verfügbar"))
    } else {
      div(class = "info-box",
          h3("Entwicklung ausgewählter Länder"),
          plotlyOutput("threatened_recent_plot", height = "400px"))
    }
  })
  
  output$threatened_recent_plot <- renderPlotly({
    req(threatened_species_data)
    
    selected_countries <- c("Austria", "Germany", "Switzerland", "United States", 
                            "Brazil", "Australia", "China")
    
    plot_data <- threatened_species_data$data %>%
      filter(Entity %in% selected_countries, Year >= 1990)
    
    plot_ly(plot_data, x = ~Year, y = ~get(names(plot_data)[4]), 
            color = ~Entity, type = 'scatter', mode = 'lines+markers',
            colors = c('#DA2A1D', '#FC7F3F', '#F9E814', '#60C659', '#3D1244', '#999', '#000')) %>%
      layout(xaxis = list(title = "Jahr"),
             yaxis = list(title = "Anzahl bedrohter Arten"),
             hovermode = 'x unified')
  })
  
  # Detailanalyse - Plots
  output$detail_plots_ui <- renderUI({
    if (is.null(arten_data)) {
      div(class = "info-box no-data",
          h3("Detailanalyse"),
          div(class = "no-data-overlay",
              "⚠ Detailanalyse nicht verfügbar", br(),
              "Bitte lokale Artendaten laden"))
    } else {
      tagList(
        div(class = "info-box",
            h3("Gefilterte Artenliste"),
            DT::dataTableOutput("detail_tabelle")
        ),
        div(class = "info-box",
            h3("Verteilung der gefilterten Arten"),
            plotlyOutput("detail_plot", height = "350px")
        )
      )
    }
  })
  
  # Gefilterte Daten
  gefilterte_daten <- reactive({
    req(arten_data)
    daten <- arten_data
    
    if (!is.null(input$filter_tiergruppe) && input$filter_tiergruppe != "alle") {
      daten <- daten %>% filter(tiergruppe_deutsch == input$filter_tiergruppe)
    }
    
    if (!is.null(input$filter_gefaehrdung) && length(input$filter_gefaehrdung) > 0) {
      daten <- daten %>% filter(gefaehrdung %in% input$filter_gefaehrdung)
    }
    
    daten
  })
  
  output$detail_tabelle <- DT::renderDataTable({
    daten <- gefilterte_daten() %>%
      left_join(kategorien, by = c("gefaehrdung" = "code")) %>%
      select(deutscher_name, wissenschaftlicher_name, name, tiergruppe_deutsch) %>%
      rename("Deutscher Name" = deutscher_name,
             "Wissenschaftlicher Name" = wissenschaftlicher_name,
             "Gefährdungsstatus" = name,
             "Tiergruppe" = tiergruppe_deutsch)
    
    datatable(daten, options = list(
      pageLength = 10,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')
    ))
  })
  
  output$detail_plot <- renderPlotly({
    daten <- gefilterte_daten() %>%
      group_by(gefaehrdung) %>%
      summarise(anzahl = n(), .groups = 'drop') %>%
      left_join(kategorien, by = c("gefaehrdung" = "code"))
    
    plot_ly(daten, x = ~name, y = ~anzahl, type = 'bar',
            marker = list(color = ~farbe),
            text = ~paste(anzahl, "Arten"),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Anzahl Arten"),
             showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)