# app.R

library(shiny)
library(datateachr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(sf)

# remove rows with missing data
vancouver_trees <- vancouver_trees %>%
  drop_na()

# load map data
neigh <- sf::st_read(
  "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/local-area-boundary/exports/geojson?lang=en&timezone=America%2FLos_Angeles"
)

# only shows the streets with more than 100 trees on the select
streets_over_100 <- vancouver_trees %>%
  count(on_street) %>%
  filter(n >= 100) %>%
  pull(on_street)

# ---- UI ----
ui <- fluidPage(
  # title
  titlePanel("Vancouver Trees Explorer"),

  sidebarLayout(
    sidebarPanel(
      helpText(
        "Explore trees in Vancouver using the vancouver_trees dataset.",
        "Use the controls below to filter trees by location and genus, and find the place that you would like to live in!"
      ),

      # select neighbourhood
      selectizeInput(
        inputId = "neighbourhood",
        label   = "Neighbourhood:",
        choices = sort(unique(vancouver_trees$neighbourhood_name)),
        multiple = TRUE,
        options = list(
          placeholder = "Select one or more neighbourhoods"
        )
      ),

      # select street name
      selectizeInput(
        inputId = "on_street",
        label   = "Street Name:",
        choices = sort(streets_over_100),
        multiple = TRUE,
        options = list(
          placeholder = "Select one or more streets"
        )
      ),

      # select genus
      selectizeInput(
        inputId = "genus",
        label   = "Genus:",
        choices = sort(unique(vancouver_trees$genus_name)),
        multiple = TRUE,
        options = list(
          placeholder = "Select one or more genus"
        )
      ),

      # select longitude range
      sliderInput(
        inputId = "longitude_range",
        label   = "longitude range:",
        min     = min(vancouver_trees$longitude, na.rm = TRUE),
        max     = max(vancouver_trees$longitude, na.rm = TRUE),
        value   = c(
          quantile(vancouver_trees$longitude, 0.0, na.rm = TRUE),
          quantile(vancouver_trees$longitude, 1.0, na.rm = TRUE)
        )
      ),

      # select latitude range
      sliderInput(
        inputId = "latitude_range",
        label   = "latitude range:",
        min     = min(vancouver_trees$latitude, na.rm = TRUE),
        max     = max(vancouver_trees$latitude, na.rm = TRUE),
        value   = c(
          quantile(vancouver_trees$latitude, 0.0, na.rm = TRUE),
          quantile(vancouver_trees$latitude, 1.0, na.rm = TRUE)
        )
      )
    ),

    mainPanel(
      br(),
      textOutput("result_count"),
      br(),
      tabsetPanel(
        tabPanel(
          title = "Tree Genus Location Map",
          br(),
          plotOutput("location_plot"),
          fluidRow(
            column(
              width = 6,
              h4("Neighbourhoods"),
              verbatimTextOutput("neighbourhood_list")
            ),
            column(
              width = 6,
              h4("Streets"),
              verbatimTextOutput("street_list")
            )
          ),
        ),
        tabPanel(
          title = "Overview Table by Neighbourhood",
          br(),
          DT::dataTableOutput("neighbourhood_overview_table"),
        ),
        tabPanel(
          title = "Overview Table by Street",
          br(),
          DT::dataTableOutput("street_overview_table"),
        ),
      ),
      br(),
      downloadButton("download_data", "Download filtered data (CSV)"),
      br(),
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  filtered_trees <- reactive({
    data <- vancouver_trees

    # filtered by neighbourhood
    if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0) {
      data <- data %>%
        filter(neighbourhood_name %in% input$neighbourhood)
    }

    # filtered by on_street
    if (!is.null(input$on_street) && length(input$on_street) > 0) {
      data <- data %>%
        filter(on_street %in% input$on_street)
    }

    # filtered by genus
    if (!is.null(input$genus) && length(input$genus) > 0) {
      data <- data %>%
        filter(genus %in% input$genus)
    }

    # filtered by longitude
    data <- data %>%
      filter(
        longitude >= input$longitude_range[1],
        longitude <= input$longitude_range[2]
      )

    # filtered by latitude
    data <- data %>%
      filter(
        latitude >= input$latitude_range[1],
        latitude <= input$latitude_range[2]
      )

    data
  })

  # location map
  output$location_plot <- renderPlot({
    data <- filtered_trees() %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    if (nrow(data) == 0) {
      plot.new()
      title("No trees found for the selected filters.")
      return()
    }

    ggplot() +
      geom_sf(data = neigh, fill = "white", color = "grey40", size = 0.4) +
      geom_sf(data = data, aes(color = genus_name), alpha = 0.6, size = 1) +
      labs(x = "Longitude",
           y = "Latitude",
           color = "Genus") +
      theme_minimal() +
      # adjust the map if longitude_range and latitude_range are changed
      coord_sf(
        xlim = input$longitude_range,
        ylim = input$latitude_range
      )
  })

  # neighbourhood list text
  output$neighbourhood_list <- renderText({
    data <- filtered_trees()

    if (nrow(data) == 0) {
      return("No neighbourhoods available.")
    }

    neighbourhoods <- sort(unique(data$neighbourhood_name))

    if (length(neighbourhoods) > 10) {
      extra <- length(neighbourhoods) - 10
      return(
        paste(
          paste(neighbourhoods[1:10], collapse = "\n"),
          paste0("... and ", extra, " more"),
          sep = "\n"
        )
      )
    }

    paste(neighbourhoods, collapse = "\n")
  })

  # street list text
  output$street_list <- renderText({
    data <- filtered_trees()

    if (nrow(data) == 0) {
      return("No streets available.")
    }

    streets <- sort(unique(data$on_street))

    if (length(streets) > 10) {
      extra <- length(streets) - 10
      return(
        paste(
          paste(streets[1:10], collapse = "\n"),
          paste0("... and ", extra, " more"),
          sep = "\n"
        )
      )
    }

    paste(streets, collapse = "\n")
  })

  # overview table by neighbourhood
  output$neighbourhood_overview_table <- DT::renderDataTable({
    data <- filtered_trees()

    data %>%
      group_by(neighbourhood_name) %>%
      summarise(
        number_of_trees = n(),
        number_of_genera = n_distinct(genus_name)
      ) %>%
      arrange(desc(number_of_trees))
  })

  # overview table by street
  output$street_overview_table <- DT::renderDataTable({
    data <- filtered_trees()

    data %>%
      group_by(on_street) %>%
      summarise(
        number_of_trees = n(),
        number_of_genera = n_distinct(genus_name)
      ) %>%
      arrange(desc(number_of_trees))
  })

  # count
  output$result_count <- renderText({
    n <- nrow(filtered_trees())
    paste(n, "trees that match your filter are found.")
  })

  # download the filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      "vancouver_trees_filtered.csv"
    },
    content = function(file) {
      write.csv(filtered_trees(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
