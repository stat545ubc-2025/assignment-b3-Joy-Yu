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
        "Explore street trees in Vancouver using the vancouver_trees dataset.",
        "Use the controls below to filter trees by neighbourhood, genus, and diameter."
      ),

      # select neighbourhood
      selectInput(
        inputId = "neighbourhood",
        label   = "Neighbourhood:",
        choices = c("All", sort(unique(vancouver_trees$neighbourhood_name))),
        selected = "All"
      ),

      # select street name
      selectInput(
        inputId = "on_street",
        label   = "Street Name:",
        choices = c("All", sort(streets_over_100)),
        selected = "All"
      ),

      # select genus
      selectInput(
        inputId = "genus",
        label   = "Genus:",
        choices = c("All", sort(unique(vancouver_trees$genus_name))),
        selected = "All"
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
      # rab panel
      tabsetPanel(
        tabPanel(
          title = "Plot",
          br(),
          p("Tree Genus Distribution"),
          plotOutput("location_plot")
        ),
        tabPanel(
          title = "Table",
          br(),
          textOutput("result_count"),
          br(),
          DT::dataTableOutput("tree_table"),
          br(),
          downloadButton("download_data", "Download filtered data (CSV)")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  filtered_trees <- reactive({
    data <- vancouver_trees

    # filtered by neighbourhood
    if (input$neighbourhood != "All") {
      data <- data %>%
        filter(neighbourhood_name == input$neighbourhood)
    }

    # filtered by on_street
    if (input$on_street != "All") {
      data <- data %>%
        filter(on_street == input$on_street)
    }

    # filtered by genus
    if (input$genus != "All") {
      data <- data %>%
        filter(genus_name == input$genus)
    }

    # filtered by longitude
    data <- data %>%
      filter(
        longitude >= input$longitude_range[1],
        longitude <= input$longitude_range[2]
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
      theme_minimal()
  })

  # dynamic table
  output$tree_table <- DT::renderDataTable({
    data <- filtered_trees()

    data %>%
      select(
        tree_id,
        common_name,
        genus_name,
        species_name,
        diameter,
        height_range_id,
        neighbourhood_name,
        std_street,
        on_street
      )
  })

  # count
  output$result_count <- renderText({
    n <- nrow(filtered_trees())
    paste("We found", n, "trees that match your filters.")
  })

  # download
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
