# --- MigraFISH Shiny database explorer with map -------------------------------
#
# This app reads the public MigraFISH database release and visualises species'
# Pfafstetter level-5 basin distributions using HydroBASINS level-5 polygons.
#
# Expected files:
# - output/MigraFISH_v2026.1.csv
# - data/hybas_[af/ar/as/au/eu/gr/na/sa/si]_lev05_v1c.shp and associated files

library(shiny)
library(DT)
library(tidyverse)
library(sf)
library(leaflet)
library(here)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

split_choices <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(character(0))
  }

  sort(
    unique(
      trimws(
        unlist(str_split(as.character(x), ";"))
      )
    )
  )
}

# --- Read database ------------------------------------------------------------

db <- read_csv(
  here("output", "MigraFISH_v2026.1.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# Convert count field to numeric for summaries
db <- db %>%
  mutate(
    count_final_pfaf5_ids = as.integer(count_final_pfaf5_ids),
    Confidence = as.character(Confidence)
  )

# Make sure optional columns exist
if (!"hydrobasin_regions" %in% names(db)) db$hydrobasin_regions <- NA_character_
if (!"pfaf3_regions" %in% names(db)) db$pfaf3_regions <- NA_character_

species_choices <- sort(unique(na.omit(db$Species)))

# --- Load HydroBASINS level-5 polygons ---------------------------------------

continents <- c("af", "ar", "as", "au", "eu", "gr", "na", "sa", "si")

continent_lookup <- tibble(
  continent = c("af", "ar", "as", "au", "eu", "gr", "na", "sa", "si"),
  continent_name = c(
    "Africa", "Arctic", "Asia", "Australia", "Europe",
    "Greenland", "North America", "South America", "Siberia"
  )
)

hybas5 <- continents %>%
  map_dfr(~ st_read(
    here("data", paste0("hybas_", .x, "_lev05_v1c.shp")),
    quiet = TRUE
  ) %>%
    mutate(
      HYBAS_ID = as.character(HYBAS_ID),
      PFAF_ID = as.character(PFAF_ID),
      continent = .x
    ) %>%
    select(HYBAS_ID, PFAF_ID, continent, geometry)) %>%
  left_join(continent_lookup, by = "continent") %>%
  st_make_valid()

# Do not simplify if this creates geometry artefacts
# hybas5 <- hybas5 %>%
#   st_simplify(dTolerance = 0.005, preserveTopology = TRUE)

# --- User interface -----------------------------------------------------------

ui <- fluidPage(

  titlePanel("MigraFISH database explorer"),

  fluidRow(
    column(
      width = 12,
      h4("MigraFISH: A global database of freshwater migratory fishes integrating behaviour and spatial distribution"),

      p(HTML(
        "<strong>Twan Stoffers</strong> (corresponding author)<br>
        Department of Community and Ecosystem Ecology, Leibniz Institute of Freshwater Ecology and Inland Fisheries (IGB), Berlin, Germany<br>
        Aquaculture Biology and Fisheries Ecology Group, Wageningen University & Research (WUR), Wageningen, The Netherlands<br><br>
        <strong>Email:</strong> <a href='mailto:twan.stoffers@wur.nl'>twan.stoffers@wur.nl</a><br>
        <strong>ORCID:</strong> <a href='https://orcid.org/0000-0002-2329-3032' target='_blank'>0000-0002-2329-3032</a>"
      )),

      p("This application provides an interactive explorer for the MigraFISH database, allowing users to browse migratory fish species, inspect their migration characteristics, and visualise their spatial distributions across Pfafstetter level-5 basins derived from HydroBASINS."),

      p(HTML(
        "<strong>Help improve MigraFISH</strong><br>
        MigraFISH is intended as a living database and will be updated regularly. Users are encouraged to submit corrections, additional species records, updated migration classifications, improved spatial distributions, taxonomic updates, or relevant literature."
      )),

      hr()
    )
  ),

  tabsetPanel(

    tabPanel(
      "Database Explorer",

      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            "species",
            "Select species",
            choices = species_choices,
            selected = if (length(species_choices) > 0) species_choices[1] else NULL,
            options = list(placeholder = "Type species name")
          ),

          selectInput(
            "migration_class",
            "Migration class",
            choices = c("All", sort(unique(na.omit(db$Migration_class)))),
            selected = "All"
          ),

          selectInput(
            "threat",
            "IUCN threat category",
            choices = c("All", sort(unique(na.omit(db$Threat_Category)))),
            selected = "All"
          ),

          selectInput(
            "length_km",
            "Migration length",
            choices = c("All", sort(unique(na.omit(db$Length_km)))),
            selected = "All"
          ),

          selectInput(
            "confidence",
            "Confidence",
            choices = c("All", sort(unique(na.omit(db$Confidence)))),
            selected = "All"
          ),

          selectizeInput(
            "hydro_region",
            "HydroBASINS region / continent",
            choices = c("All", split_choices(db$hydrobasin_regions)),
            selected = "All"
          ),

          selectizeInput(
            "pfaf3_region",
            "Pfafstetter level-3 region",
            choices = c("All", split_choices(db$pfaf3_regions)),
            selected = "All",
            options = list(placeholder = "Type Pfaf3 code")
          ),

          hr(),

          downloadButton(
            "download_filtered",
            "Download filtered database"
          ),

          br(), br(),

          downloadButton(
            "download_full",
            "Download full database"
          )
        ),

        mainPanel(
          h3("Database summary"),
          fluidRow(
            column(3, strong(textOutput("n_species")), br(), "Species"),
            column(3, strong(textOutput("n_spatial")), br(), "Species with spatial data"),
            column(3, strong(textOutput("n_basins")), br(), "Occupied Pfaf5 basins"),
            column(3, strong(textOutput("n_threatened")), br(), "Threatened species")
          ),
          hr(),

          h3(textOutput("species_title")),
          verbatimTextOutput("species_info"),
          leafletOutput("species_map", height = 600),
          br(),
          DTOutput("table")
        )
      )
    ),

    tabPanel(
      "Submit Update",

      h3("Submit an update to MigraFISH"),

      p("Please use this form to suggest corrections, new records, updated migration classifications, improved spatial information, taxonomic changes, or additional literature."),

      textInput("submit_name", "Your name"),
      textInput("submit_affiliation", "Affiliation"),
      textInput("submit_email", "Your email"),
      textInput("submit_species", "Species name"),

      selectInput(
        "submit_type",
        "Type of update",
        choices = c(
          "Migration classification",
          "Spatial distribution",
          "Taxonomy",
          "Additional literature",
          "New species",
          "Other"
        )
      ),

      textAreaInput(
        "submit_comments",
        "Description of update",
        rows = 8,
        placeholder = "Please describe the suggested update and include references or evidence where possible."
      ),

      fileInput(
        "submit_file",
        "Optional supporting file",
        accept = c(
          ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".csv", ".txt",
          ".zip", ".shp", ".dbf", ".shx", ".prj"
        )
      ),

      p(HTML(
        "<em>Note:</em> The email button opens your email client with the update details pre-filled.
        Please manually attach the selected supporting file before sending."
      )),

      uiOutput("submit_email_button")
    )
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  filtered_db <- reactive({
    x <- db

    migration_class <- input$migration_class %||% "All"
    threat <- input$threat %||% "All"
    length_km <- input$length_km %||% "All"
    confidence <- input$confidence %||% "All"
    hydro_region <- input$hydro_region %||% "All"
    pfaf3_region <- input$pfaf3_region %||% "All"

    if (length(migration_class) > 0 && migration_class != "All") {
      x <- x %>% filter(Migration_class == migration_class)
    }

    if (length(threat) > 0 && threat != "All") {
      x <- x %>% filter(Threat_Category == threat)
    }

    if (length(length_km) > 0 && length_km != "All") {
      x <- x %>% filter(Length_km == length_km)
    }

    if (length(confidence) > 0 && confidence != "All") {
      x <- x %>% filter(Confidence == confidence)
    }

    if (length(hydro_region) > 0 && hydro_region != "All") {
      x <- x %>% filter(str_detect(hydrobasin_regions, fixed(hydro_region)))
    }

    if (length(pfaf3_region) > 0 && pfaf3_region != "All") {
      x <- x %>% filter(str_detect(pfaf3_regions, fixed(pfaf3_region)))
    }

    x
  })

  output$n_species <- renderText({
    n_distinct(db$Species)
  })

  output$n_spatial <- renderText({
    sum(!is.na(db$final_pfaf5_ids))
  })

  output$n_basins <- renderText({
    db %>%
      separate_rows(final_pfaf5_ids, sep = ";") %>%
      filter(!is.na(final_pfaf5_ids), final_pfaf5_ids != "") %>%
      summarise(n = n_distinct(final_pfaf5_ids)) %>%
      pull(n)
  })

  output$n_threatened <- renderText({
    db %>%
      filter(Threat_Category %in% c("CR", "EN", "VU")) %>%
      summarise(n = n_distinct(Species)) %>%
      pull(n)
  })

  selected_species_data <- reactive({
    req(input$species)
    db %>% filter(Species == input$species)
  })

  selected_species_polygons <- reactive({
    req(input$species)

    sp <- selected_species_data()

    if (nrow(sp) == 0 || is.na(sp$final_pfaf5_ids[1])) {
      return(hybas5[0, ])
    }

    pfafs <- str_split(sp$final_pfaf5_ids[1], ";")[[1]]
    pfafs <- pfafs[pfafs != "" & !is.na(pfafs)]

    hybas5 %>%
      filter(PFAF_ID %in% pfafs)
  })

  output$species_title <- renderText({
    input$species
  })

  output$species_info <- renderPrint({
    sp <- selected_species_data()

    if (nrow(sp) == 0) {
      cat("No information available.")
    } else {
      cat("Migration class:", sp$Migration_class[1], "\n")
      cat("Threat category:", sp$Threat_Category[1], "\n")
      cat("Population trend:", sp$Population_Trend[1], "\n")
      cat("Migration length:", sp$Length_km[1], "\n")
      cat("Confidence:", sp$Confidence[1], "\n")
      cat("Spatial data sources:", sp$spatial_data_sources[1], "\n")
      cat("Number of level-5 basins:", sp$count_final_pfaf5_ids[1], "\n")
      cat("HydroBASINS regions:", sp$hydrobasin_regions[1], "\n")
      cat("Pfafstetter level-3 regions:", sp$pfaf3_regions[1], "\n")
    }
  })

  output$species_map <- renderLeaflet({
    species_polygons <- selected_species_polygons()

    if (nrow(species_polygons) == 0) {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0, lat = 20, zoom = 2)
    } else {
      leaflet(species_polygons) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          popup = ~paste0(
            "PFAF5: ", PFAF_ID,
            "<br>HYBAS5: ", HYBAS_ID,
            "<br>Region: ", continent_name
          ),
          weight = 1,
          fillOpacity = 0.5
        )
    }
  })

  output$table <- renderDT({

    display_db <- filtered_db() %>%
      select(
        Species,
        Migration_class,
        Length_km,
        Threat_Category,
        Population_Trend,
        Confidence,
        hydrobasin_regions,
        pfaf3_regions,
        count_final_pfaf5_ids,
        spatial_data_sources
      )

    datatable(
      display_db,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("MigraFISH_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_db(), file)
    }
  )

  output$download_full <- downloadHandler(
    filename = function() {
      paste0("MigraFISH_full_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(db, file)
    }
  )

  output$submit_email_button <- renderUI({

    uploaded_file_text <- if (!is.null(input$submit_file)) {
      paste("Supporting file selected in app:", input$submit_file$name)
    } else {
      "Supporting file selected in app: none"
    }

    subject <- URLencode(
      paste("MigraFISH database update:", input$submit_species),
      reserved = TRUE
    )

    body <- URLencode(
      paste(
        "MigraFISH database update",
        "",
        paste("Name:", input$submit_name),
        paste("Affiliation:", input$submit_affiliation),
        paste("Email:", input$submit_email),
        paste("Species:", input$submit_species),
        paste("Update type:", input$submit_type),
        "",
        "Description of update:",
        input$submit_comments,
        "",
        uploaded_file_text,
        "",
        "Please attach any supporting files manually before sending this email.",
        sep = "\n"
      ),
      reserved = TRUE
    )

    tags$a(
      href = paste0(
        "mailto:twan.stoffers@wur.nl",
        "?subject=", subject,
        "&body=", body
      ),
      class = "btn btn-primary",
      "Submit update by email"
    )
  })
}

shinyApp(ui, server)
