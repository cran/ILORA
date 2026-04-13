#' ILORA: A package for retrieving species data from ILORA database
#'
#' Provides tools to access, query, and visualize species occurrence data from the Indian Alien Flora Information (ILORA) database.
#' The package enables users to retrieve species names, explore available variables, and extract species-level data based on user-defined criteria.
#' It also supports exploratory data analysis and visualization to facilitate ecological and biogeographical research.
#' Further details about the database are available at <https://ilora2020.wixsite.com/ilora2020>.
#'
#' @name ILORA
#' @import DBI
#' @import RPostgres
#' @import sf
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import dplyr
#' @import tidyr
#' @import RColorBrewer
#' @import glue
NULL

# Import necessary libraries
#library(DBI)
#library(RPostgres)
#library(ggplot2)
#library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(dplyr)
#library(tidyr)
#library(plotly)
#library(RColorBrewer)




connect_ilora_db <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host = "ilora-db19-ilora-2019.e.aivencloud.com",
    port = 10594,
    dbname = "defaultdb",
    user = "read_user",
    password = "Iloradata2019@",
    sslmode = "require"
  )
}

can_connect_db <- function() {
  tryCatch({
    con <- connect_ilora_db()
    DBI::dbDisconnect(con)
    TRUE
  }, error = function(e) FALSE)
}


#' Retrieve Species Names from ILORA Database
#'
#' This function connects to the ILORA database and retrieves all species names
#' available in the `species_name` table. It returns them as a character vector
#' in alphabetical order.
#'
#' @return A character vector containing all species names in the ILORA database.
#'         If no species are found, returns an empty character vector.
#'
#' @examples
#' \donttest{
#' species_names <- get_species_names()
#' head(species_names)
#' }
#'
#' @export
get_species_names <- function() {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "
    SELECT acc_species_name
    FROM species_name
    ORDER BY acc_species_name;
  "

  result <- tryCatch(
    DBI::dbGetQuery(con, query),
    error = function(e) {
      stop("Failed to retrieve species names: ", e$message)
    }
  )

  if (nrow(result) == 0) {
    message("No species found in the database.")
    return(character(0))
  }

  return(result$acc_species_name)
}


#' Retrieve Column Names by Table from ILORA Database
#'
#' This function connects to the ILORA database and retrieves all column names
#' for tables in the `public` schema. Columns are grouped by table name.
#'
#' @param include_id Logical; if FALSE (default), identifier columns such as
#'   `acc_species_id` are excluded. If TRUE, all columns are included.
#'
#' @return A named list. Each element is a character vector of column names for a table.
#'   If no tables/columns are found, returns an empty list.
#'
#' @examples
#' \donttest{
#' # Retrieve variables excluding ID columns
#' get_variable_names()
#'
#' # Retrieve variables including ID columns
#' get_variable_names(include_id = TRUE)
#' }
#'
#' @export
get_variable_names <- function(include_id = FALSE) {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "
    SELECT table_name, column_name
    FROM information_schema.columns
    WHERE table_schema = 'public'
    ORDER BY table_name, ordinal_position;
  "

  df <- tryCatch(
    DBI::dbGetQuery(con, query),
    error = function(e) {
      stop("Failed to retrieve variable names: ", e$message)
    }
  )

  if (nrow(df) == 0) {
    message("No tables/columns found in the database.")
    return(list())
  }

  if (!include_id) {
    df <- df[!grepl("acc_species_id", df$column_name, ignore.case = TRUE), ]
  }

  df <- df[!grepl("^pg_|^sql_", df$table_name), ]

  result <- split(df$column_name, df$table_name)

  return(result)
}


#' Retrieve Table Names from ILORA Database
#'
#' This function connects to the ILORA database and returns all table names
#' in the `public` schema, excluding system tables.
#'
#' @return A character vector of table names. Returns an empty vector if no tables are found.
#'
#' @examples
#' \donttest{
#' # Retrieve all user tables
#' get_table_names()
#' }
#'
#' @export
get_table_names <- function() {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "
    SELECT table_name
    FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_type = 'BASE TABLE'
      AND table_name NOT LIKE 'pg_%'
      AND table_name NOT LIKE 'sql_%'
    ORDER BY table_name;
  "

  result <- DBI::dbGetQuery(con, query)

  return(result$table_name)
}


#' Open ILORA Variable Description PDF
#'
#' Opens the ILORA variable description PDF file (`ILORA_HowToRead.pdf`)
#' in the system's default PDF viewer. This guide helps users understand
#' the variables and structure of the ILORA dataset.
#'
#' @details
#' This function uses \code{utils::browseURL()} to open the PDF file.
#' Depending on the user's system configuration, the file may open in
#' a web browser, external PDF viewer, or within RStudio's viewer pane.
#'
#' @return
#' No return value. This function is called for its side effect of
#' opening the PDF file.
#'
#' @examples
#' \donttest{
#' ilora_variable_guide()
#' }
#'
#' @export
ilora_variable_guide <- function() {

  pdf_path <- system.file(
    "extdata",
    "ILORA_HowToRead.pdf",
    package = "ILORA"
  )

  if (pdf_path == "") {
    stop("Could not find the file in the package. Please reinstall the package.")
  }

  utils::browseURL(pdf_path)
}


#' Retrieve Data for One or More Species from ILORA Database
#'
#' Fetches either specific variables across tables or an entire table
#' for one or more species from the ILORA database.
#'
#' @param species_name Character vector of species names to fetch data for.
#'                     Must match `acc_species_name` in the database.
#' @param variables Character vector, optional. Specific variables to retrieve.
#'                  Should match variable names returned by `get_variable_names()`.
#' @param table_name Character, optional. Name of the table to retrieve data from.
#'                   Either `variables` or `table_name` must be provided, not both.
#'
#' @return A data frame containing the requested species data.
#'         Returns an error if species or variables/table are not found.
#'
#' @examples
#' \donttest{
#' # Retrieve specific variables
#' get_data("Quercus robur L.", variables = c("Longitude", "Latitude"))
#'
#' # Retrieve all columns from a specific table
#' get_data("Quercus robur L.", table_name = "habitat")
#' }
#'
#' @export

get_data <- function(species_name, variables = NULL, table_name = NULL) {
  if (is.null(species_name) || length(species_name) == 0) {
    stop("Species vector cannot be empty.")
  }
  if (!is.null(variables) && !is.null(table_name)) {
    stop("Specify either variables or table_name, not both.")
  }

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  species_lower <- tolower(species_name)
  id_query <- sprintf(
    "SELECT acc_species_id, acc_species_name
     FROM species_name
     WHERE LOWER(acc_species_name) IN (%s)",
    paste(DBI::dbQuoteString(con, species_lower), collapse = ", ")
  )

  species_df <- DBI::dbGetQuery(con, id_query)
  if (nrow(species_df) == 0) stop("Species not found in database.")
  ids <- species_df$acc_species_id
  ids_string <- paste(ids, collapse = ", ")

  # =========================================================
  # OPTION 1: TABLE
  # =========================================================
  if (!is.null(table_name)) {
    valid_tables <- get_table_names()
    if (!table_name %in% valid_tables) stop("Invalid table name.")

    query <- paste0(
      "SELECT * FROM ", table_name,
      " WHERE acc_species_id IN (", ids_string, ")"
    )

    return(DBI::dbGetQuery(con, query))
  }

  # =========================================================
  # OPTION 2: VARIABLES
  # =========================================================
  if (!is.null(variables)) {

    var_map <- get_variable_names(include_id = TRUE)
    mapping <- utils::stack(var_map)

    variables_lower <- tolower(variables)
    matched <- mapping[tolower(mapping$values) %in% variables_lower, ]
    if (nrow(matched) == 0) stop("None of the variables found in database.")

    tables_needed <- unique(as.character(matched$ind))

    select_parts <- c("s.acc_species_name")
    join_clauses <- character()
    alias_counter <- 1

    for (tbl in tables_needed) {
      alias <- paste0("t", alias_counter)

      join_clauses <- c(
        join_clauses,
        paste0(" LEFT JOIN ", tbl, " ", alias,
               " ON s.acc_species_id = ", alias, ".acc_species_id ")
      )

      cols <- matched$values[tolower(matched$ind) == tolower(tbl)]

      cols_quoted <- paste0(alias, '.\"', cols, '\"')

      select_parts <- c(select_parts, cols_quoted)
      alias_counter <- alias_counter + 1
    }

    select_clause <- paste(select_parts, collapse = ", ")
    join_clause <- paste(join_clauses, collapse = " ")

    query <- paste0(
      "SELECT DISTINCT ", select_clause,
      " FROM species_name s ",
      join_clause,
      " WHERE s.acc_species_id IN (", ids_string, ")"
    )

    return(DBI::dbGetQuery(con, query))
  }

  stop("Please provide either variables or table_name.")
}


#' Generate a Color Palette
#'
#' Returns a vector of colors based on a specified palette name. Supports
#' built-in R palettes, `RColorBrewer` palettes, `viridis` palettes, and
#' custom diverging palettes.
#'
#' @param n Integer. Number of colors to generate.
#' @param palette Character. Name of the palette. Supported values:
#'   `"rainbow"`, `"heat"`, `"terrain"`, `"topo"`, `"cm"`,
#'   `"viridis"`, `"plasma"`, `"inferno"`, `"magma"`, `"cividis"`,
#'   `"green"`, `"red"`, `"purple"`, `"orange"`, `"yellow"`, `"brown"`,
#'   `"diverging1"`, `"diverging2"`, `"diverging3"`, `"diverging4"`.
#'
#' @return Character vector of color hex codes.
#'
#' @importFrom grDevices rainbow heat.colors terrain.colors topo.colors cm.colors colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis plasma inferno magma cividis
#'
#' @export
#'
#' @examples
#' # Generate 5 colors from rainbow palette
#' generate_palette(5, "rainbow")
#'
#' # Generate 10 colors from viridis palette
#' generate_palette(10, "viridis")

generate_palette <- function(n, palette) {
  switch(palette,
         "rainbow" = rainbow(n),
         "heat" = heat.colors(n),
         "terrain" = terrain.colors(n),
         "topo" = topo.colors(n),
         "cm" = cm.colors(n),
         "viridis" = viridis::viridis(n),
         "plasma" = viridis::plasma(n),
         "inferno" = viridis::inferno(n),
         "magma" = viridis::magma(n),
         "cividis" = viridis::cividis(n),
         "green" = brewer.pal(n, "Greens"),
         "red" = brewer.pal(n, "Reds"),
         "purple" = brewer.pal(n, "Purples"),
         "orange" = brewer.pal(n, "Oranges"),
         "yellow" = brewer.pal(n, "Yellows"),
         "brown" = brewer.pal(n, "Browns"),
         "diverging1" = colorRampPalette(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))(n),
         "diverging2" = colorRampPalette(c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"))(n),
         "diverging3" = colorRampPalette(c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"))(n),
         "diverging4" = colorRampPalette(c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"))(n),
         rainbow(n)
  )
}


#' Plot Latitude and Longitude of Species on a Map of India
#'
#' Visualizes occurrences of one or more species on a map of India using their
#' latitude and longitude data from the ILORA database.
#'
#' @param species_names Character vector. Names of species to plot. Must match
#'   `acc_species_name` in the database.
#' @param custom_colors Optional. Named vector of colors for each species. If
#'   provided, length must match `species_names`.
#' @param palette Optional. Character string specifying a palette for colors.
#'   Options include `"rainbow"`, `"heat"`, `"terrain"`, `"topo"`, `"cm"`,
#'   `"viridis"`, `"plasma"`, `"inferno"`, `"magma"`, `"cividis"`, `"green"`,
#'   `"red"`, `"purple"`, `"orange"`, `"yellow"`, `"brown"`, `"grey"`,
#'   `"diverging1"`, `"diverging2"`, `"diverging3"`, `"diverging4"`.
#' @param opacity Numeric. Opacity of the plotted points (0 to 1). Default 0.7.
#' @param highlight Logical. If TRUE, highlights regions containing species occurrences. Default FALSE.
#'
#' @return A `ggplot` object displaying the species occurrences on a map of India.
#'
#' @details
#' Plots multiple species occurrences with either a specified palette or
#' custom colors. Ensures spatial coordinates align with India's shapefile CRS.
#'
#' @examples
#' \donttest{
#' # Plot multiple species with custom colors
#' plot_species_on_map(
#'   c("Cyanus segetum Hill", "Cuphea llavea Lex."),
#'   custom_colors = c("orange", "blue")
#' )
#'
#' # Plot a single species using the heat palette and reduced opacity
#' plot_species_on_map("Quercus robur L.", palette = "heat", opacity = 0.6)
#'
#' # Plot a species using custom colors and viridis palette
#' plot_species_on_map("Avena sativa L.", custom_colors = c("orange"), palette = "viridis")
#' }
#'
#' @export
plot_species_on_map <- function(species_names,
                                custom_colors = NULL,
                                palette = "viridis",
                                opacity = 0.7,
                                highlight = FALSE) {

  # Fetch species data
  species_data <- get_data(
    species_names,
    variables = c("Longitude", "Latitude")
  )

  # Check if data is returned
  if (is.null(species_data) || nrow(species_data) == 0) {
    warning("No data found for the given species. Returning empty plot.")
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  colnames(species_data) <- tolower(colnames(species_data))
  species_data <- tidyr::drop_na(species_data, longitude, latitude)

  if (nrow(species_data) == 0) {
    warning("No valid latitude/longitude data available. Returning empty plot.")
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  # Load India shapefile
  shp_path <- system.file("extdata", "india.shp", package = "ILORA")
  if (shp_path == "") {
    warning("Shapefile not found in package extdata folder. Returning empty plot.")
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  india <- sf::st_read(shp_path, quiet = TRUE)
  species_data_sf <- sf::st_as_sf(species_data, coords = c("longitude", "latitude"), crs = 4326)
  species_data_sf <- sf::st_transform(species_data_sf, sf::st_crs(india))

  unique_species <- unique(species_data$acc_species_name)

  # Assign colors
  if (!is.null(custom_colors)) {
    if (length(custom_colors) != length(unique_species)) {
      warning("Length of custom_colors does not match number of species. Using default palette.")
      species_colors <- generate_palette(length(unique_species), palette)
    } else {
      species_colors <- custom_colors
    }
  } else {
    species_colors <- generate_palette(length(unique_species), palette)
  }
  names(species_colors) <- unique_species

  # Highlight occurrence regions
  if (highlight) {
    intersects <- sf::st_intersects(india, species_data_sf, sparse = FALSE)
    india$occurrence <- rowSums(intersects) > 0
  }

  # Build the plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = india, fill = "grey90", color = "white") +
    ggplot2::geom_sf(data = species_data_sf, ggplot2::aes(color = acc_species_name),
                     size = 2, alpha = opacity) +
    ggplot2::scale_color_manual(values = species_colors) +
    ggplot2::labs(
      title = "Occurrences of Species in India",
      x = "Longitude",
      y = "Latitude",
      color = "Species"
    ) +
    ggplot2::theme_minimal()

  if (highlight) {
    plot <- plot +
      ggplot2::geom_sf(data = india, ggplot2::aes(fill = occurrence),
                       color = NA, alpha = 0.3) +
      ggplot2::scale_fill_manual(values = c("FALSE" = NA, "TRUE" = "#808080"))
  }

  message("Plot created with ", nrow(species_data_sf), " occurrence points.")
  return(plot)
}



#' Plot the Number of Species by Invasion Status
#'
#' Retrieves species data from the `sp_categorization` table in the ILORA database
#' and creates a bar plot showing the number of species in each invasion status category.
#' Categories include Invasive (In), Naturalized (Nt), Casual Aliens (CA),
#' Cryptogenic (CG), and Native (N).
#'
#' @return A `ggplot` object representing the number of species in each invasion
#'   status category.
#'
#' @examples
#' \donttest{
#' # Generate a bar plot of species counts for each invasion status
#' bar_plot()
#' }
#'
#' @export
bar_plot <- function() {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "
    SELECT \"Invasion_Status\" AS invasion_status,
           COUNT(DISTINCT acc_species_id) AS count
    FROM sp_categorization
    WHERE \"Invasion_Status\" IS NOT NULL
    GROUP BY \"Invasion_Status\"
  "

  data <- DBI::dbGetQuery(con, query)

  if (nrow(data) == 0) stop("No data found.")

  data$count <- as.numeric(data$count)

  status_labels <- c(
    "In" = "Invasive",
    "Nt" = "Naturalized",
    "CA" = "Casual aliens",
    "CG" = "Cryptogenic",
    "N" = "Native"
  )

  data <- data[data$invasion_status %in% names(status_labels), ]

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = factor(invasion_status, levels = names(status_labels)),
      y = count,
      fill = invasion_status
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::scale_x_discrete(labels = status_labels) +
    ggplot2::labs(
      title = "Number of Species for Each Invasion Status Category",
      x = "Invasion Status Category",
      y = "Number of Species"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  return(p)
}


#' Species Count Plot by Growth Habit and Duration
#'
#' Retrieves species data from the `general_information` table in the ILORA database,
#' calculates the number of distinct species for each growth habit and duration category,
#' and generates bar plots to visualize these counts. Summary tables are also included.
#'
#' @return A list containing:
#' \describe{
#'   \item{plot_growthhabit}{A `ggplot` object showing species counts by growth habit.}
#'   \item{plot_duration}{A `ggplot` object showing species counts by duration.}
#'   \item{growthhabit_summary}{A data frame summarizing species counts by growth habit.}
#'   \item{duration_summary}{A data frame summarizing species counts by duration.}
#' }
#'
#' @examples
#' \donttest{
#' # Generate and display plots for growth habit and duration
#' plots <- species_count_plot()
#' print(plots$plot_growthhabit)
#' print(plots$plot_duration)
#' }
#'
#' @export
species_count_plot <- function() {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query_growth <- "
    SELECT COALESCE(\"Growth.Habit\", 'Unknown') AS growthhabit,
           COUNT(DISTINCT acc_species_id) AS count
    FROM general_information
    GROUP BY COALESCE(\"Growth.Habit\", 'Unknown')
    ORDER BY count DESC
  "
  growthhabit_count <- DBI::dbGetQuery(con, query_growth)

  if (nrow(growthhabit_count) == 0) stop("No growth habit data found.")

  query_duration <- "
    SELECT COALESCE(\"Duration\", 'Unknown') AS duration,
           COUNT(DISTINCT acc_species_id) AS count
    FROM general_information
    GROUP BY COALESCE(\"Duration\", 'Unknown')
    ORDER BY count DESC
  "
  duration_count <- DBI::dbGetQuery(con, query_duration)
  if (nrow(duration_count) == 0) stop("No duration data found.")

  growthhabit_count$count <- as.numeric(growthhabit_count$count)
  duration_count$count <- as.numeric(duration_count$count)

  growthhabit_count$growthhabit <- factor(
    growthhabit_count$growthhabit,
    levels = growthhabit_count$growthhabit[order(growthhabit_count$count, decreasing = TRUE)]
  )

  duration_count$duration <- factor(
    duration_count$duration,
    levels = duration_count$duration[order(duration_count$count, decreasing = TRUE)]
  )

  plot_growthhabit <- ggplot2::ggplot(
    growthhabit_count,
    ggplot2::aes(x = growthhabit, y = count)
  ) +
    ggplot2::geom_col(fill = "#1f78b4") +
    ggplot2::labs(
      title = "Number of Species by Growth Habit",
      x = "Growth Habit",
      y = "Number of Species"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  plot_duration <- ggplot2::ggplot(
    duration_count,
    ggplot2::aes(x = duration, y = count)
  ) +
    ggplot2::geom_col(fill = "#33a02c") +
    ggplot2::labs(
      title = "Number of Species by Duration",
      x = "Duration",
      y = "Number of Species"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(list(
    plot_growthhabit = plot_growthhabit,
    plot_duration = plot_duration,
    growthhabit_summary = growthhabit_count,
    duration_summary = duration_count
  ))
}



#' Species Count Plot by Introduction Pathways
#'
#' Retrieves species data from the `introduction` table in the ILORA database,
#' counts distinct species for each reported introduction pathway, and generates a 3D pie chart
#' using Plotly. The summary table of counts is also printed.
#'
#' @return A `plotly` object representing a 3D pie chart of species counts by introduction pathway.
#'         The summary table of top pathways is printed as a message.
#'
#' @examples
#' \donttest{
#' # Generate pie chart for species introduction pathways
#' plot <- introduction_pathways_plot()
#' plot  # Displays the Plotly pie chart
#' }
#'
#' @export
introduction_pathways_plot <- function() {
  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "
    SELECT
      COALESCE(NULLIF(TRIM(\"Introduction_pathways_reported\"), ''), 'Unknown') AS pathway,
      COUNT(DISTINCT acc_species_id) AS count
    FROM introduction
    GROUP BY pathway
    ORDER BY count DESC
  "

  pathway_counts <- DBI::dbGetQuery(con, query)

  if (nrow(pathway_counts) == 0) {
    stop("No data found.")
  }

  # Keep top 6 pathways + "Others"
  top_pathways <- pathway_counts[order(-pathway_counts$count), ][1:min(6, nrow(pathway_counts)), ]
  other_count <- sum(pathway_counts$count) - sum(top_pathways$count)
  if (other_count > 0) {
    top_pathways <- rbind(
      top_pathways,
      data.frame(pathway = "Others", count = other_count)
    )
  }

  n <- nrow(top_pathways)
  base_colors <- RColorBrewer::brewer.pal(max(3, min(8, n)), "Set2")
  colors <- colorRampPalette(base_colors)(n)

  plot_pathways <- plotly::plot_ly(
    data = top_pathways,
    labels = ~pathway,
    values = ~count,
    type = "pie",
    marker = list(colors = colors),
    textinfo = "label+percent",
    textposition = "inside",
    insidetextfont = list(color = "#FFFFFF"),
    hoverinfo = "text",
    text = ~paste(pathway, ": ", count)
  )

  plot_pathways <- plotly::layout(
    plot_pathways,
    title = "Species Distribution by Introduction Pathway",
    showlegend = TRUE
  )

  colnames(top_pathways) <- c("Introduction Pathway", "Number of Species")
  print(top_pathways, row.names = FALSE)

  return(plot_pathways)

}



#' Histogram of Species First Record Year
#'
#' Retrieves first record years of species from the `introduction` table
#' in the ILORA database within a specified year interval and plots a histogram using ggplot2.
#'
#' @param start_year Numeric. Start year of the interval.
#' @param end_year Numeric. End year of the interval.
#' @param binwidth Numeric, optional. Width of histogram bins. If `NULL` (default), it
#'   is automatically determined based on the year range.
#'
#' @return A `ggplot` object representing the histogram of species first record years.
#'
#' @examples
#' \donttest{
#' # Plot histogram for species first records between 1990 and 2000
#' plot_first_record_year_histogram(start_year = 1990, end_year = 2000)
#' }
#'
#' @export
plot_first_record_year_histogram <- function(start_year, end_year, binwidth = NULL) {

  if (missing(start_year) || missing(end_year)) {
    stop("Please provide both start_year and end_year.")
  }
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("start_year and end_year must be numeric.")
  }
  if (start_year > end_year) {
    stop("start_year cannot be greater than end_year.")
  }

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- sprintf("
    SELECT \"First_record_date\"::INT AS year
    FROM introduction
    WHERE \"First_record_date\" IS NOT NULL
      AND \"First_record_date\" >= %d
      AND \"First_record_date\" <= %d
  ", start_year, end_year)

  data <- DBI::dbGetQuery(con, query)

  if (nrow(data) == 0) {
    stop("No data found in the given year range.")
  }

  if (is.null(binwidth)) {
    range_years <- end_year - start_year
    binwidth <- ifelse(range_years <= 10, 1,
                       ifelse(range_years <= 30, 2, 5))
  }

  ggplot2::ggplot(data, ggplot2::aes(x = year)) +
    ggplot2::geom_histogram(
      binwidth = binwidth,
      fill = "#69b3a2",
      color = "black",
      alpha = 0.7
    ) +
    ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, by = binwidth)) +
    ggplot2::labs(
      title = sprintf("Histogram of Species First Record Year (%d-%d)", start_year, end_year),
      x = "First Record Year",
      y = "Number of Records"
    ) +
    ggplot2::theme_minimal()
}



#' Calculate Minimum Residence Time (MRT) for Species
#'
#' Computes the Minimum Residence Time (MRT) for species based on their first recorded year
#' in the ILORA database. MRT is defined as the difference between the current year
#' and the species' first record year. Users can calculate MRT for specific species or all species.
#'
#' @param species Character vector or single string specifying species names to calculate MRT for.
#'                Use `"ALL"` to calculate MRT for all species in the database. Defaults to `"ALL"`.
#'
#' @return A data frame with columns:
#'   - `acc_species_name`: The accepted species name.
#'   - `first_year`: The first recorded year of the species.
#'   - `mrt`: Minimum Residence Time (current year minus first recorded year).
#'
#'   If a specified species is not found in the database, a message is printed indicating the missing species.
#'   Returns `NULL` if no matching records are found.
#'
#' @examples
#' \donttest{
#' # Calculate MRT for all species
#' all_species_mrt <- calculate_mrt(species = "ALL")
#'
#' # Calculate MRT for specific species
#' species <- c("Species A", "Species B", "Species C")
#' specific_species_mrt <- calculate_mrt(species)
#' }
#'
#' @export
#'
calculate_mrt <- function(species = "ALL") {
  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  safe_species <- function(x) DBI::dbQuoteString(con, x)

  if (identical(species, "ALL")) {
    query <- sprintf("
      SELECT
        s.acc_species_name,
        MIN(\"First_record_date\"::INT) AS first_year,
        %d - MIN(\"First_record_date\"::INT) AS mrt
      FROM introduction i
      JOIN species_name s ON s.acc_species_id = i.acc_species_id
      WHERE \"First_record_date\" IS NOT NULL
      GROUP BY s.acc_species_name
      ORDER BY mrt DESC
    ", current_year)
  } else {
    if (!is.character(species)) stop("Species must be a character vector.")

    species_list <- paste0("'", tolower(species), "'", collapse = ", ")

    query <- sprintf("
      SELECT
        s.acc_species_name,
        MIN(\"First_record_date\"::INT) AS first_year,
        %d - MIN(\"First_record_date\"::INT) AS mrt
      FROM introduction i
      JOIN species_name s ON s.acc_species_id = i.acc_species_id
      WHERE LOWER(s.acc_species_name) IN (%s)
      GROUP BY s.acc_species_name
      ORDER BY mrt DESC
    ", current_year, species_list)
  }

  result <- DBI::dbGetQuery(con, query)

  if (nrow(result) == 0) {
    message("No records found.")
    return(NULL)
  }

  if (!identical(species, "ALL")) {
    all_species <- DBI::dbGetQuery(con, sprintf("
      SELECT LOWER(acc_species_name) AS acc_species_name
      FROM species_name
      WHERE LOWER(acc_species_name) IN (%s)
    ", species_list))

    missing_species <- setdiff(tolower(species), all_species$acc_species_name)
    if (length(missing_species) > 0) {
      message("Species not found in database: ", paste(missing_species, collapse = ", "))
    }
  }

  return(result)
}




#' Calculate Total Number of Uses for Each Species
#'
#' Computes the total number of uses for species based on the `economic_uses` table
#' in the ILORA database. It sums across all use-related columns for each species.
#'
#' @param species Optional character vector of species names to filter results.
#'   If `NULL` (default), results for all species are returned.
#'
#' @return A data frame with columns:
#'   - `acc_species_name`: The accepted species name.
#'   - `acc_species_id`: Internal species ID.
#'   - `total_uses`: Sum of all recorded uses across relevant economic use columns.
#'
#'   If no matching records are found, a message is printed and `NULL` is returned.
#'
#' @examples
#' \donttest{
#' # Calculate total uses for all species
#' all_species_uses <- calculate_total_uses()
#'
#' # Calculate total uses for specific species
#' species_list <- c("Quercus robur L.", "Azadirachta indica")
#' specific_species_uses <- calculate_total_uses(species_list)
#' }
#'
#' @export
#'
calculate_total_uses <- function(species = NULL) {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!is.null(species) && length(species) == 0) {
    stop("Species vector cannot be empty.")
  }
  if (!is.null(species) && !is.character(species)) {
    stop("Species must be a character vector.")
  }

  cols_query <- "
    SELECT column_name
    FROM information_schema.columns
    WHERE table_name = 'economic_uses'
      AND column_name ~ '^[0-9]{4}_'
  "
  cols_df <- DBI::dbGetQuery(con, cols_query)
  if (nrow(cols_df) == 0) stop("No use columns found in economic_uses table.")

  use_cols <- cols_df$column_name

  sum_expr <- paste(
    sprintf("COALESCE(NULLIF(\"%s\", '')::NUMERIC, 0)", use_cols),
    collapse = " + "
  )

  species_filter <- ""
  if (!is.null(species)) {

    all_species_df <- DBI::dbGetQuery(con, "SELECT acc_species_name FROM species_name")

    species_lower <- tolower(species)
    existing_species <- all_species_df$acc_species_name[tolower(all_species_df$acc_species_name) %in% species_lower]
    missing_species <- setdiff(species, existing_species)
    if (length(missing_species) > 0) {
      message("Species not found in database: ", paste(missing_species, collapse = ", "))
    }

    if (length(existing_species) == 0) {
      message("No matching species found in database.")
      return(NULL)
    }

    species_list <- paste(DBI::dbQuoteString(con, tolower(existing_species)), collapse = ", ")
    species_filter <- sprintf("WHERE LOWER(s.acc_species_name) IN (%s)", species_list)
  }

  query <- sprintf("
    SELECT
      s.acc_species_name,
      eu.acc_species_id,
      (%s) AS total_uses
    FROM economic_uses eu
    JOIN species_name s
      ON eu.acc_species_id = s.acc_species_id
    %s
    ORDER BY total_uses DESC
  ", sum_expr, species_filter)

  result <- DBI::dbGetQuery(con, query)

  if (nrow(result) == 0) {
    message("No data found for the provided species.")
    return(NULL)
  }

  return(result)
}

#' Calculate Market Metrics for Seed and Plant Prices
#'
#' Retrieves market data from the `market_dynamics` table in the ILORA database
#' and computes key metrics for seed and plant prices. Metrics include average prices,
#' and identification of species with the highest and lowest prices for seeds,
#' nursery-live plants (NL), and plant-live plants (PL).
#'
#' @return A named list containing:
#' \describe{
#'   \item{averages}{A data frame with average seed, NL plant, and PL plant prices.}
#'   \item{highest_seed}{Data frame with species having the highest seed price.}
#'   \item{lowest_seed}{Data frame with species having the lowest seed price.}
#'   \item{highest_nl_plant}{Data frame with species having the highest NL plant price.}
#'   \item{lowest_nl_plant}{Data frame with species having the lowest NL plant price.}
#'   \item{highest_pl_plant}{Data frame with species having the highest PL plant price.}
#'   \item{lowest_pl_plant}{Data frame with species having the lowest PL plant price.}
#' }
#'
#' @details
#' NL refers to Nursery-Live plants and PL refers to Plant-Live plants.
#' The function calculates averages across all species recorded in the database and identifies
#' extremes for each price category.
#'
#' @examples
#' \donttest{
#' # Compute market metrics
#' market_metrics <- calculate_market_metrics()
#'
#' # View average prices
#' market_metrics$averages
#'
#' # View species with highest seed price
#' market_metrics$highest_seed
#' }
#'
#' @export
#'
calculate_market_metrics <- function() {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- "
    WITH base AS (
      SELECT
        acc_species_id,
        \"NL.Seed.Price(Rs)\" AS seed_price,
        \"NL.Plant.Price(Rs)\" AS nl_price,
        \"PL.Plant.Price(Rs)\" AS pl_price
      FROM market_dynamics
    ),

    summary AS (
      SELECT
        AVG(seed_price) AS avg_seed_price,
        AVG(nl_price) AS avg_nl_price,
        AVG(pl_price) AS avg_pl_price,
        MAX(seed_price) AS max_seed_price,
        MIN(seed_price) AS min_seed_price,
        MAX(nl_price) AS max_nl_price,
        MIN(nl_price) AS min_nl_price,
        MAX(pl_price) AS max_pl_price,
        MIN(pl_price) AS min_pl_price
      FROM base
    )

    SELECT
      s.acc_species_name,
      b.*,
      summary.*
    FROM base b
    JOIN species_name s ON s.acc_species_id = b.acc_species_id
    CROSS JOIN summary
  "

  data <- DBI::dbGetQuery(con, query)

  if (nrow(data) == 0) {
    message("No data found.")
    return(NULL)
  }

  result <- list(
    averages = data[1, c("avg_seed_price", "avg_nl_price", "avg_pl_price")],

    highest_seed = subset(data, seed_price == max_seed_price,
                          select = c("acc_species_name", "seed_price")),

    lowest_seed = subset(data, seed_price == min_seed_price,
                         select = c("acc_species_name", "seed_price")),

    highest_nl_plant = subset(data, nl_price == max_nl_price,
                              select = c("acc_species_name", "nl_price")),

    lowest_nl_plant = subset(data, nl_price == min_nl_price,
                             select = c("acc_species_name", "nl_price")),

    highest_pl_plant = subset(data, pl_price == max_pl_price,
                              select = c("acc_species_name", "pl_price")),

    lowest_pl_plant = subset(data, pl_price == min_pl_price,
                             select = c("acc_species_name", "pl_price"))
  )

  return(result)
}



#' Visualize Native Range of Species on a Global Map
#'
#' This function retrieves the native range of one or more species from the ILORA database
#' and plots their distribution on a global map using TDWG Level 2 regions.
#'
#' @param species_name Character vector of species names to plot. Use `"ALL"` to plot all species in the database.
#'
#' @return A ggplot2 object showing the native range of the specified species.
#'
#' @details
#' The function joins the native range data from the ILORA database with a TDWG Level 2 shapefile
#' to display species’ native regions. Each species is color-coded, and overlapping ranges are handled
#' to avoid duplicate polygons.
#'
#' @examples
#' \donttest{
#' # Visualize native range of specific species
#' visualize_native_range(c("Cyanus segetum Hill", "Avena sativa L."))
#' }
#'
#' @export
#'
visualize_native_range <- function(species_name) {

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (length(species_name) == 1 && toupper(species_name) == "ALL") {

    query <- "
      SELECT DISTINCT
        \"Acc_Species_Name\" AS species_name,
        \"TDWG_Level2\" AS tdwg_level2
      FROM native_range
      WHERE \"TDWG_Level2\" IS NOT NULL
    "

  } else {

    if (!is.character(species_name)) stop("species_name must be a character vector")

    species_lower <- tolower(species_name)
    species_quoted <- paste(DBI::dbQuoteString(con, species_lower), collapse = ", ")

    query <- sprintf("
      SELECT DISTINCT
        \"Acc_Species_Name\" AS species_name,
        \"TDWG_Level2\" AS tdwg_level2
      FROM native_range
      WHERE LOWER(\"Acc_Species_Name\") IN (%s)
        AND \"TDWG_Level2\" IS NOT NULL
    ", species_quoted)
  }

  native_range_data <- DBI::dbGetQuery(con, query)
  if (nrow(native_range_data) == 0) stop("No data found for provided species.")

  shapefile_path <- system.file("extdata", "level2.shp", package = "ILORA")
  if (shapefile_path == "") stop("Shapefile 'level2.shp' not found in extdata.")

  world_map <- sf::st_read(shapefile_path, quiet = TRUE)

  if (is.na(sf::st_crs(world_map))) {
    stop("Shapefile has no CRS defined. Please fix the source data.")
  }

  if (sf::st_crs(world_map)$epsg != 4326) {
    world_map <- sf::st_transform(world_map, 4326)
  }
  if (!"LEVEL2_COD" %in% colnames(world_map)) stop("Expected column 'LEVEL2_COD' not found")
  world_map <- dplyr::rename(world_map, tdwg_level2 = LEVEL2_COD)

  world_map <- suppressWarnings(sf::st_make_valid(world_map))
  areas <- sf::st_area(world_map)
  world_map <- world_map[areas > units::set_units(0, "m^2"), ]
  world_map <- sf::st_simplify(world_map, dTolerance = 0.001)

  native_range_data$tdwg_level2 <- as.character(native_range_data$tdwg_level2)
  world_map$tdwg_level2 <- as.character(world_map$tdwg_level2)

  native_range_map <- dplyr::inner_join(world_map, native_range_data, by = "tdwg_level2")
  native_range_map <- native_range_map %>%
    dplyr::distinct(tdwg_level2, species_name, .keep_all = TRUE)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_map, fill = "grey90", color = "white") +
    ggplot2::geom_sf(data = native_range_map, ggplot2::aes(fill = species_name), color = "white") +
    ggplot2::labs(
      title = if (length(species_name) > 1) "Native Range of Selected Species"
      else paste("Native Range of", species_name),
      fill = "Species"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    )

  return(plot)
}


#' Visualize Naturalized Range of Species on a Global Map
#'
#' This function retrieves the naturalized range of one or more species from the ILORA database
#' and plots their distribution on a global map using TDWG Level 2 regions.
#'
#' @param species_name Character vector of species names to plot. Use `"ALL"` to plot all species in the database.
#'
#' @return A ggplot2 object showing the naturalized range of the specified species.
#'
#' @details
#' The function joins the naturalized range data from the ILORA database with a TDWG Level 2 shapefile
#' to display species’ naturalized regions. Each species is color-coded.
#'
#' @examples
#' \donttest{
#' # Visualize naturalized range of specific species
#' visualize_naturalized_range(c("Cyanus segetum Hill", "Avena sativa L."))
#' }
#'
#' @export
#'
visualize_naturalized_range <- function(species_name) {
  # Connect to database
  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Build query based on species input
  if (length(species_name) == 1 && toupper(species_name) == "ALL") {
    query <- "
      SELECT DISTINCT
        \"Acc_Species_Name\" AS species_name,
        \"TDWG_Level2_Names\" AS tdwg_level2
      FROM naturalized_range
      WHERE \"TDWG_Level2_Names\" IS NOT NULL
    "
  } else {
    if (!is.character(species_name)) stop("species_name must be a character vector")
    species_lower <- tolower(species_name)
    species_quoted <- paste(DBI::dbQuoteString(con, species_lower), collapse = ", ")

    query <- sprintf("
      SELECT DISTINCT
        \"Acc_Species_Name\" AS species_name,
        \"TDWG_Level2_Names\" AS tdwg_level2
      FROM naturalized_range
      WHERE LOWER(\"Acc_Species_Name\") IN (%s)
        AND \"TDWG_Level2_Names\" IS NOT NULL
    ", species_quoted)
  }

  # Execute query safely
  naturalized_range_data <- tryCatch(
    DBI::dbGetQuery(con, query),
    error = function(e) {
      stop("Database query failed: ", e$message)
    }
  )

  if (nrow(naturalized_range_data) == 0) {
    warning("No data found for the provided species name(s). Returning world map only.")
  } else {
    message(nrow(naturalized_range_data), " rows retrieved from database.")
  }

  # Load shapefile
  shapefile_path <- system.file("extdata", "level2.shp", package = "ILORA")
  if (shapefile_path == "") stop("Shapefile 'level2.shp' not found in extdata.")

  world_map <- sf::st_read(shapefile_path, quiet = TRUE)

  # Validate CRS
  if (is.na(sf::st_crs(world_map))) stop("Shapefile has no CRS defined. Please fix source data.")
  if (sf::st_crs(world_map)$epsg != 4326) world_map <- sf::st_transform(world_map, 4326)

  # Check required columns
  if (!all(c("LEVEL2_COD", "LEVEL2_NAM") %in% colnames(world_map))) {
    stop("Shapefile must contain 'LEVEL2_COD' and 'LEVEL2_NAM' columns.")
  }

  # Clean and simplify geometries
  world_map <- suppressWarnings(sf::st_make_valid(world_map))
  world_map <- sf::st_simplify(world_map, dTolerance = 0.001)

  if (nrow(naturalized_range_data) == 0) {
    # Return only world map if no data
    return(
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_map, fill = "grey90", color = "white") +
        ggplot2::labs(title = "No species data matched") +
        ggplot2::theme_minimal()
    )
  }

  # Merge species data with shapefile
  naturalized_range_data$tdwg_level2 <- as.character(naturalized_range_data$tdwg_level2)
  world_map$LEVEL2_NAM <- as.character(world_map$LEVEL2_NAM)

  naturalized_range_map <- dplyr::inner_join(
    world_map,
    naturalized_range_data,
    by = c("LEVEL2_NAM" = "tdwg_level2")
  )

  naturalized_range_map <- dplyr::distinct(
    naturalized_range_map, LEVEL2_NAM, species_name, .keep_all = TRUE
  )

  # Warn if no matches after join
  if (nrow(naturalized_range_map) == 0) {
    warning("No matches found between shapefile and species data. Returning world map only.")
    return(
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_map, fill = "grey90", color = "white") +
        ggplot2::labs(title = "No species data matched") +
        ggplot2::theme_minimal()
    )
  }

  # Build plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_map, fill = "grey90", color = "white") +
    ggplot2::geom_sf(
      data = naturalized_range_map,
      ggplot2::aes(fill = species_name),
      color = "white"
    ) +
    ggplot2::labs(
      title = ifelse(length(species_name) > 1,
                     "Naturalized Range of Selected Species",
                     paste("Naturalized Range of", species_name)),
      fill = "Species"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right"
    )

  return(plot)
}


#' Exploratory Data Analysis (EDA)
#'
#' This function acts as a dynamic interface to call any available EDA function
#' from the ILORA package. Users specify which function to execute and pass
#' any required arguments to that function.
#'
#' @param func Character string specifying the name of the function to call.
#' @param ... Additional arguments to pass to the specified function.
#'
#' @return The output of the specified EDA function. This could be a ggplot object,
#' a Plotly object, a list of plots and summaries, or a data frame, depending on the function.
#'
#' @details
#' Available functions that can be called through `EDA()` include:
#' \describe{
#'   \item{plot_species_on_map}{Plots specified species on a map of India.}
#'   \item{bar_plot}{Bar plot showing counts of species across 5 invasion status categories.}
#'   \item{species_count_plot}{Returns a list of bar plots for growth habit and duration, plus summary tables.}
#'   \item{introduction_pathways_plot}{Pie chart showing species counts by introduction pathways.}
#'   \item{plot_first_record_year_histogram}{Histogram of species first record years within a given interval.}
#'   \item{calculate_mrt}{Calculates Minimum Residence Time (MRT) for species as current year minus first record year.}
#'   \item{calculate_total_uses}{Calculates total number of uses for each species.}
#'   \item{calculate_market_metrics}{Returns metrics for average seed/plant prices and species with highest/lowest prices.}
#'   \item{visualize_native_range}{Visualizes the native range of species on a global map.}
#'   \item{visualize_naturalized_range}{Visualizes the naturalized range of species on a global map.}
#' }
#'
#' Use `?function_name` for detailed help on each individual function.
#'
#' @examples
#' \donttest{
#' # Call the bar_plot function
#' EDA("bar_plot")
#'
#' # Call plot_species_on_map with additional arguments
#' EDA("plot_species_on_map",
#'     species_names = c("Quercus robur L."),
#'     palette = "inferno",
#'     opacity = 0.9,
#'     highlight = FALSE)
#' }
#'
#' @export

EDA <- function(func, ...) {
  available_functions <- list(
    plot_species_on_map = plot_species_on_map,
    bar_plot = bar_plot,
    species_count_plot = species_count_plot,
    introduction_pathways_plot = introduction_pathways_plot,
    plot_first_record_year_histogram = plot_first_record_year_histogram,
    calculate_mrt = calculate_mrt,
    calculate_total_uses = calculate_total_uses,
    calculate_market_metrics = calculate_market_metrics,
    visualize_native_range = visualize_native_range,
    visualize_naturalized_range = visualize_naturalized_range
  )

  if (!is.null(available_functions[[func]])) {
    result <- do.call(available_functions[[func]], list(...))
    return(result)
  } else {
    stop("The function name provided does not exist in the available functions.")
  }
}



#' Retrieve Comprehensive Species Information from ILORA Database
#'
#' This function fetches detailed data for one or more species from multiple ILORA database tables.
#' It retrieves taxonomy, invasion status, general information, native range, introduction pathways,
#' uses, market data, habitat, naturalized range, occurrence and distribution, geography, and climate.
#'
#' @param species_name Character vector of species names to query. Must be non-empty.
#' @return A structured list containing data frames for each category:
#' \describe{
#'   \item{taxonomy}{Taxonomic classification.}
#'   \item{invasion_status}{Invasion status of the species.}
#'   \item{general_info}{General species information.}
#'   \item{native_range}{Native range at TDWG level 2.}
#'   \item{introduction}{Introduction pathway and first record date in India.}
#'   \item{uses}{Economic uses across different sectors.}
#'   \item{market_info}{Market metrics such as plant prices.}
#'   \item{habitat}{Habitat information.}
#'   \item{naturalized_range}{Naturalized range at TDWG level 2.}
#'   \item{occurrence_distribution}{List containing occurrence and distribution data frames.}
#'   \item{geography}{List containing LULC, anthrome, and ecoregion data frames.}
#'   \item{climate}{Climate-related data.}
#' }
#'
#' @examples
#' \donttest{
#' species_details <- get_species_details(c("Azadirachta indica", "Quercus robur L."))
#' species_details$taxonomy
#' species_details$market_info
#' }
#' @export
get_species_details <- function(species_name) {

  if (!is.character(species_name) || length(species_name) == 0) {
    stop("species_name must be a non-empty character vector")
  }

  con <- connect_ilora_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # =========================================================
  #  STEP 1: Get species IDs safely
  # =========================================================
  species_df <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT acc_species_id, acc_species_name
    FROM species_name
    WHERE acc_species_name IN ({species_name*})
  ", .con = con))

  if (nrow(species_df) == 0) {
    stop("Species not found in database.")
  }

  ids <- species_df$acc_species_id

  # =========================================================
  #  Helper function for fetching tables
  # =========================================================
  fetch_table <- function(table_name) {

    query <- glue::glue_sql("
      SELECT s.acc_species_name,
             t.*
      FROM species_name s
      LEFT JOIN {`table_name`} t
      ON s.acc_species_id = t.acc_species_id
      WHERE s.acc_species_id IN ({ids*})
    ", .con = con)

    result <- tryCatch(
      DBI::dbGetQuery(con, query),
      error = function(e) {
        stop(
          paste0("Failed to fetch data from table '", table_name, "': ", e$message),
          call. = FALSE
        )
      }
    )

    names(result) <- tolower(names(result))

    return(result)
  }

  # =========================================================
  #  TAXONOMY
  # =========================================================
  ids_string <- paste(ids, collapse = ", ")

  taxonomy_query <- paste0(
    "SELECT s.acc_species_name,
            sc.\"Class\", sc.\"Order_taxonomic\", sc.\"Family\",
            sc.\"Genus\", sc.\"Species\", sc.\"Invasion_Status\"
     FROM species_name s
     LEFT JOIN sp_categorization sc
     ON s.acc_species_id = sc.acc_species_id
     WHERE s.acc_species_id IN (", ids_string, ")"
  )

  taxonomy <- DBI::dbGetQuery(con, taxonomy_query)

  #  Normalize column names
  names(taxonomy) <- tolower(names(taxonomy))

  #  Extract invasion status
  invasion_status <- taxonomy[, c("acc_species_name", "invasion_status"), drop = FALSE]

  # =========================================================
  #  BUILD RESULT STRUCTURE
  # =========================================================
  result <- list(
    taxonomy = taxonomy,
    invasion_status = invasion_status,

    general_info = fetch_table("general_information"),
    native_range = fetch_table("native_range"),
    introduction = fetch_table("introduction"),
    uses = fetch_table("economic_uses"),
    market_info = fetch_table("market_dynamics"),
    habitat = fetch_table("habitat"),
    naturalized_range = fetch_table("naturalized_range"),

    #  REQUIRED nested structure
    occurrence_distribution = list(
      occurrence = fetch_table("occurrence"),
      distribution = fetch_table("distribution")
    ),

    geography = list(
      lulc = fetch_table("lulc"),
      anthrome = fetch_table("anthrome"),
      ecoregions = fetch_table("ecoregions")
    ),

    climate = fetch_table("climate")
  )

  # =========================================================
  #  FINAL FIX: Match expected column name in tests
  # =========================================================
  fix_names <- function(df) {
    if ("acc_species_name" %in% names(df)) {
      names(df)[names(df) == "acc_species_name"] <- "Acc_Species_Name"
    }
    return(df)
  }

  # Apply to all outputs
  result$taxonomy <- fix_names(result$taxonomy)
  result$invasion_status <- fix_names(result$invasion_status)

  result$general_info <- fix_names(result$general_info)
  result$native_range <- fix_names(result$native_range)
  result$introduction <- fix_names(result$introduction)
  result$uses <- fix_names(result$uses)
  result$market_info <- fix_names(result$market_info)
  result$habitat <- fix_names(result$habitat)
  result$naturalized_range <- fix_names(result$naturalized_range)

  result$occurrence_distribution$occurrence <- fix_names(result$occurrence_distribution$occurrence)
  result$occurrence_distribution$distribution <- fix_names(result$occurrence_distribution$distribution)

  result$geography$lulc <- fix_names(result$geography$lulc)
  result$geography$anthrome <- fix_names(result$geography$anthrome)
  result$geography$ecoregions <- fix_names(result$geography$ecoregions)

  result$climate <- fix_names(result$climate)

  return(result)
}
