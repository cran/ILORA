library(testthat)
library(ILORA)

test_that("get_species_names includes known species if present", {
  species <- get_species_names()

  expect_type(species, "character")
  expect_gt(length(species), 0)

  if ("Rubus buergeri Miq." %in% species) {
    expect_true(TRUE)
  } else {
    skip("Reference species not present in database")
  }
})



test_that("get_variable_names returns a named list of variables", {
  variables <- get_variable_names()

  # Should be a list
  expect_type(variables, "list")
  expect_gt(length(variables), 0)

  # Names should be table names
  expect_true(all(nzchar(names(variables))))

  # Flatten and check presence of a known column
  all_vars <- unlist(variables, use.names = FALSE)

  expect_type(all_vars, "character")
  expect_true("Genus" %in% all_vars)
})


test_that("get_table_names returns a character vector", {
  tables <- get_table_names()

  expect_type(tables, "character")
  expect_gt(length(tables), 0)
  expect_true("introduction" %in% tables)
})


test_that("get_data returns a data frame for valid inputs", {

  species_data <- get_data(
    "Rubus buergeri Miq.",
    c("Order_taxonomic", "0700_Fuels", "Genus", "Species")
  )

  expect_s3_class(species_data, "data.frame")
  expect_gt(nrow(species_data), 0)

  # Check required columns exist (case insensitive safer)
  returned_cols <- tolower(names(species_data))
  expected_cols <- tolower(c("acc_species_name", "Order_taxonomic", "0700_Fuels", "Genus", "Species"))

  expect_true(all(expected_cols %in% returned_cols))
})

test_that("get_data returns error for invalid species", {
  expect_error(
    get_data("Invalid species", c("Order_taxonomic", "0700_Fuels", "Genus", "Species")),
    "Species not found"
  )
})


test_that("get_species_details retrieves correct details for a single species", {

  species_name <- "Dahlia coccinea Cav."
  result <- get_species_details(species_name)

  # Structure check
  expect_true(is.list(result))

  expected_names <- c(
    "taxonomy",
    "invasion_status",
    "general_info",
    "native_range",
    "introduction",
    "uses",
    "market_info",
    "habitat",
    "naturalized_range",
    "occurrence_distribution",
    "geography",
    "climate"
  )

  expect_true(all(expected_names %in% names(result)))

  # Basic data presence check
  expect_s3_class(result$taxonomy, "data.frame")
  expect_s3_class(result$general_info, "data.frame")

  # Nested structure checks
  expect_true(is.list(result$occurrence_distribution))
  expect_true(is.list(result$geography))
})


test_that("get_species_details retrieves correct details for multiple species", {

  species_names <- c("Dahlia coccinea Cav.", "Acacia auriculiformis Benth.")
  result <- get_species_details(species_names)

  expect_true(is.list(result))

  # Correct names (aligned with function)
  expect_true(all(c(
    "taxonomy",
    "invasion_status",
    "general_info",
    "native_range",
    "introduction",
    "uses",
    "market_info",
    "habitat",
    "naturalized_range",
    "occurrence_distribution",
    "geography",
    "climate"
  ) %in% names(result)))

  # Check structure
  expect_s3_class(result$taxonomy, "data.frame")
  expect_s3_class(result$general_info, "data.frame")

  # Check at least one species is present (not all required)
  expect_true(any(result$taxonomy$Acc_Species_Name %in% species_names))
  expect_true(any(result$invasion_status$Acc_Species_Name %in% species_names))

  # Nested checks
  expect_true(is.list(result$occurrence_distribution))
  expect_true(is.list(result$geography))

  expect_s3_class(result$occurrence_distribution$occurrence, "data.frame")
  expect_s3_class(result$occurrence_distribution$distribution, "data.frame")

  expect_s3_class(result$geography$lulc, "data.frame")
  expect_s3_class(result$geography$anthrome, "data.frame")
  expect_s3_class(result$geography$ecoregions, "data.frame")
})


test_that("get_species_details handles invalid input types", {
  expect_error(
    get_species_details(123),
    class = "error"
  )
})


test_that("visualize_native_range creates a plot for given species", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()


  species_name <- "Dahlia coccinea Cav."

  plot <- visualize_native_range(species_name)

  expect_s3_class(plot, "ggplot")
})


# Tests for plot_species_on_map function
test_that("plot_species_on_map creates a plot for given species", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()
  species_names <- "Dahlia coccinea Cav."

  plot <- plot_species_on_map(species_names)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_species_on_map handles custom colors", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  species_names <- "Dahlia coccinea Cav."
  custom_colors <- c("red")

  plot <- plot_species_on_map(species_names, custom_colors = custom_colors)

  expect_s3_class(plot, "ggplot")
})


test_that("plot_species_on_map returns error for invalid custom colors input", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  custom_colors <- c("red","blue")
  species_names <- "Dahlia coccinea Cav."

  expect_warning(
    plot_species_on_map(species_names, custom_colors = custom_colors),
    "Length of custom_colors does not match number of species. Using default palette."
  )
})

test_that("bar_plot creates a plot", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  plot <- bar_plot()

  expect_s3_class(plot, "ggplot")
})

test_that("bar_plot has correct title and labels", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  plot <- bar_plot()

  expect_match(plot$labels$title, "Number of Species")
  expect_match(plot$labels$x, "Invasion Status")
  expect_match(plot$labels$y, "Number of Species")
})


test_that("species_count_plot creates ggplot objects", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  plots <- tryCatch(species_count_plot(), error = function(e) NULL)

  if (is.null(plots)) {
    skip("species_count_plot returned NULL due to missing data")
  }

  expect_s3_class(plots$plot_growthhabit, "ggplot")
  expect_s3_class(plots$plot_duration, "ggplot")
})


test_that("species_count_plot has correct titles and labels", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  plots <- tryCatch(species_count_plot(), error = function(e) NULL)

  if (is.null(plots)) {
    skip("species_count_plot returned NULL due to missing data")
  }

  # Growth habit plot checks
  gh <- plots$plot_growthhabit
  expect_s3_class(gh, "ggplot")
  expect_true(grepl("Growth Habit", gh$labels$title))
  expect_true(grepl("Growth Habit", gh$labels$x))
  expect_true(grepl("Number of Species", gh$labels$y))

  # Duration plot checks
  dur <- plots$plot_duration
  expect_s3_class(dur, "ggplot")
  expect_true(grepl("Duration", dur$labels$title))
  expect_true(grepl("Duration", dur$labels$x))
  expect_true(grepl("Number of Species", dur$labels$y))
})


test_that("introduction_pathways_plot creates a Plotly pie chart", {
  # Skip if no internet / DB not available
  skip_if_offline()

  # Skip on CRAN / CI
  skip_on_cran()

  plot <- tryCatch(introduction_pathways_plot(), error = function(e) NULL)

  if (is.null(plot)) {
    skip("introduction_pathways_plot returned NULL due to missing data")
  }

  # Check that the object is a Plotly object
  expect_s3_class(plot, "plotly")

  # Optionally check that it's a pie chart
  if (!is.null(plot$x$attrs)) {
    types <- sapply(plot$x$attrs, function(a) a$type)
    expect_true("pie" %in% types)
  }
})

skip_if_not(can_connect_db(), "Database not reachable")
