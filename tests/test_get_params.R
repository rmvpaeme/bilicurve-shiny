# Functional tests for the bilicurve Shiny app, driven by the GET-parameter
# scenarios documented in README.md.
#
# Run from the project root:
#   Rscript tests/test_get_params.R
#
# The GET-request handler in app.R simply copies query parameters onto the
# matching inputs (advanced, prematuur, geboorte_GET, afname_GET, bili_GET, ...)
# via updateTextInput. These tests reproduce that post-GET state by setting the
# same inputs directly with shiny::testServer, then assert that the newData()
# reactive produces the expected data and that the plot renders without error.

library(shiny)
library(testthat)

# Load ui + server (app.R ends in shinyApp(); sourcing does not launch it).
source("app.R", local = TRUE)

# Inputs needed so vals$initial_date is populated (guards the nchar() check that
# runs for every branch in newData()); the actual birth time for the advanced
# term curve comes from geboorte_GET, not from these.
base_inputs <- list(
  selected_language = "nl",
  geboortedag = "2023-11-22",
  # timeInput delivers a POSIXct; mirror that here (the app reads it via hm()).
  geboorteuur = as.POSIXct("1970-01-01 10:00", tz = "UTC"),
  afnameuur1 = as.POSIXct("1970-01-01 10:00", tz = "UTC"),
  afnameuur2 = as.POSIXct("1970-01-01 10:00", tz = "UTC"),
  afnameuur3 = as.POSIXct("1970-01-01 10:00", tz = "UTC")
)

set_case <- function(session, ...) {
  do.call(session$setInputs, c(base_inputs, list(...)))
}

# --- README scenario 1: term curve (advanced) --------------------------------
test_that("term GET request produces correct sample data", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "nee",
      naam = "testbaby",
      geboorte_GET = "2023-11-22 10:00:00",
      afname_GET = "2023-11-23 10:00:00,2023-11-24 10:00:00",
      bili_GET = "10,9",
      PML_geboorte_GET = "37.28",
      PT_start_GET = NA, PT_stop_GET = NA, PT_aantalLampen_GET = NA
    )
    df <- newData()$df
    expect_equal(nrow(df), 2)
    expect_equal(df$biliwaarde, c(10, 9))
    expect_equal(df$`tijd in uren`, c(24, 48))
    expect_equal(unique(df$`PML bij geboorte`), 37.28)
    expect_true(all(df$annotation == "staal"))
  })
})

# --- README scenario 2: preterm curve (advanced) -----------------------------
test_that("preterm GET request produces correct sample data", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "ja",
      naam = "testbaby",
      PML_GET = "23+1/7,24+1/7",
      bili_GET = "10,9",
      PT_start_GET = NA, PT_stop_GET = NA, PT_aantalLampen_GET = NA
    )
    df <- newData()$df
    expect_equal(nrow(df), 2)
    expect_equal(df$biliwaarde, c(10, 9))
    expect_equal(round(df$`postmenstruele leeftijd`, 5),
                 round(c(23 + 1 / 7, 24 + 1 / 7), 5))
  })
})

# --- README scenario 3: term curve with phototherapy -------------------------
test_that("term GET request with phototherapy fills df_PT", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "nee",
      geboorte_GET = "2023-11-22 10:00:00",
      afname_GET = "2023-11-23 10:00:00,2023-11-24 10:00:00",
      bili_GET = "10,9",
      PML_geboorte_GET = "37.28",
      PT_start_GET = "2023-11-23 11:00:00,2023-11-24 12:00:00",
      PT_stop_GET = "2023-11-23 15:00:00,2023-11-24 14:00:00",
      PT_aantalLampen_GET = "1,2"
    )
    df_PT <- newData()$df_PT
    expect_true(all(df_PT$specified))
    expect_equal(nrow(df_PT), 2)
    expect_equal(as.character(df_PT$PT_aantalLampen), c("1", "2"))
  })
})

# --- README scenario 4: preterm curve with phototherapy ----------------------
test_that("preterm GET request with phototherapy fills df_PT", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "ja",
      PML_GET = "23+1/7,24+1/7",
      bili_GET = "10,9",
      PT_start_GET = "23.14,24.14",
      PT_stop_GET = "23.90,24.90",
      PT_aantalLampen_GET = "1,2"
    )
    df_PT <- newData()$df_PT
    expect_true(all(df_PT$specified))
    expect_equal(nrow(df_PT), 2)
    expect_equal(as.character(df_PT$PT_aantalLampen), c("1", "2"))
    expect_equal(as.numeric(df_PT$PT_start), c(23.14, 24.14))
  })
})

# --- Smoke tests: the plot renders without error -----------------------------
# These exercise the renderPlot path, including the filter()s on the renamed
# annotation identifiers ("staal", week labels, TcB threshold). Reads ./data/*.
test_that("term curve renders without error (no risk factors)", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "nee",
      bili_risk = "nee",
      geboorte_GET = "2023-11-22 10:00:00",
      afname_GET = "2023-11-23 10:00:00,2023-11-24 10:00:00",
      bili_GET = "10,9",
      PML_geboorte_GET = "37.28",
      PT_start_GET = NA, PT_stop_GET = NA, PT_aantalLampen_GET = NA
    )
    expect_no_error(output$bilicurve)
  })
})

test_that("term curve renders without error (with risk factors)", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "nee",
      bili_risk = "ja",
      geboorte_GET = "2023-11-22 10:00:00",
      afname_GET = "2023-11-23 10:00:00,2023-11-24 10:00:00",
      bili_GET = "10,9",
      PML_geboorte_GET = "37.28",
      PT_start_GET = NA, PT_stop_GET = NA, PT_aantalLampen_GET = NA
    )
    expect_no_error(output$bilicurve)
  })
})

test_that("preterm curve renders without error", {
  testServer(server, {
    set_case(
      session,
      advanced = "ja",
      prematuur = "ja",
      bili_risk = "nee",
      PML_GET = "23+1/7,24+1/7",
      bili_GET = "10,9",
      PT_start_GET = NA, PT_stop_GET = NA, PT_aantalLampen_GET = NA
    )
    expect_no_error(output$bilicurve)
  })
})
