#' The entry point server-side
#'
#' @param input,output,session Internal {shiny} parameters.
#' @noRd
app_server <- function(input, output, session) {
  con <- entry_db_connect()
  session$onSessionEnded(function() {
    try(DBI::dbDisconnect(con), silent = TRUE)
  })

  season <- entry_season()
  rv <- reactiveValues(customer_id = NULL, name = NULL)

  observeEvent(input$login, {
    customer_id <- check_credentials(con, input$email, input$password)
    if (is.na(customer_id)) {
      customer_id <- dev_login(input$email, input$password, con)
    }

    if (is.na(customer_id)) {
      output$login_message <- renderText("Invalid email or password")
      return()
    }

    rv$customer_id <- customer_id
    rv$name <- get_customer_name(con, customer_id)
    output$login_message <- NULL
  })

  observeEvent(input$signout, {
    rv$customer_id <- NULL
    rv$name <- NULL
    output$login_message <- NULL
  })

  leagues <- reactive({
    req(rv$customer_id)
    get_customer_leagues(con, rv$customer_id, season)
  })

  selected_league_row <- reactive({
    req(input$league)
    sel <- parse_league_value(input$league)
    df <- leagues()
    df[df$platform == sel$platform & as.integer(df$league_id) == sel$league_id, , drop = FALSE]
  })

  # The manager bound to the selected league for this customer, when the mapping
  # provides one.
  mapped_competitor <- reactive({
    row <- selected_league_row()
    if (nrow(row) == 1L && !is.na(row$competitor_id[[1]]) && nzchar(row$competitor_id[[1]])) {
      row$competitor_id[[1]]
    } else {
      NA_character_
    }
  })

  # Prefer the registry container_url; fall back to the deploy naming convention.
  league_container_url <- reactive({
    row <- selected_league_row()
    if (nrow(row) == 1L && !is.null(row$container_url) &&
          !is.na(row$container_url[[1]]) && nzchar(row$container_url[[1]])) {
      row$container_url[[1]]
    } else {
      sel <- parse_league_value(input$league)
      league_space_url(sel$platform, sel$league_id)
    }
  })

  output$app <- renderUI({
    if (is.null(rv$customer_id)) {
      login_ui()
    } else {
      shell_ui(rv$name, leagues())
    }
  })

  output$competitor_ui <- renderUI({
    if (!is.na(mapped_competitor())) {
      return(NULL)
    }

    sel <- parse_league_value(input$league)
    comps <- get_league_competitors(con, sel$platform, sel$league_id, season)
    shiny::selectInput(
      "competitor",
      "Manager",
      choices = stats::setNames(comps$competitor_id, comps$competitor_name),
      width = "240px"
    )
  })

  output$iframe <- renderUI({
    sel <- parse_league_value(req(input$league))
    competitor_id <- if (!is.na(mapped_competitor())) mapped_competitor() else input$competitor
    req(competitor_id)

    url <- league_iframe_url(
      league_container_url(),
      customer_id = rv$customer_id,
      platform = sel$platform,
      league_id = sel$league_id,
      competitor_id = competitor_id
    )

    tags$iframe(
      src = url,
      style = "border: 0; width: 100%; height: calc(100vh - 150px);"
    )
  })
}
