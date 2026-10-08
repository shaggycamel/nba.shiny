#' Entry point UI
#'
#' @noRd
app_ui <- function(request) {
  bslib::page_fluid(
    theme = nba_theme(),
    tags$head(tags$style(HTML("html, body { height: 100%; }"))),
    uiOutput("app")
  )
}

#' @noRd
login_ui <- function() {
  bslib::card(
    bslib::card_body(
      tags$h4("NBA Shiny"),
      shiny::textInput("email", "Email"),
      shiny::passwordInput("password", "Password"),
      shiny::actionButton("login", "Sign in"),
      tags$div(style = "color: red;", textOutput("login_message"))
    ),
    style = "max-width: 380px; margin: 10vh auto;"
  )
}

#' @noRd
shell_ui <- function(name, leagues) {
  choices <- stats::setNames(
    paste(leagues$platform, leagues$league_id, sep = ":"),
    leagues$league_name
  )

  tagList(
    tags$div(
      class = "d-flex justify-content-between align-items-center px-3 py-2 border-bottom",
      tags$strong(name),
      shiny::actionButton("signout", "Sign out", class = "btn-sm")
    ),
    tags$div(
      class = "d-flex gap-3 align-items-end px-3 py-2",
      shiny::selectInput("league", "League", choices = choices, width = "320px"),
      uiOutput("competitor_ui")
    ),
    uiOutput("iframe")
  )
}
