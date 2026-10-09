#' Entry point UI
#'
#' @noRd
app_ui <- function(request) {
  www <- system.file("app/www", package = "nba.shiny.entry")
  if (nzchar(www)) {
    shiny::addResourcePath("www", www)
  }

  bslib::page_fluid(
    theme = nba_theme(),
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "www/favicon.ico"),
      tags$style(HTML(
        "html, body { height: 100%; }
         .container-fluid { height: 100%; padding: 0 !important; }
         #app { height: 100%; }

         /* Full-bleed frame that owns the whole viewport. */
         .app-shell { height: 100%; min-height: 0; }
         .app-shell-frame { height: 100%; min-height: 0; position: relative; }
         .app-shell-frame iframe {
           position: absolute; inset: 0; width: 100%; height: 100%;
           border: 0; display: block;
         }
         .app-frame-empty {
           height: 100%; display: flex; align-items: center;
           justify-content: center; color: var(--bs-secondary-color, #6c757d);
         }

         .selectize-dropdown-content { min-width: 100%; box-sizing: border-box; }"
      )),
      tags$script(HTML(
        "window.addEventListener('message', function (e) {
           var d = e.data || {};
           if (d.type !== 'nba:choose') return;
           var allowed = window.NBA_ALLOWED_ORIGINS || [];
           if (allowed.indexOf(e.origin) === -1) return;
           if (window.Shiny) Shiny.setInputValue('nba_choose', Date.now(), {priority: 'event'});
         });
         document.addEventListener('keydown', function (e) {
           if (e.key === 'Enter' && e.target && e.target.id === 'password') {
             var b = document.getElementById('login');
             if (b && !b.disabled) b.click();
           }
         });"
      ))
    ),
    uiOutput("app")
  )
}

#' Login modal, styled to match the league dashboard's switch modal
#'
#' @noRd
login_modal <- function() {
  modalDialog(
    title = "NBA Shiny",
    shiny::textInput("email", "Email", placeholder = "you@example.com"),
    shiny::passwordInput("password", "Password"),
    span(textOutput("login_message"), style = "color:red"),
    footer = tagList(
      actionButton(
        "login",
        "Sign in",
        style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
      )
    ),
    size = "m",
    easyClose = FALSE
  )
}

#' League chooser modal, matching the league dashboard's switch modal
#'
#' @noRd
league_chooser_modal <- function(leagues) {
  choices <- stats::setNames(
    paste(leagues$platform, leagues$league_id, sep = ":"),
    leagues$league_name
  )

  modalDialog(
    selectizeInput(
      "league_choice",
      label = NULL,
      choices = choices,
      options = list(
        placeholder = "Select Fantasy League",
        onInitialize = I("function(){this.setValue('');}")
      ),
      width = "100%"
    ),
    span(textOutput("league_choice_message"), style = "color:red"),
    footer = tagList(
      actionButton(
        "league_choose_confirm",
        "Kobeee!",
        style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
      )
    ),
    size = "m",
    easyClose = TRUE
  )
}

#' @noRd
shell_ui <- function(leagues) {
  origins <- entry_allowed_origins(leagues)

  tagList(
    tags$script(HTML(sprintf(
      "window.NBA_ALLOWED_ORIGINS = %s;",
      jsonlite::toJSON(origins, auto_unbox = FALSE)
    ))),
    tags$div(
      class = "app-shell",
      tags$div(
        class = "app-shell-frame",
        uiOutput("iframe")
      )
    )
  )
}
