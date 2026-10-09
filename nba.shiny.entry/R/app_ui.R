#' Entry point UI
#'
#' @noRd
app_ui <- function(request) {
  bslib::page_fluid(
    theme = nba_theme(),
    tags$head(
      tags$style(HTML(
        "html, body { height: 100%; }
         .container-fluid { height: 100%; padding: 0 !important; }
         #app { height: 100%; }

         /* Full-bleed shell: a header/toolbar that keeps its height and a
            frame that owns the rest of the viewport. */
         .app-shell { display: flex; flex-direction: column; height: 100%; min-height: 0; }
         .app-shell-toolbar { flex: 0 0 auto; }
         .app-shell-frame { flex: 1 1 auto; min-height: 0; position: relative; }
         .app-shell-frame iframe {
           position: absolute; inset: 0; width: 100%; height: 100%;
           border: 0; display: block;
         }

         /* Shown until the embedded dashboard fires its load event. */
         .app-frame-loading {
           position: absolute; inset: 0; z-index: 3;
           display: flex; flex-direction: column; align-items: center;
           justify-content: center; gap: .25rem;
           background: var(--bs-body-bg, #fff);
           color: var(--bs-secondary-color, #6c757d);
         }
         .app-frame-empty {
           position: absolute; inset: 0;
           display: flex; align-items: center; justify-content: center;
           color: var(--bs-secondary-color, #6c757d);
         }

         .app-login {
           height: 100%; min-height: 100%;
           display: flex; align-items: center; justify-content: center;
           padding: 1rem;
         }"
      )),
      tags$script(HTML(
        "window.addEventListener('load', function () {
           if (!window.Shiny) return;
           Shiny.addCustomMessageHandler('login-busy', function (busy) {
             var b = document.getElementById('login');
             if (!b) return;
             if (busy) {
               if (!b.dataset.idleLabel) b.dataset.idleLabel = b.innerHTML;
               b.disabled = true;
               b.innerHTML = '<span class=\"spinner-border spinner-border-sm me-2\" role=\"status\" aria-hidden=\"true\"></span>Signing in...';
             } else {
               b.disabled = false;
               if (b.dataset.idleLabel) b.innerHTML = b.dataset.idleLabel;
             }
           });
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

#' @noRd
login_ui <- function() {
  tags$div(
    class = "app-login",
    bslib::card(
      class = "shadow-sm",
      style = "width: 100%; max-width: 380px;",
      bslib::card_body(
        tags$form(
          onsubmit = "return false;",
          tags$h3(class = "mb-1", "NBA Shiny"),
          tags$p(class = "text-muted", "Sign in to your fantasy dashboard"),
          shiny::textInput("email", "Email", placeholder = "you@example.com"),
          shiny::passwordInput("password", "Password"),
          shiny::actionButton("login", "Sign in", class = "btn-primary w-100"),
          tags$div(class = "text-danger small mt-2", textOutput("login_message"))
        )
      )
    )
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
      class = "app-shell",
      tags$div(
        class = "app-shell-toolbar d-flex flex-wrap align-items-center gap-3 px-3 py-2 border-bottom",
        tags$strong(class = "me-auto", name),
        tags$div(
          class = "d-flex align-items-center gap-2",
          tags$label(`for` = "league", class = "visually-hidden", "League"),
          shiny::selectInput("league", label = NULL, choices = choices, width = "240px")
        ),
        uiOutput("competitor_ui"),
        shiny::actionButton("signout", "Sign out", class = "btn-sm btn-outline-secondary")
      ),
      tags$div(
        class = "app-shell-frame",
        uiOutput("iframe")
      )
    )
  )
}
