#' Handoff signing secret
#'
#' The HMAC secret used to sign and verify league-handoff tokens. Read from the
#' `NBA_HANDOFF_SECRET` environment variable. The entry point and every league
#' container must share the same value.
#'
#' @return A string.
#' @export
handoff_secret <- function() {
  Sys.getenv("NBA_HANDOFF_SECRET")
}

handoff_payload <- function(customer_id, platform, league_id, competitor_id, exp) {
  paste(customer_id, platform, league_id, competitor_id, exp, sep = "|")
}

#' Sign a league-handoff token
#'
#' @param customer_id,platform,league_id,competitor_id Values bound into the token.
#' @param exp Expiry, as seconds since the epoch.
#' @param secret HMAC secret. Defaults to [handoff_secret()].
#'
#' @return Hex HMAC-SHA256 signature.
#' @export
sign_handoff_token <- function(
  customer_id,
  platform,
  league_id,
  competitor_id,
  exp,
  secret = handoff_secret()
) {
  if (!nzchar(secret)) {
    stop("NBA_HANDOFF_SECRET is not set", call. = FALSE)
  }

  digest::hmac(
    key = secret,
    object = handoff_payload(customer_id, platform, league_id, competitor_id, exp),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Verify a league-handoff token
#'
#' Recomputes the signature and checks it matches and has not expired.
#'
#' @param sig The signature to verify.
#' @inheritParams sign_handoff_token
#' @param now Current time, for testability.
#'
#' @return `TRUE` if the token is valid and unexpired, otherwise `FALSE`.
#' @export
verify_handoff_token <- function(
  sig,
  customer_id,
  platform,
  league_id,
  competitor_id,
  exp,
  secret = handoff_secret(),
  now = Sys.time()
) {
  if (is.null(sig) || !nzchar(sig) || !nzchar(secret)) {
    return(FALSE)
  }

  exp_num <- suppressWarnings(as.numeric(exp))
  if (is.na(exp_num) || exp_num < as.numeric(now)) {
    return(FALSE)
  }

  expected <- sign_handoff_token(customer_id, platform, league_id, competitor_id, exp, secret)
  identical(tolower(sig), expected)
}

#' Build a signed handoff query string
#'
#' @inheritParams sign_handoff_token
#' @param exp Expiry (seconds since epoch). Computed from `ttl` when `NULL`.
#' @param ttl Token lifetime in seconds when `exp` is not supplied.
#' @param now Current time, for testability.
#'
#' @return A URL query string including `sig`.
#' @export
handoff_query <- function(
  customer_id,
  platform,
  league_id,
  competitor_id,
  exp = NULL,
  ttl = 300,
  secret = handoff_secret(),
  now = Sys.time()
) {
  if (is.null(exp)) {
    exp <- as.integer(as.numeric(now) + ttl)
  }

  sig <- sign_handoff_token(customer_id, platform, league_id, competitor_id, exp, secret)

  values <- c(
    platform = platform,
    league_id = league_id,
    competitor_id = competitor_id,
    customer_id = customer_id,
    exp = exp,
    sig = sig
  )

  enc <- vapply(values, \(x) utils::URLencode(as.character(x), reserved = TRUE), character(1))
  paste(names(enc), enc, sep = "=", collapse = "&")
}
