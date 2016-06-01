
confirm <- function(message, when = NULL, default = TRUE, ...) {
  c(
    list(
      message = message,
      when = when,
      default = default,
      name = NA_character_,
      type = "confirm"
    ),
    check_named(list(...))
  )
}

input <- function(message, when = NULL, default = "", filter = NULL,
                  nextline = TRUE, wrap = TRUE, validate = NULL, ...) {
  c(
    list(
      message = message,
      when = when,
      default = default,
      filter = filter,
      nextline = nextline,
      wrap = wrap,
      validate = validate,
      type = "input"
    ),
    check_named(list(...))
  )
}

choose <- function(message, when = NULL, choices, default = NA, ...) {

  c(
    list(
      message = message,
      when = when,
      choices = choices,
      default = default,
      type = "choose"
    ),
    check_named(list(...))
  )
}

checkbox <- function(message, when = NULL, choices, default = numeric(),
                     ...) {
  c(
    list(
      message = message,
      when = when,
      choices = choices,
      default = default,
      type = "checkbox"
    ),
    check_named(list(...))
  )
}

constant <- function(message = "", when = NULL, value, ...) {
  c(
    list(
      message = message,
      when = when,
      value = value,
      type = "constant"
    ),
    check_named(list(...))
  )
}

banner <- function(message = "", when = NULL, ...) {
  c(
    list(
      message = message,
      when = when,
      type = "banner"
    ),
    check_named(list(...))
  )
}
