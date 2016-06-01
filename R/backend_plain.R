
#' @export

ask_backend_plain <- R6::R6Class(
  "ask_backend_plain",
  inherit = ask::ask_backend,
  public = list(
    initialize = function() {
      ask_backend_plain_init(self, private, super)
    },
    ask = function(question) {
      ask_backend_plain_ask(self, private, question)
    }
  ),
  private = list(
    emph = NULL
  )
)

#' @importFrom crayon combine_styles green bold

ask_backend_plain_init <- function(self, private, super) {
  super$initialize()
  private$emph <- combine_styles(green, bold)
  self
}

ask_backend_plain_ask <- function(self, private, question) {
  switch(
    question$type,
    "confirm"  = ask_backend_plain_confirm (self, private, question),
    "input"    = ask_backend_plain_input   (self, private, question),
    "choose"   = ask_backend_plain_choose  (self, private, question),
    "checkbox" = ask_backend_plain_checkbox(self, private, question),
    "constant" = ask_backend_plain_constant(self, private, question),
    "banner"   = ask_backend_plain_banner  (self, private, question)
  )
}

#' @importFrom clisymbols symbol
#' @importFrom crayon finish
#' @importFrom stats start

ask_backend_plain_confirm <- function(self, private, question) {
  prompt <- c(" (y/N) ", " (Y/n) ")[question$default + 1]
  repeat {
    ans <- readline(
      prompt = bold(question$message) %+% prompt %+% start(private$emph)
    )
    res <- NA
    if (ans == "") res <- question$default
    if (tolower(ans) == "y" || tolower(ans) == "yes") res <- TRUE
    if (tolower(ans) == "n" || tolower(ans) == "no" ) res <- FALSE
    if (!is.na(res)) break
    msg(finish(private$emph) %+% "Sorry, did not get it.", appendLF = TRUE)
  }
  msg(finish(private$emph))
  res
}

ask_backend_plain_input <- function(self, private, question) {

  message <- question$message
  default <- question$default

  if (default != "") message <- message %+% " (" %+% default %+% ")"

  repeat {
    result <- readline(bold(message) %+% " " %+% start(private$emph))
    if (is.null(question$validate)) break
    valres <- question$validate(result)
    if (identical(valres, TRUE)) break
    error_msg(finish(private$emph), valres)
  }
  if (result == "") result <- default
  msg(finish(private$emph))

  if (!is.null(question$filter)) result <- question$filter(result)
  result
}

ask_backend_plain_choose <- function(self, private, question) {

  message <- question$message
  default <- question$default
  choices <- question$choices

  if (is.character(default)) default <- pmatch(default, choices)
  default <- as.numeric(default)
  stopifnot(is.na(default) || is_index(choices, default))

  msg(
    bold(message),
    "\n",
    paste0(" ", seq_along(choices), ". ", choices, collapse = "\n"),
    "\n"
  )

  repeat {
    prompt <- paste0(
      green(symbol$fancy_question_mark), " ",
      if (! is.na(default)) " (" %+% choices[default] %+% ") " else "",
      start(private$emph)
    )
    res <- readline(prompt = prompt)
    msg(finish(private$emph))
    if (res == "" && !is.na(default)) {
      res <- choices[default]
      break
    }
    suppressWarnings(res <- as.numeric(res))
    if (is.na(res) || res < 1 || res > length(choices) || ! is_integerish(res)) {
      msg("Sorry, I did not get that.", appendLF = TRUE)
    } else {
      res <- choices[res]
      break
    }
  }
  res
}

ask_backend_plain_checkbox <- function(self, private, question) {

  message <- question$message
  choices <- question$choices
  default <- question$default

  choices <- as.character(choices)
  if (is.character(default)) default <- pmatch(default, choices)
  default <- as.numeric(default)

  msg(
    bold(message),
    "\n",
    paste0(
      " ", seq_along(choices), ". ", choices,
      ifelse(seq_along(choices) %in% default, " (x)", ""),
      collapse = "\n"),
    "\n"
  )

  repeat {
    prompt <- paste0(
      green(symbol$fancy_question_mark),
      " (Comma separated numbers, dash for nothing",
      if (length(default)) ", ENTER for (x) defaults",
      ") ",
      start(private$emph)
    )

    res <- strtrim(strsplit(strtrim(readline(prompt = prompt)), ",")[[1]])
    msg(finish(private$emph))

    if (length(res) == 1 && res == "-") {
      res <- numeric()
    } else if (length(res) == 0) {
      res <- default
    }

    res <- suppressWarnings(res <- as.numeric(res))
    if (any(is.na(res)) || any(!is_integerish(res)) ||
        any(res < 1) || any(res > length(choices))) {
      msg("Sorry, I did not get that.", appendLF = TRUE)
    } else {
      res <- choices[sort(unique(res))]
      break
    }
  }
  res
}

ask_backend_plain_constant <- function(self, private, question) {
  question$value
}

ask_backend_plain_banner <- function(self, private, question) {
  msg(question$message, appendLF = TRUE)
  NULL
}
