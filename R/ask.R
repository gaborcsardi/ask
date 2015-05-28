
questions <- list()

questions$confirm <- function(name, message, default = TRUE) {
  prompt <- c(" (y/N) ", " (Y/n) ")[default + 1]
  repeat {
    msg(message %+% prompt)
    ans <- readline()
    res <- NA
    if (ans == "") res <- default
    if (tolower(ans) == "y" || tolower(ans) == "yes") res <- TRUE
    if (tolower(ans) == "n" || tolower(ans) == "no" ) res <- FALSE
    if (!is.na(res)) break
    msg("Sorry, did not get it.", appendLF = TRUE);
  }
  res
}

questions$input <- function(name, message, default = "") {
  if (default != "") message <- message %+% " (" %+% default %+% ")"
  msg(message %+% " ")
  readline()
}

#' @importFrom utils menu

questions$rawlist <- function(name, message, choices, default = choices[1]) {
  stopifnot(default %in% choices)
  repeat {
    msg(message, appendLF = TRUE)
    msg(paste0(" ", seq_along(choices), ". ", choices, "\n"))
    msg("[?] (" %+% default %+% ") ")
    res <- readline()
    if (res == "") {
      res <- default;
      break;
    }
    suppressWarnings(res <- as.numeric(res))
    if (is.na(res) || res < 1 || res > length(choices) || ! is_integerish(res)) {
      msg("Sorry, I did not get that.", appendLF = TRUE)
    } else {
      res <- choices[res]
      break;
    }
  }
  res
}

#' Ask a question to the user, through the command line
#'
#' @param ... Questions to ask, see details below.
#' @return A named list with the answers.
#'
#' @export
#' @importFrom lazyeval lazy_dots lazy_eval
#' @examples
#' \dontrun{
#' ask(
#'   q("input", "name", "What is your name?"),
#'   q("confirm", "cool", "Are you cool?"),
#'   q("rawlist", "booze", "Select your poison!", c("Beer", "Wine"))
#' )
#' }

ask <- function(...) {

  check_tty()

  qs <- lazy_dots(...)

  answers <- list()

  question <- function(type, name, message, ..., validate = NULL,
                       filter = NULL, when = NULL) {
    if (! type %in% names(questions)) stop("Unknown question type");

    if (!is.null(when) && ! when(answers)) return(NULL)

    repeat {
      result <- questions[[type]](name, message, ...)
      if (!is.null(filter)) result <- filter(result)
      if (is.null(validate)) break
      valres <- validate(result)
      if (identical(valres, TRUE)) break
      error_msg(valres)
    }

    answers[[name]] <- result
    answers
  }

  for (q in qs) {
    answers <- lazy_eval(q, data = list(q = question))
  }
  answers
}


msg <- function(..., appendLF = FALSE) {
  message(..., appendLF = appendLF)
}

error_msg <- function(..., appendLF = TRUE) {
  msg(..., appendLF = appendLF)
}

`%+%` <- function(l, r) {
  stopifnot(length(l) == 1, length(r) == 1)
  paste0(as.character(l), as.character(r))
}
