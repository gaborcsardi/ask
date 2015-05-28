
questions <- list()

questions$confirm <- function(message, default = TRUE) {
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

questions$input <- function(message, default = "", filter = NULL) {
  if (default != "") message <- message %+% " (" %+% default %+% ")"
  msg(message %+% " ")
  result <- readline()
  if (!is.null(filter)) result <- filter(result)
  result
}

#' @importFrom utils menu

questions$choose <- function(message, choices, default = NA) {
  default <- as.numeric(default)
  stopifnot(is.na(default) || is_index(choices, default))
  repeat {
    msg(message, appendLF = TRUE)
    msg(paste0(" ", seq_along(choices), ". ", choices, "\n"))
    if (is.na(default)) {
      msg("[?] ")
    } else {
      msg("[?] (" %+% default %+% ") ")
    }
    res <- readline()
    if (res == "" && !is.na(default)) {
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
#'
#'
#' @param ... Questions to ask, see details below.
#' @return A named list with the answers.
#'
#' @export
#' @importFrom lazyeval lazy_dots lazy_eval
#' @examples
#' \dontrun{
#' ask(
#'   name =  input("What is your name?"),
#'   cool = confirm("Are you cool?"),
#'   drink = choose("Select your poison!", c("Beer", "Wine"))
#' )
#' }

ask <- function(...) {

  check_tty()

  qs <- lazy_dots(...)
  qs_names <- names(qs)

  if (is.null(qs_names) || any(qs_names == "") || any(duplicated(qs_names))) {
    stop("Questions must have unique names")
  }

  if (any(unlist(lapply(qs, function(x) class(x$expr))) != "call")) {
    stop("Questions must be function calls")
  }

  qs_fun_names <- unlist(lapply(qs, function(x) as.character(x$expr[[1]])))

  unknown_fun <- setdiff(qs_fun_names, names(questions))
  if (length(unknown_fun)) {
    stop("Unknown question types: ", paste(unknown_fun, collapse = ", "))
  }

  answers <- list()
  type <- NA_character_
  name <- NA_character_

  question <- function(message, ..., validate = NULL, when = NULL) {
    if (! type %in% names(questions)) stop("Unknown question type");

    if (!is.null(when) && ! when(answers)) return(NULL)

    repeat {
      result <- questions[[type]](name, message, ...)
      if (is.null(validate)) break
      valres <- validate(result)
      if (identical(valres, TRUE)) break
      error_msg(valres)
    }

    answers[[name]] <- result
    answers
  }

  for (q in seq_along(qs)) {
    name <- qs_names[q]
    type <- qs_fun_names[q]
    data <- structure(list(question), names = type)
    answers <- lazy_eval(qs[[q]], data = data)
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
