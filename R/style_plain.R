
style_plain <- list()

style_plain$confirm <- function(message, default = TRUE) {
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

style_plain$input <- function(message, default = "", filter = NULL,
                            validate = NULL) {
  if (default != "") message <- message %+% " (" %+% default %+% ")"

  msg(message %+% " ")
  repeat {
    result <- readline()
    if (is.null(validate)) break
    valres <- validate(result)
    if (identical(valres, TRUE)) break
    error_msg(valres)
  }

  if (!is.null(filter)) result <- filter(result)
  result
}

#' @importFrom utils menu

style_plain$choose <- function(message, choices, default = NA) {
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
