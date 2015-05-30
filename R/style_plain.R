
style_plain <- list()

#' @importFrom crayon combine_styles magenta bold

style_plain$confirm <- function(message, default = TRUE) {
  prompt <- c(" (y/N) ", " (Y/n) ")[default + 1]
  emph <- combine_styles(magenta, bold)
  repeat {
    msg(message %+% prompt)
    cat(start(emph))
    ans <- readline()
    res <- NA
    if (ans == "") res <- default
    if (tolower(ans) == "y" || tolower(ans) == "yes") res <- TRUE
    if (tolower(ans) == "n" || tolower(ans) == "no" ) res <- FALSE
    if (!is.na(res)) break
    cat(finish(emph))
    msg("Sorry, did not get it.", appendLF = TRUE);
    cat(start(emph))
  }
    cat(finish(emph))
  res
}

#' @importFrom crayon magenta start finish

style_plain$input <- function(message, default = "", filter = NULL,
                            validate = NULL) {
  if (default != "") message <- message %+% " (" %+% default %+% ")"

  msg(message %+% " ")
  repeat {
    cat(start(magenta))
    result <- readline()
    if (is.null(validate)) break
    valres <- validate(result)
    if (identical(valres, TRUE)) break
    cat(finish(magenta))
    error_msg(valres)
    cat(start(magenta))
  }
  cat(finish(magenta))

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
