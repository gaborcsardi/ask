
style_plain <- list()

#' @importFrom prettysymbols symbol
#' @importFrom crayon combine_styles magenta bold start finish

style_plain$confirm <- function(message, default = TRUE) {
  prompt <- c(" (y/N) ", " (Y/n) ")[default + 1]
  emph <- combine_styles(magenta, bold)
  repeat {
    ans <- readline(prompt = bold(message) %+% prompt %+% start(emph))
    res <- NA
    if (ans == "") res <- default
    if (tolower(ans) == "y" || tolower(ans) == "yes") res <- TRUE
    if (tolower(ans) == "n" || tolower(ans) == "no" ) res <- FALSE
    if (!is.na(res)) break
    msg(finish(emph) %+% "Sorry, did not get it.", appendLF = TRUE)
  }
  msg(finish(emph))
  res
}

#' @importFrom crayon combine_styles magenta bold start finish

style_plain$input <- function(message, default = "", filter = NULL,
                            validate = NULL) {
  if (default != "") message <- message %+% " (" %+% default %+% ")"

  emph <- combine_styles(magenta, bold)

  repeat {
    result <- readline(bold(message) %+% " " %+% start(emph))
    if (is.null(validate)) break
    valres <- validate(result)
    if (identical(valres, TRUE)) break
    error_msg(finish(emph), valres)
  }
  msg(finish(emph))

  if (!is.null(filter)) result <- filter(result)
  result
}

#' @importFrom crayon green magenta bold combine_styles

style_plain$choose <- function(message, choices, default = NA) {
  default <- as.numeric(default)
  stopifnot(is.na(default) || is_index(choices, default))
  emph <- combine_styles(magenta, bold)

  msg(
    bold(message),
    "\n",
    paste0(" ", seq_along(choices), ". ", choices, collapse = "\n"),
    "\n"
  )

  repeat {
    prompt <- paste0(
      green(symbol$fancy_question_mark), " ",
      if (! is.na(default)) " (" %+% default %+% ") " else "",
      start(emph)
    )
    res <- readline(prompt = prompt)
    msg(finish(emph))
    if (res == "" && !is.na(default)) {
      res <- default
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

#' @importFrom crayon green magenta bold combine_styles

style_plain$checkbox <- function(message, choices) {

  choices <- as.character(choices)
  emph <- combine_styles(magenta, bold)

  msg(
    bold(message),
    "\n",
    paste0(" ", seq_along(choices), ". ", choices, collapse = "\n"),
    "\n"
  )

  repeat {

    prompt <- paste0(
      green(symbol$fancy_question_mark),
      " (Please use comma separated numbers) ",
      start(emph)
    )

    res <- strtrim(strsplit(strtrim(readline(prompt = prompt)), ",")[[1]])
    msg(finish(emph))
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
