
check_tty <- function() {
  stopifnot(isatty(stdin()))
}

is_integerish <- function(x) {
  all(round(x) == x)
}

is_index <- function(vector, idx) {
  is_integerish(idx) && length(idx) == 1 &&
    idx >= 1 && idx <= length(vector)
}

#' @importFrom crayon bold

msg <- function(..., appendLF = FALSE) {
  message(finish(bold), ..., appendLF = appendLF)
}

#' @importFrom clisymbols symbol
#' @importFrom crayon red

error_msg <- function(..., appendLF = TRUE, markup = TRUE) {
  str <- paste0(...)
  if (markup) str <- red(paste0(symbol$cross, " ", str))
  msg(str, appendLF = appendLF)
}

`%+%` <- function(l, r) {
  stopifnot(length(l) == 1, length(r) == 1)
  paste0(as.character(l), as.character(r))
}

can_move_cursor <- function() {
  cmd <- "(tput cuu1 && tput cud1) > /dev/null 2> /dev/null"
  suppressWarnings(try(system(cmd), silent = TRUE)) == 0
}

cursor_up <- function(num) {
  cat("\033[", num, "A", sep = "")
}

strtrim <- function(x) {
  gsub("\\s+$", "", gsub("^\\s+", "", x))
}

make_spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}

terminal_width <- function() {
  as.numeric(system("tput cols", intern = TRUE))
}

wrap_if <- function(x, wrap) {
  if (wrap) {
    paste(
      strwrap(x, indent = 2, exdent = 2, width = terminal_width()),
      collapse = "\n"
    )
  } else {
    x
  }
}

is_rstudio <- function() {
  Sys.getenv("RSTUDIO") == 1
}

is_pkg_installed <- function(pkg) {
  tryCatch(
    { packageDescription(pkg); TRUE },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
}

has_rstudio_addin_support <- function() {
  is_rstudio() &&
    rstudioapi::versionInfo()$version >= "0.99.857" &&
    is_pkg_installed("shiny") &&
    packageVersion("shiny") >= "0.13.0" &&
    is_pkg_installed("rstudioapi") &&
    is_pkg_installed("htmltools") &&
    packageVersion("htmltools") >= "0.3" &&
    is_pkg_installed("miniUI") &&
    packageVersion("miniUI") >= "0.1.1"
}
