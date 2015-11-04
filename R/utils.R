
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
