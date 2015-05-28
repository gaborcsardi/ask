
check_tty <- function() {
  stopifnot(isatty(stdin()))
}

is_integerish <- function(x) {
  stopifnot(all(round(x) == x))
}
