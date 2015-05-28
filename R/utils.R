
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
