
#' @export

ask_backend_fancy <- R6::R6Class(
  "ask_backend_fancy",
  inherit = ask::ask_backend,
  public = list(
    initialize = function() {
      ask_backend_fancy_init(self, private, super)
    },
    ask = function(question) {
      ask_backend_fancy_ask(self, private, question)
    }
  ),
  private = list(
    emph = NULL
  )
)

#' @importFrom crayon combine_styles green bold

ask_backend_fancy_init <- function(self, private, super) {
  super$initialize()
  private$emph <- combine_styles(green, bold)
  self
}

ask_backend_fancy_ask <- function(self, private, question) {
  switch(
    question$type,
    "confirm"  = ask_backend_fancy_confirm (self, private, question),
    "input"    = ask_backend_fancy_input   (self, private, question),
    "choose"   = ask_backend_fancy_choose  (self, private, question),
    "checkbox" = ask_backend_fancy_checkbox(self, private, question),
    "constant" = ask_backend_fancy_constant(self, private, question),
    "banner"   = ask_backend_fancy_banner  (self, private, question)
  )
}

#' @importFrom keypress keypress
#' @importFrom crayon blue red green
#' @importFrom clisymbols symbol

ask_backend_fancy_confirm <- function(self, private, question) {
  prompt <- c(" (y/N) ", " (Y/n) ")[question$default + 1]
  msg(question$message %+% prompt)
  repeat {
    ans <- keypress()
    ans <- tolower(ans)
    if (ans == 'y' || ans == 'n' || ans == 'enter') break
  }
  ans <- ans == 'y' || (question$default && ans == 'enter')
  msg(c(green(symbol$tick), red(symbol$cross))[2 - ans], "\n", sep = "")
  ans
}

#' @importFrom readline read_line

ask_backend_fancy_input <- function(self, private, question) {

  message  <- question$message
  default  <- question$default
  filter   <- question$filter
  nextline <- question$nextline
  wrap     <- question$wrap
  validate <- question$validate

  tw <- terminal_width()

  orig_message <- message
  if (default != "") message <- message %+% " (" %+% default %+% ")"

  emph <- blue

  if (nextline) {
    premsg <- message %+% start(emph) %+% "\n"
    prompt <- "  "
  } else {
    premsg <- ""
    prompt <- message %+% " " %+% start(emph)
  }

  on.exit(msg(finish(emph)), add = TRUE)

  repeat {
    cat(premsg)
    orig_result <- result <- read_line(prompt, multiline = nextline)
    if (result == "") { orig_result <- result; result <- default }
    if (is.null(validate)) break
    valres <- validate(result)
    if (identical(valres, TRUE)) break
    error_msg(finish(emph), valres)
  }

  if (nextline) {
    uplines <- ceiling((nchar(result) + 3) / tw) + 1
    cursor_up(uplines)
    spaces1 <- make_spaces(tw)
    spaces2 <- make_spaces(nchar(orig_result, type = "width"))
    msg(orig_message %+% spaces1 %+% "\r  " %+% spaces2, appendLF = TRUE)

    cursor_up(uplines - 1)
    msg(green(wrap_if(result, wrap)), appendLF = TRUE)

  } else if (!nextline) {
    cursor_up(1)
    spaces <- make_spaces(nchar(default, type = "width") + 3)
    msg(orig_message %+% " " %+% green(result) %+% spaces, appendLF = TRUE)
  }

  result <- sub("^  ", "", wrap_if(result, wrap))

  if (!is.null(filter)) result <- filter(result)
  result
}

ask_backend_fancy_choose <- function(self, private, question) {

  message <- question$message
  choices <- question$choices
  default <- question$default

  if (is.character(default)) default <- pmatch(default, choices)
  default <- as.numeric(default)
  stopifnot(is.na(default) || is_index(choices, default))

  current <- default
  if (is.na(current)) current <- 1

  msg(message, appendLF = TRUE)

  draw <- function(empty = FALSE) {
    choices[current] <- blue(choices[current])
    pointer <- blue(symbol$pointer)
    pr <- paste("", ifelse(seq_along(choices) == current, pointer, " "),
                choices, collapse = "\n")
    if (empty) pr <- gsub("[^\n]", " ", pr)
    msg(pr, appendLF = TRUE)
  }

  draw()
  repeat {
    repeat {
      ans <- keypress()
      if (ans %in% c("up", "down", "n", "p", "enter", " ")) break
    }
    if (ans %in% c("up", "p") && current != 1) {
      current <- current - 1
      cursor_up(length(choices))
      draw()
    } else if (ans %in% c("down", "n") && current != length(choices)) {
      current <- current + 1
      cursor_up(length(choices))
      draw()
    } else if (ans %in% c("enter", " ")) {
      break
    }
  }

  cursor_up(length(choices) + 1)
  msg(message, " ", green(choices[current]), appendLF = TRUE)
  draw(empty = TRUE)
  cursor_up(length(choices))

  choices[current]
}

ask_backend_fancy_checkbox <- function(self, private, question) {

  message <- question$message
  choices <- question$choices
  default <- question$default

  choices <- as.character(choices)

  if (is.character(default)) default <- pmatch(default, choices)
  default <- as.numeric(default)

  selected <- default
  current <- 1

  msg(message, appendLF = TRUE)

  draw <- function(empty = FALSE) {
    choices[selected] <- green(choices[selected])
    pointer <- blue(symbol$pointer)
    box_empty <- symbol$radio_off
    box_fill <- green(symbol$radio_on)
    pr <- paste(
      "",
      ifelse(seq_along(choices) == current, pointer, " "),
      ifelse(seq_along(choices) %in% selected, box_fill, box_empty),
      choices, collapse = "\n"
    )
    if (empty) pr <- gsub("[^\n]", " ", pr)
    msg(pr, appendLF = TRUE)
  }

  draw()
  repeat {
    repeat {
      ans <- keypress()
      if (ans %in% c("up", "down", "n", "p", "enter", " ")) break
    }
    if (ans %in% c("up", "p") && current != 1) {
      current <- current - 1
      cursor_up(length(choices))
      draw()

    } else if (ans %in% c("down", "n") && current != length(choices)) {
      current <- current + 1
      cursor_up(length(choices))
      draw()

    } else if (ans == " ") {
      if (current %in% selected) {
        selected <- setdiff(selected, current)
      } else {
        selected <- c(selected, current)
      }
      cursor_up(length(choices))
      draw()

    } else if (ans == "enter") {
      break
    }
  }

  res <- choices[sort(selected)]

  cursor_up(length(choices) + 1)
  msg(message, " ", paste(green(res), collapse = ", "), appendLF = TRUE)
  draw(empty = TRUE)
  cursor_up(length(choices))

  res
}

ask_backend_fancy_constant <- function(self, private, question) {
  question$value
}

ask_backend_fancy_banner <- function(self, private, question) {
  msg(question$message, appendLF = TRUE)
  NULL
}
