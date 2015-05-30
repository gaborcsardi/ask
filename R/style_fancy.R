
#' @include style_plain.R
#' @importFrom prettysymbols symbol

style_fancy <- list()

#' @importFrom keypress keypress
#' @importFrom crayon magenta red bold

style_fancy$confirm <- function(message, default = TRUE) {
  prompt <- c(" (y/N) ", " (Y/n) ")[default + 1]
  msg(message %+% prompt)
  repeat {
    ans <- keypress()
    ans <- tolower(ans)
    if (ans == 'y' || ans == 'n' || ans == '\n') break
  }
  ans <- ans == 'y' || (default && ans == '\n')
  msg(c(magenta(bold('Y')), red('N'))[2 - ans], "\n", sep = "")
  ans
}

style_fancy$input <- style_plain$input

#' @importFrom crayon magenta

style_fancy$choose <- function(message, choices, default = NA) {

  current <- default
  if (is.na(current)) current <- 1

  msg(message, appendLF = TRUE)

  draw <- function() {
    choices[current] <- magenta(choices[current])
    pointer <- magenta(symbol$pointer)
    pr <- paste("", ifelse(seq_along(choices) == current, pointer, " "),
                choices, collapse = "\n")
    msg(pr, appendLF = TRUE)
  }

  draw()
  repeat {
    repeat {
      ans <- keypress()
      if (ans %in% c("up", "down", "n", "p", "\n", " ")) break
    }
    if (ans %in% c("up", "p") && current != 1) {
      current <- current - 1
      cursor_up(length(choices))
      draw()
    } else if (ans %in% c("down", "n") && current != length(choices)) {
      current <- current + 1
      cursor_up(length(choices))
      draw()
    } else if (ans %in% c("\n", " ")) {
      break
    }
  }

  choices[current]
}

style_fancy$checkbox <- function(message, choices) {

  choices <- as.character(choices)
  selected <- numeric()
  current <- 1

  msg(message, appendLF = TRUE)

  draw <- function() {
    choices[selected] <- magenta(choices[selected])
    pointer <- magenta(symbol$pointer)
    box_empty <- symbol$radio_off
    box_fill <- magenta(symbol$radio_on)
    pr <- paste(
      "",
      ifelse(seq_along(choices) == current, pointer, " "),
      ifelse(seq_along(choices) %in% selected, box_fill, box_empty),
      choices, collapse = "\n"
    )
    msg(pr, appendLF = TRUE)
  }

  draw()
  repeat {
    repeat {
      ans <- keypress()
      if (ans %in% c("up", "down", "n", "p", "\n", " ")) break
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

    } else if (ans == "\n") {
      break
    }
  }
  choices[sort(selected)]
}
