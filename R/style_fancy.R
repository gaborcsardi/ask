
#' @include style_plain.R

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
  cat(c(magenta(bold('Y')), red('N'))[2 - ans], "\n", sep = "")
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
    pr <- paste(ifelse(seq_along(choices) == current, magenta(" > "), "   "),
                choices, sep = "", collapse = "\n")
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
