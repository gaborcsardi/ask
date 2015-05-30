
#' @include style_plain.R

style_fancy <- list()

#' @importFrom keypress keypress

style_fancy$confirm <- function(message, default = TRUE) {
  prompt <- c(" (y/N) ", " (Y/n) ")[default + 1]
  msg(message %+% prompt)
  repeat {
    ans <- keypress()
    ans <- tolower(ans)
    if (ans == 'y' || ans == 'n' || ans == '\n') break
  }
  cat("\n")
  ans == 'y' || (default && ans == '\n')
}

style_fancy$input <- style_plain$input

style_fancy$choose <- function(message, choices, default = NA) {

  current <- default
  if (is.na(current)) current <- 1

  msg(message, appendLF = TRUE)
  
  draw <- function() {
    pr <- paste(ifelse(seq_along(choices) == current, " > ", "   "),
                choices, sep = "", collapse = "\n")
    message(pr)
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
      break;
    }
  }
  
  choices[current]
}
