
#' Ask questions to the user, at the command line, and get the answers
#'
#' @details
#' Ask a series of questions to the user, and return all
#' results together in a list.
#'
#' The \code{ask} function takes named arguments only:
#' \itemize{
#'   \item Each argument corresponds to a question to the user.
#'   \item The name of the argument is the identifier of the
#'     question, the answer will have the same name in the result list.
#'   \item Each argument is a function call. The name of the function
#'     is the type of the question. See question types below.
#'   \item Questions are asked in the order they are given. See
#'     \sQuote{Conditional execution} below for more flexible workflows.
#' }
#'
#' @section Question types:
#' \describe{
#'   \item{input}{One line of text input.}
#'   \item{confirm}{A yes-no question, \sQuote{y} and \sQuote{yes}
#'     are considered as positive, \sQuote{n} and \sQuote{no} as negative
#'     answers (case insensitively).}
#'   \item{choose}{Choose one item form multiple items.}
#'   \item{checkbox}{Select multiple values from a list.}
#' }
#'
#' @section \sQuote{input} type:
#' \preformatted{
#'   input(message, default = "", filter = NULL, validate = NULL,
#'         when = NULL)
#' }
#' \describe{
#'   \item{\code{message}}{The message to print.}
#'   \item{\code{default}}{The default vaue to return if the user just
#'     presses enter.}
#'   \item{\code{filter}}{If not \code{NULL}, then it must be a function,
#'     that is called to filter the entered result.}
#'   \item{\code{validate}}{If not \code{NULL}, then it must be a function
#'     that is called to validate the input. The function must return
#'     \code{TRUE} for valid inputs and an error message (character scalar)
#'     for invalid ones.}
#'   \item{\code{when}}{See \sQuote{Conditional execution} below.}
#' }
#'
#' @section \sQuote{confirm} type:
#' \preformatted{
#'   confirm(message, default = TRUE, when = NULL)
#' }
#' \describe{
#'   \item{\code{message}}{The message to print.}
#'   \item{\code{default}}{The default answer if the user just presses
#'     enter.}
#'   \item{\code{when}}{See \sQuote{Conditional execution} below.}
#' }
#'
#' @section \sQuote{choose} type:
#' \preformatted{
#'   choose(message, choices, default = NA, when = NULL)
#' }
#' \describe{
#'   \item{\code{message}}{Message to print.}
#'   \item{\code{choices}}{Possible choices, character vector.}
#'   \item{\code{default}}{Index of the default choice (if the user
#'     hits enter, or \code{NA} for no default.}
#'   \item{\code{when}}{See \sQuote{Conditional execution} below.}
#' }
#'
#' @section \sQuote{checkbox} type:
#' \preformatted{
#'   checkbox(message, choices, when = NULL)
#' }
#' \describe{
#'   \item{\code{message}}{Message to print.}
#'   \item{\code{choices}}{Possible choices, character vector.}
#'   \item{\code{when}}{See \sQuote{Conditional execution} below.}
#' }
#'
#' @section Conditional execution:
#' The \code{when} argument to a question can be used for conditional
#' execution of questions. If it is given (and not \code{NULL}), then
#' it must be a function. It is called with the answers list up to that
#' point, and it should return \code{TRUE} or \code{FALSE}. For \code{TRUE},
#' the question is shown to the user and the result is inserted into the
#' answer list. For \code{FALSE}, the question is not shown, and the
#' answer list is not chagned.
#'
#' @param ... Questions to ask, see details below.
#' @param .prompt Prompt to prepend to all questions.
#' @return A named list with the answers.
#'
#' @export
#' @importFrom lazyeval lazy_dots lazy_eval
#' @importFrom crayon green
#' @importFrom prettysymbols symbol
#' @examples
#' \dontrun{
#' ask(
#'   name =  input("What is your name?"),
#'   cool = confirm("Are you cool?"),
#'   drink = choose("Select your poison!", c("Beer", "Wine")),
#'   language = checkbox("Favorite languages?", c("C", "C++", "Python", "R"))
#' )
#' }

ask <- function(..., .prompt = green(paste0(symbol$star, " "))) {

  if (!interactive()) { stop("ask() can only be used in interactive mode" ) }

  qs <- lazy_dots(...)
  qs_names <- names(qs)

  if (is.null(qs_names) || any(qs_names == "") || any(duplicated(qs_names))) {
    stop("Questions must have unique names")
  }

  if (any(unlist(lapply(qs, function(x) class(x$expr))) != "call")) {
    stop("Questions must be function calls")
  }

  qs_fun_names <- unlist(lapply(qs, function(x) as.character(x$expr[[1]])))

  questions <- get_style()

  unknown_fun <- setdiff(qs_fun_names, names(questions))
  if (length(unknown_fun)) {
    stop("Unknown question types: ", paste(unknown_fun, collapse = ", "))
  }

  answers <- list()

  question <- function(message, ..., when = NULL, type, name) {
    if (! type %in% names(questions)) stop("Unknown question type");

    if (!is.null(when) && ! when(answers)) return(NULL)

    answers[[name]] <- questions[[type]](.prompt %+% message, ...)
    answers
  }

  for (q in seq_along(qs)) {
    qs_call <- qs[[q]]
    qs_call$expr[[1]] <- as.name("question")
    qs_call$expr$type <- unname(qs_fun_names[[q]])
    qs_call$expr$name <- unname(qs_names[[q]])
    answers <- lazy_eval(qs_call, data = list(question = question))
  }
  answers
}

#' @importFrom keypress has_keypress_support

get_style <- function() {
  if (can_move_cursor() && has_keypress_support()) {
    style_fancy
  } else {
    style_plain
  }
}
