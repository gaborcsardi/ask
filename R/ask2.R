
questions <- R6::R6Class(
  "questions",
  public = list(
    initialize = function(...) ask_init(self, private, ...),
    ask = function(backend = "auto") ask_ask(self, private, backend),
    answers = function() private$answers
  ),

  private = list(
    my_questions = list(),
    my_answers = list()
  )
)

ask_init <- function(self, private, ...) {

  private$my_questions <- check_named(list(...))
  qs_names <- names(private$my_questions)
  for (q in seq_along(private$my_questions)) {
    private$my_questions[[q]]$name <- names(questions)[q]
  }

  private$my_answers <- structure(
    vector(length(qs_names), mode = "list"),
    names = qs_names
  )

  self
}

ask_ask <- function(self, private, backend) {
  backend <- select_backend(backend)
  bclass <- get_backend(backend)
  bobj <- bclass$new()

  bobj$start(private$my_questions)
  for (q in names(private$my_questions)) {
    if (is.null(private$my_questions[[q]]$when) ||
        private$my_questions[[q]]$when(private$answers)) {
      private$my_answers[[q]] <- bobj$ask(private$my_questions[[q]])
    }
  }
  bobj$finish()

  private$my_answers
}
