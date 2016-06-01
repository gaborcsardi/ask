
registered_backends <- new.env()

#' @export

register_backend <- function(
  backend,
  package = environmentName(topenv(parent.frame()))) {

  stopifnot(
    is_string(backend),
    is_string(package)
  )

  if (backend %in% ls(registered_backends) &&
      registered_backends[[backend]] != package) {
    message("Overwriting registered ask backend ", backend)
  }

  assign(backend, package, envir = registered_backends)
}

#' @export

deregister_backend <- function(
  backend,
  package = environmentName(topenv(parent.frame()))) {

  stopifnot(
    is_string(backend),
    is_string(package)
  )

  if (! backend %in% ls(registered_backends)) {
    warning("Ask backend not registered: ", backend)

  } else {
    if (! identical(registered_backends[[backend]], package)) {
      warning(
        "Ask backend was registered from package ",
        registered_backends[[backend]], ", removed from package ",
        package
      )
    }
    rm(list = backend, envir = registered_backends)
  }
}

#' @importFrom keypress has_keypress_support

select_backend <- function(backend = "auto") {
  backend <- as.character(backend)
  available <- ls(registered_backends)

  if (backend != "auto") {
    if (! backend %in% available) {
      warning("Selected backend is not available")
    }
    return(backend)
  }

  if (is_rstudio() && "rstudio" %in% available) {
    "rstudio"

  } else if (can_move_cursor() && has_keypress_support()) {
    "fancy"

  } else {
    "plain"
  }
}

get_backend <- function(backend) {
  stopifnot(is_string(backend))

  if (! backend %in% ls(registered_backends)) {
    stop("Ask backend not available: ", backend)
  }

  pkg <- registered_backends[[backend]]

  if (! pkg %in% loadedNamespaces()) {
    stop("Package not loaded for ask backend ", backend, ": ", pkg)
  }

  getExportedValue(pkg, paste0("ask_backend_", backend))
}

#' @export

ask_backend <- R6::R6Class(
  "ask_backend",

  public = list(
    initialize = function() ask_backend_init(self, private),
    start = function(questions) ask_backend_start(self, private, questions),
    finish = function() ask_backend_finish(self, private),
    ask = function(question) ask_backend_ask(self, private, question)
  ),

  private = list(
    questions = NULL
  )
)

ask_backend_init <- function(self, private) {
  ## Nothing to do currently
  self
}

ask_backend_start <- function(self, private, questions) {
  private$questions = questions
  self
}

ask_backend_finish <- function(self, private) {
  ## Nothing to do currently
  self
}

ask_backend_ask <- function(self, private, question) {
  stop("This method must be implemented in the child classes")
}
