
style_rstudio_addin <- list(onepage = TRUE)

style_rstudio_addin$do <- function(questions, prompt) {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Mason, building an R package"),
    miniUI::miniContentPanel(
      h4("Use Mason to build an R package"),
      hr(),
      fluidLayout(
        questions_here(questions)
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      invisible(stopApp())
    })

  }

  viewer <- dialogViewer("Mason", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}

questions_here <- function(questions) {

  question <- function(message, ..., when = NULL, type, name) {
    style_rstudio_addin[[type]](message = message, ...)
  }

  lapply(questions, function(q) {
    result <- lazy_eval(q, data = list(question = question, answers = NULL))
    div(class = "col-xs-%s col-md-%s", result)
  })
}

style_rstudio_addin$input <- function(
  message, default = "", filter = NULL, nextline = TRUE, wrap = TRUE,
  validate = NULL) {
  textInput("text", label = h4(message),  value = default)
}

style_rstudio_addin$confirm <- function(message, default = TRUE) {
  div(
    h4(message),
    checkboxInput("checkbox", label = "Yes", value = default)
  )
}

style_rstudio_addin$choose <- function(message, choices, default = NA) {
  radioButtons(
    "radio",
    label = h4(message),
    choices = choices,
    selected = if (is.na(default)) NULL else default
  )
}

style_rstudio_addin$checkbox <- function(message, choices, default = numeric()) {
  checkboxGroupInput(
    "checkGroup",
    label = h4(message),
    choices = choices,
    selected = choices[default]
  )
}

style_rstudio_addin$constant <- function(message = "", value) {
  value
}
