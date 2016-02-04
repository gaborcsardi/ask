
onepage_objs <- function(questions) {
  lapply(
    seq_along(questions),
    function(i) question_call(questions[i])
  )
}

ask_onepage <- function(question_style, questions, .prompt) {
  qlist <- onepage_objs(questions)
  question_style$do(qlist, prompt = .prompt)
}
