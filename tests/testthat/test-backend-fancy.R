
context("Fancy backend")

test_that("confirm", {

  skip("Testing not implemented yet")

  q <- questions$new(
    cool = confirm("Are you cool"),
    gas  = confirm("Are you gas", default = FALSE)
  )

  q$ask(backend = "fancy")
})

test_that("input", {

  skip("Testing not implemented yet")

  q <- questions$new(
    name  = input("Name", default = "foobar"),
    color = input("Color")
  )

  q$ask(backend = "fancy")
})

test_that("input, filter", {

  skip("Testing not implemented yet")

  q <- questions$new(
    name = input("Name", filter = tolower)
  )

  q$ask(backend = "fancy")
})

test_that("input, validate", {

  skip("Testing not implemented yet")

  q <- questions$new(
    name = input(
      "Name",
      validate = function(x) {
        if (nchar(x) == 0) "Please type a name" else TRUE
      }
    )
  )

  q$ask(backend = "fancy")
})

test_that("choose", {

  skip("Testing not implemented yet")

  q <- questions$new(
    engine = choose(
      "Favorite engine",
      choices = c("Thomas", "Rosie", "Hero", "Cathlyn", "Gordon")
    )
  )

  q$ask(backend = "fancy")
})

test_that("choose, default", {

  skip("Testing not implemented yet")

  q <- questions$new(
    engine = choose(
      "Favorite engine",
      choices = c("Thomas", "Rosie", "Hero", "Cathlyn", "Gordon"),
      default = "Rosie"
    )
  )

  q$ask(backend = "fancy")
})

test_that("checkbox", {

  skip("Testing not implemented yet")

  q <- questions$new(
    language = checkbox(
      "Programming languages",
      choices = c("C", "C++", "JavaScript", "Python", "R")
    )
  )

  q$ask(backend = "fancy")
})

test_that("checkbox, default", {

  skip("Testing not implemented yet")

  q <- questions$new(
    language = checkbox(
      "Programming languages",
      choices = c("C", "C++", "JavaScript", "Python", "R"),
      default = c("JavaScript", "R")
    )
  )

  q$ask(backend = "fancy")
})

test_that("constant", {

  skip("Testing not implemented yet")

  q <- questions$new(
    cool = constant(value = "yes")
  )

  q$ask(backend = "fancy")
})

test_that("banner", {

  skip("Testing not implemented yet")

  q <- questions$new(
    welcome = banner("Welcome!"),
    cool = confirm("Are you cool?", default = TRUE),
    bye = banner("Thanks, bye now!")
  )

  q$ask(backend = "fancy")
})

