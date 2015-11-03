
# Friendly R CLI

> `ask` helps build friendly command line interfaces

[![Linux Build Status](https://travis-ci.org/gaborcsardi/ask.svg?branch=master)](https://travis-ci.org/gaborcsardi/ask)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/ask?svg=true)](https://ci.appveyor.com/project/gaborcsardi/ask)
[![](http://www.r-pkg.org/badges/version/ask)](http://www.r-pkg.org/pkg/ask)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/ask)](http://www.r-pkg.org/pkg/ask)

## Introduction

Command line interfaces don't have to be grey and boring.
Just because some ancient terminals do not support color,
Unicode glyphs and cursor movement, that does not mean that
we can't use them on newer ones.

`ask` makes use of terminal colors and other advanced
properties that are actually supported by 99.9% of the
terminals today.

Sadly, some commonly used R IDEs do not emulate a terminal
at all, so `ask()` will look a lot less nice in these
(but it will still work fine):
* R Studio
* R.app (the default OS X R GUI)
* RWin (the default Windows R GUI)
* Emacs ESS

`ask` was inspired by the
[Inquirer.js](https://github.com/SBoudrias/Inquirer.js) project.

## Installation

Install using the `devtools` package:

```r
devtools::install_github("gaborcsardi/readline")
devtools::install_github("gaborcsardi/ask")
```

## Usage

Call the `ask()` function with all your questions to the user,
and the answers are returned in a list. See various question types below.

Typical usage looks like this. (The example is taken from
[Inquirer.js](https://github.com/SBoudrias/Inquirer.js).)

```r
library(ask)
message("Welcome to R Pizza!")
ask(
  to_be_delivered = confirm("Is it for a delivery?", default = FALSE),
  phone = input("What's your phone number?",
    validate = function(v) {
	  good <- grepl("^[- 0-9\\(\\)]+$", v) &&
	          nchar(gsub("[^0-9]", "", v)) == 10
	  if (good) TRUE else "Please enter a valid phone number"
  }),
  size = choose("What size do you need?", c("Large", "Medium", "Small")),
  quantity = input("How many do you need?", validate = function(v) {
      good <- !is.na(as.integer(v))
      if (good) TRUE else "Please enter a number"
	}, filter = as.integer),
  toppings = choose("What about the topping?",
    c("Peperonni and cheese", "All dressed", "Hawaïan")),
  beverage = choose("You also get a free 2L beverage",
    c("Pepsi", "7up", "Coke")),
  comments = input("Any comments about your purchase experience?",
    default = "Nope, all good!"),
  prize = choose("For leaving a comment, you get a freebie",
    c("Cake", "Fries"), when = function(a) a$comments != "Nope, all good!")
)
```

![](/inst/ask-pizza.png)

The result is the answer list, a named list:

```r
$to_be_delivered
[1] FALSE

$phone
[1] "555-555-5555"

$size
[1] "Medium"

$quantity
[1] 2

$toppings
[1] "Peperonni and cheese"

$beverage
[1] "Pepsi"

$comments
[1] "Nice CLI!"

$prize
[1] "Cake"
```

The `ask()` function takes named arguments only:
 * Each argument corresponds to a question to the user.
 * The name of the argument is the identifier of the
   question, the answer will have the same name in the result list.
 * Each argument is a function call. The name of the function
   is the type of the question. See question types below.
 * Questions are asked in the order they are given. See
   [Conditional execution](#conditional-execution) below for more
   flexible workflows.

## Question types

### `input`: one line of text

![](/inst/ask-input.png)

### `confirm`: a yes-no question

![](/inst/ask-confirm.png)

### `choose`: choose one item from a list

![](/inst/ask-choose.png)

### `checkbox`: select multiple values from a list

![](/inst/ask-checkbox.png)

### `constant`: not a question, defines a constant

This is sometimes useful.

## Conditional execution

The `when` argument to a question can be used for conditional
execution of questions. If it is given (and not `NULL`), then
it must be a function. It is called with the answers list up to that
point, and it should return `TRUE` or `FALSE`. For `TRUE`,
the question is shown to the user and the result is inserted into the
answer list. For `FALSE`, the question is not shown, and the
answer list is not chagned.

## License

MIT © [Gábor Csárdi](http://gaborcsardi.org).
