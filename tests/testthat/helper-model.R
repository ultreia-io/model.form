example_path <- function() form_resource("examples/survey-form.yaml", "model.form")
example_model <- function() read_form_model(example_path())
definition <- function() form_definition(example_model())
expect_bad <- function(d, property, message) {
  result <- check_form_model(d, "fixture.yaml")
  expect_false(result$valid)
  selected <- result$diagnostics[grepl(property, result$diagnostics$path, fixed = TRUE), ]
  expect_gt(nrow(selected), 0L)
  expect_true(any(grepl(message, selected$message, fixed = TRUE)))
  expect_true(all(result$diagnostics$resource == "fixture.yaml"))
  expect_error(as_form_model(d, "fixture.yaml"), class = "model_form_error")
}
yaml_file <- function(text) {
  p <- tempfile(fileext = ".yaml")
  writeLines(text, p, useBytes = TRUE)
  p
}
