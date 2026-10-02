test_that("parser rejects duplicate keys, syntax errors and executable YAML", {
  for (text in c("a: 1\na: 2", "a: {b: 1, b: 2}", "a: [1,", "a: !expr 1 + 1", "a: !unknown data", "a: &x 1", "a: *x", "a: {<<: {b: 1}}", "---\na: 1\n---\na: 2", "%YAML 1.1\na: 1", "...")) {
    path <- yaml_file(text)
    error <- tryCatch(read_form_model(path), model_form_error = identity)
    expect_s3_class(error, "model_form_error")
    expect_match(conditionMessage(error), path, fixed = TRUE)
    expect_identical(error$resource, path)
  }
  expect_error(read_form_model(yaml_file("true: value")), "mapping keys must be strings")
  expect_error(read_form_model(yaml_file("42: value")), "mapping keys must be strings")
  expect_error(read_form_model(yaml_file("a: 1\na: 2")), "Duplicate")
})

test_that("YAML typing never silently coerces identifiers or versions", {
  text <- readLines(example_path())
  for (value in c("1.0", "01", "true", "null", "yes", "off")) {
    replacement <- paste0("  version: ", value)
    changed <- sub("  version:.*", replacement, text)
    expect_error(read_form_model(yaml_file(changed)), "version: expected string")
  }
  changed <- sub("  version:.*", "  version: '01'", text)
  expect_identical(form_metadata(read_form_model(yaml_file(changed)))$form$version, "01")
  d <- definition()
  d$sheets[[1]]$fields[[1]]$default <- "001"
  model <- read_form_model(yaml_file(yaml::as.yaml(d)))
  expect_identical(form_field(model, "Metadata.submission")$default, "001")
  d$sheets[[1]]$fields[[1]]$default <- 1
  expect_bad(d, ".default", "incompatible")
})

test_that("literal YAML text remains data", {
  path <- yaml_file(c(readLines(example_path()), "# & ! * ignored comment"))
  expect_s3_class(read_form_model(path), "form_model")
  raw <- model.form:::read_yaml_data(yaml_file("a: |\n  !tag\n  &anchor\nb: 'can''t !tag'\nc: \"escaped \\\" quote\""))
  expect_identical(raw$a, "!tag\n&anchor\n")
  expect_identical(raw$b, "can't !tag")
})
