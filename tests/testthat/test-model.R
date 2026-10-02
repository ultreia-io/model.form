test_that("installed resources and ordered accessors work independently of cwd", {
  m <- example_model()
  expect_s3_class(m, "form_model")
  expect_identical(m$form$id, "survey")
  expect_true(check_form_model(form_definition(m))$valid)
  expect_identical(names(form_fields(m, "Metadata")),
                   c("Metadata.submission", "Metadata.date", "Metadata.comment"))
  expect_identical(names(form_sheets(m)), c("Metadata", "Observations"))
  expect_identical(form_references(m), "stations")
  expect_identical(form_layout(m, "Observations")$start_row, 6L)
  expect_identical(form_field(m, "Observations.unit")$enum, list("cm", "m"))
  expect_identical(form_field(m, "Observations.station")$reference$severity, "error")
  expect_identical(form_metadata(m)$schema_version, "1.0.0")
  expect_identical(form_metadata(m)$form$version, "2026.1")
  expect_output(print(m), "survey @ 2026.1")
  expect_output(print(check_form_model(form_definition(m))), "valid")
  expect_output(print(check_form_model(list())), "invalid")
  expect_error(form_fields(m, "absent"), "unknown sheet")
  expect_error(form_field(m, "absent"), "unknown field")
  expect_error(form_layout(m, "absent"), "unknown sheet")
  expect_error(form_definition(list()), class = "model_form_error")
})

test_that("compact declarations expose effective field values", {
  m <- example_model()
  expect_identical(names(m$sheets$Metadata$fields), c("submission", "date", "comment"))
  expect_false("mandatory" %in% names(m$sheets$Metadata$fields$comment))
  expect_identical(m$sheets$Observations$fields$station$reference, "stations")
  expect_false(form_field(m, "Metadata.comment")$mandatory)
  expect_true(form_field(m, "Metadata.submission")$mandatory)
  expect_identical(form_field(m, "Observations.count")$column, 2L)
  expect_identical(form_field(m, "Observations.station")$reference$severity, "error")

  d <- form_definition(m)
  d$form$version <- "2026.2"
  revised <- as_form_model(d)
  expect_identical(form_metadata(revised)$form$version, "2026.2")
  expect_identical(form_metadata(m)$form$version, "2026.1")
  schema <- form_schema()
  schema$contract_version <- "changed"
  expect_identical(form_schema()$contract_version, "1.0.0")
  expect_error(form_schema("2"), "unsupported schema")
})

test_that("selection requires exact form identity and rejects ambiguity", {
  path <- example_path()
  expect_identical(select_form_model(path, "survey", "2026.1"), example_model())
  expect_s3_class(select_form_model("examples/survey-form.yaml", "survey", "2026.1", "model.form"), "form_model")
  expect_error(select_form_model(path, "Survey", "2026.1"), "matched 0")
  expect_error(select_form_model(path, "survey", "2026"), "matched 0")
  expect_error(select_form_model(c(path, path), "survey", "2026.1"), "matched 2")
  expect_error(select_form_model(character(), "x", "1"), "paths")
  expect_error(select_form_model(path, "survey", 1), "version")
  expect_error(select_form_model(c(path, yaml_file("invalid")), "survey", "2026.1"), "expected mapping")
})

test_that("resources and argument errors are explicit", {
  expect_error(form_resource("../DESCRIPTION", "model.form"), "parent traversal")
  expect_error(form_resource("/tmp/missing", "model.form"), "relative path")
  expect_error(form_resource("DESCRIPTION", "not.a.real.package.123"), "not installed")
  expect_error(form_resource("missing", "model.form"), "does not exist")
  expect_error(form_resource("examples", "model.form"), "file does not exist")
  expect_error(read_form_model(tempfile()), "does not exist")
  expect_error(read_form_model(tempdir()), "regular file")
  expect_error(read_form_model(NA_character_), "nonempty string")
  expect_error(check_form_model(definition(), resource = ""), "nonempty string")
})

test_that("resource symlinks cannot escape their package directory", {
  skip_on_os("windows")
  root <- tempfile("resources-")
  pkg <- file.path(root, "exampleforms")
  dir.create(pkg, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c("Package: exampleforms", "Version: 1.0.0"), file.path(pkg, "DESCRIPTION"))
  outside <- file.path(root, "outside.yaml")
  writeLines("outside: package", outside)
  expect_true(file.symlink(outside, file.path(pkg, "escape.yaml")))
  old <- .libPaths()
  on.exit(.libPaths(old), add = TRUE)
  .libPaths(c(root, old))
  expect_error(form_resource("escape.yaml", "exampleforms"),
               "escapes package", class = "model_form_error")
})

test_that("hidden mutable attributes cannot enter model values", {
  d <- definition()
  attr(d, "state") <- new.env()
  expect_bad(d, "$", "expected mapping")
  d <- definition()
  attr(d$sheets[[1]]$fields[[1]]$type, "state") <- new.env()
  expect_bad(d, ".type", "expected string")
})

test_that("reference shorthand preserves declarations and resolves severity", {
  compact <- example_model()
  d <- form_definition(compact)
  d$sheets$Observations$fields$station$reference <- list(id = "stations", severity = "error")
  expect_identical(form_fields(compact), form_fields(as_form_model(d)))
  d$sheets$Observations$fields$station$reference$severity <- "warning"
  expect_identical(form_field(as_form_model(d), "Observations.station")$reference,
                   list(id = "stations", severity = "warning"))
  expect_identical(form_definition(compact)$sheets$Observations$fields$station,
                   list(mandatory = TRUE, type = "string", reference = "stations"))
})

test_that("cell locations decode across column letter boundaries", {
  columns <- c(Z12 = 26, AA12 = 27, AZ12 = 52, BA12 = 53, ZZ12 = 702, AAA12 = 703)
  for (location in names(columns)) {
    d <- definition()
    d$sheets$Metadata$layout$row_count <- 12L
    d$sheets$Metadata$layout$column_count <- 703L
    d$sheets$Metadata$fields$submission$location <- location
    field <- form_field(as_form_model(d), "Metadata.submission")
    expect_equal(field[c("row", "column")], list(row = 12, column = unname(columns[[location]])))
    expect_identical(field$location, location)
    expect_false(any(c("row", "column", "constraints") %in% names(d$sheets$Metadata$fields$submission)))
  }
  expect_true(all(vapply(form_sheets(example_model()), function(sheet) {
    identical(names(sheet), c("layout", "fields"))
  }, logical(1))))
})

test_that("required_field remains one name in declarations and accessors", {
  m <- example_model()
  expect_identical(form_definition(m)$sheets$Observations$fields$unit$required_field, "length")
  expect_identical(form_field(m, "Observations.unit")$required_field, "length")
})
