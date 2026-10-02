test_that("schema rejects missing, unsupported and mistyped properties", {
  d <- definition(); d$extra <- TRUE
  expect_bad(d, "$.extra", "unsupported property")
  d <- definition(); d$form$id <- NULL
  expect_bad(d, "$.form.id", "missing required")
  d <- definition(); d$form["id"] <- list(NULL)
  expect_bad(d, "$.form.id", "expected string")
  d <- definition(); d$form$id <- "bad id"
  expect_bad(d, "$.form.id", "pattern")
  d <- definition(); d$form$title <- "unsupported"
  expect_bad(d, ".title", "unsupported")
  d <- definition(); d$sheets <- stats::setNames(list(), character())
  expect_bad(d, "$.sheets", "at least")
  d <- definition(); d$schema_version <- "2.0.0"
  expect_bad(d, "$.schema_version", "unsupported value")
  d <- definition(); d$sheets[[1]]$fields[[1]]$mandatory <- "true"
  expect_bad(d, ".mandatory", "expected boolean")
  d <- definition(); d$form <- c(d$form, list(id = "duplicate"))
  expect_bad(d, "$.form.id", "duplicate property")
  for (v in list(new.env(), function() 1, as.Date("2026-01-01"), NA, Inf, NaN, c(1, 2))) {
    d <- definition(); d$sheets[[1]]$fields[[1]]$default <- v
    expect_false(check_form_model(d)$valid)
  }
  d <- definition(); d$sheets[[1]]$fields[[1]]$location <- "A0"
  expect_bad(d, ".location", "pattern")
})

test_that("mapping keys and reference requirements are linked", {
  d <- definition(); d$references <- c(d$references, d$references[1])
  expect_bad(d, "$.references", "duplicate reference")
  d <- definition(); names(d$sheets)[2] <- "metadata"
  expect_bad(d, "$.sheets", "duplicate workbook")
  d <- definition(); d$sheets[[2]]$fields[[1]]$reference <- "absent"
  expect_bad(d, ".reference", "unknown reference")
  d <- definition(); d$sheets[[2]]$fields[[2]]$reference <- list(id = "stations")
  expect_bad(d, ".reference.id", "string field")
  d <- definition(); d$sheets[[2]]$fields[[4]]$required_field <- "absent"
  expect_bad(d, ".required_field", "unknown field")
  d <- definition(); d$sheets[[2]]$fields[[4]]$required_field <- "unit"
  expect_bad(d, ".required_field", "own mandatory")
})

test_that("declared constraints and defaults are coherent", {
  d <- definition(); d$sheets[[2]]$fields[[2]]$minimum <- 1001
  expect_bad(d, ".minimum", "exceeds maximum")
  d <- definition(); d$sheets[[2]]$fields[[2]]$minimum <- 0.5
  expect_bad(d, ".fields.", "whole numbers")
  d <- definition(); d$sheets[[2]]$fields[[1]]$minimum <- 1
  expect_bad(d, ".fields.", "numeric bounds")
  d <- definition(); d$sheets[[2]]$fields[[2]]$pattern <- "["
  expect_bad(d, ".pattern", "text or temporal")
  expect_bad(d, ".pattern", "invalid PCRE")
  d <- definition(); d$sheets[[2]]$fields[[4]]$enum <- list("cm", "cm")
  expect_bad(d, ".enum", "duplicate enum")
  d <- definition(); d$sheets[[2]]$fields[[2]]$enum <- list(-1, 1001)
  expect_bad(d, ".enum[1]", "below minimum")
  expect_bad(d, ".enum[2]", "above maximum")
  d <- definition(); d$sheets[[2]]$fields[[4]]$default <- "feet"
  expect_bad(d, ".default", "not in enum")
  d <- definition(); d$sheets[[1]]$fields[[1]]$default <- "bad"
  expect_bad(d, ".default", "does not match pattern")
  for (v in c("2026-02-30", "tomorrow", "2026-2-1")) {
    d <- definition(); d$sheets[[1]]$fields[[2]]$default <- v
    expect_bad(d, ".default", "incompatible")
  }
  d <- definition(); d$sheets[[1]]$fields[[2]]$default <- "2026-02-28"
  d$sheets[[2]]$fields[[6]]$default <- "2026-02-28T12:01:02Z"
  expect_true(check_form_model(d)$valid)
  d$sheets[[2]]$fields[[6]]$default <- "2026-02-28T99:01:02Z"
  expect_bad(d, ".default", "incompatible")
})

test_that("workbook layout locations and extents are checked", {
  changes <- list(
    list(c("sheets",2,"layout","start_row"), NULL, ".start_row", "requires start_row"),
    list(c("sheets",2,"layout","column_count"), NULL, ".column_count", "requires column_count"),
    list(c("sheets",1,"layout","column_count"), NULL, ".column_count", "requires column_count"),
    list(c("sheets",1,"layout","start_row"), 1L, ".layout", "cannot have table"),
    list(c("sheets",2,"layout","header_row"), 6L, ".header_row", "unsupported property"),
    list(c("sheets",2,"layout","row_count"), 1L, ".start_row", "sheet extent"),
    list(c("sheets",1,"fields",1,"location"), NULL, ".location", "requires a location"),
    list(c("sheets",1,"fields",1,"header"), "X", ".header", "unsupported property"),
    list(c("sheets",1,"fields",1,"location"), "C100", ".location", "sheet extent"),
    list(c("sheets",1,"fields",1,"location"), "CV2", ".location", "sheet extent"),
    list(c("sheets",1,"fields",2,"location"), "C2", ".location", "duplicate workbook location"),
    list(c("sheets",2,"fields",1,"location"), "A1", ".location", "declared order"),
    list(c("sheets",2,"fields",1,"column"), 1L, ".column", "unsupported property"),
    list(c("sheets",2,"fields",2,"header"), "STATION", ".header", "unsupported property"),
    list(c("sheets",2,"layout","column_count"), 5L, ".column_count", "field count")
  )
  set <- function(x, path, value) {
    key <- path[[1]]
    if (grepl("^[0-9]+$", key)) key <- as.integer(key)
    if (length(path) == 1L) x[[key]] <- value else x[[key]] <- set(x[[key]], path[-1], value)
    x
  }
  for (change in changes) expect_bad(set(definition(), change[[1]], change[[2]]), change[[3]], change[[4]])
  d <- definition()
  d$sheets$Empty <- list(fields = stats::setNames(list(), character()),
                         layout = list(kind = "table", column_count = 0L,
                                       start_row = 1L, row_count = 0L))
  expect_true(check_form_model(d)$valid)
})

test_that("temporal defaults must be text", {
  d <- definition(); d$sheets[[1]]$fields[[2]]$default <- 20260101
  expect_bad(d, ".default", "incompatible")
})

test_that("compact constraints reject invalid declarations", {
  d <- definition(); d$sheets$Metadata$fields$comment$constraints <- list(mandatory = TRUE)
  expect_bad(d, ".constraints", "unsupported property")
  d <- definition(); d$sheets$Metadata$fields$comment$mandatory <- TRUE
  expect_bad(d, ".default", "incompatible")
  for (reference in list("bad id", "", TRUE, 42, list(id = "stations", severity = "notice"))) {
    d <- definition(); d$sheets$Observations$fields$station$reference <- reference
    expect_false(check_form_model(d)$valid)
  }
  d <- definition(); d$sheets$Observations$fields$station$reference <- list(id = "missing")
  expect_bad(d, ".reference.id", "unknown reference")
})

test_that("locations are single uppercase A1 addresses", {
  for (location in list("", "E0", "E01", "e3", "3E", "E-3", "E3:F4", "$E$3", 3, list(row = 3, column = 5))) {
    d <- definition(); d$sheets$Metadata$fields$submission$location <- location
    expect_false(check_form_model(d)$valid)
  }
  d <- definition(); d$sheets$Metadata$fields$submission$row <- 2L
  expect_bad(d, ".row", "unsupported property")
})

test_that("required_field accepts exactly one local field name", {
  for (value in list(list("length"), list("length", "count"), list(fields = list("length")), "", TRUE)) {
    d <- definition(); d$sheets$Observations$fields$unit$required_field <- value
    expect_false(check_form_model(d)$valid)
  }
})


test_that("cell layouts may omit an exact row extent", {
  d <- definition()
  d$sheets$Metadata$layout$row_count <- NULL
  d$sheets$Metadata$fields$comment$location <- "C25"
  expect_true(check_form_model(d)$valid)
  expect_equal(form_field(as_form_model(d), "Metadata.comment")$row, 25L)
})
