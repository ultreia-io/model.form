#' Check the model argument shape
#'
#' Require a complete definition list inheriting from `form_model`.
#' This is a shallow guard, not structural or semantic definition validation.
#' @param model Object passed to a model accessor.
#' @return Invisibly returns `NULL` on success.
#' @section Errors:
#' Raises `model_form_error` with resource `"<argument>"` and path `"model"`
#' when the object does not have the expected shape.
#' @keywords internal
model_arg <- function(model) {
  required <- c("schema_version", "form", "references", "sheets")
  if (!inherits(model, "form_model") || !is.list(model) ||
      !all(required %in% names(model))) {
    abort_model("<argument>", "model", "expected a form_model; use as_form_model()")
  }
}

#' Get the complete model definition
#'
#' Return the checked definition as an ordinary nested list, preserving sequence
#' order, absent properties, and explicit `NULL` values.
#' @param model A `form_model` returned by [read_form_model()] or [as_form_model()].
#' @return A named list containing `schema_version`, `form`, `references`, and
#'   `sheets`, as described by [form_schema()].
#' @details Accessors do not revalidate the definition. Changing a returned list
#'   leaves the original model unchanged. Pass an edited definition to
#'   [as_form_model()] to check it and construct a new model.
#' @section Errors:
#' Raises `model_form_error` for an invalid model argument. Direct edits to model
#' internals bypass construction checks and can cause other accessor errors.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' names(form_definition(m))
#' d <- form_definition(m)
#' d$form$version <- "2026.2"
#' revised <- as_form_model(d)
#' form_metadata(revised)$form$version
#' form_metadata(m)$form$version # Original version is unchanged.
#' @export
form_definition <- function(model) {
  model_arg(model)
  unclass(model)
}

#' Get form identity and versions
#'
#' Read the form identity and model schema version without fields or sheets.
#' @inheritParams form_definition
#' @return A named list with `form` and `schema_version`.
#' @section Errors:
#' Raises `model_form_error` for an invalid model argument.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' meta <- form_metadata(m)
#' meta$form$id
#' meta$form$version
#' @seealso [form_definition()]
#' @export
form_metadata <- function(model) {
  d <- form_definition(model)
  d[c("form", "schema_version")]
}

#' Get fields from all sheets or one sheet
#'
#' Flatten selected sheets into a list named `<sheet>.<field>`. Fields retain
#' sheet order, then their order within each sheet. Derived `id`, `name`, `sheet`,
#' `column`, `mandatory`, and reference severity values make the public result
#' directly usable while [form_definition()] retains the compact declaration.
#' @inheritParams form_definition
#' @param sheet One exact, case-sensitive worksheet name, or `NULL` for all sheets.
#' @return A named list of effective field definitions.
#' @details An omitted `mandatory` becomes `FALSE`. A reference ID
#'   shorthand expands to a mapping; omitted reference `severity` becomes
#'   `"error"`. Numeric cell coordinates, table columns, and full field IDs are derived.
#'   No other optional property receives a value. Use [form_definition()] when
#'   you need the compact declaration with omissions preserved.
#' @section Errors:
#' Raises `model_form_error` for an invalid model, invalid `sheet` argument,
#' or unknown worksheet name. There is no partial matching or fallback.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' names(form_fields(m))
#' fields <- form_fields(m, sheet = "Observations")
#' names(fields)
#' vapply(fields, function(field) field$type, character(1))
#' @seealso [form_field()], [form_sheets()]
#' @export
form_fields <- function(model, sheet = NULL) {
  sheets <- form_sheets(model)
  if (!is.null(sheet)) {
    text_arg(sheet, "sheet")
    if (!sheet %in% names(sheets)) abort_model("<model>", "sheets", paste0("unknown sheet: ", sheet))
    sheets <- sheets[sheet]
  }
  result <- list()
  for (sheet_name in names(sheets)) {
    definition <- sheets[[sheet_name]]
    for (i in seq_along(definition$fields)) {
      local_name <- names(definition$fields)[[i]]
      field <- definition$fields[[i]]
      field$id <- paste(sheet_name, local_name, sep = ".")
      field$name <- local_name
      field$sheet <- sheet_name
      field$mandatory <- isTRUE(field$mandatory)
      if (definition$layout$kind == "table") field$column <- i else {
        cell <- cell_coordinates(field$location)
        field$row <- cell$row
        field$column <- cell$column
      }
      if (is.character(field$reference)) {
        field$reference <- list(id = field$reference)
      }
      if (!is.null(field$reference) && is.null(field$reference$severity)) {
        field$reference$severity <- "error"
      }
      result[[field$id]] <- field
    }
  }
  result
}

#' Get one field by identifier
#'
#' Look up a field across all sheets using its model-wide unique ID.
#' The declared constraints are returned as data; they are not evaluated.
#' @inheritParams form_definition
#' @param id One exact `<sheet>.<field>` identifier.
#' @return One field definition list, with the same properties as an element of
#'   [form_fields()]. Optional properties remain absent when not declared.
#' @details Both an absent property and explicit null yield `NULL` with `$`.
#'   Use `"default" %in% names(field)` to distinguish these cases.
#' @section Errors:
#' Raises `model_form_error` for an invalid model, invalid `id` argument, or unknown
#' identifier. A missing field is an error, not a `NULL` result.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' form_field(m, "Observations.unit")$enum
#' comment <- form_field(m, "Metadata.comment")
#' "default" %in% names(comment) # TRUE: explicitly declared null.
#' comment$default
#' "default" %in% names(form_field(m, "Metadata.submission")) # FALSE: absent.
#' @seealso [form_fields()]
#' @export
form_field <- function(model, id) {
  text_arg(id, "id")
  fields <- form_fields(model)
  if (!id %in% names(fields)) abort_model("<model>", "fields", paste0("unknown field: ", id))
  fields[[id]]
}

#' Get workbook sheet definitions
#'
#' Return sheets in declaration order, named by their worksheet names.
#' No workbook is opened or inspected.
#' @inheritParams form_definition
#' @return A named list of sheet definitions with `fields` and `layout`.
#' @section Errors:
#' Raises `model_form_error` for an invalid model argument.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' sheets <- form_sheets(m)
#' names(sheets)
#' sheets$Observations$layout$kind
#' @seealso [form_fields()], [form_layout()]
#' @export
form_sheets <- function(model) form_definition(model)$sheets

#' Get a sheet's cell or table layout
#'
#' Look up a worksheet by name and return its declared layout.
#' @inheritParams form_definition
#' @param id One exact, case-sensitive worksheet name.
#' @return A layout list with `kind` (`"cells"` or `"table"`). Table layouts
#'   have `start_row`. Optional sheet extents remain
#'   absent unless declared. Coordinates belong to fields, not the layout.
#' @section Errors:
#' Raises `model_form_error` for an invalid model, invalid `id` argument,
#' or unknown worksheet name.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' form_field(m, "Metadata.submission")[c("row", "column")]
#' table <- form_layout(m, "Observations")
#' table$start_row
#' @seealso [form_sheets()]
#' @export
form_layout <- function(model, id) {
  text_arg(id, "id")
  sheets <- form_sheets(model)
  if (!id %in% names(sheets)) abort_model("<model>", "sheets", paste0("unknown sheet: ", id))
  sheets[[id]]$layout
}

#' Get external reference requirements
#'
#' Return names of reference datasets needed by downstream validation.
#' @inheritParams form_definition
#' @return A character vector of reference names, or `character(0)`.
#' @section Errors:
#' Raises `model_form_error` for an invalid model argument.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' refs <- form_references(m)
#' refs
#' form_field(m, "Observations.station")$reference
#' @seealso [form_field()], [form_metadata()]
#' @export
form_references <- function(model) unlist(form_definition(model)$references, use.names = FALSE)

#' Print a form model summary
#'
#' Display the form ID, form version, and schema version,
#' followed by field and sheet counts. The definition is not revalidated.
#' @param x A `form_model` returned by [read_form_model()] or [as_form_model()].
#' @param ... Additional arguments; currently ignored.
#' @return `x`, invisibly. The summary is written to standard output.
#' @section Errors:
#' Raises `model_form_error` for an invalid model argument. Expects an unmodified,
#' checked model; direct edits to its internals can cause other errors.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' print(m)
#' @seealso [form_definition()]
#' @export
print.form_model <- function(x, ...) {
  d <- form_definition(x)
  cat("<form_model> ", d$form$id, " @ ", d$form$version,
      " (schema ", d$schema_version, ")\n", sep = "")
  cat(length(form_fields(x)), "fields;", length(d$sheets), "sheets\n")
  invisible(x)
}

#' Print definition check results
#'
#' Display `valid` or `invalid`, followed by the diagnostics table when nonempty.
#' Diagnostics show the resource, path, and message without row numbers. Printing
#' uses the recorded result; it does not run the checks again.
#' @param x An unmodified `form_model_check` returned by [check_form_model()].
#' @param ... Additional arguments; currently ignored.
#' @return `x`, invisibly. The result is written to standard output.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' print(check_form_model(form_definition(m)))
#' d <- form_definition(m)
#' d$sheets[[1]]$fields[[1]]$type <- "unsupported"
#' print(check_form_model(d, resource = "edited definition"))
#' @seealso [check_form_model()]
#' @export
print.form_model_check <- function(x, ...) {
  cat("<form_model_check> ", if (x$valid) "valid" else "invalid", "\n", sep = "")
  if (nrow(x$diagnostics)) print(x$diagnostics, row.names = FALSE)
  invisible(x)
}

#' Decode a checked cell location
#' @param location Uppercase A1-style address already checked by the schema.
#' @return List with one-based numeric `row` and `column` coordinates.
#' @keywords internal
cell_coordinates <- function(location) {
  letters <- sub("[0-9]+$", "", location)
  list(row = as.numeric(sub("^[A-Z]+", "", location)),
       column = Reduce(function(value, digit) value * 26 + digit,
                       utf8ToInt(letters) - utf8ToInt("A") + 1, init = 0))
}
