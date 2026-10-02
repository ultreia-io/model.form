#' Inspect the installed model-definition schema
#'
#' The installed YAML file is the single editable structural contract. The small
#' `model.form/schema/1` dialect is documented in the model-authoring vignette;
#' it is not a general JSON Schema implementation.
#' @param version Exact schema contract version; currently `"1.0.0"`.
#' @return An ordinary nested list, independently parsed on each call.
#' @section Errors:
#' Raises `model_form_error` for unsupported versions or unavailable schema files.
#' @examples
#' form_schema()$contract_version
#' @export
form_schema <- function(version = "1.0.0") {
  text_arg(version, "version")
  if (!identical(version, "1.0.0")) abort_model("<schema>", "version", "unsupported schema version")
  read_yaml_data(form_resource("schema/model-1.0.0.yaml", "model.form"))
}

#' Check for ordinary value attributes
#'
#' Test attributes only; this does not check the value's type or contents.
#' @param x Any R object.
#' @return `TRUE` when `x` has no attributes other than optional names.
#' @keywords internal
plain <- function(x) all(names(attributes(x)) %in% "names")
#' Check a scalar against a schema type
#'
#' Require one nonmissing atomic value with no attributes except optional names.
#' Numeric values must be finite; integer values may use integer or double storage.
#' No coercion is performed.
#' @param x Value to check.
#' @param type One of `string`, `boolean`, `number`, `integer`, or `scalar`.
#'   The `scalar` type accepts strings, booleans, and finite numbers.
#' @return One logical value; `FALSE` for an incompatible value or unknown type.
#' @keywords internal
scalar_type <- function(x, type) {
  if (!is.atomic(x) || !plain(x) || length(x) != 1L || is.na(x)) return(FALSE)
  switch(type,
    string = is.character(x),
    boolean = is.logical(x),
    number = is.numeric(x) && is.finite(x),
    integer = is.numeric(x) && is.finite(x) && x == floor(x),
    scalar = is.character(x) || is.logical(x) || (is.numeric(x) && is.finite(x)),
    FALSE)
}

#' Collect structural definition diagnostics
#'
#' Recursively check mappings, sequences, and scalars against the package schema.
#' Resolve schema references and nullable nodes before checking types, required
#' properties, allowed values, and size or pattern constraints. A type mismatch
#' stops traversal of that node; other nodes can still produce diagnostics.
#' @param x Definition value at the current node.
#' @param node Schema node describing `x`.
#' @param definitions Named schema definitions used to resolve `ref` entries.
#'   The schema is package-owned and must contain valid, acyclic references.
#' @param path Diagnostic location, starting at `"$"` for the definition root.
#' @param add Callback accepting `(path, message)` to append a diagnostic.
#'   Its return value is ignored.
#' @return Invisibly returns `NULL`; findings are passed to `add`.
#' @keywords internal
schema_check <- function(x, node, definitions, path, add) {
  while (!is.null(node$ref)) node <- definitions[[node$ref]]
  if (is.null(x) && isTRUE(node$nullable)) return(invisible(NULL))
  if (!is.null(node$shorthand) && !is.list(x)) {
    return(schema_check(x, node$shorthand, definitions, path, add))
  }
  type <- node$type
  valid <- switch(type,
    mapping = is.list(x) && plain(x) && !is.null(names(x)),
    sequence = is.list(x) && plain(x) && is.null(names(x)),
    scalar_type(x, type))
  if (!valid) {
    add(path, paste0("expected ", type))
    return(invisible(NULL))
  }
  if (type == "mapping") {
    keys <- names(x)
    for (key in unique(keys[duplicated(keys)])) add(paste0(path, ".", key), "duplicate property")
    if (!is.null(node$values)) {
      if (!is.null(node$min_items) && length(x) < node$min_items) {
        add(path, paste0("requires at least ", node$min_items, " items"))
      }
      for (key in keys) {
        if (!is.null(node$key_pattern) && !grepl(node$key_pattern, key)) {
          add(paste0(path, ".", key), "key does not match the required pattern")
        }
        schema_check(x[[key]], node$values, definitions, paste0(path, ".", key), add)
      }
    } else {
      for (key in setdiff(keys, names(node$properties))) add(paste0(path, ".", key), "unsupported property")
      for (key in setdiff(unlist(node$required), keys)) add(paste0(path, ".", key), "missing required property")
      for (key in intersect(keys, names(node$properties))) {
        schema_check(x[[key]], node$properties[[key]], definitions, paste0(path, ".", key), add)
      }
    }
  } else if (type == "sequence") {
    if (length(x) < node$min_items) add(path, paste0("requires at least ", node$min_items, " items"))
    for (i in seq_along(x)) schema_check(x[[i]], node$items, definitions, paste0(path, "[", i, "]"), add)
  } else {
    if (!is.null(node$enum) && !x %in% unlist(node$enum)) add(path, "unsupported value")
    if (!is.null(node$minimum) && x < node$minimum) add(path, paste0("must be >= ", node$minimum))
    if (!is.null(node$min_length) && nchar(x) < node$min_length) add(path, "must not be empty")
    if (!is.null(node$pattern) && !grepl(node$pattern, x)) add(path, "does not match the required pattern")
  }
  invisible(NULL)
}

#' Check a declared value against its field type
#'
#' Used for model defaults and enum entries. Bounds, patterns, and enum membership
#' are checked separately; this helper does not validate submitted form data.
#' @param x Declared value to check. `NULL` is accepted for nonmandatory fields.
#' @param field Structurally valid field definition with `type` and optional
#'   `mandatory`.
#' @details Dates require `YYYY-MM-DD`; datetimes require `YYYY-MM-DDTHH:MM:SSZ`.
#'   Both must parse and format back to the identical string, using UTC for
#'   datetimes. Other types use [scalar_type()].
#' @return One logical value indicating type and mandatory-value compatibility.
#' @keywords internal
field_value <- function(x, field) {
  if (is.null(x)) return(!isTRUE(field$mandatory))
  if (field$type %in% c("date", "datetime")) {
    if (!scalar_type(x, "string")) return(FALSE)
    pattern <- if (field$type == "date") "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" else
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
    if (!grepl(pattern, x)) return(FALSE)
    parsed <- if (field$type == "date") as.Date(x, format = "%Y-%m-%d") else
      as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    if (is.na(parsed)) return(FALSE)
    fmt <- if (field$type == "date") "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%SZ"
    return(identical(format(parsed, fmt, tz = "UTC"), x))
  }
  scalar_type(x, field$type)
}

#' Collect model consistency diagnostics
#'
#' Check identifier uniqueness, references, field constraints, declared defaults
#' and enums, workbook coordinates, and same-sheet conditional dependencies.
#' This checks model definitions only, never submitted data or external references.
#' @param d Definition that has already passed structural schema checking.
#' @param add Callback accepting `(path, message)` to append a diagnostic.
#'   Its return value is ignored.
#' @return Invisibly returns `NULL`; findings are passed to `add`.
#' @seealso [check_form_model()]
#' @keywords internal
semantic_check <- function(d, add) {
  references <- unlist(d$references, use.names = FALSE)
  for (i in which(duplicated(references))) {
    add(paste0("$.references[", i, "]"), paste0("duplicate reference: ", references[[i]]))
  }
  sheet_names <- names(d$sheets)
  if (anyDuplicated(tolower(sheet_names))) add("$.sheets", "duplicate workbook sheet names (case insensitive)")
  for (sheet_index in seq_along(d$sheets)) {
    sheet_name <- sheet_names[[sheet_index]]
    sheet <- d$sheets[[sheet_index]]
    layout <- sheet$layout
    table <- layout$kind == "table"
    layout_path <- paste0("$.sheets.", sheet_name, ".layout")
    if (table && is.null(layout$start_row)) add(paste0(layout_path, ".start_row"), "table requires start_row")
    if (table && is.null(layout$column_count)) {
      add(paste0(layout_path, ".column_count"), "table requires column_count")
    }
    if (!table && is.null(layout$column_count)) {
      add(paste0(layout_path, ".column_count"), "cell layout requires column_count")
    }
    if (!table && "start_row" %in% names(layout)) {
      add(layout_path, "cell layout cannot have table row properties")
    }
    if (!is.null(layout$row_count) && !is.null(layout$start_row) && layout$start_row > layout$row_count + 1) {
      add(paste0(layout_path, ".start_row"), "start row exceeds sheet extent")
    }
    if (table && !is.null(layout$column_count) && layout$column_count != length(sheet$fields)) {
      add(paste0(layout_path, ".column_count"), "column count differs from field count")
    }
    locations <- character()
    field_names <- names(sheet$fields)
    for (field_index in seq_along(sheet$fields)) {
      field_name <- field_names[[field_index]]
      field <- sheet$fields[[field_index]]
      field_path <- paste0("$.sheets.", sheet_name, ".fields.", field_name)
      constraints <- field
      if (!is.null(constraints$minimum) || !is.null(constraints$maximum)) {
        if (!field$type %in% c("integer", "number")) {
          add(field_path, "numeric bounds require a numeric field")
        }
        if (!is.null(constraints$minimum) && !is.null(constraints$maximum) &&
            constraints$minimum > constraints$maximum) {
          add(paste0(field_path, ".minimum"), "minimum exceeds maximum")
        }
        if (field$type == "integer" && any(unlist(constraints[c("minimum", "maximum")]) %% 1 != 0)) {
          add(field_path, "integer bounds must be whole numbers")
        }
      }
      pattern_ok <- TRUE
      if (!is.null(constraints$pattern)) {
        if (!field$type %in% c("string", "date", "datetime")) {
          add(paste0(field_path, ".pattern"), "pattern requires a text or temporal field")
        }
        pattern_ok <- tryCatch({
          withCallingHandlers(grepl(constraints$pattern, "", perl = TRUE),
                              warning = function(w) stop(conditionMessage(w)))
          TRUE
        }, error = function(e) FALSE)
        if (!pattern_ok) add(paste0(field_path, ".pattern"), "invalid PCRE regular expression")
      }
      if (!is.null(constraints$reference)) {
        reference <- constraints$reference
        reference_path <- paste0(field_path, ".reference")
        if (is.list(reference)) {
          reference <- reference$id
          reference_path <- paste0(reference_path, ".id")
        }
        if (!reference %in% references) {
          add(reference_path, paste0("unknown reference: ", reference))
        }
        if (field$type != "string") {
          add(reference_path, "reference constraints require a string field")
        }
      }
      dependency <- field$required_field
      if (!is.null(dependency)) {
        if (!dependency %in% field_names) {
          add(paste0(field_path, ".required_field"), paste0("unknown field: ", dependency))
        }
        if (dependency == field_name) {
          add(paste0(field_path, ".required_field"), "field cannot condition its own mandatory value")
        }
      }
      values <- constraints$enum
      labels <- if (length(values)) paste0(field_path, ".enum[", seq_along(values), "]") else character()
      if ("default" %in% names(field)) {
        values <- c(values, list(field$default))
        labels <- c(labels, paste0(field_path, ".default"))
      }
      for (j in seq_along(values)) {
        value <- values[[j]]
        if (!field_value(value, field)) {
          add(labels[[j]], paste0("value incompatible with ", field$type, " / mandatory"))
          next
        }
        if (is.null(value)) next
        if (is.numeric(value)) {
          if (!is.null(constraints$minimum) && value < constraints$minimum) add(labels[[j]], "value below minimum")
          if (!is.null(constraints$maximum) && value > constraints$maximum) add(labels[[j]], "value above maximum")
        }
        if (is.character(value) && !is.null(constraints$pattern) && pattern_ok &&
            !grepl(constraints$pattern, value, perl = TRUE)) {
          add(labels[[j]], "value does not match pattern")
        }
      }
      equal <- function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
      if (!is.null(constraints$enum)) {
        for (j in seq_along(constraints$enum)) {
          if (j > 1L && any(vapply(constraints$enum[seq_len(j - 1L)], equal,
                                  logical(1), y = constraints$enum[[j]]))) {
            add(paste0(field_path, ".enum[", j, "]"), "duplicate enum value")
          }
        }
        if ("default" %in% names(field) &&
            !any(vapply(constraints$enum, equal, logical(1), y = field$default))) {
          add(paste0(field_path, ".default"), "default is not in enum")
        }
      }
      if (table && !is.null(field$location)) {
        add(paste0(field_path, ".location"), "table fields use their declared order, not fixed locations")
      }
      if (!table && is.null(field$location)) add(paste0(field_path, ".location"), "cell field requires a location")
      if (!table && !is.null(field$location)) {
        cell <- cell_coordinates(field$location)
        if (!is.null(layout$column_count) && cell$column > layout$column_count) add(paste0(field_path, ".location"), "column exceeds sheet extent")
        if (!is.null(layout$row_count) && cell$row > layout$row_count) add(paste0(field_path, ".location"), "row exceeds sheet extent")
        if (field$location %in% locations) add(paste0(field_path, ".location"), "duplicate workbook location")
        locations <- c(locations, field$location)
      }
    }
  }
}

#' Check a model definition
#'
#' Check an ordinary definition list against the installed YAML schema, then
#' check identifiers, references, constraints and layout consistency. This does
#' not execute submitted-data validation. Semantic checks run only if structure
#' is valid, preventing misleading follow-on errors.
#' @param definition Ordinary named list with the structure described by
#'   [form_schema()]. YAML sequences are unnamed lists, including singleton lists.
#' @param resource Diagnostic source label, default `"<list>"`.
#' @return A `form_model_check` list with logical `valid` and `diagnostics`, a
#'   data frame with character columns `resource`, `path`, `message` (zero rows
#'   on success). Missing and explicit `NULL` properties are distinct.
#' @section Errors:
#' Invalid definitions return diagnostics rather than throwing. Invalid API
#' arguments or a missing installed schema raise `model_form_error`.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' check_form_model(form_definition(m))
#' @export
check_form_model <- function(definition, resource = "<list>") {
  text_arg(resource, "resource")
  diagnostics <- data.frame(resource = character(), path = character(), message = character(), stringsAsFactors = FALSE)
  # Append one finding with the source label captured by this check.
  add <- function(path, message) {
    diagnostics[nrow(diagnostics) + 1L, ] <<- list(resource, path, message)
  }
  schema <- form_schema()
  schema_check(definition, schema$definitions[[schema$root]], schema$definitions, "$", add)
  if (!nrow(diagnostics)) semantic_check(definition, add)
  structure(list(valid = nrow(diagnostics) == 0L, diagnostics = diagnostics), class = "form_model_check")
}

#' Construct a checked value-like model
#'
#' @inheritParams check_form_model
#' @return The checked definition as a `form_model` list. It contains only the
#'   declared model properties. Sheet and field mapping keys remain list names.
#'   The diagnostic `resource` label is not retained. Reconstruct after edits to
#'   recheck consistency.
#' @section Errors:
#' Raises `model_form_error` on invalid definitions with all diagnostics attached.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' as_form_model(form_definition(m))
#' @export
as_form_model <- function(definition, resource = "<list>") {
  result <- check_form_model(definition, resource)
  if (!result$valid) {
    first <- result$diagnostics[1L, ]
    abort_model(resource, first$path, first$message, result$diagnostics)
  }
  structure(definition, class = "form_model")
}
