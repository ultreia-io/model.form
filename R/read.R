#' Check the restricted YAML syntax before parsing
#'
#' Scan YAML source for constructs excluded from model resources before libyaml
#' can resolve aliases, merge mappings, or discard explicit tags.
#' @param text One character string containing the YAML source, with lines
#'   separated by newline characters. Supplied by [read_yaml_data()].
#' @param resource Source path or diagnostic label used in error conditions.
#' @details
#' The scanner masks quoted strings and comments and skips indented literal or
#' folded block scalar content. It tracks quotes across lines, including escaped
#' double quotes and doubled single quotes, so punctuation in prose remains data.
#'
#' Outside those regions, it rejects tags, anchors, aliases, merge keys,
#' directives, and document markers. This is a lexical guard, not a YAML parser:
#' passing it does not establish valid YAML syntax or a valid model definition.
#' Parsing and subsequent definition checks are performed by the caller.
#' @return Invisibly returns `NULL` when no excluded construct is found.
#'   The source text is not modified.
#' @section Errors:
#' Raises `model_form_error` at the first detected excluded construct. The
#' condition records `resource` and a `path` of the form `"$ (line N)"`, where
#' `N` is the one-based source line number.
#' @seealso [read_yaml_data()], [read_form_model()]
#' @keywords internal
yaml_profile <- function(text, resource) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  quote <- ""
  block <- NULL
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    indent <- nchar(line) - nchar(sub("^ *", "", line))
    if (!is.null(block)) {
      if (!nzchar(trimws(line)) || indent > block) next
      block <- NULL
    }
    chars <- strsplit(line, "", fixed = TRUE)[[1L]]
    visible <- ""
    j <- 1L
    while (j <= length(chars)) {
      ch <- chars[[j]]
      if (nzchar(quote)) {
        if (quote == '"' && ch == "\\") {
          j <- j + 2L
          next
        }
        if (ch == quote) {
          if (quote == "'" && j < length(chars) && chars[[j + 1L]] == "'") {
            j <- j + 2L
            next
          }
          quote <- ""
        }
        visible <- paste0(visible, " ")
      } else if (ch %in% c("'", '"') &&
                 (j == 1L || grepl("[[:space:]:,\\[{?-]$", visible))) {
        quote <- ch
        visible <- paste0(visible, "Q")
      } else {
        if (ch == "#" && (j == 1L || grepl("[[:space:]]$", visible))) break
        visible <- paste0(visible, ch)
      }
      j <- j + 1L
    }
    if (grepl("(^[[:space:]]*([-?][[:space:]]+)?|:[[:space:]]+|[\\[{,][[:space:]]*)[!&*]|(^|[[:space:],{])<<[[:space:]]*:|^%|^---|^\\.\\.\\.", visible)) {
      abort_model(resource, paste0("$ (line ", i, ")"),
                  "YAML tags, anchors, aliases, merges, directives and document markers are unsupported")
    }
    if (grepl("[|>][-+0-9]*[[:space:]]*$", visible)) block <- indent
  }
}

#' Signal a model error with source context
#'
#' Construct and signal the error condition shared by resource loading and
#' model checking. The condition retains structured context for callers.
#' @param resource Source path or diagnostic label, such as `"<argument>"`.
#' @param path Location within the resource, source line, or argument name.
#' @param message One character string describing the failure.
#' @param diagnostics Optional diagnostics data frame from definition checking;
#'   otherwise `NULL`.
#' @return Does not return; always signals an error.
#' @section Condition:
#' The condition inherits from `model_form_error`, `error`, and `condition`.
#' Its `message` joins `resource`, `path`, and the supplied message with `": "`.
#' It also contains `resource`, `path`, and `diagnostics` fields and a `NULL`
#' `call`, so the printed error does not include an internal function call.
#' @seealso [check_form_model()], [read_form_model()]
#' @keywords internal
abort_model <- function(resource, path, message, diagnostics = NULL) {
  stop(structure(list(message = paste0(resource, ": ", path, ": ", message),
                      call = NULL, resource = resource, path = path,
                      diagnostics = diagnostics),
                 class = c("model_form_error", "error", "condition")))
}

#' Require one nonempty string argument
#'
#' Check a shared argument shape without trimming, coercing, or changing it.
#' Whitespace-only strings satisfy this check; interpretation belongs to callers.
#' @param x Value to check.
#' @param name Argument name used as the diagnostic path.
#' @return Invisibly returns `NULL` when `x` is a length-one character vector
#'   that is neither `NA` nor the empty string.
#' @section Errors:
#' Raises `model_form_error` with resource `"<argument>"` and path `name`
#' when the value does not satisfy the argument shape.
#' @seealso [abort_model()]
#' @keywords internal
text_arg <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    abort_model("<argument>", name, "expected one nonempty string")
  }
}

#' Read a YAML resource as data
#'
#' Read an explicit UTF-8 file, enforce the restricted YAML syntax, and parse it
#' without evaluating expressions. Used for both models and the installed schema.
#' @param path One nonempty character string naming an existing file.
#' @details
#' [yaml_profile()] checks excluded constructs before parsing. The parser uses
#' YAML 1.1 scalar typing with `eval.expr = FALSE`. A sequence handler preserves
#' sequences as lists. A mapping handler checks that keys are strings before
#' converting mappings to named lists, then removes the parser's key attribute.
#' Duplicate mapping keys are rejected by the YAML parser.
#'
#' This helper does not apply the model schema or semantic checks. Call
#' [read_form_model()] to load and validate a complete model definition.
#' @return The parsed R value. Mappings are named lists, sequences are unnamed
#'   lists, and scalars use the YAML parser's types, including `NULL` for null.
#' @section Errors:
#' Raises `model_form_error` for an invalid path, a missing file, a directory,
#' excluded YAML syntax, parsing failures, or non-string mapping keys. Warnings
#' during parsing are treated as errors. Existing `model_form_error` conditions
#' retain their context; other reading or parsing errors are wrapped with
#' `resource = path` and diagnostic path `"$"`.
#' @seealso [yaml_profile()], [read_form_model()], [form_schema()]
#' @keywords internal
read_yaml_data <- function(path) {
  text_arg(path, "path")
  if (!file.exists(path) || dir.exists(path)) abort_model(path, "$", "file does not exist or is not a regular file")
  tryCatch({
    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    yaml_profile(text, path)
    # Keep sequences as lists and check map key types before string conversion.
    handler_error <- NULL
    handlers <- list(seq = identity, map = function(x) {
      keys <- attr(x, "keys")
      if (!all(vapply(keys, function(k) is.character(k) && length(k) == 1L && !is.na(k), logical(1)))) {
        handler_error <<- "mapping keys must be strings; quote numeric or boolean codes"
        return(x)
      }
      attr(x, "keys") <- NULL
      names(x) <- vapply(keys, identity, character(1))
      x
    })
    result <- withCallingHandlers(yaml::yaml.load(text, as.named.list = FALSE,
      handlers = handlers, eval.expr = FALSE), warning = function(w) stop(conditionMessage(w)))
    if (!is.null(handler_error)) abort_model(path, "$", handler_error)
    result
  }, error = function(e) {
    if (inherits(e, "model_form_error")) stop(e)
    abort_model(path, "$", conditionMessage(e))
  })
}

#' Locate an installed model resource
#'
#' Resolve a file relative to an explicitly named package's installed directory.
#' Absolute paths, parent traversal, and symlinks escaping the package are rejected.
#' @param path Relative resource path, for example `examples/survey-form.yaml`.
#' @param package Installed package name. No working-directory search is performed.
#' @return One absolute existing file path.
#' @section Errors:
#' Raises `model_form_error` when arguments, package, or resource are invalid.
#' @examples
#' form_resource("examples/survey-form.yaml", "model.form")
#' @export
form_resource <- function(path, package) {
  text_arg(path, "path")
  text_arg(package, "package")
  if (grepl("^[/\\\\~]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$)", path)) {
    abort_model(package, path, "expected a relative path without parent traversal")
  }
  root <- system.file(package = package)
  if (!nzchar(root)) abort_model(package, path, "package is not installed")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  file <- file.path(root, path)
  if (!file.exists(file) || dir.exists(file)) abort_model(package, path, "resource file does not exist")
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  if (!startsWith(file, paste0(root, "/"))) abort_model(package, path, "resource escapes package directory")
  file
}

#' Load a model definition from YAML
#'
#' Parse a single UTF-8 YAML file, check its structure and semantics, and return
#' a value-like model. Sequences retain order; no defaults or coercions are applied.
#' @param path Explicit file path, or relative installed resource path when
#'   `package` is supplied.
#' @param package Optional installed resource package name; otherwise `NULL`.
#' @return The checked definition as a `form_model` S3 list. The source path is
#'   used for loading diagnostics and is not retained in the returned model.
#' @section Errors:
#' Raises `model_form_error` for malformed YAML or invalid definitions. Conditions
#' include `resource`, `path`, and, for definition errors, a diagnostics data frame.
#' Parsing uses YAML 1.1 typing: quote versions, identifiers and codes. Only
#' `true`/`false` should be used for booleans; legacy YAML boolean spellings parse
#' as booleans too. Tags, aliases, anchors, merges and multi-document syntax are rejected.
#' @examples
#' m <- read_form_model("examples/survey-form.yaml", "model.form")
#' form_metadata(m)
#' @export
read_form_model <- function(path, package = NULL) {
  if (!is.null(package)) path <- form_resource(path, package)
  definition <- read_yaml_data(path)
  as_form_model(definition, normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Load a model using its conventional filename
#'
#' Resolve exactly `<name>-form.yaml` within the supplied directory, then load
#' and validate it with [read_form_model()]. No directory scanning or version
#' guessing occurs. Put different versions in explicitly chosen directories.
#' @param name Filename prefix, without `-form.yaml`. Starts with a letter and
#'   contains only letters, digits, underscores, dots, or hyphens. It identifies
#'   the file family; the form's own ID and version remain in its YAML content.
#' @param directory Explicit directory containing the model file. For installed
#'   resources, use `system.file("forms", package = "your.package")`.
#' @return A validated `form_model`, as returned by [read_form_model()].
#' @section Errors:
#' Raises `model_form_error` for invalid names, missing files, or invalid models.
#' A name cannot contain a path separator or traverse to another directory.
#' @examples
#' directory <- system.file("examples", package = "model.form")
#' model <- load_form_model("survey", directory)
#' form_metadata(model)$form$id
#' @export
load_form_model <- function(name, directory) {
  text_arg(name, "name")
  text_arg(directory, "directory")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", name)) {
    abort_model("<argument>", "name", "expected a model name without a path or file suffix")
  }
  read_form_model(file.path(directory, paste0(name, "-form.yaml")))
}

#' Select exactly one model resource
#'
#' Each supplied file is loaded and checked. Invalid unrelated files fail too;
#' directories are never scanned and there is no implicit latest-version fallback.
#' @param paths Nonempty character vector of explicit YAML file paths.
#' @param id Exact form identifier.
#' @param version Exact described form version string.
#' @param package Optional installed package containing all relative `paths`.
#' @return The unique matching `form_model`.
#' @section Errors:
#' Raises `model_form_error` for invalid resources, missing matches or ambiguity,
#' including when the same resource is supplied twice.
#' @examples
#' select_form_model("examples/survey-form.yaml", "survey", "2026.1",
#'                   package = "model.form")
#' @export
select_form_model <- function(paths, id, version, package = NULL) {
  if (!is.character(paths) || !length(paths) || anyNA(paths) || any(!nzchar(paths))) {
    abort_model("<selection>", "paths", "expected explicit resource paths")
  }
  text_arg(id, "id")
  text_arg(version, "version")
  models <- lapply(paths, read_form_model, package = package)
  matches <- vapply(models, function(m) {
    d <- form_definition(m)
    identical(d$form$id, id) && identical(d$form$version, version)
  }, logical(1))
  if (sum(matches) != 1L) {
    abort_model(paste(paths, collapse = ", "), "$",
                paste0("exact selection matched ", sum(matches), " resources for ",
                       id, " / ", version))
  }
  models[[which(matches)]]
}
