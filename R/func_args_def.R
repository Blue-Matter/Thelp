T.defaults <- function(fn, envir = globalenv(), overwrite = TRUE, verbose = TRUE) {
  fname <- if (is.character(fn)) fn else deparse(substitute(fn))
  f <- if (is.function(fn)) fn else match.fun(fn)
  fmls <- formals(args(f))  # args() also handles primitives
  if (is.null(fmls)) {
    message("No formal arguments for ", fname)
    return(invisible(character(0)))
  }

  # Arguments that have a default (drop '...' and ones with no default)
  has_def <- vapply(names(fmls), function(nm)
    nm != "..." && !identical(fmls[[nm]], quote(expr = )), logical(1))
  pending <- names(fmls)[has_def]

  # Evaluate in a scratch env whose parent is the function's enclosure, so
  # defaults see the function's namespace and each other (e.g. b = a * 2).
  # Repeated passes resolve defaults that refer to args later in the signature.
  eval_env <- new.env(parent = if (is.primitive(f)) baseenv() else environment(f))
  errs <- list()
  repeat {
    progress <- FALSE
    for (nm in pending) {
      res <- tryCatch(list(ok = TRUE, val = eval(fmls[[nm]], eval_env)),
                      error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      if (res$ok) {
        assign(nm, res$val, envir = eval_env)
        pending <- setdiff(pending, nm)
        errs[[nm]] <- NULL
        progress <- TRUE
      } else errs[[nm]] <- res$msg
    }
    if (!progress || !length(pending)) break
  }

  for (nm in names(errs))
    warning(sprintf("Could not evaluate default for '%s': %s", nm, errs[[nm]]), call. = FALSE)

  # Copy evaluated defaults to the target environment, in signature order
  assigned <- intersect(names(fmls), ls(eval_env, all.names = TRUE))
  if (!overwrite) assigned <- assigned[!vapply(assigned, exists, logical(1),
                                               envir = envir, inherits = FALSE)]
  for (nm in assigned) assign(nm, get(nm, envir = eval_env), envir = envir)

  if (verbose) message("Assigned from ", fname, "(): ", paste(assigned, collapse = ", "))
  invisible(assigned)
}
