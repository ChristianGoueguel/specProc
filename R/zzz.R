.onLoad <- function(libname, pkgname) {
  # The "rsimpls" engine of parsnip::pls(). parsnip is suggested, so the
  # engine is registered now if parsnip is loaded, and otherwise when it is.
  # A failure must not stop parsnip from loading.
  register <- function(...) {
    tryCatch(register_pls_rsimpls(), error = function(e) {
      warning("specProc could not register the \"rsimpls\" engine of parsnip::pls(): ",
              conditionMessage(e), call. = FALSE)
    })
  }
  if (isNamespaceLoaded("parsnip")) register()
  setHook(packageEvent("parsnip", "onLoad"), register)
}
