.onLoad <- function(libname, pkgname) {
  # The parsnip engines of specProc. parsnip is suggested, so they are
  # registered now if parsnip is loaded, and otherwise when it is. A failure
  # must not stop parsnip from loading.
  register <- function(...) {
    engines <- list(rsimpls = register_pls_rsimpls, lts = register_linear_reg_lts,
                    mcd = register_discrim_mcd)
    for (eng in names(engines)) {
      tryCatch(engines[[eng]](), error = function(e) {
        warning("specProc could not register the \"", eng, "\" parsnip engine: ",
                conditionMessage(e), call. = FALSE)
      })
    }
  }
  if (isNamespaceLoaded("parsnip")) register()
  setHook(packageEvent("parsnip", "onLoad"), register)
}
