
.onLoad <- function(libname, pkgname) {
  register_backend("plain")
  register_backend("fancy")
}

.onUnload <- function(libpath) {
  deregister_backend("plain")
  deregister_backend("fancy")
}
