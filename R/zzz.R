# Unload the DLL related to c++ files if the refinr package is unloaded.
.onUnload <- function (libpath) {
  library.dynam.unload("refinr", libpath)
}
