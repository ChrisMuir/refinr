# Package uses the pipe (%>%) from dplyr, this line of code keeps R CMD check
# happy. See https://github.com/hadley/dplyr/issues/789.
if(getRversion() >= "2.15.1")  utils::globalVariables(".")

# Unload the DLL related to c++ files if the refinr package is unloaded.
.onUnload <- function (libpath) {
  library.dynam.unload("refinr", libpath)
}
