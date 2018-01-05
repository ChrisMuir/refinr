# Unload the DLL related to c++ files if the refinr package is unloaded.
.onUnload <- function (libpath) {
  library.dynam.unload("refinr", libpath)
}

# Set up pkg environment to house a list of acceptable input args for exported
# function "n_gram_merge". Doing this because the ellipsis args get passed to
# func stringdist::stringdistmatrix half-way through completion of
# n_gram_merge, and it's preferable for n_gram_merge to fail immediately if
# any args are passed that do not fit for either n_gram_merge nor
# stringdistmatrix. This also has te added benefit of not failing if stringdist
# updates their pkg and changes their func args.
refinr_env <- new.env()
sd_args <- names(formals(stringdist::stringdistmatrix))
sd_args <- sd_args[!sd_args %in% c("a", "b", "weight")]
assign("sd_args", sd_args, envir = refinr_env)
