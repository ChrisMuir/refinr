# Unload the DLL related to c++ files if the refinr package is unloaded.
.onUnload <- function (libpath) {
  library.dynam.unload("refinr", libpath)
}

# Set up pkg environment to house a list of acceptable input args for
# stringdist::stringdistmatrix. The ellipsis args for func "n_gram_merge()" get
# passed to stringdistmatrix half-way through completion, and it's preferable
# for n_gram_merge to fail immediately if any passed args are invalid. This
# also has the added benefit of not failing if stringdist updates their pkg
# and changes their func args.
refinr_env <- new.env()
sd_args <- names(formals(stringdist::stringdistmatrix))
sd_args <- sd_args[!sd_args %in% c("a", "b", "weight")]
assign("sd_args", sd_args, envir = refinr_env)
