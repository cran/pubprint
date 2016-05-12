#############################################################################
# zzz.R
#############################################################################

#' @include defaults.R
NULL

.onLoad <- function(lib, pkg)
{
    if (requireNamespace("knitr", quietly = TRUE))
        # is pubprint running in knit function?
        if (0 < length(grep("^knit", sys.calls())))
            eval(pp_opts_out$set(pp_init_out(knitr::opts_knit$get("out.format"))))

}
