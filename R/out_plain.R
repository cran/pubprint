#############################################################################
# out_plain.R
#############################################################################

out.plain.init <- function()
{
    return(list(specialchar = out.default.specialchar,
                math = out.default.math,
                operator = out.default.operator,
                number = out.default.number,
                identifier = out.default.identifier,
                term = out.default.term,
                concat = out.default.concat,
                subscript = out.default.subscript,
                superscript = out.default.superscript,
                bracket = out.default.bracket,
                above = out.default.above,
                below = out.default.below))
}
