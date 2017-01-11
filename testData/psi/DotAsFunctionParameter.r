not.Ident <- function(ch, TRAFO=identity, ...)
		    vapply(ch, function(.)
                           !identical(TRAFO(get(., i)),
                                      TRAFO(get(., lib.pos)), ...),
                           NA)