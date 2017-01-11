missE <- lapply(nf, function(n)
			    substitute(missing(.), list(. = as.name(n))))