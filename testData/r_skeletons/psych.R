##
## Exported symobls in package `psych`
##

## Exported package methods

vgQ.bimin <- function (L) 
{
    L2 <- L[, -1]
    lvg <- vgbQ.bimin(L2)
    v <- lvg$f
    G <- lvg$Gq
    G <- cbind(G[, 1], G)
    G[, 1] <- 0
    return(list(f = v, Gq = G))
}


cortest.jennrich <- function (R1, R2, n1 = NULL, n2 = NULL) 
{
    p <- dim(R1)[2]
    if (dim(R1)[1] != p) {
        n1 <- dim(R1)[1]
        R1 <- cor(R1, use = "pairwise")
        warning("R1 matrix was not square, correlations found")
    }
    if (dim(R2)[1] != dim(R2)[2]) {
        n2 <- dim(R2)[1]
        R2 <- cor(R2, use = "pairwise")
        warning("R2 matrix was not square, correlations found")
    }
    if (!is.matrix(R1)) 
        R1 <- as.matrix(R1)
    if (!is.matrix(R2)) 
        R2 <- as.matrix(R2)
    if (dim(R1)[2] != dim(R2)[2]) 
        stop("correlation matrices M and S must be of the same size!")
    if (is.null(n2)) 
        n2 <- n1
    if (!is.null(n1) & !is.null(n2)) 
        c <- n1 * n2/(n1 + n2)
    else c <- 1
    R <- (n1 * R1 + n2 * R2)/(n1 + n2)
    S <- R * R
    S.inv <- solve(S)
    R.inv <- solve(R)
    R.diff <- R1 - R2
    Z <- sqrt(c) * R.inv %*% R.diff
    chi2 <- tr(Z %*% t(Z))/2 - t(diag(Z)) %*% S.inv %*% diag(Z)
    chi2 <- chi2[1, 1]
    p <- dim(R1)[1]
    df <- p * (p - 1)/2
    results <- list(chi2 = chi2, prob = pchisq(chi2, df, lower.tail = FALSE))
    return(results)
}


omega2latex <- function (f, digits = 2, rowlabels = TRUE, apa = TRUE, short.names = FALSE, 
    cumvar = FALSE, cut = 0.2, font.size = "scriptsize", heading = "An omega analysis table from the psych package in R", 
    caption = "omega2latex", label = "default", silent = FALSE, 
    file = NULL, append = FALSE) 
{
    if (class(f)[2] == "omega") 
        f$loadings <- f$schmid$sl
    x <- unclass(f$loadings)
    nfactors <- ncol(x)
    h2 <- rowSums(x^2)
    u2 <- 1 - h2
    vtotal <- sum(h2 + u2)
    nvar <- dim(x)[2]
    comment <- paste("% Called in the psych package ", match.call())
    header <- paste("\\begin{", font.size, "} \\begin{table}[htpb]", 
        "\\caption{", caption, " with cut = ", cut, "\n $\\omega_h  = ", 
        round(f$omega_h, digits), "\\;\\;\\;\\alpha (\\lambda_3) = ", 
        round(f$alpha, digits), "\\;\\;\\;\\lambda_6^* = ", round(f$G6, 
            digits), "\\;\\;\\; \\omega_t = ", round(f$omega.tot, 
            digits), "$ }\n\\begin{center}\n\\begin{tabular}", 
        sep = "")
    header <- c(header, "{l", rep("r", nvar), "}\n")
    if (apa) 
        header <- c(header, "\\multicolumn{", nvar, "}{l}{", 
            heading, "}", "\\cr \n \\hline ")
    if (apa) {
        footer <- paste(" \\hline ")
    }
    footer <- paste(footer, "\n\\end{tabular}\n\\end{center}\n\\label{", 
        label, "}\n\\end{table} \n\\end{", font.size, "}\n", 
        sep = "")
    x[abs(x) < cut] <- NA
    x <- round(x, digits = digits)
    cname <- colnames(x)
    if (short.names) 
        cname <- 1:nvar
    names1 <- paste(cname[1:(nvar - 1)], " & ")
    lastname <- paste(cname[nvar], "\\cr \n")
    if (apa) {
        allnames <- c("Variable  &  ", names1, lastname, " \\hline \n")
    }
    else {
        allnames <- c("  &  ", names1, lastname, "\\cr \n")
    }
    x <- format(x, drop0trailing = FALSE)
    value <- apply(x, 1, paste, collapse = "  &  ")
    value <- gsub("NA", "  ", value, fixed = TRUE)
    if (rowlabels) 
        value <- {
            paste(sanitize.latex(names(value)), "  & ", value)
        }
    else {
        paste("  &  ", value)
    }
    values <- paste(value, "\\cr", "\n")
    x <- f$loadings
    nvar <- nrow(x)
    vx <- colSums(x^2)[1:(ncol(x) - 3)]
    vx <- round(vx, digits)
    loads <- c("\\hline \\cr SS loadings &", paste(vx, " & ", 
        sep = ""), "\\cr  \n")
    if (!silent) {
        cat(comment, "\n")
        cat(header)
        cat(allnames)
        cat(values)
        cat(loads)
        cat(footer)
    }
    result <- c(header, allnames, values, loads, footer)
    if (!is.null(file)) 
        write.table(result, file = file, row.names = FALSE, col.names = FALSE, 
            quote = FALSE, append = append)
    invisible(result)
}


topBottom <- function (x, hlength = 4, tlength = 4, digits = 2) 
{
    if (is.data.frame(x) | is.matrix(x)) {
        if (is.matrix(x)) 
            x <- data.frame(unclass(x))
        nvar <- dim(x)[2]
        ellipsis <- rep("...", nvar)
        h <- data.frame(head(x, hlength))
        t <- data.frame(tail(x, tlength))
        for (i in 1:nvar) {
            if (is.numeric(h[1, i])) {
                h[i] <- round(h[i], digits)
                t[i] <- round(t[i], digits)
            }
            else {
                ellipsis[i] <- NA
            }
        }
        head.tail <- rbind(h, t)
        head.tail <- as.matrix(head.tail)
    }
    else {
        h <- head(x, hlength)
        t <- tail(x, tlength)
        head.tail <- as.matrix(rbind(h, t))
    }
    return(head.tail)
}


fa.multi.diagram <- function (multi.results, sort = TRUE, labels = NULL, flabels = NULL, 
    cut = 0.2, gcut = 0.2, simple = TRUE, errors = FALSE, digits = 1, 
    e.size = 0.1, rsize = 0.15, side = 3, main = NULL, cex = NULL, 
    color.lines = TRUE, marg = c(0.5, 0.5, 1.5, 0.5), adj = 2, 
    ...) 
{
    if (color.lines) {
        colors <- c("black", "red")
    }
    else {
        colors <- c("black", "black")
    }
    Phi <- NULL
    if (is.null(cex)) 
        cex <- 1
    old.par <- par(mar = marg)
    on.exit(par(old.par))
    if (sort) 
        multi.results$f1 <- fa.sort(multi.results$f1)
    factors <- as.matrix(multi.results$f1$loadings)
    if (is.null(main)) {
        main <- "Hierarchical (multilevel) Structure"
    }
    gloading <- multi.results$f2$loading
    nvar <- num.var <- dim(factors)[1]
    num.factors <- dim(factors)[2]
    num.2level <- dim(gloading)[2]
    e.size <- e.size * 10/nvar
    vars <- paste("V", 1:num.var, sep = "")
    if (!is.null(labels)) {
        vars <- paste(labels)
    }
    else {
        vars <- rownames(factors)
    }
    if (!is.null(flabels)) {
        fact <- flabels
    }
    else {
        fact <- c(paste("F", 1:num.factors, sep = ""))
    }
    colnames(factors)[1:length(fact)] <- fact
    var.rect <- list()
    fact.rect <- list()
    max.len <- max(nchar(rownames(factors))) * rsize
    cex <- min(cex, 40/nvar)
    xleft <- -max.len/2
    xright <- nvar + 1
    plot(0, type = "n", xlim = c(xleft - max.len, xright + 1), 
        ylim = c(1, nvar + 1), frame.plot = FALSE, axes = FALSE, 
        ylab = "", xlab = "", main = main)
    vloc <- xleft
    gloc <- xright
    grouploc <- (xright)/2
    start <- 0
    end <- num.factors
    for (v in 1:nvar) {
        var.rect[[v]] <- dia.rect(vloc, nvar - v + 1, rownames(factors)[v], 
            xlim = c(0, nvar), ylim = c(0, nvar), cex = cex, 
            ...)
    }
    f.scale <- (nvar + 1)/(num.factors + 1)
    f.shift <- nvar/num.factors
    for (f in 1:num.factors) {
        fact.rect[[f]] <- dia.ellipse(grouploc, (num.factors + 
            1 - f) * f.scale, colnames(factors)[f + start], xlim = c(0, 
            nvar), ylim = c(0, nvar), e.size = e.size, ...)
        for (v in 1:nvar) {
            if (abs(factors[v, f + start]) > cut) {
                dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                  col = colors[((sign(factors[v, f + start]) < 
                    0) + 1)], lty = ((sign(factors[v, f + start]) < 
                    0) + 1), labels = round(factors[v, f + start], 
                    digits), adj = f%%adj + 1)
            }
        }
    }
    g.ellipse <- list()
    f.2scale <- (num.var + 1)/(num.2level + 1)
    for (g in 1:num.2level) {
        g.ellipse[[g]] <- dia.ellipse(gloc, (num.2level + 1 - 
            g) * f.2scale, colnames(gloading)[g], xlim = c(0, 
            nvar), ylim = c(0, nvar), e.size = e.size, ...)
        for (f in 1:num.factors) {
            if (abs(gloading[f, g]) > cut) {
                dia.arrow(from = g.ellipse[[g]], to = fact.rect[[f]], 
                  col = colors[((sign(gloading[f, g]) < 0) + 
                    1)], lty = ((sign(gloading[f, g]) < 0) + 
                    1), labels = round(gloading[f, g], digits), 
                  adj = f%%adj + 1)
            }
        }
    }
    if (errors) {
        for (v in 1:nvar) {
            dia.self(location = var.rect[[v]], scale = 0.5, side = side)
        }
    }
}


phi <- function (t, digits = 2) 
{
    stopifnot(prod(dim(t)) == 4 || length(t) == 4)
    if (is.vector(t)) 
        t <- matrix(t, 2)
    r.sum <- rowSums(t)
    c.sum <- colSums(t)
    total <- sum(r.sum)
    r.sum <- r.sum/total
    c.sum <- c.sum/total
    v <- prod(r.sum, c.sum)
    phi <- (t[1, 1]/total - c.sum[1] * r.sum[1])/sqrt(v)
    names(phi) <- NULL
    return(round(phi, digits))
}


structure.diagram <- function (fx = NULL, Phi = NULL, fy = NULL, labels = NULL, cut = 0.3, 
    errors = FALSE, simple = TRUE, regression = FALSE, lr = TRUE, 
    Rx = NULL, Ry = NULL, digits = 1, e.size = 0.1, main = "Structural model", 
    ...) 
{
    xmodel <- fx
    ymodel <- fy
    num.y <- num.x <- 0
    if (!is.null(fx)) {
        if (!is.null(class(xmodel)) && (length(class(xmodel)) > 
            1)) {
            if (class(xmodel)[1] == "psych" && class(xmodel)[2] == 
                "omega") {
                Phi <- xmodel$schmid$phi
                xmodel <- xmodel$schmid$oblique
            }
            else {
                if (class(xmodel)[1] == "psych" && ((class(xmodel)[2] == 
                  "fa") | (class(xmodel)[2] == "principal"))) {
                  if (!is.null(xmodel$Phi)) 
                    Phi <- xmodel$Phi
                  xmodel <- as.matrix(xmodel$loadings)
                }
            }
        }
        else {
            if (!is.matrix(xmodel) & !is.data.frame(xmodel) & 
                !is.vector(xmodel)) {
                if (!is.null(xmodel$Phi)) 
                  Phi <- xmodel$Phi
                xmodel <- as.matrix(xmodel$loadings)
            }
            else {
                xmodel <- xmodel
            }
        }
        if (!is.matrix(xmodel)) {
            factors <- as.matrix(xmodel)
        }
        else {
            factors <- xmodel
        }
        num.var <- num.xvar <- dim(factors)[1]
        if (is.null(num.xvar)) {
            num.xvar <- length(factors)
            num.xfactors <- 1
        }
        else {
            num.factors <- num.xfactors <- dim(factors)[2]
        }
        if (is.null(labels)) {
            vars <- xvars <- rownames(xmodel)
        }
        else {
            xvars <- vars <- labels
        }
        if (is.null(vars)) {
            vars <- xvars <- paste("x", 1:num.xvar, sep = "")
        }
        fact <- colnames(xmodel)
        if (is.null(fact)) {
            fact <- paste("X", 1:num.xfactors, sep = "")
        }
        if (is.numeric(factors)) {
            factors <- round(factors, digits)
        }
    }
    else {
        num.xvar <- dim(Rx)[1]
        num.xfactors <- 0
        num.yfactors <- 0
        num.factors <- 0
        if (is.null(labels)) {
            vars <- xvars <- rownames(Rx)
        }
        else {
            xvars <- vars <- labels
        }
    }
    num.yfactors <- 0
    num.yvar <- 0
    if (!is.null(ymodel)) {
        if (is.list(ymodel) & !is.data.frame(ymodel)) {
            ymodel <- as.matrix(ymodel$loadings)
        }
        else {
            ymodel <- ymodel
        }
        if (!is.matrix(ymodel)) {
            y.factors <- as.matrix(ymodel)
        }
        else {
            y.factors <- ymodel
        }
        num.y <- dim(y.factors)[1]
        if (is.null(num.y)) {
            num.y <- length(ymodel)
            num.yfactors <- 1
        }
        else {
            num.yfactors <- dim(y.factors)[2]
        }
        num.yvar <- num.y
        yvars <- rownames(ymodel)
        if (is.null(yvars)) {
            yvars <- paste("y", 1:num.y, sep = "")
        }
        if (is.null(labels)) {
            vars <- c(xvars, yvars)
        }
        else {
            yvars <- labels[(num.xvar + 1):(num.xvar + num.y)]
        }
        yfact <- colnames(ymodel)
        if (is.null(yfact)) {
            yfact <- paste("Y", 1:num.yfactors, sep = "")
        }
        fact <- c(fact, yfact)
        if (is.numeric(y.factors)) {
            y.factors <- round(y.factors, digits)
        }
    }
    if (!is.null(Ry) & is.null(ymodel)) {
        num.yvar <- num.y <- dim(Ry)[1]
        yvars <- colnames(Ry)
    }
    num.var <- num.xvar + num.y
    num.factors <- num.xfactors + num.yfactors
    sem <- matrix(rep(NA), 6 * (num.var * num.factors + num.factors), 
        ncol = 3)
    lavaan <- vector("list", num.xfactors + num.yfactors)
    colnames(sem) <- c("Path", "Parameter", "Value")
    var.rect <- list()
    fact.rect <- list()
    if (is.numeric(Phi)) {
        Phi <- round(Phi, digits)
    }
    if (!is.null(Rx)) {
        x.curves <- 2
        if (is.numeric(Rx)) {
            Rx <- round(Rx, digits)
        }
    }
    else {
        x.curves <- 0
    }
    if (!is.null(Ry)) {
        y.curves <- 3
        if (is.numeric(Ry)) {
            Ry <- round(Ry, digits)
        }
    }
    else {
        y.curves <- 0
    }
    length.labels <- 0
    strwd <- try(length.labels <- max(strwidth(xvars), strwidth("abc"))/1.8, 
        silent = TRUE)
    if (class(strwd) == "try-error") {
        length.labels = max(nchar(xvars), 3)/1.8
    }
    if (lr) {
        limx <- c(-(length.labels + x.curves), max(num.xvar, 
            num.yvar) + 2 + y.curves)
        limy <- c(0, max(num.xvar, num.yvar) + 1)
    }
    else {
        limy <- c(-(length.labels + x.curves), max(num.xvar, 
            num.yvar) + 2 + y.curves)
        limx <- c(0, max(num.xvar, num.yvar) + 1)
        if (errors) 
            limy <- c(-1, max(num.xvar, num.yvar) + 2)
    }
    scale.xaxis <- 3
    if (lr) {
        plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
            axes = FALSE, ylab = "", xlab = "", main = main)
    }
    else {
        plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
            axes = FALSE, ylab = "", xlab = "", main = main)
    }
    k <- num.factors
    for (v in 1:num.xvar) {
        if (lr) {
            var.rect[[v]] <- dia.rect(0, num.xvar - v + 1, xvars[v], 
                xlim = limx, ylim = limy, ...)
        }
        else {
            var.rect[[v]] <- dia.rect(v, 0, xvars[v], xlim = limy, 
                ylim = limx, ...)
        }
    }
    nvar <- num.xvar
    f.scale <- (num.xvar + 1)/(num.xfactors + 1)
    if (num.xfactors > 0) {
        for (f in 1:num.xfactors) {
            if (!regression) {
                if (lr) {
                  fact.rect[[f]] <- dia.ellipse(limx[2]/scale.xaxis, 
                    (num.xfactors + 1 - f) * f.scale, fact[f], 
                    xlim = c(0, nvar), ylim = c(0, nvar), e.size = e.size, 
                    ...)
                }
                else {
                  fact.rect[[f]] <- dia.ellipse(f * f.scale, 
                    limy[2]/scale.xaxis, fact[f], ylim = c(0, 
                      nvar), xlim = c(0, nvar), e.size = e.size, 
                    ...)
                }
            }
            else {
                if (lr) {
                  fact.rect[[f]] <- dia.rect(limx[2]/scale.xaxis, 
                    (num.xfactors + 1 - f) * f.scale, fact[f], 
                    xlim = c(0, nvar), ylim = c(0, nvar), ...)
                }
                else {
                  fact.rect[[f]] <- dia.rect(f * f.scale, limy[2]/scale.xaxis, 
                    fact[f], xlim = c(0, nvar), ylim = c(0, nvar), 
                    ...)
                }
            }
            for (v in 1:num.xvar) {
                if (is.numeric(factors[v, f])) {
                  if (simple && (abs(factors[v, f]) == max(abs(factors[v, 
                    ]))) && (abs(factors[v, f]) > cut) | (!simple && 
                    (abs(factors[v, f]) > cut))) {
                    if (!regression) {
                      if (lr) {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                          labels = factors[v, f], col = ((sign(factors[v, 
                            f]) < 0) + 1), lty = ((sign(factors[v, 
                            f]) < 0) + 1))
                      }
                      else {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$top, 
                          labels = factors[v, f], col = ((sign(factors[v, 
                            f]) < 0) + 1), lty = ((sign(factors[v, 
                            f]) < 0) + 1))
                      }
                    }
                    else {
                      dia.arrow(to = fact.rect[[f]]$left, from = var.rect[[v]]$right, 
                        labels = factors[v, f], col = ((sign(factors[v, 
                          f]) < 0) + 1))
                    }
                  }
                }
                else {
                  if (factors[v, f] != "0") {
                    if (!regression) {
                      if (lr) {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                          labels = factors[v, f])
                      }
                      else {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$top, 
                          labels = factors[v, f])
                      }
                    }
                    else {
                      if (lr) {
                        dia.arrow(to = fact.rect[[f]], from = var.rect[[v]]$right, 
                          labels = factors[v, f])
                      }
                      else {
                        dia.arrow(to = fact.rect[[f]], from = var.rect[[v]]$top, 
                          labels = factors[v, f])
                      }
                    }
                  }
                }
            }
        }
        if (num.xfactors == 1) {
            lavaan[[1]] <- paste(fact[1], "=~ ")
            for (i in 1:num.xvar) {
                sem[i, 1] <- paste(fact[1], "->", vars[i], sep = "")
                lavaan[[1]] <- paste0(lavaan[[1]], " + ", vars[i])
                if (is.numeric(factors[i])) {
                  sem[i, 2] <- vars[i]
                }
                else {
                  sem[i, 2] <- factors[i]
                }
            }
        }
        k <- num.xvar + 1
        k <- 1
        for (f in 1:num.xfactors) {
            lavaan[[f]] <- paste0(fact[f], " =~ ")
            for (i in 1:num.xvar) {
                if ((!is.numeric(factors[i, f]) && (factors[i, 
                  f] != "0")) || ((is.numeric(factors[i, f]) && 
                  abs(factors[i, f]) > cut))) {
                  sem[k, 1] <- paste(fact[f], "->", vars[i], 
                    sep = "")
                  lavaan[[f]] <- paste0(lavaan[[f]], " + ", vars[i])
                  if (is.numeric(factors[i, f])) {
                    sem[k, 2] <- paste("F", f, vars[i], sep = "")
                  }
                  else {
                    sem[k, 2] <- factors[i, f]
                  }
                  k <- k + 1
                }
            }
        }
    }
    if (errors) {
        for (i in 1:num.xvar) {
            if (lr) {
                dia.self(var.rect[[i]], side = 2)
            }
            else {
                dia.self(var.rect[[i]], side = 1)
            }
            sem[k, 1] <- paste(vars[i], "<->", vars[i], sep = "")
            sem[k, 2] <- paste("x", i, "e", sep = "")
            k <- k + 1
        }
    }
    if (!is.null(ymodel) | !is.null(Ry)) {
        if (lr) {
            y.adj <- num.yvar/2 - num.xvar/2
            f.yscale <- limy[2]/(num.yfactors + 1)
            y.fadj <- 0
        }
        else {
            y.adj <- num.xvar/2 - num.yvar/2
            f.yscale <- limx[2]/(num.yfactors + 1)
            y.fadj <- 0
        }
        for (v in 1:num.yvar) {
            if (lr) {
                var.rect[[v + num.xvar]] <- dia.rect(limx[2] - 
                  y.curves, limy[2] - v + y.adj, yvars[v], xlim = limx, 
                  ylim = limy, ...)
            }
            else {
                var.rect[[v + num.xvar]] <- dia.rect(v + y.adj, 
                  limx[2], yvars[v], xlim = limy, ylim = limx, 
                  ...)
            }
        }
    }
    if (!is.null(ymodel)) {
        for (f in 1:num.yfactors) {
            if (lr) {
                fact.rect[[f + num.xfactors]] <- dia.ellipse(2 * 
                  limx[2]/scale.xaxis, (num.yfactors + 1 - f) * 
                  f.yscale + y.fadj, yfact[f], xlim = c(0, nvar), 
                  ylim = c(0, nvar), e.size = e.size, ...)
            }
            else {
                fact.rect[[f + num.xfactors]] <- dia.ellipse(f * 
                  f.yscale + y.fadj, 2 * limx[2]/scale.xaxis, 
                  yfact[f], ylim = c(0, nvar), xlim = c(0, nvar), 
                  e.size = e.size, ...)
            }
            for (v in 1:num.yvar) {
                if (is.numeric(y.factors[v, f])) {
                  {
                    if (simple && (abs(y.factors[v, f]) == max(abs(y.factors[v, 
                      ]))) && (abs(y.factors[v, f]) > cut) | 
                      (!simple && (abs(factors[v, f]) > cut))) {
                      if (lr) {
                        dia.arrow(from = fact.rect[[f + num.xfactors]], 
                          to = var.rect[[v + num.xvar]]$left, 
                          labels = y.factors[v, f], col = ((sign(y.factors[v, 
                            f]) < 0) + 1))
                      }
                      else {
                        dia.arrow(from = fact.rect[[f + num.xfactors]], 
                          to = var.rect[[v + num.xvar]]$bottom, 
                          labels = y.factors[v, f], col = ((sign(y.factors[v, 
                            f]) < 0) + 1))
                      }
                    }
                  }
                }
                else {
                  if (factors[v, f] != "0") {
                    if (lr) {
                      dia.arrow(from = fact.rect[[f + num.xfactors]], 
                        to = var.rect[[v + num.xvar]]$left, labels = y.factors[v, 
                          f])
                    }
                    else {
                      dia.arrow(from = fact.rect[[f + num.xfactors]], 
                        to = var.rect[[v + num.xvar]]$bottom, 
                        labels = y.factors[v, f])
                    }
                  }
                }
            }
        }
        if (num.yfactors == 1) {
            for (i in 1:num.y) {
                sem[k, 1] <- paste(fact[1 + num.xfactors], "->", 
                  yvars[i], sep = "")
                if (is.numeric(y.factors[i])) {
                  sem[k, 2] <- paste("Fy", yvars[i], sep = "")
                }
                else {
                  sem[k, 2] <- y.factors[i]
                }
                k <- k + 1
            }
        }
        else {
            for (i in 1:num.y) {
                for (f in 1:num.yfactors) {
                  if ((y.factors[i, f] != "0") && (abs(y.factors[i, 
                    f]) > cut)) {
                    sem[k, 1] <- paste(fact[f + num.xfactors], 
                      "->", vars[i + num.xvar], sep = "")
                    if (is.numeric(y.factors[i, f])) {
                      sem[k, 2] <- paste("Fy", f, vars[i + num.xvar], 
                        sep = "")
                    }
                    else {
                      sem[k, 2] <- y.factors[i, f]
                    }
                    k <- k + 1
                  }
                }
            }
        }
        if (errors) {
            for (i in 1:num.y) {
                if (lr) {
                  dia.self(var.rect[[i + num.xvar]], side = 3)
                }
                else {
                  dia.self(var.rect[[i + num.xvar]], side = 3)
                }
                sem[k, 1] <- paste(vars[i + num.xvar], "<->", 
                  vars[i + num.xvar], sep = "")
                sem[k, 2] <- paste("y", i, "e", sep = "")
                k <- k + 1
            }
        }
    }
    if (!is.null(Rx)) {
        for (i in 2:num.xvar) {
            for (j in 1:(i - 1)) {
                if ((!is.numeric(Rx[i, j]) && ((Rx[i, j] != "0") || 
                  (Rx[j, i] != "0"))) || ((is.numeric(Rx[i, j]) && 
                  abs(Rx[i, j]) > cut))) {
                  if (lr) {
                    if (abs(i - j) < 2) {
                      dia.curve(from = var.rect[[j]]$left, to = var.rect[[i]]$left, 
                        labels = Rx[i, j], scale = -3 * (i - 
                          j)/num.xvar)
                    }
                    else {
                      dia.curve(from = var.rect[[j]]$left, to = var.rect[[i]]$left, 
                        labels = Rx[i, j], scale = -3 * (i - 
                          j)/num.xvar)
                    }
                  }
                  else {
                    if (abs(i - j) < 2) {
                      dia.curve(from = var.rect[[j]]$bottom, 
                        to = var.rect[[i]]$bottom, labels = Rx[i, 
                          j], scale = -3 * (i - j)/num.xvar)
                    }
                    else {
                      dia.curve(from = var.rect[[j]]$bottom, 
                        to = var.rect[[i]]$bottom, labels = Rx[i, 
                          j], scale = -3 * (i - j)/num.xvar)
                    }
                  }
                }
            }
        }
    }
    if (!is.null(Ry)) {
        for (i in 2:num.yvar) {
            for (j in 1:(i - 1)) {
                if ((!is.numeric(Ry[i, j]) && ((Ry[i, j] != "0") || 
                  (Ry[j, i] != "0"))) || ((is.numeric(Ry[i, j]) && 
                  abs(Ry[i, j]) > cut))) {
                  if (lr) {
                    if (abs(i - j) < 2) {
                      dia.curve(from = var.rect[[j + num.xvar]]$right, 
                        to = var.rect[[i + num.xvar]]$right, 
                        labels = Ry[i, j], scale = 3 * (i - j)/num.xvar)
                    }
                    else {
                      dia.curve(from = var.rect[[j + num.xvar]]$right, 
                        to = var.rect[[i + num.xvar]]$right, 
                        labels = Ry[i, j], scale = 3 * (i - j)/num.xvar)
                    }
                  }
                  else {
                    if (abs(i - j) < 2) {
                      dia.curve(from = var.rect[[j + num.xvar]]$bottom, 
                        to = var.rect[[i + num.xvar]]$bottom, 
                        labels = Ry[i, j], scale = 3 * (i - j)/num.xvar)
                    }
                    else {
                      dia.curve(from = var.rect[[j + num.xvar]]$bottom, 
                        to = var.rect[[i + num.xvar]]$bottom, 
                        labels = Ry[i, j], scale = 3 * (i - j)/num.xvar)
                    }
                  }
                }
            }
        }
    }
    if (!regression) {
        if (!is.null(Phi)) {
            if (!is.matrix(Phi)) {
                if (!is.null(fy)) {
                  Phi <- matrix(c(1, 0, Phi, 1), ncol = 2)
                }
                else {
                  Phi <- matrix(c(1, Phi, Phi, 1), ncol = 2)
                }
            }
            if (num.xfactors > 1) {
                for (i in 2:num.xfactors) {
                  for (j in 1:(i - 1)) {
                    {
                      if ((!is.numeric(Phi[i, j]) && ((Phi[i, 
                        j] != "0") || (Phi[j, i] != "0"))) || 
                        ((is.numeric(Phi[i, j]) && abs(Phi[i, 
                          j]) > cut))) {
                        if (Phi[i, j] == Phi[j, i]) {
                          if (lr) {
                            dia.curve(from = fact.rect[[i]]$right, 
                              to = fact.rect[[j]]$right, labels = Phi[i, 
                                j], scale = 2 * (i - j)/num.xfactors)
                          }
                          else {
                            dia.curve(from = fact.rect[[i]]$top, 
                              to = fact.rect[[j]]$top, labels = Phi[i, 
                                j], scale = 2 * (i - j)/num.xfactors)
                          }
                          sem[k, 1] <- paste(fact[i], "<->", 
                            fact[j], sep = "")
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          if (Phi[i, j] != "0") {
                            if (lr) {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[j]], 
                                  to = fact.rect[[i]], labels = Phi[i, 
                                    j], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[j]]$right, 
                                  to = fact.rect[[i]]$right, 
                                  labels = Phi[i, j], scale = 2 * 
                                    (i - j)/num.xfactors)
                              }
                            }
                            else {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[j]], 
                                  to = fact.rect[[i]], labels = Phi[i, 
                                    j], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[j]]$top, 
                                  to = fact.rect[[i]]$top, labels = Phi[i, 
                                    j], scale = 2 * (i - j)/num.xfactors)
                              }
                            }
                            sem[k, 1] <- paste(fact[j], " ->", 
                              fact[i], sep = "")
                            sem[k, 2] <- paste("rF", j, "F", 
                              i, sep = "")
                          }
                          else {
                            if (lr) {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[i]], 
                                  to = fact.rect[[j]], labels = Phi[j, 
                                    i], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[i]]$right, 
                                  to = fact.rect[[j]]$right, 
                                  labels = Phi[j, i], scale = 2 * 
                                    (i - j)/num.xfactors)
                              }
                            }
                            else {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[i]], 
                                  to = fact.rect[[j]], labels = Phi[j, 
                                    i], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[i]]$top, 
                                  to = fact.rect[[j]]$top, labels = Phi[j, 
                                    i], scale = 2 * (i - j)/num.xfactors)
                              }
                            }
                            sem[k, 1] <- paste(fact[i], "<-", 
                              fact[j], sep = "")
                            sem[k, 2] <- paste("rF", i, "F", 
                              j, sep = "")
                          }
                        }
                      }
                      else {
                        sem[k, 1] <- paste(fact[i], "<->", fact[j], 
                          sep = "")
                        if (is.numeric(Phi[i, j])) {
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          sem[k, 2] <- Phi[i, j]
                        }
                      }
                      k <- k + 1
                    }
                  }
                }
            }
            if (!is.null(ymodel)) {
                for (i in 1:num.xfactors) {
                  for (j in 1:num.yfactors) {
                    if ((!is.numeric(Phi[j + num.xfactors, i]) && 
                      (Phi[j + num.xfactors, i] != "0")) || ((is.numeric(Phi[j + 
                      num.xfactors, i]) && abs(Phi[j + num.xfactors, 
                      i]) > cut))) {
                      dia.arrow(from = fact.rect[[i]], to = fact.rect[[j + 
                        num.xfactors]], Phi[j + num.xfactors, 
                        i])
                      sem[k, 1] <- paste(fact[i], "->", fact[j + 
                        num.xfactors], sep = "")
                    }
                    else {
                      sem[k, 1] <- paste(fact[i], "<->", fact[j + 
                        num.xfactors], sep = "")
                    }
                    if (is.numeric(Phi[j + num.xfactors, i])) {
                      sem[k, 2] <- paste("rX", i, "Y", j, sep = "")
                    }
                    else {
                      sem[k, 2] <- Phi[j + num.xfactors, i]
                    }
                    k <- k + 1
                  }
                }
            }
        }
    }
    if (num.factors > 0) {
        for (f in 1:num.factors) {
            sem[k, 1] <- paste(fact[f], "<->", fact[f], sep = "")
            sem[k, 3] <- "1"
            k <- k + 1
        }
        model = sem[1:(k - 1), ]
        class(model) <- "mod"
        lavaan <- unlist(lavaan)
        lavaan <- noquote(lavaan)
        result <- list(sem = model, lavaan = lavaan)
        return(invisible(result))
    }
}


fa.poly <- function (x, nfactors = 1, n.obs = NA, n.iter = 1, rotate = "oblimin", 
    SMC = TRUE, missing = FALSE, impute = "median", min.err = 0.001, 
    max.iter = 50, symmetric = TRUE, warnings = TRUE, fm = "minres", 
    alpha = 0.1, p = 0.05, scores = "regression", oblique.scores = TRUE, 
    weight = NULL, global = TRUE, ...) 
{
    cl <- match.call()
    ncat <- 8
    n.obs <- dim(x)[1]
    tx <- table(as.matrix(x))
    if (dim(tx)[1] == 2) {
        tet <- tetrachoric(x)
        typ = "tet"
    }
    else {
        tab <- apply(x, 2, function(x) table(x))
        if (is.list(tab)) {
            len <- lapply(tab, function(x) length(x))
        }
        else {
            len <- dim(tab)[1]
        }
        nvar <- ncol(x)
        dvars <- subset(1:nvar, len == 2)
        pvars <- subset(1:nvar, ((len > 2) & (len <= ncat)))
        cvars <- subset(1:nvar, (len > ncat))
        if (length(pvars) == ncol(x)) {
            tet <- polychoric(x, weight = weight, global = global)
            typ = "poly"
        }
        else {
            tet <- mixed.cor(x, weight = weight, global = global)
            typ = "mixed"
        }
    }
    r <- tet$rho
    f <- fa(r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, 
        SMC = SMC, missing = FALSE, impute = impute, min.err = min.err, 
        max.iter = max.iter, symmetric = symmetric, warnings = warnings, 
        fm = fm, alpha = alpha, scores = scores, oblique.scores = oblique.scores, 
        ...)
    f$Call <- cl
    fl <- f$loadings
    nvar <- dim(fl)[1]
    if (n.iter > 1) {
        e.values <- list(pc = vector("list", n.iter), fa = vector("list", 
            n.iter))
        replicates <- vector("list", n.iter)
        rep.rots <- vector("list", n.iter)
        for (trials in 1:n.iter) {
            xs <- x[sample(n.obs, n.obs, replace = TRUE), ]
            if (typ != "tet") {
                tets <- mixed.cor(xs, weight = weight, global = global)
            }
            else {
                tets <- tetrachoric(xs, weight = weight)
            }
            r <- tets$rho
            values.samp <- eigen(tets$rho)$values
            e.values[["pc"]][[trials]] <- values.samp
            fs <- fa(r, nfactors = nfactors, rotate = rotate, 
                SMC = SMC, missing = FALSE, impute = impute, 
                min.err = min.err, max.iter = max.iter, symmetric = symmetric, 
                warnings = warnings, fm = fm, alpha = alpha, 
                ...)
            e.values[["fa"]][[trials]] <- fs$values
            if (nfactors > 1) {
                t.rot <- target.rot(fs$loadings, fl)
                replicates[[trials]] <- t.rot$loadings
                if (!is.null(fs$Phi)) {
                  phis <- fs$Phi
                  rep.rots[[trials]] <- phis[lower.tri(phis)]
                }
            }
            else {
                replicates[[trials]] <- fs$loadings
            }
        }
        replicates <- matrix(unlist(replicates), ncol = nfactors * 
            nvar, byrow = TRUE)
        if (!is.null(f$Phi)) {
            rep.rots <- matrix(unlist(rep.rots), ncol = nfactors * 
                (nfactors - 1)/2, byrow = TRUE)
            z.rot <- fisherz(rep.rots)
            means.rot <- colMeans(z.rot, na.rm = TRUE)
            sds.rot <- apply(z.rot, 2, sd, na.rm = TRUE)
            sds.rot <- fisherz2r(sds.rot)
            ci.rot.lower <- means.rot + qnorm(p/2) * sds.rot
            ci.rot.upper <- means.rot + qnorm(1 - p/2) * sds.rot
            means.rot <- fisherz2r(means.rot)
            ci.rot.lower <- fisherz2r(ci.rot.lower)
            ci.rot.upper <- fisherz2r(ci.rot.upper)
            ci.rot <- data.frame(lower = ci.rot.lower, upper = ci.rot.upper)
        }
        else {
            rep.rots <- NULL
            means.rot <- NULL
            sds.rot <- NULL
            z.rot <- NULL
            ci.rot <- NULL
        }
        z.replicates <- fisherz(replicates)
        means <- matrix(colMeans(z.replicates, na.rm = TRUE), 
            ncol = nfactors)
        sds <- matrix(apply(z.replicates, 2, sd, na.rm = TRUE), 
            ncol = nfactors)
        ci.lower <- means + qnorm(p/2) * sds
        ci.upper <- means + qnorm(1 - p/2) * sds
        means <- fisherz2r(means)
        sds <- fisherz2r(sds)
        ci.lower <- fisherz2r(ci.lower)
        ci.upper <- fisherz2r(ci.upper)
        ci <- data.frame(lower = ci.lower, upper = ci.upper)
        class(means) <- "loadings"
        colnames(means) <- colnames(sds) <- colnames(fl)
        rownames(means) <- rownames(sds) <- rownames(fl)
        ei.pc <- describe(matrix(unlist(e.values$pc), ncol = nvar, 
            byrow = TRUE))
        ei.fa <- describe(matrix(unlist(e.values$fa), ncol = nvar, 
            byrow = TRUE))
        e.stats <- list(ob.fa = f$values, ob.pc = f$e.values, 
            pc = ei.pc, fa = ei.fa)
        results <- list(fa = f, rho = tet$rho, tau = tet$tau, 
            n.obs = n.obs, means = means, sds = sds, ci = ci, 
            means.rot = means.rot, sds.rot = sds.rot, ci.rot = ci.rot, 
            Call = cl, replicates = replicates, rep.rots = rep.rots, 
            e.values = e.values, e.stats = e.stats)
        class(results) <- c("psych", "fa.ci")
    }
    else {
        results <- list(fa = f, rho = r, tau = tet$tau, n.obs = n.obs, 
            Call = cl)
        if (oblique.scores) {
            results$scores <- factor.scores(x, f = f$loadings, 
                Phi = f$Phi, method = scores, rho = r)
        }
        else {
            results$scores <- factor.scores(x, f = f$Structure, 
                method = scores, rho = r)
        }
        class(results) <- c("psych", "fa")
    }
    return(results)
}


cohen.kappa <- function (x, w = NULL, n.obs = NULL, alpha = 0.05) 
{
    cl <- match.call()
    p <- dim(x)[1]
    len <- p
    bad <- FALSE
    if ((dim(x)[2] == p) || (dim(x)[2] < 3)) {
        result <- cohen.kappa1(x, w = w, n.obs = n.obs, alpha = alpha)
    }
    else {
        nvar <- dim(x)[2]
        ck <- matrix(NA, nvar, nvar)
        if (!is.null(colnames(x))) {
            colnames(ck) <- rownames(ck) <- colnames(x)
        }
        else {
            colnames(ck) <- rownames(ck) <- paste("R", 1:nvar, 
                sep = "")
        }
        diag(ck) <- 1
        result <- list(cohen.kappa = ck)
        k <- 2
        for (i in 2:nvar) {
            for (j in 1:(i - 1)) {
                x1 <- data.frame(x[, i], x[, j])
                x1 <- na.omit(x1)
                ck1 <- cohen.kappa1(x1, w = w, n.obs = n.obs, 
                  alpha = alpha)
                result[[paste(colnames(ck)[j], rownames(ck)[i])]] <- ck1
                if (ck1$bad) {
                  warning("No variance detected in cells ", i, 
                    "  ", j)
                  bad <- TRUE
                }
                ck[i, j] <- result[[k]]$kappa
                ck[j, i] <- result[[k]]$weighted.kappa
                k <- k + 1
            }
        }
        result[[1]] <- ck
    }
    if (bad) 
        message("At least one item had no variance.  Try describe(your.data) to find the problem.")
    class(result) <- c("psych", "kappa")
    return(result)
}


r2t <- function (rho, n) 
{
    return(rho * sqrt((n - 2)/(1 - rho^2)))
}


sim.structural <- function (fx = NULL, Phi = NULL, fy = NULL, f = NULL, n = 0, 
    uniq = NULL, raw = TRUE, items = FALSE, low = -2, high = 2, 
    d = NULL, cat = 5, mu = 0) 
{
    cl <- match.call()
    if (is.null(f)) {
        if (is.null(fy)) {
            f <- fx
        }
        else {
            f <- superMatrix(fx, fy)
        }
    }
    f <- as.matrix(f)
    if (!is.null(Phi)) {
        if (length(Phi) == 1) 
            Phi <- matrix(c(1, Phi, Phi, 1), 2, 2)
    }
    nf <- ncol(f)
    nvar <- nrow(f)
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar/nf - 1))
        d <- rep(d, nf)
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    a <- rep(1, nvar)
    if (is.vector(f)) {
        f <- as.matrix(f)
        Phi <- 1
    }
    if (!is.null(Phi)) {
        model <- f %*% Phi %*% t(f)
    }
    else {
        model <- f %*% t(f)
    }
    if (is.null(uniq)) {
        diag(model) <- 1
    }
    else {
        diag(model) <- uniq + diag(model)
    }
    nvar <- dim(f)[1]
    if (is.null(rownames(model))) {
        colnames(model) <- rownames(model) <- paste("V", 1:nvar, 
            sep = "")
    }
    if (n > 0) {
        mu <- rep(mu, nvar)
        eX <- eigen(model)
        observed <- matrix(rnorm(nvar * n), n)
        observed <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
            0)), nvar) %*% t(observed) + mu)
        if (items) {
            observedp <- matrix(t(pnorm(a * t(observed) - d)), 
                n, nvar)
            observed[] <- rbinom(n * nvar, cat, observedp)
        }
        colnames(observed) <- colnames(model)
        r <- cor(observed)
    }
    reliability <- diag(f %*% t(f))
    if (n < 1) {
        results <- list(model = model, reliability = reliability)
    }
    else {
        if (!raw) {
            results <- list(model = model, reliability = reliability, 
                r = r, N = n)
        }
        else {
            results <- list(model = model, reliability = reliability, 
                r = r, observed = observed, N = n)
        }
    }
    results$Call <- cl
    class(results) <- c("psych", "sim")
    return(results)
}


score.irt <- function (stats = NULL, items, keys = NULL, cut = 0.3, bounds = c(-4, 
    4), mod = "logistic") 
{
    message("score.irt is deprecated and has been replaced by scoreIrt, please change your call")
    scoreIrt(stats = stats, items = items, keys = keys, cut = cut, 
        bounds = bounds, mod = mod)
}


test.irt <- function (nvar = 9, n.obs = 1000, mod = "logistic", type = "tetra", 
    low = -3, high = 3, seed = NULL) 
{
    if (!is.null(seed)) 
        set.seed(seed)
    if (type == "tetra") {
        x.sim <- sim.irt(nvar = nvar, n = n.obs, low = low, high = high, 
            mod = mod)
    }
    else {
        x.sim <- sim.poly(nvar = nvar, n = n.obs, low = low, 
            high = high, mod = mod)
    }
    x.irt <- irt.fa(x.sim$items[, 1:nvar], sort = FALSE, plot = FALSE)
    xnvart <- data.frame(x.sim$items, theta = x.sim$theta)
    xnvart <- dfOrder(xnvart, c(1:nvar))
    x.fsall <- psych::factor.scores(xnvart[1:nvar], x.irt$fa, 
        method = "regression")$scores
    x.df <- data.frame(xnvart, fs = x.fsall)
    xdelta <- x.irt$irt$difficulty[[1]]
    xbeta <- x.irt$irt$discrimination
    x.scores <- data.matrix(x.df[1:nvar])
    irt.sc <- scoreIrt(x.irt, x.scores)
    irt.scn <- scoreIrt(x.irt, x.scores, mod = "normal")
    ras <- 1
    x.tot <- rowSums(x.scores[, 1:nvar])
    if (type == "tetra") {
        pl2 <- simpleScore(x.scores, xdelta, xbeta)
        pl1 <- simpleScore(x.scores, xdelta, rep(ras, nvar))
        pn2 <- simpleScore(x.scores, xdelta, xbeta, mod = "normal")
        pn1 <- simpleScore(x.scores, xdelta, rep(ras, nvar), 
            mod = "normal")
        x.df.sc <- data.frame(logist2pl = pl2, pl1, pn2, pn1, 
            x.tot, fs = x.df$MR1, irt.sc[, 1], irt.scn[, 1], 
            theta = x.df$theta)
        colnames(x.df.sc) <- c("PL2", "PL1", "PN2", "PN1", "total", 
            "factor", "irt", "irt-N", "theta")
    }
    else {
        x.df.sc <- data.frame(x.tot, fs = x.df$MR1, irt.sc[, 
            1], irt.scn[, 1], theta = x.df$theta)
        colnames(x.df.sc) <- c("total", "factor", "irt", "irt-N", 
            "theta")
    }
    pairs.panels(x.df.sc)
    invisible(x.df.sc)
}


lookup <- function (x, y, criteria = NULL) 
{
    if (is.null(criteria)) {
        temp <- match(x, rownames(y))
    }
    else {
        temp <- match(x, y[, criteria])
    }
    y <- (y[temp[!is.na(temp)], , drop = FALSE])
    return(y)
}


irt.responses <- function (theta, items, breaks = 11, show.missing = FALSE, show.legend = TRUE, 
    legend.location = "topleft", colors = NULL, ...) 
{
    if (is.null(colors)) 
        colors = c("black", "blue", "red", "darkgreen", "gold2", 
            "gray50", "cornflowerblue", "mediumorchid2")
    item.counts <- names(table(as.vector(unlist(items))))
    uniqueitems <- as.numeric(item.counts)
    nalt <- length(uniqueitems) + 1
    nvar <- ncol(items)
    theta.min <- min(theta, na.rm = TRUE)
    theta.max <- max(theta, na.rm = TRUE)
    binrange <- cut(theta, breaks = breaks)
    binnums <- as.numeric(binrange)
    items <- as.matrix(items)
    stats <- by(items, binnums, function(x) response.frequencies(x, 
        uniqueitems = uniqueitems))
    stats.m <- unlist(stats)
    stats.m <- matrix(stats.m, ncol = nvar * nalt, byrow = TRUE)
    theta <- seq(theta.min, theta.max, length.out = breaks)
    for (i in 1:nvar) {
        plot(theta, stats.m[, i], ylim = c(0, 1), typ = "l", 
            xlab = "theta", ylab = "P(response)", main = paste(colnames(items)[i]), 
            col = colors[1], ...)
        for (j in 1:(nalt - 2 + show.missing)) {
            points(theta, stats.m[, i + nvar * j], typ = "l", 
                lty = (j + 1), col = colors[j + 1], ...)
        }
        if (show.legend) {
            legend(legend.location, paste(item.counts[1:(nalt - 
                1 + show.missing)]), text.col = colors[1:(nalt - 
                1 + show.missing)], lty = 1:(nalt - 1 + show.missing), 
                ncol = 4, bty = "n")
        }
    }
}


isCorrelation <- function (x) 
{
    return(!is.data.frame(x) && isSymmetric(x))
}


VSS.sim <- function (ncases = 1000, nvariables = 16, nfactors = 4, meanloading = 0.5, 
    dichot = FALSE, cut = 0) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    if (dichot) {
        items <- (items[, 1:nvariables] >= cut)
        items <- items + 0
    }
    return(items)
}


omega.diagram <- function (om.results, sl = TRUE, sort = TRUE, labels = NULL, 
    flabels = NULL, cut = 0.2, gcut = 0.2, simple = TRUE, errors = FALSE, 
    digits = 1, e.size = 0.1, rsize = 0.15, side = 3, main = NULL, 
    cex = NULL, color.lines = TRUE, marg = c(0.5, 0.5, 1.5, 0.5), 
    adj = 2, ...) 
{
    if (color.lines) {
        colors <- c("black", "red")
    }
    else {
        colors <- c("black", "black")
    }
    Phi <- NULL
    if (is.null(cex)) 
        cex <- 1
    old.par <- par(mar = marg)
    on.exit(par(old.par))
    if ((length(class(om.results)) > 1) && ((class(om.results)[2] == 
        "extend"))) {
        extend <- TRUE
        class(om.results)[2] <- "omega"
    }
    else {
        extend <- FALSE
    }
    if ((length(class(om.results)) > 1) && ((class(om.results)[2] == 
        "omegaSem"))) {
        if (is.null(om.results$omega.efa$cfa.loads)) {
            cfa.loads <- om.results$cfa.loads
        }
        else {
            cfa.loads <- om.results$omega.efa$cfa.loads
        }
        if (sort) 
            cfa.loads <- fa.sort(cfa.loads)
        factors <- as.matrix(cfa.loads)
        gloading <- cfa.loads[, 1, drop = FALSE]
        nvar <- num.var <- nrow(gloading)
        num.factors <- ncol(factors) - 1
        sl = TRUE
        main <- "Omega from SEM"
    }
    else {
        if (extend) 
            class(om.results)[2] <- "omega"
        if (sort) 
            om.results <- fa.sort(om.results)
        if (sl) {
            factors <- as.matrix(om.results$schmid$sl)
            if (is.null(main)) {
                main <- "Omega with Schmid Leiman Transformation"
            }
        }
        else {
            factors <- as.matrix(om.results$schmid$oblique)
            if (is.null(main)) {
                main <- "Hierarchical (multilevel) Structure"
            }
        }
        gloading <- om.results$schmid$gloading
        nvar <- num.var <- dim(factors)[1]
        if (sl) {
            num.factors <- dim(factors)[2] - 1 - (!extend) * 
                3
        }
        else {
            num.factors <- dim(factors)[2]
        }
    }
    e.size <- e.size * 10/nvar
    vars <- paste("V", 1:num.var, sep = "")
    if (!is.null(labels)) {
        vars <- paste(labels)
    }
    else {
        vars <- rownames(factors)
    }
    if (!is.null(flabels)) {
        fact <- flabels
    }
    else {
        if (sl) {
            fact <- c("g", paste("F", 1:num.factors, "*", sep = ""))
        }
        else {
            fact <- c(paste("F", 1:num.factors, sep = ""))
        }
    }
    colnames(factors)[1:length(fact)] <- fact
    var.rect <- list()
    fact.rect <- list()
    max.len <- max(nchar(rownames(factors))) * rsize
    cex <- min(cex, 40/nvar)
    xleft <- -max.len/2
    xright <- nvar + 1
    plot(0, type = "n", xlim = c(xleft - max.len, xright + 1), 
        ylim = c(1, nvar + 1), frame.plot = FALSE, axes = FALSE, 
        ylab = "", xlab = "", main = main)
    if (sl) {
        vloc <- (xright)/2
        gloc <- xleft
        grouploc <- xright
        start <- 1
        end <- num.factors + 1
    }
    else {
        vloc <- xleft
        gloc <- xright
        grouploc <- (xright)/2
        start <- 0
        end <- num.factors
    }
    for (v in 1:nvar) {
        var.rect[[v]] <- dia.rect(vloc, nvar - v + 1, rownames(factors)[v], 
            xlim = c(0, nvar), ylim = c(0, nvar), cex = cex, 
            ...)
    }
    f.scale <- (nvar + 1)/(num.factors + 1)
    f.shift <- nvar/num.factors
    for (f in 1:num.factors) {
        fact.rect[[f]] <- dia.ellipse(grouploc, (num.factors + 
            1 - f) * f.scale, colnames(factors)[f + start], xlim = c(0, 
            nvar), ylim = c(0, nvar), e.size = e.size, ...)
        for (v in 1:nvar) {
            if (abs(factors[v, f + start]) > cut) {
                dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                  col = colors[((sign(factors[v, f + start]) < 
                    0) + 1)], lty = ((sign(factors[v, f + start]) < 
                    0) + 1), labels = round(factors[v, f + start], 
                    digits), adj = f%%adj + 1)
            }
        }
    }
    g.ellipse <- dia.ellipse(gloc, (num.var + 1)/2, "g", xlim = c(0, 
        nvar), ylim = c(0, nvar), e.size = e.size, ...)
    if (!sl) {
        for (f in 1:num.factors) {
            dia.arrow(from = g.ellipse, to = fact.rect[[f]], 
                col = colors[((sign(gloading[f]) < 0) + 1)], 
                lty = ((sign(gloading[f]) < 0) + 1), labels = round(gloading[f], 
                  digits), adj = f%%adj + 1)
        }
    }
    else {
        for (i in 1:nvar) {
            if (abs(factors[i, 1]) > gcut) {
                dia.arrow(from = g.ellipse, to = var.rect[[i]]$left, 
                  col = colors[((sign(factors[i, 1]) < 0) + 1)], 
                  lty = ((sign(factors[i, 1]) < 0) + 1), labels = round(factors[i, 
                    1], digits), adj = 1)
            }
        }
    }
    if (errors) {
        for (v in 1:nvar) {
            dia.self(location = var.rect[[v]], scale = 0.5, side = side)
        }
    }
}


iclust.sort <- function (ic.load, cut = 0, labels = NULL, keys = FALSE, clustsort = TRUE) 
{
    ICLUST.sort(ic.load, labels, keys, clustsort)
}


phi.demo <- function (n = 1000, r = 0.6, cuts = c(-2, -1, 0, 1, 2)) 
{
    latent <- rnorm(n)
    err <- rnorm(n)
    observed <- latent * (r) + err * sqrt(1 - r * r)
    trunc <- matrix(rep(observed, length(cuts)), ncol = length(cuts))
    for (j in 1:length(cuts)) {
        trunc[, j] <- (trunc[, j] > cuts[j])
    }
    d.mat <- data.frame(latent, observed, trunc)
    pairs.panels(d.mat, main = "Phi coefficients for extreme cut point normal data")
    trunc.cor <- cor(d.mat)
    freq <- apply(d.mat, 2, mean)
    tetra <- tetrachoric(d.mat[, 3:7], correct = FALSE)
    yule <- YuleCor(trunc)
    for (i in 4:length(d.mat)) {
        for (j in 3:i) {
            trunc.cor[j, i] <- phi2tetra(trunc.cor[i, j], c(freq[i], 
                freq[j]))
        }
    }
    result <- list(tetrachoric = tetra, phis = trunc.cor, Yule = yule, 
        data = d.mat)
    class(result) <- c("psych", "phi.demo")
    return(result)
}


cortest.mat <- function (R1, R2 = NULL, n1 = NULL, n2 = NULL) 
{
    cl <- match.call()
    p <- dim(R1)[2]
    if (dim(R1)[1] != p) {
        n1 <- dim(R1)[1]
        R1 <- cor(R1, use = "pairwise")
        warning("R1 matrix was not square, correlations found")
    }
    if (!is.matrix(R1)) 
        R1 <- as.matrix(R1)
    if (is.null(n1)) {
        warning("n1 not specified, set to 100")
        n1 <- 100
    }
    if (is.null(R2)) {
        message("Bartlett's test of is R = I")
        detR1 <- det(R1)
        chi2 <- -log(detR1) * (n1 - 1 - (2 * p + 5)/6)
        df <- p * (p - 1)/2
        pval <- pchisq(chi2, df, lower.tail = FALSE)
        n.obs <- n1
    }
    else {
        if (dim(R2)[1] != dim(R2)[2]) {
            n2 <- dim(R2)[1]
            R2 <- cor(R2, use = "pairwise")
            warning("R2 matrix was not square, correlations found")
        }
        if (p != dim(R2)[2]) 
            stop("correlation matrices R1 and R2 must be of the same size!")
        if (!is.matrix(R2)) 
            R2 <- as.matrix(R2)
        R1.inv <- solve(R1)
        R2.inv <- solve(R2)
        R.inv.2 <- R1.inv %*% R2
        R.inv.1 <- R2.inv %*% R1
        E1 <- 0.5 * (sum((diag(R.inv.2))) - log(det(R.inv.2)) - 
            p)
        E2 <- 0.5 * (sum((diag(R.inv.1))) - log(det(R.inv.1)) - 
            p)
        df1 <- p * (p - 1)/2
        df <- 2 * df1
        if (is.null(n2)) {
            n2 <- n1
        }
        n.obs <- min(n1, n2)
        chi21 <- E1 * (n1 - 1 - (2 * p - 5)/6)
        chi22 <- E2 * (n2 - 1 - (2 * p - 5)/6)
        chi2 <- chi21 + chi22
    }
    results <- list(chi2 = chi2, prob = pchisq(chi2, df, lower.tail = FALSE), 
        df = df, n.obs = n.obs, Call = cl)
    class(results) <- c("psych", "cortest")
    return(results)
}


vss <- function (x, n = 8, rotate = "varimax", diagonal = FALSE, fm = "minres", 
    n.obs = NULL, plot = TRUE, title = "Very Simple Structure", 
    use = "pairwise", cor = "cor", ...) 
{
    cl <- match.call()
    if (rotate == "oblimin") {
        if (!requireNamespace("GPArotation")) {
            stop("You must have GPArotation installed to use oblimin rotation")
        }
    }
    old_rotate = rotate
    complexrow <- function(x, c) {
        n = length(x)
        temp <- x
        x <- rep(0, n)
        for (j in 1:c) {
            locmax <- which.max(abs(temp))
            x[locmax] <- sign(temp[locmax]) * max(abs(temp))
            temp[locmax] <- 0
        }
        return(x)
    }
    complexmat <- function(x, c) {
        nrows <- dim(x)[1]
        ncols <- dim(x)[2]
        for (i in 1:nrows) {
            x[i, ] <- complexrow(x[i, ], c)
        }
        return(x)
    }
    map <- function(x, n) {
        nvar <- dim(x)[2]
        min.partial <- rep(NA, n)
        e <- eigen(x)
        evect <- e$vectors
        comp <- evect %*% diag(sqrt(e$values))
        if (n >= nvar) {
            n1 <- nvar - 1
        }
        else {
            n1 <- n
        }
        for (i in 1:n1) {
            c11.star <- x - comp[, 1:i] %*% t(comp[, 1:i])
            d <- diag(1/sqrt(diag(c11.star)))
            rstar <- d %*% c11.star %*% d
            diag(rstar) <- 0
            min.partial[i] <- sum(rstar * rstar)/(nvar * (nvar - 
                1))
        }
        return(min.partial)
    }
    if (dim(x)[2] < n) 
        n <- dim(x)[2]
    complexfit <- array(0, dim = c(n, n))
    complexresid <- array(0, dim = c(n, n))
    vss.df <- data.frame(dof = rep(0, n), chisq = NA, prob = NA, 
        sqresid = NA, fit = NA, RMSEA = NA, BIC = NA, SABIC = NA, 
        complex = NA, eChisq = NA, SRMR = NA, eCRMS = NA, eBIC = NA)
    if (dim(x)[1] != dim(x)[2]) {
        n.obs <- dim(x)[1]
        switch(cor, cor = {
            x <- cor(x, use = use)
        }, cov = {
            x <- cov(x, use = use)
            covar <- TRUE
        }, tet = {
            x <- tetrachoric(x)$rho
        }, poly = {
            x <- polychoric(x)$rho
        }, mixed = {
            x <- mixed.cor(x, use = use)$rho
        }, Yuleb = {
            x <- YuleCor(x, , bonett = TRUE)$rho
        }, YuleQ = {
            x <- YuleCor(x, 1)$rho
        }, YuleY = {
            x <- YuleCor(x, 0.5)$rho
        })
    }
    else {
        if (!is.matrix(x)) 
            x <- as.matrix(x)
    }
    if (is.null(n.obs)) {
        message("n.obs was not specified and was arbitrarily set to 1000.  This only affects the chi square values.")
        n.obs <- 1000
    }
    map.values <- map(x, n)
    if (n > dim(x)[2]) {
        n <- dim(x)[2]
    }
    for (i in 1:n) {
        PHI <- diag(i)
        if (i < 2) {
            (rotate = "none")
        }
        else {
            rotate = old_rotate
        }
        if (!(fm == "pc")) {
            f <- fa(x, i, rotate = rotate, n.obs = n.obs, warnings = FALSE, 
                fm = fm, scores = "none", cor = cor, ...)
            if (i == 1) {
                original <- x
                sqoriginal <- original * original
                totaloriginal <- sum(sqoriginal) - diagonal * 
                  sum(diag(sqoriginal))
            }
        }
        else {
            f <- principal(x, i)
            if (i == 1) {
                original <- x
                sqoriginal <- original * original
                totaloriginal <- sum(sqoriginal) - diagonal * 
                  sum(diag(sqoriginal))
            }
            if ((rotate == "varimax") & (i > 1)) {
                f <- varimax(f$loadings)
                PHI <- diag(i)
            }
            else {
                if (((rotate == "promax") | (rotate == "Promax")) & 
                  (i > 1)) {
                  f <- Promax(f$loadings)
                  PHI <- f$Phi
                }
                else {
                  if ((rotate == "oblimin") & (i > 1)) {
                    f <- GPArotation::oblimin(f$loadings)
                    U <- f$Th
                    phi <- t(U) %*% U
                    PHI <- cov2cor(phi)
                  }
                }
            }
        }
        load <- as.matrix(f$loadings)
        model <- load %*% PHI %*% t(load)
        residual <- original - model
        sqresid <- residual * residual
        totalresid <- sum(sqresid) - diagonal * sum(diag(sqresid))
        fit <- 1 - totalresid/totaloriginal
        if ((fm != "pc")) {
            vss.df[i, "dof"] <- f$dof
            vss.df[i, "chisq"] <- f$STATISTIC
            vss.df[i, "prob"] <- f$PVAL
            vss.df[i, "eChisq"] <- f$chi
            vss.df[i, "SRMR"] <- f$rms
            vss.df[i, "eRMS"] <- f$rms
            vss.df[i, "eCRMS"] <- f$crms
            vss.df[i, "eBIC"] <- f$EBIC
            if (!is.null(f$RMSEA)) {
                vss.df[i, "RMSEA"] <- f$RMSEA[1]
            }
            else {
                vss.df[i, "RMSEA"] <- NA
            }
            if (!is.null(f$BIC)) {
                vss.df[i, "BIC"] <- f$BIC
            }
            else {
                vss.df[i, "BIC"] <- NA
            }
            if (!is.null(f$SABIC)) {
                vss.df[i, "SABIC"] <- f$SABIC
            }
            else {
                vss.df[i, "SABIC"] <- NA
            }
            if (!is.null(f$complexity)) {
                vss.df[i, "complex"] <- mean(f$complexity)
            }
            else {
                vss.df[i, "complex"] <- NA
            }
        }
        vss.df[i, "sqresid"] <- totalresid
        vss.df[i, "fit"] <- fit
        for (c in 1:i) {
            simpleload <- complexmat(load, c)
            model <- simpleload %*% PHI %*% t(simpleload)
            residual <- original - model
            sqresid <- residual * residual
            totalsimple <- sum(sqresid) - diagonal * sum(diag(sqresid))
            simplefit <- 1 - totalsimple/totaloriginal
            complexresid[i, c] <- totalsimple
            complexfit[i, c] <- simplefit
        }
    }
    vss.stats <- data.frame(vss.df, cfit = complexfit, cresidual = complexresid)
    if (plot) 
        VSS.plot(vss.stats, title = title)
    vss.results <- list(title = title, map = map.values, cfit.1 = complexfit[, 
        1], cfit.2 = complexfit[, 2], vss.stats = vss.stats, 
        call = cl)
    class(vss.results) <- c("psych", "vss")
    return(vss.results)
}


multi.hist <- function (x, nrow = NULL, ncol = NULL, density = TRUE, freq = FALSE, 
    bcol = "white", dcol = c("black", "black"), dlty = c("dashed", 
        "dotted"), main = "Histogram, Density, and Normal Fit", 
    ...) 
{
    if ((!is.matrix(x)) & (!is.data.frame(x))) {
        nvar <- 1
        x <- as.matrix(x, ncol = 1)
    }
    else {
        x <- as.data.frame(x)
        nvar <- dim(x)[2]
    }
    if (length(dcol) < 2) 
        dcol <- c(dcol, dcol)
    if (!density & (main == "Histogram, Density, and Normal Fit")) 
        main = "Histogram"
    nsize = ceiling(sqrt(nvar))
    if (is.null(nrow)) {
        nrow <- nsize
    }
    else {
        ncol <- nvar/nrow
    }
    if (is.null(ncol)) {
        ncol <- ceiling(nvar/nsize)
    }
    else {
        nrow <- nvar/ncol
    }
    old.par <- par(no.readonly = TRUE)
    par(mfrow = c(nrow, ncol))
    for (i in 1:nvar) {
        xlab = names(x)[i]
        if (density) {
            histo.density(x[, i], xlab = xlab, main = main, freq = freq, 
                bcol, dcol = dcol, dlty = dlty, ...)
        }
        else {
            hist(x[, i], main = main, xlab = xlab, freq = freq, 
                bcol, dcol = dcol, dlty = dlty, ...)
        }
    }
    on.exit(par(old.par))
}


super.matrix <- function (x, y) 
{
    .Deprecated("super.matrix", msg = "super.matrix is deprecated.  Please use the superMatrix function")
    xy <- superMatrix(x, y)
    return(xy)
}


winsor.means <- function (x, trim = 0.2, na.rm = TRUE) 
{
    if (is.vector(x)) {
        ans <- win.mean(x, trim = trim, na.rm = na.rm)
    }
    else {
        if (is.matrix(x) | is.data.frame(x)) {
            ans <- apply(x, 2, win.mean, trim = trim, na.rm = na.rm)
        }
    }
    return(ans)
}


minkowski <- function (r = 2, add = FALSE, main = NULL, xl = 1, yl = 1) 
{
    segments = 51
    x <- cos((0:segments) * pi/(2 * segments))
    min.circle <- cbind(x * xl, yl * ((1 - x^r)/(x^r + (1 - x^r)))^(1/r))
    if (add) {
        points(min.circle, type = "l")
    }
    else plot(min.circle, ylim = c(-1, 1), xlim = c(-1, 1), typ = "l", 
        xlab = "", ylab = "", main = main)
    points(-min.circle, typ = "l")
    points(-min.circle[, 1], min.circle[, 2], typ = "l")
    points(min.circle[, 1], -min.circle[, 2], typ = "l")
}


fa <- function (r, nfactors = 1, n.obs = NA, n.iter = 1, rotate = "oblimin", 
    scores = "regression", residuals = FALSE, SMC = TRUE, covar = FALSE, 
    missing = FALSE, impute = "median", min.err = 0.001, max.iter = 50, 
    symmetric = TRUE, warnings = TRUE, fm = "minres", alpha = 0.1, 
    p = 0.05, oblique.scores = FALSE, np.obs = NULL, use = "pairwise", 
    cor = "cor", weight = NULL, ...) 
{
    cl <- match.call()
    if (isCorrelation(r)) {
        if (is.na(n.obs) && (n.iter > 1)) 
            stop("You must specify the number of subjects if giving a correlation matrix and doing confidence intervals")
    }
    f <- fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, 
        scores = scores, residuals = residuals, SMC = SMC, covar = covar, 
        missing = missing, impute = impute, min.err = min.err, 
        max.iter = max.iter, symmetric = symmetric, warnings = warnings, 
        fm = fm, alpha = alpha, oblique.scores = oblique.scores, 
        np.obs = np.obs, use = use, cor = cor, weight = weight, 
        ...)
    fl <- f$loadings
    nvar <- dim(fl)[1]
    if (n.iter > 1) {
        if (is.na(n.obs)) {
            n.obs <- f$n.obs
        }
        replicates <- list()
        rep.rots <- list()
        replicateslist <- parallel::mclapply(1:n.iter, function(x) {
            if (isCorrelation(r)) {
                mu <- rep(0, nvar)
                eX <- eigen(r)
                X <- matrix(rnorm(nvar * n.obs), n.obs)
                X <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
                  0)), nvar) %*% t(X))
            }
            else {
                X <- r[sample(n.obs, n.obs, replace = TRUE), 
                  ]
            }
            fs <- fac(X, nfactors = nfactors, rotate = rotate, 
                scores = "none", SMC = SMC, missing = missing, 
                impute = impute, min.err = min.err, max.iter = max.iter, 
                symmetric = symmetric, warnings = warnings, fm = fm, 
                alpha = alpha, oblique.scores = oblique.scores, 
                np.obs = np.obs, use = use, cor = cor, ...)
            if (nfactors == 1) {
                replicates <- list(loadings = fs$loadings)
            }
            else {
                t.rot <- target.rot(fs$loadings, fl)
                if (!is.null(fs$Phi)) {
                  phis <- fs$Phi
                  replicates <- list(loadings = t.rot$loadings, 
                    phis = phis[lower.tri(t.rot$Phi)])
                }
                else {
                  replicates <- list(loadings = t.rot$loadings)
                }
            }
        })
        replicates <- matrix(unlist(replicateslist), nrow = n.iter, 
            byrow = TRUE)
        means <- colMeans(replicates, na.rm = TRUE)
        sds <- apply(replicates, 2, sd, na.rm = TRUE)
        if (length(means) > (nvar * nfactors)) {
            means.rot <- means[(nvar * nfactors + 1):length(means)]
            sds.rot <- sds[(nvar * nfactors + 1):length(means)]
            ci.rot.lower <- means.rot + qnorm(p/2) * sds.rot
            ci.rot.upper <- means.rot + qnorm(1 - p/2) * sds.rot
            ci.rot <- data.frame(lower = ci.rot.lower, upper = ci.rot.upper)
        }
        else {
            rep.rots <- NULL
            means.rot <- NULL
            sds.rot <- NULL
            z.rot <- NULL
            ci.rot <- NULL
        }
        means <- matrix(means[1:(nvar * nfactors)], ncol = nfactors)
        sds <- matrix(sds[1:(nvar * nfactors)], ncol = nfactors)
        tci <- abs(means)/sds
        ptci <- 1 - pnorm(tci)
        if (!is.null(rep.rots)) {
            tcirot <- abs(means.rot)/sds.rot
            ptcirot <- 1 - pnorm(tcirot)
        }
        else {
            tcirot <- NULL
            ptcirot <- NULL
        }
        ci.lower <- means + qnorm(p/2) * sds
        ci.upper <- means + qnorm(1 - p/2) * sds
        ci <- data.frame(lower = ci.lower, upper = ci.upper)
        class(means) <- "loadings"
        colnames(means) <- colnames(sds) <- colnames(fl)
        rownames(means) <- rownames(sds) <- rownames(fl)
        f$cis <- list(means = means, sds = sds, ci = ci, p = 2 * 
            ptci, means.rot = means.rot, sds.rot = sds.rot, ci.rot = ci.rot, 
            p.rot = ptcirot, Call = cl, replicates = replicates, 
            rep.rots = rep.rots)
        results <- f
        results$Call <- cl
        class(results) <- c("psych", "fa.ci")
    }
    else {
        results <- f
        results$Call <- cl
        class(results) <- c("psych", "fa")
    }
    return(results)
}


irt2latex <- function (f, digits = 2, rowlabels = TRUE, apa = TRUE, short.names = FALSE, 
    font.size = "scriptsize", heading = "An IRT factor analysis table from R", 
    caption = "fa2latex", label = "default", silent = FALSE, 
    file = NULL, append = FALSE) 
{
    if (class(f)[2] != "polyinfo") {
        nf <- length(f$plot$sumInfo)
    }
    else {
        nf <- length(f$sumInfo)
    }
    for (i in (1:nf)) {
        if (class(f)[2] != "polyinfo") {
            x <- f$plot$sumInfo[[i]]
        }
        else {
            x <- f$sumInfo[[i]]
        }
        if (nf > 1) {
            rowmax <- apply(x, 1, max, na.rm = TRUE)
            rowmax <- which(rowmax < 0.001, arr.ind = TRUE)
            if (!is.null(rowmax)) 
                x <- x[-rowmax, ]
        }
        nvar <- ncol(x)
        comment <- paste("%", match.call())
        header <- paste("\\begin{", font.size, "} \\begin{table}[htpb]", 
            "\\caption{", caption, "}\n\\begin{center}\n\\begin{tabular}", 
            sep = "")
        header <- c(header, "{l", rep("r", nvar), "}\n")
        if (apa) 
            header <- c(header, "\\multicolumn{", nvar, "}{l}{", 
                heading, " for factor ", i, " }", "\\cr  \\hline \\cr", 
                "\n & \\multicolumn{7}{c}{Item information at $\\theta$}  \\cr \\cline{2-8}  ")
        if (apa) {
            footer <- paste(" \\hline ")
        }
        footer <- paste(footer, "\n\\end{tabular}\n\\end{center}\n\\label{", 
            label, "}\n\\end{table} \n\\end{", font.size, "}\n", 
            sep = "")
        x <- round(x, digits = digits)
        cname <- colnames(x)
        if (short.names) 
            cname <- 1:nvar
        names1 <- paste(cname[1:(nvar - 1)], " & ")
        lastname <- paste(cname[nvar], "\\cr \n")
        if (apa) {
            allnames <- c("Item  &  ", names1, lastname, " \\hline \n")
        }
        else {
            allnames <- c("  &  ", names1, lastname, "\\cr \n")
        }
        x <- format(x, drop0trailing = FALSE)
        value <- apply(x, 1, paste, collapse = "  &  ")
        if (rowlabels) 
            value <- paste(sanitize.latex(names(value)), "  & ", 
                value)
        values <- paste(value, "\\cr", "\n")
        if (class(f)[2] != "polyinfo") {
            test.info <- colSums(f$plot$sumInfo[[i]])
        }
        else {
            test.info <- colSums(f$sumInfo[[i]])
        }
        sem <- sqrt(1/test.info)
        reliab <- 1 - 1/test.info
        summary <- rbind(test.info, sem, reliab)
        summary <- round(summary, digits)
        summary <- format(summary, nsmall = digits)
        summary <- cbind(c("Test.info", "SEM", "Reliability"), 
            summary)
        summary <- apply(summary, 1, paste, collapse = "  & ")
        summary <- paste(summary, "\\cr \n")
        if (!silent) {
            cat(comment, "\n")
            cat(header)
            cat(allnames)
            cat(values)
            cat("\\hline \n & \\multicolumn{7}{c}{Summary statistics at $\\theta$} \\cr \\cline{2-8}")
            cat(summary)
            cat(footer)
        }
    }
    result <- c(header, allnames, values, summary, footer)
    if (!is.null(file)) 
        write.table(result, file = file, row.names = FALSE, col.names = FALSE, 
            quote = FALSE, append = append)
    invisible(result)
}


circadian.cor <- function (angle, data = NULL, hours = TRUE, na.rm = TRUE) 
{
    if (!is.null(data)) 
        angle <- data[, angle]
    if (hours) {
        angle <- angle * 2 * pi/24
    }
    nvar <- dim(angle)[2]
    correl <- diag(nvar)
    x <- cos(angle)
    y <- sin(angle)
    mx <- colMeans(x, na.rm = na.rm)
    my <- colMeans(y, na.rm = na.rm)
    mean.angle <- sign(my) * acos((mx)/sqrt(mx^2 + my^2))
    for (i in 1:nvar) {
        for (j in 1:i) {
            covar <- sum(sin(angle[, i] - mean.angle[i]) * sin(angle[, 
                j] - mean.angle[j]), na.rm = na.rm)
            correl[i, j] <- correl[j, i] <- covar
        }
    }
    var <- diag(correl)/colSums(!is.na(angle))
    sd <- diag(sqrt(1/diag(correl)))
    correl <- sd %*% correl %*% sd
    colnames(correl) <- rownames(correl) <- colnames(angle)
    return(correl)
}


kurtosi <- function (x, na.rm = TRUE, type = 3) 
{
    if (length(dim(x)) == 0) {
        if (na.rm) {
            x <- x[!is.na(x)]
        }
        if (is.matrix(x)) {
            mx <- colMeans(x, na.rm = na.rm)
        }
        else {
            mx <- mean(x, na.rm = na.rm)
        }
        sdx <- sd(x, na.rm = na.rm)
        n <- length(x[!is.na(x)])
        switch(type, {
            kurt <- sum((x - mx)^4, na.rm = na.rm) * n/(sum((x - 
                mx)^2, na.rm = na.rm)^2) - 3
        }, {
            kurt <- n * (n + 1) * sum((x - mx)^4, na.rm = na.rm)/((n - 
                1) * (n - 2) * (n - 3) * (sum((x - mx)^2, na.rm = na.rm)/(n - 
                1))^2) - 3 * (n - 1)^2/((n - 2) * (n - 3))
        }, {
            kurt <- sum((x - mx)^4)/(n * sdx^4) - 3
        })
    }
    else {
        kurt <- rep(NA, dim(x)[2])
        mx <- apply(x, 2, mean, na.rm = na.rm)
        if (type == 3) 
            sdx <- apply(x, 2, sd, na.rm = na.rm)
        for (i in 1:dim(x)[2]) {
            n <- length(x[!is.na(x[, i]), i])
            switch(type, {
                kurt[i] <- sum((x[, i] - mx[i])^4, na.rm = na.rm) * 
                  length(x[, i])/(sum((x[, i] - mx[i])^2, na.rm = na.rm)^2) - 
                  3
            }, {
                xi <- x[, i] - mx[i]
                kurt[i] <- n * (n + 1) * sum((x[, i] - mx[i])^4, 
                  na.rm = na.rm)/((n - 1) * (n - 2) * (n - 3) * 
                  (sum((x[, i] - mx[i])^2, na.rm = na.rm)/(n - 
                    1))^2) - 3 * (n - 1)^2/((n - 2) * (n - 3))
            }, {
                kurt[i] <- sum((x[, i] - mx[i])^4, na.rm = na.rm)/((length(x[, 
                  i]) - sum(is.na(x[, i]))) * sdx[i]^4) - 3
            }, {
                NULL
            })
            names(kurt) <- colnames(x)
        }
    }
    return(kurt)
}


read.file <- function (f = NULL, header = TRUE, ...) 
{
    if (missing(f)) 
        f <- file.choose()
    suffix <- file_ext(f)
    switch(suffix, sav = {
        result <- read.spss(f, use.value.labels = FALSE, to.data.frame = TRUE, 
            ...)
        message("Data from the SPSS sav file ", f, " has been loaded.")
    }, csv = {
        result <- read.table(f, header = header, sep = ",", ...)
        message("Data from the .csv file ", f, " has been loaded.")
    }, txt = {
        result <- read.table(f, header = header, ...)
        message("Data from the .txt file ", f, " has been loaded.")
    }, text = {
        result <- read.table(f, header = header, ...)
        message("Data from the .text file ", f, " has been loaded.")
    }, data = {
        result <- read.table(f, header = header, ...)
        message("Data from the .data file ", f, " has been loaded.")
    }, dat = {
        result <- read.table(f, header = header, ...)
        message("Data from the .data file ", f, " has been loaded.")
    }, rds = {
        result <- readRDS(f, ...)
        message("File ", f, " has been loaded.")
    }, Rds = {
        result <- readRDS(f, ...)
        message("File ", f, " has been loaded.")
    }, Rda = {
        result <- f
        message("To load this .rda file, you must \nload(\"", 
            f, "\")")
    }, rda = {
        result <- f
        message("To load this .rda file, you must \nload(\"", 
            f, "\")")
    }, jmp = {
        result <- f
        message("I am sorrry.  To read this .jmp file, it must first be saved as a \"txt\" or \"csv\" file.")
    }, {
        message("I  am sorry. \nI can not tell from the suffix what file type is this.  I will try just a normal read.table but can not guarantee the result.")
        result <- read.table(f, header = header, ...)
    })
    return(result)
}


interp.median <- function (x, w = 1, na.rm = TRUE) 
{
    im <- interp.quantiles(x, q = 0.5, w, na.rm = na.rm)
    return(im)
}


bestItems <- function (x, criteria = 1, cut = 0.3, abs = TRUE, dictionary = NULL, 
    cor = TRUE, digits = 2) 
{
    if ((nrow(x) != ncol(x)) && cor) {
        x <- cor(x, use = "pairwise")
    }
    if (abs) {
        ord <- order(abs(x[, criteria]), decreasing = TRUE)
        value <- x[ord, criteria, drop = FALSE]
        count <- sum(abs(value) > cut, na.rm = TRUE)
        value <- value[1:count, , drop = FALSE]
    }
    else {
        ord <- order(x[, criteria], decreasing = TRUE)
        value <- x[ord, criteria]
        value <- value[value, criteria > cut]
    }
    value <- round(data.frame(value), digits)
    if ((!is.null(dictionary)) && !is.factor(dictionary)) {
        temp <- lookup(rownames(value), dictionary)
        value <- merge(value, temp, by = "row.names", all.x = TRUE, 
            sort = FALSE)
        rownames(value) <- value[, "Row.names"]
        value <- value[-1]
        if (abs) {
            ord <- order(abs(value[, criteria]), decreasing = TRUE)
        }
        else {
            ord <- order(value[, criteria], decreasing = TRUE)
        }
        value <- value[ord, ]
    }
    return(value)
}


print.psych <- function (x, digits = 2, all = FALSE, cut = NULL, sort = FALSE, 
    short = TRUE, lower = TRUE, signif = NULL, ...) 
{
    if (length(class(x)) > 1) {
        value <- class(x)[2]
    }
    else {
        if ((!is.null(x$communality.iterations)) | (!is.null(x$uniquenesses)) | 
            (!is.null(x$rotmat)) | (!is.null(x$Th))) {
            value <- fa
        }
    }
    if (all) 
        value <- "all"
    if (value == "score.items") 
        value <- "scores"
    if (value == "set.cor") 
        value <- "setCor"
    switch(value, esem = {
        print.psych.esem(x, digits = digits, short = short, cut = cut, 
            ...)
    }, extension = {
        print.psych.fa(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, extend = {
        print.psych.fa(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, fa = {
        print.psych.fa(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, fa.ci = {
        print.psych.fa.ci(x, digits = digits, all = all, ...)
    }, iclust = {
        print.psych.iclust(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, omega = {
        print.psych.omega(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, omegaSem = {
        print.psych.omegaSem(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, principal = {
        print.psych.fa(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, schmid = {
        print.psych.schmid(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, stats = {
        print.psych.stats(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, vss = {
        print.psych.vss(x, digits = digits, all = all, cut = cut, 
            sort = sort, ...)
    }, cta = {
        print.psych.cta(x, digits = digits, all = all, ...)
    }, mediate = {
        print.psych.mediate(x, digits = digits, short = short, 
            ...)
    }, all = {
        class(x) <- "list"
        print(x)
    }, alpha = {
        cat("\nReliability analysis ", x$title, " \n")
        cat("Call: ")
        print(x$call)
        cat("\n ")
        print(x$total, digits = digits)
        if (!is.null(x$total$ase)) {
            cat("\n lower alpha upper     95% confidence boundaries\n")
            cat(round(c(x$total$raw_alpha - 1.96 * x$total$ase, 
                x$total$raw_alpha, x$total$raw_alpha + 1.96 * 
                  x$total$ase), digits = digits), "\n")
        }
        if (!is.null(x$boot.ci)) {
            cat("\n lower median upper bootstrapped confidence intervals\n", 
                round(x$boot.ci, digits = digits))
        }
        cat("\n Reliability if an item is dropped:\n")
        print(x$alpha.drop, digits = digits)
        cat("\n Item statistics \n")
        print(x$item.stats, digits = digits)
        if (!is.null(x$response.freq)) {
            cat("\nNon missing response frequency for each item\n")
            print(round(x$response.freq, digits = digits))
        }
    }, bestScales = {
        df <- data.frame(correlation = x$r, n.items = x$n.items)
        cat("The items most correlated with the criteria yield r's of \n")
        print(round(df, digits = digits))
        if (length(x$value) > 0) {
            cat("\nThe best items, their correlations and content  are \n")
            print(x$value)
        } else {
            cat("\nThe best items and their correlations are \n")
            for (i in 1:length(x$short.key)) {
                print(round(x$short.key[[i]], digits = digits))
            }
        }
    }, bifactor = {
        cat("Call: ")
        print(x$Call)
        cat("Alpha:                ", round(x$alpha, digits), 
            "\n")
        cat("G.6:                  ", round(x$G6, digits), "\n")
        cat("Omega Hierarchical:   ", round(x$omega_h, digits), 
            "\n")
        cat("Omega Total           ", round(x$omega.tot, digits), 
            "\n")
        print(x$f, digits = digits, sort = sort)
    }, circ = {
        cat("Tests of circumplex structure \n")
        cat("Call:")
        print(x$Call)
        res <- data.frame(x[1:4])
        print(res, digits = 2)
    }, circadian = {
        if (!is.null(x$Call)) {
            cat("Call: ")
            print(x$Call)
        }
        cat("\nCircadian Statistics :\n")
        if (!is.null(x$F)) {
            cat("\nCircadian F test comparing groups :\n")
            print(round(x$F, digits))
            if (short) cat("\n To see the pooled and group statistics, print with the short=FALSE option")
        }
        if (!is.null(x$pooled) && !short) {
            cat("\nThe pooled circadian statistics :\n")
            print(x$pooled)
        }
        if (!is.null(x$bygroup) && !short) {
            cat("\nThe  circadian statistics by group:\n")
            print(x$bygroup)
        }
        if (!is.null(x$phase.rel)) {
            cat("\nSplit half reliabilities are split half correlations adjusted for test length\n")
            x.df <- data.frame(phase = x$phase.rel, fits = x$fit.rel)
            print(round(x.df, digits))
        }
        if (is.data.frame(x)) {
            class(x) <- "data.frame"
            print(round(x, digits = digits))
        }
    }, cluster.cor = {
        cat("Call: ")
        print(x$Call)
        cat("\n(Standardized) Alpha:\n")
        print(x$alpha, digits)
        cat("\n(Standardized) G6*:\n")
        print(x$G6, digits)
        cat("\nAverage item correlation:\n")
        print(x$av.r, digits)
        cat("\nNumber of items:\n")
        print(x$size)
        cat("\nSignal to Noise ratio based upon average r and n \n")
        print(x$sn, digits = digits)
        cat("\nScale intercorrelations corrected for attenuation \n raw correlations below the diagonal, alpha on the diagonal \n corrected correlations above the diagonal:\n")
        print(x$corrected, digits)
    }, cluster.loadings = {
        cat("Call: ")
        print(x$Call)
        cat("\n(Standardized) Alpha:\n")
        print(x$alpha, digits)
        cat("\n(Standardized) G6*:\n")
        print(x$G6, digits)
        cat("\nAverage item correlation:\n")
        print(x$av.r, digits)
        cat("\nNumber of items:\n")
        print(x$size)
        cat("\nScale intercorrelations corrected for attenuation \n raw correlations below the diagonal, alpha on the diagonal \n corrected correlations above the diagonal:\n")
        print(x$corrected, digits)
        cat("\nItem by scale intercorrelations\n corrected for item overlap and scale reliability\n")
        print(x$loadings, digits)
    }, comorbid = {
        cat("Call: ")
        print(x$Call)
        cat("Comorbidity table \n")
        print(x$twobytwo, digits = digits)
        cat("\nimplies phi = ", round(x$phi, digits), " with Yule = ", 
            round(x$Yule, digits), " and tetrachoric correlation of ", 
            round(x$tetra$rho, digits))
        cat("\nand normal thresholds of ", round(-x$tetra$tau, 
            digits))
    }, cor.ci = {
        cat("Call:")
        print(x$Call)
        cat("\n Coefficients and bootstrapped confidence intervals \n")
        lowerMat(x$rho)
        phis <- x$rho[lower.tri(x$rho)]
        cci <- data.frame(lower.emp = x$ci$low.e, lower.norm = x$ci$lower, 
            estimate = phis, upper.norm = x$ci$upper, upper.emp = x$ci$up.e, 
            p = x$ci$p)
        rownames(cci) <- rownames(x$ci)
        cat("\n scale correlations and bootstrapped confidence intervals \n")
        print(round(cci, digits = digits))
    }, cor.cip = {
        class(x) <- NULL
        cat("\n High and low confidence intervals \n")
        print(round(x, digits = digits))
    }, corr.test = {
        cat("Call:")
        print(x$Call)
        cat("Correlation matrix \n")
        print(round(x$r, digits))
        cat("Sample Size \n")
        print(x$n)
        if (x$sym) {
            cat("Probability values (Entries above the diagonal are adjusted for multiple tests.) \n")
        } else {
            if (x$adjust != "none") {
                cat("Probability values  adjusted for multiple tests. \n")
            }
        }
        print(round(x$p, digits))
        cat("\n To see confidence intervals of the correlations, print with the short=FALSE option\n")
        if (!short) {
            cat("\n Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci\n")
            print(round(x$ci, digits))
        }
    }, corr.p = {
        cat("Call:")
        print(x$Call)
        cat("Correlation matrix \n")
        print(round(x$r, digits))
        cat("Sample Size \n")
        print(x$n)
        if (x$sym) {
            cat("Probability values (Entries above the diagonal are adjusted for multiple tests.) \n")
        } else {
            if (x$adjust != "none") {
                cat("Probability values  adjusted for multiple tests. \n")
            }
        }
        print(round(x$p, digits))
        cat("\n To see confidence intervals of the correlations, print with the short=FALSE option\n")
        if (!short) {
            cat("\n Confidence intervals based upon normal theory.  To get bootstrapped values, try cor.ci\n")
            print(round(x$ci, digits))
        }
    }, cortest = {
        cat("Tests of correlation matrices \n")
        cat("Call:")
        print(x$Call)
        cat(" Chi Square value", round(x$chi, digits), " with df = ", 
            x$df, "  with probability <", signif(x$p, digits), 
            "\n")
        if (!is.null(x$z)) cat("z of differences = ", round(x$z, 
            digits), "\n")
    }, cor.wt = {
        cat("Weighted Correlations \n")
        cat("Call:")
        print(x$Call)
        lowerMat(x$r, digits = digits)
    }, describe = {
        if (!is.null(x$signif)) {
            if (missing(signif)) signif <- x$signif
            x$signif <- NULL
        }
        if (length(dim(x)) == 1) {
            class(x) <- "list"
            attr(x, "call") <- NULL
            if (!missing(signif)) x <- signifNum(x, digits = signif)
            print(round(x, digits = digits))
        } else {
            class(x) <- "data.frame"
            if (!missing(signif)) x <- signifNum(x, digits = signif)
            print(round(x, digits = digits))
        }
    }, describeBy = {
        print(unclass(x), digits = digits)
    }, describeData = {
        if (length(dim(x)) == 1) {
            class(x) <- "list"
            attr(x, "call") <- NULL
            print(round(x, digits = digits))
        } else {
            cat("n.obs = ", x$n.obs, "of which ", x$complete.cases, 
                "  are complete cases.   Number of variables = ", 
                x$nvar, " of which all are numeric ", x$all.numeric, 
                " \n")
            print(x$variables)
        }
    }, faBy = {
        cat("Call: ")
        print(x$Call)
        cat("\n Factor analysis by Groups\n")
        cat("\nAverage standardized loadings (pattern matrix) based upon correlation matrix for all cases as well as each group\n")
        cat("\nlow and high ", x$quant, "% quantiles\n")
        print(x$faby.sum, digits)
        if (!short) {
            print(x$mean.loading, digits = digits)
            cat("\n Average factor intercorrelations for all cases and  each group\n")
            print(x$mean.Phi, digits = 2)
            cat("\nStandardized loadings (pattern matrix) based upon correlation matrix for all cases as well as each group\n")
            print(x$loadings, digits = digits)
            cat("\n With factor intercorrelations for all cases and for each group\n")
            print(x$Phi, digits = 2)
            if (!is.null(x$fa)) {
                cat("\nFactor analysis results for each group\n")
                print(x$faby.sum, digits)
            }
        }
    }, guttman = {
        cat("Call: ")
        print(x$Call)
        cat("\nAlternative estimates of reliability\n")
        cat("\nGuttman bounds \nL1 = ", round(x$lambda.1, digits), 
            "\nL2 = ", round(x$lambda.2, digits), "\nL3 (alpha) = ", 
            round(x$lambda.3, digits), "\nL4 (max) = ", round(x$lambda.4, 
                digits), "\nL5 = ", round(x$lambda.5, digits), 
            "\nL6 (smc) = ", round(x$lambda.6, digits), "\n")
        cat("TenBerge bounds \nmu0 = ", round(x$tenberge$mu0, 
            digits), "mu1 = ", round(x$tenberge$mu1, digits), 
            "mu2 = ", round(x$tenberge$mu2, digits), "mu3 = ", 
            round(x$tenberge$mu3, digits), "\n")
        cat("\nalpha of first PC = ", round(x$alpha.pc, digits), 
            "\nestimated greatest lower bound based upon communalities= ", 
            round(x$glb, digits), "\n")
        cat("\nbeta found by splitHalf  = ", round(x$beta, digits), 
            "\n")
    }, ICC = {
        cat("Call: ")
        print(x$Call)
        cat("\nIntraclass correlation coefficients \n")
        print(x$results, digits = digits)
        cat("\n Number of subjects =", x$n.obs, "    Number of Judges = ", 
            x$n.judge)
    }, iclust.sort = {
        nvar <- ncol(x$sort)
        x$sort[4:nvar] <- round(x$sort[4:nvar], digits)
        print(x$sort)
    }, irt.fa = {
        cat("Item Response Analysis using Factor Analysis \n")
        cat("\nCall: ")
        print(x$Call)
        if (!is.null(x$plot)) print(x$plot)
        if (!short) {
            nf <- length(x$irt$difficulty)
            for (i in 1:nf) {
                temp <- data.frame(discrimination = x$irt$discrimination[, 
                  i], location = x$irt$difficulty[[i]])
                cat("\nItem discrimination and location for factor ", 
                  colnames(x$irt$discrimination)[i], "\n")
                print(round(temp, digits))
            }
            cat("\n These parameters were based on the following factor analysis\n")
            print(x$fa)
        } else {
            summary(x$fa)
        }
    }, irt.poly = {
        cat("Item Response Analysis using Factor Analysis  \n")
        cat("\nCall: ")
        print(x$Call)
        if (!is.null(x$plot)) print(x$plot)
        if (!short) {
            nf <- length(x$irt$difficulty)
            for (i in 1:nf) {
                temp <- data.frame(discrimination = x$irt$discrimination[, 
                  i], location = x$irt$difficulty[[i]])
                cat("\nItem discrimination and location for factor ", 
                  colnames(x$irt$discrimination)[i], "\n")
                print(round(temp, digits))
            }
            cat("\n These parameters were based on the following factor analysis\n")
            print(x$fa)
        } else {
            summary(x$fa)
        }
    }, kappa = {
        if (is.null(x$cohen.kappa)) {
            cat("Call: ")
            print(x$Call)
            cat("\nCohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries \n")
            print(x$confid, digits = digits)
            cat("\n Number of subjects =", x$n.obs, "\n")
        } else {
            cat("\nCohen Kappa (below the diagonal) and Weighted Kappa (above the diagonal) \nFor confidence intervals and detail print with all=TRUE\n")
            print(x$cohen.kappa, digits)
        }
    }, mardia = {
        cat("Call: ")
        print(x$Call)
        cat("\nMardia tests of multivariate skew and kurtosis\n")
        cat("Use describe(x) the to get univariate tests")
        cat("\nn.obs =", x$n.obs, "  num.vars = ", x$n.var, "\n")
        cat("b1p = ", round(x$b1p, digits), "  skew = ", round(x$skew, 
            digits), " with probability = ", signif(x$p.skew, 
            digits))
        cat("\n small sample skew = ", round(x$small.skew, digits), 
            " with probability = ", signif(x$p.small, digits))
        cat("\nb2p = ", round(x$b2p, digits), "  kurtosis = ", 
            round(x$kurtosis, digits), " with probability = ", 
            signif(x$p.kurt, digits))
    }, mchoice = {
        cat("Call: ")
        print(x$Call)
        cat("\n(Unstandardized) Alpha:\n")
        print(x$alpha, digits = digits)
        cat("\nAverage item correlation:\n")
        print(x$av.r, digits = digits)
        if (!is.null(x$item.stats)) {
            cat("\nitem statistics \n")
            print(round(x$item.stats, digits = digits))
        }
    }, mixed = {
        cat("Call: ")
        print(x$Call)
        if (is.null(x$rho)) {
            if (lower) {
                lowerMat(x, digits = digits)
            } else {
                print(x, digits)
            }
        } else {
            if (lower) {
                if (length(x$rho) > 1) {
                  lowerMat(x$rho, digits = digits)
                } else {
                  print(x$rho, digits)
                }
            }
        }
    }, paired.r = {
        cat("Call: ")
        print(x$Call)
        print(x$test)
        if (is.null(x$z)) {
            cat("t =", round(x$t, digits))
        } else {
            cat("z =", round(x$z, digits))
        }
        cat("  With probability = ", round(x$p, digits))
    }, parallel = {
        cat("Call: ")
        print(x$Call)
        if (!is.null(x$fa.values) & !is.null(x$pc.values)) {
            parallel.df <- data.frame(fa = x$fa.values, fa.sim = x$fa.sim, 
                pc = x$pc.values, pc.sim = x$pc.sim)
            fa.test <- x$nfact
            pc.test <- x$ncomp
            cat("Parallel analysis suggests that ")
            cat("the number of factors = ", fa.test, " and the number of components = ", 
                pc.test, "\n")
            cat("\n Eigen Values of \n")
            colnames(parallel.df) <- c("Original factors", "Simulated data", 
                "Original components", "simulated data")
        }
        if (is.na(fa.test)) fa.test <- 0
        if (is.na(pc.test)) pc.test <- 0
        if (!any(is.na(parallel.df))) {
            print(round(parallel.df[1:max(fa.test, pc.test), 
                ], digits))
        } else {
            if (!is.null(x$fa.values)) {
                cat("\n eigen values of factors\n")
                print(round(x$fa.values, digits))
            }
            if (!is.null(x$fa.sim)) {
                cat("\n eigen values of simulated factors\n")
                print(round(x$fa.sim, digits))
            }
            if (!is.null(x$pc.values)) {
                cat("\n eigen values of components \n")
                print(round(x$pc.values, digits))
            }
            if (!is.null(x$pc.sim)) {
                cat("\n eigen values of simulated components\n")
                print(round(x$pc.sim, digits = digits))
            }
        }
    }, partial.r = {
        cat("partial correlations \n")
        print(round(unclass(x), digits))
    }, phi.demo = {
        print(x$tetrachoric)
        cat("\nPearson (phi) below the diagonal, phi2tetras above the diagonal\n")
        print(round(x$phis, digits))
        cat("\nYule correlations")
        print(x$Yule)
    }, poly = {
        cat("Call: ")
        print(x$Call)
        cat("Polychoric correlations \n")
        if (!is.null(x$twobytwo)) {
            print(x$twobytwo, digits = digits)
            cat("\n implies tetrachoric correlation of ", round(-x$rho, 
                digits))
        } else {
            if (lower) {
                lowerMat(x$rho, digits)
            } else {
                print(x$rho, digits)
            }
            cat("\n with tau of \n")
            print(x$tau, digits)
        }
    }, polydi = {
        cat("Call: ")
        print(x$Call)
        cat("Correlations of polytomous with dichotomous\n")
        print(x$rho, digits)
        cat("\n with tau of \n")
        print(x$tau, digits)
    }, polyinfo = {
        cat("Item Response Analysis using Factor Analysis  \n")
        cat("\n Summary information by factor and item")
        names(x$sumInfo) <- paste("Factor", 1:length(x$sumInfo))
        for (f in 1:length(x$sumInfo)) {
            cat("\n Factor = ", f, "\n")
            temp <- x$sumInfo[[f]]
            temps <- rowSums(temp)
            if (sort) {
                ord <- order(temps, decreasing = TRUE)
                temp <- temp[ord, ]
                temps <- temps[ord]
            }
            temp <- temp[temps > 0, ]
            summary <- matrix(c(colSums(temp), sqrt(1/colSums(temp)), 
                1 - 1/colSums(temp)), nrow = 3, byrow = TRUE)
            rownames(summary) <- c("Test Info", "SEM", "Reliability")
            temp <- rbind(temp, summary)
            if (ncol(temp) == 61) {
                print(round(temp[, seq(1, 61, 10)], digits = digits))
            } else {
                print(round(temp, digits = digits))
            }
        }
        if (!short) {
            cat("\n Average information (area under the curve) \n")
            AUC <- x$AUC
            max.info <- x$max.info
            if (dim(AUC)[2] == 1) {
                item <- 1:length(AUC)
            } else {
                item <- 1:dim(AUC)[1]
            }
            if (sort) {
                cluster <- apply(AUC, 1, which.max)
                ord <- sort(cluster, index.return = TRUE)
                AUC <- AUC[ord$ix, , drop = FALSE]
                max.info <- max.info[ord$ix, , drop = FALSE]
                items <- table(cluster)
                first <- 1
                for (i in 1:length(items)) {
                  if (items[i] > 0) {
                    last <- first + items[i] - 1
                    ord <- sort(abs(AUC[first:last, i]), decreasing = TRUE, 
                      index.return = TRUE)
                    AUC[first:last, ] <- AUC[item[ord$ix + first - 
                      1], ]
                    max.info[first:last, ] <- max.info[item[ord$ix + 
                      first - 1], ]
                    rownames(AUC)[first:last] <- rownames(max.info)[first:last] <- rownames(AUC)[ord$ix + 
                      first - 1]
                    first <- first + items[i]
                  }
                }
            }
            print(AUC, digits = digits)
            cat("\nMaximum value is at \n")
            print(max.info, digits = digits)
        }
    }, overlap = {
        cat("Call: ")
        print(x$Call)
        cat("\n(Standardized) Alpha:\n")
        print(x$alpha, digits)
        cat("\n(Standardized) G6*:\n")
        print(x$G6, digits)
        cat("\nAverage item correlation:\n")
        print(x$av.r, digits)
        cat("\nNumber of items:\n")
        print(x$size)
        cat("\nSignal to Noise ratio based upon average r and n \n")
        print(x$sn, digits = digits)
        cat("\nScale intercorrelations corrected for item overlap and attenuation \n adjusted for overlap correlations below the diagonal, alpha on the diagonal \n corrected correlations above the diagonal:\n")
        print(x$corrected, digits)
        if (short) {
            cat("\n In order to see the item by scale loadings and frequency counts of the data\n print with the short option = FALSE")
        } else {
            if (!is.null(x$item.cor)) {
                cat("\nItem by scale correlations:\n corrected for item overlap and scale reliability\n")
                print(round(x$item.cor, digits = digits))
            }
        }
    }, r.test = {
        cat("Correlation tests \n")
        cat("Call:")
        print(x$Call)
        cat(x$Test, "\n")
        if (!is.null(x$t)) {
            cat(" t value", round(x$t, digits), "   with probability <", 
                signif(x$p, digits))
        }
        if (!is.null(x$z)) {
            cat(" z value", round(x$z, digits), "   with probability ", 
                round(x$p, digits))
        }
        if (!is.null(x$ci)) {
            cat("\n and confidence interval ", round(x$ci, digits))
        }
    }, residuals = {
        if (lower) {
            lowerMat(x, digits = digits)
        } else {
            print(x, digits)
        }
    }, scree = {
        cat("Scree of eigen values \nCall: ")
        print(x$Call)
        if (!is.null(x$fv)) {
            cat("Eigen values of factors ")
            print(round(x$fv, digits))
        }
        if (!is.null(x$pcv)) {
            cat("Eigen values of Principal Components")
            print(round(x$pcv, digits))
        }
    }, scores = {
        cat("Call: ")
        print(x$Call)
        if (x$raw) {
            cat("\n(Unstandardized) Alpha:\n")
        } else {
            cat("\n(Standardized) Alpha:\n")
        }
        print(x$alpha, digits = digits)
        if (!is.null(x$ase)) {
            cat("\nStandard errors of unstandardized Alpha:\n")
            rownames(x$ase) <- "ASE  "
            print(x$ase, digit = digits)
        }
        if (!is.null(x$alpha.ob)) {
            cat("\nStandardized Alpha of observed scales:\n")
            print(x$alpha.ob, digits = digits)
        }
        cat("\nAverage item correlation:\n")
        print(x$av.r, digits = digits)
        cat("\n Guttman 6* reliability: \n")
        print(x$G6, digits = digits)
        cat("\nSignal/Noise based upon av.r : \n")
        print(x$sn, digits = digits)
        cat("\nScale intercorrelations corrected for attenuation \n raw correlations below the diagonal, alpha on the diagonal \n corrected correlations above the diagonal:\n")
        if (!is.null(x$alpha.ob)) {
            cat("\nNote that these are the correlations of the complete scales based on the correlation matrix,\n not the observed scales based on the raw items.\n")
        }
        print(x$corrected, digits)
        if (short) {
            cat("\n In order to see the item by scale loadings and frequency counts of the data\n print with the short option = FALSE")
        } else {
            if (!is.null(x$item.cor)) {
                cat("\nItem by scale correlations:\n corrected for item overlap and scale reliability\n")
                print(round(x$item.corrected, digits = digits))
            }
            if (!is.null(x$response.freq)) {
                cat("\nNon missing response frequency for each item\n")
                print(round(x$response.freq, digits = digits))
            }
        }
    }, setCor = {
        cat("Call: ")
        print(x$Call)
        if (x$raw) {
            cat("\nMultiple Regression from raw data \n")
        } else {
            cat("\nMultiple Regression from matrix input \n")
        }
        cat("\nBeta weights \n")
        print(round(x$beta, digits))
        cat("\nMultiple R \n")
        print(round(x$R, digits))
        cat("multiple R2 \n")
        print(x$R2, digits)
        cat("\n Unweighted multiple R \n")
        print(round(x$ruw, digits))
        cat(" Unweighted multiple R2 \n")
        print(round(x$ruw^2, digits))
        if (!is.null(x$se)) {
            cat("\n SE of Beta weights \n")
            print(round(x$se, digits))
            cat("\n t of Beta Weights \n")
            print(round(x$t, digits))
            cat("\nProbability of t < \n")
            print(signif(x$Probability, digits))
            cat("\n Shrunken R2 \n")
            print(x$shrunkenR2, digits)
            cat("\nStandard Error of R2  \n")
            print(x$seR2, digits)
            cat("\nF \n")
            print(round(x$F, digits))
            cat("\nProbability of F < \n")
            print(signif(x$probF, digits + 1))
            cat("\n degrees of freedom of regression \n")
            print(x$df)
        }
        if (!is.null(x$cancor)) {
            cat("\nVarious estimates of between set correlations\n")
            cat("Squared Canonical Correlations \n")
            print(x$cancor2, digits = digits)
            if (!is.null(x$Chisq)) {
                cat("Chisq of canonical correlations \n")
                print(x$Chisq, digits = digits)
            }
            cat("\n Average squared canonical correlation = ", 
                round(x$T, digits = digits))
            cat("\n Cohen's Set Correlation R2 = ", round(x$Rset, 
                digits = digits))
            if (!is.null(x$Rset.shrunk)) {
                cat("\n Shrunken Set Correlation R2 = ", round(x$Rset.shrunk, 
                  digits = digits))
                cat("\n F and df of Cohen's Set Correlation ", 
                  round(c(x$Rset.F, x$Rsetu, x$Rsetv), digits = digits))
            }
            cat("\nUnweighted correlation between the two sets = ", 
                round(x$Ruw, digits))
        }
    }, sim = {
        if (is.matrix(x)) {
            x <- unclass(x)
            round(x, digits)
        } else {
            cat("Call: ")
            print(x$Call)
            cat("\n $model (Population correlation matrix) \n")
            print(x$model, digits)
            if (!is.null(x$reliability)) {
                cat("\n$reliability (population reliability) \n")
                print(x$reliability, digits)
            }
            if (!is.null(x$N) && !is.null(x$r)) {
                cat("\n$r  (Sample correlation matrix  for sample size = ", 
                  x$N, ")\n")
                print(x$r, digits)
            }
        }
    }, smoother = {
        x <- unclass(x)
        print(x)
    }, split = {
        cat("Split half reliabilities  ")
        cat("\nCall: ")
        print(x$Call)
        cat("\nMaximum split half reliability (lambda 4) = ", 
            round(x$maxrb, digits = digits))
        cat("\nGuttman lambda 6                          = ", 
            round(x$lambda6, digits = digits))
        cat("\nAverage split half reliability            = ", 
            round(x$meanr, digits = digits))
        cat("\nGuttman lambda 3 (alpha)                  = ", 
            round(x$alpha, digits = digits))
        cat("\nMinimum split half reliability  (beta)    = ", 
            round(x$minrb, digits = digits))
    }, statsBy = {
        cat("Statistics within and between groups  ")
        cat("\nCall: ")
        print(x$Call)
        cat("Intraclass Correlation 1 (Percentage of variance due to groups) \n")
        print(round(x$ICC1, digits))
        cat("Intraclass Correlation 2 (Reliability of group differences) \n")
        print(round(x$ICC2, digits))
        cat("eta^2 between groups  \n")
        print(round(x$etabg^2, digits))
        if (!short) {
            cat("Correlation between groups \n")
            lowerMat(x$rbg)
            cat("Correlation within groups \n")
            lowerMat(x$rwg)
        }
    }, tau = {
        cat("Tau values from dichotomous or polytomous data \n")
        class(x) <- NULL
        print(x, digits)
    }, tetra = {
        cat("Call: ")
        print(x$Call)
        cat("tetrachoric correlation \n")
        if (!is.null(x$twobytwo)) {
            print(x$twobytwo, digits = digits)
            cat("\n implies tetrachoric correlation of ", round(x$rho, 
                digits))
        } else {
            if (is.matrix(x$rho) && lower) {
                lowerMat(x$rho, digits)
            } else {
                print(x$rho, digits)
            }
            cat("\n with tau of \n")
            print(x$tau, digits)
        }
    }, thurstone = {
        cat("Thurstonian scale (case 5) scale values ")
        cat("\nCall: ")
        print(x$Call)
        print(x$scale)
        cat("\n Goodness of fit of model  ", round(x$GF, digits))
    }, KMO = {
        cat("Kaiser-Meyer-Olkin factor adequacy")
        cat("\nCall: ")
        print(x$Call)
        cat("Overall MSA = ", round(x$MSA, digits))
        cat("\nMSA for each item = \n")
        print(round(x$MSAi, digits))
    }, yule = {
        cat("Yule and Generalized Yule coefficients")
        cat("\nCall: ")
        print(x$Call)
        cat("\nYule coefficient \n")
        print(round(x$rho, digits))
        cat("\nUpper and Lower Confidence Intervals = \n")
        print(round(x$ci, digits))
    }, Yule = {
        cat("Yule and Generalized Yule coefficients")
        cat("\nLower CI  Yule coefficient Upper CI \n")
        print(round(c(x$lower, x$rho, x$upper), digits))
    })
}


fa.multi <- function (r, nfactors = 3, nfact2 = 1, n.obs = NA, n.iter = 1, 
    rotate = "oblimin", scores = "regression", residuals = FALSE, 
    SMC = TRUE, covar = FALSE, missing = FALSE, impute = "median", 
    min.err = 0.001, max.iter = 50, symmetric = TRUE, warnings = TRUE, 
    fm = "minres", alpha = 0.1, p = 0.05, oblique.scores = FALSE, 
    np.obs = NULL, use = "pairwise", cor = "cor", ...) 
{
    cl <- match.call()
    if (nfactors < 2) 
        stop("number of lower level factors must be at least 2")
    f1 <- fa(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, 
        scores = scores, residuals = residuals, SMC = SMC, covar = covar, 
        missing = missing, impute = impute, min.err = min.err, 
        max.iter = max.iter, symmetric = symmetric, warnings = warnings, 
        fm = fm, alpha = alpha, oblique.scores = oblique.scores, 
        np.obs = np.obs, use = use, cor = cor, ...)
    f2 <- fa(f1$Phi, nfactors = nfact2, rotate = rotate)
    result <- list(f1 = f1, f2 = f2)
    return(result)
}


sim.omega <- function (nvar = 12, nfact = 3, n = 500, g = NULL, sem = FALSE, 
    fbig = NULL, fsmall = c(-0.2, 0.2), bipolar = TRUE, om.fact = 3, 
    flip = TRUE, option = "equal", ntrials = 10) 
{
    results <- matrix(NaN, nrow = ntrials, ncol = 12)
    colnames(results) <- c("n", "om.model", "omega", "ev.N", 
        "e.f1", "omega.f1", "Beta", "omegaCFA", "omegaSem", "rms", 
        "RMSEA", "coeff.v")
    for (i in 1:ntrials) {
        x <- try(sim.minor(nvar = nvar, nfact = nfact, n = n, 
            g = g, fbig = fbig, fsmall = fsmall, bipolar = bipolar))
        if (is.null(g)) {
            omega.model <- 0
        }
        else {
            gsum <- colSums(x$fload)[1]
            omega.model <- gsum^2/sum(x$model)
        }
        results[i, "om.model"] <- omega.model
        observed.cor <- cor(x$observed)
        ev <- eigen(observed.cor)$values
        f1 <- fa(observed.cor)$loadings
        om.fa <- sum(f1)^2/sum(observed.cor)
        e.f1 <- sum(f1^2)/nvar
        sem.model <- omega.sem(x$fload, sl = TRUE, nf = nfact)
        if (sem) {
            if (!requireNamespace("sem")) {
                stop("You must have the sem package installed to use omegaSem")
            }
            else {
                sem.om <- try(sem(model = sem.model, S = observed.cor, 
                  N = n))
            }
            omega.cfa <- omegaFromSem(observed.cor, sem.om, flip = flip)
            if (omega.cfa$omega > 1) 
                omega.cfa$omega <- NA
            results[i, "omegaCFA"] <- omega.cfa$omega
        }
        else {
            omega.cfa <- NULL
        }
        results[i, "n"] <- n
        if (n > 0) {
            if (sem) {
                om <- try(omegaSem(x$observed, om.fact, flip = flip, 
                  plot = FALSE, option = option))
            }
            else {
                om <- try(omega(x$observed, om.fact, flip = flip, 
                  plot = FALSE, option = option))
            }
            ic <- suppressWarnings(ICLUST(x$observed, 1, plot = FALSE))
        }
        else {
            if (sem) {
                om <- try(omegaSem(x$model, om.fact, flip = flip, 
                  plot = FALSE, option = option))
            }
            else {
                om <- try(omega(x$model, om.fact, flip = flip, 
                  plot = FALSE, option = option))
                if (class(om) == "try-error") {
                  message("Error in sem. iteration = ", i)
                  om <- NA
                  next
                }
            }
            ic <- suppressWarnings(ICLUST(x$model, 1, plot = FALSE))
        }
        results
        if (sem) {
            results[i, "omega"] <- om$omegaSem$omega_h
            loads <- om$omegaSem$schmid$sl
        }
        else {
            results[i, "omega"] <- om$omega_h
            loads <- om$schmid$sl
        }
        p2 <- loads[, ncol(loads)]
        mp2 <- mean(p2)
        vp2 <- var(p2)
        results[i, "coeff.v"] <- sqrt(vp2)/mp2
        results[i, "Beta"] <- ic$beta
        results[i, "ev.N"] <- ev[1]/nvar
        results[i, "e.f1"] <- e.f1
        results[i, "omega.f1"] <- om.fa
        if (sem) {
            if (!is.null(om$omegaSem$schmid$RMSEA)) {
                results[i, "RMSEA"] <- om$omegaSem$schmid$RMSEA[1]
            }
            else {
                results[i, "RMSEA"] <- NA
            }
            if (!is.null(om$omegaSem$schmid$rms)) 
                results[i, "rms"] <- om$omegaSem$schmid$rms
            results[i, "omegaSem"] <- om$omega.efa$omega
            if (results[i, "omegaSem"] > 1) {
                warning("Bad result from sem   case = ", i)
                results[i, "omegaSem"] <- NA
            }
        }
        else {
            if (!is.null(om$schmid$RMSEA)) {
                results[i, "RMSEA"] <- om$schmid$RMSEA[1]
            }
            else {
                results[i, "RMSEA"] <- NA
            }
            if (!is.null(om$schmid$rms)) 
                results[i, "rms"] <- om$schmid$rms
            results[i, "omegaSem"] <- NA
        }
    }
    if (n == 0) {
        results <- results[, -which(colnames(results) == "RMSEA")]
        if (!sem) 
            results <- results[, -which(colnames(results) == 
                "omegaSem")]
    }
    else {
        if (!sem) 
            results <- results[, -which(colnames(results) == 
                "omegaSem")]
    }
    return(results)
}


interp.q <- function (x, q = 0.5, w = 1, na.rm = TRUE) 
{
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    n <- length(x)
    n2 <- (n + 1) * q
    o <- order(x)
    x <- x[o]
    ml <- x[floor(n2)]
    mh <- x[ceiling(n2)]
    m <- (mh + ml)/2
    xb <- sum(x < m)
    xa <- sum(x > m)
    am <- n - xa - xb
    if (am > 1) {
        im <- m - 0.5 * w + w * (n * q - xb)/am
    }
    else {
        im <- m
    }
    return(im)
}


vgQ.targetQ <- function (L, Target = NULL) 
{
    if (is.null(Target)) 
        stop("argument Target must be specified.")
    Gq <- 2 * (L - Target)
    Gq[is.na(Gq)] <- 0
    list(Gq = Gq, f = sum((L - Target)^2, na.rm = TRUE), Method = "Target rotation")
}


SD <- function (x, na.rm = TRUE) 
{
    if (is.matrix(x)) 
        apply(x, 2, SD, na.rm = na.rm)
    else if (is.vector(x)) 
        sqrt(var(x, na.rm = na.rm, use = "pair"))
    else if (is.data.frame(x)) 
        apply(x, 2, SD, na.rm = na.rm)
    else sqrt(var(as.vector(x), na.rm = na.rm, use = "pair"))
}


omegah <- function (m, nfactors = 3, fm = "minres", key = NULL, flip = TRUE, 
    digits = 2, title = "Omega", sl = TRUE, labels = NULL, plot = TRUE, 
    n.obs = NA, rotate = "oblimin", Phi = NULL, option = "equal", 
    covar = FALSE, ...) 
{
    if (!requireNamespace("GPArotation") && (rotate != "cluster")) {
        stop("I am sorry, you need to have the  GPArotation package installed")
    }
    cl <- match.call()
    nvar <- dim(m)[2]
    raw.data <- NULL
    if (is.null(Phi)) {
        if (!isCorrelation(m)) {
            n.obs <- dim(m)[1]
            m <- as.matrix(m)
            raw.data <- m
            if (covar) {
                m <- cov(m, use = "pairwise")
            }
            else {
                m <- cor(m, use = "pairwise")
            }
        }
        else {
            if (!covar) 
                m <- cov2cor(as.matrix(m))
        }
        if (is.null(colnames(m))) {
            rownames(m) <- colnames(m) <- paste("V", 1:nvar, 
                sep = "")
        }
        m.names <- colnames(m)
        if (!is.null(key)) {
            m <- diag(key) %*% m %*% diag(key)
            colnames(m) <- m.names
            flip <- FALSE
        }
        else {
            key <- rep(1, nvar)
        }
        signkey <- strtrim(key, 1)
        signkey[signkey == "1"] <- ""
        m.names <- paste(m.names, signkey, sep = "")
        colnames(m) <- rownames(m) <- m.names
        if ((nvar < 6) && (fm == "mle")) {
            message(paste("In omega, 3 factors are too many for ", 
                nvar, " variables using mle.  Using minres instead", 
                sep = ""))
            fm <- "minres"
        }
    }
    else {
        m.names <- rownames(m)
    }
    gf <- schmid(m, nfactors, fm, digits, rotate = rotate, n.obs = n.obs, 
        Phi = Phi, option = option, covar = covar, ...)
    if (!is.null(Phi)) {
        model <- m
        nfactors <- dim(model)[2]
        m <- factor.model(model, Phi = Phi, U2 = FALSE)
        nvar <- dim(m)[2]
        if (is.null(rownames(m))) {
            colnames(m) <- rownames(m) <- paste("V", 1:nvar)
        }
    }
    gload <- gf$sl[, 1]
    if (flip) {
        key <- sign(gload)
        key[key == 0] <- 1
        if (sum(key) < nvar) {
            m <- diag(key) %*% m %*% diag(key)
            gf$sl[, 1:(nfactors + 1)] <- diag(key) %*% gf$sl[, 
                1:(nfactors + 1)]
            signkey <- strtrim(key, 1)
            signkey[signkey == "1"] <- ""
            m.names <- paste(m.names, signkey, sep = "")
            colnames(m) <- rownames(m) <- m.names
            rownames(gf$sl) <- m.names
        }
    }
    Vt <- sum(m)
    Vitem <- sum(diag(m))
    gload <- gf$sl[, 1]
    gsq <- (sum(gload))^2
    uniq <- sum(gf$sl[, "u2"])
    if ((nfactors == 1) && (fm == "pc")) {
        gsq <- Vt - uniq
        warning("omega_h is not meaningful for a principal components analysis with one component")
    }
    om.tot <- (Vt - uniq)/Vt
    om.limit <- gsq/(Vt - uniq)
    alpha <- ((Vt - Vitem)/Vt) * (nvar/(nvar - 1))
    sum.smc <- sum(smc(m, covar = covar))
    lambda.6 <- (Vt + sum.smc - sum(diag(m)))/Vt
    if (!is.null(digits)) {
        omega <- list(omega_h = gsq/Vt, alpha = alpha, lambda.6 = lambda.6, 
            omega.tot = om.tot, schmid = gf, key = key, title = title)
        dg <- max(digits - 1, 1)
    }
    else {
        omega <- list(omega_h = gsq/Vt, alpha = alpha, omega.tot = om.tot, 
            schmid = gf, key = key, title = title)
        dg <- 1
    }
    ev <- colSums(gf$sl[, 1:(nfactors + 1)]^2)
    ECV <- ev[1]/sum(ev)
    omega.stats <- factor.stats(m, gf$sl[, 1:(nfactors + 1)], 
        n.obs = n.obs)
    general.stats <- factor.stats(m, as.matrix(gf$sl[, 1]), n.obs = n.obs)
    if (nfactors < 2) 
        plot <- FALSE
    omega.model <- omega.sem(omega, sl = sl)
    omg <- omgo <- omt <- rep(NA, nfactors + 1)
    sub <- apply(gf$sl, 1, function(x) which.max(abs(x[2:(nfactors + 
        1)])))
    grs <- 0
    for (group in (1:nfactors)) {
        groupi <- which(sub == group)
        if (length(groupi) > 0) {
            Vgr <- sum(m[groupi, groupi])
            gr <- sum(gf$sl[groupi, (group + 1)])
            grs <- grs + gr^2
            omg[group + 1] <- gr^2/Vgr
            omgo[group + 1] <- sum(gf$sl[groupi, 1])^2/Vgr
            omt[group + 1] <- (gr^2 + sum(gf$sl[groupi, 1])^2)/Vgr
        }
        omgo[1] <- sum(gf$sl[, 1])^2/sum(m)
        omg[1] <- grs/sum(m)
        omt[1] <- om.tot
        om.group <- data.frame(total = omt, general = omgo, group = omg)
        rownames(om.group) <- colnames(gf$sl)[1:(nfactors + 1)]
        if (!is.null(raw.data)) {
            scores <- raw.data %*% omega.stats$weights
        }
        else {
            scores <- NULL
        }
    }
    omega <- list(omega_h = gsq/Vt, omega.lim = om.limit, alpha = alpha, 
        omega.tot = om.tot, G6 = lambda.6, schmid = gf, key = key, 
        stats = omega.stats, ECV = ECV, gstats = general.stats, 
        call = cl, title = title, R = m, model = omega.model, 
        omega.group = om.group, scores = scores)
    class(omega) <- c("psych", "omega")
    if (plot) 
        omega.diagram(omega, main = title, sl = sl, labels = labels, 
            digits = dg)
    return(omega)
}


ICLUST.cluster <- function (r.mat, ICLUST.options, smc.items) 
{
    output <- ICLUST.options$output
    num.var <- nrow(r.mat)
    keep.clustering <- TRUE
    results <- data.frame(matrix(rep(0, 18 * (num.var - 1)), 
        ncol = 18))
    names(results) <- c("Item/Cluster", "Item/Clust", "similarity", 
        "correlation", "alpha1", "alpha2", "beta1", "beta2", 
        "size1", "size2", "rbar1", "rbar2", "r1", "r2", "alpha", 
        "beta", "rbar", "size")
    rownames(results) <- paste("C", 1:(num.var - 1), sep = "")
    digits <- ICLUST.options$digits
    clusters <- diag(1, nrow = nrow(r.mat))
    if (is.null(rownames(r.mat))) {
        rownames(r.mat) <- paste("V", 1:num.var, sep = "")
    }
    rownames(clusters) <- rownames(r.mat)
    colnames(clusters) <- paste("V", 1:num.var, sep = "")
    diag(r.mat) <- 0
    row.range <- apply(r.mat, 1, range, na.rm = TRUE)
    item.max <- pmax(abs(row.range[1, ]), abs(row.range[2, ]))
    diag(r.mat) <- 1
    count = 1
    while (keep.clustering) {
        cluster.stats <- cluster.cor(clusters, r.mat, FALSE, 
            SMC = ICLUST.options$SMC, item.smc = smc.items)
        sim.mat <- cluster.stats$cor
        diag(sim.mat) <- 0
        if (ICLUST.options$correct) {
            row.range <- apply(sim.mat, 1, range, na.rm = TRUE)
            row.max <- pmax(abs(row.range[1, ]), abs(row.range[2, 
                ]))
        }
        else {
            row.max <- rep(1, nrow(sim.mat))
        }
        item.rel <- cluster.stats$alpha
        for (i in 1:length(item.rel)) {
            if (cluster.stats$size[i] < 2) {
                item.rel[i] <- row.max[i]
            }
        }
        if (output > 3) 
            print(sim.mat, digits = digits)
        if (ICLUST.options$correct) {
            sq.max <- diag(1/sqrt(item.rel))
            sim <- sq.max %*% sim.mat %*% sq.max
        }
        else {
            sim <- sim.mat
        }
        diag(sim) <- NA
        test.alpha <- FALSE
        test.beta <- FALSE
        while (!(test.alpha & test.beta)) {
            max.cell <- which.max(sim)
            if (length(max.cell) < 1) {
                keep.clustering <- FALSE
                break
            }
            sign.max <- 1
            if (ICLUST.options$reverse) {
                min.cell <- which.min(sim)
                if (sim[max.cell] < abs(sim[min.cell])) {
                  sign.max <- -1
                  max.cell <- min.cell
                }
                if (sim[max.cell] < 0) {
                  sign.max <- -1
                }
            }
            max.col <- trunc(max.cell/nrow(sim)) + 1
            max.row <- max.cell - (max.col - 1) * nrow(sim)
            if (max.row < 1) {
                max.row <- nrow(sim)
                max.col <- max.col - 1
            }
            size1 <- cluster.stats$size[max.row]
            if (size1 < 2) {
                V1 <- 1
                beta1 <- item.rel[max.row]
                alpha1 <- item.rel[max.row]
                rbar1 <- item.rel[max.row]
            }
            else {
                rbar1 <- results[cluster.names[max.row], "rbar"]
                beta1 <- results[cluster.names[max.row], "beta"]
                alpha1 <- results[cluster.names[max.row], "alpha"]
                V1 <- size1 + size1 * (size1 - 1) * rbar1
            }
            size2 <- cluster.stats$size[max.col]
            if (size2 < 2) {
                V2 <- 1
                beta2 <- item.rel[max.col]
                alpha2 <- item.rel[max.col]
                rbar2 <- item.rel[max.col]
            }
            else {
                rbar2 <- results[cluster.names[max.col], "rbar"]
                beta2 <- results[cluster.names[max.col], "beta"]
                alpha2 <- results[cluster.names[max.col], "alpha"]
                V2 <- size2 + size2 * (size2 - 1) * rbar2
            }
            Cov12 <- sign.max * sim.mat[max.cell] * sqrt(V1 * 
                V2)
            r12 <- Cov12/(size1 * size2)
            V12 <- V1 + V2 + 2 * Cov12
            size12 <- size1 + size2
            V12c <- (V12 - size12) * (size12/(size12 - 1))
            rbar <- V12c/(size12^2)
            alpha <- V12c/V12
            beta.weighted <- size12^2 * r12/V12
            beta.unweighted <- 2 * sign.max * sim.mat[max.cell]/(1 + 
                sign.max * sim.mat[max.cell])
            if (ICLUST.options$weighted) {
                beta.combined <- beta.weighted
            }
            else {
                beta.combined <- beta.unweighted
            }
            if (ICLUST.options$cor.gen) {
                c1 <- r12 * size1 * size1 + Cov12
                c2 <- sign.max * (r12 * size2 * size2 + Cov12)
            }
            else {
                c1 <- size1^2 * rbar1 + Cov12
                c2 <- sign.max * (size2^2 * rbar2 + Cov12)
            }
            if ((size1 < 2) && (size2 < 2)) {
                r1 <- sqrt(abs(rbar1))
                r2 <- sign.max * r1
            }
            else {
                if (ICLUST.options$correct.cluster) {
                  if (TRUE) {
                    r1 <- c1/sqrt((V1 - size1 + size1 * rbar1) * 
                      V12)
                    if (size2 < 2) {
                      r2 <- c2/sqrt(abs(rbar2) * V12)
                    }
                    else {
                      r2 <- c2/sqrt((V2 - size2 + size2 * rbar2) * 
                        V12c)
                    }
                  }
                  else {
                    if (size1 < 2) {
                      r1 <- c1/sqrt(abs(rbar1) * V12)
                    }
                    else {
                      r1 <- c1/sqrt((V1 - size1 + size1 * rbar1) * 
                        V12c)
                    }
                    if (size2 < 2) {
                      r2 <- c2/sqrt(abs(rbar2) * V12)
                    }
                    else {
                      r2 <- c2/sqrt((V2 - size2 + size2 * rbar2) * 
                        V12c)
                    }
                  }
                }
                else {
                  if (TRUE) {
                    r1 <- c1/sqrt(V1 * V12)
                    r2 <- sign.max * c2/sqrt(V2 * V12)
                  }
                  else {
                    r1 <- sign.max * c1/sqrt(V1 * V12)
                  }
                  r2 <- c2/sqrt(V2 * V12)
                }
            }
            test.alpha <- TRUE
            if (ICLUST.options$alpha > 0) {
                if (ICLUST.options$alpha.size < min(size1, size2)) {
                  switch(ICLUST.options$alpha, {
                    if (alpha < min(alpha1, alpha2)) {
                      if (output > 2) {
                        print(paste("do not combine ", cluster.names[max.row], 
                          "with", cluster.names[max.col], "new alpha =", 
                          alpha, "old alpha1 =", alpha1, "old alpha2 =", 
                          alpha2))
                      }
                      test.alpha <- FALSE
                    }
                  }, {
                    if (alpha < mean(alpha1, alpha2)) {
                      if (output > 2) {
                        print(paste("do not combine ", cluster.names[max.row], 
                          "with", cluster.names[max.col], "new alpha =", 
                          alpha, "old alpha1 =", alpha1, "old alpha2 =", 
                          alpha2))
                      }
                      test.alpha <- FALSE
                    }
                  }, {
                    if (alpha < max(alpha1, alpha2)) {
                      if (output > 2) {
                        print(paste("do not combine ", cluster.names[max.row], 
                          "with", cluster.names[max.col], "new alpha =", 
                          alpha, "old alpha1 =", alpha1, "old alpha2 =", 
                          alpha2))
                      }
                      test.alpha <- FALSE
                    }
                  })
                }
            }
            test.beta <- TRUE
            if (ICLUST.options$beta > 0) {
                if (ICLUST.options$beta.size < min(size1, size2)) {
                  switch(ICLUST.options$beta, {
                    if (beta.combined < min(beta1, beta2)) {
                      if (output > 2) {
                        print(paste("do not combine ", cluster.names[max.row], 
                          "with", cluster.names[max.col], "new beta =", 
                          round(beta.combined, digits), "old beta1 =", 
                          round(beta1, digits), "old beta2 =", 
                          round(beta2, digits)))
                      }
                      test.beta <- FALSE
                    }
                  }, {
                    if (beta.combined < mean(beta1, beta2)) {
                      if (output > 2) {
                        print(paste("do not combine ", cluster.names[max.row], 
                          "with", cluster.names[max.col], "new beta =", 
                          round(beta.combined, digits), "old beta1 =", 
                          round(beta1, digits), "old beta2 =", 
                          round(beta2, digits)))
                      }
                      test.beta <- FALSE
                    }
                  }, {
                    if (beta.combined < max(beta1, beta2)) {
                      if (output > 2) {
                        print(paste("do not combine ", cluster.names[max.row], 
                          "with", cluster.names[max.col], "new beta =", 
                          round(beta.combined, digits), "old beta1 =", 
                          round(beta1, digits), "old beta2 =", 
                          round(beta2, digits)))
                      }
                      test.beta <- FALSE
                    }
                  })
                }
            }
            if (test.beta & test.alpha) {
                break
            }
            else {
                if ((ICLUST.options$n.clus > 0) & ((num.var - 
                  count) >= ICLUST.options$n.clus)) {
                  warning("Clusters formed as requested do not meet the alpha and beta criteria. Perhaps you should rethink the number of cluster settings.")
                  break
                }
                else {
                  if (beta.combined < ICLUST.options$beta.min) {
                    keep.clustering <- FALSE
                    break
                  }
                  else {
                    sim[max.row, max.col] <- NA
                    sim[max.col, max.row] <- NA
                  }
                }
            }
        }
        if (keep.clustering) {
            clusters[, max.row] <- clusters[, max.row] + sign.max * 
                clusters[, max.col]
            cluster.names <- colnames(clusters)
            results[count, 1] <- cluster.names[max.row]
            results[count, 2] <- cluster.names[max.col]
            results[count, "similarity"] <- sim[max.cell]
            results[count, "correlation"] <- sim.mat[max.cell]
            results[count, "alpha1"] <- item.rel[max.row]
            results[count, "alpha2"] <- item.rel[max.col]
            size1 <- cluster.stats$size[max.row]
            size2 <- cluster.stats$size[max.col]
            results[count, "size1"] <- size1
            results[count, "size2"] <- size2
            results[count, "beta1"] <- beta1
            results[count, "beta2"] <- beta2
            results[count, "rbar1"] <- rbar1
            results[count, "rbar2"] <- rbar2
            results[count, "r1"] <- r1
            results[count, "r2"] <- r2
            results[count, "beta"] <- beta.combined
            results[count, "alpha"] <- alpha
            results[count, "rbar"] <- rbar
            results[count, "size"] <- size12
            cluster.names[max.row] <- paste("C", count, sep = "")
            colnames(clusters) <- cluster.names
            clusters <- clusters[, -max.col]
            cluster.names <- colnames(clusters)
        }
        if (output > 1) 
            print(results[count, ], digits = digits)
        count = count + 1
        if ((num.var - count) < ICLUST.options$n.clus) {
            keep.clustering <- FALSE
        }
        if (num.var - count < 1) {
            keep.clustering <- FALSE
        }
    }
    ICLUST.cluster <- list(results = results, clusters = clusters, 
        number <- num.var - count)
}


cor.plot.upperLowerCi <- function (R, numbers = TRUE, cuts = c(0.001, 0.01, 0.05), select = NULL, 
    main = "Upper and lower confidence intervals of correlations", 
    ...) 
{
    lower <- R$ci$lower
    upper <- R$ci$upper
    temp <- lower
    if (is.null(R$r)) {
        cn = colnames(R$rho)
        rl <- R$rho[lower.tri(R$rho)]
    }
    else {
        cn = colnames(R$r)
        rl <- R$r[lower.tri(R$r)]
    }
    lower[rl < 0] <- upper[rl < 0]
    upper[rl < 0] <- temp[rl < 0]
    m <- length(lower)
    n <- floor((sqrt(1 + 8 * m) + 1)/2)
    X <- diag(n)
    X[lower.tri(X)] <- upper
    X <- t(X)
    X[lower.tri(X)] <- lower
    diag(X) <- 1
    colnames(X) <- rownames(X) <- cn
    if (is.null(R$ptci)) {
        pval <- R$p
    }
    else {
        pval = 2 * (1 - R$ptci)
    }
    cor.plot(X, numbers = numbers, pval = pval, cuts = cuts, 
        select = select, main = main, ...)
    class(X) <- c("psych", "cor.cip")
    colnames(X) <- abbreviate(rownames(X, 4))
    invisible(X)
}


circular.cor <- function (angle, na.rm = TRUE) 
{
    nvar <- dim(angle)[2]
    correl <- diag(nvar)
    x <- cos(angle)
    y <- sin(angle)
    mx <- colMeans(x, na.rm = na.rm)
    my <- colMeans(y, na.rm = na.rm)
    mean.angle <- sign(my) * acos((mx)/sqrt(mx^2 + my^2))
    for (i in 1:nvar) {
        for (j in 1:i) {
            covar <- sum(sin(angle[, i] - mean.angle[i]) * sin(angle[, 
                j] - mean.angle[j]))
            correl[i, j] <- correl[j, i] <- covar
        }
    }
    var <- diag(correl)
    sd <- diag(sqrt(1/diag(correl)))
    correl <- sd %*% correl %*% sd
    colnames(correl) <- rownames(correl) <- colnames(angle)
    return(list(correl, var))
}


splitHalf <- function (r, raw = FALSE, brute = FALSE, n.sample = 10000, covar = FALSE, 
    check.keys = TRUE, key = NULL, use = "pairwise") 
{
    cl <- match.call()
    split <- function(o, n) {
        A <- B <- rep(0, n)
        A[o] <- B[-o] <- 1
        A[-o] <- B[o] <- 0
        AB <- cbind(A, B)
        R <- t(AB) %*% r %*% AB
        Rab <- R[1, 2]/sqrt(R[1, 1] * R[2, 2])
        rab <- 4 * R[1, 2]/sum(R)
        result <- list(rab = rab, AB = AB)
    }
    keys <- key
    maxrb <- -9999
    minrb <- 2
    n <- ncol(r)
    n2 <- trunc(n/2)
    n.obs <- nrow(r)
    if (n.obs > n) {
        r <- cov(r, use = use)
    }
    if (!covar) 
        r <- cov2cor(r)
    if (check.keys && is.null(keys)) {
        p1 <- principal(r)
        if (any(p1$loadings < 0)) 
            warning("Some items were negatively correlated with total scale and were automatically reversed.")
        keys <- 1 - 2 * (p1$loadings < 0)
    }
    if (is.null(keys)) {
        keys <- rep(1, n)
    }
    else {
        keys <- as.vector(keys)
        if (length(keys) < n) {
            temp <- keys
            keys <- rep(1, n)
            names(keys) <- colnames(r)
            keys[temp] <- -1
        }
    }
    key.d <- diag(keys)
    r <- key.d %*% r %*% key.d
    signkey <- strtrim(keys, 1)
    signkey[signkey == "1"] <- ""
    colnames(r) <- paste(colnames(r), signkey, sep = "")
    e <- 0
    m <- n2
    h <- m
    sumr <- sum(r)
    alpha <- ((sumr - tr(r))/sumr) * n/(n - 1)
    tsmc <- sum(smc(r))
    lambda6 <- (sumr - tr(r) + tsmc)/sumr
    sumr <- 0
    x <- seq_len(n)
    a <- seq_len(m)
    count <- round(choose(n, m))/2
    if (brute || ((count <= n.sample) && !raw)) {
        brute <- TRUE
        if (raw) 
            result <- rep(NA, count)
        o <- a
        sp <- split(o, n)
        if (raw) 
            result[1] <- sp$rab
        maxrb <- sp$rab
        maxAB <- sp$AB
        minrb <- sp$rab
        minAB <- sp$AB
        i <- 2
        while (i < (count + 1)) {
            if (e < n - h) {
                h <- 1L
                e <- a[m]
                j <- 1L
            }
            else {
                e <- a[m - h]
                h <- h + 1L
                j <- 1L:h
            }
            a[m - h + j] <- e + j
            o <- x[a]
            sp <- split(o, n)
            if (raw) 
                result[i] <- sp$rab
            sumr <- sumr + sp$rab
            if (sp$rab > maxrb) {
                maxrb <- sp$rab
                maxAB <- sp$AB
            }
            if (sp$rab < minrb) {
                minrb <- sp$rab
                minAB <- sp$AB
            }
            i <- i + 1L
        }
    }
    else {
        result <- rep(NA, n.sample)
        sumr <- 0
        for (i in 1:n.sample) {
            o <- sample(n, n2)
            sp <- split(o, n)
            if (raw) 
                result[i] <- sp$rab
            sumr <- sumr + sp$rab
            if (sp$rab > maxrb) {
                maxrb <- sp$rab
                maxAB <- sp$AB
            }
            if (sp$rab < minrb) {
                minrb <- sp$rab
                minAB <- sp$AB
            }
        }
    }
    if (brute) {
        meanr <- sumr/count
    }
    else {
        meanr <- sumr/n.sample
    }
    meansp <- 2 * meanr/(1 + meanr)
    kd <- diag(key.d)
    maxAB = maxAB * kd
    minAB = minAB * kd
    if (raw) {
        results <- list(maxrb = maxrb, minrb = minrb, maxAB = maxAB, 
            minAB = minAB, meanr = meanr, alpha = alpha, lambda6 = lambda6, 
            raw = result, Call = cl)
    }
    else {
        results <- list(maxrb = maxrb, minrb = minrb, maxAB = maxAB, 
            minAB = minAB, meanr = meanr, alpha = alpha, lambda6 = lambda6, 
            Call = cl)
    }
    class(results) <- c("psych", "split")
    return(results)
}


ICLUST.rgraph <- function (ic.results, out.file = NULL, min.size = 1, short = FALSE, 
    labels = NULL, size = c(8, 6), node.font = c("Helvetica", 
        14), edge.font = c("Helvetica", 10), rank.direction = c("RL", 
        "TB", "LR", "BT"), digits = 2, title = "ICLUST", label.font = 2, 
    ...) 
{
    if (!requireNamespace("Rgraphviz")) {
        stop("I am sorry, you need to have the  Rgraphviz package installed")
        nodes <- function() {
        }
        addEdge <- function() {
        }
        subGraph <- function() {
        }
    }
    clusters <- as.matrix(ic.results$clusters)
    results <- ic.results$results
    rank.direction <- match.arg(rank.direction)
    num.var <- dim(clusters)[1]
    num.clust <- num.var - dim(clusters)[2]
    vars <- paste("V", 1:num.var, sep = "")
    clust <- paste("C", 1:num.clust, sep = "")
    clust.graph <- new("graphNEL", nodes = c(vars, clust), edgemode = "directed")
    graph.shape <- c(rep("box", num.var), rep("ellipse", num.clust))
    graph.rank <- c(rep("sink", num.var), rep("", num.clust))
    names(graph.shape) <- nodes(clust.graph)
    names(graph.rank) <- nodes(clust.graph)
    edge.label <- rep("", num.clust * 2)
    edge.name <- rep("", num.clust * 2)
    names(edge.label) <- seq(1:num.clust * 2)
    for (i in 1:num.clust) {
        if (results[i, 1] > 0) {
            clust.graph <- addEdge(row.names(results)[i], results[i, 
                1], clust.graph, 1)
            edge.label[(i - 1) * 2 + 1] <- round(results[i, "r1"], 
                digits)
            edge.name[(i - 1) * 2 + 1] <- paste(row.names(results)[i], 
                "~", results[i, 1], sep = "")
            clust.graph <- addEdge(row.names(results)[i], results[i, 
                2], clust.graph, 1)
            edge.label[i * 2] <- round(results[i, "r2"], digits)
            edge.name[i * 2] <- paste(row.names(results)[i], 
                "~", results[i, 2], sep = "")
        }
    }
    nAttrs <- list()
    eAttrs <- list()
    if (!is.null(labels)) {
        var.labels <- c(labels, row.names(results))
        names(var.labels) <- nodes(clust.graph)
        nAttrs$label <- var.labels
        names(edge.label) <- edge.name
        node.font.size <- as.numeric(node.font[2])
        n.font.size <- c(rep(label.font * node.font.size, length(labels)), 
            rep(node.font.size, (length(var.labels) - length(labels))))
        names(n.font.size) <- nodes(clust.graph)
        nAttrs$fontsize <- n.font.size
    }
    names(edge.label) <- edge.name
    e.font.size <- rep(6, num.clust * 2)
    names(e.font.size) <- edge.name
    nAttrs$shape <- graph.shape
    eAttrs$fontsize <- e.font.size
    nAttrs$rank <- graph.rank
    eAttrs$label <- edge.label
    attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE), 
        graph = list(rankdir = rank.direction, fontsize = 8, 
            bgcolor = "white"))
    obs.var <- subGraph(vars, clust.graph)
    cluster.vars <- subGraph(clust, clust.graph)
    observed <- list(list(graph = obs.var, cluster = TRUE, attrs = c(rank = "sink")))
    plot(clust.graph, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
        attrs = attrs, subGList = observed, main = title)
    if (!is.null(out.file)) {
        toDotty(clust.graph, out.file, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
            attrs = attrs, subGList = observed)
    }
}


structure.list <- function (nvars, f.list, f = NULL, f.labels = NULL, item.labels = NULL) 
{
    nfactors <- length(f.list)
    fmodel <- matrix(rep(0, nvars * nfactors), ncol = nfactors)
    for (i in 1:nfactors) {
        if (!is.null(f.list[[i]])) {
            list.i <- unlist(f.list[[i]])
            fmodel[abs(list.i), i] <- paste(f, letters[i], list.i, 
                sep = "")
        }
    }
    if (!is.null(f.labels)) {
        colnames(fmodel) <- f.labels
    }
    else {
        colnames(fmodel) <- names(f.list)
    }
    if (!is.null(item.labels)) {
        rownames(fmodel) <- item.labels
    }
    return(fmodel)
}


error.bars <- function (x, stats = NULL, ylab = "Dependent Variable", xlab = "Independent Variable", 
    main = NULL, eyes = TRUE, ylim = NULL, xlim = NULL, alpha = 0.05, 
    sd = FALSE, labels = NULL, pos = NULL, arrow.len = 0.05, 
    arrow.col = "black", add = FALSE, bars = FALSE, within = FALSE, 
    col = "blue", ...) 
{
    SCALE = 0.5
    if (is.null(stats)) {
        x.stats <- describe(x)
        if (within) {
            x.smc <- smc(x, covar = TRUE)
            x.stats$se <- sqrt((x.stats$sd^2 - x.smc)/x.stats$n)
        }
        if (is.null(dim(x))) {
            z <- 1
        }
        else {
            z <- dim(x)[2]
        }
        names <- colnames(x)
    }
    else {
        x.stats <- stats
        z <- dim(x.stats)[1]
        names <- rownames(stats)
    }
    min.x <- min(x.stats$mean, na.rm = TRUE)
    max.x <- max(x.stats$mean, na.rm = TRUE)
    max.se <- max(x.stats$se, na.rm = TRUE)
    {
        if (!sd) {
            if (is.null(stats)) {
                ci <- qt(1 - alpha/2, x.stats$n - 1)
            }
            else {
                ci <- rep(1, z)
            }
        }
        else {
            ci <- sqrt(x.stats$n)
            max.se <- max(ci * x.stats$se, na.rm = TRUE)
        }
    }
    if (is.null(main)) {
        if (!sd) {
            main = paste((1 - alpha) * 100, "% confidence limits", 
                sep = "")
        }
        else {
            main = paste("Means and standard deviations")
        }
    }
    if (is.null(ylim)) {
        if (is.na(max.x) | is.na(max.se) | is.na(min.x) | is.infinite(max.x) | 
            is.infinite(min.x) | is.infinite(max.se)) {
            ylim = c(0, 1)
        }
        else {
            if (bars) {
                ylim = c(min(0, min.x - 3 * max.se), max.x + 
                  3 * max.se)
            }
            else {
                ylim = c(min.x - 3 * max.se, max.x + 3 * max.se)
            }
        }
    }
    if (bars) {
        mp = barplot(x.stats$mean, ylim = ylim, xlab = xlab, 
            ylab = ylab, main = main, col = col, ...)
        axis(1, mp[1:z], names)
        axis(2)
        box()
    }
    else {
        if (!add) {
            if (missing(xlim)) {
                if (is.null(x.stats$values)) {
                  xlim <- c(0.5, z + 0.5)
                }
                else {
                  xlim <- c(min(x.stats$values) - 0.5, max(x.stats$values) + 
                    0.5)
                }
            }
            if (is.null(x.stats$values)) {
                plot(x.stats$mean, ylim = ylim, xlab = xlab, 
                  ylab = ylab, xlim = xlim, axes = FALSE, main = main, 
                  ...)
                axis(1, 1:z, names, ...)
                axis(2)
                box()
            }
            else {
                plot(x.stats$values, x.stats$mean, ylim = ylim, 
                  xlim = xlim, xlab = xlab, ylab = ylab, main = main, 
                  ...)
            }
        }
        else {
            points(x.stats$mean, ...)
        }
    }
    if (!is.null(labels)) {
        lab <- labels
    }
    else {
        lab <- paste("V", 1:z, sep = "")
    }
    if (length(pos) == 0) {
        locate <- rep(1, z)
    }
    else {
        locate <- pos
    }
    if (length(labels) == 0) 
        lab <- rep("", z)
    else lab <- labels
    s <- c(1:z)
    if (bars) {
        arrows(mp[s], x.stats$mean[s] - ci[s] * x.stats$se[s], 
            mp[s], x.stats$mean[s] + ci[s] * x.stats$se[s], length = arrow.len, 
            angle = 90, code = 3, col = par("fg"), lty = NULL, 
            lwd = par("lwd"), xpd = NULL)
    }
    else {
        if (is.null(x.stats$values)) {
            arrows(s[s], x.stats$mean[s] - ci[s] * x.stats$se[s], 
                s[s], x.stats$mean[s] + ci[s] * x.stats$se[s], 
                length = arrow.len, angle = 90, code = 3, col = arrow.col)
        }
        else {
            arrows(x.stats$values, x.stats$mean[s] - ci[s] * 
                x.stats$se[s], x.stats$values, x.stats$mean[s] + 
                ci[s] * x.stats$se[s], length = arrow.len, angle = 90, 
                code = 3, col = arrow.col)
        }
        if (eyes) {
            if (length(col) == 1) 
                col <- rep(col, z)
            ln <- seq(-3, 3, 0.1)
            rev <- (length(ln):1)
            for (s in 1:z) {
                if (!is.null(x.stats$values[s])) {
                  catseyes(x = x.stats$values[s], y = x.stats$mean[s], 
                    se = x.stats$se[s], n = x.stats$n[s], alpha = alpha, 
                    density = -10, col = col[s])
                }
                else {
                  catseyes(x = s, y = x.stats$mean[s], se = x.stats$se[s], 
                    n = x.stats$n[s], alpha = alpha, density = -10, 
                    col = col[s])
                }
            }
        }
    }
}


factor.plot <- function (ic.results, cluster = NULL, cut = 0, labels = NULL, 
    title, jiggle = FALSE, amount = 0.02, pch = 18, pos, show.points = TRUE, 
    ...) 
{
    fa.plot(ic.results, cluster = cluster, cut = cut, labels = labels, 
        title = title, jiggle = jiggle, amount = amount, pch = pch, 
        pos = pos, show.points = show.points, ...)
}


winsor.mean <- function (x, trim = 0.2, na.rm = TRUE) 
{
    if (is.vector(x)) {
        ans <- win.mean(x, trim = trim, na.rm = na.rm)
    }
    else {
        if (is.matrix(x) | is.data.frame(x)) {
            ans <- apply(x, 2, win.mean, trim = trim, na.rm = na.rm)
        }
    }
    return(ans)
}


interp.boxplot <- function (x, w = 1, na.rm = TRUE) 
{
    stats <- interp.quartiles(x, w, na.rm = na.rm)
    return(stats)
}


lowerCor <- function (x, digits = 2, use = "pairwise", method = "pearson") 
{
    R <- cor(x, use = use, method = method)
    lowerMat(R, digits)
    invisible(R)
}


plot.poly.parallel <- function (x, show.legend = TRUE, fa = "both", ...) 
{
    e.values <- x$pc.values
    values <- x$fa.values
    pcm <- x$pc.sim$mean
    fam <- x$fa.sim$mean
    switch(fa, both = {
        plot(e.values, type = "b", main = "Eigen values of tetrachoric/polychoric matrix", 
            ylab = "Eigen values of original  and simulated factors and components", 
            ylim = c(0, max(e.values)), xlab = "Factor Number", 
            pch = 4, col = "blue")
        points(values, type = "b", pch = 2, col = "blue")
        points(pcm, type = "l", lty = "dotted", pch = 2, col = "red")
        points(fam, type = "l", lty = "dashed", pch = 2, col = "red")
        if (!is.null(x$pcs.sim)) points(x$pcs.sim$mean, type = "l", 
            lty = "dashed", pch = 2, col = "red")
        if (!is.null(x$fas.sim)) points(x$fas.sim$mean, type = "l", 
            lty = "dashed", pch = 2, col = "red")
    }, fa = {
        plot(values, type = "b", main = "Eigen values of tetrachoric/polychoric matrix", 
            ylab = "Eigen values of original  and simulated factors ", 
            ylim = c(0, max(e.values)), xlab = "Factor Number", 
            pch = 2, col = "blue")
        points(fam, type = "l", lty = "dashed", pch = 2, col = "red")
    }, pc = {
        plot(e.values, type = "b", main = "Eigen values of tetrachoric/polychoric matrix", 
            ylab = "Eigen values of original  and simulated factors and components", 
            ylim = c(0, max(e.values)), xlab = "Factor Number", 
            pch = 4, col = "blue")
        points(pcm, type = "l", lty = "dotted", pch = 2, col = "red")
    })
    if (show.legend) {
        switch(fa, both = {
            if (!is.null(x$pcs.sim)) {
                legend("topright", c("PC  Actual Data", " PC  Resampled Data", 
                  " PC  Simulated Data", "FA  Actual Data", " FA  Resampled Data", 
                  " FA  Simulated Data"), col = c("blue", "red", 
                  "red", "blue", "red", "red"), pch = c(4, NA, 
                  NA, 2, NA, NA), text.col = "green4", lty = c("solid", 
                  "dotted", "dashed", "solid", "dotted", "dashed"), 
                  merge = TRUE, bg = "gray90")
            } else {
                legend("topright", c("PC  Actual Data", " PC  Resampled Data", 
                  "FA  Actual Data", " FA  Resampled Data"), 
                  col = c("blue", "red", "blue", "red"), pch = c(4, 
                    NA, 2, NA), text.col = "green4", lty = c("solid", 
                    "dotted", "solid", "dotted"), merge = TRUE, 
                  bg = "gray90")
            }
        }, fa = {
            legend("topright", c("FA  Actual Data", " FA  Resampled Data"), 
                col = c("blue", "red"), pch = c(2, NA), text.col = "green4", 
                lty = c("solid", "dotted"), merge = TRUE, bg = "gray90")
        }, pc = {
            legend("topright", c("PC  Actual Data", " PC  Resampled Data"), 
                col = c("blue", "red"), pch = c(4, NA), text.col = "green4", 
                lty = c("solid", "dotted"), merge = TRUE, bg = "gray90")
        })
    }
    fa.test <- which(!(values > fam))[1] - 1
    pc.test <- which(!(e.values > pcm))[1] - 1
    cat("Parallel analysis suggests that ")
    cat("the number of factors = ", fa.test, " and the number of components = ", 
        pc.test, "\n")
}


biplot.psych <- function (x, labels = NULL, cex = c(0.75, 1), main = "Biplot from fa", 
    hist.col = "cyan", xlim.s = c(-3, 3), ylim.s = c(-3, 3), 
    xlim.f = c(-1, 1), ylim.f = c(-1, 1), maxpoints = 100, adjust = 1.2, 
    col, pos, arrow.len = 0.1, pch = 16, choose = NULL, cuts = 1, 
    cutl = 0, group = NULL, ...) 
{
    if (is.null(x$scores)) 
        stop("Biplot requires factor/component scores. \nYou need to r run fa/pca from the raw data")
    op <- par()
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    MAR <- par("mar")
    MFROW <- par("mfrow")
    if (is.list(x$scores)) 
        x$scores <- x$scores$scores
    if (is.list(x$fa)) 
        x$loadings <- x$fa$loadings
    if (!is.null(choose)) {
        x$scores <- x$scores[, choose, drop = FALSE]
        x$loadings <- x$loadings[, choose, drop = FALSE]
    }
    colnames(x$scores) <- colnames(x$loadings)
    if ((missing(group)) || (is.null(group))) 
        group <- rep(1, nrow(x$scores))
    n.dims <- dim(x$loadings)[2]
    if (missing(col)) {
        col <- c("black", "red", "blue", "#FF0000FF", "#00FF00FF", 
            "#00FFFFFF", "#0000FFFF", "#FF00FFFF")
    }
    ch.col <- col
    if (n.dims == 2) {
        op <- par(pty = "s")
        if (!is.null(main)) 
            op1 <- c(op, par(mar = MAR + c(0, 0, 1, 0)))
        plotone(x$scores, x$loading, labels = labels, xlim.s = xlim.s, 
            ylim.s = ylim.s, xlim.f = xlim.f, ylim.f = ylim.f, 
            maxpoints = maxpoints, adjust = adjust, col = col, 
            pos = pos, arrow.len = arrow.len, pch = pch, choose = choose, 
            cuts = cuts, cutl = cutl, group = group, ch.col = ch.col, 
            ...)
    }
    else {
        op1 <- par(mfrow = c(n.dims, n.dims), mar = c(2, 3, 3, 
            2))
        if (nrow(x$scores) > maxpoints) {
            labels <- rep(".", nrow(x$scores))
        }
        else {
            labels <- rep("o", nrow(x$scores))
        }
        for (i in 1:n.dims) {
            for (j in 1:n.dims) {
                if (i == j) {
                  h <- hist(x$scores[, i], freq = FALSE, main = colnames(x$loadings)[i], 
                    xlab = "", ylab = "", col = hist.col)
                  breaks <- h$breaks
                  nB <- length(breaks)
                  tryd <- try(d <- density(x$scores[, i], na.rm = TRUE, 
                    bw = "nrd", adjust = adjust), silent = TRUE)
                  if (class(tryd) != "try-error") {
                    lines(d)
                  }
                }
                else {
                  plotone(x$scores[, c(j, i)], x$loadings[, c(j, 
                    i)], main = NULL, xlim.s = xlim.s, ylim.s = ylim.s, 
                    xlim.f = xlim.f, ylim.f = ylim.f, maxpoints = maxpoints, 
                    adjust = adjust, col = col, pos = pos, arrow.len = arrow.len, 
                    pch = pch, choose = choose, cuts = cuts, 
                    cutl = cutl, group = group, ch.col = ch.col, 
                    ...)
                }
            }
        }
    }
    par(mar = MAR)
    par(mfrow = MFROW)
}


comorbidity <- function (d1, d2, com, labels = NULL) 
{
    cl <- match.call()
    twobytwo <- matrix(c(com, d1 - com, d2 - com, 1 - d1 - d2 + 
        com), ncol = 2)
    if (is.null(labels)) {
        colnames(twobytwo) <- c("D1", "d1")
        rownames(twobytwo) <- c("D2", "d2")
    }
    else {
        colnames(twobytwo) <- c(labels[1], paste("-", labels[1], 
            sep = ""))
        rownames(twobytwo) <- c(labels[2], paste("-", labels[2], 
            sep = ""))
    }
    phi <- phi(twobytwo)
    Yule <- Yule(twobytwo)
    tetra <- tetrachoric(twobytwo)
    answer <- list(twobytwo = twobytwo, phi = phi, Yule = Yule, 
        tetra = tetra, Call = cl)
    class(answer) <- c("psych", "comorbid")
    return(answer)
}


item.sim <- function (nvar = 72, nsub = 500, circum = FALSE, xloading = 0.6, 
    yloading = 0.6, gloading = 0, xbias = 0, ybias = 0, categorical = FALSE, 
    low = -3, high = 3, truncate = FALSE, cutpoint = 0) 
{
    avloading <- (xloading + yloading)/2
    errorweight <- sqrt(1 - (avloading^2 + gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) * xloading + xbias
    truey <- rnorm(nsub) * yloading + ybias
    if (circum) {
        radia <- seq(0, 2 * pi, len = nvar + 1)
        rad <- radia[which(radia < 2 * pi)]
    }
    else rad <- c(rep(0, nvar/4), rep(pi/2, nvar/4), rep(pi, 
        nvar/4), rep(3 * pi/2, nvar/4))
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    trueitem <- outer(truex, cos(rad)) + outer(truey, sin(rad))
    item <- gloading * g + trueitem + errorweight * error
    if (categorical) {
        item = round(item)
        item[(item <= low)] <- low
        item[(item > high)] <- high
    }
    if (truncate) {
        item[item < cutpoint] <- 0
    }
    return(item)
}


table2matrix <- function (x, labs = NULL) 
{
    n <- sum(x)
    nrows <- dim(x)[1]
    ncol <- dim(x)[2]
    rowval <- as.numeric(rownames(x))
    colval <- as.numeric(colnames(x))
    xm <- matrix(NaN, nrow = n, ncol = 2)
    k <- 1
    for (rows in 1:nrows) {
        for (cols in 1:ncol) {
            case <- x[rows, cols]
            if (case > 0) {
                for (cases in 1:case) {
                  xm[k, 1] <- rowval[rows]
                  xm[k, 2] <- colval[cols]
                  k <- k + 1
                }
            }
        }
    }
    if (!is.null(labs)) 
        colnames(xm) <- labs
    return(xm)
}


corCi <- function (x, keys = NULL, n.iter = 100, p = 0.05, overlap = FALSE, 
    poly = FALSE, method = "pearson", plot = TRUE, ...) 
{
    cl <- match.call()
    n.obs <- dim(x)[1]
    if (is.null(keys) && overlap) 
        overlap <- FALSE
    if (poly) {
        ncat <- 8
        nvar <- dim(x)[2]
        tx <- table(as.matrix(x))
        if (dim(tx)[1] == 2) {
            tet <- tetrachoric(x)
            typ = "tet"
        }
        else {
            tab <- apply(x, 2, function(x) table(x))
            if (is.list(tab)) {
                len <- lapply(tab, function(x) length(x))
            }
            else {
                len <- dim(tab)[1]
            }
            dvars <- subset(1:nvar, len == 2)
            pvars <- subset(1:nvar, ((len > 2) & (len <= ncat)))
            cvars <- subset(1:nvar, (len > ncat))
            if (length(pvars) == ncol(x)) {
                tet <- polychoric(x)
                typ = "poly"
            }
            else {
                tet <- mixed.cor(x)
                typ = "mixed"
            }
        }
        Rho <- tet$rho
    }
    else {
        Rho <- cor(x, use = "pairwise", method = method)
    }
    if (!is.null(keys)) {
        bad <- FALSE
        if (any(is.na(Rho))) {
            warning(sum(is.na(Rho)), " of the item correlations are NA and thus finding scales that include those items will not work.\n We will try to do it for those  scales which do not include those items.\n         \n Proceed with caution. ")
            bad <- TRUE
            rho <- apply(keys, 2, function(x) colMeans(apply(keys, 
                2, function(x) colMeans(Rho * x, na.rm = TRUE)) * 
                x, na.rm = TRUE))
        }
        else {
            rho <- t(keys) %*% Rho %*% keys
        }
    }
    else {
        rho <- Rho
    }
    if (overlap) {
        key.var <- diag(t(keys) %*% keys)
        var <- diag(rho)
        n.keys <- ncol(keys)
        key.av.r <- (var - key.var)/(key.var * (key.var - 1))
        item.cov <- t(keys) %*% Rho
        raw.cov <- item.cov %*% keys
        adj.cov <- raw.cov
        for (i in 1:(n.keys)) {
            for (j in 1:i) {
                adj.cov[i, j] <- adj.cov[j, i] <- raw.cov[i, 
                  j] - sum(keys[, i] * keys[, j]) + sum(keys[, 
                  i] * keys[, j] * sqrt(key.av.r[i] * key.av.r[j]))
            }
        }
        diag(adj.cov) <- diag(raw.cov)
        rho <- cov2cor(adj.cov)
    }
    rho <- cov2cor(rho)
    nvar <- dim(rho)[2]
    if (n.iter > 1) {
        replicates <- list()
        rep.rots <- list()
        replicates <- mclapply(1:n.iter, function(XX) {
            xs <- x[sample(n.obs, n.obs, replace = TRUE), ]
            {
                if (poly) {
                  if (typ != "tet") {
                    tets <- mixed.cor(xs)
                  }
                  else {
                    tets <- tetrachoric(xs)
                  }
                  R <- tets$rho
                }
                else {
                  R <- cor(xs, use = "pairwise", method = method)
                }
                if (!is.null(keys)) {
                  if (bad) {
                    covariances <- apply(keys, 2, function(x) colMeans(apply(keys, 
                      2, function(x) colMeans(R * x, na.rm = TRUE)) * 
                      x, na.rm = TRUE))
                  }
                  else {
                    covariances <- t(keys) %*% R %*% keys
                  }
                  r <- cov2cor(covariances)
                }
                else {
                  r <- R
                }
                if (overlap) {
                  var <- diag(covariances)
                  item.cov <- t(keys) %*% R
                  raw.cov <- item.cov %*% keys
                  adj.cov <- raw.cov
                  key.av.r <- (var - key.var)/(key.var * (key.var - 
                    1))
                  for (i in 1:(n.keys)) {
                    for (j in 1:i) {
                      adj.cov[i, j] <- adj.cov[j, i] <- raw.cov[i, 
                        j] - sum(keys[, i] * keys[, j]) + sum(keys[, 
                        i] * keys[, j] * sqrt(key.av.r[i] * key.av.r[j]))
                    }
                  }
                  diag(adj.cov) <- diag(raw.cov)
                  r <- cov2cor(adj.cov)
                }
                rep.rots <- r[lower.tri(r)]
            }
        })
    }
    replicates <- matrix(unlist(replicates), ncol = nvar * (nvar - 
        1)/2, byrow = TRUE)
    z.rot <- fisherz(replicates)
    means.rot <- colMeans(z.rot, na.rm = TRUE)
    sds.rot <- apply(z.rot, 2, sd, na.rm = TRUE)
    sds.rot <- fisherz2r(sds.rot)
    ci.rot.lower <- means.rot + qnorm(p/2) * sds.rot
    ci.rot.upper <- means.rot + qnorm(1 - p/2) * sds.rot
    means.rot <- fisherz2r(means.rot)
    ci.rot.lower <- fisherz2r(ci.rot.lower)
    ci.rot.upper <- fisherz2r(ci.rot.upper)
    low.e <- apply(replicates, 2, quantile, p/2, na.rm = TRUE)
    up.e <- apply(replicates, 2, quantile, 1 - p/2, na.rm = TRUE)
    tci <- abs(means.rot)/sds.rot
    ptci <- pnorm(tci)
    ci.rot <- data.frame(lower = ci.rot.lower, low.e = low.e, 
        upper = ci.rot.upper, up.e = up.e, p = 2 * (1 - ptci))
    cnR <- abbreviate(colnames(rho), minlength = 5)
    k <- 1
    for (i in 1:(nvar - 1)) {
        for (j in (i + 1):nvar) {
            rownames(ci.rot)[k] <- paste(cnR[i], cnR[j], sep = "-")
            k <- k + 1
        }
    }
    results <- list(rho = rho, means = means.rot, sds = sds.rot, 
        tci = tci, ptci = ptci, ci = ci.rot, Call = cl, replicates = rep.rots)
    if (plot) {
        cor.plot(rho, numbers = TRUE, cuts = c(0.001, 0.01, 0.05), 
            pval = 2 * (1 - ptci), ...)
    }
    class(results) <- c("psych", "cor.ci")
    return(results)
}


winsor <- function (x, trim = 0.2, na.rm = TRUE) 
{
    if (is.vector(x)) {
        ans <- wins(x, trim = trim, na.rm = na.rm)
    }
    else {
        if (is.matrix(x) | is.data.frame(x)) {
            ans <- apply(x, 2, wins, trim = trim, na.rm = na.rm)
        }
    }
    return(ans)
}


factor2cluster <- function (loads, cut = 0) 
{
    if (!is.matrix(loads)) {
        l <- loads$loadings
    }
    else {
        l <- loads
    }
    l <- as.matrix(l)
    nrows <- dim(l)[1]
    ncols <- dim(l)[2]
    if (ncols == 1) {
        m1 <- matrix(rep(1, nrows), ncol = 1)
    }
    else {
        m1 <- matrix(apply(t(apply(l, 1, abs)), 1, which.max), 
            ncol = 1)
    }
    id <- matrix(c(1:nrows, m1), ncol = 2)
    factor2cluster <- matrix(rep(0, ncols * nrows), ncol = ncols)
    factor2cluster[id] <- sign(l[id]) * ((abs(l[id]) > cut) + 
        0)
    rownames(factor2cluster) <- rownames(l)
    colnames(factor2cluster) <- colnames(l)
    nitems <- colSums(abs(factor2cluster))
    for (i in ncols:1) {
        if (nitems[i] < 1) {
            factor2cluster <- factor2cluster[, -i, drop = FALSE]
        }
    }
    return(factor2cluster)
}


structure.graph <- function (fx, Phi = NULL, fy = NULL, out.file = NULL, labels = NULL, 
    cut = 0.3, errors = TRUE, simple = TRUE, regression = FALSE, 
    size = c(8, 6), node.font = c("Helvetica", 14), edge.font = c("Helvetica", 
        10), rank.direction = c("RL", "TB", "LR", "BT"), digits = 1, 
    title = "Structural model", ...) 
{
    if (!requireNamespace("Rgraphviz")) {
        stop("I am sorry, you need to have loaded the Rgraphviz package")
        nodes <- function() {
        }
        addEdge <- function() {
        }
        subGraph <- function() {
        }
    }
    xmodel <- fx
    ymodel <- fy
    if (!is.null(class(xmodel)) && (length(class(xmodel)) > 1)) {
        if (class(xmodel)[1] == "psych" && class(xmodel)[2] == 
            "omega") {
            Phi <- xmodel$schmid$phi
            xmodel <- xmodel$schmid$oblique
        }
        else {
            if (class(xmodel)[1] == "psych" && ((class(xmodel)[2] == 
                "fa") | (class(xmodel)[2] == "principal"))) {
                if (!is.null(xmodel$Phi)) 
                  Phi <- xmodel$Phi
                xmodel <- as.matrix(xmodel$loadings)
            }
        }
    }
    else {
        if (!is.matrix(xmodel) & !is.data.frame(xmodel) & !is.vector(xmodel)) {
            if (!is.null(xmodel$Phi)) 
                Phi <- xmodel$Phi
            xmodel <- as.matrix(xmodel$loadings)
        }
        else {
            xmodel <- xmodel
        }
    }
    if (!is.matrix(xmodel)) {
        factors <- as.matrix(xmodel)
    }
    else {
        factors <- xmodel
    }
    rank.direction <- match.arg(rank.direction)
    num.y <- 0
    num.var <- num.xvar <- dim(factors)[1]
    if (is.null(num.xvar)) {
        num.xvar <- length(factors)
        num.xfactors <- 1
    }
    else {
        num.factors <- num.xfactors <- dim(factors)[2]
    }
    if (is.null(labels)) {
        vars <- xvars <- rownames(xmodel)
    }
    else {
        xvars <- vars <- labels
    }
    if (is.null(vars)) {
        vars <- xvars <- paste("x", 1:num.xvar, sep = "")
    }
    fact <- colnames(xmodel)
    if (is.null(fact)) {
        fact <- paste("X", 1:num.xfactors, sep = "")
    }
    num.yfactors <- 0
    if (!is.null(ymodel)) {
        if (is.list(ymodel) & !is.data.frame(ymodel)) {
            ymodel <- as.matrix(ymodel$loadings)
        }
        else {
            ymodel <- ymodel
        }
        if (!is.matrix(ymodel)) {
            y.factors <- as.matrix(ymodel)
        }
        else {
            y.factors <- ymodel
        }
        num.y <- dim(y.factors)[1]
        if (is.null(num.y)) {
            num.y <- length(ymodel)
            num.yfactors <- 1
        }
        else {
            num.yfactors <- dim(y.factors)[2]
        }
        yvars <- rownames(ymodel)
        if (is.null(yvars)) {
            yvars <- paste("y", 1:num.y, sep = "")
        }
        if (is.null(labels)) {
            vars <- c(xvars, yvars)
        }
        else {
            yvars <- labels[(num.xvar + 1):(num.xvar + num.y)]
        }
        yfact <- colnames(ymodel)
        if (is.null(yfact)) {
            yfact <- paste("Y", 1:num.yfactors, sep = "")
        }
        fact <- c(fact, yfact)
        num.var <- num.xvar + num.y
        num.factors <- num.xfactors + num.yfactors
    }
    sem <- matrix(rep(NA), 6 * (num.var * num.factors + num.factors), 
        ncol = 3)
    colnames(sem) <- c("Path", "Parameter", "Value")
    edge.weights <- rep(1, num.var * 2 * num.factors)
    if (!regression) {
        k <- num.factors
        clust.graph <- new("graphNEL", nodes = c(vars, fact), 
            edgemode = "directed")
        graph.shape <- c(rep("box", num.var), rep("ellipse", 
            num.factors))
        if (num.y > 0) {
            graph.rank <- c(rep("min", num.xvar), rep("max", 
                num.y), rep("same", num.xfactors), rep("", num.yfactors))
        }
        else {
            graph.rank <- c(rep("min", num.var * 2), rep("", 
                num.factors))
        }
        names(graph.shape) <- nodes(clust.graph)
        names(graph.rank) <- nodes(clust.graph)
        edge.label <- rep("", num.var * 2 * k)
        edge.name <- rep("", num.var * 2 * k)
        names(edge.label) <- seq(1:num.var * 2 * k)
        edge.dir <- rep("forward", num.var * 2 * k)
        edge.arrows <- rep("open", num.var * 2 * k)
        if (num.xfactors == 1) {
            for (i in 1:num.xvar) {
                clust.graph <- addEdge(fact[1], vars[i], clust.graph, 
                  1)
                if (is.numeric(factors[i])) {
                  edge.label[i] <- round(factors[i], digits)
                }
                else {
                  edge.label[i] <- factors[i]
                }
                edge.name[i] <- paste(fact[1], "~", vars[i], 
                  sep = "")
                sem[i, 1] <- paste(fact[1], "->", vars[i], sep = "")
                if (is.numeric(factors[i])) {
                  sem[i, 2] <- vars[i]
                }
                else {
                  sem[i, 2] <- factors[i]
                }
            }
            k <- num.xvar + 1
        }
        else {
            k <- 1
            for (i in 1:num.xvar) {
                for (f in 1:num.xfactors) {
                  if ((!is.numeric(factors[i, f]) && (factors[i, 
                    f] != "0")) || ((is.numeric(factors[i, f]) && 
                    abs(factors[i, f]) > cut))) {
                    clust.graph <- addEdge(fact[f], vars[i], 
                      clust.graph, 1)
                    if (is.numeric(factors[i, f])) {
                      edge.label[k] <- round(factors[i, f], digits)
                    }
                    else {
                      edge.label[k] <- factors[i, f]
                    }
                    edge.name[k] <- paste(fact[f], "~", vars[i], 
                      sep = "")
                    sem[k, 1] <- paste(fact[f], "->", vars[i], 
                      sep = "")
                    if (is.numeric(factors[i, f])) {
                      sem[k, 2] <- paste("F", f, vars[i], sep = "")
                    }
                    else {
                      sem[k, 2] <- factors[i, f]
                    }
                    k <- k + 1
                  }
                }
            }
        }
        if (errors) {
            for (i in 1:num.xvar) {
                clust.graph <- addEdge(vars[i], vars[i], clust.graph, 
                  1)
                edge.name[k] <- paste(vars[i], "~", vars[i], 
                  sep = "")
                edge.arrows[k] <- "closed"
                sem[k, 1] <- paste(vars[i], "<->", vars[i], sep = "")
                sem[k, 2] <- paste("x", i, "e", sep = "")
                k <- k + 1
            }
        }
    }
    else {
        if (title == "Structural model") 
            title <- "Regression model"
        k <- num.var + 1
        yvars <- "Y1"
        clust.graph <- new("graphNEL", nodes = c(vars, yvars), 
            edgemode = "directed")
        graph.rank <- c(rep("min", num.var), rep("", 1))
        names(graph.rank) <- nodes(clust.graph)
        graph.shape <- rep("box", k)
        names(graph.shape) <- nodes(clust.graph)
        graph.rank <- c(rep("min", num.var), rep("", 1))
        names(graph.rank) <- nodes(clust.graph)
        edge.label <- rep("", k)
        edge.name <- rep("", k)
        names(edge.label) <- seq(1:k)
        edge.dir <- rep("back", k)
        names(edge.dir) <- rep("", k)
        edge.arrows <- rep("open", k)
        for (i in 1:num.xvar) {
            clust.graph <- addEdge(yvars, vars[i], clust.graph, 
                1)
            if (is.numeric(vars[i])) {
                edge.label[i] <- round(factors[i], digits)
            }
            else {
                edge.label[i] <- factors[i]
            }
            edge.name[i] <- paste(yvars, "~", vars[i], sep = "")
        }
    }
    if (!is.null(ymodel)) {
        if (num.yfactors == 1) {
            for (i in 1:num.y) {
                clust.graph <- addEdge(yvars[i], fact[1 + num.xfactors], 
                  clust.graph, 1)
                if (is.numeric(y.factors[i])) {
                  edge.label[k] <- round(y.factors[i], digits)
                }
                else {
                  edge.label[k] <- y.factors[i]
                }
                edge.name[k] <- paste(yvars[i], "~", fact[1 + 
                  num.xfactors], sep = "")
                edge.dir[k] <- paste("back")
                sem[k, 1] <- paste(fact[1 + num.xfactors], "->", 
                  yvars[i], sep = "")
                if (is.numeric(y.factors[i])) {
                  sem[k, 2] <- paste("Fy", yvars[i], sep = "")
                }
                else {
                  sem[k, 2] <- y.factors[i]
                }
                k <- k + 1
            }
        }
        else {
            for (i in 1:num.y) {
                for (f in 1:num.yfactors) {
                  if ((!is.numeric(y.factors[i, f]) && (y.factors[i, 
                    f] != "0")) || ((is.numeric(y.factors[i, 
                    f]) && abs(y.factors[i, f]) > cut))) {
                    clust.graph <- addEdge(vars[i + num.xvar], 
                      fact[f + num.xfactors], clust.graph, 1)
                    if (is.numeric(y.factors[i, f])) {
                      edge.label[k] <- round(y.factors[i, f], 
                        digits)
                    }
                    else {
                      edge.label[k] <- y.factors[i, f]
                    }
                    edge.name[k] <- paste(vars[i + num.xvar], 
                      "~", fact[f + num.xfactors], sep = "")
                    edge.dir[k] <- paste("back")
                    sem[k, 1] <- paste(fact[f + num.xfactors], 
                      "->", vars[i + num.xvar], sep = "")
                    if (is.numeric(y.factors[i, f])) {
                      sem[k, 2] <- paste("Fy", f, vars[i + num.xvar], 
                        sep = "")
                    }
                    else {
                      sem[k, 2] <- y.factors[i, f]
                    }
                    k <- k + 1
                  }
                }
            }
        }
        if (errors) {
            for (i in 1:num.y) {
                clust.graph <- addEdge(vars[i + num.xvar], vars[i + 
                  num.xvar], clust.graph, 1)
                edge.name[k] <- paste(vars[i + num.xvar], "~", 
                  vars[i + num.xvar], sep = "")
                edge.dir[k] <- paste("back")
                edge.arrows[k] <- "closed"
                sem[k, 1] <- paste(vars[i + num.xvar], "<->", 
                  vars[i + num.xvar], sep = "")
                sem[k, 2] <- paste("y", i, "e", sep = "")
                k <- k + 1
            }
        }
    }
    nAttrs <- list()
    eAttrs <- list()
    if (!is.null(labels)) {
        var.labels <- c(labels, fact)
        names(var.labels) <- nodes(clust.graph)
        nAttrs$label <- var.labels
        names(edge.label) <- edge.name
    }
    if (!regression) {
        if (!is.null(Phi)) {
            if (!is.matrix(Phi)) {
                if (!is.null(fy)) {
                  Phi <- matrix(c(1, 0, Phi, 1), ncol = 2)
                }
                else {
                  Phi <- matrix(c(1, Phi, Phi, 1), ncol = 2)
                }
            }
            if (num.xfactors > 1) {
                for (i in 2:num.xfactors) {
                  for (j in 1:(i - 1)) {
                    if ((!is.numeric(Phi[i, j]) && ((Phi[i, j] != 
                      "0") || (Phi[j, i] != "0"))) || ((is.numeric(Phi[i, 
                      j]) && abs(Phi[i, j]) > cut))) {
                      clust.graph <- addEdge(fact[i], fact[j], 
                        clust.graph, 1)
                      if (is.numeric(Phi[i, j])) {
                        edge.label[k] <- round(Phi[i, j], digits)
                      }
                      else {
                        edge.label[k] <- Phi[i, j]
                      }
                      edge.name[k] <- paste(fact[i], "~", fact[j], 
                        sep = "")
                      if (!is.numeric(Phi[i, j])) {
                        if (Phi[i, j] == Phi[j, i]) {
                          edge.dir[k] <- "both"
                          sem[k, 1] <- paste(fact[i], "<->", 
                            fact[j], sep = "")
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          if (Phi[i, j] != "0") {
                            edge.dir[k] <- "forward"
                            sem[k, 1] <- paste(fact[i], " ->", 
                              fact[j], sep = "")
                            sem[k, 2] <- paste("rF", i, "F", 
                              j, sep = "")
                          }
                          else {
                            edge.dir[k] <- "back"
                            sem[k, 1] <- paste(fact[i], "<-", 
                              fact[j], sep = "")
                            sem[k, 2] <- paste("rF", i, "F", 
                              j, sep = "")
                          }
                        }
                      }
                      else {
                        sem[k, 1] <- paste(fact[i], "<->", fact[j], 
                          sep = "")
                        edge.dir[k] <- "both"
                        if (is.numeric(Phi[i, j])) {
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          sem[k, 2] <- Phi[i, j]
                        }
                      }
                      edge.weights[k] <- 1
                      k <- k + 1
                    }
                  }
                }
            }
            if (!is.null(ymodel)) {
                for (i in 1:num.xfactors) {
                  for (j in 1:num.yfactors) {
                    if ((!is.numeric(Phi[j + num.xfactors, i]) && 
                      (Phi[j + num.xfactors, i] != "0")) || ((is.numeric(Phi[j + 
                      num.xfactors, i]) && abs(Phi[j + num.xfactors, 
                      i]) > cut))) {
                      clust.graph <- addEdge(fact[j + num.xfactors], 
                        fact[i], clust.graph, 1)
                      if (is.numeric(Phi[j + num.xfactors, i])) {
                        edge.label[k] <- round(Phi[j + num.xfactors, 
                          i], digits)
                      }
                      else {
                        edge.label[k] <- Phi[j + num.xfactors, 
                          i]
                      }
                      edge.name[k] <- paste(fact[j + num.xfactors], 
                        "~", fact[i], sep = "")
                      if (Phi[j + num.xfactors, i] != Phi[i, 
                        j + num.xfactors]) {
                        edge.dir[k] <- "back"
                        sem[k, 1] <- paste(fact[i], "->", fact[j + 
                          num.xfactors], sep = "")
                      }
                      else {
                        edge.dir[k] <- "both"
                        sem[k, 1] <- paste(fact[i], "<->", fact[j + 
                          num.xfactors], sep = "")
                      }
                      if (is.numeric(Phi[j + num.xfactors, i])) {
                        sem[k, 2] <- paste("rX", i, "Y", j, sep = "")
                      }
                      else {
                        sem[k, 2] <- Phi[j + num.xfactors, i]
                      }
                      k <- k + 1
                    }
                  }
                }
            }
        }
    }
    else {
        if (!is.null(Phi)) {
            if (!is.matrix(Phi)) 
                Phi <- matrix(c(1, Phi, 0, 1), ncol = 2)
            for (i in 2:num.xvar) {
                for (j in 1:(i - 1)) {
                  clust.graph <- addEdge(vars[i], vars[j], clust.graph, 
                    1)
                  if (is.numeric(Phi[i, j])) {
                    edge.label[k] <- round(Phi[i, j], digits)
                  }
                  else {
                    edge.label[k] <- Phi[i, j]
                  }
                  edge.name[k] <- paste(vars[i], "~", vars[j], 
                    sep = "")
                  if (Phi[i, j] != Phi[j, i]) {
                    edge.dir[k] <- "back"
                  }
                  else {
                    edge.dir[k] <- "both"
                  }
                  k <- k + 1
                }
            }
        }
        edge.arrows <- rep("open", k)
    }
    for (f in 1:num.factors) {
        sem[k, 1] <- paste(fact[f], "<->", fact[f], sep = "")
        sem[k, 3] <- "1"
        k <- k + 1
    }
    obs.xvar <- subGraph(vars[1:num.xvar], clust.graph)
    if (!is.null(ymodel)) {
        obs.yvar <- subGraph(vars[(num.xvar + 1):num.var], clust.graph)
    }
    obs.var <- subGraph(vars, clust.graph)
    if (!regression) {
        cluster.vars <- subGraph(fact, clust.graph)
    }
    else {
        cluster.vars <- subGraph(yvars, clust.graph)
    }
    observed <- list(list(graph = obs.xvar, cluster = TRUE, attrs = c(rank = "min")))
    names(edge.label) <- edge.name
    names(edge.dir) <- edge.name
    names(edge.arrows) <- edge.name
    names(edge.weights) <- edge.name
    nAttrs$shape <- graph.shape
    nAttrs$rank <- graph.rank
    eAttrs$label <- edge.label
    eAttrs$dir <- edge.dir
    eAttrs$arrowhead <- edge.arrows
    eAttrs$arrowtail <- edge.arrows
    eAttrs$weight <- edge.weights
    attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE), 
        graph = list(rankdir = rank.direction, fontsize = 6, 
            bgcolor = "white"))
    if (!is.null(ymodel)) {
        observed <- list(list(graph = obs.xvar, cluster = TRUE, 
            attrs = c(rank = "sink")), list(graph = obs.yvar, 
            cluster = TRUE, attrs = c(rank = "source")))
    }
    else {
        observed <- list(list(graph = obs.xvar, cluster = TRUE, 
            attrs = c(rank = "max")))
    }
    plot(clust.graph, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
        attrs = attrs, subGList = observed, main = title)
    if (!is.null(out.file)) {
        toDotty(clust.graph, out.file, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
            attrs = attrs)
    }
    model = sem[1:(k - 1), ]
    class(model) <- "mod"
    return(model)
}


sim.multilevel <- function (nvar = 9, ngroups = 4, ncases = 16, rwg, rbg, eta) 
{
    e.wg <- eigen(rwg)
    v.wg <- pmax(e.wg$values, 0)
    etabg <- sqrt(1 - eta^2)
    e.bg <- eigen(rbg)
    v.bg <- pmax(e.bg$values, 0)
    wg <- matrix(rnorm(nvar * ncases), ncases)
    wg <- scale(wg)
    wg <- t(e.wg$vectors %*% sqrt(diag(v.wg)) %*% t(wg))
    bg <- matrix(rnorm(nvar * ngroups), ngroups)
    bg <- scale(bg)
    bg <- e.bg$vectors %*% sqrt(diag(v.bg)) %*% t(bg)
    bg <- matrix(rep(bg, (ncases/ngroups)), nrow = ncases, byrow = TRUE)
    gr <- rep((1:ngroups), (ncases/ngroups))
    XY <- wg %*% diag(eta^2) + bg %*% diag(etabg^2)
    XY <- cbind(gr, XY)
    colnames(XY) <- c("Group", paste("V", 1:nvar, sep = ""))
    result <- list(wg = wg, bg = bg, xy = XY)
}


dia.arrow <- function (from, to, labels = NULL, scale = 1, cex = 1, adj = 2, 
    both = FALSE, pos = NULL, l.cex, gap.size = NULL, ...) 
{
    if (missing(gap.size)) 
        gap.size <- 0.2
    if (missing(l.cex)) 
        l.cex <- cex
    radius1 <- radius2 <- 0
    if (is.list(from)) {
        if (!is.null(from$radius)) {
            radius1 <- from$radius
            radius2 <- 0
            from <- from$center
        }
    }
    if (is.list(to)) {
        if (!is.null(to$radius)) {
            radius2 <- to$radius
            to <- to$center
        }
    }
    theta <- atan((from[2] - to[2])/(from[1] - to[1]))
    dist <- sqrt((to[1] - from[1])^2 + (to[2] - from[2])^2)
    if ((adj > 3) || (adj < 1)) {
        x <- (to[1] + from[1])/2
        y <- (to[2] + from[2])/2
    }
    else {
        x <- from[1] - sign(from[1] - to[1]) * (4 - adj) * cos(theta) * 
            dist/4
        y <- from[2] - sign(from[1] - to[1]) * (4 - adj) * sin(theta) * 
            dist/4
    }
    if (is.null(labels)) {
        h.size <- 0
    }
    else {
        h.size <- nchar(labels) * cex * gap.size
    }
    if (is.null(labels)) {
        v.size <- 0
    }
    else {
        v.size <- cex * 0.7 * gap.size
    }
    if (from[1] < to[1]) {
        h.size <- -h.size
        radius1 <- -radius1
        radius2 <- -radius2
    }
    x0 <- from[1] - cos(theta) * radius1
    y0 <- from[2] - sin(theta) * radius1
    xr <- x + h.size * cos(theta) * v.size
    xl <- x - h.size * cos(theta) * v.size
    yr <- y + v.size * sin(theta) * h.size
    yl <- y - v.size * sin(theta) * h.size
    xe <- to[1] + cos(theta) * radius2
    ye <- to[2] + sin(theta) * radius2
    if (!is.null(labels)) 
        text(x, y, labels, cex = l.cex, pos = pos, ...)
    arrows(x0, y0, xr, yr, length = (both + 0) * 0.1 * scale, 
        angle = 30, code = 1, ...)
    arrows(xl, yl, xe, ye, length = 0.1 * scale, angle = 30, 
        code = 2, ...)
}


VSS.plot <- function (x, title = "Very Simple Structure", line = FALSE) 
{
    op <- par(no.readonly = TRUE)
    n = dim(x)
    symb = c(49, 50, 51, 52)
    plot(x$cfit.1, ylim = c(0, 1), type = "b", ylab = "Very Simple Structure Fit", 
        xlab = "Number of Factors", pch = 49)
    if (line) 
        lines(x$fit)
    title(main = title)
    x$cfit.2[1] <- NA
    x$cfit.3[1] <- NA
    x$cfit.3[2] <- NA
    x$cfit.4[1] <- NA
    x$cfit.4[2] <- NA
    x$cfit.4[3] <- NA
    lines(x$cfit.2)
    points(x$cfit.2, pch = 50)
    lines(x$cfit.3)
    points(x$cfit.3, pch = symb[3])
    lines(x$cfit.4)
    points(x$cfit.4, pch = symb[4])
    par(op)
}


df2latex <- function (x, digits = 2, rowlabels = TRUE, apa = TRUE, short.names = TRUE, 
    font.size = "scriptsize", big.mark = NULL, drop.na = TRUE, 
    heading = "A table from the psych package in R", caption = "df2latex", 
    label = "default", char = FALSE, stars = FALSE, silent = FALSE, 
    file = NULL, append = FALSE, cut = 0, big = 0) 
{
    nvar <- dim(x)[2]
    rname <- rownames(x)
    tempx <- x
    comment <- paste("%", match.call())
    header <- paste("\\begin{table}[htpb]", "\\caption{", caption, 
        "}\n\\begin{center}\n\\begin{", font.size, "} \n\\begin{tabular}", 
        sep = "")
    if (stars) {
        if (rowlabels) {
            header <- c(header, "{l", rep("S", (nvar)), "}\n")
        }
        else {
            header <- c(header, "{", rep("S", (nvar + 1)), "}\n")
        }
    }
    else {
        if (rowlabels) {
            header <- c(header, "{l", rep("r", (nvar)), "}\n")
        }
        else {
            header <- c(header, "{", rep("r", (nvar + 1)), "}\n")
        }
    }
    if (apa) {
        header <- c(header, "\\multicolumn{", nvar, "}{l}{", 
            heading, "}", "\\cr \n \\hline ")
        footer <- paste(" \\hline ")
    }
    else {
        footer <- NULL
    }
    if (stars) {
        footer <- paste(" \\hline \n                      \n \\multicolumn{7}{l}{\\scriptsize{\\emph{Note: }\\textsuperscript{***}$p<.001$; \n                      \\textsuperscript{**}$p<.01$; \n                      \\textsuperscript{*}$p<.05$", 
            ".}}", sep = "")
    }
    else {
        footer <- paste(" \\hline ")
    }
    footer <- paste(footer, "\n\\end{tabular}\n\\end{", font.size, 
        "}\n\\end{center}\n\\label{", label, "}\n\\end{table} \n\n", 
        sep = "")
    if (!char) {
        if (!is.null(digits)) {
            if (is.numeric(x)) {
                x <- round(x, digits = digits)
            }
            else {
                x <- try(round(x, digits = digits))
            }
            if (cut > 0) 
                x[abs(x) < cut] <- NA
        }
    }
    cname <- colnames(x)
    if (short.names) 
        cname <- abbreviate(cname, minlength = digits + 3)
    names1 <- paste(cname[1:(nvar - 1)], " & ")
    lastname <- paste(cname[nvar], "\\cr \n")
    if (apa) {
        allnames <- c("Variable  &  ", names1, lastname, " \\hline \n")
    }
    else {
        if (rowlabels) {
            allnames <- c("  &  ", names1, lastname, "\\cr \n")
        }
        else {
            allnames <- c(names1, lastname, "\\cr \n")
        }
    }
    if (!char) {
        if (is.null(big.mark)) {
            x <- format(x, drop0trailing = FALSE)
            if (big > 0) {
                x[tempx > big] <- paste0("\\bf{", x[tempx > big], 
                  "}")
            }
        }
        else {
            x <- prettyNum(x, big.mark = ",", drop0trailing = FALSE)
        }
    }
    value <- apply(x, 1, paste, collapse = "  &  ")
    if (rowlabels) {
        value <- paste(sanitize.latex(rname), "  & ", value)
    }
    else {
        value <- paste("  & ", value)
    }
    values <- paste(value, "\\cr", "\n")
    if (drop.na) 
        values <- gsub("NA", "  ", values, fixed = TRUE)
    if (!silent) {
        cat(comment, "\n")
        cat(header)
        cat(allnames)
        cat(values)
        cat(footer)
    }
    result <- c(header, allnames, values, footer)
    if (!is.null(file)) 
        write.table(result, file = file, row.names = FALSE, col.names = FALSE, 
            quote = FALSE, append = append)
    invisible(result)
}


lowerMat <- function (R, digits = 2) 
{
    lowleft <- lower.tri(R, diag = TRUE)
    nvar <- ncol(R)
    nc <- digits + 3
    width <- getOption("width")
    k1 <- width/(nc + 2)
    if (is.null(colnames(R))) {
        colnames(R) <- paste("C", 1:nvar, sep = "")
    }
    if (is.null(rownames(R))) {
        rownames(R) <- paste("R", 1:nvar, sep = "")
    }
    colnames(R) <- abbreviate(colnames(R), minlength = digits + 
        3)
    nvar <- ncol(R)
    nc <- digits + 3
    if (k1 * nvar < width) {
        k1 <- nvar
    }
    k1 <- floor(k1)
    fx <- format(round(R, digits = digits))
    if (nrow(R) == ncol(R)) {
        fx[!lowleft] <- ""
    }
    for (k in seq(0, nvar, k1)) {
        if (k < nvar) {
            print(fx[(k + 1):nvar, (k + 1):min((k1 + k), nvar)], 
                quote = FALSE)
        }
    }
}


sim.poly.npl <- function (nvar = 5, n = 500, low = -2, high = 2, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, cat = 5) 
{
    cat <- cat - 1
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    if (is.null(a)) {
        a <- rep(1, nvar)
    }
    theta <- rnorm(n, mu, sd)
    item <- matrix(t(c + (z - c)/(1 + exp(a * t((-theta %+% t(d)))))), 
        n, nvar)
    item[] <- rbinom(n * nvar, cat, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(items = item, discrimination = a, difficulty = d, 
        gamma = c, zeta = z, theta = theta)
    return(result)
}


write.file <- function (x, f = NULL, row.names = FALSE, ...) 
{
    if (missing(f)) 
        f <- file.choose(TRUE)
    suffix <- file_ext(f)
    switch(suffix, txt = {
        write.table(x, f, row.names = row.names, ...)
    }, text = {
        write.table(x, f, row.names = row.names, ...)
    }, csv = {
        write.table(x, f, sep = ",", row.names = row.names, ...)
    }, Rds = {
        saveRDS(x, f)
    }, rds = {
        saveRDS(x, f)
    })
}


cosinor.plot <- function (angle, x = NULL, data = NULL, IDloc = NULL, ID = NULL, 
    hours = TRUE, period = 24, na.rm = TRUE, ylim = NULL, ylab = "observed", 
    xlab = "Time (double plotted)", main = "Cosine fit", add = FALSE, 
    multi = FALSE, typ = "l", ...) 
{
    if (!multi) {
        main <- paste("ID = ", ID, " ", x)
    }
    if (!is.null(data)) {
        if (is.matrix(data)) 
            data <- data.frame(data)
        if (!is.null(IDloc)) {
            x <- data[data[, IDloc] == ID, c(angle, x)]
            angle <- x[, 1, drop = FALSE]
        }
        else {
            x <- data[, c(angle, x)]
            angle <- x[, 1, drop = FALSE]
            main <- c(main, " ", IDloc)
        }
    }
    else {
        if (is.null(x) && is.null(data)) {
            x <- data.frame(x)
            x <- angle
            angle <- angle[1]
        }
        else {
            x <- data.frame(x)
            x <- cbind(angle, x)
            angle <- x[1]
            x <- x[-1]
        }
    }
    if (hours) {
        angle <- angle * 2 * pi/24
    }
    xx <- 1:96
    fit <- cosinor1(angle, x = x[2], period = period, na.rm = na.rm)
    m.resp <- mean(x[, 2], na.rm = TRUE)
    s.resp <- sd(x[, 2], na.rm = TRUE)
    sd.time <- sd(cos(angle[, 1]), na.rm = TRUE)
    if (missing(ylim)) 
        ylim = c(min(x[, 2], (m.resp - fit[1, 3]), na.rm = na.rm), 
            max(x[, 2], (m.resp + fit[1, 3]), na.rm = na.rm))
    if (!multi | !missing(main)) {
        main <- paste(main, " ", round(fit[1, 1], 2))
    }
    else {
        main = main
    }
    if (!add) {
        plot(xx/2, cos((xx/2 - fit[1, 1]) * pi/12) * s.resp * 
            fit[1, 2]/0.707 + m.resp, typ = typ, ylim = ylim, 
            ylab = ylab, xlab = xlab, main = main, ...)
    }
    else {
        points(xx/2, cos((xx/2 - fit[1, 1]) * pi/12) * s.resp * 
            fit[1, 2]/0.707 + m.resp, typ = typ, ylim = ylim, 
            ylab = ylab, xlab = xlab, main = main, ...)
    }
    if (!multi) {
        points(c(x[, 1], x[, 1] + 24), rep(x[, 2], 2), ...)
    }
    else {
        points(xx/2, cos((xx/2 - fit[1, 1]) * pi/12) * s.resp * 
            fit[1, 2]/0.707 + m.resp, typ = "l", ...)
    }
}


sim.poly.npn <- function (nvar = 5, n = 500, low = -2, high = 2, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, cat = 5) 
{
    cat <- cat - 1
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    if (is.null(a)) {
        a <- rep(1, nvar)
    }
    theta <- rnorm(n, mu, sd)
    item <- matrix(t(c + (z - c) * pnorm(a * t(theta %+% t(-d)))), 
        n, nvar)
    item[] <- rbinom(n * nvar, cat, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(items = item, discrimination = a, difficulty = d, 
        gamma = c, zeta = z, theta = theta)
    return(result)
}


Promax <- function (x, m = 4, normalize = FALSE, pro.m = 4) 
{
    if (missing(m)) 
        m <- pro.m
    if (!is.matrix(x) & !is.data.frame(x)) {
        if (!is.null(x$loadings)) 
            x <- as.matrix(x$loadings)
    }
    else {
        x <- x
    }
    if (ncol(x) < 2) 
        return(x)
    dn <- dimnames(x)
    xx <- stats::varimax(x)
    x <- xx$loadings
    Q <- x * abs(x)^(m - 1)
    U <- lm.fit(x, Q)$coefficients
    d <- try(diag(solve(t(U) %*% U)), silent = TRUE)
    if (class(d) == "try-error") {
        warning("Factors are exactly uncorrelated and the model produces a singular matrix. An approximation is used")
        ev <- eigen(t(U) %*% U)
        ev$values[ev$values < .Machine$double.eps] <- 100 * .Machine$double.eps
        UU <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
        diag(UU) <- 1
        d <- diag(solve(UU))
    }
    U <- U %*% diag(sqrt(d))
    dimnames(U) <- NULL
    z <- x %*% U
    U <- xx$rotmat %*% U
    ui <- solve(U)
    Phi <- ui %*% t(ui)
    dimnames(z) <- dn
    class(z) <- "loadings"
    result <- list(loadings = z, rotmat = U, Phi = Phi)
    class(result) <- c("psych", "fa")
    return(result)
}


irt.stats.like <- function (items, stats = NULL, keys = NULL, cut = 0.3) 
{
    results <- list()
    tau <- irt.tau(items)
    if (!is.null(stats)) {
        nf <- dim(stats$loadings)[2]
        diffi <- list()
        for (i in 1:nf) {
            diffi[[i]] <- tau/sqrt(1 - stats$loadings[, i]^2)
        }
        discrim <- stats$loadings/sqrt(1 - stats$loadings^2)
    }
    if (!is.null(keys)) {
        if (is.null(dim(keys))) {
            nf <- 1
        }
        else {
            nf <- dim(keys)[2]
        }
        diffi <- list()
        for (i in 1:nf) {
            diffi[[i]] <- tau
        }
        discrim <- keys
    }
    class(diffi) <- NULL
    class(discrim) <- NULL
    irt <- list(difficulty = diffi, discrimination = discrim)
    results$irt <- irt
    class(results) <- c("psych", "irt.poly")
    return(results)
}


alpha <- function (x, keys = NULL, cumulative = FALSE, title = NULL, max = 10, 
    na.rm = TRUE, check.keys = FALSE, n.iter = 1, delete = TRUE, 
    use = "pairwise", warnings = TRUE, n.obs = NULL) 
{
    alpha.1 <- function(C, R) {
        n <- dim(C)[2]
        alpha.raw <- (1 - tr(C)/sum(C)) * (n/(n - 1))
        sumR <- sum(R)
        alpha.std <- (1 - n/sumR) * (n/(n - 1))
        smc.R <- smc(R)
        G6 <- (1 - (n - sum(smc.R))/sumR)
        av.r <- (sumR - n)/(n * (n - 1))
        mod1 <- matrix(av.r, n, n)
        Res1 <- R - mod1
        GF1 = 1 - sum(Res1^2)/sum(R^2)
        Rd <- R - diag(R)
        diag(Res1) <- 0
        GF1.off <- 1 - sum(Res1^2)/sum(Rd^2)
        sn <- n * av.r/(1 - av.r)
        Q = (2 * n^2/((n - 1)^2 * (sum(C)^3))) * (sum(C) * (tr(C %*% 
            C) + (tr(C))^2) - 2 * (tr(C) * sum(C %*% C)))
        result <- list(raw = alpha.raw, std = alpha.std, G6 = G6, 
            av.r = av.r, sn = sn, Q = Q, GF1, GF1.off)
        return(result)
    }
    cl <- match.call()
    if (!is.matrix(x) && !is.data.frame(x)) 
        stop("Data must either be a data frame or a matrix")
    if (!is.null(keys)) {
        if (is.list(keys)) {
            select <- sub("-", "", unlist(keys))
            x <- x[select]
            keys <- make.keys(x, keys)
        }
    }
    nvar <- dim(x)[2]
    nsub <- dim(x)[1]
    scores <- NULL
    response.freq <- NULL
    if (!isCorrelation(x)) {
        item.var <- apply(x, 2, sd, na.rm = na.rm)
        bad <- which((item.var <= 0) | is.na(item.var))
        if ((length(bad) > 0) && delete) {
            for (baddy in 1:length(bad)) {
                warning("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted")
            }
            x <- x[, -bad]
            nvar <- nvar - length(bad)
        }
        response.freq <- response.frequencies(x, max = max)
        C <- cov(x, use = use)
    }
    else {
        C <- x
    }
    if (is.null(colnames(x))) 
        colnames(x) <- paste0("V", 1:nvar)
    p1 <- principal(x, scores = FALSE)
    if (any(p1$loadings < 0)) {
        if (check.keys) {
            if (warnings) 
                warning("Some items were negatively correlated with total scale and were automatically reversed.\n This is indicated by a negative sign for the variable name.")
            keys <- 1 - 2 * (p1$loadings < 0)
        }
        else {
            if (is.null(keys) && warnings) {
                warning("Some items were negatively correlated with the total scale and probably \nshould be reversed.  \nTo do this, run the function again with the 'check.keys=TRUE' option")
                if (warnings) 
                  cat("Some items (", rownames(p1$loadings)[(p1$loadings < 
                    0)], ") were negatively correlated with the total scale and \nprobably should be reversed.  \nTo do this, run the function again with the 'check.keys=TRUE' option")
            }
        }
    }
    if (is.null(keys)) {
        keys <- rep(1, nvar)
    }
    else {
        keys <- as.vector(keys)
        if (length(keys) < nvar) {
            temp <- keys
            keys <- rep(1, nvar)
            names(keys) <- colnames(x)
            keys[temp] <- -1
        }
    }
    key.d <- diag(keys)
    C <- key.d %*% C %*% key.d
    signkey <- strtrim(keys, 1)
    signkey[signkey == "1"] <- ""
    colnames(x) <- paste(colnames(x), signkey, sep = "")
    if (nsub != nvar) {
        if (any(keys < 0)) {
            min.item <- min(x, na.rm = na.rm)
            max.item <- max(x, na.rm = na.rm)
            adjust <- max.item + min.item
            flip.these <- which(keys < 0)
            x[, flip.these] <- adjust - x[, flip.these]
        }
        if (cumulative) {
            total <- rowSums(x, na.rm = na.rm)
        }
        else {
            total <- rowMeans(x, na.rm = na.rm)
        }
        mean.t <- mean(total, na.rm = na.rm)
        sdev <- sd(total, na.rm = na.rm)
        raw.r <- cor(total, x, use = use)
        t.valid <- colSums(!is.na(x))
    }
    else {
        total <- NULL
        totals <- TRUE
    }
    R <- cov2cor(C)
    drop.item <- vector("list", nvar)
    alpha.total <- alpha.1(C, R)
    if (nvar > 2) {
        for (i in 1:nvar) {
            drop.item[[i]] <- alpha.1(C[-i, -i, drop = FALSE], 
                R[-i, -i, drop = FALSE])
        }
    }
    else {
        drop.item[[1]] <- drop.item[[2]] <- c(rep(R[1, 2], 2), 
            smc(R)[1], R[1, 2], NA, NA)
    }
    by.item <- data.frame(matrix(unlist(drop.item), ncol = 8, 
        byrow = TRUE))
    if (max(nsub, n.obs) > nvar) {
        by.item[6] <- sqrt(by.item[6]/(max(nsub, n.obs)))
        by.item <- by.item[-c(7:8)]
        colnames(by.item) <- c("raw_alpha", "std.alpha", "G6(smc)", 
            "average_r", "S/N", "alpha se")
    }
    else {
        by.item <- by.item[-c(6:8)]
        colnames(by.item) <- c("raw_alpha", "std.alpha", "G6(smc)", 
            "average_r", "S/N")
    }
    rownames(by.item) <- colnames(x)
    Vt <- sum(R)
    item.r <- colSums(R)/sqrt(Vt)
    RC <- R
    diag(RC) <- smc(R)
    Vtc <- sum(RC)
    item.rc <- colSums(RC)/sqrt(Vtc)
    if (nvar > 1) {
        r.drop <- rep(0, nvar)
        for (i in 1:nvar) {
            v.drop <- sum(C[-i, -i, drop = FALSE])
            c.drop <- sum(C[, i]) - C[i, i]
            r.drop[i] <- c.drop/sqrt(C[i, i] * v.drop)
        }
    }
    item.means <- colMeans(x, na.rm = na.rm)
    item.sd <- apply(x, 2, sd, na.rm = na.rm)
    if (nsub > nvar) {
        Unidim <- alpha.total[7]
        Fit.off <- alpha.total[8]
        ase = sqrt(alpha.total$Q/nsub)
        alpha.total <- data.frame(alpha.total[1:5], ase = ase, 
            mean = mean.t, sd = sdev)
        colnames(alpha.total) <- c("raw_alpha", "std.alpha", 
            "G6(smc)", "average_r", "S/N", "ase", "mean", "sd")
        alpha.total <- data.frame(alpha.total[1:5], ase = ase, 
            mean = mean.t, sd = sdev)
        colnames(alpha.total) <- c("raw_alpha", "std.alpha", 
            "G6(smc)", "average_r", "S/N", "ase", "mean", "sd")
        rownames(alpha.total) <- ""
        stats <- data.frame(n = t.valid, raw.r = t(raw.r), std.r = item.r, 
            r.cor = item.rc, r.drop = r.drop, mean = item.means, 
            sd = item.sd)
    }
    else {
        if (is.null(n.obs)) {
            Unidim <- alpha.total[7]
            Fit.off <- alpha.total[8]
            alpha.total <- data.frame(alpha.total[1:5])
            colnames(alpha.total) <- c("raw_alpha", "std.alpha", 
                "G6(smc)", "average_r", "S/N")
        }
        else {
            Unidim <- alpha.total[7]
            Fit.off <- alpha.total[8]
            alpha.total <- data.frame(alpha.total[1:5], ase = sqrt(alpha.total$Q/n.obs))
            colnames(alpha.total) <- c("raw_alpha", "std.alpha", 
                "G6(smc)", "average_r", "S/N", "ase")
        }
        rownames(alpha.total) <- ""
        stats <- data.frame(r = item.r, r.cor = item.rc, r.drop = r.drop)
    }
    rownames(stats) <- colnames(x)
    if (n.iter > 1) {
        if (nsub == nvar) {
            message("bootstrapped confidence intervals require raw data")
            boot <- NULL
            boot.ci <- NULL
        }
        else {
            boot <- vector("list", n.iter)
            boot <- mclapply(1:n.iter, function(XX) {
                xi <- x[sample.int(nsub, replace = TRUE), ]
                C <- cov(xi, use = "pairwise")
                if (!is.null(keys)) {
                  key.d <- diag(keys)
                  xi <- key.d %*% C %*% key.d
                }
                R <- cov2cor(C)
                alpha.1(C, R)
            })
            boot <- matrix(unlist(boot), ncol = 8, byrow = TRUE)
            colnames(boot) <- c("raw_alpha", "std.alpha", "G6(smc)", 
                "average_r", "s/n", "ase", "Unidim", "Goodfit")
            boot.ci <- quantile(boot[, 1], c(0.025, 0.5, 0.975))
        }
    }
    else {
        boot = NULL
        boot.ci <- NULL
    }
    names(Unidim) <- "Unidim"
    names(Fit.off) <- "Fit.off"
    result <- list(total = alpha.total, alpha.drop = by.item, 
        item.stats = stats, response.freq = response.freq, keys = keys, 
        scores = total, nvar = nvar, boot.ci = boot.ci, boot = boot, 
        Unidim = Unidim, Fit = Fit.off, call = cl, title = title)
    class(result) <- c("psych", "alpha")
    return(result)
}


cor.smooth <- function (x, eig.tol = 10^-12) 
{
    eigens <- try(eigen(x), TRUE)
    if (class(eigens) == as.character("try-error")) {
        warning("I am sorry, there is something seriously wrong with the correlation matrix,\ncor.smooth failed to  smooth it because some of the eigen values are NA.  \nAre you sure you specified the data correctly?")
    }
    else {
        if (min(eigens$values) < .Machine$double.eps) {
            warning("Matrix was not positive definite, smoothing was done")
            eigens$values[eigens$values < eig.tol] <- 100 * eig.tol
            nvar <- dim(x)[1]
            tot <- sum(eigens$values)
            eigens$values <- eigens$values * nvar/tot
            cnames <- colnames(x)
            rnames <- rownames(x)
            x <- eigens$vectors %*% diag(eigens$values) %*% t(eigens$vectors)
            x <- cov2cor(x)
            colnames(x) <- cnames
            rownames(x) <- rnames
        }
    }
    return(x)
}


esem.diagram <- function (esem = NULL, labels = NULL, cut = 0.3, errors = FALSE, 
    simple = TRUE, regression = FALSE, lr = TRUE, digits = 1, 
    e.size = 0.1, adj = 2, main = "Exploratory Structural Model", 
    ...) 
{
    sort.f <- function(x) {
        nvar <- ncol(x)
        if (is.null(nvar)) {
            return(x)
        }
        else {
            nitem <- nrow(x)
            cluster <- data.frame(item <- seq(1:nitem), clust = rep(0, 
                nitem))
            cluster$clust <- apply(abs(x), 1, which.max)
            ord <- sort(cluster$clust, index.return = TRUE)
            x[1:nitem, ] <- x[ord$ix, ]
            rownames(x) <- rownames(x)[ord$ix]
            return(x)
        }
    }
    xmodel <- sort.f(esem$loadsX)
    ymodel <- sort.f(esem$loadsY)
    Phi <- esem$Phi
    num.y <- num.x <- 0
    vars <- NULL
    num.xvar <- dim(xmodel)[1]
    num.yvar <- dim(ymodel)[1]
    num.xfactors <- dim(xmodel)[2]
    num.yfactors <- dim(ymodel)[2]
    if (is.null(num.xvar)) 
        num.xvar <- length(xmodel)
    if (is.null(num.yvar)) 
        num.yvar <- length(ymodel)
    if (is.null(num.xfactors)) 
        num.xfactors <- 1
    if (is.null(num.yfactors)) 
        num.yfactors <- 1
    e.size <- e.size * 10/max(num.xvar, num.yvar)
    if (is.null(labels)) {
        xvars <- rownames(xmodel)
    }
    else {
        xvars <- vars <- labels
    }
    if (is.null(ncol(xmodel))) 
        xvars <- names(xmodel)
    fact <- colnames(xmodel)
    if (is.null(fact)) {
        fact <- paste0("X", 1:num.xfactors)
    }
    if (is.null(ncol(ymodel))) {
        yvars <- names(ymodel)
    }
    else {
        yvars <- rownames(ymodel)
    }
    if (is.null(yvars)) {
        yvars <- paste0("y", 1:num.y)
    }
    if (is.null(labels)) {
        vars <- c(xvars, yvars)
    }
    else {
        yvars <- labels[(num.xvar + 1):(num.xvar + num.y)]
    }
    yfact <- colnames(ymodel)
    if (is.null(yfact)) {
        yfact <- paste0("Y", 1:num.yfactors)
    }
    fact <- c(fact, yfact)
    num.var <- num.xvar + num.y
    num.factors <- num.xfactors + num.yfactors
    sem <- matrix(rep(NA), 6 * (num.var * num.factors + num.factors), 
        ncol = 3)
    colnames(sem) <- c("Path", "Parameter", "Value")
    var.rect <- list()
    fact.rect <- list()
    length.labels <- 0
    strwd <- try(length.labels <- max(strwidth(xvars), strwidth(yvars), 
        strwidth("abc"))/1.8, silent = TRUE)
    if (class(strwd) == "try-error") {
        length.labels = max(nchar(xvars), 3)/1.8
    }
    if (lr) {
        limx <- c(-(length.labels), max(num.xvar, num.yvar) + 
            3)
        limy <- c(0, max(num.xvar, num.yvar) + 1)
    }
    else {
        limy <- c(-(length.labels), max(num.xvar, num.yvar) + 
            3)
        limx <- c(0, max(num.xvar, num.yvar) + 1)
        if (errors) 
            limy <- c(-1, max(num.xvar, num.yvar) + 2)
    }
    scale.xaxis <- 3
    if (lr) {
        plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
            axes = FALSE, ylab = "", xlab = "", main = main)
    }
    else {
        plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
            axes = FALSE, ylab = "", xlab = "", main = main)
    }
    k <- num.factors
    x.adjust <- num.xvar/max(num.xvar, num.yvar)
    for (v in 1:num.xvar) {
        if (lr) {
            var.rect[[v]] <- dia.rect(0, (num.xvar - v + 1)/x.adjust, 
                xvars[v], xlim = limx, ylim = limy, ...)
        }
        else {
            var.rect[[v]] <- dia.rect(v, 0, xvars[v], xlim = limy, 
                ylim = limx, ...)
        }
    }
    nvar <- num.xvar
    f.scale <- (num.xvar + 1)/(num.xfactors + 1)/x.adjust
    factors <- round(xmodel, digits)
    if (is.null(ncol(factors))) 
        factors <- matrix(factors)
    if (num.xfactors > 0) {
        for (f in 1:num.xfactors) {
            if (!regression) {
                if (lr) {
                  fact.rect[[f]] <- dia.ellipse(limx[2]/scale.xaxis, 
                    (num.xfactors + 1 - f) * f.scale, fact[f], 
                    xlim = c(0, nvar), ylim = c(0, nvar), e.size = e.size, 
                    ...)
                }
                else {
                  fact.rect[[f]] <- dia.ellipse(f * f.scale, 
                    limy[2]/scale.xaxis, fact[f], ylim = c(0, 
                      nvar), xlim = c(0, nvar), e.size = e.size, 
                    ...)
                }
            }
            else {
                if (lr) {
                  fact.rect[[f]] <- dia.rect(limx[2]/scale.xaxis, 
                    (num.xfactors + 1 - f) * f.scale, fact[f], 
                    xlim = c(0, nvar), ylim = c(0, nvar), ...)
                }
                else {
                  fact.rect[[f]] <- dia.rect(f * f.scale, limy[2]/scale.xaxis, 
                    fact[f], xlim = c(0, nvar), ylim = c(0, nvar), 
                    ...)
                }
            }
            for (v in 1:num.xvar) {
                if (is.numeric(factors[v, f])) {
                  if (simple && (abs(factors[v, f]) == max(abs(factors[v, 
                    ]))) && (abs(factors[v, f]) > cut) | (!simple && 
                    (abs(factors[v, f]) > cut))) {
                    if (!regression) {
                      if (lr) {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                          labels = factors[v, f], col = ((sign(factors[v, 
                            f]) < 0) + 1), lty = ((sign(factors[v, 
                            f]) < 0) + 1), adj = f%%adj + 1)
                      }
                      else {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$top, 
                          labels = factors[v, f], col = ((sign(factors[v, 
                            f]) < 0) + 1), lty = ((sign(factors[v, 
                            f]) < 0) + 1))
                      }
                    }
                    else {
                      dia.arrow(to = fact.rect[[f]]$left, from = var.rect[[v]]$right, 
                        labels = factors[v, f], col = ((sign(factors[v, 
                          f]) < 0) + 1))
                    }
                  }
                }
                else {
                  if (factors[v, f] != "0") {
                    if (!regression) {
                      if (lr) {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                          labels = factors[v, f])
                      }
                      else {
                        dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$top, 
                          labels = factors[v, f])
                      }
                    }
                    else {
                      if (lr) {
                        dia.arrow(to = fact.rect[[f]], from = var.rect[[v]]$right, 
                          labels = factors[v, f])
                      }
                      else {
                        dia.arrow(to = fact.rect[[f]], from = var.rect[[v]]$top, 
                          labels = factors[v, f])
                      }
                    }
                  }
                }
            }
        }
        if (num.xfactors == 1) {
            for (i in 1:num.xvar) {
                sem[i, 1] <- paste(fact[1], "->", vars[i], sep = "")
                if (is.numeric(factors[i])) {
                  sem[i, 2] <- vars[i]
                }
                else {
                  sem[i, 2] <- factors[i]
                }
            }
        }
        k <- num.xvar + 1
        k <- 1
        for (i in 1:num.xvar) {
            for (f in 1:num.xfactors) {
                if ((!is.numeric(factors[i, f]) && (factors[i, 
                  f] != "0")) || ((is.numeric(factors[i, f]) && 
                  abs(factors[i, f]) > cut))) {
                  sem[k, 1] <- paste(fact[f], "->", vars[i], 
                    sep = "")
                  if (is.numeric(factors[i, f])) {
                    sem[k, 2] <- paste("F", f, vars[i], sep = "")
                  }
                  else {
                    sem[k, 2] <- factors[i, f]
                  }
                  k <- k + 1
                }
            }
        }
    }
    if (errors) {
        for (i in 1:num.xvar) {
            if (lr) {
                dia.self(var.rect[[i]], side = 2)
            }
            else {
                dia.self(var.rect[[i]], side = 1)
            }
            sem[k, 1] <- paste(vars[i], "<->", vars[i], sep = "")
            sem[k, 2] <- paste("x", i, "e", sep = "")
            k <- k + 1
        }
    }
    if (!is.null(ymodel)) {
        if (lr) {
            y.adj <- num.yvar/max(num.xvar, num.yvar)
            f.yscale <- limy[2]/(num.yfactors + 1)
            y.fadj <- 0
        }
        else {
            y.adj <- num.yvar/2
            f.yscale <- limx[2]/(num.yfactors + 1)
            y.fadj <- 0
        }
        for (v in 1:num.yvar) {
            if (lr) {
                var.rect[[v + num.xvar]] <- dia.rect(limx[2] - 
                  0.35, limy[2] - v/y.adj, yvars[v], xlim = limx, 
                  ylim = limy, ...)
            }
            else {
                var.rect[[v + num.xvar]] <- dia.rect(v/y.adj, 
                  limx[2], yvars[v], xlim = limy, ylim = limx, 
                  ...)
            }
        }
    }
    if (!is.null(ymodel)) {
        y.factors <- round(ymodel, digits)
        if (is.null(ncol(y.factors))) {
            y.factors <- matrix(y.factors)
        }
        num.y <- nrow(y.factors)
        for (f in 1:num.yfactors) {
            if (lr) {
                fact.rect[[f + num.xfactors]] <- dia.ellipse(2 * 
                  limx[2]/scale.xaxis, (num.yfactors + 1 - f) * 
                  f.yscale + y.fadj, yfact[f], xlim = c(0, nvar), 
                  ylim = c(0, nvar), e.size = e.size, ...)
            }
            else {
                fact.rect[[f + num.xfactors]] <- dia.ellipse(f * 
                  f.yscale + y.fadj, 2 * limx[2]/scale.xaxis, 
                  yfact[f], ylim = c(0, nvar), xlim = c(0, nvar), 
                  e.size = e.size, ...)
            }
            for (v in 1:num.yvar) {
                if (is.numeric(y.factors[v, f])) {
                  {
                    if (simple && (abs(y.factors[v, f]) == max(abs(y.factors[v, 
                      ]))) && (abs(y.factors[v, f]) > cut) | 
                      (!simple && (abs(factors[v, f]) > cut))) {
                      if (lr) {
                        dia.arrow(from = fact.rect[[f + num.xfactors]], 
                          to = var.rect[[v + num.xvar]]$left, 
                          labels = y.factors[v, f], col = ((sign(y.factors[v, 
                            f]) < 0) + 1), adj = f%%adj + 1)
                      }
                      else {
                        dia.arrow(from = fact.rect[[f + num.xfactors]], 
                          to = var.rect[[v + num.xvar]]$bottom, 
                          labels = y.factors[v, f], col = ((sign(y.factors[v, 
                            f]) < 0) + 1))
                      }
                    }
                  }
                }
                else {
                  if (factors[v, f] != "0") {
                    if (lr) {
                      dia.arrow(from = fact.rect[[f + num.xfactors]], 
                        to = var.rect[[v + num.xvar]]$left, labels = y.factors[v, 
                          f])
                    }
                    else {
                      dia.arrow(from = fact.rect[[f + num.xfactors]], 
                        to = var.rect[[v + num.xvar]]$bottom, 
                        labels = y.factors[v, f])
                    }
                  }
                }
            }
        }
        if (num.yfactors == 1) {
            for (i in 1:num.y) {
                sem[k, 1] <- paste(fact[1 + num.xfactors], "->", 
                  yvars[i], sep = "")
                if (is.numeric(y.factors[i])) {
                  sem[k, 2] <- paste("Fy", yvars[i], sep = "")
                }
                else {
                  sem[k, 2] <- y.factors[i]
                }
                k <- k + 1
            }
        }
        else {
            for (i in 1:num.y) {
                for (f in 1:num.yfactors) {
                  if (abs(y.factors[i, f]) > cut) {
                    sem[k, 1] <- paste(fact[f + num.xfactors], 
                      "->", vars[i + num.xvar], sep = "")
                    if (is.numeric(y.factors[i, f])) {
                      sem[k, 2] <- paste("Fy", f, vars[i + num.xvar], 
                        sep = "")
                    }
                    else {
                      sem[k, 2] <- y.factors[i, f]
                    }
                    k <- k + 1
                  }
                }
            }
        }
        if (errors) {
            for (i in 1:num.y) {
                if (lr) {
                  dia.self(var.rect[[i + num.xvar]], side = 3)
                }
                else {
                  dia.self(var.rect[[i + num.xvar]], side = 3)
                }
                sem[k, 1] <- paste(vars[i + num.xvar], "<->", 
                  vars[i + num.xvar], sep = "")
                sem[k, 2] <- paste("y", i, "e", sep = "")
                k <- k + 1
            }
        }
    }
    Phi <- round(Phi, digits)
    if (!regression) {
        if (!is.null(Phi)) {
            if (!is.matrix(Phi)) {
                if (!is.null(FALSE)) {
                  Phi <- matrix(c(1, 0, Phi, 1), ncol = 2)
                }
                else {
                  Phi <- matrix(c(1, Phi, Phi, 1), ncol = 2)
                }
            }
            if (num.xfactors > 1) {
                for (i in 2:num.xfactors) {
                  for (j in 1:(i - 1)) {
                    {
                      if ((!is.numeric(Phi[i, j]) && ((Phi[i, 
                        j] != "0") || (Phi[j, i] != "0"))) || 
                        ((is.numeric(Phi[i, j]) && abs(Phi[i, 
                          j]) > cut))) {
                        if (Phi[i, j] == Phi[j, i]) {
                          if (lr) {
                            dia.curve(from = fact.rect[[i]]$right, 
                              to = fact.rect[[j]]$right, labels = Phi[i, 
                                j], scale = 2 * (i - j)/num.xfactors)
                          }
                          else {
                            dia.curve(from = fact.rect[[i]]$top, 
                              to = fact.rect[[j]]$top, labels = Phi[i, 
                                j], scale = 2 * (i - j)/num.xfactors)
                          }
                          sem[k, 1] <- paste(fact[i], "<->", 
                            fact[j], sep = "")
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          if (Phi[i, j] != "0") {
                            if (lr) {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[j]], 
                                  to = fact.rect[[i]], labels = Phi[i, 
                                    j], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[j]]$right, 
                                  to = fact.rect[[i]]$right, 
                                  labels = Phi[i, j], scale = 2 * 
                                    (i - j)/num.xfactors)
                              }
                            }
                            else {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[j]], 
                                  to = fact.rect[[i]], labels = Phi[i, 
                                    j], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[j]]$top, 
                                  to = fact.rect[[i]]$top, labels = Phi[i, 
                                    j], scale = 2 * (i - j)/num.xfactors)
                              }
                            }
                            sem[k, 1] <- paste(fact[j], " ->", 
                              fact[i], sep = "")
                            sem[k, 2] <- paste("rF", j, "F", 
                              i, sep = "")
                          }
                          else {
                            if (lr) {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[i]], 
                                  to = fact.rect[[j]], labels = Phi[j, 
                                    i], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[i]]$right, 
                                  to = fact.rect[[j]]$right, 
                                  labels = Phi[j, i], scale = 2 * 
                                    (i - j)/num.xfactors)
                              }
                            }
                            else {
                              if (abs(i - j) < 2) {
                                dia.arrow(from = fact.rect[[i]], 
                                  to = fact.rect[[j]], labels = Phi[j, 
                                    i], scale = 2 * (i - j)/num.xfactors)
                              }
                              else {
                                dia.curved.arrow(from = fact.rect[[i]]$top, 
                                  to = fact.rect[[j]]$top, labels = Phi[j, 
                                    i], scale = 2 * (i - j)/num.xfactors)
                              }
                            }
                            sem[k, 1] <- paste(fact[i], "<-", 
                              fact[j], sep = "")
                            sem[k, 2] <- paste("rF", i, "F", 
                              j, sep = "")
                          }
                        }
                      }
                      else {
                        sem[k, 1] <- paste(fact[i], "<->", fact[j], 
                          sep = "")
                        if (is.numeric(Phi[i, j])) {
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          sem[k, 2] <- Phi[i, j]
                        }
                      }
                      k <- k + 1
                    }
                  }
                }
            }
            if (!is.null(ymodel)) {
                for (i in 1:num.xfactors) {
                  for (j in 1:num.yfactors) {
                    if ((!is.numeric(Phi[j + num.xfactors, i]) && 
                      (Phi[j + num.xfactors, i] != "0")) || ((is.numeric(Phi[j + 
                      num.xfactors, i]) && abs(Phi[j + num.xfactors, 
                      i]) > cut))) {
                      dia.arrow(from = fact.rect[[i]], to = fact.rect[[j + 
                        num.xfactors]], Phi[j + num.xfactors, 
                        i], adj = i%%adj + 1)
                      sem[k, 1] <- paste(fact[i], "->", fact[j + 
                        num.xfactors], sep = "")
                    }
                    else {
                      sem[k, 1] <- paste(fact[i], "<->", fact[j + 
                        num.xfactors], sep = "")
                    }
                    if (is.numeric(Phi[j + num.xfactors, i])) {
                      sem[k, 2] <- paste("rX", i, "Y", j, sep = "")
                    }
                    else {
                      sem[k, 2] <- Phi[j + num.xfactors, i]
                    }
                    k <- k + 1
                  }
                }
            }
        }
    }
    if (num.factors > 0) {
        for (f in 1:num.factors) {
            sem[k, 1] <- paste(fact[f], "<->", fact[f], sep = "")
            sem[k, 3] <- "1"
            k <- k + 1
        }
        model = sem[1:(k - 1), ]
        class(model) <- "mod"
        return(invisible(model))
    }
}


fa.extension <- function (Roe, fo, correct = TRUE) 
{
    cl <- match.call()
    omega <- FALSE
    if (!is.null(class(fo)[2])) {
        if (class(fo)[2] == "fa") {
            if (!is.null(fo$Phi)) {
                Phi <- fo$Phi
            }
            else {
                Phi <- NULL
            }
            fl <- fo$loadings
            fs <- fo$Structure
        }
        else {
            if (class(fo)[2] == "omega") {
                omega <- TRUE
                w <- fo$stats$weights
                fl <- fo$schmid$sl
                Phi <- NULL
                fl <- fl[, 1:(dim(fl)[2] - 3)]
                nfactors <- dim(fl)[2]
                fe <- t(t(w) %*% Roe)
                foblique <- fo$schmid$oblique
                feoblique <- t(Roe) %*% foblique %*% (solve(t(foblique) %*% 
                  (foblique)))
                feoblique <- feoblique %*% solve(fo$schmid$phi)
            }
        }
    }
    if (!omega) 
        fe <- t(Roe) %*% fl %*% (solve(t(fl) %*% (fl)))
    if (!is.null(Phi)) 
        fe <- fe %*% solve(Phi)
    if (!correct) {
        d <- diag(t(fl) %*% fo$weight)
        fe <- (fe * d)
    }
    colnames(fe) <- colnames(fl)
    if (!is.null(Phi)) {
        resid <- Roe - fl %*% Phi %*% t(fe)
    }
    else {
        resid <- fl %*% t(fe)
    }
    result <- list(loadings = fe, Phi = Phi, resid = resid, Call = cl)
    if (!omega) {
        result <- list(loadings = fe, Phi = Phi, resid = resid, 
            Call = cl)
    }
    else {
        result <- list(loadings = fe, oblique = feoblique, Phi = Phi, 
            resid = resid, Call = cl)
    }
    class(result) <- c("psych", "extension")
    return(result)
}


logistic <- function (x, d = 0, a = 1, c = 0, z = 1) 
{
    c + (z - c)/(1 + exp(a * (d - x)))
}


dia.curved.arrow <- function (from, to, labels = NULL, scale = 1, both = FALSE, ...) 
{
    radius1 <- radius2 <- 0
    if (is.list(from)) {
        if (!is.null(from$radius)) {
            radius1 <- from$radius
            radius2 <- 0
            from <- from$center
        }
    }
    if (is.list(to)) {
        if (!is.null(to$radius)) {
            radius2 <- to$radius
            to <- to$center
        }
    }
    theta <- atan((from[2] - to[2])/(from[1] - to[1]))
    if (from[1] < to[1]) {
        radius1 <- -radius1
        radius2 <- -radius2
    }
    from[1] <- from[1] - cos(theta) * radius1
    from[2] <- from[2] - sin(theta) * radius1
    to[1] <- to[1] + cos(theta) * radius2
    to[2] <- to[2] + sin(theta) * radius2
    scale <- 0.8 * scale
    n <- 40
    if (is.null(labels)) {
        shift <- 0
    }
    else {
        shift <- 4
    }
    if (abs(from[1] - to[1]) < abs(from[2] - to[2])) {
        x <- c(from[1], (from[1] + to[1])/2 + scale, to[1])
        y <- c(from[2], (from[2] + to[2])/2, to[2])
        sp <- spline(y, x, n)
        lines(sp$y[1:(n/2 - shift)], sp$x[1:(n/2 - shift)])
        lines(sp$y[(n/2 + shift):n], sp$x[(n/2 + shift):n])
        arrows(sp$y[3], sp$x[3], sp$y[1], sp$x[1], length = 0.5 * 
            abs(sp$x[2] - sp$x[1]))
        if (both) 
            arrows(sp$y[n - 3], sp$x[n - 3], sp$y[n], sp$x[n], 
                length = 0.5 * abs(sp$x[2] - sp$x[1]))
        text(sp$y[n/2], sp$x[n/2], labels, ...)
    }
    else {
        x <- c(from[1], (from[1] + to[1])/2, to[1])
        y <- c(from[2], (from[2] + to[2])/2 + scale, to[2])
        sp <- spline(x, y, n)
        lines(sp$x[1:(n/2 - shift)], sp$y[1:(n/2 - shift)])
        lines(sp$x[(n/2 + shift):n], sp$y[(n/2 + shift):n])
        if (both) {
            arrows(sp$x[3], sp$y[3], sp$x[1], sp$y[1], length = 1 * 
                abs(sp$y[2] - sp$y[1]))
            arrows(sp$x[n - 3], sp$y[n - 3], sp$x[n], sp$y[n], 
                length = 1 * abs(sp$y[2] - sp$y[1]))
        }
        else {
            if ((from[1] > to[1])) {
                arrows(sp$x[3], sp$y[3], sp$x[1], sp$y[1], length = 1 * 
                  abs(sp$y[2] - sp$y[1]))
            }
            else {
                arrows(sp$x[n - 3], sp$y[n - 3], sp$x[n], sp$y[n], 
                  length = 1 * abs(sp$y[2] - sp$y[1]))
            }
            text(sp$x[n/2], sp$y[n/2], labels, ...)
        }
    }
}


lowerUpper <- function (lower, upper = NULL, diff = FALSE) 
{
    if (is.null(upper)) {
        upper <- lower
        upper[lower.tri(upper)] <- t(upper)[lower.tri(t(upper))]
        lower <- t(lower)
        lower[lower.tri(lower)] <- t(lower)[lower.tri(lower)]
        result <- list(lower = lower, upper = upper)
    }
    else {
        if (nrow(lower) != ncol(lower)) {
            stop("lower matrix must be square")
        }
        if (nrow(upper) != ncol(upper)) {
            stop("upper matrix must be square")
        }
        if (nrow(lower) != ncol(upper)) {
            stop("lower and upper matrices must have the same dimensions")
        }
        result <- lower
        colnames(result) <- colnames(upper)
        rownames(result) <- rownames(lower)
        if (diff) 
            upper <- lower - upper
        result[lower.tri(result)] <- upper[lower.tri(upper)]
        result <- t(result)
        diag(result) <- NA
    }
    return(result)
}


diagram <- function (fit, ...) 
{
    fa <- principal <- vss <- iclust <- omega <- lavaan <- FALSE
    if (length(class(fit)) == 1) {
        if (class(fit) == "lavaan") 
            lavaan <- TRUE
    }
    if (length(class(fit)) > 1) {
        if (class(fit)[2] == "fa") 
            fa <- TRUE
        if (class(fit)[2] == "vss") 
            vss <- TRUE
        if (class(fit)[2] == "iclust") 
            iclust <- TRUE
        if (class(fit)[2] == "omega") 
            omega <- TRUE
        if (class(fit)[2] == "principal") 
            principal <- TRUE
    }
    if (fa) {
        fa.diagram(fit, ...)
    }
    if (principal) {
        fa.diagram(fit, ...)
    }
    if (iclust) {
        iclust.diagram(fit, ...)
    }
    if (omega) {
        omega.diagram(fit, ...)
    }
    if (lavaan) {
        lavaan.diagram(fit, ...)
    }
}


fac <- function (r, nfactors = 1, n.obs = NA, rotate = "oblimin", scores = "tenBerge", 
    residuals = FALSE, SMC = TRUE, covar = FALSE, missing = FALSE, 
    impute = "median", min.err = 0.001, max.iter = 50, symmetric = TRUE, 
    warnings = TRUE, fm = "minres", alpha = 0.1, oblique.scores = FALSE, 
    np.obs = NULL, use = "pairwise", cor = "cor", weight = NULL, 
    ...) 
{
    cl <- match.call()
    control <- NULL
    "fit.residuals" <- function(Psi, S, nf, S.inv, fm) {
        diag(S) <- 1 - Psi
        if (!is.null(S.inv)) 
            sd.inv <- diag(1/diag(S.inv))
        eigens <- eigen(S)
        eigens$values[eigens$values < .Machine$double.eps] <- 100 * 
            .Machine$double.eps
        if (nf > 1) {
            loadings <- eigens$vectors[, 1:nf] %*% diag(sqrt(eigens$values[1:nf]))
        }
        else {
            loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
        }
        model <- loadings %*% t(loadings)
        switch(fm, wls = {
            residual <- sd.inv %*% (S - model)^2 %*% sd.inv
        }, gls = {
            residual <- (S.inv %*% (S - model))^2
        }, uls = {
            residual <- (S - model)^2
        }, minres = {
            residual <- (S - model)^2
            diag(residual) <- 0
        }, minchi = {
            residual <- (S - model)^2
            residual <- residual * np.obs
            diag(residual) <- 0
        })
        error <- sum(residual)
    }
    "fit" <- function(S, nf, fm, covar) {
        S.smc <- smc(S, covar)
        if ((fm == "wls") | (fm == "gls")) {
            S.inv <- solve(S)
        }
        else {
            S.inv <- NULL
        }
        if (!covar && (sum(S.smc) == nf) && (nf > 1)) {
            start <- rep(0.5, nf)
        }
        else {
            start <- diag(S) - S.smc
        }
        if (fm == "ml" || fm == "mle") {
            res <- optim(start, FAfn, FAgr, method = "L-BFGS-B", 
                lower = 0.005, upper = 1, control = c(list(fnscale = 1, 
                  parscale = rep(0.01, length(start))), control), 
                nf = nf, S = S)
        }
        else {
            res <- optim(start, fit.residuals, gr = FAgr.minres, 
                method = "L-BFGS-B", lower = 0.005, upper = 1, 
                control = c(list(fnscale = 1, parscale = rep(0.01, 
                  length(start)))), nf = nf, S = S, S.inv = S.inv, 
                fm = fm)
        }
        if ((fm == "wls") | (fm == "gls")) {
            Lambda <- FAout.wls(res$par, S, nf)
        }
        else {
            Lambda <- FAout(res$par, S, nf)
        }
        result <- list(loadings = Lambda, res = res, S = S)
    }
    FAfn <- function(Psi, S, nf) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
        e <- E$values[-(1:nf)]
        e <- sum(log(e) - e) - nf + nrow(S)
        -e
    }
    FAgr <- function(Psi, S, nf) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1:nf, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1:nf] - 1, 0)), 
            nf)
        load <- diag(sqrt(Psi)) %*% load
        g <- load %*% t(load) + diag(Psi) - S
        diag(g)/Psi^2
    }
    FAgr.minres <- function(Psi, S, nf, S.inv, fm) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1:nf, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1:nf] - 1, 0)), 
            nf)
        load <- diag(sqrt(Psi)) %*% load
        g <- load %*% t(load) + diag(Psi) - S
        if (fm == "minchi") {
            g <- g * np.obs
        }
        diag(g)/Psi^2
    }
    FAout <- function(Psi, S, q) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), 
            q)
        diag(sqrt(Psi)) %*% load
    }
    FAout.wls <- function(Psi, S, q) {
        diag(S) <- 1 - Psi
        E <- eigen(S, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE] %*% diag(sqrt(E$values[1L:q, 
            drop = FALSE]), q)
        return(L)
    }
    "MRFA" <- function(S, nf) {
        com.glb <- glb.algebraic(S)
        L <- FAout.wls(1 - com.glb$solution, S, nf)
        h2 <- com.glb$solution
        result <- list(loadings = L, communality = h2)
    }
    if (fm == "mle" || fm == "MLE" || fm == "ML") 
        fm <- "ml"
    if (!any(fm %in% (c("pa", "minrank", "wls", "gls", "minres", 
        "minchi", "uls", "ml", "mle")))) {
        message("factor method not specified correctly, minimum residual (unweighted least squares  used")
        fm <- "minres"
    }
    x.matrix <- r
    n <- dim(r)[2]
    if (!isCorrelation(r)) {
        matrix.input <- FALSE
        n.obs <- dim(r)[1]
        if (missing) {
            x.matrix <- as.matrix(x.matrix)
            miss <- which(is.na(x.matrix), arr.ind = TRUE)
            if (impute == "mean") {
                item.means <- colMeans(x.matrix, na.rm = TRUE)
                x.matrix[miss] <- item.means[miss[, 2]]
            }
            else {
                item.med <- apply(x.matrix, 2, median, na.rm = TRUE)
                x.matrix[miss] <- item.med[miss[, 2]]
            }
        }
        np.obs <- count.pairwise(r)
        if (covar) {
            cor <- "cov"
        }
        if (!is.null(weight)) 
            cor <- "wtd"
        switch(cor, cor = {
            r <- cor(r, use = use)
        }, cov = {
            r <- cov(r, use = use)
            covar <- TRUE
        }, wtd = {
            r <- cor.wt(r, w = weight)$r
        }, tet = {
            r <- tetrachoric(r)$rho
        }, poly = {
            r <- polychoric(r)$rho
        }, mixed = {
            r <- mixed.cor(r, use = use)$rho
        }, Yuleb = {
            r <- YuleCor(r, , bonett = TRUE)$rho
        }, YuleQ = {
            r <- YuleCor(r, 1)$rho
        }, YuleY = {
            r <- YuleCor(r, 0.5)$rho
        })
    }
    else {
        matrix.input <- TRUE
        if (fm == "minchi") {
            if (is.null(np.obs)) {
                fm <- "minres"
                message("factor method minchi does not make sense unless we know the sample size, minres used instead")
            }
        }
        if (is.na(n.obs) && !is.null(np.obs)) 
            n.obs <- max(as.vector(np.obs))
        if (!is.matrix(r)) {
            r <- as.matrix(r)
        }
        if (!covar) {
            r <- cov2cor(r)
        }
    }
    if (!residuals) {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, np.obs = np.obs, communality = c(rep(0, 
                n)), loadings = matrix(rep(0, n * n), ncol = n), 
            fit = 0)
    }
    else {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, np.obs = np.obs, communality = c(rep(0, 
                n)), loadings = matrix(rep(0, n * n), ncol = n), 
            residual = matrix(rep(0, n * n), ncol = n), fit = 0, 
            r = r)
    }
    if (is.null(SMC)) 
        SMC = TRUE
    r.mat <- r
    Phi <- NULL
    colnames(r.mat) <- rownames(r.mat) <- colnames(r)
    if (any(is.na(r))) {
        bad <- TRUE
        tempr <- r
        wcl <- NULL
        while (bad) {
            wc <- table(which(is.na(tempr), arr.ind = TRUE))
            wcl <- c(wcl, as.numeric(names(which(wc == max(wc)))))
            tempr <- r[-wcl, -wcl]
            if (any(is.na(tempr))) {
                bad <- TRUE
            }
            else {
                bad <- FALSE
            }
        }
        cat("\nLikely variables with missing values are ", colnames(r)[wcl], 
            " \n")
        stop("I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue.\nPlease drop those variables and try again.")
    }
    if (is.logical(SMC)) {
        if (SMC) {
            if (nfactors <= n) {
                diag(r.mat) <- smc(r, covar = covar)
            }
            else {
                if (warnings) {
                  message("In fa, too many factors requested for this number of variables to use SMC for communality estimates, 1s are used instead")
                }
            }
        }
        else {
            diag(r.mat) <- 1
        }
    }
    else {
        diag(r.mat) <- SMC
    }
    orig <- diag(r)
    comm <- sum(diag(r.mat))
    err <- comm
    i <- 1
    comm.list <- list()
    if (fm == "pa") {
        e.values <- eigen(r, symmetric = symmetric)$values
        while (err > min.err) {
            eigens <- eigen(r.mat, symmetric = symmetric)
            if (nfactors > 1) {
                loadings <- eigens$vectors[, 1:nfactors] %*% 
                  diag(sqrt(eigens$values[1:nfactors]))
            }
            else {
                loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
            }
            model <- loadings %*% t(loadings)
            new <- diag(model)
            comm1 <- sum(new)
            diag(r.mat) <- new
            err <- abs(comm - comm1)
            if (is.na(err)) {
                warning("imaginary eigen value condition encountered in fa\n Try again with SMC=FALSE \n exiting fa")
                break
            }
            comm <- comm1
            comm.list[[i]] <- comm1
            i <- i + 1
            if (i > max.iter) {
                if (warnings) {
                  message("maximum iteration exceeded")
                }
                err <- 0
            }
        }
        eigens <- eigens$values
    }
    if (fm == "minrank") {
        mrfa <- MRFA(r, nfactors)
        loadings <- mrfa$loadings
        model <- loadings %*% t(loadings)
        e.values <- eigen(r)$values
        S <- r
        diag(S) <- diag(model)
        eigens <- eigen(S)$values
    }
    if ((fm == "wls") | (fm == "minres") | (fm == "minchi") | 
        (fm == "gls") | (fm == "uls") | (fm == "ml") | (fm == 
        "mle")) {
        uls <- fit(r, nfactors, fm, covar = covar)
        e.values <- eigen(r)$values
        result.res <- uls$res
        loadings <- uls$loadings
        model <- loadings %*% t(loadings)
        S <- r
        diag(S) <- diag(model)
        eigens <- eigen(S)$values
    }
    if (!is.double(loadings)) {
        warning("the matrix has produced imaginary results -- proceed with caution")
        loadings <- matrix(as.double(loadings), ncol = nfactors)
    }
    if (nfactors > 1) {
        sign.tot <- vector(mode = "numeric", length = nfactors)
        sign.tot <- sign(colSums(loadings))
        sign.tot[sign.tot == 0] <- 1
        loadings <- loadings %*% diag(sign.tot)
    }
    else {
        if (sum(loadings) < 0) {
            loadings <- -as.matrix(loadings)
        }
        else {
            loadings <- as.matrix(loadings)
        }
        colnames(loadings) <- "MR1"
    }
    switch(fm, wls = {
        colnames(loadings) <- paste("WLS", 1:nfactors, sep = "")
    }, pa = {
        colnames(loadings) <- paste("PA", 1:nfactors, sep = "")
    }, gls = {
        colnames(loadings) <- paste("GLS", 1:nfactors, sep = "")
    }, ml = {
        colnames(loadings) <- paste("ML", 1:nfactors, sep = "")
    }, minres = {
        colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
    }, minrank = {
        colnames(loadings) <- paste("MRFA", 1:nfactors, sep = "")
    }, minchi = {
        colnames(loadings) <- paste("MC", 1:nfactors, sep = "")
    })
    rownames(loadings) <- rownames(r)
    loadings[loadings == 0] <- 10^-15
    model <- loadings %*% t(loadings)
    f.loadings <- loadings
    rot.mat <- NULL
    if (rotate != "none") {
        if (nfactors > 1) {
            if (rotate == "varimax" | rotate == "Varimax" | rotate == 
                "quartimax" | rotate == "bentlerT" | rotate == 
                "geominT" | rotate == "targetT" | rotate == "bifactor" | 
                rotate == "TargetT" | rotate == "equamax" | rotate == 
                "varimin" | rotate == "specialT" | rotate == 
                "Promax" | rotate == "promax" | rotate == "cluster" | 
                rotate == "biquartimin" | rotate == "TargetQ" | 
                rotate == "specialQ") {
                Phi <- NULL
                switch(rotate, varimax = {
                  rotated <- stats::varimax(loadings)
                  loadings <- rotated$loadings
                  rot.mat <- rotated$rotmat
                }, Varimax = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::Varimax(loadings, ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, quartimax = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::quartimax(loadings, 
                    ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, bentlerT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::bentlerT(loadings, 
                    ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, geominT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::geominT(loadings, ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, targetT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::targetT(loadings, Tmat = diag(ncol(loadings)), 
                    ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, bifactor = {
                  rot <- bifactor(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, TargetT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rot <- GPArotation::targetT(loadings, Tmat = diag(ncol(loadings)), 
                    ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, equamax = {
                  rot <- equamax(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, varimin = {
                  rot <- varimin(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, specialT = {
                  rot <- specialT(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, Promax = {
                  pro <- Promax(loadings, ...)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                  rot.mat <- pro$rotmat
                }, promax = {
                  pro <- kaiser(loadings, rotate = "Promax", 
                    ...)
                  loadings <- pro$loadings
                  rot.mat <- pro$rotmat
                  Phi <- pro$Phi
                }, cluster = {
                  loadings <- varimax(loadings, ...)$loadings
                  pro <- target.rot(loadings)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                  rot.mat <- pro$rotmat
                }, biquartimin = {
                  ob <- biquartimin(loadings, ...)
                  loadings <- ob$loadings
                  Phi <- ob$Phi
                  rot.mat <- t(solve(ob$Th))
                }, TargetQ = {
                  ob <- TargetQ(loadings, ...)
                  loadings <- ob$loadings
                  Phi <- ob$Phi
                  rot.mat <- t(solve(ob$Th))
                }, specialQ = {
                  ob <- specialQ(loadings, ...)
                  loadings <- ob$loadings
                  Phi <- ob$Phi
                  rot.mat <- t(solve(pro$Th))
                })
            }
            else {
                if (rotate == "oblimin" | rotate == "quartimin" | 
                  rotate == "simplimax" | rotate == "geominQ" | 
                  rotate == "bentlerQ" | rotate == "targetQ") {
                  if (!requireNamespace("GPArotation")) {
                    warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
                    Phi <- NULL
                  }
                  else {
                    ob <- try(do.call(getFromNamespace(rotate, 
                      "GPArotation"), list(loadings, ...)))
                    if (class(ob) == as.character("try-error")) {
                      warning("The requested transformaton failed, Promax was used instead as an oblique transformation")
                      ob <- Promax(loadings)
                    }
                    loadings <- ob$loadings
                    Phi <- ob$Phi
                    rot.mat <- t(solve(ob$Th))
                  }
                }
                else {
                  message("Specified rotation not found, rotate='none' used")
                }
            }
        }
    }
    signed <- sign(colSums(loadings))
    signed[signed == 0] <- 1
    loadings <- loadings %*% diag(signed)
    if (!is.null(Phi)) {
        Phi <- diag(signed) %*% Phi %*% diag(signed)
    }
    switch(fm, wls = {
        colnames(loadings) <- paste("WLS", 1:nfactors, sep = "")
    }, pa = {
        colnames(loadings) <- paste("PA", 1:nfactors, sep = "")
    }, gls = {
        colnames(loadings) <- paste("GLS", 1:nfactors, sep = "")
    }, ml = {
        colnames(loadings) <- paste("ML", 1:nfactors, sep = "")
    }, minres = {
        colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
    }, minrank = {
        colnames(loadings) <- paste("MRFA", 1:nfactors, sep = "")
    }, uls = {
        colnames(loadings) <- paste("ULS", 1:nfactors, sep = "")
    }, minchi = {
        colnames(loadings) <- paste("MC", 1:nfactors, sep = "")
    })
    if (nfactors > 1) {
        ev.rotated <- diag(t(loadings) %*% loadings)
        ev.order <- order(ev.rotated, decreasing = TRUE)
        loadings <- loadings[, ev.order]
    }
    rownames(loadings) <- colnames(r)
    if (!is.null(Phi)) {
        Phi <- Phi[ev.order, ev.order]
    }
    class(loadings) <- "loadings"
    if (nfactors < 1) 
        nfactors <- n
    if (max(abs(loadings) > 1) && !covar) 
        warning(" A Heywood case was detected.  Examine the loadings carefully.")
    result <- factor.stats(r, loadings, Phi, n.obs = n.obs, np.obs = np.obs, 
        alpha = alpha)
    result$rotation <- rotate
    result$communality <- diag(model)
    if (fm == "minrank") {
        result$communalities <- mrfa$communality
    }
    else {
        if (fm == "pa") {
            result$communalities <- comm1
        }
        else {
            result$communalities <- 1 - result.res$par
        }
    }
    result$uniquenesses <- diag(r - model)
    result$values <- eigens
    result$e.values <- e.values
    result$loadings <- loadings
    result$model <- model
    result$fm <- fm
    result$rot.mat <- rot.mat
    if (!is.null(Phi)) {
        colnames(Phi) <- rownames(Phi) <- colnames(loadings)
        result$Phi <- Phi
        Structure <- loadings %*% Phi
    }
    else {
        Structure <- loadings
    }
    class(Structure) <- "loadings"
    result$Structure <- Structure
    if (fm == "pa") 
        result$communality.iterations <- unlist(comm.list)
    if (oblique.scores) {
        result$scores <- factor.scores(x.matrix, f = loadings, 
            Phi = Phi, method = scores)
    }
    else {
        result$scores <- factor.scores(x.matrix, f = Structure, 
            method = scores)
    }
    result$weights <- result$scores$weights
    result$scores <- result$scores$scores
    if (!is.null(result$scores)) 
        colnames(result$scores) <- colnames(loadings)
    result$factors <- nfactors
    result$r <- r
    result$np.obs <- np.obs
    result$fn <- "fa"
    result$fm <- fm
    result$Call <- cl
    class(result) <- c("psych", "fa")
    return(result)
}


dia.shape <- function (x, y = NULL, labels = NULL, cex = 1, e.size = 0.05, 
    xlim = c(0, 1), ylim = c(0, 1), shape = 1, ...) 
{
    if (shape == 1) {
        dia.rect(x, y = NULL, labels = NULL, cex = 1, xlim = c(0, 
            1), ylim = c(0, 1), ...)
    }
    if (shape == 2) {
        dia.ellipse(x, y = NULL, labels = NULL, cex = 1, e.size = 0.05, 
            xlim = c(0, 1), ylim = c(0, 1), ...)
    }
    if (shape == 3) {
        dia.triangle(x, y = NULL, labels = NULL, cex = 1, xlim = c(0, 
            1), ylim = c(0, 1), ...)
    }
}


circ.tests <- function (loads, loading = TRUE, sorting = TRUE) 
{
    cl <- match.call()
    circ.gap <- function(loads, loading = TRUE, sorting = TRUE) {
        if (loading) {
            l <- loads$loadings
        }
        else {
            l <- loads
        }
        l <- l[, 1:2]
        commun = rowSums(l * l)
        theta = sign(l[, 2]) * acos(l[, 1]/sqrt(commun))
        if (sorting) {
            theta <- sort(theta)
        }
        gaps <- diff(theta)
        test <- var(gaps)
        return(test)
    }
    circ.fisher <- function(loads, loading = TRUE) {
        if (loading) {
            l <- loads$loadings
        }
        else {
            l <- loads
        }
        l <- l[, 1:2]
        radius <- rowSums(l * l)
        test <- sd(radius)/mean(radius)
        return(test)
    }
    circ.rt <- function(loads, loading = TRUE) {
        if (loading) {
            l <- loads$loadings
        }
        else {
            l <- loads
        }
        l <- l[, 1:2]
        qmc <- rep(0, 10)
        for (i in 0:9) {
            theta <- 5 * i
            rl <- factor.rotate(l, theta, 1, 2)
            l2 <- rl * rl
            qmc[i] <- sum(apply(l2, 1, var))
        }
        test <- sd(qmc)/mean(qmc)
    }
    circ.v2 <- function(loads, loading = TRUE) {
        if (loading) {
            l <- loads$loadings
        }
        else {
            l <- loads
        }
        l <- l[, 1:2]
        crit <- rep(0, 10)
        for (i in 0:9) {
            theta <- 5 * i
            rl <- factor.rotate(l, theta, 1, 2)
            l2 <- rl * rl
            suml2 <- sum(l2)
            crit[i] <- var(l2[, 1]/suml2)
        }
        test <- sd(crit)/mean(crit)
        return(test)
    }
    gap.test <- circ.gap(loads, loading, sorting)
    fisher.test <- circ.fisher(loads, loading)
    rotation.test <- circ.rt(loads, loading)
    variance.test <- circ.v2(loads, loading)
    circ.tests <- list(gaps = gap.test, fisher = fisher.test, 
        RT = rotation.test, VT = variance.test, Call = cl)
    class(circ.tests) <- c("psych", "circ")
    return(circ.tests)
}


parcels <- function (x, size = 3, max = TRUE, flip = TRUE, congruence = FALSE) 
{
    if (nrow(x) != ncol(x)) {
        x <- cor(x, use = "pairwise")
    }
    if (congruence) {
        x <- factor.congruence(x, x)
    }
    if (max) {
        diag(x) <- 0
        nvar <- nrow(x)
        row.range <- apply(x, 1, range, na.rm = TRUE)
        row.max <- pmax(abs(row.range[1, ]), abs(row.range[2, 
            ]))
        diag(x) <- row.max
        sd.inv <- diag(1/sqrt(row.max))
        similar <- sd.inv %*% x %*% sd.inv
    }
    if (size == 2) {
        key <- parcels2(x, flip = flip)
    }
    else {
        key <- parcels3(x, flip = flip)
    }
    rownames(key) <- colnames(x)
    colnames(key) <- paste("P", 1:ncol(key), sep = "")
    return(key)
}


circadian.phase <- function (angle, x = NULL, code = NULL, data = NULL, hours = TRUE, 
    period = 24, plot = FALSE, opti = FALSE, na.rm = TRUE) 
{
    if (!is.null(data)) {
        if (is.matrix(data)) 
            data <- data.frame(data)
        if (is.character(angle)) 
            angle <- which(colnames(data) == angle)
        if (!is.null(code)) {
            if (is.character(code)) 
                codeloc <- which(colnames(data) == code)
            x <- data[, c(angle, x, codeloc)]
        }
        else {
            x <- data[, c(angle, x)]
        }
        angle <- x[1]
        x <- x[-1]
    }
    else {
        if (is.null(x) && is.null(code)) {
            angle <- data.frame(angle)
            x <- angle
            angle <- angle[, 1]
        }
        else {
            x <- data.frame(x)
            x <- cbind(angle, x)
            angle <- x[1]
            x <- x[-1]
        }
    }
    if (hours) {
        angle <- angle * 2 * pi/period
        x <- cbind(angle, x)
    }
    nvar <- dim(x)[2]
    if (is.null(code)) {
        fit <- cosinor1(angle, x[-1], period = period, opti = opti, 
            na.rm = na.rm)
        m.resp <- mean(x[, 1])
        s.resp <- sd(x[, 1])
        if (plot) {
        }
    }
    else {
        fit.list <- by(x, x[code], function(x) cosinor1(angle = x[1], 
            x = x[-c(1, which(colnames(x) == code))], period = period, 
            opti = opti, na.rm = na.rm))
        ncases <- length(fit.list)
        fit <- matrix(unlist(fit.list), nrow = ncases, byrow = TRUE)
        colnames(fit) <- c(paste(colnames(x)[-c(1, which(colnames(x) == 
            code))], "phase", sep = "."), paste(colnames(x)[-c(1, 
            which(colnames(x) == code))], "fit", sep = "."), 
            paste(colnames(x)[-c(1, which(colnames(x) == code))], 
                "amp", sep = "."), paste(colnames(x)[-c(1, which(colnames(x) == 
                code))], "sd", sep = "."), paste(colnames(x)[-c(1, 
                which(colnames(x) == code))], "mean", sep = "."), 
            paste(colnames(x)[-c(1, which(colnames(x) == code))], 
                "intercept", sep = "."))
        rownames(fit) <- names(fit.list)
    }
    x <- NA
    return(fit)
}


polychoric <- function (x, smooth = TRUE, global = TRUE, polycor = FALSE, ML = FALSE, 
    std.err = FALSE, weight = NULL, correct = 0.5, progress = TRUE, 
    na.rm = TRUE, delete = TRUE) 
{
    if (polycor) 
        message("The polycor option has been removed from the polychoric function in the psych package.  Please fix the call.")
    if (ML) 
        message("The ML option has been removed from the polychoric function in the psych package.  Please fix the call.")
    if (std.err) 
        message("The std.error option has been removed from the polychoric function in the psych package.  Please fix the call.")
    myfun <- function(x, i, j, gminx, gmaxx, gminy, gmaxy) {
        polyc(x[, i], x[, j], tau[, i], tau[, j], global = global, 
            weight = weight, correct = correct, gminx = gminx, 
            gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy)
    }
    matpLower <- function(x, nvar, gminx, gmaxx, gminy, gmaxy) {
        k <- 1
        il <- vector()
        jl <- vector()
        for (i in 2:nvar) {
            for (j in 1:(i - 1)) {
                il[k] <- i
                jl[k] <- j
                k <- k + 1
            }
        }
        poly <- mcmapply(function(i, j) myfun(x, i, j, gminx = gminx, 
            gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy), il, 
            jl)
        mat <- diag(nvar)
        if (length(dim(poly)) == 2) {
            mat[upper.tri(mat)] <- as.numeric(poly[1, ])
            mat <- t(mat) + mat
            fixed <- as.numeric(poly[3, ])
            diag(mat) <- 1
            fixed <- sum(fixed)
            if (fixed > 0) 
                message(fixed, " cells were adjusted for 0 values using the correction for continuity. Examine your data carefully.")
            return(mat)
        }
        else {
            warning("Something is wrong in polycor ")
            return(poly)
            stop("we need to quit because something was seriously wrong.  Please look at the results")
        }
    }
    if (!is.null(weight)) {
        if (length(weight) != nrow(x)) {
            stop("length of the weight vector must match the number of cases")
        }
    }
    cl <- match.call()
    nvar <- dim(x)[2]
    nsub <- dim(x)[1]
    if ((prod(dim(x)) == 4) | is.table(x)) {
        result <- polytab(x, correct = correct)
        print("You seem to have a table, I will return just one correlation.")
    }
    else {
        x <- as.matrix(x)
        if (!is.numeric(x)) {
            x <- matrix(as.numeric(x), ncol = nvar)
            message("Converted non-numeric input to numeric")
        }
        nvalues <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 
            1
        if (nvalues > 8) 
            stop("You have more than 8 categories for your items, polychoric is probably not needed")
        item.var <- apply(x, 2, sd, na.rm = na.rm)
        bad <- which((item.var <= 0) | is.na(item.var))
        if ((length(bad) > 0) & delete) {
            for (baddy in 1:length(bad)) {
                message("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted")
            }
            x <- x[, -bad]
            nvar <- nvar - length(bad)
        }
        xmin <- apply(x, 2, function(x) min(x, na.rm = TRUE))
        xmin <- min(xmin)
        x <- t(t(x) - xmin + 1)
        gminx <- gminy <- 1
        xmax <- apply(x, 2, function(x) max(x, na.rm = TRUE))
        xmax <- max(xmax)
        gmaxx <- gmaxy <- xmax
        if (min(xmax) != max(xmax)) {
            global <- FALSE
            message("The items do not have an equal number of response alternatives, global set to FALSE.")
        }
        xfreq <- apply(x, 2, tabulate, nbins = nvalues)
        n.obs <- colSums(xfreq)
        xfreq <- t(t(xfreq)/n.obs)
        tau <- qnorm(apply(xfreq, 2, cumsum))[1:(nvalues - 1), 
            ]
        if (!is.matrix(tau)) 
            tau <- matrix(tau, ncol = nvar)
        rownames(tau) <- 1:(nvalues - 1)
        colnames(tau) <- colnames(x)
        mat <- matrix(0, nvar, nvar)
        colnames(mat) <- rownames(mat) <- colnames(x)
        mat <- matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy)
        if (any(is.na(mat))) {
            message("some correlations are missing, smoothing turned off")
            smooth <- FALSE
        }
        if (smooth) {
            mat <- cor.smooth(mat)
        }
        colnames(mat) <- rownames(mat) <- colnames(x)
        tau <- t(tau)
        result <- list(rho = mat, tau = tau, n.obs = nsub, Call = cl)
        class(result) <- c("psych", "poly")
    }
    return(result)
}


polar <- function (f, sort = TRUE) 
{
    if (!is.matrix(f) && !is.data.frame(f)) {
        fload <- f$loadings
    }
    else {
        fload <- f
    }
    nf <- dim(fload)[2]
    n.var <- dim(fload)[1]
    polar <- matrix(0, nrow = n.var, ncol = nf * (nf - 1) + 1)
    if (!is.null(rownames(fload))) {
        rownames(polar) <- rownames(fload)
    }
    else {
        rownames(polar) <- paste("v", 1:n.var, sep = "")
    }
    colnames(polar) <- rep(NA, nf * (nf - 1) + 1)
    polar[, 1] <- seq(1:n.var)
    colnames(polar)[1] <- "Var"
    k <- 2
    kk <- nf * (nf - 1)/2
    for (i in 2:nf) {
        for (j in 1:(i - 1)) {
            vector.length <- fload[, i]^2 + fload[, j]^2
            theta = sign(fload[, i]) * 180 * acos(fload[, j]/sqrt(vector.length))/pi
            polar[, k] <- theta%%360
            polar[, k + kk] <- vector.length
            colnames(polar)[k] <- paste("theta", i, j, sep = "")
            colnames(polar)[k + kk] <- paste("vecl", i, j, sep = "")
            k <- k + 1
        }
    }
    if (sort) {
        polar <- polar[order(polar[, 2]), ]
    }
    return(polar)
}


mssd <- function (x, group = NULL, lag = 1, na.rm = TRUE) 
{
    if (is.null(group)) {
        if (is.vector(x)) {
            result <- sum(diff(x, lag = lag, na.rm = na.rm)^2, 
                na.rm = na.rm)/(length(x) - 1)
        }
        else {
            x <- as.matrix(x)
            n <- colSums(!is.na(x)) - 1
            result <- colSums(diff(x, lag = lag, na.rm = na.rm)^2, 
                na.rm = na.rm)/n
        }
    }
    else {
        x <- as.matrix(x)
        if (NROW(group) != NROW(x)) 
            group <- x[, group]
        nvar <- ncol(x)
        cname <- colnames(x)
        temp <- by(x, group, mssd, na.rm = na.rm, lag = lag)
        rownn <- lapply(temp, is.null)
        if (sum(as.integer(rownn)) > 0) {
            rown <- names(temp)[-which(rownn == TRUE)]
        }
        else {
            rown <- names(temp)
        }
        result <- t(matrix(unlist(temp), nrow = nvar))
        colnames(result) <- cname
        rownames(result) <- rown
    }
    return(result)
}


mediate <- function (y, x, m = NULL, data, mod = NULL, n.obs = NULL, use = "pairwise", 
    n.iter = 5000, alpha = 0.05, std = FALSE, plot = TRUE) 
{
    cl <- match.call()
    if (class(y) == "formula") {
        yn <- all.vars(y)
        y <- yn[1]
        x <- yn[2]
        m <- yn[3:length(yn)]
    }
    if (is.numeric(y)) 
        y <- colnames(data)[y]
    if (is.numeric(x)) 
        x <- colnames(data)[x]
    if (!is.null(m)) 
        if (is.numeric(m)) 
            m <- colnames(data)[m]
    if (!is.null(mod)) {
        if (is.numeric(mod)) {
            nmod <- length(mod)
            mod <- colnames(data)[mod]
        }
    }
    if (is.null(mod)) {
        nmod <- 0
    }
    else {
        nmod <- length(mod)
    }
    if (ncol(data) == nrow(data)) {
        raw <- FALSE
        if (nmod > 0) {
            stop("Moderation Analysis requires the raw data")
        }
        else {
            data <- data[c(y, x, m), c(y, x, m)]
        }
    }
    else {
        if (nmod > 0) {
            data <- data[, c(y, x, mod, m)]
        }
        else {
            data <- data[, c(y, x, m)]
        }
    }
    var.names <- list(IV = x, DV = y, med = m, mod = mod)
    if (!is.matrix(data)) 
        data <- as.matrix(data)
    if ((dim(data)[1] != dim(data)[2])) {
        n.obs = dim(data)[1]
        if (!is.null(mod)) 
            data <- scale(data, scale = FALSE)
        C <- cov(data, use = use)
        raw <- TRUE
    }
    else {
        raw <- FALSE
        C <- data
        nvar <- ncol(C)
        if (is.null(n.obs)) {
            n.obs <- 1000
            message("The data matrix was a correlation matrix and the number of subjects was not specified. \n The replication data matrices were simulated based upon the observed correlation matrix and  n.obs set to 1000")
        }
        else {
            message("The replication data matrices were simulated based upon the specified number of subjects and the observed correlation matrix.")
        }
        eX <- eigen(C)
        data <- matrix(rnorm(nvar * n.obs), n.obs)
        data <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 0)), 
            nvar) %*% t(data))
        colnames(data) <- c(y, x, m)
    }
    if (std) {
        C <- cov2cor(C)
    }
    if (nmod > 0) {
        if (!raw) {
            stop("Moderation analysis requires the raw data")
        }
        else {
            ivXm <- matrix(data[, x] * data[, mod], ncol = length(mod))
            colnames(ivXm) <- paste0(abbreviate(x), "X", abbreviate(mod))
            data <- cbind(data, ivXm)
            if (nmod > 0) {
                ivX <- c(x, mod, colnames(ivXm))
                x <- ivX
                var.names$IV <- x
            }
            C <- cov(data, use = use)
            if (std) {
                C <- cov2cor(C)
            }
        }
    }
    xy <- c(x, y)
    numx <- length(x)
    numy <- length(y)
    if (!is.null(m)) {
        numm <- length(m)
        nxy <- numx + numy
        m.matrix <- C[c(x, m), c(x, m), drop = FALSE]
    }
    else {
        numm <- 0
        nxy <- numx
    }
    df <- n.obs - nxy - 1
    xy.matrix <- C[c(x, m), y, drop = FALSE]
    total.reg <- matReg(x, y, C, n.obs)
    direct <- total.reg$beta
    if (numm > 0) {
        a.reg <- matReg(x, m, C, n.obs)
        b.reg <- matReg(c(x, m), y, C, n.obs)
        cprime.reg <- matReg(c(x, m), y, C, n.obs)
        a <- a.reg$beta
        b <- b.reg$beta[-(1:numx), , drop = FALSE]
        c <- total.reg$beta
        cprime <- cprime.reg$beta
        ab <- a %*% b
        indirect <- c - ab
        if (is.null(n.obs)) {
            message("Bootstrap is not meaningful unless raw data are provided or the number of subjects is specified.")
            mean.boot <- sd.boot <- ci.quant <- boot <- se <- tvalue <- prob <- NA
        }
        else {
            boot <- boot.mediate(data, x, y, m, n.iter = n.iter, 
                std = std, use = use)
            mean.boot <- colMeans(boot)
            sd.boot <- apply(boot, 2, sd)
            ci.quant <- apply(boot, 2, function(x) quantile(x, 
                c(alpha/2, 1 - alpha/2), na.rm = TRUE))
            mean.boot <- matrix(mean.boot)
            ci.ab <- matrix(ci.quant, nrow = 2 * numx * numy)
            boots <- list(mean = mean.boot, sd = sd.boot, ci = ci.quant, 
                ci.ab = ci.ab)
        }
    }
    else {
        a.reg <- NA
        b.reg <- NA
        c.reg <- NA
        a <- b <- c <- ab <- cprime <- boot <- boots <- indirect <- cprime.reg <- NA
    }
    result <- list(var.names = var.names, a = a, b = b, ab = ab, 
        c = c, direct = direct, indirect = indirect, cprime = cprime, 
        total.reg = total.reg, a.reg = a.reg, b.reg = b.reg, 
        cprime.reg = cprime.reg, boot = boots, boot.values = boot, 
        sdnames = colnames(data), C = C, Call = cl)
    class(result) <- c("psych", "mediate")
    if (plot) {
        if (is.null(m)) {
            moderate.diagram(result)
        }
        else {
            if (is.null(mod)) {
                mediate.diagram(result)
            }
            else {
                mediate.diagram(result, main = "Moderation model")
            }
        }
    }
    return(result)
}


TargetQ <- function (L, Tmat = diag(ncol(L)), normalize = FALSE, eps = 1e-05, 
    maxit = 1000, Target = NULL) 
{
    if (requireNamespace("GPArotation")) {
        GPArotation::GPFoblq(L, Tmat = Tmat, normalize = normalize, 
            eps = eps, maxit = maxit, method = "targetQ", Target)
    }
    else {
        stop("TargetQ requires GPArotation")
    }
}


dia.curve <- function (from, to, labels = NULL, scale = 1, ...) 
{
    radius1 <- radius2 <- 0
    if (is.list(from)) {
        if (!is.null(from$radius)) {
            radius1 <- from$radius
            radius2 <- 0
            from <- from$center
        }
    }
    if (is.list(to)) {
        if (!is.null(to$radius)) {
            radius2 <- to$radius
            to <- to$center
        }
    }
    theta <- atan((from[2] - to[2])/(from[1] - to[1]))
    if ((from[1] < to[1])) {
        radius1 <- -radius1
        radius2 <- -radius2
    }
    from[1] <- from[1] - cos(theta) * radius1
    from[2] <- from[2] - sin(theta) * radius1
    to[1] <- to[1] + cos(theta) * radius2
    to[2] <- to[2] + sin(theta) * radius2
    n <- 40
    scale <- 0.8 * scale
    if (is.null(labels)) {
        shift <- 0
    }
    else {
        shift <- 4
    }
    if (abs(from[1] - to[1]) < abs(from[2] - to[2])) {
        x <- c(from[1], (from[1] + to[1])/2 + scale, to[1])
        y <- c(from[2], (from[2] + to[2])/2, to[2])
        sp <- spline(y, x, n)
        lines(sp$y[1:(n/2 - shift)], sp$x[1:(n/2 - shift)])
        lines(sp$y[(n/2 + shift):n], sp$x[(n/2 + shift):n])
        arrows(sp$y[3], sp$x[3], sp$y[1], sp$x[1], length = 0.5 * 
            abs(sp$x[2] - sp$x[1]))
        arrows(sp$y[n - 3], sp$x[n - 3], sp$y[n], sp$x[n], length = 0.5 * 
            abs(sp$x[2] - sp$x[1]))
        text(sp$y[n/2], sp$x[n/2], labels, ...)
    }
    else {
        x <- c(from[1], (from[1] + to[1])/2, to[1])
        y <- c(from[2], (from[2] + to[2])/2 + scale, to[2])
        sp <- spline(x, y, n)
        lines(sp$x[1:(n/2 - shift)], sp$y[1:(n/2 - shift)])
        lines(sp$x[(n/2 + shift):n], sp$y[(n/2 + shift):n])
        arrows(sp$x[3], sp$y[3], sp$x[1], sp$y[1], length = 1 * 
            abs(sp$y[2] - sp$y[1]))
        arrows(sp$x[n - 3], sp$y[n - 3], sp$x[n], sp$y[n], length = 1 * 
            abs(sp$y[2] - sp$y[1]))
        text(sp$x[n/2], sp$y[n/2], labels, ...)
    }
}


sim.spherical <- function (simple = FALSE, nx = 7, ny = 12, nsub = 500, xloading = 0.55, 
    yloading = 0.55, zloading = 0.55, gloading = 0, xbias = 0, 
    ybias = 0, zbias = 0, categorical = FALSE, low = -3, high = 3, 
    truncate = FALSE, cutpoint = 0) 
{
    nvar <- nx * (ny - 1)
    errorweight <- sqrt(1 - (xloading^2 + yloading^2 + zloading^2 + 
        gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) + xbias
    truey <- rnorm(nsub) + ybias
    truez <- rnorm(nsub) + zbias
    true <- matrix(c(g, truex, truey, truez), nsub)
    if (!simple) {
        f1 <- rep(cos(seq(0, pi, length.out = nx)), each = ny - 
            1) * xloading
        f2 <- rep(cos(seq(0, 2 * pi * (ny - 1)/ny, length.out = ny - 
            1)), nx) * yloading
        f3 <- rep(sin(seq(0, 2 * pi * (ny - 1)/ny, length.out = ny - 
            1)), nx) * zloading
        f2 <- f2 * (1 - f1^2)
        f3 <- f3 * (1 - f1^2)
        f <- matrix(c(rep(gloading, (ny - 1) * nx), f1, f2, f3), 
            (ny - 1) * nx)
    }
    else {
        nvar <- 4 * ny
        f1 <- rep(c(1, 0, -1, 0), each = ny)
        f2 <- rep(c(1, 0, -1, 0), ny)
        f3 <- rep(c(0, 1, 0, -1), ny)
        f <- matrix(c(rep(gloading, ny * 4), f1, f2, f3), 4 * 
            ny)
    }
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    item <- true %*% t(f) + errorweight * error
    if (categorical) {
        item = round(item)
        item[(item <= low)] <- low
        item[(item > high)] <- high
    }
    if (truncate) {
        item[item < cutpoint] <- 0
    }
    colnames(item) <- paste("V", 1:nvar, sep = "")
    return(item)
}


block.random <- function (n, ncond = NULL) 
{
    if (is.null(ncond)) {
        ncond <- 2
        IVs <- 1
        conditions <- c(ncond)
    }
    else {
        if (length(ncond) < 2) {
            IVs <- 1
            conditions <- c(ncond)
        }
        else {
            IVs <- length(ncond)
            conditions <- ncond
            ncond <- prod(ncond)
        }
    }
    if (floor(n/ncond) * ncond != n) {
        stop("number of subjects much be a multiple of number of conditions")
    }
    blocks <- matrix(rep(NA, n * (1 + IVs)), ncol = 1 + IVs)
    rcond <- rep(NA, n)
    if (is.null(names(conditions))) {
        colnames(blocks) <- c("blocks", paste("IV", 1:IVs, sep = ""))
    }
    else {
        colnames(blocks) <- c("blocks", names(conditions))
    }
    rownames(blocks) <- paste("S", 1:n, sep = "")
    for (block in 1:(n/ncond)) {
        blocks[((block - 1) * ncond + 1):(block * ncond), 1] <- block
        rcond[((block - 1) * ncond + 1):(block * ncond)] <- sample(ncond, 
            replace = FALSE)
    }
    for (i in 1:IVs) {
        if (i < 2) {
            blocks[, i + 1] <- ceiling((rcond%%conditions[i] + 
                1))
        }
        else {
            blocks[, i + 1] <- ceiling((rcond%%prod(conditions[1:i]) + 
                1)/prod(conditions[1:(i - 1)]))
        }
    }
    return(blocks)
}


score.items <- function (keys, items, totals = FALSE, ilabels = NULL, missing = TRUE, 
    impute = "median", delete = TRUE, min = NULL, max = NULL, 
    digits = 2) 
{
    message("score.items has been replaced by scoreItems, please change your call")
    scoreItems(keys = keys, items = items, totals = totals, ilabels = ilabels, 
        missing = missing, impute = impute, delete = delete, 
        min = min, max = max, digits = digits)
}


scoreOverlap <- function (keys, r, correct = TRUE, SMC = TRUE, av.r = TRUE, item.smc = NULL, 
    impute = TRUE) 
{
    tol = sqrt(.Machine$double.eps)
    cl <- match.call()
    bad <- FALSE
    if (is.list(keys)) 
        keys <- make.keys(r, keys)
    if (!is.matrix(keys)) 
        keys <- as.matrix(keys)
    if ((dim(r)[1] != dim(r)[2])) {
        r <- cor(r, use = "pairwise")
    }
    if (any(abs(r[!is.na(r)]) > 1)) 
        warning("Something is seriously wrong with the correlation matrix, some correlations had absolute values > 1!  Please check your data.")
    if (any(is.na(r))) {
        bad <- TRUE
    }
    if (SMC && is.null(item.smc)) {
        item.smc <- smc(r)
    }
    else {
        diag(r) <- NA
        item.smc <- apply(r, 1, function(x) max(abs(x), na.rm = TRUE))
        item.smc[is.infinite(item.smc)] <- 1
        diag(r) <- 1
    }
    if (all(item.smc == 1)) 
        SMC <- FALSE
    if (!bad) {
        covar <- t(keys) %*% r %*% keys
    }
    else {
        covar <- apply(keys, 2, function(x) colSums(apply(keys, 
            2, function(x) colSums(r * x, na.rm = TRUE)) * x, 
            na.rm = TRUE))
    }
    var <- diag(covar)
    n.keys <- ncol(keys)
    item.var <- item.smc
    raw.r <- cov2cor(covar)
    key.var <- diag(t(keys) %*% keys)
    key.smc <- t(keys) %*% item.smc
    key.alpha <- ((var - key.var)/var) * (key.var/(key.var - 
        1))
    key.lambda6 <- (var - key.var + key.smc)/var
    key.alpha[is.nan(key.alpha)] <- 1
    key.alpha[!is.finite(key.alpha)] <- 1
    key.av.r <- key.alpha/(key.var - key.alpha * (key.var - 1))
    colnames(raw.r) <- rownames(raw.r) <- colnames(keys)
    names(key.lambda6) <- colnames(keys)
    key.lambda6 <- drop(key.lambda6)
    n.keys <- ncol(keys)
    sn <- key.av.r * key.var/(1 - key.av.r)
    if (!bad) {
        item.cov <- t(keys) %*% r
        raw.cov <- item.cov %*% keys
    }
    else {
        item.cov <- apply(keys, 2, function(x) colSums(r * x, 
            na.rm = TRUE))
        raw.cov <- apply(keys, 2, function(x) colSums(item.cov * 
            x, na.rm = TRUE))
        item.cov <- t(item.cov)
    }
    adj.cov <- raw.cov
    for (i in 1:(n.keys)) {
        for (j in 1:i) {
            if (av.r) {
                adj.cov[i, j] <- adj.cov[j, i] <- raw.cov[i, 
                  j] - sum(keys[, i] * keys[, j]) + sum(keys[, 
                  i] * keys[, j] * sqrt(key.av.r[i] * key.av.r[j]))
            }
            else {
                adj.cov[i, j] <- adj.cov[j, i] <- raw.cov[i, 
                  j] - sum(keys[, i] * keys[, j]) + sum(keys[, 
                  i] * keys[, j] * sqrt(item.smc[i] * abs(keys[, 
                  i]) * item.smc[j] * abs(keys[, j])))
            }
        }
    }
    scale.var <- diag(raw.cov)
    diag(adj.cov) <- diag(raw.cov)
    adj.r <- cov2cor(adj.cov)
    diag(r) <- item.var
    if (!bad) {
        item.cov <- t(keys) %*% r
    }
    else {
        item.cov <- t(apply(keys, 2, function(x) colSums(r * 
            x, na.rm = TRUE)))
    }
    if (n.keys > 1) {
        item.cor <- sqrt(diag(1/(key.lambda6 * scale.var))) %*% 
            (item.cov)
        rownames(item.cor) <- colnames(keys)
    }
    else {
        item.cor <- r %*% keys/sqrt(key.lambda6 * scale.var)
    }
    colnames(item.cor) <- colnames(r)
    item.cor <- t(item.cor)
    if (correct) {
        cluster.corrected <- correct.cor(adj.r, t(key.alpha))
        result <- list(cor = adj.r, sd = sqrt(var), corrected = cluster.corrected, 
            alpha = key.alpha, av.r = key.av.r, size = key.var, 
            sn = sn, G6 = key.lambda6, item.cor = item.cor, Call = cl)
    }
    else {
        result <- list(cor = adj.r, sd = sqrt(var), alpha = key.alpha, 
            av.r = key.av.r, size = key.var, sn = sn, G6 = key.lambda6, 
            item.cor = item.cor, Call = cl)
    }
    class(result) <- c("psych", "overlap")
    return(result)
}


sim.general <- function (nvar = 9, nfact = 3, g = 0.3, r = 0.3, n = 0) 
{
    r1 <- matrix(r, nvar/nfact, nvar/nfact)
    R <- matrix(g, nvar, nvar)
    rf <- superMatrix(r1, r1)
    if (nfact > 2) {
        for (f in 1:(nfact - 2)) {
            rf <- superMatrix(r1, rf)
        }
    }
    R <- R + rf
    diag(R) <- 1
    colnames(R) <- rownames(R) <- paste((paste("V", 1:(nvar/nfact), 
        sep = "")), rep(1:nfact, each = (nvar/nfact)), sep = "gr")
    if (n > 0) {
        eX <- eigen(R)
        x <- matrix(rnorm(nvar * n), n)
        x <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 0)), 
            nvar) %*% t(x))
        return(x)
    }
    else {
        return(R)
    }
}


circ.sim <- function (nvar = 72, nsub = 500, circum = TRUE, xloading = 0.6, 
    yloading = 0.6, gloading = 0, xbias = 0, ybias = 0, categorical = FALSE, 
    low = -3, high = 3, truncate = FALSE, cutpoint = 0) 
{
    avloading <- (xloading + yloading)/2
    errorweight <- sqrt(1 - (avloading^2 + gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) * xloading + xbias
    truey <- rnorm(nsub) * yloading + ybias
    if (circum) {
        radia <- seq(0, 2 * pi, len = nvar + 1)
        rad <- radia[which(radia < 2 * pi)]
    }
    else rad <- c(rep(0, nvar/4), rep(pi/2, nvar/4), rep(pi, 
        nvar/4), rep(3 * pi/2, nvar/4))
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    trueitem <- outer(truex, cos(rad)) + outer(truey, sin(rad))
    item <- gloading * g + trueitem + errorweight * error
    if (categorical) {
        item = round(item)
        item[(item <= low)] <- low
        item[(item > high)] <- high
    }
    if (truncate) {
        item[item < cutpoint] <- 0
    }
    return(item)
}


factor.scores <- function (x, f, Phi = NULL, method = c("Thurstone", "tenBerge", 
    "Anderson", "Bartlett", "Harman", "components"), rho = NULL, 
    impute = "none") 
{
    if (length(method) > 1) 
        method <- "tenBerge"
    if (method == "regression") 
        method <- "Thurstone"
    if (length(class(f)) > 1) {
        if (class(f)[2] == "irt.fa") 
            f <- f$fa
    }
    if (!is.matrix(f)) {
        Phi <- f$Phi
        f <- loadings(f)
        if (ncol(f) == 1) {
            method <- "Thurstone"
        }
    }
    nf <- dim(f)[2]
    if (is.null(Phi)) 
        Phi <- diag(1, nf, nf)
    if (dim(x)[1] == dim(f)[1]) {
        r <- as.matrix(x)
        square <- TRUE
    }
    else {
        square <- FALSE
        if (!is.null(rho)) {
            r <- rho
        }
        else {
            r <- cor(x, use = "pairwise")
        }
    }
    switch(method, Thurstone = {
        w <- try(solve(r, f), silent = TRUE)
        if (class(w) == "try-error") {
            message("In factor.scores, the correlation matrix is singular, an approximation is used")
            r <- cor.smooth(r)
        }
        w <- try(solve(r, f), silent = TRUE)
        if (class(w) == "try-error") {
            message("I was unable to calculate the factor score weights, factor loadings used instead")
            w <- f
        }
        colnames(w) <- colnames(f)
        rownames(w) <- rownames(f)
    }, tenBerge = {
        L <- f %*% matSqrt(Phi)
        r.5 <- invMatSqrt(r)
        r <- cor.smooth(r)
        inv.r <- try(solve(r), silent = TRUE)
        if (class(inv.r) == as.character("try-error")) {
            warning("The tenBerge based scoring could not invert the correlation matrix, regression scores found instead")
            ev <- eigen(r)
            ev$values[ev$values < .Machine$double.eps] <- 100 * 
                .Machine$double.eps
            r <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
            diag(r) <- 1
            w <- solve(r, f)
        } else {
            C <- r.5 %*% L %*% invMatSqrt(t(L) %*% inv.r %*% 
                L)
            w <- r.5 %*% C %*% matSqrt(Phi)
        }
        colnames(w) <- colnames(f)
        rownames(w) <- rownames(f)
    }, Harman = {
        m <- t(f) %*% f
        inv.m <- solve(m)
        w <- f %*% inv.m
    }, Anderson = {
        I <- diag(1, nf, nf)
        h2 <- diag(f %*% Phi %*% t(f))
        U2 <- 1 - h2
        inv.U2 <- diag(1/U2)
        w <- inv.U2 %*% f %*% invMatSqrt(t(f) %*% inv.U2 %*% 
            r %*% inv.U2 %*% f)
        colnames(w) <- colnames(f)
        rownames(w) <- rownames(f)
    }, Bartlett = {
        I <- diag(1, nf, nf)
        h2 <- diag(f %*% Phi %*% t(f))
        U2 <- 1 - h2
        inv.U2 <- diag(1/U2)
        w <- inv.U2 %*% f %*% (solve(t(f) %*% inv.U2 %*% f))
        colnames(w) <- colnames(f)
        rownames(w) <- rownames(f)
    }, none = {
        w <- NULL
    }, components = {
        w <- try(solve(r, f), silent = TRUE)
        w <- f
    })
    if (is.null(w)) {
        results <- list(scores = NULL, weights = NULL)
    }
    else {
        R2 <- diag(t(w) %*% f)
        if (any(R2 > 1) || (prod(!is.nan(R2)) < 1) || (prod(R2) < 
            0)) {
            R2[abs(R2) > 1] <- NA
            R2[R2 <= 0] <- NA
        }
        r.scores <- cov2cor(t(w) %*% r %*% w)
        if (square) {
            class(w) <- NULL
            results <- list(scores = NULL, weights = w)
            results$r.scores <- r.scores
            results$R2 <- R2
        }
        else {
            missing <- rowSums(is.na(x))
            if (impute != "none") {
                x <- data.matrix(x)
                miss <- which(is.na(x), arr.ind = TRUE)
                if (impute == "mean") {
                  item.means <- colMeans(x, na.rm = TRUE)
                  x[miss] <- item.means[miss[, 2]]
                }
                else {
                  item.med <- apply(x, 2, median, na.rm = TRUE)
                  x[miss] <- item.med[miss[, 2]]
                }
            }
            if (method != "components") {
                scores <- scale(x) %*% w
            }
            else {
                scores <- x %*% w
            }
            results <- list(scores = scores, weights = w)
            results$r.scores <- r.scores
            results$missing <- missing
            results$R2 <- R2
        }
    }
    return(results)
}


sim.minor <- function (nvar = 12, nfact = 3, n = 0, g = NULL, fbig = NULL, 
    fsmall = c(-0.2, 0.2), bipolar = TRUE) 
{
    if (is.null(fbig)) {
        loads <- c(0.8, 0.6)
    }
    else {
        loads <- fbig
    }
    loads <- sample(loads, nvar/nfact, replace = TRUE)
    if (nfact == 1) {
        fx <- matrix(loads, ncol = 1)
    }
    else {
        fx <- matrix(c(rep(c(loads, rep(0, nvar)), (nfact - 1)), 
            loads), ncol = nfact)
    }
    if (bipolar) 
        fx <- 2 * ((sample(2, nvar, replace = TRUE)%%2) - 0.5) * 
            fx
    if (!is.null(g)) {
        if (length(g) < nvar) {
            g <- sample(g, nvar, replace = TRUE)
        }
        fx <- cbind(g, fx)
    }
    fsmall <- c(fsmall, rep(0, nvar/4))
    fs <- matrix(sample(fsmall, nvar * floor(nvar/2), replace = TRUE), 
        ncol = floor(nvar/2))
    fload <- cbind(fx, fs)
    if (is.null(g)) {
        colnames(fload) <- c(paste("F", 1:nfact, sep = ""), paste("m", 
            1:(nvar/2), sep = ""))
    }
    else {
        colnames(fload) <- c("g", paste("F", 1:nfact, sep = ""), 
            paste("m", 1:(nvar/2), sep = ""))
    }
    rownames(fload) <- paste("V", 1:nvar, sep = "")
    results <- sim(fload, n = n)
    results$fload <- fload
    class(results) <- c("psych", "sim")
    return(results)
}


VSS.parallel <- function (ncases, nvariables, scree = FALSE, rotate = "none") 
{
    simdata = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    if (scree) {
        VSS.scree(simdata)
        testsim <- simdata
    }
    else {
        testsim <- VSS(simdata, 8, rotate)
    }
    return(testsim)
}


irt.select <- function (x, y) 
{
    if (is.null(dim(x$tau))) {
        typ = "tet"
    }
    else {
        typ = "poly"
    }
    rho <- x$rho[y, y]
    tau <- x$tau[y]
    n.obs <- x$n.obs
    result <- list(rho = rho, tau = tau, n.obs = n.obs)
    class(result) <- c("psych", typ)
    return(result)
}


circadian.linear.cor <- function (angle, x = NULL, data = NULL, hours = TRUE) 
{
    if (!is.null(data)) 
        angle <- data[, angle]
    if (hours) {
        angle <- angle * 2 * pi/24
    }
    if (is.null(x)) {
        x <- angle[2:dim(angle)[2]]
        angle <- angle[1]
    }
    cos.angle <- cos(angle)
    sin.angle <- sin(angle)
    cor.cos <- cor(cos.angle, x, use = "pairwise")
    cor.sin <- cor(sin.angle, x, use = "pairwise")
    if (!is.vector(angle)) {
        cor.cs <- diag(cor(cos.angle, sin.angle, use = "pairwise"))
    }
    else {
        cor.cs <- cor(cos.angle, sin.angle, use = "pairwise")
    }
    R <- sqrt((cor.cos^2 + cor.sin^2 - 2 * cor.cos * cor.sin * 
        cor.cs)/(1 - cor.cs^2)) * sign(cor.cos)
    return(R)
}


predict.psych <- function (object, data, old.data, ...) 
{
    data <- as.matrix(data)
    if (ncol(data) == 1) 
        data <- t(data)
    if (missing(old.data)) {
        data <- scale(data)
    }
    else {
        stats <- describe(old.data)
        data <- scale(data, center = stats$mean, scale = stats$sd)
    }
    wt <- object$weights
    pred <- data %*% wt
    return(pred)
}


densityBy <- function (x, grp = NULL, grp.name = NULL, ylab = "Observed", 
    xlab = "", main = "Density plot", density = 20, restrict = TRUE, 
    xlim = NULL, add = FALSE, col = NULL, pch = 20, scale = NULL, 
    ...) 
{
    SCALE = 0.3
    count.valid <- function(x) {
        sum(!is.na(x))
    }
    if (missing(col)) {
        col <- c("blue", "red")
    }
    nvar <- nvarg <- ncol(x)
    if (!is.null(grp)) {
        if (!is.data.frame(grp) && !is.list(grp) && (length(grp) < 
            NROW(x))) 
            grp <- x[, grp]
        Qnt <- apply(x, 2, function(xx) by(xx, grp, quantile, 
            prob = c(0, 1, 0.5, 0.25, 0.75), na.rm = TRUE))
        meanX <- apply(x, 2, function(xx) by(xx, grp, mean, na.rm = TRUE))
        nX <- apply(x, 2, function(xx) by(xx, grp, count.valid))
        meanX <- matrix(unlist(meanX))
        Qnt <- matrix(unlist(Qnt), nrow = 5)
        ngrp <- ncol(Qnt)/nvar
        nvarg <- ncol(Qnt)
    }
    else {
        Qnt <- apply(x, 2, quantile, prob = c(0, 1, 0.5, 0.25, 
            0.75), na.rm = TRUE)
        meanX <- apply(x, 2, mean, na.rm = TRUE)
    }
    minx <- Qnt[1, ]
    maxx <- Qnt[2, ]
    medx <- Qnt[3, ]
    Q25 <- Qnt[4, ]
    Q75 <- Qnt[5, ]
    rangex <- apply(x, 2, range, na.rm = TRUE)
    names <- colnames(x)
    tot.n.obs <- nrow(x)
    if (!is.null(grp)) {
        if (missing(grp.name)) 
            grp.name <- 1:ngrp
        names <- paste(rep(names, each = ngrp), grp.name[1:ngrp], 
            sep = " ")
        col <- rep(col, nvar * ngrp)
    }
    d <- list(nvar)
    if (missing(xlim)) 
        xlim <- c(0.5, nvarg + 0.5)
    for (i in 1:nvar) {
        if (!is.null(grp)) {
            if (restrict) {
                d[[i]] <- by(x[, i], grp, function(xx) density(xx, 
                  na.rm = TRUE, from = rangex[1, i], to = rangex[2, 
                    i]))
            }
            else {
                d[[i]] <- by(x[, i], grp, function(xx) density(xx, 
                  na.rm = TRUE))
            }
        }
        else {
            if (restrict) {
                d[[i]] <- density(x[, i], na.rm = TRUE, from = minx[i], 
                  to = maxx[i])
            }
            else {
                d[[i]] <- density(x[, i], na.rm = TRUE)
            }
        }
    }
    if (!add) {
        plot(meanX, ylim = c(min(minx), max(maxx)), xlim = xlim, 
            axes = FALSE, xlab = xlab, ylab = ylab, main = main, 
            pch = pch, ...)
        axis(1, 1:nvarg, names)
        axis(2)
        box()
    }
    if (!is.null(grp)) 
        d <- unlist(d, recursive = FALSE)
    rev <- (length(d[[1]]$y):1)
    for (i in 1:nvarg) {
        if (!is.null(scale)) {
            width <- scale * sqrt(nX[[i]]/tot.n.obs)/max(d[[i]]$y)
        }
        else {
            width <- SCALE/max(d[[i]]$y)
        }
        polygon(width * c(-d[[i]]$y, d[[i]]$y[rev]) + i, c(d[[i]]$x, 
            d[[i]]$x[rev]), density = density, col = col[i], 
            ...)
        dmd <- max(which(d[[i]]$x < medx[i]))
        d25 <- max(which(d[[i]]$x <= Q25[i]))
        d75 <- max(which(d[[i]]$x <= Q75[i]))
        segments(x0 = width * d[[i]]$y[dmd] + i, y0 = d[[i]]$x[dmd], 
            x1 = -width * d[[i]]$y[dmd] + i, y1 = d[[i]]$x[dmd], 
            lwd = 2)
        segments(x0 = width * d[[i]]$y[d25] + i, y0 = d[[i]]$x[d25], 
            x1 = -width * d[[i]]$y[d25] + i, y1 = d[[i]]$x[d25])
        segments(x0 = width * d[[i]]$y[d75] + i, y0 = d[[i]]$x[d75], 
            x1 = -width * d[[i]]$y[d75] + i, y1 = d[[i]]$x[d75])
    }
}


simulation.circ <- function (samplesize = c(100, 200, 400, 800), numberofvariables = c(16, 
    32, 48, 72)) 
{
    ncases = length(samplesize)
    nvar <- length(numberofvariables)
    results <- matrix(NaN, ncol = ncases, nrow = nvar * ncases)
    results.ls <- list()
    case <- 1
    for (ss in 1:ncases) {
        for (nv in 1:nvar) {
            circ.data <- circ.sim(nvar = numberofvariables[nv], 
                nsub = samplesize[ss])
            sim.data <- circ.sim(nvar = numberofvariables[nv], 
                nsub = samplesize[ss], circum = FALSE)
            elipse.data <- circ.sim(nvar = numberofvariables[nv], 
                nsub = samplesize[ss], yloading = 0.4)
            r.circ <- cor(circ.data)
            r.sim <- cor(sim.data)
            r.elipse <- cor(elipse.data)
            pc.circ <- principal(r.circ, 2)
            pc.sim <- principal(r.sim, 2)
            pc.elipse <- principal(r.elipse, 2)
            case <- case + 1
            results.ls[[case]] <- list(numberofvariables[nv], 
                samplesize[ss], circ.tests(pc.circ)[1:4], circ.tests(pc.elipse)[1:4], 
                circ.tests(pc.sim)[1:4])
        }
    }
    results.mat <- matrix(unlist(results.ls), ncol = 14, byrow = TRUE)
    colnames(results.mat) <- c("nvar", "n", "c-gap", "c-fisher", 
        "c-RT", "c-VT", "e-gap", "e-fisher", "e-RT", "e-VT", 
        "s-gap", "s-fisher", "s-RT", "s-VT")
    results.df <- data.frame(results.mat)
    return(results.df)
}


cor2 <- function (x, y = NULL, digits = 2, use = "pairwise", method = "pearson") 
{
    multi <- FALSE
    if (is.list(x) && is.null(y)) {
        multi <- TRUE
        n <- length(x)
        xi <- x[[1]]
        for (i in 2:n) {
            xi <- cbind(xi, x[[i]])
        }
        R <- cor(xi, use = use, method = method)
    }
    else {
        R <- cor(x, y, use = use, method = method)
    }
    if (multi) {
        lowerMat(R, digits)
    }
    else {
        print(round(R, digits))
    }
    invisible(R)
}


poly.mat <- function (x, short = TRUE, std.err = FALSE, ML = FALSE) 
{
    .Deprecated("polychoric", msg = "poly.mat is deprecated.  Please use the polychoric function instead.")
    return(polychoric(x))
}


superMatrix <- function (x, y = NULL) 
{
    if (is.list(x)) {
        if (is.null(y)) {
            y <- x[-1]
        }
        else {
            y <- list(x[-1], y)
        }
        x <- x[[1]]
    }
    if (is.list(y)) {
        if (length(y) > 1) {
            x <- superMatrix(x, y[[1]])
            xy <- superMatrix(x, y[-1])
        }
        else {
            y <- y[[1]]
            xy <- superMatrix(x, y)
        }
    }
    else {
        if (is.vector(x)) {
            x <- matrix(x)
            colnames(x) <- "X"
            if (dim(x)[1] < 2) {
                rownames(x) <- "X"
            }
            else {
                rownames(x) <- paste("Vx", 1:dim(x)[1], sep = "")
            }
        }
        else {
            if (is.null(colnames(x))) 
                colnames(x) <- paste("X", 1:dim(x)[2], sep = "")
            if (is.null(rownames(x))) 
                rownames(x) <- paste("Vx", 1:dim(x)[1], sep = "")
        }
        if (is.vector(y)) {
            y <- matrix(y)
            colnames(y) <- "Y"
            if (dim(y)[1] < 2) {
                rownames(y) <- "Y"
            }
            else {
                rownames(y) <- paste("Vy", 1:dim(y)[1], sep = "")
            }
        }
        else {
            if (is.null(colnames(y))) 
                colnames(y) <- paste("Y", 1:dim(y)[2], sep = "")
            if (is.null(rownames(y))) 
                rownames(y) <- paste("Vy", 1:dim(y)[1], sep = "")
        }
        fillx <- rbind(x, matrix(0, ncol = dim(x)[2], nrow = dim(y)[1]))
        filly <- rbind(matrix(0, ncol = dim(y)[2], nrow = dim(x)[1]), 
            y)
        xy <- cbind(fillx, filly)
        colnames(xy) <- c(colnames(x), colnames(y))
        rownames(xy) <- c(rownames(x), rownames(y))
    }
    return(xy)
}


count.pairwise <- function (x, y = NULL, diagonal = TRUE) 
{
    if (is.null(y)) {
        n <- t(!is.na(x)) %*% (!is.na(x))
    }
    else {
        n <- t(!is.na(x)) %*% (!is.na(y))
    }
    if (!diagonal) 
        diag(n) <- NA
    return(n)
}


score.multiple.choice <- function (key, data, score = TRUE, totals = FALSE, ilabels = NULL, 
    missing = TRUE, impute = "median", digits = 2, short = TRUE, 
    skew = FALSE) 
{
    cl <- match.call()
    if (!is.matrix(data)) {
        if (!is.data.frame(data)) {
            stop("data must be either a data frame or matrix!")
        }
        else data <- as.matrix(data)
    }
    nvar <- dim(data)[2]
    response.freq <- response.frequencies(data)
    alternatives <- dim(response.freq)[2]
    if (length(key) == nvar) {
        items <- t(t(data) == key[])
        items <- items + 0
    }
    else {
        stop("key must have as many elements as columns of 'data' ")
    }
    if (score) {
        if (skew) {
            item.stats <- describe(items, ranges = FALSE, skew = skew, 
                fast = FALSE)[, 2:7]
        }
        else {
            item.stats <- describe(items, ranges = FALSE, skew = skew, 
                fast = TRUE)[, 2:4]
        }
        miss.rep <- rowSums(is.na(items))
        if (missing) {
            miss <- which(is.na(items), arr.ind = TRUE)
            if (impute == "mean") {
                item.means <- colMeans(items, na.rm = TRUE)
                items[miss] <- item.means[miss[, 2]]
            }
            else {
                item.med <- apply(items, 2, median, na.rm = TRUE)
                items[miss] <- item.med[miss[, 2]]
            }
        }
        keys <- rep(1, nvar)
        scores <- rowSums(items, na.rm = TRUE)
        slabels <- colnames(keys)
        if (is.null(slabels)) {
            if (totals) {
                slabels <- paste("Totals")
            }
            else {
                slabels <- paste("Averages")
            }
        }
        names(scores) <- slabels
        r.items <- cov(items, use = "pairwise")
        sum.item.var <- tr(r.items)
        var.scales <- sum(r.items)
        alpha.scale <- (var.scales - sum.item.var) * nvar/((nvar - 
            1) * var.scales)
        av.r <- alpha.scale/(nvar - alpha.scale * (nvar - 1))
        item.cor <- cor(items, scores, use = "pairwise")
        if (is.null(ilabels)) {
            ilabels <- paste("I", 1:nvar, sep = "")
        }
        rownames(item.cor) <- ilabels
        if (!totals) {
            scores <- scores/nvar
        }
        item.stats <- cbind(key, response.freq, item.cor, item.stats)
        colnames(item.stats)[alternatives + 2] <- "r"
        if (short) {
            results <- list(item.stats = round(item.stats, digits), 
                alpha = round(alpha.scale, digits), av.r = round(av.r, 
                  digits), Call = cl)
        }
        else if (sum(miss.rep) > 0) {
            results <- list(scores = scores, missing = miss.rep, 
                item.stats = round(item.stats, digits), alpha = round(alpha.scale, 
                  digits), av.r = round(av.r, digits))
        }
        else {
            results <- list(scores = scores, item.stats = item.stats, 
                alpha = round(alpha.scale, digits), av.r = round(av.r, 
                  digits), Call = cl)
        }
        class(results) <- c("psych", "mchoice")
        return(results)
    }
    else {
        return(items)
    }
}


errorCircles <- function (x, y, data, ydata = NULL, group = NULL, paired = FALSE, 
    labels = NULL, main = NULL, xlim = NULL, ylim = NULL, xlab = NULL, 
    ylab = NULL, add = FALSE, pos = NULL, offset = 1, arrow.len = 0.2, 
    alpha = 0.05, sd = FALSE, bars = TRUE, circles = TRUE, colors = NULL, 
    col.arrows = NULL, col.text = NULL, circle.size = 1, ...) 
{
    xvar <- x
    yvar <- y
    if (is.null(colors)) 
        colors <- "black"
    if (is.null(col.arrows)) 
        col.arrows <- colors
    if (is.null(col.text)) 
        col.text <- colors
    if (!is.null(group)) {
        data <- statsBy(data, group = group)
    }
    x <- list()
    if (paired) {
        x$mean <- t(data$mean[, xvar])
        x$sd <- t(data$sd[, xvar])
        x$n <- t(data$n[, xvar])
    }
    else {
        x$mean <- data$mean[, xvar]
        x$sd <- data$sd[, xvar]
        x$n <- data$n[, xvar]
    }
    xmin <- min(x$mean, na.rm = TRUE)
    xmax <- max(x$mean, na.rm = TRUE)
    x$se <- x$sd/sqrt(x$n)
    if (sd) {
        max.sex <- max(x$sd, na.rm = TRUE)
        if (is.null(xlim)) {
            xlim = c(xmin - max.sex, xmax + max.sex)
        }
    }
    else {
        max.sex <- max(x$se, na.rm = TRUE)
    }
    y <- list()
    if (!is.null(ydata)) {
        y$mean <- ydata$mean[, yvar]
        y$sd <- ydata$sd[, yvar]
        y$n <- ydata$n[, yvar]
    }
    else {
        y$mean <- data$mean[, yvar]
        y$sd <- data$sd[, yvar]
        y$n <- data$n[, yvar]
    }
    ymin <- min(y$mean, na.rm = TRUE)
    ymax <- max(y$mean, na.rm = TRUE)
    y$se <- y$sd/sqrt(y$n)
    if (sd) {
        max.sey <- max(y$sd, na.rm = TRUE)
        if (is.null(ylim)) {
            ylim = c(ymin - max.sey, ymax + max.sey)
        }
    }
    else {
        max.sey <- max(y$se, na.rm = TRUE)
    }
    if (is.null(xlim)) 
        xlim = c(xmin - 2 * max.sex, xmax + 2 * max.sex)
    if (is.null(ylim)) 
        ylim = c(ymin - 2 * max.sey, ymax + 2 * max.sey)
    if (is.null(main)) {
        if (!sd) {
            main = paste((1 - alpha) * 100, "% confidence limits", 
                sep = "")
        }
        else {
            main = paste("Means and standard deviations")
        }
    }
    if (paired) {
        if (is.null(xlab)) 
            xlab <- "Group 1"
        if (is.null(ylab)) 
            ylab <- "Group 2"
    }
    else {
        if (is.null(xlab)) 
            xlab <- colnames(data$mean)[xvar]
        if (is.null(ylab)) 
            ylab <- colnames(data$mean)[yvar]
    }
    if (add) {
        if (paired) {
            points(x$mean, typ = "p", col = colors, ...)
        }
        else {
            points(x$mean, y$mean, typ = "p", col = colors, ...)
        }
    }
    else {
        if (paired) {
            plot(x$mean, xlim = xlim, ylim = ylim, xlab = xlab, 
                ylab = ylab, main = main, typ = "p", col = colors, 
                ...)
        }
        else {
            plot(x$mean, y$mean, xlim = xlim, ylim = ylim, xlab = xlab, 
                ylab = ylab, main = main, typ = "p", col = colors, 
                ...)
        }
    }
    N <- x$n
    Nmax <- max(N)
    cix <- qt(1 - alpha/2, x$n - 1)
    ciy <- qt(1 - alpha/2, y$n - 1)
    if (paired) {
        z <- nrow(x$mean)
    }
    else {
        z <- length(x$mean)
    }
    if (sd) {
        x$se <- x$sd
        y$se <- y$sd
        cix <- ciy <- rep(1, z)
    }
    if (is.null(pos)) {
        locate <- rep(1, z)
    }
    else {
        locate <- pos
    }
    if (is.null(labels)) {
        labels <- rownames(x$mean)
    }
    if (is.null(labels)) {
        lab <- paste("V", 1:z, sep = "")
    }
    else {
        lab <- labels
    }
    if (length(colors) < z) 
        colors <- rep(colors, z)
    if (length(col.text) < z) 
        col.text <- rep(col.text, z)
    if (length(col.arrows) < z) 
        col.arrows <- rep(col.arrows, z)
    for (i in 1:z) {
        if (paired) {
            xcen <- x$mean[i, 1]
            ycen <- x$mean[i, 2]
            xse <- x$se[i, 1]
            yse <- x$se[i, 2]
        }
        else {
            xcen <- x$mean[i]
            ycen <- y$mean[i]
            xse <- x$se[i]
            yse <- y$se[i]
        }
        if (bars) {
            if (max(x$se, na.rm = TRUE) > 0) 
                arrows(xcen - cix[i] * xse, ycen, xcen + cix[i] * 
                  xse, ycen, length = arrow.len, angle = 90, 
                  code = 3, col = col.arrows[i], lty = NULL, 
                  lwd = par("lwd"), xpd = NULL)
            if (max(y$se, na.rm = TRUE) > 0) 
                arrows(xcen, ycen - ciy[i] * yse, xcen, ycen + 
                  ciy[i] * yse, length = arrow.len, angle = 90, 
                  code = 3, col = col.arrows[i], lty = NULL, 
                  lwd = par("lwd"), xpd = NULL)
        }
        text(xcen, ycen, labels = lab[i], pos = locate[i], col = col.text[i], 
            offset = offset, ...)
        if (circles) {
            xrange <- xlim[2] - xlim[1]
            yrange <- ylim[2] - ylim[1]
            xscale <- max(x$se) * circle.size
            yscale <- max(y$se) * circle.size
            ellipse(xcen, ycen, sqrt(xscale * x$n[i]/Nmax), sqrt(yscale * 
                x$n[i]/Nmax), col = col.arrows[i])
        }
    }
    if (!is.null(group)) 
        return(invisible(data))
}


sim.poly.mat <- function (R, m, n) 
{
    e <- eigen(R)
    v <- pmax(e$values, 0)
    nvar <- ncol(R)
    ncat <- ncol(m)
    X <- matrix(rnorm(nvar * n), n)
    X <- t(e$vectors %*% sqrt(diag(v)) %*% t(X))
    marg <- m
    Y <- matrix(0, ncol = n, nrow = nvar)
    for (i in 1:(ncat)) {
        Y[t(X) > marg[, i]] <- i
    }
    return(t(Y))
}


keysort <- function (keys) 
{
    items <- 1:nrow(keys)
    weights <- items %*% abs(keys)
    ord <- order(weights)
    keys[] <- keys[, ord]
}


describe <- function (x, na.rm = TRUE, interp = FALSE, skew = TRUE, ranges = TRUE, 
    trim = 0.1, type = 3, check = TRUE, fast = NULL, quant = NULL, 
    IQR = FALSE) 
{
    cl <- match.call()
    valid <- function(x) {
        sum(!is.na(x))
    }
    if (!na.rm) 
        x <- na.omit(x)
    if (is.null(fast)) {
        if (prod(dim(x)) > 10^7) {
            fast <- TRUE
        }
        else {
            fast <- FALSE
        }
    }
    if (fast) {
        skew <- FALSE
    }
    numstats <- 10 + length(quant) + IQR
    if (is.null(dim(x)[2])) {
        len <- 1
        nvar <- 1
        stats = matrix(rep(NA, numstats), ncol = numstats)
        stats[1, 1] <- valid(x)
        stats[1, 2] <- mean(x, na.rm = na.rm)
        stats[1, 10] <- sd(x, na.rm = na.rm)
        if (interp) {
            stats[1, 3] <- interp.median(x, na.rm = na.rm)
        }
        else {
            stats[1, 3] <- median(x, na.rm = na.rm)
        }
        stats[1, 9] <- mean(x, na.rm = na.rm, trim = trim)
        stats[1, 4] <- min(x, na.rm = na.rm)
        stats[1, 5] <- max(x, na.rm = na.rm)
        stats[1, 6] <- skew(x, na.rm = na.rm, type = type)
        stats[1, 7] <- mad(x, na.rm = na.rm)
        stats[1, 8] <- kurtosi(x, na.rm = na.rm, type = type)
        vars <- 1
        if (!is.null(quant)) {
            Qnt <- quantile(x, prob = quant, na.rm = TRUE)
            stats[1, (IQR + 11):numstats] <- t(Qnt)
        }
        if (IQR) {
            Quart <- t(quantile(x, prob = c(0.25, 0.75), na.rm = TRUE))
            Iqr <- Quart[, 2] - Quart[, 1]
            stats[1, 11] <- Iqr
        }
        rownames(stats) <- "X1"
    }
    else {
        nvar <- ncol(x)
        stats = matrix(rep(NA, nvar * numstats), ncol = numstats)
        if (is.null(colnames(x))) 
            colnames(x) <- paste0("X", 1:ncol(x))
        rownames(stats) <- colnames(x)
        stats[, 1] <- apply(x, 2, valid)
        vars <- c(1:nvar)
        if (!is.matrix(x) && check) {
            for (i in 1:nvar) {
                if (!is.numeric(x[[i]])) {
                  if (fast) {
                    x[[i]] <- NA
                  }
                  else {
                    if (is.factor(unlist(x[[i]])) | is.character(unlist(x[[i]]))) {
                      x[[i]] <- as.numeric(x[[i]])
                    }
                    else {
                      x[[i]] <- NA
                    }
                  }
                  rownames(stats)[i] <- paste(rownames(stats)[i], 
                    "*", sep = "")
                }
            }
        }
        x <- as.matrix(x)
        if (!is.numeric(x)) {
            message("Converted non-numeric matrix input to numeric.  Are you sure you wanted to do this. Please check your data")
            x <- matrix(as.numeric(x), ncol = nvar)
            rownames(stats) <- paste0(rownames(stats), "*")
        }
        stats[, 2] <- apply(x, 2, mean, na.rm = na.rm)
        stats[, 10] <- apply(x, 2, sd, na.rm = na.rm)
        if (skew) {
            stats[, 6] <- skew(x, na.rm = na.rm, type = type)
            stats[, 8] <- kurtosi(x, na.rm = na.rm, type = type)
        }
        if (ranges) {
            if (fast) {
                stats[, 4] <- apply(x, 2, min, na.rm = na.rm)
                stats[, 5] <- apply(x, 2, max, na.rm = na.rm)
            }
            else {
                stats[, 4] <- apply(x, 2, min, na.rm = na.rm)
                stats[, 5] <- apply(x, 2, max, na.rm = na.rm)
                stats[, 7] <- apply(x, 2, mad, na.rm = na.rm)
                stats[, 9] <- apply(x, 2, mean, na.rm = na.rm, 
                  trim = trim)
                if (interp) {
                  stats[, 3] <- apply(x, 2, interp.median, na.rm = na.rm)
                }
                else {
                  stats[, 3] <- apply(x, 2, median, na.rm = na.rm)
                }
            }
        }
        if (!is.null(quant)) {
            Qnt <- apply(x, 2, quantile, prob = quant, na.rm = TRUE)
            stats[, (IQR + 11):numstats] <- t(Qnt)
        }
        if (IQR) {
            Quart <- t(apply(x, 2, quantile, prob = c(0.25, 0.75), 
                na.rm = TRUE))
            Iqr <- Quart[, 2] - Quart[, 1]
            stats[, 11] <- Iqr
        }
    }
    if (numstats > (10 + IQR)) {
        colnames(stats)[(11 + IQR):numstats] <- paste0("Q", quant[1:length(quant)])
    }
    if (fast) {
        answer <- data.frame(vars = vars, n = stats[, 1], mean = stats[, 
            2], sd = stats[, 10], se = stats[, 10]/sqrt(stats[, 
            1]))
    }
    if (skew) {
        if (ranges) {
            answer <- data.frame(vars = vars, n = stats[, 1], 
                mean = stats[, 2], sd = stats[, 10], median = stats[, 
                  3], trimmed = stats[, 9], mad = stats[, 7], 
                min = stats[, 4], max = stats[, 5], range = stats[, 
                  5] - stats[, 4], skew = stats[, 6], kurtosis = stats[, 
                  8], se = stats[, 10]/sqrt(stats[, 1]))
        }
        else {
            answer <- data.frame(vars = vars, n = stats[, 1], 
                mean = stats[, 2], sd = stats[, 10], skew = stats[, 
                  6], kurtosis = stats[, 8], se = stats[, 10]/sqrt(stats[, 
                  1]))
        }
    }
    else {
        if (ranges) {
            answer <- data.frame(vars = vars, n = stats[, 1], 
                mean = stats[, 2], sd = stats[, 10], min = stats[, 
                  4], max = stats[, 5], range = stats[, 5] - 
                  stats[, 4], se = stats[, 10]/sqrt(stats[, 1]))
        }
        else {
            answer <- data.frame(vars = vars, n = stats[, 1], 
                mean = stats[, 2], sd = stats[, 10], se = stats[, 
                  10]/sqrt(stats[, 1]))
        }
    }
    if (IQR) 
        answer <- data.frame(answer, IQR = stats[, 11])
    if (numstats > (10 + IQR)) {
        if (nvar > 1) {
            answer <- data.frame(answer, stats[, (IQR + 11):numstats])
        }
        else {
            answer <- data.frame(answer, t(stats[, (IQR + 11):numstats]))
        }
    }
    class(answer) <- c("psych", "describe", "data.frame")
    return(answer)
}


iclust.diagram <- function (ic, labels = NULL, short = FALSE, digits = 2, cex = NULL, 
    min.size = NULL, e.size = 1, colors = c("black", "blue"), 
    main = "ICLUST diagram", cluster.names = NULL, marg = c(0.5, 
        0.5, 1.5, 0.5)) 
{
    old.par <- par(mar = marg)
    on.exit(par(old.par))
    clusters <- ic$results
    num <- nrow(clusters)
    num.var <- num + 1
    if (is.null(cex)) 
        cex <- min(16/num.var, 1)
    if (is.null(labels)) {
        var.labels <- rownames(ic$loadings)
    }
    else {
        var.labels = labels
    }
    if (short) {
        var.labels <- paste("V", 1:num.var, sep = "")
    }
    if (is.null(var.labels)) {
        var.labels <- paste("V", 1:num.var, sep = "")
    }
    fixed <- fix.names(ic, var.labels)
    clusters <- fixed$ic$results
    max.len <- max(nchar((var.labels)))
    if (is.null(cluster.names)) 
        cluster.names <- rownames(clusters)
    names(cluster.names) <- rownames(clusters)
    length.labels <- max(max.len * 0.15 * cex, 0.25 * cex)
    nc <- length(ic$size)
    nvar <- sum(ic$size)
    last <- dim(clusters)[1]
    max.size <- max(ic$size)
    limx <- c(-length.labels, nvar + 2)
    limy <- c(0, nvar + 1)
    if (nvar < 12) 
        e.size <- e.size * 0.7
    if (is.null(min.size)) 
        min.size <- 0.1 * nvar
    plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
        axes = FALSE, ylab = "", xlab = "", main = main)
    new.max.len <- max(strwidth(var.labels, units = "user"))
    if (new.max.len > max.len) {
        limx <- c(-new.max.len/2, nvar + 2)
        plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
            axes = FALSE, ylab = "", xlab = "", main = main)
    }
    top <- num.var
    done <- 0
    if (nc == 1) {
        head <- num
        size <- num.var
        y.loc <- clusters[head, "size2"]
        down(clusters, head, size, y.loc, old.head = NULL, old.loc = NULL, 
            min.size = min.size, e.size = e.size, digits = digits, 
            cex = cex, limx = limx, limy = limy, colors = colors, 
            cluster.names = cluster.names)
    }
    else {
        for (clust in 1:nc) {
            size <- sum(abs(ic$clusters[, clust]))
            if (substr(colnames(ic$clusters)[clust], 1, 1) == 
                "C") {
                head <- which(rownames(clusters) == colnames(ic$clusters)[clust])
                cluster <- clusters[head, ]
                y.loc <- clusters[head, "size2"] + done
                down(clusters, head, size, y.loc, old.head = NULL, 
                  old.loc = NULL, min.size = min.size, e.size = e.size, 
                  digits = digits, cex = cex, limx = limx, limy = limy, 
                  colors = colors, cluster.names = cluster.names)
            }
            else {
                v.name <- names(which(ic$clusters[, clust] == 
                  1))
                dia.rect(0, done + 0.5, v.name, xlim = limx, 
                  ylim = limy, cex = cex)
            }
            done <- done + size
        }
    }
}


cluster.cor <- function (keys, r.mat, correct = TRUE, SMC = TRUE, item.smc = NULL, 
    impute = TRUE) 
{
    tol = sqrt(.Machine$double.eps)
    cl <- match.call()
    if (!is.matrix(keys)) 
        keys <- as.matrix(keys)
    if (any(is.na(r.mat))) {
        SMC = FALSE
        warning("Missing values in the correlation matrix do not allow for SMC's to be found")
    }
    r.mat[is.na(r.mat)] <- -9999999
    if (SMC && is.null(item.smc)) {
        item.smc <- smc(r.mat)
    }
    else {
        item.smc <- rep(1, dim(r.mat)[1])
    }
    covar <- t(keys) %*% r.mat %*% keys
    var <- diag(covar)
    sd.inv <- 1/sqrt(var)
    ident.sd <- diag(sd.inv, ncol = length(sd.inv))
    cluster.correl <- ident.sd %*% covar %*% ident.sd
    cluster.correl[abs(cluster.correl) > (1 + tol)] <- NA
    key.var <- diag(t(keys) %*% keys)
    key.smc <- t(keys) %*% item.smc
    key.alpha <- ((var - key.var)/var) * (key.var/(key.var - 
        1))
    key.lambda6 <- (var - key.var + key.smc)/var
    key.alpha[is.nan(key.alpha)] <- 1
    key.alpha[!is.finite(key.alpha)] <- 1
    key.av.r <- key.alpha/(key.var - key.alpha * (key.var - 1))
    colnames(cluster.correl) <- colnames(keys)
    rownames(cluster.correl) <- colnames(keys)
    names(key.lambda6) <- colnames(keys)
    key.lambda6 <- drop(key.lambda6)
    if (any(is.na(cluster.correl)) && impute) {
        warning("Some of the correlations were NA and were imputed")
        r.mat[r.mat < -1] <- NA
        n.keys <- ncol(keys)
        keys[keys == 0] <- NA
        for (i in 1:n.keys) {
            if (any(is.na(cluster.correl[i, ]))) {
                for (j in 1:n.keys) {
                  if (is.na(cluster.correl[i, j])) {
                    temp <- mean(colMeans((keys[, i] * r.mat), 
                      na.rm = TRUE) * keys[, j], na.rm = TRUE) * 
                      key.var[i] * key.var[j]
                    adjusted.r <- temp * ident.sd[i, i] * ident.sd[j, 
                      j]
                    cluster.correl[i, j] <- adjusted.r
                  }
                }
            }
        }
    }
    sn <- key.av.r * key.var/(1 - key.av.r)
    if (correct) {
        cluster.corrected <- correct.cor(cluster.correl, t(key.alpha))
        result <- list(cor = cluster.correl, sd = sqrt(var), 
            corrected = cluster.corrected, alpha = key.alpha, 
            av.r = key.av.r, size = key.var, sn = sn, G6 = key.lambda6, 
            Call = cl)
    }
    else {
        result <- list(cor = cluster.correl, sd = sqrt(var), 
            alpha = key.alpha, av.r = key.av.r, size = key.var, 
            sn = sn, G6 = key.lambda6, Call = cl)
    }
    class(result) <- c("psych", "cluster.cor")
    return(result)
}


factor.rotate <- function (f, angle, col1 = 1, col2 = 2, plot = FALSE, ...) 
{
    if (!is.matrix(f)) {
        f <- f$loadings
    }
    nvar <- dim(f)[2]
    if (!is.matrix(f)) {
        if (!is.data.frame(f)) {
            stop("f must be either a data frame or a matrix")
        }
        else {
            f <- as.matrix(f)
        }
    }
    rot <- diag(1, nvar, nvar)
    theta <- pi * angle/180
    rot[col1, col1] <- cos(theta)
    rot[col2, col2] <- cos(theta)
    rot[col1, col2] <- -sin(theta)
    rot[col2, col1] <- sin(theta)
    result <- f %*% rot
    if (plot) {
        fa.plot(result, ...)
        abline(a = 0, b = tan(-theta), lty = "dashed")
        abline(a = 0, b = tan(-theta + pi/2), lty = "dashed")
    }
    return(result)
}


read.clipboard.fwf <- function (header = FALSE, widths = rep(1, 10), ...) 
{
    MAC <- Sys.info()[1] == "Darwin"
    if (!MAC) {
        if (header) 
            read.clipboard <- read.fwf(file("clipboard"), header = TRUE, 
                widths = widths, ...)
        else read.clipboard <- read.fwf(file("clipboard"), widths = widths, 
            ...)
    }
    else {
        if (header) 
            read.clipboard <- read.fwf(pipe("pbpaste"), header = TRUE, 
                widths = widths, ...)
        else read.clipboard <- read.fwf(pipe("pbpaste"), widths = widths, 
            ...)
    }
}


plot.residuals <- function (x, main, type = c("qq", "chi", "hist", "cor"), std, 
    bad = 4, numbers = TRUE, upper = FALSE, diag = FALSE, ...) 
{
    if (missing(type)) 
        type <- "qq"
    nr <- nrow(x)
    nc <- ncol(x)
    if (!is.null(rownames(x))) {
        rname <- rownames(x)
    }
    else {
        rname <- paste0("V", 1:nr)
    }
    diag(x) <- NA
    switch(type, hist = {
        if (missing(std)) std <- FALSE
        x <- x[lower.tri(x, diag = TRUE)]
        std.x <- x/sd(x, na.rm = TRUE)
        if (std) {
            if (missing(main)) main <- "Histogram of standardized residuals"
            hist(std.x, main = main, ...)
        } else {
            if (missing(main)) main <- "Histogram of residuals"
            hist(x, main = main, ...)
        }
    }, qq = {
        if (missing(std)) std <- TRUE
        x <- x[lower.tri(x, diag = TRUE)]
        if (std) {
            if (missing(main)) main <- "Plot of standardized residuals"
            std.x <- x/sd(x, na.rm = TRUE)
            xy <- qqnorm(std.x, main = main)
            qqline(std.x)
            worst <- order(abs(std.x), decreasing = TRUE)
        } else {
            if (missing(main)) main <- "Plot of raw residuals"
            xy <- qqnorm(x, main = main, ...)
            qqline(x)
            worst <- order(abs(x), decreasing = TRUE)
        }
        worstItems <- arrayInd(worst[1:bad], c(nr, nc))
        pos <- rep(4, bad)
        pos[x[worst[1:bad]] > 0] <- 2
        text(xy$x[worst[1:bad]], xy$y[worst[1:bad]], paste(rname[worstItems[, 
            2]], rname[worstItems[, 1]]), pos = pos, ...)
    }, chi = {
        if (missing(std)) std <- TRUE
        x <- x[lower.tri(x, diag = TRUE)]
        if (std) {
            x <- x/sd(x, na.rm = TRUE)
            if (missing(main)) main <- "Plot of squared standardized residuals"
        } else {
            if (missing(main)) main <- "Plot of squared residuals"
        }
        nx <- length(x) - nr
        xy <- qqplot(qchisq(ppoints(nx), df = 1), y = x^2, main = main, 
            ylab = "Quantiles of Squared residuals", xlab = "Expected value for quantile")
        qqline(x^2, distribution = function(p) qchisq(p, df = 1))
        worst <- order(abs(x^2), decreasing = TRUE)
        worstItems <- arrayInd(worst[1:5], c(nr, nc))
        text(xy$x[nx:(nx - 4)], xy$y[nx:(nx - 4)], paste(rname[worstItems[, 
            2]], rname[worstItems[, 1]]), pos = 2, ...)
    }, cor = {
        if (missing(main)) main <- "Plot of residual correlations"
        cor.plot(x, main = main, numbers = numbers, upper = upper, 
            diag = diag)
    })
}


dia.ellipse1 <- function (x, y, e.size = 0.05, xlim = c(0, 1), ylim = c(0, 1), 
    ...) 
{
    segments = 51
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    xrange = (xlim[2] - xlim[1])
    yrange = (ylim[2] - ylim[1])
    xs <- e.size * xrange
    ellipse <- unit.circle
    ellipse[, 1] <- ellipse[, 1] * xs + x
    ellipse[, 2] <- ellipse[, 2] * xs + y
    lines(ellipse, ...)
    return(xs)
}


score.irt.2 <- function (stats, items, keys = NULL, cut = 0.3, bounds = c(-4, 
    4), mod = "logistic") 
{
    bySubject <- function(i, count.f, total.f, items.f, discrim.f, 
        diffi.f) {
        if (count.f[i] > 0) {
            beta = discrim.f[!is.na(items.f[i, ])]
            delta = diffi.f[!is.na(items.f[i, ])]
            if ((sum(items.f[i, ], na.rm = TRUE) == 0) || (prod(items.f[i, 
                ], na.rm = TRUE) == 1)) {
                if (sum(items.f[i, ], na.rm = TRUE) == 0) {
                  if (mod == "logistic") {
                    myfit <- optimize(irtLimit.2par, bounds, 
                      beta = beta, delta = delta, scores = rep(0, 
                        sum(!is.na(items.f[i, ]))))
                  }
                  else {
                    myfit <- optimize(irtLimit.2par.norm, bounds, 
                      beta = beta, delta = delta, scores = rep(0, 
                        sum(!is.na(items.f[i, ]))))
                  }
                  theta <- myfit$minimum
                  fit <- myfit$objective
                }
                else {
                  if (prod(items.f[i, ], na.rm = TRUE) == 1) {
                    if (mod == "logistic") {
                      myfit <- optimize(irtLimit.2par, bounds, 
                        beta = beta, delta = delta, scores = rep(1, 
                          sum(!is.na(items.f[i, ]))))
                    }
                    else {
                      myfit <- optimize(irtLimit.2par.norm, bounds, 
                        beta = beta, delta = delta, scores = rep(1, 
                          sum(!is.na(items.f[i, ]))))
                    }
                    theta <- myfit$minimum
                    fit <- myfit$objective
                  }
                }
            }
            else {
                scores = t(items.f[i, !is.na(items.f[i, ])])
                if (mod == "logistic") {
                  myfit <- optimize(irtLimit.2par, bounds, beta = beta, 
                    delta = delta, scores = scores)
                }
                else {
                  myfit <- optimize(irtLimit.2par.norm, bounds, 
                    beta = beta, delta = delta, scores = scores)
                }
                theta <- myfit$minimum
                fit <- myfit$objective
            }
        }
        else {
            total.f[i] <- NA
            theta <- NA
            fit <- NA
        }
        return(list(theta, total.f[i], fit))
    }
    bigFunction <- function(f, n.obs, stats, items, keys = NULL, 
        cut = 0.3, bounds = c(-5, 5), mod = "logistic") {
        nf <- length(stats$difficulty)
        diff <- stats$difficulty[[f]]
        cat <- dim(diff)[2]
        if (nf < 2) {
            discrim <- stats$discrimination
            if (!is.null(keys)) {
                discrim <- discrim * abs(keys)
            }
        }
        else {
            discrim <- stats$discrimination[, f]
            if (!is.null(keys)) {
                discrim <- discrim * abs(keys[, f])
            }
        }
        fit <- rep(NA, n.obs)
        theta <- rep(NA, n.obs)
        if (is.null(keys)) {
            items.f <- items[, (abs(discrim[, f]) > cut), drop = FALSE]
            diffi.f <- diff[(abs(discrim[, f]) > cut)]
            discrim.f <- discrim[(abs(discrim[, f]) > cut), drop = FALSE]
        }
        else {
            items.f <- items[, (abs(keys[, f]) > 0), drop = FALSE]
            discrim.f <- discrim[(abs(keys[, f]) > 0), drop = FALSE]
            diffi.f <- diff[(abs(keys[, f]) > 0)]
        }
        diffi.vect <- as.vector(t(diffi.f))
        discrim.F.vect <- as.vector(t(discrim.f))
        if (is.matrix(discrim)) 
            discrim.F.vect <- drop(discrim.F.vect)
        total <- rowMeans(t(t(items.f) * sign(discrim.F.vect)), 
            na.rm = TRUE)
        count <- rowSums(!is.na(items.f))
        subjecttheta <- mapply(bySubject, c(1:n.obs), MoreArgs = list(count, 
            total, items.f, discrim.f, diffi.f))
        subjecttheta <- matrix(unlist(subjecttheta), ncol = 3, 
            byrow = TRUE)
        theta <- subjecttheta[, 1]
        total <- subjecttheta[, 2]
        fit <- subjecttheta[, 3]
        theta[theta < bounds[1]] <- bounds[1]
        theta[theta > bounds[2]] <- bounds[2]
        nf <- length(stats$difficulty)
        n.obs <- dim(items)[1]
        nvar <- dim(items)[2]
        scores <- list(theta, total, fit)
        return(scores)
    }
    nf <- length(stats$difficulty)
    n.obs <- dim(items)[1]
    min.item <- min(items, na.rm = TRUE)
    items <- items - min.item
    scores <- mcmapply(bigFunction, c(1:nf), MoreArgs = list(n.obs = n.obs, 
        items = items, stats = stats, keys = keys, cut = cut, 
        bounds = bounds, mod = mod))
    nf <- length(stats$difficulty)
    scores <- matrix(unlist(scores), ncol = nf * 3)
    scores <- scores[, c(seq(1, nf * 3, 3), seq(2, nf * 3 + 1, 
        3), seq(3, nf * 3 + 2, 3))]
    colnames(scores) <- paste(rep(c("theta", "total", "fit"), 
        each = nf), 1:nf, sep = "")
    return(scores)
}


factor.fit <- function (r, f) 
{
    r2 <- sum(r * r)
    rstar <- factor.residuals(r, f)
    rstar2 <- sum(rstar * rstar)
    fit <- 1 - rstar2/r2
    return(fit)
}


cor.wt <- function (data, vars = NULL, w = NULL, sds = NULL, cor = TRUE) 
{
    cl <- match.call()
    if (is.list(data) && !is.data.frame(data)) {
        w <- data$n
        sds <- data$sd
        x <- data$mean
    }
    else {
        x <- data
    }
    if (!is.null(vars)) {
        x <- x[, vars]
        w <- w[, vars]
        sds <- sds[, vars]
    }
    if (is.null(w)) 
        w <- matrix(rep(rep(1/nrow(x), nrow(x)), ncol(x)), nrow = nrow(x), 
            ncol = ncol(x))
    if (is.null(ncol(w))) {
        wt <- w/sum(w)
    }
    else {
        wt <- t(t(w)/colSums(w))
    }
    cnames <- colnames(x)
    for (i in 1:ncol(x)) {
        if (is.factor(x[, i]) || is.logical(x[, i])) {
            x[, i] <- as.numeric(x[, i])
            colnames(x)[i] <- paste(cnames[i], "*", sep = "")
        }
    }
    means <- colSums(x * wt, na.rm = TRUE)
    xc <- scale(x, center = means, scale = FALSE)
    if (is.null(sds)) {
        xs <- xc/sqrt(w)
    }
    else {
        xs <- xc * sds/sqrt(w)
    }
    xwt <- sqrt(wt) * xc
    if (any(is.na(xwt))) {
        cov <- apply(xwt, 2, function(x) colSums(xwt * x, na.rm = TRUE))
    }
    else {
        cov <- crossprod(xwt)
    }
    if (cor) {
        r <- cov2cor(cov)
    }
    else {
        r <- cov
    }
    xw <- wt * xc
    result <- list(r = r, xwt = xwt, wt = wt, mean = means, xc = xc, 
        xs = xs)
    result$Call <- cl
    class(result) <- c("psych", "cor.wt")
    return(result)
}


sim.poly.ideal.npl <- function (nvar = 5, n = 500, low = -2, high = 2, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, cat = 5, theta = NULL) 
{
    cat <- cat - 1
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    if (is.null(a)) {
        a <- rep(1, nvar)
    }
    if (is.null(theta)) {
        theta <- rnorm(n, mu, sd)
    }
    item <- 2 * matrix((c + (z - c) * exp(a * t(-theta %+% t(d)))/(1 + 
        2 * exp(a * t(-theta %+% t(d))) + exp(a * t(2 * (-theta %+% 
        t(d)))))), n, nvar, byrow = TRUE)
    p <- item
    item[] <- rbinom(n * nvar, cat, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(p = p, items = item, discrimination = a, difficulty = d, 
        gamma = c, zeta = z, theta = theta)
    return(result)
}


Yule2poly <- function (Q, m, n = NULL, correct = TRUE) 
{
    .Deprecated("Yule2tet", msg = "Yule2poly has been replaced by Yule2tet, please try again")
    t <- Yule.inv(Q, m, n = n)
    r <- tetrachoric(t, correct = correct)$rho
    return(r)
}


rescale <- function (x, mean = 100, sd = 15, df = TRUE) 
{
    if (df) {
        x <- data.frame(t(t(scale(x)) * sd + mean))
    }
    else {
        x <- t(t(scale(x)) * sd + mean)
    }
    return(x)
}


interp.quart <- function (x, w = 1, na.rm = TRUE) 
{
    q <- c(0.25, 0.5, 0.75)
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    n <- length(x)
    n2 <- (n + 1) * q
    N <- n * q
    o <- order(x)
    x <- x[o]
    ml <- x[floor(n2)]
    mh <- x[ceiling(n2)]
    m <- (mh + ml)/2
    im <- xa <- xb <- rep(NA, 3)
    for (i in 1:3) {
        xb[i] <- sum(x < m[i])
        xa[i] <- sum(x > m[i])
    }
    am <- n - xa - xb
    for (i in 1:3) {
        if (am[i] > 1) {
            im[i] <- m[i] - 0.5 * w + w * (N[i] - xb[i])/am[i]
        }
        else im[i] <- m[i]
    }
    return(im)
}


sim.poly.ideal.npn <- function (nvar = 5, n = 500, low = -2, high = 2, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, cat = 5) 
{
    warning("Not ready for prime time")
    cat <- cat - 1
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    if (is.null(a)) {
        a <- rep(1, nvar)
    }
    theta <- rnorm(n, mu, sd)
    item <- matrix(t(c + (z - c) * pnorm(abs(a * t(theta %+% 
        t(-d))))), n, nvar)
    item[] <- rbinom(n * nvar, cat, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(items = item, discrimination = a, difficulty = d, 
        gamma = c, zeta = z, theta = theta)
    return(result)
}


read.https <- function (filename, header = TRUE) 
{
    temp <- tempfile()
    download.file(filename, destfile = temp, method = "curl")
    result <- read.table(temp, header = header)
    unlink(temp)
    return(result)
}


sim.structure <- function (fx = NULL, Phi = NULL, fy = NULL, f = NULL, n = 0, 
    uniq = NULL, raw = TRUE, items = FALSE, low = -2, high = 2, 
    d = NULL, cat = 5, mu = 0) 
{
    cl <- match.call()
    if (is.null(f)) {
        if (is.null(fy)) {
            f <- fx
        }
        else {
            f <- superMatrix(fx, fy)
        }
    }
    f <- as.matrix(f)
    if (!is.null(Phi)) {
        if (length(Phi) == 1) 
            Phi <- matrix(c(1, Phi, Phi, 1), 2, 2)
    }
    nf <- ncol(f)
    nvar <- nrow(f)
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar/nf - 1))
        d <- rep(d, nf)
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    a <- rep(1, nvar)
    if (is.vector(f)) {
        f <- as.matrix(f)
        Phi <- 1
    }
    if (!is.null(Phi)) {
        model <- f %*% Phi %*% t(f)
    }
    else {
        model <- f %*% t(f)
    }
    if (is.null(uniq)) {
        diag(model) <- 1
    }
    else {
        diag(model) <- uniq + diag(model)
    }
    nvar <- dim(f)[1]
    if (is.null(rownames(model))) {
        colnames(model) <- rownames(model) <- paste("V", 1:nvar, 
            sep = "")
    }
    if (n > 0) {
        mu <- rep(mu, nvar)
        eX <- eigen(model)
        observed <- matrix(rnorm(nvar * n), n)
        observed <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
            0)), nvar) %*% t(observed) + mu)
        if (items) {
            observedp <- matrix(t(pnorm(a * t(observed) - d)), 
                n, nvar)
            observed[] <- rbinom(n * nvar, cat, observedp)
        }
        colnames(observed) <- colnames(model)
        r <- cor(observed)
    }
    reliability <- diag(f %*% t(f))
    if (n < 1) {
        results <- list(model = model, reliability = reliability)
    }
    else {
        if (!raw) {
            results <- list(model = model, reliability = reliability, 
                r = r, N = n)
        }
        else {
            results <- list(model = model, reliability = reliability, 
                r = r, observed = observed, N = n)
        }
    }
    results$Call <- cl
    class(results) <- c("psych", "sim")
    return(results)
}


sim <- function (fx = NULL, Phi = NULL, fy = NULL, alpha = 0.8, lambda = 0, 
    n = 0, mu = NULL, raw = TRUE) 
{
    cl <- match.call()
    if (is.null(fx)) {
        fx <- matrix(c(rep(c(0.8, 0.7, 0.6, rep(0, 12)), 3), 
            0.8, 0.7, 0.6), ncol = 4)
        if (is.null(Phi)) {
            Phi <- diag(1, 4, 4)
            Phi <- alpha^abs(row(Phi) - col(Phi)) + lambda^2
            diag(Phi) <- max((alpha + lambda), 1)
            Phi <- cov2cor(Phi)
        }
        if (is.null(mu)) {
            mu <- c(0, 0.5, 1, 2)
        }
    }
    if (is.null(fy)) {
        f <- fx
    }
    else {
        f <- superMatrix(fx, fy)
    }
    if (is.null(mu)) {
        mu <- rep(0, ncol(fx))
    }
    means <- fx %*% mu
    if (is.vector(f)) {
        f <- as.matrix(f)
        Phi <- 1
    }
    if (!is.null(Phi)) {
        model <- f %*% Phi %*% t(f)
    }
    else {
        model <- f %*% t(f)
    }
    diag(model) <- 1
    nvar <- dim(f)[1]
    colnames(model) <- rownames(model) <- paste("V", 1:nvar, 
        sep = "")
    if (n > 0) {
        eX <- eigen(model)
        observed <- matrix(rnorm(nvar * n), n)
        observed <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
            0)), nvar) %*% t(observed) + rep(means, n))
        r <- cor(observed)
    }
    reliability <- diag(f %*% t(f))
    if (n < 1) {
        results <- list(model = model, reliability = reliability)
    }
    else {
        if (!raw) {
            results <- list(model = model, reliability = reliability, 
                r = r, N = n)
        }
        else {
            results <- list(model = model, reliability = reliability, 
                r = r, observed = observed, N = n)
        }
    }
    results$Call <- cl
    class(results) <- c("psych", "sim")
    return(results)
}


omega.graph <- function (om.results, out.file = NULL, sl = TRUE, labels = NULL, 
    size = c(8, 6), node.font = c("Helvetica", 14), edge.font = c("Helvetica", 
        10), rank.direction = c("RL", "TB", "LR", "BT"), digits = 1, 
    title = "Omega", ...) 
{
    if (!requireNamespace("Rgraphviz")) {
        stop("I am sorry, you need to have the  Rgraphviz package installed")
        nodes <- function() {
        }
        addEdge <- function() {
        }
        subGraph <- function() {
        }
    }
    if (sl) {
        factors <- as.matrix(om.results$schmid$sl)
    }
    else {
        factors <- as.matrix(om.results$schmid$oblique)
    }
    rank.direction <- match.arg(rank.direction)
    num.var <- dim(factors)[1]
    if (sl) {
        num.factors <- dim(factors)[2] - 4
    }
    else {
        num.factors <- dim(factors)[2]
    }
    gloading <- om.results$schmid$gloading
    vars <- paste("V", 1:num.var, sep = "")
    if (!is.null(labels)) {
        vars <- paste(labels)
    }
    else {
        vars <- rownames(factors)
    }
    if (sl) {
        fact <- c("g", paste("F", 1:num.factors, "*", sep = ""))
    }
    else {
        fact <- c("g", paste("F", 1:num.factors, sep = ""))
    }
    clust.graph <- new("graphNEL", nodes = c(vars, fact), edgemode = "directed")
    graph.shape <- c(rep("box", num.var), rep("ellipse", num.factors + 
        1))
    graph.rank <- c("sink", rep("same", num.var), rep("source", 
        num.factors))
    names(graph.shape) <- nodes(clust.graph)
    names(graph.rank) <- nodes(clust.graph)
    if (sl) {
        edge.label <- rep("", num.var * 2)
        edge.dir <- rep("forward", num.var * 2)
        edge.arrows <- rep("open", num.var * 2)
        edge.name <- rep("", num.var * 2)
        names(edge.label) <- seq(1:num.var * 2)
        names(edge.dir) <- rep("", num.var * 2)
        names(edge.arrows) <- rep("", num.var * 2)
        sem <- matrix(rep(NA, 6 * (2 * num.var + num.factors)), 
            ncol = 3)
    }
    else {
        edge.label <- rep("", num.var + num.factors)
        edge.name <- rep("", num.var + num.factors)
        edge.arrows <- rep("open", num.var + num.factors)
        edge.dir <- rep("forward", num.var * 2)
        names(edge.label) <- seq(1:num.var + num.factors)
        names(edge.dir) <- seq(1:num.var + num.factors)
        names(edge.arrows) <- seq(1:num.var + num.factors)
        sem <- matrix(rep(NA, 6 * (num.var + num.factors) + 3), 
            ncol = 3)
    }
    if (sl) {
        l <- matrix(factors[, 2:(num.factors + 1)], ncol = num.factors)
    }
    else {
        l <- factors
    }
    m1 <- matrix(apply(t(apply(l, 1, abs)), 1, which.max), ncol = 1)
    if (sl) {
        k <- num.var
        for (i in 1:num.var) {
            clust.graph <- addEdge(vars[i], fact[1], clust.graph, 
                1)
            edge.label[i] <- round(factors[i, 1], digits)
            edge.name[i] <- paste(vars[i], "~", fact[1], sep = "")
            edge.arrows[i] <- paste("open")
            edge.dir[i] <- paste("back")
            sem[i, 1] <- paste(fact[1], "->", vars[i], sep = "")
            sem[i, 2] <- vars[i]
        }
    }
    else {
        k <- num.factors
        for (j in 1:num.factors) {
            clust.graph <- addEdge(fact[1], fact[j + 1], clust.graph, 
                1)
            edge.label[j] <- round(gloading[j], digits)
            edge.name[j] <- paste(fact[1], "~", fact[j + 1], 
                sep = "")
            sem[j, 1] <- paste(fact[1], "->", fact[1 + j], sep = "")
            sem[j, 2] <- paste("g", fact[1 + j], sep = "")
        }
    }
    for (i in 1:num.var) {
        clust.graph <- addEdge(fact[1 + m1[i]], vars[i], clust.graph, 
            1)
        edge.label[i + k] <- round(l[i, m1[i]], digits)
        edge.name[i + k] <- paste(fact[1 + m1[i]], "~", vars[i], 
            sep = "")
        edge.arrows[i + k] <- paste("open")
        sem[i + k, 1] <- paste(fact[1 + m1[i]], "->", vars[i], 
            sep = "")
        sem[i + k, 2] <- paste(fact[1 + m1[i]], vars[i], sep = "")
    }
    if (sl) {
        k <- num.var * 2
        for (i in 1:num.var) {
            sem[i + k, 1] <- paste(vars[i], "<->", vars[i], sep = "")
            sem[i + k, 2] <- paste("e", i, sep = "")
        }
        k <- k + num.var
        for (f in 1:num.factors) {
            sem[f + k, 1] <- paste(fact[1 + f], "<->", fact[1 + 
                f], sep = "")
            sem[f + k, 3] <- "1"
        }
        k <- k + num.factors
        sem[k + 1, 1] <- paste("g <->g")
        sem[k + 1, 3] <- "1"
        k <- k + 1
    }
    else {
        k <- num.var + num.factors
        for (i in 1:num.var) {
            sem[i + k, 1] <- paste(vars[i], "<->", vars[i], sep = "")
            sem[i + k, 2] <- paste("e", i, sep = "")
        }
        k <- 2 * num.var + num.factors
        for (f in 1:num.factors) {
            sem[f + k, 1] <- paste(fact[f + 1], "<->", fact[f + 
                1], sep = "")
            sem[f + k, 3] <- "1"
        }
        k <- 2 * num.var + 2 * num.factors
        sem[k + 1, 1] <- paste("g<->g")
        sem[k + 1, 3] <- "1"
        k <- k + 1
    }
    nAttrs <- list()
    eAttrs <- list()
    if (FALSE) {
        if (!is.null(labels)) {
            var.labels <- c(labels, fact)
            names(var.labels) <- nodes(clust.graph)
            nAttrs$label <- var.labels
            names(edge.label) <- edge.name
        }
    }
    names(edge.label) <- edge.name
    names(edge.dir) <- edge.name
    names(edge.arrows) <- edge.name
    nAttrs$shape <- graph.shape
    nAttrs$rank <- graph.rank
    eAttrs$label <- edge.label
    if (sl) {
        eAttrs$dir <- edge.dir
        eAttrs$arrowhead <- edge.arrows
        eAttrs$arrowtail <- edge.arrows
    }
    attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE), 
        graph = list(rankdir = rank.direction, fontsize = 10, 
            bgcolor = "white"))
    plot(clust.graph, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
        attrs = attrs, main = title)
    if (!is.null(out.file)) {
        toDotty(clust.graph, out.file, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
            attrs = attrs)
    }
    colnames(sem) <- c("Path", "Parameter", "Initial Value")
    return(sem = sem[1:k, ])
}


describeBy <- function (x, group = NULL, mat = FALSE, type = 3, digits = 15, 
    ...) 
{
    if (is.null(group)) {
        answer <- describe(x, type = type)
        warning("no grouping variable requested")
    }
    else {
        if (!is.data.frame(group) && !is.list(group) && (length(group) < 
            NROW(x))) 
            group <- x[, group]
        answer <- by(x, group, describe, type = type, ...)
        class(answer) <- c("psych", "describeBy")
    }
    if (mat) {
        ncol <- length(answer[[1]])
        n.var <- nrow(answer[[1]])
        n.col <- ncol(answer[[1]])
        n.grouping <- length(dim(answer))
        n.groups <- prod(dim(answer))
        names <- names(answer[[1]])
        row.names <- attr(answer[[1]], "row.names")
        dim.names <- attr(answer, "dimnames")
        mat.ans <- matrix(NaN, ncol = ncol, nrow = n.var * n.groups)
        labels.ans <- matrix(NaN, ncol = n.grouping + 1, nrow = n.var * 
            n.groups)
        colnames(labels.ans) <- c("item", paste("group", 1:n.grouping, 
            sep = ""))
        colnames(mat.ans) <- colnames(answer[[1]])
        rn <- 1:(n.var * n.groups)
        k <- 1
        labels.ans[, 1] <- seq(1, (n.var * n.groups))
        group.scale <- cumprod(c(1, dim(answer)))
        for (var in 1:(n.var * n.groups)) {
            for (group in 1:n.grouping) {
                groupi <- ((trunc((var - 1)/group.scale[group]))%%dim(answer)[group]) + 
                  1
                labels.ans[var, group + 1] <- dim.names[[group]][[groupi]]
            }
        }
        k <- 1
        for (var in 1:n.var) {
            for (group in 1:n.groups) {
                rn[k] <- paste(row.names[var], group, sep = "")
                for (stat in 1:n.col) {
                  if (!is.null(answer[[group]][[stat]][var])) {
                    mat.ans[k, stat] <- round(answer[[group]][[stat]][var], 
                      digits)
                  }
                  else {
                    mat.ans[k, stat] <- NA
                  }
                }
                k <- k + 1
            }
        }
        answer <- data.frame(labels.ans, mat.ans)
        rownames(answer) <- rn
    }
    return(answer)
}


cor.ci <- function (x, keys = NULL, n.iter = 100, p = 0.05, overlap = FALSE, 
    poly = FALSE, method = "pearson", plot = TRUE, ...) 
{
    corCi(x = x, keys = keys, n.iter = n.iter, p = p, overlap = overlap, 
        poly = poly, method = method, plot = plot, ...)
}


sim.dichot <- function (nvar = 72, nsub = 500, circum = FALSE, xloading = 0.6, 
    yloading = 0.6, gloading = 0, xbias = 0, ybias = 0, low = 0, 
    high = 0) 
{
    avloading <- (xloading + yloading)/2
    errorweight <- sqrt(1 - (avloading^2 + gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) * xloading + xbias
    truey <- rnorm(nsub) * yloading + ybias
    if (circum) {
        radia <- seq(0, 2 * pi, len = nvar + 1)
        rad <- radia[which(radia < 2 * pi)]
    }
    else rad <- c(rep(0, nvar/4), rep(pi/2, nvar/4), rep(pi, 
        nvar/4), rep(3 * pi/2, nvar/4))
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    trueitem <- outer(truex, cos(rad)) + outer(truey, sin(rad))
    item <- gloading * g + trueitem + errorweight * error
    nvar2 <- nvar/2
    iteml <- (item[, (1:nvar2) * 2 - 1] >= low)
    itemh <- (item[, (1:nvar2) * 2] >= high)
    item <- cbind(iteml, itemh) + 0
    return(item)
}


sim.simplex <- function (nvar = 12, alpha = 0.8, lambda = 0, beta = 1, mu = NULL, 
    n = 0) 
{
    cl <- match.call()
    R <- matrix(0, nvar, nvar)
    R[] <- alpha^abs(col(R) - row(R)) * beta + lambda^2
    diag(R) <- max((alpha * beta) + lambda, 1)
    R <- cov2cor(R)
    colnames(R) <- rownames(R) <- paste("V", 1:nvar, sep = "")
    if (is.null(mu)) {
        mu <- rep(0, nvar)
    }
    if (n > 0) {
        observed <- matrix(rnorm(nvar * n), n)
        eX <- eigen(R)
        observed.scores <- matrix(rnorm(nvar * n), n)
        observed.scores <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
            0)), nvar) %*% t(observed) + mu)
        observed <- cor(observed.scores)
        results <- list(model = R, r = observed, observed = observed.scores)
        results$Call <- cl
        class(results) <- c("psych", "sim")
    }
    else {
        results <- R
    }
    results
}


logit <- function (p) 
{
    log(p/(1 - p))
}


irt.fa <- function (x, nfactors = 1, correct = TRUE, plot = TRUE, n.obs = NULL, 
    rotate = "oblimin", fm = "minres", sort = FALSE, ...) 
{
    cl <- match.call()
    if (is.matrix(x) | is.data.frame(x)) {
        if (is.null(n.obs)) 
            n.obs <- dim(x)[1]
        nvar <- ncol(x)
        vname <- colnames(x)
        x <- as.matrix(x)
        if (!is.numeric(x)) {
            message("Converted non-numeric matrix input to numeric. \n Are you sure you wanted to do this?\n Please check your data")
            x <- matrix(as.numeric(x), ncol = nvar)
        }
        colnames(x) <- vname
        tx <- table(as.matrix(x))
        if (dim(tx)[1] == 2) {
            tet <- tetrachoric(x, correct = correct)
            typ = "tet"
        }
        else {
            tet <- polychoric(x)
            typ = "poly"
        }
        r <- tet$rho
        tau <- tet$tau
    }
    else {
        if (!is.null(x$rho)) {
            r <- x$rho
            tau <- x$tau
            if (is.null(n.obs)) {
                n.obs <- x$n.obs
            }
            typ <- class(x)[2]
            if (typ == "irt.fa") 
                typ <- "tet"
        }
        else {
            stop("x must  be a data.frame or matrix or the result from tetra or polychoric")
        }
    }
    t <- fa(r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, 
        fm = fm, ...)
    if (sort) {
        t <- fa.sort(t)
        if (typ != "tet") {
            tau <- tau[t$order, ]
        }
        else {
            tau <- tau[t$order]
        }
    }
    nf <- dim(t$loadings)[2]
    diffi <- list()
    for (i in 1:nf) {
        diffi[[i]] <- tau/sqrt(1 - t$loadings[, i]^2)
    }
    discrim <- t$loadings/sqrt(1 - t$loadings^2)
    if (any(is.nan(discrim))) {
        for (i in 1:nf) {
            bad <- which(is.nan(discrim[, i]))
            if (length(bad) > 0) {
                warning("A discrimination  with a NaN value was replaced with the maximum discrimination for factor ", 
                  i, " and  item(s)  ", bad, "\nexamine the factor analysis object (fa)  to identify the Heywood case. \nThe item informations are probably suspect as well for this factor.  \nYou might try a different factor extraction technique. ")
                discrim[is.nan(discrim[, i]), i] <- max(discrim[, 
                  i], na.rm = TRUE)
                diffi[[i]][bad, ] <- tau[bad, ]
            }
        }
    }
    class(diffi) <- NULL
    class(discrim) <- NULL
    tl <- t$loadings
    class(tl) <- NULL
    irt <- list(difficulty = diffi, discrimination = discrim)
    nlevels <- dim(diffi[[1]])[2]
    result <- list(irt = irt, fa = t, rho = r, tau = tau, n.obs = n.obs, 
        Call = cl)
    switch(typ, tet = {
        class(result) <- c("psych", "irt.fa")
    }, tetra = {
        class(result) <- c("psych", "irt.fa")
    }, poly = {
        class(result) <- c("psych", "irt.poly")
    }, irt.poly = {
        class(result) <- c("psych", "irt.poly")
    })
    if (plot) {
        pr <- plot(result)
        result$plot <- pr
    }
    return(result)
}


fa.graph <- function (fa.results, out.file = NULL, labels = NULL, cut = 0.3, 
    simple = TRUE, size = c(8, 6), node.font = c("Helvetica", 
        14), edge.font = c("Helvetica", 10), rank.direction = c("RL", 
        "TB", "LR", "BT"), digits = 1, main = "Factor Analysis", 
    ...) 
{
    Phi <- NULL
    if (!missing(out.file)) {
        out <- file(out.file, "w")
        on.exit(close(out))
    }
    else out <- stdout()
    if ((!is.matrix(fa.results)) && (!is.data.frame(fa.results))) {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) 
            Phi <- fa.results$Phi
    }
    else {
        factors <- as.matrix(fa.results)
    }
    rank.direction <- match.arg(rank.direction)
    if (length(labels) == 0) {
        labels <- rownames(fa.results$loadings)
    }
    else {
        labels = labels
    }
    num.var <- dim(factors)[1]
    if (is.null(num.var)) {
        num.var <- length(factors)
        num.factors <- 1
    }
    else {
        num.factors <- dim(factors)[2]
    }
    vars <- paste("V", 1:num.var, sep = "")
    fact <- paste("F", 1:num.factors, sep = "")
    cat(file = out, paste("digraph Factor ", " {\n", sep = ""))
    cat(file = out, paste("  rankdir=", rank.direction, ";\n", 
        sep = ""))
    cat(file = out, paste("  size=\"", size[1], ",", size[2], 
        "\";\n", sep = ""))
    cat(file = out, paste("  node [fontname=\"", node.font[1], 
        "\" fontsize=", node.font[2], " shape=box, width=2];\n", 
        sep = ""))
    cat(file = out, paste("  edge [fontname=\"", edge.font[1], 
        "\" fontsize=", edge.font[2], "];\n", sep = ""))
    if (simple) {
        rowmax <- apply(abs(factors), 1, max)
        factors[abs(factors) < rowmax] <- 0
    }
    for (i in 1:num.var) {
        cat(file = out, paste("V", i, "  [label = \"", labels[i], 
            "\"];\n", sep = ""))
    }
    cat(file = out, paste("node [shape=ellipse, width =\"1\"];\n", 
        sep = ""))
    for (nf in 1:num.factors) {
        for (i in 1:num.var) {
            if (abs(factors[i, nf]) > cut) {
                cat(file = out, paste(colnames(factors)[nf], 
                  "-> V", i, " [ label = ", round(factors[i, 
                    nf], digits), " ];\n", sep = ""))
            }
        }
    }
    if (!is.null(Phi)) {
        for (f in 2:num.factors) {
            for (f1 in 1:(f - 1)) {
                if (abs(Phi[f, f1]) > cut) {
                  cat(file = out, paste(colnames(factors)[f], 
                    " -> ", colnames(factors)[f1], " [ label = ", 
                    round(Phi[f, f1], digits), " , dir=\"both\" ];\n", 
                    sep = ""))
                }
            }
        }
    }
    cat(file = out, paste("{ rank=same;\n", sep = ""))
    for (i in 1:num.var) {
        cat(file = out, paste("V", i, ";", sep = ""))
    }
    cat(file = out, paste("}", sep = ""))
    cat(file = out, paste("{ rank=same;\n", sep = ""))
    for (nf in 1:num.factors) {
        cat(file = out, paste(paste(colnames(factors)[nf], ";", 
            sep = "")))
    }
    cat(file = out, paste("}}", sep = ""))
}


draw.cor <- function (r = 0.5, expand = 10, theta = 30, phi = 30, N = 101, 
    nbcol = 30, box = TRUE, main = "Bivariate density  rho = ", 
    cuts = NULL, all = TRUE, ellipses = TRUE, ze = 0.15) 
{
    sigma <- matrix(c(1, r, r, 1), 2, 2)
    x <- seq(-3, 3, length = N)
    y <- x
    f <- function(x, y, sigma = sigma) {
        r <- dmnorm(cbind(x, y), varcov = sigma)
    }
    z <- outer(x, y, f, sigma = sigma)
    nrz <- nrow(z)
    ncz <- ncol(z)
    jet.colors <- colorRampPalette(c("light blue", "red"))
    nbcol <- 100
    color <- jet.colors(nbcol)
    color <- jet.colors(nbcol)
    nbcol <- length(color)
    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, 
        -ncz]
    facetcol <- cut(zfacet, nbcol)
    if (is.null(cuts)) {
        pmat <- persp(x, y, z, col = color[facetcol], phi = phi, 
            theta = theta, scale = FALSE, expand = expand, box = box, 
            main = paste(main, r))
    }
    else {
        if (!all) {
            z[, y > cuts[2]] <- NA
            z[x > cuts[1], ] <- NA
        }
        z[, abs(y - cuts[2]) < 6/(N)] <- 0
        z[abs(x - cuts[1]) < 6/(N), ] <- 0
        pmat <- persp(x, y, z, col = color[facetcol], phi = phi, 
            theta = theta, scale = FALSE, expand = expand, box = box, 
            main = paste(main, r))
    }
    if (ellipses) {
        angles <- (0:N) * 2 * pi/N
        unit.circle <- cbind(cos(angles), sin(angles))
        if (abs(r) > 0) 
            theta1 <- sign(r)/sqrt(2)
        else theta1 = 1/sqrt(2)
        shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta1, 
            theta1, -theta1, theta1), ncol = 2, byrow = TRUE)
        ellipse <- unit.circle %*% shape * 2
        lines(trans3d(ellipse[, 1], ellipse[, 2], z = ze, pmat = pmat), 
            col = "red", lwd = 2)
    }
    pmat <- persp(x, y, z, col = color[facetcol], phi = phi, 
        theta = theta, scale = FALSE, expand = expand, box = box, 
        main = paste(main, r))
}


score.irt.poly <- function (stats, items, keys = NULL, cut = 0.3, bounds = c(-4, 
    4), mod = "logistic") 
{
    big.poly <- function(f, n.obs, stats, items, keys = NULL, 
        cut = 0.3, bounds = c(-5, 5), mod = "logistic") {
        nf <- ncol(stats$discrimination)
        diff <- stats$difficulty[[f]]
        if (nf < 2) {
            discrim <- stats$discrimination
            if (!is.null(keys)) {
                discrim <- discrim * abs(keys)
            }
        }
        else {
            discrim <- stats$discrimination[, f]
            if (!is.null(keys)) {
                discrim <- discrim * abs(keys[, f])
            }
        }
        cat <- dim(diff)[2]
        total <- rep(NA, n.obs)
        fit <- rep(NA, n.obs)
        theta <- rep(NA, n.obs)
        item.f <- t(items)
        item.f[abs(discrim) < cut] <- NA
        item.f <- t(item.f)
        if (!is.null(keys)) {
            item.f <- item.f[, (abs(keys[, f]) > 0), drop = FALSE]
            discrim.f <- discrim[(abs(keys[, f]) > 0), drop = FALSE]
            diffi.f <- diff[(abs(keys[, f]) > 0), byrows = TRUE]
            diffi.vect <- as.vector(t(diff[(abs(keys[, f]) > 
                0), byrows = TRUE]))
            discrim.F.vect <- rep(discrim.f, each = cat)
        }
        else {
            discrim.f <- discrim
            diffi.f <- diff
            diffi.vect <- as.vector(t(diff))
            discrim.F.vect <- rep(discrim.f, each = cat)
        }
        total <- rowMeans(t(t(item.f) * as.vector(sign(discrim.f))), 
            na.rm = TRUE)
        num.keyed <- rowSums(!is.na(item.f))
        num.reversed <- rowSums(!is.na(item.f[, discrim.f < 0, 
            drop = FALSE]))
        total <- total + num.reversed * (max.item - min.item + 
            1)/num.keyed + min.item
        total[is.nan(total)] <- NA
        count <- rowSums(!is.na(item.f))
        for (subj in 1:n.obs) {
            if (count[subj] > 0) {
                newscore <- NULL
                score <- item.f[subj, ]
                for (i in 1:ncol(item.f)) {
                  if (is.na(score[i])) {
                    newscore <- c(newscore, rep(NA, cat))
                  }
                  else {
                    if (very.close(score[i], (cat))) {
                      newscore <- c(newscore, rep(1, cat))
                    }
                    else {
                      newscore <- c(newscore, rep(1, score[i]), 
                        rep(0, cat - score[i]))
                    }
                  }
                }
                beta = discrim.F.vect[!is.na(score)]
                delta = diffi.vect[!is.na(score)]
                if ((very.close(total[subj], min.item)) | (very.close(total[subj], 
                  (max.item + min.item)))) {
                  if (very.close(total[subj], min.item)) {
                    if (mod == "logistic") {
                      myfit <- optimize(irtLimit.2par, bounds, 
                        beta = discrim.F.vect, delta = diffi.vect, 
                        scores = newscore)
                    }
                    else {
                      myfit <- suppressWarnings(optimize(irtLimit.2par.norm, 
                        bounds, beta = discrim.F.vect, delta = diffi.vect, 
                        scores = newscore))
                    }
                    theta[subj] <- myfit$minimum
                    fit[subj] <- myfit$objective
                  }
                  else {
                    if (very.close(total[subj], (max.item + min.item))) {
                      if (mod == "logistic") {
                        myfit <- optimize(irtLimit.2par, bounds, 
                          beta = discrim.F.vect, delta = diffi.vect, 
                          scores = newscore)
                      }
                      else {
                        myfit <- suppressWarnings(optimize(irtLimit.2par.norm, 
                          bounds, beta = discrim.F.vect, delta = diffi.vect, 
                          scores = newscore))
                      }
                      theta[subj] <- myfit$minimum
                      fit[subj] <- myfit$objective
                    }
                  }
                }
                else {
                  if (mod == "logistic") {
                    myfit <- optimize(irtLimit.2par, bounds, 
                      beta = discrim.F.vect, delta = diffi.vect, 
                      scores = newscore)
                  }
                  else {
                    myfit <- suppressWarnings(optimize(irtLimit.2par.norm, 
                      bounds, beta = discrim.F.vect, delta = diffi.vect, 
                      scores = newscore))
                  }
                  theta[subj] <- myfit$minimum
                  fit[subj] <- myfit$objective
                }
            }
            else {
                fit[subj] <- NA
                theta[subj] <- NA
            }
        }
        if ((!is.null(keys)) & (all(keys[, f] == -1))) {
            theta <- -theta
            total <- -total
        }
        theta[theta < bounds[1]] <- bounds[1]
        theta[theta > bounds[2]] <- bounds[2]
        scores <- list(theta, total, fit)
        return(scores)
    }
    min.item <- min(items, na.rm = TRUE)
    items <- items - min.item
    max.item <- max(items, na.rm = TRUE)
    nf <- length(stats$difficulty)
    n.obs <- dim(items)[1]
    nvar <- dim(items)[2]
    scores <- mcmapply(big.poly, 1:nf, MoreArgs = list(n.obs = n.obs, 
        stats = stats, items = items, keys = keys, cut = 0.3, 
        bounds = bounds, mod = mod))
    scores <- matrix(unlist(scores), ncol = nf * 3)
    scores <- scores[, c(seq(1, nf * 3, 3), seq(2, nf * 3 + 1, 
        3), seq(3, nf * 3 + 2, 3))]
    colnames(scores) <- paste(rep(c("theta", "total", "fit"), 
        each = nf), 1:nf, sep = "")
    return(scores)
}


factor.minres <- function (r, nfactors = 1, residuals = FALSE, rotate = "varimax", 
    n.obs = NA, scores = FALSE, SMC = TRUE, missing = FALSE, 
    impute = "median", min.err = 0.001, digits = 2, max.iter = 50, 
    symmetric = TRUE, warnings = TRUE, fm = "minres") 
{
    cl <- match.call()
    .Deprecated("fa", msg = "factor.minres is deprecated.  Please use the fa function.")
    "fit.residuals.ols" <- function(Psi, S, nf) {
        diag(S) <- 1 - Psi
        eigens <- eigen(S)
        eigens$values[eigens$values < .Machine$double.eps] <- 100 * 
            .Machine$double.eps
        if (nf > 1) {
            loadings <- eigens$vectors[, 1:nf] %*% diag(sqrt(eigens$values[1:nf]))
        }
        else {
            loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
        }
        model <- loadings %*% t(loadings)
        residual <- (S - model)^2
        diag(residual) <- 0
        error <- sum(residual)
    }
    "fit.residuals.min.res" <- function(Psi, S, nf) {
        diag(S) <- 1 - Psi
        eigens <- eigen(S)
        if (nf > 1) {
            loadings <- eigens$vectors[, 1:nf] %*% diag(sqrt(eigens$values[1:nf]))
        }
        else {
            loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
        }
        model <- loadings %*% t(loadings)
        residual <- (S - model)
        diag(residual) <- 0
        error <- det(residual)
    }
    "min.res" <- function(S, nf) {
        S.smc <- smc(S)
        if (sum(S.smc) == nf) {
            start <- rep(0.5, nf)
        }
        else {
            start <- 1 - S.smc
        }
        res <- optim(start, fit.residuals.ols, method = "L-BFGS-B", 
            lower = 0.005, upper = 1, control = c(list(fnscale = 1, 
                parscale = rep(0.01, length(start)))), nf = nf, 
            S = S)
        Lambda <- FAout(res$par, S, nf)
        result <- list(loadings = Lambda, res = res)
    }
    FAout <- function(Psi, S, q) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), 
            q)
        diag(sqrt(Psi)) %*% load
    }
    FAfn <- function(Psi, S, q) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
        e <- E$values[-(1L:q)]
        e <- sum(log(e) - e) - q + nrow(S)
        -e
    }
    if ((fm != "pa") & (fm != "minres")) {
        message("factor method not specified correctly, minimum residual used  used")
        fm <- "minres"
    }
    n <- dim(r)[2]
    if (n != dim(r)[1]) {
        n.obs <- dim(r)[1]
        if (scores) {
            x.matrix <- r
            if (missing) {
                miss <- which(is.na(x.matrix), arr.ind = TRUE)
                if (impute == "mean") {
                  item.means <- colMeans(x.matrix, na.rm = TRUE)
                  x.matrix[miss] <- item.means[miss[, 2]]
                }
                else {
                  item.med <- apply(x.matrix, 2, median, na.rm = TRUE)
                  x.matrix[miss] <- item.med[miss[, 2]]
                }
            }
        }
        r <- cor(r, use = "pairwise")
    }
    else {
        if (!is.matrix(r)) {
            r <- as.matrix(r)
        }
        sds <- sqrt(diag(r))
        r <- r/(sds %o% sds)
    }
    if (!residuals) {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), fit = 0)
    }
    else {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), residual = matrix(rep(0, n * 
                n), ncol = n), fit = 0)
    }
    r.mat <- r
    Phi <- NULL
    colnames(r.mat) <- rownames(r.mat) <- colnames(r)
    if (SMC) {
        if (nfactors < n/2) {
            diag(r.mat) <- smc(r)
        }
        else {
            if (warnings) 
                message("too many factors requested for this number of variables to use SMC, 1s used instead")
        }
    }
    orig <- diag(r)
    comm <- sum(diag(r.mat))
    err <- comm
    i <- 1
    comm.list <- list()
    if (fm == "pa") {
        while (err > min.err) {
            eigens <- eigen(r.mat, symmetric = symmetric)
            if (nfactors > 1) {
                loadings <- eigens$vectors[, 1:nfactors] %*% 
                  diag(sqrt(eigens$values[1:nfactors]))
            }
            else {
                loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
            }
            model <- loadings %*% t(loadings)
            new <- diag(model)
            comm1 <- sum(new)
            diag(r.mat) <- new
            err <- abs(comm - comm1)
            if (is.na(err)) {
                warning("imaginary eigen value condition encountered in fa,\n Try again with SMC=FALSE \n exiting fa")
                break
            }
            comm <- comm1
            comm.list[[i]] <- comm1
            i <- i + 1
            if (i > max.iter) {
                if (warnings) {
                  message("maximum iteration exceeded")
                }
                err <- 0
            }
        }
    }
    if (fm == "minres") {
        uls <- min.res(r, nfactors)
        eigens <- eigen(r)
        result$par <- uls$res
        loadings <- uls$loadings
    }
    if (!is.double(loadings)) {
        warning("the matrix has produced imaginary results -- proceed with caution")
        loadings <- matrix(as.double(loadings), ncol = nfactors)
    }
    if (FALSE) {
        if (nfactors > 1) {
            maxabs <- apply(apply(loadings, 2, abs), 2, which.max)
            sign.max <- vector(mode = "numeric", length = nfactors)
            for (i in 1:nfactors) {
                sign.max[i] <- sign(loadings[maxabs[i], i])
            }
            loadings <- loadings %*% diag(sign.max)
        }
        else {
            mini <- min(loadings)
            maxi <- max(loadings)
            if (abs(mini) > maxi) {
                loadings <- -loadings
            }
            loadings <- as.matrix(loadings)
            if (fm == "minres") {
                colnames(loadings) <- "MR1"
            }
            else {
                colnames(loadings) <- "PA1"
            }
        }
    }
    if (nfactors > 1) {
        sign.tot <- vector(mode = "numeric", length = nfactors)
        sign.tot <- sign(colSums(loadings))
        loadings <- loadings %*% diag(sign.tot)
    }
    else {
        if (sum(loadings) < 0) {
            loadings <- -as.matrix(loadings)
        }
        else {
            loadings <- as.matrix(loadings)
        }
        colnames(loadings) <- "MR1"
    }
    if (fm == "minres") {
        colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
    }
    else {
        colnames(loadings) <- paste("PA", 1:nfactors, sep = "")
    }
    rownames(loadings) <- rownames(r)
    loadings[loadings == 0] <- 10^-15
    model <- loadings %*% t(loadings)
    f.loadings <- loadings
    if (rotate != "none") {
        if (nfactors > 1) {
            if (rotate == "varimax" | rotate == "quartimax") {
                rotated <- do.call(rotate, list(loadings))
                loadings <- rotated$loadings
                Phi <- NULL
            }
            else {
                if ((rotate == "promax") | (rotate == "Promax")) {
                  pro <- Promax(loadings)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                }
                else {
                  if (rotate == "cluster") {
                    loadings <- varimax(loadings)$loadings
                    pro <- target.rot(loadings)
                    loadings <- pro$loadings
                    Phi <- pro$Phi
                  }
                  else {
                    if (rotate == "oblimin" | rotate == "quartimin" | 
                      rotate == "simplimax") {
                      if (!requireNamespace("GPArotation")) {
                        warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
                        Phi <- NULL
                      }
                      else {
                        ob <- do.call(rotate, list(loadings))
                        loadings <- ob$loadings
                        Phi <- ob$Phi
                      }
                    }
                  }
                }
            }
        }
    }
    if (nfactors > 1) {
        ev.rotated <- diag(t(loadings) %*% loadings)
        ev.order <- order(ev.rotated, decreasing = TRUE)
        loadings <- loadings[, ev.order]
    }
    rownames(loadings) <- colnames(r)
    if (!is.null(Phi)) {
        Phi <- Phi[ev.order, ev.order]
    }
    class(loadings) <- "loadings"
    if (nfactors < 1) 
        nfactors <- n
    result <- factor.stats(r, loadings, Phi, n.obs)
    result$communality <- round(diag(model), digits)
    result$uniquenesses <- round(diag(r - model), digits)
    result$values <- round(eigens$values, digits)
    result$loadings <- loadings
    if (!is.null(Phi)) {
        result$Phi <- Phi
    }
    if (fm == "pa") 
        result$communality.iterations <- round(unlist(comm.list), 
            digits)
    if (scores) {
        result$scores <- factor.scores(x.matrix, loadings)
    }
    result$factors <- nfactors
    result$fn <- "factor.minres"
    result$fm <- fm
    result$Call <- cl
    class(result) <- c("psych", "fa")
    return(result)
}


scoreFast <- function (keys, items, totals = FALSE, ilabels = NULL, missing = TRUE, 
    impute = "none", delete = TRUE, min = NULL, max = NULL, digits = 2) 
{
    cl <- match.call()
    raw.data <- TRUE
    if (impute == FALSE) 
        impute <- "none"
    if (is.list(keys)) {
        select <- sub("-", "", unlist(keys))
        select <- select[!duplicated(select)]
        items <- items[select]
        keys <- make.keys(items, keys)
    }
    keys <- as.matrix(keys)
    n.keys <- dim(keys)[2]
    n.items <- dim(keys)[1]
    abskeys <- abs(keys)
    keynames <- colnames(keys)
    num.item <- diag(t(abskeys) %*% abskeys)
    num.ob.item <- num.item
    if (!missing) 
        items <- na.omit(items)
    n.subjects <- dim(items)[1]
    items <- as.matrix(items)
    item.var <- apply(items, 2, sd, na.rm = TRUE)
    bad <- which((item.var == 0) | is.na(item.var))
    if ((length(bad) > 0) && delete) {
        for (baddy in 1:length(bad)) {
            warning("Item= ", colnames(items)[bad][baddy], " had no variance and was deleted from the data and the keys.")
        }
        items <- items[, -bad]
        keys <- as.matrix(keys[-bad, ])
        n.items <- n.items - length(bad)
        abskeys <- abs(keys)
        colnames(keys) <- keynames
    }
    item.means <- colMeans(items, na.rm = TRUE)
    if (is.null(min)) {
        min <- min(items, na.rm = TRUE)
    }
    if (is.null(max)) {
        max <- max(items, na.rm = TRUE)
    }
    miss.rep <- (is.na(items) + 0) %*% abs(keys)
    num.item <- diag(t(abskeys) %*% abskeys)
    num.ob.item <- num.item
    if (impute != "none") {
        miss <- which(is.na(items), arr.ind = TRUE)
        if (impute == "mean") {
            item.means <- colMeans(items, na.rm = TRUE)
            items[miss] <- item.means[miss[, 2]]
        }
        else {
            item.med <- apply(items, 2, median, na.rm = TRUE)
            items[miss] <- item.med[miss[, 2]]
        }
        scores <- items %*% keys
    }
    else {
        scores <- matrix(NaN, ncol = n.keys, nrow = n.subjects)
        totals <- FALSE
        for (scale in 1:n.keys) {
            pos.item <- items[, which(keys[, scale] > 0)]
            neg.item <- items[, which(keys[, scale] < 0)]
            neg.item <- max + min - neg.item
            sub.item <- cbind(pos.item, neg.item)
            scores[, scale] <- rowMeans(sub.item, na.rm = TRUE)
            rs <- rowSums(!is.na(sub.item))
            num.ob.item[scale] <- mean(rs[rs > 0])
        }
    }
    slabels <- colnames(keys)
    if (is.null(slabels)) {
        if (totals) {
            slabels <- paste("S", 1:n.keys, sep = "")
        }
        else {
            slabels <- paste("A", 1:n.keys, sep = "")
        }
    }
    colnames(scores) <- slabels
    results <- scores
    return(results)
}


cta.15 <- function (n = 3, t = 5000, cues = NULL, act = NULL, inhibit = NULL, 
    consume = NULL, ten = NULL, type = "both", fast = 2) 
{
    compare <- FALSE
    if (n > 4) {
        colours <- rainbow(n)
    }
    else {
        colours <- c("black", "blue", "red", "green")
    }
    step <- 0.05
    ten.start <- ten
    act.start <- act
    tendencies.m <- matrix(NA, ncol = t, nrow = n)
    actions.m <- matrix(NA, ncol = t, nrow = n)
    if (is.null(cues)) {
        cues <- 2^(n - 1:n)
    }
    if (is.null(inhibit)) {
        inhibit <- matrix(1, ncol = n, nrow = n)
        diag(inhibit) <- 0.05
    }
    if (n > 1) {
        colnames(inhibit) <- rownames(inhibit) <- paste("A", 
            1:n, sep = "")
    }
    if (is.null(consume)) {
        consume <- diag(0.05, ncol = n, nrow = n)
    }
    excite <- diag(step, n)
    if (is.null(ten.start)) {
        ten <- rep(0, n)
    }
    else {
        ten <- ten.start
    }
    if (is.null(act.start)) {
        act <- cues
    }
    else {
        act <- act.start
    }
    maxact <- minact <- minten <- maxten <- 0
    counts <- rep(0, n)
    transitions <- matrix(0, ncol = n, nrow = n)
    frequency <- matrix(0, ncol = n, nrow = n)
    actions <- tendencies <- rep(0, n)
    acts <- tends <- rep(0, n)
    colnames(frequency) <- paste("T", 1:n, sep = "")
    rownames(frequency) <- paste("F", 1:n, sep = "")
    names(tendencies) <- paste("T", 1:n, sep = "")
    names(actions) <- paste("A", 1:n, sep = "")
    old.act <- which.max(act)
    for (i in 1:t) {
        ten <- cues %*% excite + ten - act %*% excite %*% consume
        act <- ten %*% excite + act - act %*% excite %*% inhibit
        act[act < 0] <- 0
        tendencies <- tendencies + ten
        actions <- actions + act
        maxact <- max(maxact, act)
        minact <- min(minact, act)
        maxten <- max(maxten, ten)
        minten <- min(minten, ten)
        which.act <- which.max(act)
        counts[which.act] <- counts[which.act] + 1
        acts[which.act] <- acts[which.act] + act[which.act]
        tends[which.act] <- tends[which.act] + ten[which.act]
        transitions[old.act, which.act] <- transitions[old.act, 
            which.act] + 1
        if (old.act != which.act) {
            frequency[old.act, which.act] <- frequency[old.act, 
                which.act] + 1
            frequency[which.act, which.act] <- frequency[which.act, 
                which.act] + 1
        }
        old.act <- which.act
        tendencies.m[, i] <- ten
        actions.m[, i] <- act
    }
    yl <- range(tendencies.m)
    plot(tendencies.m[1, ], ylim = yl, xlab = "time", ylab = "Tendency", 
        col = "black", typ = "l", main = "Action tendencies over time", 
        lwd = 2)
    for (j in 2:n) {
        points(tendencies.m[j, ], lty = j, col = colours[j], 
            typ = "l", lwd = 2)
    }
    yl <- range(actions.m)
    plot(actions.m[1, ], ylim = yl, xlab = "time", ylab = "Actions", 
        col = "black", typ = "l", main = "Actions over time", 
        lwd = 2)
    for (j in 2:n) {
        points(actions.m[j, ], lty = j, col = colours[j], typ = "l", 
            lwd = 2)
    }
    if (FALSE) {
        plots <- 1
        action <- FALSE
        if (type != "none") {
            if (type == "state") {
                op <- par(mfrow = c(1, 1))
                if (is.null(ten.start)) {
                  ten <- rep(0, n)
                }
                else {
                  ten <- ten.start
                }
                if (is.null(act.start)) {
                  act <- cues
                }
                else {
                  act <- act.start
                }
                plot(ten[1], ten[2], xlim = c(minten, maxten), 
                  ylim = c(minten, maxten), col = "black", main = "State diagram", 
                  xlab = "Tendency 1", ylab = "Tendency 2")
                for (i in 1:t) {
                  ten <- cues %*% excite + ten - act %*% excite %*% 
                    consume
                  act <- ten %*% excite + act - act %*% excite %*% 
                    inhibit
                  act[act < 0] <- 0
                  if (!(i%%fast)) 
                    points(ten[1], ten[2], col = "black", pch = 20, 
                      cex = 0.2)
                }
            }
            else {
                if (type == "both") {
                  if (compare) {
                    op <- par(mfrow = c(2, 2))
                  }
                  else {
                    op <- par(mfrow = c(2, 1))
                  }
                  plots <- 2
                }
                else {
                  op <- par(mfrow = c(1, 1))
                }
                if (type == "action") {
                  action <- TRUE
                }
                else {
                  if (type == "tend") 
                    action <- FALSE
                }
                for (k in 1:plots) {
                  if (is.null(ten.start)) {
                    ten <- rep(0, n)
                  }
                  else {
                    ten <- ten.start
                  }
                  if (is.null(act.start)) {
                    act <- cues
                  }
                  else {
                    act <- act.start
                  }
                  if (action) 
                    plot(rep(1, n), act, xlim = c(0, t), ylim = c(minact, 
                      maxact), xlab = "time", ylab = "action", 
                      main = "Actions over time")
                  else plot(rep(1, n), ten, xlim = c(0, t), ylim = c(minten, 
                    maxten), xlab = "time", ylab = "action tendency", 
                    main = "Action Tendencies over time")
                  for (i in 1:t) {
                    ten <- cues %*% excite + ten - act %*% excite %*% 
                      consume
                    act <- ten %*% excite + act - act %*% excite %*% 
                      inhibit
                    act[act < 0] <- 0
                    if (!(i%%fast)) {
                      if (action) 
                        points(rep(i, n), act, col = colours, 
                          cex = 0.2)
                      else points(rep(i, n), ten, col = colours, 
                        cex = 0.2)
                    }
                  }
                  action <- TRUE
                }
            }
        }
    }
    acts <- acts/counts
    tends <- tends/counts
    cta.df <- data.frame(cues = cues, time = round(counts/t, 
        2), frequency = rowSums(frequency), tendencies = round(t(tendencies/t)), 
        actions = round(acts))
    results <- list(cues = cues, cta = cta.df, inihibition = inhibit, 
        time = counts/t, frequency = frequency, tendencies = tendencies/t, 
        actions = actions/t)
    class(results) <- c("psych", "cta")
    return(results)
}


lavaan.diagram <- function (fit, title, ...) 
{
    if (is.null(fit@Model@GLIST$beta)) {
        model <- "cfa"
    }
    else {
        model <- "sem"
    }
    if (missing(title)) {
        if (model == "cfa") {
            title = "Confirmatory structure"
        }
        else {
            title = "Structural model"
        }
    }
    fx = fit@Model@GLIST$lambda
    colnames(fx) <- fit@Model@dimNames[[1]][[2]]
    rownames(fx) <- fit@Model@dimNames[[1]][[1]]
    Phi <- fit@Model@GLIST$psi
    Rx <- fit@Model@GLIST$theta
    v.labels <- fit@Model@dimNames[[1]][[1]]
    if (model == "cfa") {
        structure.diagram(fx = fx, Phi = Phi, Rx = Rx, labels = v.labels, 
            main = title, ...)
    }
    else {
        structure.diagram(fx = fx, Phi = t(fit@Model@GLIST$beta), 
            Rx = Rx, labels = v.labels, main = title, ...)
    }
}


mardia <- function (x, na.rm = TRUE, plot = TRUE) 
{
    cl <- match.call()
    x <- as.matrix(x)
    if (na.rm) 
        x <- na.omit(x)
    n <- dim(x)[1]
    p <- dim(x)[2]
    x <- scale(x, scale = FALSE)
    S <- cov(x)
    S.inv <- solve(S)
    D <- x %*% S.inv %*% t(x)
    b1p <- sum(D^3)/n^2
    b2p <- tr(D^2)/n
    chi.df <- p * (p + 1) * (p + 2)/6
    k <- (p + 1) * (n + 1) * (n + 3)/(n * ((n + 1) * (p + 1) - 
        6))
    small.skew <- n * k * b1p/6
    M.skew <- n * b1p/6
    M.kurt <- (b2p - p * (p + 2)) * sqrt(n/(8 * p * (p + 2)))
    p.skew <- 1 - pchisq(M.skew, chi.df)
    p.small <- 1 - pchisq(small.skew, chi.df)
    p.kurt <- 2 * (1 - pnorm(abs(M.kurt)))
    d = sqrt(diag(D))
    if (plot) {
        qqnorm(d)
        qqline(d)
    }
    results <- list(n.obs = n, n.var = p, b1p = b1p, b2p = b2p, 
        skew = M.skew, small.skew = small.skew, p.skew = p.skew, 
        p.small = p.small, kurtosis = M.kurt, p.kurt = p.kurt, 
        d = d, Call = cl)
    class(results) <- c("psych", "mardia")
    return(results)
}


Yule2poly.matrix <- function (x, v) 
{
    .Deprecated("Yule2tetra", msg = "This function has been replaced by Yule2tetra")
}


sim.hierarchical <- function (gload = NULL, fload = NULL, n = 0, raw = FALSE, mu = NULL) 
{
    cl <- match.call()
    if (is.null(gload)) 
        gload = matrix(c(0.9, 0.8, 0.7), nrow = 3)
    if (is.null(fload)) {
        fload <- matrix(c(0.8, 0.7, 0.6, rep(0, 9), 0.7, 0.6, 
            0.5, rep(0, 9), 0.6, 0.5, 0.4), ncol = 3)
    }
    fcor <- gload %*% t(gload)
    diag(fcor) <- 1
    model <- fload %*% fcor %*% t(fload)
    diag(model) <- 1
    nvar <- dim(fload)[1]
    colnames(model) <- rownames(model) <- paste("V", 1:nvar, 
        sep = "")
    if (n > 0) {
        if (is.null(mu)) 
            mu <- rep(0, nvar)
        eX <- eigen(model)
        observed <- matrix(rnorm(nvar * n), n)
        observed <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
            0)), nvar) %*% t(observed))
        observed <- t(t(observed) + mu)
        colnames(observed) <- paste("V", 1:nvar, sep = "")
        r <- cor(observed)
        if (!raw) {
            result <- list(model = model, r = r, N = n, Call = cl)
        }
        else {
            result <- list(model = model, r = r, observed = observed, 
                N = n, Call = cl)
        }
        class(result) <- c("psych", "sim")
        return(result)
    }
    else {
        return(model)
    }
}


scaling.fits <- function (model, data, test = "logit", digits = 2, rowwise = TRUE) 
{
    model <- as.matrix(model)
    data <- as.matrix(data)
    if (test == "choice") {
        model <- as.vector(model)
        if (min(model) <= 0) 
            model <- model - min(model)
        prob = model/(model %+% t(model))
    }
    else {
        pdif <- model %+% -t(model)
        if (test == "logit") {
            prob <- 1/(1 + exp(-pdif))
        }
        else {
            if (test == "normal") {
                prob <- pnorm(pdif)
            }
        }
    }
    if (rowwise) {
        prob = 1 - prob
    }
    error <- data - prob
    sum.error2 <- sum(error^2, na.rm = TRUE)
    sum.data2 <- sum(data^2, na.rm = TRUE)
    gof <- 1 - sum.error2/sum.data2
    fit <- list(GF = gof, original = sum.data2, resid = sum.error2, 
        residual = round(error, digits))
    return(fit)
}


describeData <- function (x, head = 4, tail = 4) 
{
    valid <- function(x) {
        sum(!is.na(x))
    }
    nvar <- ncol(x)
    all.numeric <- nvar
    ans <- matrix(NA, nrow = nvar, ncol = 2)
    nobs <- nrow(x)
    cc <- 0
    cc <- try(complete.cases(x), silent = TRUE)
    if (class(cc) == "try-error") 
        cc <- NA
    cc <- sum(cc, na.rm = TRUE)
    for (i in 1:nvar) {
        if (is.numeric(x[, i])) {
            ans[i, 2] <- 1
        }
        else {
            if ((is.factor(x[, i])) || (is.logical(x[, i]))) {
                ans[i, 2] <- 2
            }
            else {
                if (is.character(x[, i])) {
                  ans[i, 2] <- 3
                }
                else {
                  ans[i, 2] <- 4
                }
            }
        }
        ans[i, 1] <- valid(x[, i])
    }
    if (is.numeric(unlist(x))) {
        all.numeric <- TRUE
    }
    else {
        all.numeric <- FALSE
    }
    H1 <- t(x[1:head, 1:nvar])
    T1 <- t(x[(nobs - tail + 1):nobs, 1:nvar])
    temp <- data.frame(V = 1:nvar, ans, H1, T1)
    colnames(temp) <- c("variable #", "n.obs", "type", paste("H", 
        1:head, sep = ""), paste("T", 1:tail, sep = ""))
    rownames(temp)[temp[, "type"] != 1] <- paste(rownames(temp)[temp[, 
        "type"] != 1], "*", sep = "")
    result <- (list(n.obs = nobs, nvar = nvar, all.numeric = all.numeric, 
        complete.cases = cc, variables = temp))
    class(result) <- c("psych", "describeData")
    return(result)
}


cosinor.period <- function (angle, x = NULL, code = NULL, data = NULL, hours = TRUE, 
    period = seq(23, 26, 1), plot = FALSE, opti = FALSE, na.rm = TRUE) 
{
    if (!is.null(data)) {
        if (is.matrix(data)) 
            data <- data.frame(data)
        if (is.character(angle)) 
            angle <- which(colnames(data) == angle)
        if (!is.null(code)) {
            if (is.character(code)) 
                codeloc <- which(colnames(data) == code)
            x <- data[, c(angle, x, codeloc)]
        }
        else {
            x <- data[, c(angle, x)]
        }
        angle <- x[1]
        x <- x[-1]
    }
    else {
        if (is.null(x) && is.null(code)) {
            angle <- data.frame(angle)
            x <- angle
            angle <- angle[, 1]
        }
        else {
            x <- data.frame(x)
            x <- cbind(angle, x)
            angle <- x[1]
            x <- x[-1]
        }
    }
    xdata <- x
    old.angle <- angle
    per <- period
    fit.period <- list()
    for (i in 1:length(per)) {
        period <- per[i]
        if (hours) {
            angle <- old.angle * 2 * pi/period
            x <- cbind(angle, xdata)
        }
        nvar <- dim(xdata)[2] - 1
        if (is.null(code)) {
            fit <- cosinor1(angle, x[-1], period = period, opti = opti, 
                na.rm = na.rm)
            m.resp <- mean(x[, 1])
            s.resp <- sd(x[, 1])
        }
        else {
            fit.list <- by(x, x[code], function(x) cosinor1(angle = x[1], 
                x = x[-c(1, which(colnames(x) == code))], period = period, 
                opti = opti, na.rm = na.rm))
            ncases <- length(fit.list)
            fit <- matrix(unlist(fit.list), nrow = ncases, byrow = TRUE)
            colnames(fit) <- c(paste(colnames(x)[-c(1, which(colnames(x) == 
                code))], "phase", sep = "."), paste(colnames(x)[-c(1, 
                which(colnames(x) == code))], "fit", sep = "."), 
                paste(colnames(x)[-c(1, which(colnames(x) == 
                  code))], "amp", sep = "."), paste(colnames(x)[-c(1, 
                  which(colnames(x) == code))], "sd", sep = "."), 
                paste(colnames(x)[-c(1, which(colnames(x) == 
                  code))], "mean", sep = "."), paste(colnames(x)[-c(1, 
                  which(colnames(x) == code))], "intercept", 
                  sep = "."))
            rownames(fit) <- names(fit.list)
        }
        fit.period[[i]] <- list(fit)
    }
    x <- NA
    ncols <- 6 * length(x)
    fit.m <- matrix(unlist(fit.period), nrow = ncases, byrow = FALSE)
    maxfit <- per
    np <- length(per)
    fits <- cbind(matrix(NA, nrow = ncases, ncol = nvar), fit)
    for (j in 1:ncases) {
        for (i in 1:nvar) {
            for (p in 1:np) {
                maxfit[p] <- fit.period[[p]][[1]][j, i + nvar]
            }
            max.period <- which.max(maxfit)
            fits[j, i] <- per[max.period]
            fits[j, i + nvar] <- fit.period[[max.period]][[1]][j, 
                i]
            fits[j, i + 2 * nvar] <- fit.period[[max.period]][[1]][j, 
                i + nvar]
            fits[j, i + 3 * nvar] <- fit.period[[max.period]][[1]][j, 
                i + 2 * nvar]
            fits[j, i + 4 * nvar] <- fit.period[[max.period]][[1]][j, 
                i + 3 * nvar]
            fits[j, i + 5 * nvar] <- fit.period[[max.period]][[1]][j, 
                i + 4 * nvar]
            fits[j, i + 6 * nvar] <- fit.period[[max.period]][[1]][j, 
                i + 5 * nvar]
        }
    }
    return(fits)
}


phi2tetra <- function (ph, m, n = NULL, correct = TRUE) 
{
    if (!is.matrix(ph) && !is.data.frame(ph)) {
        result <- phi2tet(ph, m[1], m[2], n = n, correct = correct)
    }
    else {
        nvar <- nrow(ph)
        if (nvar != ncol(ph)) {
            stop("Matrix must be square")
        }
        if (length(m) != nvar) {
            stop("length of m must match the number of variables")
        }
        result <- as.matrix(ph)
        for (i in 2:nvar) {
            for (j in 1:(i - 1)) {
                result[i, j] <- result[j, i] <- phi2tet(ph[i, 
                  j], m[i], m[j], n = n, correct = correct)
            }
        }
    }
    return(result)
}


headTail <- function (x, hlength = 4, tlength = 4, digits = 2, ellipsis = TRUE) 
{
    if (is.data.frame(x) | is.matrix(x)) {
        if (is.matrix(x)) 
            x <- data.frame(unclass(x))
        nvar <- dim(x)[2]
        dots <- rep("...", nvar)
        h <- data.frame(head(x, hlength))
        t <- data.frame(tail(x, tlength))
        for (i in 1:nvar) {
            if (is.numeric(h[1, i])) {
                h[i] <- round(h[i], digits)
                t[i] <- round(t[i], digits)
            }
            else {
                dots[i] <- NA
            }
        }
        if (ellipsis) {
            head.tail <- rbind(h, "..." = dots, t)
        }
        else {
            head.tail <- rbind(h, t)
        }
    }
    else {
        h <- head(x, hlength)
        t <- tail(x, tlength)
        if (ellipsis) {
            head.tail <- rbind(h, "...       ...", t)
        }
        else {
            head.tail <- rbind(h, t)
            head.tail <- as.matrix(head.tail)
        }
    }
    return(head.tail)
}


resid.psych <- function (object, ...) 
{
    residuals(object)
}


winsor.sd <- function (x, trim = 0.2, na.rm = TRUE) 
{
    if (is.vector(x)) {
        ans <- sqrt(win.var(x, trim = trim, na.rm = na.rm))
    }
    else {
        if (is.matrix(x) | is.data.frame(x)) {
            ans <- apply(x, 2, win.var, trim = trim, na.rm = na.rm)
            ans <- sqrt(ans)
        }
    }
    return(ans)
}


irt.item.diff.rasch <- function (items) 
{
    ncases <- nrow(items)
    item.mean <- colMeans(items, na.rm = TRUE)
    item.mean[item.mean < (1/ncases)] <- 1/ncases
    irt.item.diff.rasch <- log((1/item.mean) - 1)
}


ICLUST.graph <- function (ic.results, out.file, min.size = 1, short = FALSE, 
    labels = NULL, size = c(8, 6), node.font = c("Helvetica", 
        14), edge.font = c("Helvetica", 12), rank.direction = c("RL", 
        "TB", "LR", "BT"), digits = 2, title = "ICLUST", ...) 
{
    if (!missing(out.file)) {
        out <- file(out.file, "w")
        on.exit(close(out))
    }
    else out <- stdout()
    results <- ic.results$results
    if (length(labels) == 0) {
        var.labels <- rownames(ic.results$loadings)
    }
    else {
        var.labels = labels
    }
    clusters <- as.matrix(ic.results$clusters)
    num <- nrow(results)
    if (short) {
        var.labels <- paste("V", 1:nrow, (var.labels), sep = "")
    }
    rank.direction <- match.arg(rank.direction)
    cat(file = out, paste("digraph ICLUST", " {\n", sep = ""))
    cat(file = out, paste("  rankdir=", rank.direction, ";\n", 
        sep = ""))
    cat(file = out, paste("  size=\"", size[1], ",", size[2], 
        "\";\n", sep = ""))
    cat(file = out, paste("  node [fontname=\"", node.font[1], 
        "\" fontsize=", node.font[2], " shape=box, width=2];\n", 
        sep = ""))
    cat(file = out, paste("  edge [fontname=\"", edge.font[1], 
        "\" fontsize=", edge.font[2], "];\n", sep = ""))
    cat(file = out, paste(" label = \"", title, "\";\n\tfontsize=20;\n", 
        sep = ""))
    num.var <- nrow(results) + 1
    if (num.var > dim(clusters)[1]) {
        num.var <- dim(clusters)[1]
    }
    for (i in 1:num.var) {
        if (max(clusters[i, ]) > 0) {
            cat(file = out, paste("V", i, "  [label = \"", var.labels[i], 
                "\"];\n", sep = ""))
        }
        else {
            cat(file = out, paste("V", i, "  [label = \"-", var.labels[i], 
                "\"];\n", sep = ""))
        }
    }
    cat(file = out, paste("node [shape=ellipse, width =\"1\"];\n", 
        sep = ""))
    for (i in 1:num) {
        if (results[i, 1] > 0) {
            cat(file = out, paste(row.names(results)[i], "-> ", 
                results[i, 1], " [ label = ", round(results[i, 
                  "r1"], digits), " ];\n", sep = ""))
            cat(file = out, paste(row.names(results)[i], "-> ", 
                results[i, 2], " [ label = ", round(results[i, 
                  "r2"], digits), " ];\n", sep = ""))
        }
    }
    for (i in 1:num) {
        if (results[i, 1] > 0) {
            if (results[i, "size"] > min.size) {
                cat(file = out, paste(row.names(results)[i], 
                  "  [label =   \"", row.names(results)[i], "\\n  alpha= ", 
                  round(results[i, "alpha"], digits), "\\n beta=  ", 
                  round(results[i, "beta"], digits), "\\nN= ", 
                  results[i, "size"], "\"] ;\n", sep = ""))
            }
            else {
                cat(file = out, paste(row.names(results)[i], 
                  " ;\n", sep = ""))
            }
        }
    }
    cat(file = out, paste("{ rank=same;\n", sep = ""))
    for (i in 1:num.var) {
        cat(file = out, paste("V", i, ";", sep = ""))
    }
    cat(file = out, paste("}}", sep = ""))
}


circ.simulation <- function (samplesize = c(100, 200, 400, 800), numberofvariables = c(16, 
    32, 48, 72)) 
{
    ncases = length(samplesize)
    nvar <- length(numberofvariables)
    results <- matrix(NaN, ncol = ncases, nrow = nvar * ncases)
    results.ls <- list()
    case <- 1
    for (ss in 1:ncases) {
        for (nv in 1:nvar) {
            circ.data <- circ.sim(nvar = numberofvariables[nv], 
                nsub = samplesize[ss])
            sim.data <- circ.sim(nvar = numberofvariables[nv], 
                nsub = samplesize[ss], circum = FALSE)
            elipse.data <- circ.sim(nvar = numberofvariables[nv], 
                nsub = samplesize[ss], yloading = 0.4)
            r.circ <- cor(circ.data)
            r.sim <- cor(sim.data)
            r.elipse <- cor(elipse.data)
            pc.circ <- principal(r.circ, 2)
            pc.sim <- principal(r.sim, 2)
            pc.elipse <- principal(r.elipse, 2)
            case <- case + 1
            results.ls[[case]] <- list(numberofvariables[nv], 
                samplesize[ss], circ.tests(pc.circ), circ.tests(pc.elipse), 
                circ.tests(pc.sim))
        }
    }
    results.mat <- matrix(unlist(results.ls), ncol = 14, byrow = TRUE)
    colnames(results.mat) <- c("nvar", "n", "c-gap", "c-fisher", 
        "c-RT", "c-VT", "e-gap", "e-fisher", "e-RT", "e-VT", 
        "s-gap", "s-fisher", "s-RT", "s-VT")
    results.df <- data.frame(results.mat)
    return(results.df)
}


fa.sort <- function (fa.results, polar = FALSE) 
{
    omega <- FALSE
    con.i <- FALSE
    Structure <- NULL
    if (length(class(fa.results)) > 1) {
        value <- class(fa.results)[2]
    }
    else {
        value = "other"
    }
    switch(value, omega = {
        omega <- TRUE
        factors <- as.matrix(fa.results$schmid$oblique)
        sl <- fa.results$schmid$sl
    }, fa.ci = {
        factors <- fa.results$loadings
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
        con.i <- TRUE
        ci <- fa.results$cis$ci
        cip <- fa.results$cis$p
    }, iclust = {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
    }, fa = {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
        Structure <- fa.results$Structure
    }, principal = {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
    }, extension = {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
    }, extend = {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Structure)) Structure <- fa.results$Structure
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
    }, other = {
        factors <- fa.results
    })
    nitems <- dim(factors)[1]
    nfactors <- dim(factors)[2]
    total.ord <- rep(NA, nitems)
    if (polar) {
        pol.ord <- polar(factors)[, 1]
        factors[1:nitems, ] <- factors[pol.ord, ]
        rownames(factors)[1:nitems] <- rownames(factors)[pol.ord]
    }
    else {
        if (is.null(rownames(factors))) {
            rownames(factors) <- paste("V", 1:nitems)
        }
        loads <- data.frame(item = seq(1:nitems), cluster = rep(0, 
            nitems))
        loads$cluster <- apply(abs(factors), 1, which.max)
        ord <- sort(loads$cluster, index.return = TRUE)
        factors[1:nitems, ] <- factors[ord$ix, ]
        if (!is.null(Structure)) {
            Structure[1:nitems, ] <- Structure[ord$ix, ]
            rownames(Structure)[1:nitems] <- rownames(Structure)[ord$ix]
        }
        rownames(factors)[1:nitems] <- rownames(factors)[ord$ix]
        total.ord <- ord$ix
        if (con.i) {
            ci[1:nitems, ] <- ci[ord$ix, ]
            cip[1:nitems, ] <- cip[ord$ix, ]
        }
        items <- table(loads$cluster)
        first <- 1
        item <- loads$item
        for (i in 1:length(items)) {
            if (items[i] > 0) {
                last <- first + items[i] - 1
                ord <- sort(abs(factors[first:last, i]), decreasing = TRUE, 
                  index.return = TRUE)
                factors[first:last, ] <- factors[item[ord$ix + 
                  first - 1], ]
                loads[first:last, 1] <- item[ord$ix + first - 
                  1]
                if (!is.null(Structure)) {
                  Structure[first:last, ] <- Structure[item[ord$ix + 
                    first - 1], ]
                  rownames(Structure)[first:last] <- rownames(Structure)[ord$ix + 
                    first - 1]
                }
                rownames(factors)[first:last] <- rownames(factors)[ord$ix + 
                  first - 1]
                if (con.i) {
                  ci[first:last, ] <- ci[item[ord$ix + first - 
                    1], ]
                  cip[first:last, ] <- cip[item[ord$ix + first - 
                    1], ]
                }
                total.ord[first:last] <- total.ord[ord$ix + first - 
                  1]
                first <- first + items[i]
            }
        }
    }
    if (omega) {
        fa.results$schmid$oblique <- factors
        loads <- data.frame(item = seq(1:nitems), cluster = rep(0, 
            nitems))
        nfactors <- dim(sl)[2] - 4
        if (nfactors > 1) {
            loads$cluster <- apply(abs(sl[, 2:(nfactors + 1)]), 
                1, which.max) + 1
        }
        else {
            loads$cluster <- rep(1, nitems)
        }
        ord <- sort(loads$cluster, index.return = TRUE)
        sl[1:nitems, ] <- sl[ord$ix, ]
        rownames(sl)[1:nitems] <- rownames(sl)[ord$ix]
        items <- table(loads$cluster)
        first <- 1
        item <- loads$item
        for (i in 1:length(items)) {
            if (items[i] > 0) {
                last <- first + items[i] - 1
                ord <- sort(abs(sl[first:last, i + 1]), decreasing = TRUE, 
                  index.return = TRUE)
                sl[first:last, ] <- sl[item[ord$ix + first - 
                  1], ]
                loads[first:last, 1] <- item[ord$ix + first - 
                  1]
                rownames(sl)[first:last] <- rownames(sl)[ord$ix + 
                  first - 1]
                first <- first + items[i]
            }
        }
        fa.results$schmid$sl <- sl
    }
    else {
        if ((!is.matrix(fa.results)) && (!is.data.frame(fa.results))) {
            fa.results$loadings <- factors
            if (con.i) {
                rownames(ci) <- rownames(factors)
                fa.results$ci <- ci
                rownames(cip) <- rownames(factors)
                colnames(cip) <- colnames(factors)
                fa.results$cip <- cip
            }
        }
        else {
            fa.results <- factors
        }
    }
    if (is.list(fa.results)) {
        fa.results$order <- total.ord
        fa.results$complexity <- fa.results$complexity[total.ord]
        fa.results$communality <- fa.results$communality[total.ord]
        fa.results$uniquenesses <- fa.results$uniquenesses[total.ord]
        if (!is.null(Structure)) {
            fa.results$Structure <- Structure
        }
    }
    return(fa.results)
}


p.rep <- function (p = 0.05, n = NULL, twotailed = FALSE) 
{
    df <- n - 2
    if (twotailed) 
        p <- 2 * p
    p.rep <- pnorm(qnorm((1 - p))/sqrt(2))
    if (!is.null(n)) {
        t <- -qt(p/2, df)
        r.equiv <- sqrt(t^2/(t^2 + df))
        dprime = 2 * t * sqrt(1/df)
        return(list(p.rep = p.rep, d.prime = dprime, r.equiv = r.equiv))
    }
    else {
        return(p.rep)
    }
}


statsBy.boot.summary <- function (res.list, var = "ICC2") 
{
    nreps <- length(res.list)
    nvar <- length(res.list[[1]][[var]])
    cnames <- names(res.list[[1]][[var]])
    temp <- matrix(NaN, ncol = nvar, nrow = nreps)
    colnames(temp) <- cnames
    for (i in 1:nreps) {
        temp[i, ] <- res.list[[i]][[var]]
    }
    return(temp)
}


sim.rasch <- function (nvar = 5, n = 500, low = -3, high = 3, d = NULL, a = 1, 
    mu = 0, sd = 1) 
{
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    theta <- rnorm(n, mu, sd)
    item <- matrix(t(1/(1 + exp(a * t(-theta %+% t(d))))), n, 
        nvar)
    item[] <- rbinom(n * nvar, 1, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(items = item, tau = d, theta = theta)
    return(result)
}


corPlot <- function (r, numbers = FALSE, colors = TRUE, n = 51, main = NULL, 
    zlim = c(-1, 1), show.legend = TRUE, labels = NULL, n.legend = 10, 
    keep.par = TRUE, select = NULL, pval = NULL, cuts = c(0.001, 
        0.01), cex, MAR, upper = TRUE, diag = TRUE, ...) 
{
    if (keep.par) 
        op <- par(no.readonly = TRUE)
    if (missing(MAR)) 
        MAR <- 5
    if (is.null(main)) {
        main <- "Correlation plot"
    }
    if (!is.matrix(r) & (!is.data.frame(r))) {
        if ((length(class(r)) > 1) & (class(r)[1] == "psych")) {
            switch(class(r)[2], omega = {
                r <- r$schmid$sl
                nff <- ncol(r)
                r <- r[, 1:(nff - 3)]
            }, cor.ci = {
                pval <- 2 * (1 - r$ptci)
                r <- r$rho
            }, fa = {
                r <- r$loadings
            }, pc = {
                r <- r$loadings
            }, principal = {
                r <- r$loadings
            })
        }
    }
    r <- as.matrix(r)
    if (min(dim(r)) < 2) {
        stop("You need at least two dimensions to make a meaningful plot")
    }
    minx <- min(r, na.rm = TRUE)
    maxx <- max(r, na.rm = TRUE)
    if ((minx < -1) | (maxx > 1)) 
        r <- cor(r, use = "pairwise")
    if (is.null(n)) {
        n <- dim(r)[2]
    }
    nf <- dim(r)[2]
    nvar <- dim(r)[1]
    if (!upper) 
        r[col(r) > row(r)] <- NA
    if (!diag) 
        r[col(r) == row(r)] <- NA
    if (nf == nvar) 
        r <- t(r)
    if (missing(pval) | is.null(pval)) {
        pval <- matrix(rep(1, nvar * nf), nvar)
    }
    else {
        if (length(pval) != nvar * nf) {
            pr = matrix(0, nvar, nf)
            pr[row(pr) > col(pr)] <- pval
            pr <- pr + t(pr)
            diag(pr) <- 0
            pval <- pr
        }
        pval <- con2cat(pval, cuts = cuts)
        pval <- (length(cuts) + 1 - pval)/length(cuts)
        pval <- t(pval)
    }
    if (is.null(labels)) {
        if (is.null(rownames(r))) 
            rownames(r) <- paste("V", 1:nvar)
        if (is.null(colnames(r))) 
            colnames(r) <- paste("V", 1:nf)
    }
    else {
        rownames(r) <- colnames(r) <- labels
    }
    max.len <- max(nchar(rownames(r)))/6
    if (is.null(zlim)) {
        zlim <- range(r)
    }
    if (colors) {
        gr <- colorRampPalette(c("red", "white", "blue"))
        colramp <- gr(n)
    }
    else {
        colramp <- grey((n:0)/n)
    }
    if (nvar != nf) {
        r <- t(r)
    }
    if (!is.null(select)) {
        r <- r[select, select]
        pval <- pval[select, select]
        nvar <- length(select)
    }
    ord1 <- seq(nvar, 1, -1)
    if (nf == nvar) {
        r <- r[, ord1]
        pval <- pval[, ord1]
    }
    else {
        r <- r[, ord1]
        pval <- t(pval[ord1, ])
    }
    par(mar = c(MAR + max.len, MAR + max.len, 4, 0.5))
    if (show.legend) {
        layout(matrix(c(1, 2), nrow = 1), widths = c(0.9, 0.1), 
            heights = c(1, 1))
    }
    image(r, col = colramp, axes = FALSE, main = main, zlim = zlim)
    box()
    if (nf < nvar) {
        at1 <- (0:(nf - 1))/(nf - 1)
    }
    else {
        at1 <- (0:(nvar - 1))/(nvar - 1)
    }
    at2 <- (0:(nvar - 1))/(nvar - 1)
    if (max.len > 0.5) {
        axis(2, at = at2, labels = colnames(r), las = 1, ...)
        axis(1, at = at1, labels = rownames(r), las = 2, ...)
    }
    else {
        axis(2, at = at2, labels = colnames(r), ...)
        axis(1, at = at1, labels = rownames(r), las = 1, ...)
    }
    if (numbers) {
        rx <- rep(at1, ncol(r))
        ry <- rep(at2, each = nrow(r))
        rv <- round(r, 2)
        if (missing(cex)) 
            cex = 10/max(nrow(r), ncol(r))
        text(rx, ry, rv, cex = pval * cex, ...)
    }
    if (show.legend) {
        leg <- matrix(seq(from = zlim[1], to = zlim[2], by = (zlim[2] - 
            zlim[1])/n), nrow = 1)
        par(mar = c(MAR, 0, 4, 3))
        image(leg, col = colramp, axes = FALSE, zlim = zlim)
        at2 <- seq(0, 1, 1/n.legend)
        labels = seq(zlim[1], zlim[2], (zlim[2] - zlim[1])/(length(at2) - 
            1))
        axis(4, at = at2, labels = labels, las = 2, ...)
    }
    if (keep.par) 
        par(op)
}


factor.residuals <- function (r, f) 
{
    if (is.matrix(f)) {
        rstar <- r - factor.model(f)
    }
    else {
        f <- f$loadings
        rstar <- r - factor.model(f)
    }
    return(rstar)
}


sim.irt <- function (nvar = 5, n = 500, low = -3, high = 3, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, mod = "logistic") 
{
    if (mod == "logistic") {
        result <- sim.npl(nvar, n, low, high, a, c, z, d, mu, 
            sd)
    }
    else {
        result <- sim.npn(nvar, n, low, high, a, c, z, d, mu, 
            sd)
    }
    return(result)
}


cortest.bartlett <- function (R, n = NULL, diag = TRUE) 
{
    if (dim(R)[1] != dim(R)[2]) {
        n <- dim(R)[1]
        message("R was not square, finding R from data")
        R <- cor(R, use = "pairwise")
    }
    p <- dim(R)[2]
    if (!is.matrix(R)) 
        R <- as.matrix(R)
    if (is.null(n)) {
        n <- 100
        warning("n not specified, 100 used")
    }
    if (diag) 
        diag(R) <- 1
    detR <- det(R)
    statistic <- -log(detR) * (n - 1 - (2 * p + 5)/6)
    df <- p * (p - 1)/2
    pval <- pchisq(statistic, df, lower.tail = FALSE)
    bartlett <- list(chisq = statistic, p.value = pval, df = df)
    return(bartlett)
}


interp.values <- function (x, w = 1, na.rm = TRUE) 
{
    n <- length(x)
    tabx <- table(x)
    cv <- as.numeric(names(tabx))
    k <- 1
    v <- x[order(x)]
    for (i in 1:length(tabx)) {
        for (j in 1:tabx[i]) {
            v[k] <- i - 0.5 * w + j/(tabx[i] + 1)
            k <- k + 1
        }
    }
    return(v)
}


residuals.psych <- function (object, ...) 
{
    result <- NULL
    if (length(class(object)) > 1) {
        value <- class(object)[2]
    }
    else {
        stop("No appropriate residual found")
    }
    switch(value, fa = {
        residual <- object$residual
    }, principal = {
        residual <- object$residual
    }, omega = {
        residual <- object$stats$residual
    }, irt.fa = {
        residual <- object$fa$residual
    }, esem = {
        residual <- object$residual
    }, extension = {
        residual <- object$resid
    })
    class(residual) <- c("psych", "residuals")
    return(residual)
}


item.lookup <- function (f, m, dictionary, cut = 0.3, digits = 2) 
{
    f <- fa.sort(f)
    if (!(is.matrix(f) || is.data.frame(f))) {
        h2 <- f$communality
        com <- f$complexity
        ord <- rownames(f$loadings)
        nfact <- ncol(f$loadings)
        f <- f$loadings
    }
    else {
        h2 <- NULL
        com <- NULL
        ord <- rownames(f)
        nfact <- ncol(f)
    }
    means <- m[ord]
    f <- data.frame(unclass(f), means = means)
    contents <- lookup(rownames(f), y = dictionary)
    if (!is.null(h2)) {
        results <- data.frame(round(f, digits = digits), com = round(com, 
            digits = digits), h2 = round(h2, digits = digits))
    }
    else {
        results <- data.frame(round(f, digits = digits))
    }
    results <- merge(results, contents, by = "row.names", all.x = TRUE, 
        sort = FALSE)
    rownames(results) <- results[, "Row.names"]
    results <- results[ord, -1]
    res <- results[0, ]
    for (i in 1:nfact) {
        temp <- results[abs(results[, i]) > cut, ]
        ord <- order(temp[, "means"])
        res <- rbind(res, temp[ord, ])
    }
    return(res)
}


cor2dist <- function (x) 
{
    if (dim(x)[1] != dim(x)[2]) {
        x <- cor(x, use = "pairwise")
    }
    dist <- sqrt(2 * (1 - x))
    return(dist)
}


fisherz2r <- function (z) 
{
    (exp(2 * z) - 1)/(1 + exp(2 * z))
}


wkappa <- function (x, w = NULL) 
{
    p <- dim(x)[2]
    if (dim(x)[1] != p) 
        x <- table(x[, 1], x[, 2])
    x <- as.matrix(x)
    tot <- sum(x)
    x <- x/tot
    rs <- rowSums(x)
    cs <- colSums(x)
    prob <- rs %*% t(cs)
    po <- tr(x)
    pc <- tr(prob)
    kappa <- (po - pc)/(1 - pc)
    if (is.null(w)) {
        w <- matrix(0, ncol = p, nrow = p)
        for (i in 1:p) {
            for (j in 1:p) {
                w[i, j] <- 1 - (abs(i - j))^2/9
            }
        }
    }
    weighted.prob <- w * prob
    weighted.obser <- w * x
    wpo <- sum(weighted.obser)
    wpc <- sum(weighted.prob)
    wkappa <- (wpo - wpc)/(1 - wpc)
    return(list(kappa = kappa, weighted.kappa = wkappa))
}


glb <- function (r, key = NULL) 
{
    nvar <- dim(r)[2]
    if (dim(r)[1] != dim(r)[2]) {
        r <- cor(r, use = "pairwise")
    }
    else {
        r <- cov2cor(r)
    }
    if (is.null(colnames(r))) {
        rownames(r) <- colnames(r) <- paste("V", 1:nvar, sep = "")
    }
    m <- (1 - r)/2
    diag(m) <- 1
    m.names <- colnames(m)
    if (!is.null(key)) {
        m <- diag(key) %*% m %*% diag(key)
        colnames(m) <- m.names
        flip <- FALSE
    }
    else {
        key <- rep(1, nvar)
    }
    signkey <- strtrim(key, 1)
    signkey[signkey == "1"] <- ""
    m.names <- paste(m.names, signkey, sep = "")
    colnames(m) <- rownames(m) <- m.names
    worst <- ICLUST(r, 2, plot = FALSE)
    keys <- worst$p.sorted$cluster
    best <- ICLUST(m, 2, plot = FALSE, SMC = FALSE)
    keys <- matrix(rep(0, nvar * 2), ncol = 2)
    keys <- best$p.sorted$cluster
    m1 <- r
    diag(m1) <- 0
    best.kmeans <- kmeans(m, 2, nstart = 10)
    keys.kmean <- matrix(rep(0, nvar * 2), ncol = 2)
    for (i in 1:nvar) {
        keys.kmean[i, best.kmeans$cluster[i]] <- 1
    }
    f1 <- fa(r)
    load <- f1$loadings
    ord.load <- order(load)
    key.fa <- matrix(rep(0, nvar * 2), ncol = 2)
    for (i in 1:nvar) {
        key.fa[ord.load[i], 1] <- i%%2
        key.fa[ord.load[i], 2] <- 1 - key.fa[ord.load[i], 1]
    }
    f2 <- fa(r, 2, SMC = FALSE)
    load <- f2$loadings
    key.fa2 <- matrix(rep(0, nvar * 2), ncol = 2)
    key.fa2[, 1] <- (load[, 1] > load[, 2]) + 0
    key.fa2[, 2] <- 1 - key.fa2[, 1]
    e <- eigen(r)$values[1]
    alpha.pc <- 1 - 1/e
    keys <- cbind(worst$p.sorted$cluster, keys, keys.kmean, key.fa, 
        key.fa2)
    colnames(keys) <- c("IC1", "IC2", "ICr1", "ICr2", "K1", "K2", 
        "F1", "F2", "f1", "f2")
    covar <- t(keys) %*% r %*% keys
    var <- diag(covar)
    sd.inv <- 1/sqrt(var)
    ident.sd <- diag(sd.inv, ncol = length(sd.inv))
    cluster.correl <- ident.sd %*% covar %*% ident.sd
    beta <- cluster.correl[2, 1] * 2/(1 + cluster.correl[2, 1])
    glbIC <- cluster.correl[3, 4] * 2/(1 + cluster.correl[3, 
        4])
    glb2 <- cluster.correl[5, 6] * 2/(1 + cluster.correl[5, 6])
    glb3 <- cluster.correl[7, 8] * 2/(1 + cluster.correl[7, 8])
    beta.fa <- cluster.correl[9, 10] * 2/(1 + cluster.correl[9, 
        10])
    glb.max <- max(glbIC, glb2, glb3)
    sum.smc <- sum(smc(r))
    sum.r <- sum(r)
    gamma <- (sum.r + sum.smc - sum(diag(r)))/sum.r
    tenberg <- tenberge(r)
    result <- list(beta = beta, beta.factor = beta.fa, alpha.pc = alpha.pc, 
        glb.max = glb.max, glb.IC = glbIC, glb.Km = glb2, glb.Fa = glb3, 
        r.smc = gamma, tenberge = tenberg, keys = keys)
    return(result)
}


cluster.fit <- function (original, load, clusters, diagonal = FALSE) 
{
    Pattern <- TRUE
    df <- nrow(original) * (ncol(original) - 1)/2
    sqoriginal <- original * original
    totaloriginal <- sum(sqoriginal) - diagonal * sum(diag(sqoriginal))
    load <- as.matrix(load)
    clusters <- as.matrix(clusters)
    model <- load %*% t(load)
    residual <- original - model
    sqresid <- residual * residual
    totalresid <- sum(sqresid) - diagonal * sum(diag(sqresid))
    fit <- 1 - totalresid/totaloriginal
    covar <- t(clusters) %*% original %*% clusters
    phi <- cov2cor(covar)
    phi.inv <- try(solve(phi), TRUE)
    if (class(phi.inv) == as.character("try-error")) {
        Pattern <- FALSE
        message("Could not invert cluster intercorrelation matrix, pattern matrix not found")
    }
    if (Pattern) {
        pattern <- load %*% phi.inv
        model2 <- pattern %*% t(load)
        residual <- original - model2
        sqresid <- residual * residual
        totalresid <- sum(sqresid) - (1 - diagonal) * sum(diag(sqresid))
        patternrmse <- sqrt(totalresid/(2 * df))
        fit2 <- 1 - totalresid/totaloriginal
    }
    else {
        fit2 <- NULL
        patternrmse <- 0
    }
    clusters <- abs(clusters)
    model.1 <- (load * clusters) %*% phi %*% t(load * clusters)
    residual <- original - model.1
    sqresid <- residual * residual
    totalresid <- sum(sqresid) - diagonal * sum(diag(sqresid))
    fit.1 <- 1 - totalresid/totaloriginal
    clusterrmse <- sqrt(totalresid/(2 * df))
    cluster.fit <- list(clusterfit = fit.1, structurefit = fit, 
        patternfit = fit2, clusterrmse = clusterrmse, patternrmse = patternrmse)
}


scatter.hist <- function (x, y = NULL, smooth = TRUE, ab = FALSE, correl = TRUE, 
    density = TRUE, ellipse = TRUE, digits = 2, method, cex.cor = 1, 
    title = "Scatter plot + histograms", xlab = NULL, ylab = NULL, 
    ...) 
{
    old.par <- par(no.readonly = TRUE)
    if (missing(xlab)) {
        if (!is.null(colnames(x))) {
            xlab = colnames(x)[1]
            ylab = colnames(x)[2]
        }
        else {
            xlab = "V1"
            ylab = "V2"
        }
    }
    if (is.null(y)) {
        y <- x[, 2]
        x <- x[, 1]
    }
    else {
        if (!is.null(dim(x))) {
            x <- x[, 1, drop = TRUE]
            if (!is.null(colnames(y))) 
                ylab <- colnames(y)
            if (!is.null(dim(y))) {
                y <- y[, 1, drop = TRUE]
            }
        }
    }
    xhist <- hist(x, breaks = 11, plot = FALSE)
    yhist <- hist(y, breaks = 11, plot = FALSE)
    xrange <- range(x, na.rm = TRUE)
    yrange <- range(y, na.rm = TRUE)
    nf <- layout(matrix(c(2, 4, 1, 3), 2, 2, byrow = TRUE), c(3, 
        1), c(1, 3), TRUE)
    par(mar = c(5, 4, 1, 1))
    plot(x, y, xlim = xrange, ylim = yrange, xlab = xlab, ylab = ylab, 
        ...)
    if (ab) 
        abline(lm(y ~ x))
    if (smooth) {
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
            lines(stats::lowess(x[ok], y[ok]), col = "red")
    }
    if (ellipse) {
        ellipses(x, y, add = TRUE)
    }
    par(mar = c(0, 4, 2, 0))
    mp <- barplot(xhist$density, axes = FALSE, space = 0)
    tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd", adjust = 1.2), 
        silent = TRUE)
    if (class(tryd) != "try-error") {
        d$x <- (mp[length(mp)] - mp[1] + 1) * (d$x - min(xhist$breaks))/(max(xhist$breaks) - 
            min(xhist$breaks))
        if (density) 
            lines(d)
    }
    title(title)
    par(mar = c(5, 0, 0, 2))
    mp <- barplot(yhist$density, axes = FALSE, space = 0, horiz = TRUE)
    tryd <- try(d <- density(y, na.rm = TRUE, bw = "nrd", adjust = 1.2), 
        silent = TRUE)
    if (class(tryd) != "try-error") {
        temp <- d$y
        d$y <- (mp[length(mp)] - mp[1] + 1) * (d$x - min(yhist$breaks))/(max(yhist$breaks) - 
            min(yhist$breaks))
        d$x <- temp
        if (density) 
            lines(d)
    }
    par(mar = c(3, 1, 1, 1))
    if (correl) {
        plot(1, 1, type = "n", axes = FALSE)
        med.x <- median(x, na.rm = TRUE)
        med.y <- median(y, na.rm = TRUE)
        if (missing(method)) 
            method <- "pearson"
        r = (cor(x, y, use = "pairwise", method = method))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        if (missing(cex.cor)) {
            cex <- 0.75/strwidth(txt)
        }
        else {
            cex <- cex.cor
        }
        text(1, 1, txt, cex = cex)
    }
    par(old.par)
}


r2chi <- function (rho, n) 
{
    chi <- rho^2 * n
}


Yule2phi.matrix <- function (x, v) 
{
    .Deprecated("Yule2phi", msg = "This function has been replaced by Yule2phi")
}


read.clipboard <- function (header = TRUE, ...) 
{
    MAC <- Sys.info()[1] == "Darwin"
    if (!MAC) {
        if (header) 
            return(read.table(file("clipboard"), header = TRUE, 
                ...))
        else return(read.table(file("clipboard"), ...))
    }
    else {
        if (header) {
            return(read.table(pipe("pbpaste"), header = TRUE, 
                ...))
        }
        else {
            return(read.table(pipe("pbpaste"), ...))
        }
    }
}


fisherz <- function (rho) 
{
    0.5 * log((1 + rho)/(1 - rho))
}


progressBar <- function (value, max, label = NULL) 
{
    if (class(stdout())[1] == "terminal") {
        pc <- round(100 * value/max)
        if (ceiling(100 * value/max) == floor(100 * value/max)) {
            width <- 100
            char = "."
            nw <- nchar(char, "w")
            nb <- round(width * value/max)
            cat(paste(c("\r  ", label, " |", rep.int(char, nb), 
                rep.int(" ", nw * (width - nb)), sprintf("| %3d%%", 
                  pc)), collapse = ""))
        }
    }
    flush(stdout())
}


dia.ellipse <- function (x, y = NULL, labels = NULL, cex = 1, e.size = 0.05, 
    xlim = c(0, 1), ylim = c(0, 1), ...) 
{
    text(x = x, y = y, labels = labels, cex = cex, ...)
    len <- max(strwidth(labels), strwidth("abc"))/1.6
    xs <- dia.ellipse1(x, y, xlim = xlim, ylim = ylim, e.size = e.size * 
        len, ...)
    left <- c(x - xs, y)
    right <- c(x + xs, y)
    top <- c(x, y + xs)
    bottom <- c(x, y - xs)
    center <- c(x, y)
    dia.ellipse <- list(left = left, right = right, top = top, 
        bottom = bottom, center = center, radius = xs)
}


cor2latex <- function (x, use = "pairwise", method = "pearson", adjust = "holm", 
    stars = FALSE, digits = 2, rowlabels = TRUE, lower = TRUE, 
    apa = TRUE, short.names = TRUE, font.size = "scriptsize", 
    heading = "A correlation table from the psych package in R.", 
    caption = "cor2latex", label = "default", silent = FALSE, 
    file = NULL, append = FALSE, cut = 0, big = 0) 
{
    if (stars) 
        heading <- paste(heading, "Adjust for multiple tests = ", 
            adjust)
    if (!is.na(class(x)[2]) & class(x)[2] == "corr.test") {
        r <- x$r
        p <- x$p
    }
    else {
        if (nrow(x) > ncol(x)) {
            x <- corr.test(x, use = use, method = method, adjust = adjust)
            r <- x$r
            p <- x$p
        }
        else {
            r <- x
            p <- NULL
        }
    }
    r <- round(r, digits)
    r <- format(r, nsmall = digits, drop0trailing = FALSE)
    if (lower) {
        r[upper.tri(r)] <- "~"
    }
    else {
        r[lower.tri(r)] <- "~"
    }
    if (stars && is.null(p)) 
        stop("To print significance levels, x must be be either a data frame of observations or a correlation matrix created with the corr.test function of the package psych. If you are not interested in displaying signicance level set stars = FALSE")
    mystars <- ifelse(p < 0.001, "{***}", ifelse(p < 0.01, "{**}", 
        ifelse(p < 0.05, "{*}", "")))
    mystars <- t(mystars)
    if (stars) {
        R <- matrix(paste(r, mystars, sep = ""), ncol = ncol(r))
    }
    else {
        R <- r
    }
    diag(R) <- paste(diag(r), " ", sep = "")
    rownames(R) <- colnames(r)
    colnames(R) <- colnames(r)
    if (lower) {
        R[upper.tri(R, diag = FALSE)] <- ""
    }
    else {
        R[lower.tri(R, diag = FALSE)] <- ""
    }
    if (stars) {
        char <- TRUE
    }
    else {
        char <- FALSE
    }
    return(df2latex(R, digits = digits, rowlabels = rowlabels, 
        apa = apa, short.names = short.names, font.size = font.size, 
        heading = heading, caption = caption, label = label, 
        char = TRUE, stars = stars, silent = silent, file = file, 
        append = append, cut = cut, big = big))
}


mediate.diagram <- function (medi, digits = 2, ylim = c(3, 7), xlim = c(-1, 10), 
    show.c = TRUE, main = "Mediation model", ...) 
{
    dv <- medi$var.names[["DV"]]
    iv <- as.matrix(rownames(medi$direct))
    mv <- medi$var.names[["med"]]
    mod <- medi$var.names[["mod"]]
    numx <- length(medi$var.names[["IV"]])
    numy <- length(dv)
    direct <- round(medi$direct, digits)
    C <- round(medi$C[c(iv, mv, dv), c(iv, mv, dv)], digits)
    miny <- 5 - max(length(iv)/2, length(mv), 2) - 0.5
    maxy <- 5 + max(length(iv)/2, length(mv), 2) + 0.5
    if (missing(ylim)) 
        ylim = c(miny, maxy)
    plot(NA, xlim = xlim, ylim = ylim, main = main, axes = FALSE, 
        xlab = "", ylab = "")
    var.names <- c(rownames(medi$direct), colnames(medi$direct), 
        rownames(medi$b))
    if (is.null(mv)) {
        n.mediate <- 0
    }
    else {
        n.mediate <- length(mv)
    }
    m <- list()
    c <- as.matrix(round(medi$c, digits))
    a <- as.matrix(round(medi$a, digits))
    if (ncol(a) == 1) 
        a <- t(a)
    b <- as.matrix(round(medi$b, digits))
    cprime <- as.matrix(round(medi$cprime, digits))
    x <- list()
    if ((numx > 1) && (n.mediate > 1)) {
        adj <- 3
    }
    else {
        adj <- 2
    }
    viv <- 1:numx
    for (i in 1:numx) {
        if ((numx%%2) == 0) {
            viv[i] <- switch(i, 7, 3, 6, 4, 8, 2, 9, 1, 10)
        }
        else {
            viv[i] <- switch(i, 5, 7, 3, 6, 4, 8, 2, 9)
        }
        x[[i]] <- dia.rect(1, viv[i], iv[i])
    }
    vdv <- 1:numy
    y <- list()
    for (i in 1:numy) {
        if ((numy%%2) == 0) {
            vdv[i] <- switch(i, 6, 4, 7, 3, 8, 2, 9, 1, 10)
        }
        else {
            vdv[i] <- switch(i, 5, 7, 3, 6, 4, 8, 2, 9)
        }
        y[[i]] <- dia.rect(9, vdv[i], dv[i])
    }
    v.loc <- 1:n.mediate
    if (n.mediate > 0) {
        for (mediate in 1:n.mediate) {
            if ((n.mediate%%2) == 0) {
                v.loc[mediate] <- switch(mediate, 7, 3, 9, 1, 
                  6, 4, 7, 3, 10)
            }
            else {
                switch(numx, 1:{
                  v.loc[mediate] <- switch(mediate, 7, 3, 8, 
                    1, 6, 4, 9, 2)
                }, 2:{
                  v.loc[mediate] <- switch(mediate, 5, 3, 7, 
                    2, 6, 4, 8, 2)
                }, 3:{
                  v.loc[mediate] <- switch(mediate, 5.5, 3, 7, 
                    2, 5, 4, 8, 2)
                }, 4:{
                  v.loc[mediate] <- switch(mediate, 5, 3, 7, 
                    2, 6, 4, 8, 2)
                }, 5:{
                  v.loc[mediate] <- switch(mediate, 6, 3, 7, 
                    2, 5, 4, 8, 2)
                }, 6:{
                  v.loc[mediate] <- switch(mediate, 5, 3, 7, 
                    2, 6, 4, 8, 2)
                }, 7:{
                  v.loc[mediate] <- switch(mediate, 6, 3, 7, 
                    2, 5, 4, 8, 2)
                })
            }
        }
    }
    v.loc <- sort(v.loc, decreasing = TRUE)
    if (n.mediate == 0) {
        for (j in 1:numy) {
            for (i in 1:numx) {
                dia.arrow(x[[i]]$right, y[[j]]$left, labels = paste("c = ", 
                  direct[i, j]), pos = 0, ...)
            }
        }
    }
    else {
        if (n.mediate == 1) 
            a <- t(a)
        for (mediate in 1:n.mediate) {
            m[[mediate]] <- dia.rect(5, v.loc[mediate], mv[mediate])
            for (j in 1:numy) {
                for (i in 1:numx) {
                  dia.arrow(x[[i]]$right, m[[mediate]]$left, 
                    labels = a[i, mediate], adj = adj, ...)
                  if (show.c) {
                    dia.arrow(x[[i]]$right, y[[j]]$left, labels = paste("c = ", 
                      c[i, j]), pos = 3, ...)
                  }
                  dia.arrow(x[[i]]$right, y[[j]]$left, labels = paste("c' = ", 
                    cprime[i, j]), pos = 1, ...)
                }
                dia.arrow(m[[mediate]]$right, y[[j]]$left, labels = b[mediate, 
                  j], ...)
            }
        }
    }
    rviv <- max(viv)
    if (numx > 1) {
        for (i in 2:numx) {
            for (k in 1:(i - 1)) {
                dia.curved.arrow(x[[i]]$left, x[[k]]$left, C[i, 
                  k], scale = -(numx - 1) * (abs(viv[i] - viv[k])/rviv), 
                  both = TRUE)
            }
        }
    }
}


factor.wls <- function (r, nfactors = 1, residuals = FALSE, rotate = "varimax", 
    n.obs = NA, scores = FALSE, SMC = TRUE, missing = FALSE, 
    impute = "median", min.err = 0.001, digits = 2, max.iter = 50, 
    symmetric = TRUE, warnings = TRUE, fm = "wls") 
{
    cl <- match.call()
    .Deprecated("fa", msg = "factor.wls is deprecated.  Please use the fa function with fm=wls.")
    "fit.residuals" <- function(Psi, S, nf, S.inv, fm) {
        diag(S) <- 1 - Psi
        if (fm == "wls") {
            if (!is.null(S.inv)) 
                sd.inv <- diag(1/diag(S.inv))
        }
        else {
            if (!is.null(S.inv)) 
                sd.inv <- ((S.inv))
        }
        eigens <- eigen(S)
        eigens$values[eigens$values < .Machine$double.eps] <- 100 * 
            .Machine$double.eps
        if (nf > 1) {
            loadings <- eigens$vectors[, 1:nf] %*% diag(sqrt(eigens$values[1:nf]))
        }
        else {
            loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
        }
        model <- loadings %*% t(loadings)
        if (fm == "wls") {
            residual <- sd.inv %*% (S - model)^2 %*% sd.inv
        }
        else {
            if (fm == "gls") {
                residual <- (sd.inv %*% (S - model))^2
            }
            else {
                residual <- (S - model)^2
            }
        }
        diag(residual) <- 0
        error <- sum(residual)
    }
    "fit" <- function(S, nf, fm) {
        S.smc <- smc(S)
        if ((fm == "wls") | (fm == "gls")) {
            S.inv <- solve(S)
        }
        else {
            S.inv <- NULL
        }
        if (sum(S.smc) == nf) {
            start <- rep(0.5, nf)
        }
        else {
            start <- 1 - S.smc
        }
        res <- optim(start, fit.residuals, method = "L-BFGS-B", 
            lower = 0.005, upper = 1, control = c(list(fnscale = 1, 
                parscale = rep(0.01, length(start)))), nf = nf, 
            S = S, S.inv = S.inv, fm = fm)
        if ((fm == "wls") | (fm == "gls")) {
            Lambda <- FAout.wls(res$par, S, nf)
        }
        else {
            Lambda <- FAout(res$par, S, nf)
        }
        result <- list(loadings = Lambda, res = res)
    }
    FAout <- function(Psi, S, q) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), 
            q)
        diag(sqrt(Psi)) %*% load
    }
    FAout.wls <- function(Psi, S, q) {
        diag(S) <- 1 - Psi
        E <- eigen(S, symmetric = TRUE)
        L <- E$vectors[, 1:q, drop = FALSE] %*% diag(sqrt(E$values[1:q, 
            drop = FALSE]), q)
        return(L)
    }
    if ((fm != "pa") & (fm != "minres") & (fm != "gls") & (fm != 
        "wls")) {
        message("factor method not specified correctly, weighted least squares  used")
        fm <- "wls"
    }
    n <- dim(r)[2]
    if (n != dim(r)[1]) {
        n.obs <- dim(r)[1]
        if (scores) {
            x.matrix <- r
            if (missing) {
                miss <- which(is.na(x.matrix), arr.ind = TRUE)
                if (impute == "mean") {
                  item.means <- colMeans(x.matrix, na.rm = TRUE)
                  x.matrix[miss] <- item.means[miss[, 2]]
                }
                else {
                  item.med <- apply(x.matrix, 2, median, na.rm = TRUE)
                  x.matrix[miss] <- item.med[miss[, 2]]
                }
            }
        }
        r <- cor(r, use = "pairwise")
    }
    else {
        if (!is.matrix(r)) {
            r <- as.matrix(r)
        }
        sds <- sqrt(diag(r))
        r <- r/(sds %o% sds)
    }
    if (!residuals) {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), fit = 0)
    }
    else {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), residual = matrix(rep(0, n * 
                n), ncol = n), fit = 0)
    }
    r.mat <- r
    Phi <- NULL
    colnames(r.mat) <- rownames(r.mat) <- colnames(r)
    if (SMC) {
        if (nfactors < n/2) {
            diag(r.mat) <- smc(r)
        }
        else {
            if (warnings) 
                message("too many factors requested for this number of variables to use SMC, 1s used instead")
        }
    }
    orig <- diag(r)
    comm <- sum(diag(r.mat))
    err <- comm
    i <- 1
    comm.list <- list()
    if (fm == "pa") {
        while (err > min.err) {
            eigens <- eigen(r.mat, symmetric = symmetric)
            if (nfactors > 1) {
                loadings <- eigens$vectors[, 1:nfactors] %*% 
                  diag(sqrt(eigens$values[1:nfactors]))
            }
            else {
                loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
            }
            model <- loadings %*% t(loadings)
            new <- diag(model)
            comm1 <- sum(new)
            diag(r.mat) <- new
            err <- abs(comm - comm1)
            if (is.na(err)) {
                warning("imaginary eigen value condition encountered in fa,\n Try again with SMC=FALSE \n exiting fa")
                break
            }
            comm <- comm1
            comm.list[[i]] <- comm1
            i <- i + 1
            if (i > max.iter) {
                if (warnings) {
                  message("maximum iteration exceeded")
                }
                err <- 0
            }
        }
    }
    if ((fm == "wls") | (fm == "gls")) {
        uls <- fit(r, nfactors, fm)
        eigens <- eigen(r)
        result$par <- uls$res
        loadings <- uls$loadings
    }
    if (!is.double(loadings)) {
        warning("the matrix has produced imaginary results -- proceed with caution")
        loadings <- matrix(as.double(loadings), ncol = nfactors)
    }
    if (FALSE) {
        if (nfactors > 1) {
            maxabs <- apply(apply(loadings, 2, abs), 2, which.max)
            sign.max <- vector(mode = "numeric", length = nfactors)
            for (i in 1:nfactors) {
                sign.max[i] <- sign(loadings[maxabs[i], i])
            }
            loadings <- loadings %*% diag(sign.max)
        }
        else {
            mini <- min(loadings)
            maxi <- max(loadings)
            if (abs(mini) > maxi) {
                loadings <- -loadings
            }
            loadings <- as.matrix(loadings)
            if (fm == "minres") {
                colnames(loadings) <- "MR1"
            }
            else {
                colnames(loadings) <- "PA1"
            }
        }
    }
    if (nfactors > 1) {
        sign.tot <- vector(mode = "numeric", length = nfactors)
        sign.tot <- sign(colSums(loadings))
        loadings <- loadings %*% diag(sign.tot)
    }
    else {
        if (sum(loadings) < 0) {
            loadings <- -as.matrix(loadings)
        }
        else {
            loadings <- as.matrix(loadings)
        }
        colnames(loadings) <- "MR1"
    }
    if (fm == "wls") {
        colnames(loadings) <- paste("WLS", 1:nfactors, sep = "")
    }
    else {
        if (fm == "gls") {
            colnames(loadings) <- paste("GLS", 1:nfactors, sep = "")
        }
        else {
            colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
        }
    }
    rownames(loadings) <- rownames(r)
    loadings[loadings == 0] <- 10^-15
    model <- loadings %*% t(loadings)
    f.loadings <- loadings
    if (rotate != "none") {
        if (nfactors > 1) {
            if (rotate == "varimax" | rotate == "quartimax") {
                rotated <- do.call(rotate, list(loadings))
                loadings <- rotated$loadings
                Phi <- NULL
            }
            else {
                if ((rotate == "promax") | (rotate == "Promax")) {
                  pro <- Promax(loadings)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                }
                else {
                  if (rotate == "cluster") {
                    loadings <- varimax(loadings)$loadings
                    pro <- target.rot(loadings)
                    loadings <- pro$loadings
                    Phi <- pro$Phi
                  }
                  else {
                    if (rotate == "oblimin" | rotate == "quartimin" | 
                      rotate == "simplimax") {
                      if (!requireNamespace("GPArotation")) {
                        warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
                        Phi <- NULL
                      }
                      else {
                        ob <- do.call(rotate, list(loadings))
                        loadings <- ob$loadings
                        Phi <- ob$Phi
                      }
                    }
                  }
                }
            }
        }
    }
    if (nfactors > 1) {
        ev.rotated <- diag(t(loadings) %*% loadings)
        ev.order <- order(ev.rotated, decreasing = TRUE)
        loadings <- loadings[, ev.order]
    }
    rownames(loadings) <- colnames(r)
    if (!is.null(Phi)) {
        Phi <- Phi[ev.order, ev.order]
    }
    class(loadings) <- "loadings"
    if (nfactors < 1) 
        nfactors <- n
    result <- factor.stats(r, loadings, Phi, n.obs)
    result$communality <- round(diag(model), digits)
    result$uniquenesses <- round(diag(r - model), digits)
    result$values <- round(eigens$values, digits)
    result$loadings <- loadings
    if (!is.null(Phi)) {
        result$Phi <- Phi
    }
    if (fm == "pa") 
        result$communality.iterations <- round(unlist(comm.list), 
            digits)
    if (scores) {
        result$scores <- factor.scores(x.matrix, loadings)
    }
    result$factors <- nfactors
    result$fn <- "factor.wls"
    result$Call <- cl
    class(result) <- c("psych", "fa")
    return(result)
}


cor.plot <- function (r, numbers = FALSE, colors = TRUE, n = 51, main = NULL, 
    zlim = c(-1, 1), show.legend = TRUE, labels = NULL, n.legend = 10, 
    keep.par = TRUE, select = NULL, pval = NULL, cuts = c(0.001, 
        0.01), cex, MAR, upper = TRUE, diag = TRUE, ...) 
{
    corPlot(r = r, numbers = numbers, colors = colors, n = n, 
        main = main, zlim = zlim, show.legend = show.legend, 
        labels = labels, n.legend = n.legend, keep.par = keep.par, 
        select = select, pval = pval, cuts = cuts, cex = cex, 
        MAR = MAR, upper = upper, diag = diag, ...)
}


smc <- function (R, covar = FALSE) 
{
    failed = FALSE
    wcc <- maxr <- NULL
    p <- dim(R)[2]
    if (is.null(colnames(R))) 
        colnames(R) <- paste0("V", 1:p)
    smc.all <- rep(NA, p)
    names(smc.all) <- colnames(R)
    if (dim(R)[1] != p) {
        if (covar) {
            C <- cov(R, use = "pairwise")
            vari <- diag(C)
            R <- cov2cor(C)
        }
        else {
            R <- cor(R, use = "pairwise")
        }
    }
    else {
        vari <- diag(R)
        if (!is.matrix(R)) 
            R <- as.matrix(R)
        R <- cov2cor(R)
    }
    tempR <- NULL
    if (any(is.na(R))) {
        bad <- TRUE
        tempR <- R
        vr <- diag(tempR)
        diag(tempR) <- 0
        maxr <- apply(tempR, 2, function(x) max(abs(x), na.rm = TRUE))
        diag(tempR) <- vr
        wcl <- NULL
        while (bad) {
            wc <- table(which(is.na(tempR), arr.ind = TRUE))
            wcl <- c(wcl, as.numeric(names(which(wc == max(wc)))))
            tempR <- R[-wcl, -wcl]
            if (any(is.na(tempR))) {
                bad <- TRUE
            }
            else {
                bad <- FALSE
            }
        }
        warning("Missing values (NAs) in the correlation matrix do not allow for SMC's to be found for all variables.  \nI will try to estimate SMCs for those variables by their non-NA  correlations.")
        cat("\nSMC(s) for variables ", colnames(R)[wcl], "were replaced (if possible) with smcs based upon their  (its) non-NA correlations\n")
        wc <- (which(is.na(R[, wcl]), arr.ind = TRUE))
        if (is.null(dim(wc))) {
            wcc <- as.numeric(names(table(wc)))
        }
        else {
            wcc <- as.numeric(names(table(wc[, 1])))
        }
        tempR <- R[-wcc, -wcc]
        R <- R[-wcl, -wcl]
    }
    if (!covar) {
        R <- cor.smooth(R)
    }
    R.inv <- try(solve(R), TRUE)
    if (class(R.inv) == as.character("try-error")) {
        smc <- rep(1, p)
        message("In smc, the correlation matrix was not invertible, smc's returned as 1s")
    }
    else {
        smc <- 1 - 1/diag(R.inv)
    }
    names(smc) <- colnames(R)
    if (!is.null(tempR)) {
        R.na.inv <- try(solve(tempR), TRUE)
        if (class(R.na.inv) == as.character("try-error")) {
            smc.na <- rep(1, p)
            message("Oh bother, in smc, the correlation matrix of the adjusted part was not invertible, smc's returned as 1s")
        }
        else {
            smc.na <- 1 - 1/diag(R.na.inv)
        }
    }
    else {
        smc.na <- smc
    }
    if (all(is.na(smc))) {
        message("Something is seriously wrong the correlation matrix.\nIn smc, smcs were set to 1.0")
        smc[is.na(smc)] <- 1
    }
    if (max(smc, na.rm = TRUE) > 1) {
        message("In smc, smcs > 1 were set to 1.0")
        smc[smc > 1] <- 1
    }
    if (min(smc, na.rm = TRUE) < 0) {
        message("In smc, smcs < 0 were set to .0")
        smc[smc < 0] <- 0
    }
    smc.all[names(smc.all) %in% names(smc)] <- smc
    if (!is.null(wcc)) {
        smc.all[wcl] <- smc.na[names(smc.all[wcl])]
    }
    smc <- smc.all
    if (!is.null(maxr)) {
        if (any(is.na(smc))) {
            warning("The SMCs with NA values were replaced by their maximum correlation.")
            cat("\nSMC(s) for variables ", names(smc)[is.na(smc)], 
                "were replaced with their maximum correlation \n")
        }
        smc[is.na(smc)] <- maxr[is.na(smc)]
    }
    if (covar) {
        smc <- smc * vari
    }
    return(smc)
}


factor.pa <- function (r, nfactors = 1, residuals = FALSE, rotate = "varimax", 
    n.obs = NA, scores = FALSE, SMC = TRUE, missing = FALSE, 
    impute = "median", min.err = 0.001, digits = 2, max.iter = 50, 
    symmetric = TRUE, warnings = TRUE, fm = "pa") 
{
    cl <- match.call()
    .Deprecated("fa", msg = "factor.pa is deprecated.  Please use the fa function with fm=pa")
    "fit.residuals.ols" <- function(Psi, S, nf) {
        diag(S) <- 1 - Psi
        eigens <- eigen(S)
        eigens$values[eigens$values < .Machine$double.eps] <- 100 * 
            .Machine$double.eps
        if (nf > 1) {
            loadings <- eigens$vectors[, 1:nf] %*% diag(sqrt(eigens$values[1:nf]))
        }
        else {
            loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
        }
        model <- loadings %*% t(loadings)
        residual <- (S - model)^2
        diag(residual) <- 0
        error <- sum(residual)
    }
    "min.res" <- function(S, nf) {
        S.smc <- smc(S)
        if (sum(S.smc) == nf) {
            start <- rep(0.5, nf)
        }
        else {
            start <- 1 - S.smc
        }
        res <- optim(start, fit.residuals.ols, method = "L-BFGS-B", 
            lower = 0.005, upper = 1, control = c(list(fnscale = 1, 
                parscale = rep(0.01, length(start)))), nf = nf, 
            S = S)
        Lambda <- FAout(res$par, S, nf)
        result <- list(loadings = Lambda, res = res)
    }
    FAout <- function(Psi, S, q) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE)
        L <- E$vectors[, 1L:q, drop = FALSE]
        load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), 
            q)
        diag(sqrt(Psi)) %*% load
    }
    FAfn <- function(Psi, S, q) {
        sc <- diag(1/sqrt(Psi))
        Sstar <- sc %*% S %*% sc
        E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
        e <- E$values[-(1L:q)]
        e <- sum(log(e) - e) - q + nrow(S)
        -e
    }
    if ((fm != "pa") & (fm != "minres")) {
        message("factor method not specified correctly, principal axes used")
        fm <- "pa"
    }
    n <- dim(r)[2]
    if (n != dim(r)[1]) {
        n.obs <- dim(r)[1]
        if (scores) {
            x.matrix <- r
            if (missing) {
                miss <- which(is.na(x.matrix), arr.ind = TRUE)
                if (impute == "mean") {
                  item.means <- colMeans(x.matrix, na.rm = TRUE)
                  x.matrix[miss] <- item.means[miss[, 2]]
                }
                else {
                  item.med <- apply(x.matrix, 2, median, na.rm = TRUE)
                  x.matrix[miss] <- item.med[miss[, 2]]
                }
            }
        }
        r <- cor(r, use = "pairwise")
    }
    else {
        if (!is.matrix(r)) {
            r <- as.matrix(r)
        }
        sds <- sqrt(diag(r))
        r <- r/(sds %o% sds)
    }
    if (!residuals) {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), fit = 0)
    }
    else {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), residual = matrix(rep(0, n * 
                n), ncol = n), fit = 0)
    }
    r.mat <- r
    Phi <- NULL
    colnames(r.mat) <- rownames(r.mat) <- colnames(r)
    if (SMC) {
        if (nfactors < n/2) {
            diag(r.mat) <- smc(r)
        }
        else {
            if (warnings) 
                message("too many factors requested for this number of variables to use SMC, 1s used instead")
        }
    }
    orig <- diag(r)
    comm <- sum(diag(r.mat))
    err <- comm
    i <- 1
    comm.list <- list()
    if (fm == "pa") {
        while (err > min.err) {
            eigens <- eigen(r.mat, symmetric = symmetric)
            eigens$values[eigens$values < .Machine$double.eps] <- .Machine$double.eps
            if (nfactors > 1) {
                loadings <- eigens$vectors[, 1:nfactors] %*% 
                  diag(sqrt(eigens$values[1:nfactors]))
            }
            else {
                loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
            }
            model <- loadings %*% t(loadings)
            new <- diag(model)
            comm1 <- sum(new)
            diag(r.mat) <- new
            err <- abs(comm - comm1)
            if (is.na(err)) {
                warning("imaginary eigen value condition encountered in factor.pa,\n Try again with SMC=FALSE \n exiting factor.pa")
                break
            }
            comm <- comm1
            comm.list[[i]] <- comm1
            i <- i + 1
            if (i > max.iter) {
                if (warnings) {
                  message("maximum iteration exceeded")
                }
                err <- 0
            }
        }
    }
    if (fm == "minres") {
        uls <- min.res(r, nfactors)
        eigens <- eigen(r)
        result$par <- uls$res
        loadings <- uls$loadings
    }
    if (!is.double(loadings)) {
        warning("the matrix has produced imaginary results -- proceed with caution")
        loadings <- matrix(as.double(loadings), ncol = nfactors)
    }
    if (FALSE) {
        if (nfactors > 1) {
            maxabs <- apply(apply(loadings, 2, abs), 2, which.max)
            sign.max <- vector(mode = "numeric", length = nfactors)
            for (i in 1:nfactors) {
                sign.max[i] <- sign(loadings[maxabs[i], i])
            }
            loadings <- loadings %*% diag(sign.max)
        }
        else {
            mini <- min(loadings)
            maxi <- max(loadings)
            if (abs(mini) > maxi) {
                loadings <- -loadings
            }
            loadings <- as.matrix(loadings)
            if (fm == "minres") {
                colnames(loadings) <- "mr1"
            }
            else {
                colnames(loadings) <- "PA1"
            }
        }
    }
    if (nfactors > 1) {
        sign.tot <- vector(mode = "numeric", length = nfactors)
        sign.tot <- sign(colSums(loadings))
        loadings <- loadings %*% diag(sign.tot)
    }
    else {
        if (sum(loadings) < 0) {
            loadings <- -as.matrix(loadings)
        }
        else {
            loadings <- as.matrix(loadings)
        }
        colnames(loadings) <- "PA1"
    }
    if (fm == "minres") {
        colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
    }
    else {
        colnames(loadings) <- paste("PA", 1:nfactors, sep = "")
    }
    rownames(loadings) <- rownames(r)
    loadings[loadings == 0] <- 10^-15
    model <- loadings %*% t(loadings)
    Phi <- NULL
    if (rotate != "none") {
        if (nfactors > 1) {
            if (rotate == "varimax" | rotate == "quartimax") {
                rotated <- do.call(rotate, list(loadings))
                loadings <- rotated$loadings
                Phi <- NULL
            }
            else {
                if ((rotate == "promax") | (rotate == "Promax")) {
                  pro <- Promax(loadings)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                }
                else {
                  if (rotate == "cluster") {
                    loadings <- varimax(loadings)$loadings
                    pro <- target.rot(loadings)
                    loadings <- pro$loadings
                    Phi <- pro$Phi
                  }
                  else {
                    if (rotate == "oblimin" | rotate == "quartimin" | 
                      rotate == "simplimax") {
                      if (!requireNamespace("GPArotation")) {
                        warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
                        Phi <- NULL
                      }
                      else {
                        ob <- do.call(rotate, list(loadings))
                        loadings <- ob$loadings
                        Phi <- ob$Phi
                      }
                    }
                  }
                }
            }
        }
    }
    if (nfactors > 1) {
        ev.rotated <- diag(t(loadings) %*% loadings)
        ev.order <- order(ev.rotated, decreasing = TRUE)
        loadings <- loadings[, ev.order]
    }
    rownames(loadings) <- colnames(r)
    if (!is.null(Phi)) {
        Phi <- Phi[ev.order, ev.order]
    }
    class(loadings) <- "loadings"
    if (nfactors < 1) 
        nfactors <- n
    result <- factor.stats(r, loadings, Phi, n.obs)
    result$rotate <- rotate
    result$loadings <- loadings
    result$values <- eigens$values
    result$communality <- round(diag(model), digits)
    result$uniquenesses <- round(diag(r - model), digits)
    if (!is.null(Phi)) {
        result$Phi <- Phi
    }
    if (fm == "pa") 
        result$communality.iterations <- round(unlist(comm.list), 
            digits)
    if (scores) {
        result$scores <- factor.scores(x.matrix, loadings)
    }
    result$factors <- nfactors
    result$fn <- "factor.pa"
    result$fm <- fm
    result$Call <- cl
    class(result) <- c("psych", "fa")
    return(result)
}


describe.by <- function (x, group = NULL, mat = FALSE, type = 3, ...) 
{
    .Deprecated("describeBy", msg = "describe.by is deprecated.  Please use the describeBy function")
    answer <- describeBy(x = x, group = group, mat = mat, type = type, 
        ...)
    return(answer)
}


skew <- function (x, na.rm = TRUE, type = 3) 
{
    if (length(dim(x)) == 0) {
        if (na.rm) {
            x <- x[!is.na(x)]
        }
        sdx <- sd(x, na.rm = na.rm)
        mx <- mean(x)
        n <- length(x[!is.na(x)])
        switch(type, {
            skewer <- sqrt(n) * (sum((x - mx)^3, na.rm = na.rm)/(sum((x - 
                mx)^2, na.rm = na.rm)^(3/2)))
        }, {
            skewer <- n * sqrt(n - 1) * (sum((x - mx)^3, na.rm = na.rm)/((n - 
                2) * sum((x - mx)^2, na.rm = na.rm)^(3/2)))
        }, {
            skewer <- sum((x - mx)^3)/(n * sd(x)^3)
        })
    }
    else {
        skewer <- rep(NA, dim(x)[2])
        if (is.matrix(x)) {
            mx <- colMeans(x, na.rm = na.rm)
        }
        else {
            mx <- apply(x, 2, mean, na.rm = na.rm)
        }
        sdx <- apply(x, 2, sd, na.rm = na.rm)
        for (i in 1:dim(x)[2]) {
            n <- length(x[!is.na(x[, i]), i])
            switch(type, {
                skewer[i] <- sqrt(n) * (sum((x[, i] - mx[i])^3, 
                  na.rm = na.rm)/(sum((x[, i] - mx[i])^2, na.rm = na.rm)^(3/2)))
            }, {
                skewer[i] <- n * sqrt(n - 1) * (sum((x[, i] - 
                  mx[i])^3, na.rm = na.rm)/((n - 2) * sum((x[, 
                  i] - mx[i])^2, na.rm = na.rm)^(3/2)))
            }, {
                skewer[i] <- sum((x[, i] - mx[i])^3, na.rm = na.rm)/(n * 
                  sdx[i]^3)
            })
        }
    }
    return(skewer)
}


dummy.code <- function (x) 
{
    t <- table(x)
    lt <- length(t)
    n.obs <- length(x)
    new <- matrix(0, nrow = n.obs, ncol = lt)
    xlev <- as.factor(x)
    for (i in 1:n.obs) {
        new[i, xlev[i]] <- 1
    }
    colnames(new) <- names(t)
    return(new)
}


cor.smoother <- function (x, cut = 0.01) 
{
    nvar <- ncol(x)
    result <- list()
    if (nrow(x) != nvar) 
        x <- cor(x, use = "pairwise")
    bad <- rep(NA, nvar)
    good <- rep(TRUE, nvar)
    names(good) <- names(bad) <- colnames(x)
    for (i in 1:nvar) {
        ev <- eigen(x[-i, -i])$values
        if (any(ev < 0)) {
            bad[i] <- TRUE
            good[i] <- FALSE
        }
        bad[i] <- sum((ev < 0), na.rm = TRUE)
    }
    if (sum(bad + 0) > 0) {
        result$good <- bad[(bad > 0)]
        result$bad <- good[good]
        s <- cor.smooth(x)
        possible <- arrayInd(which.max(abs(s - x)), dim(x), .dimnames = colnames(x))
        result$likely <- colnames(x)[possible]
        result$possible <- arrayInd(which(abs(s - x) > cut), 
            dim(x), .dimnames = colnames(x))
        result$possible <- sort(table(colnames(x)[result$possible]), 
            decreasing = TRUE)
    }
    else {
        result$bad <- c("all ok")
    }
    class(result) <- c("psych", "smoother")
    return(result)
}


irt.discrim <- function (item.diff, theta, items) 
{
    irt.item.discrim <- function(x, diff, theta, scores) {
        fit <- -1 * (log(scores/(1 + exp(x * (diff - theta))) + 
            (1 - scores)/(1 + exp(x * (theta - diff)))))
        mean(fit, na.rm = TRUE)
    }
    nitems <- length(item.diff)
    discrim <- matrix(NaN, nitems, 2)
    for (i in 1:nitems) {
        item.fit <- optimize(irt.item.discrim, c(-5, 5), diff = item.diff[i], 
            theta = theta, scores = items[, i])
        discrim[i, 1] <- item.fit$minimum
        discrim[i, 2] <- item.fit$objective
    }
    irt.discrim <- discrim
}


interp.quantiles <- function (x, q = 0.5, w = 1, na.rm = TRUE) 
{
    if (!(q > 0) | !(q < 1)) {
        stop("quantiles most be greater than 0 and less than 1 q = ", 
            q)
    }
    if (is.vector(x)) {
        im <- interp.q(x, q, w, na.rm = na.rm)
    }
    else {
        if ((is.matrix(x) | is.data.frame(x))) {
            n <- dim(x)[2]
            im <- matrix(NA, ncol = n)
            for (i in 1:n) {
                im[i] <- interp.q(x[, i], q, w = w, na.rm = na.rm)
            }
            colnames(im) <- colnames(x)
        }
        else {
            stop("The data must be either a vector, a matrix, or a data.frame")
        }
        return(im)
    }
}


dia.triangle <- function (x, y = NULL, labels = NULL, cex = 1, xlim = c(0, 1), 
    ylim = c(0, 1), ...) 
{
    text(x = x, y = y, labels = labels, cex = cex, ...)
    STRETCH = 0.25
    xrange = (xlim[2] - xlim[1])
    yrange = (ylim[2] - ylim[1])
    xs <- 0.1 * xrange
    ys <- 0.1 * xrange
    len <- max(strwidth(labels) * 0.7, strwidth("abc"))
    vert <- 0.7 * len
    left <- c(x - len/2, y + vert/2 - STRETCH/2)
    right <- c(x + len/2, y + vert/2 - STRETCH/2)
    top <- c(x, y + vert)
    bottom <- c(x, y - vert * STRETCH)
    xs <- c(x - len, x + len, x)
    ys <- c(y - vert * STRETCH, y - vert * STRETCH, y + vert)
    polygon(xs, ys)
    radius <- sqrt(len^2 + vert^2)
    dia.rect <- list(left = left, right = right, top = top, bottom = bottom, 
        center = c(x, y), radius = radius)
}


fa.lookup <- function (f, dictionary, digits = 2) 
{
    f <- fa.sort(f)
    if (!(is.matrix(f) || is.data.frame(f))) {
        h2 <- f$communality
        com <- f$complexity
        ord <- rownames(f$loadings)
        f <- f$loadings
    }
    else {
        h2 <- NULL
        com <- NULL
        ord <- rownames(f)
    }
    contents <- lookup(rownames(f), dictionary)
    if (!is.null(h2)) {
        results <- data.frame(round(unclass(f), digits = digits), 
            com = round(com, digits = digits), h2 = round(h2, 
                digits = digits))
    }
    else {
        results <- data.frame(round(unclass(f), digits = digits))
    }
    results <- merge(results, contents, by = "row.names", all.x = TRUE, 
        sort = FALSE)
    rownames(results) <- results[, "Row.names"]
    results <- results[ord, -1]
    return(results)
}


bifactor <- function (L, Tmat = diag(ncol(L)), normalize = FALSE, eps = 1e-05, 
    maxit = 1000) 
{
    if (requireNamespace("GPArotation")) {
        GPArotation::GPForth(L, Tmat = Tmat, normalize = normalize, 
            eps = eps, maxit = maxit, method = "bimin")
    }
    else {
        stop("Bifactor requires GPArotation")
    }
}


irt.person.rasch <- function (diff, items) 
{
    irt <- function(x, diff, scores) {
        fit <- -1 * (log(scores/(1 + exp(diff - x)) + (1 - scores)/(1 + 
            exp(x - diff))))
        mean(fit, na.rm = TRUE)
    }
    diff <- diff
    items <- items
    num <- dim(items)[1]
    fit <- matrix(NA, num, 2)
    total <- rowMeans(items, na.rm = TRUE)
    count <- rowSums(!is.na(items))
    for (i in 1:num) {
        if (count[i] > 0) {
            myfit <- optimize(irt, c(-4, 4), diff = diff, scores = items[i, 
                ])
            fit[i, 1] <- myfit$minimum
            fit[i, 2] <- myfit$objective
        }
        else {
            fit[i, 1] <- NA
            fit[i, 2] <- NA
        }
    }
    irt.person.rasch <- data.frame(total, theta = fit[, 1], fit = fit[, 
        2], count)
}


VSS <- function (x, n = 8, rotate = "varimax", diagonal = FALSE, fm = "minres", 
    n.obs = NULL, plot = TRUE, title = "Very Simple Structure", 
    use = "pairwise", cor = "cor", ...) 
{
    vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
        n.obs = n.obs, plot = plot, title = title, use = use, 
        cor = cor, ...)
}


histBy <- function (x, var, group, density = TRUE, alpha = 0.5, breaks = 21, 
    col, xlab, main = "Histograms by group", ...) 
{
    if (missing(xlab)) 
        xlab = var
    if (missing(group)) {
        if (missing(col)) 
            col12 <- col2rgb("blue", TRUE)/255
        col <- rgb(col12[1], col12[2], col12[3], alpha)
        hist(x[, var], xlab = xlab, main = main, breaks = breaks, 
            freq = FALSE, col = col, ...)
        d <- density(x[, var], na.rm = TRUE)
        if (density) 
            lines(d)
    }
    else {
        gr <- x[group]
        grp <- table(gr)
        if (missing(col)) 
            col <- rainbow(length(grp))
        col12 <- col2rgb(col, TRUE)/255
        col <- rgb(col12[1, ], col12[2, ], col12[3, ], alpha)
        xlim = range(x[var], na.rm = TRUE)
        grp <- names(grp)
        d <- density(x[(gr == grp[1]), var], na.rm = TRUE)
        hist(x[(gr == grp[1]), var], xlim = xlim, col = col[1], 
            breaks = breaks, freq = FALSE, xlab = xlab, main = main, 
            ...)
        if (density) 
            lines(d)
        for (i in (2:length(grp))) {
            hist(x[(gr == grp[i]), var], col = col[i], freq = FALSE, 
                breaks = breaks, add = TRUE, ...)
            d <- density(x[(gr == grp[i]), var], na.rm = TRUE)
            if (density) 
                lines(d)
        }
    }
}


t2r <- function (t, df) 
{
    t^2/(t^2 + df)
}


sim.anova <- function (es1 = 0, es2 = 0, es3 = 0, es12 = 0, es13 = 0, es23 = 0, 
    es123 = 0, es11 = 0, es22 = 0, es33 = 0, n = 2, n1 = 2, n2 = 2, 
    n3 = 2, within = NULL, r = 0.8, factors = TRUE, center = TRUE, 
    std = TRUE) 
{
    contrasts <- function(n) {
        if (n%%2) {
            seq(-floor(n/2), floor(n)/2)
        }
        else {
            seq(-(n - 1), (n - 1), 2)
        }
    }
    if (n1 * n2 * n3) {
        n <- n * n1 * n2 * n3
    }
    if (n1) {
        cont1 <- contrasts(n1)
        IV1 <- rep(cont1, n/n1)
    }
    else {
        IV1 <- IV1 <- rnorm(n)
    }
    if (n2) {
        cont2 <- contrasts(n2)
        if (n1) {
            IV2 <- rep(outer(rep(1, n1), contrasts(n2)), n/(n2 * 
                n1))
        }
        else {
            IV2 <- rep(cont2, n/n2)
        }
    }
    else {
        IV2 <- rnorm(n)
    }
    if (n3) {
        cont3 <- contrasts(n3)
        if (n1) {
            if (n2) {
                IV3 <- rep(outer(rep(1, n1 * n2), contrasts(n3)), 
                  n/(n1 * n2 * n3))
            }
            else {
                IV3 <- rep(outer(rep(1, n1), contrasts(n3)), 
                  n/(n1 * n3))
            }
        }
        else {
            if (n2) {
                IV3 <- rep(outer(rep(1, n2), contrasts(n3)), 
                  n/(n2 * n3))
            }
            else {
                IV3 <- rep(contrasts(n3), n/n3)
            }
        }
    }
    else {
        IV3 = rnorm(n)
    }
    if (factors) {
        if (n1) {
            iv1 <- factor(IV1)
        }
        else {
            iv1 <- IV1
        }
        if (n2) {
            iv2 <- factor(IV2)
        }
        else {
            iv2 <- IV2
        }
        if (n3) {
            iv3 <- factor(IV3)
        }
        else {
            iv3 <- IV3
        }
    }
    if (std) {
        IV1 <- IV1/sd(IV1)
        IV2 <- IV2/sd(IV2)
        IV3 <- IV3/sd(IV3)
    }
    y <- es1 * IV1 + es2 * IV2 + es3 * IV3 + es12 * IV1 * IV2 + 
        es13 * IV1 * IV3 + es23 * IV2 * IV3 + es123 * IV1 * IV2 * 
        IV3 + es11 * IV1 * IV1 + es22 * IV2 * IV2 + es33 * IV3 * 
        IV3 + rnorm(n)
    if (!is.null(within)) {
        yw <- within
        ny <- length(yw)
        y <- t(r * t(matrix(rep(y, ny), ncol = ny)) + yw + sqrt(1 - 
            r^2) * rnorm(n))
    }
    if (!center) {
        IV1 <- IV1 - min(IV1) + 1
        IV2 <- IV2 - min(IV2) + 1
        IV3 <- IV3 - min(IV3) + 1
    }
    if (factors) {
        y.df <- data.frame(IV1 = iv1, IV2 = iv2, IV3 = iv3, DV = y)
    }
    else {
        y.df <- data.frame(IV1, IV2, IV3, DV = y)
    }
}


setCor <- function (y, x, data, z = NULL, n.obs = NULL, use = "pairwise", 
    std = TRUE, square = FALSE, main = "Regression Models", plot = TRUE) 
{
    cl <- match.call()
    if (is.numeric(y)) 
        y <- colnames(data)[y]
    if (is.numeric(x)) 
        x <- colnames(data)[x]
    if (is.numeric(z)) 
        z <- colnames(data)[z]
    if ((dim(data)[1] != dim(data)[2]) | square) {
        n.obs = dim(data)[1]
        if (!is.null(z)) {
            data <- data[, c(y, x, z)]
        }
        else {
            data <- data[, c(y, x)]
        }
        if (!is.matrix(data)) 
            data <- as.matrix(data)
        if (!is.numeric(data)) 
            stop("The data must be numeric to proceed")
        C <- cov(data, use = use)
        if (std) {
            m <- cov2cor(C)
            C <- m
        }
        else {
            m <- C
        }
        raw <- TRUE
    }
    else {
        raw <- FALSE
        if (!is.matrix(data)) 
            data <- as.matrix(data)
        C <- data
        if (std) {
            m <- cov2cor(C)
        }
        else {
            m <- C
        }
    }
    nm <- dim(data)[1]
    xy <- c(x, y)
    numx <- length(x)
    numy <- length(y)
    numz <- 0
    nxy <- numx + numy
    m.matrix <- m[c(x, y), c(x, y)]
    x.matrix <- m[x, x, drop = FALSE]
    xc.matrix <- m[x, x, drop = FALSE]
    xy.matrix <- m[x, y, drop = FALSE]
    xyc.matrix <- m[x, y, drop = FALSE]
    y.matrix <- m[y, y, drop = FALSE]
    if (!is.null(z)) {
        numz <- length(z)
        zm <- m[z, z, drop = FALSE]
        za <- m[x, z, drop = FALSE]
        zb <- m[y, z, drop = FALSE]
        x.matrix <- x.matrix - za %*% solve(zm) %*% t(za)
        y.matrix <- y.matrix - zb %*% solve(zm) %*% t(zb)
        xy.matrix <- xy.matrix - za %*% solve(zm) %*% t(zb)
        m.matrix <- cbind(rbind(y.matrix, xy.matrix), rbind(t(xy.matrix), 
            x.matrix))
    }
    if (numx == 1) {
        beta <- matrix(xy.matrix, nrow = 1)/x.matrix[1, 1]
    }
    else {
        beta <- solve(x.matrix, xy.matrix)
        beta <- as.matrix(beta)
    }
    yhat <- t(xy.matrix) %*% solve(x.matrix) %*% (xy.matrix)
    resid <- y.matrix - yhat
    if (numy > 1) {
        if (is.null(rownames(beta))) {
            rownames(beta) <- x
        }
        if (is.null(colnames(beta))) {
            colnames(beta) <- y
        }
        R2 <- colSums(beta * xy.matrix)/diag(y.matrix)
    }
    else {
        colnames(beta) <- y
        R2 <- sum(beta * xy.matrix)/y.matrix
        R2 <- matrix(R2)
        rownames(beta) <- x
        rownames(R2) <- colnames(R2) <- y
    }
    px <- principal(x.matrix)
    keys.x <- diag(as.vector(1 - 2 * (px$loadings < 0)))
    py <- principal(y.matrix)
    keys.y <- diag(as.vector(1 - 2 * (py$loadings < 0)))
    Vx <- sum(keys.x %*% x.matrix %*% t(keys.x))
    Vy <- sum(keys.y %*% y.matrix %*% t(keys.y))
    ruw <- colSums(abs(xy.matrix))/sqrt(Vx)
    Ruw <- sum(diag(keys.x) %*% xy.matrix %*% t(keys.y))/sqrt(Vx * 
        Vy)
    if (numy < 2) {
        Rset <- 1 - det(m.matrix)/(det(x.matrix))
        Myx <- solve(x.matrix) %*% xy.matrix %*% t(xy.matrix)
        cc2 <- cc <- T <- NULL
    }
    else {
        if (numx < 2) {
            Rset <- 1 - det(m.matrix)/(det(y.matrix))
            Myx <- xy.matrix %*% solve(y.matrix) %*% t(xy.matrix)
            cc2 <- cc <- T <- NULL
        }
        else {
            Rset <- 1 - det(m.matrix)/(det(x.matrix) * det(y.matrix))
            if (numy > numx) {
                Myx <- solve(x.matrix) %*% xy.matrix %*% solve(y.matrix) %*% 
                  t(xy.matrix)
            }
            else {
                Myx <- solve(y.matrix) %*% t(xy.matrix) %*% solve(x.matrix) %*% 
                  (xy.matrix)
            }
        }
        cc2 <- eigen(Myx)$values
        cc <- sqrt(cc2)
        T <- sum(cc2)/length(cc2)
    }
    if (!is.null(n.obs)) {
        k <- length(x)
        uniq <- (1 - smc(x.matrix))
        se.beta <- list()
        for (i in 1:length(y)) {
            df <- n.obs - k - 1
            se.beta[[i]] <- (sqrt((1 - R2[i])/(df)) * sqrt(1/uniq))
        }
        se <- matrix(unlist(se.beta), ncol = length(y))
        colnames(se) <- colnames(beta)
        rownames(se) <- rownames(beta)
        se <- t(t(se) * sqrt(diag(C)[y]))/sqrt(diag(xc.matrix))
        tvalue <- beta/se
        prob <- 2 * (1 - pt(abs(tvalue), df))
        SE2 <- 4 * R2 * (1 - R2)^2 * (df^2)/((n.obs^2 - 1) * 
            (n.obs + 3))
        SE = sqrt(SE2)
        F <- R2 * df/(k * (1 - R2))
        pF <- 1 - pf(F, k, df)
        shrunkenR2 <- 1 - (1 - R2) * (n.obs - 1)/df
        u <- numx * numy
        m1 <- n.obs - max(numy, (numx + numz)) - (numx + numy + 
            3)/2
        s <- sqrt((numx^2 * numy^2 - 4)/(numx^2 + numy^2 - 5))
        if (numx * numy == 4) 
            s <- 1
        v <- m1 * s + 1 - u/2
        R2set.shrunk <- 1 - (1 - Rset) * ((v + u)/v)^s
        L <- 1 - Rset
        L1s <- L^(-1/s)
        Rset.F <- (L1s - 1) * (v/u)
        df.m <- n.obs - max(numy, (numx + numz)) - (numx + numy + 
            3)/2
        s1 <- sqrt((numx^2 * numy^2 - 4)/(numx^2 + numy^2 - 5))
        if (numx^2 * numy^2 < 5) 
            s1 <- 1
        df.v <- df.m * s1 + 1 - numx * numy/2
        Chisq <- -(n.obs - 1 - (numx + numy + 1)/2) * log((1 - 
            cc2))
    }
    if (is.null(n.obs)) {
        set.cor <- list(beta = beta, R = sqrt(R2), R2 = R2, Rset = Rset, 
            T = T, cancor = cc, cancor2 = cc2, raw = raw, residual = resid, 
            ruw = ruw, Ruw = Ruw, x.matrix = x.matrix, y.matrix = y.matrix, 
            Call = cl)
    }
    else {
        set.cor <- list(beta = beta, se = se, t = tvalue, Probability = prob, 
            R = sqrt(R2), R2 = R2, shrunkenR2 = shrunkenR2, seR2 = SE, 
            F = F, probF = pF, df = c(k, df), Rset = Rset, Rset.shrunk = R2set.shrunk, 
            Rset.F = Rset.F, Rsetu = u, Rsetv = df.v, T = T, 
            cancor = cc, cancor2 = cc2, Chisq = Chisq, raw = raw, 
            residual = resid, ruw = ruw, Ruw = Ruw, x.matrix = x.matrix, 
            y.matrix = y.matrix, Call = cl)
    }
    class(set.cor) <- c("psych", "setCor")
    if (plot) 
        setCor.diagram(set.cor, main = main)
    return(set.cor)
}


fa.plot <- function (ic.results, cluster = NULL, cut = 0, labels = NULL, 
    title, jiggle = FALSE, amount = 0.02, pch = 18, pos, show.points = TRUE, 
    choose = NULL, ...) 
{
    if (missing(title)) {
        title = "Plot"
        if (length(class(ic.results)) > 1) {
            if (class(ic.results)[2] == "fa") {
                title = "Factor Analysis"
            }
            else {
                if (class(ic.results)[2] == "principal") {
                  title = "Principal Component Analysis"
                }
            }
        }
    }
    if (!is.matrix(ic.results)) {
        if (!is.null(class(ic.results))) {
            if (class(ic.results)[1] == "kmeans") {
                load <- t(ic.results$centers)
            }
            else {
                load <- ic.results$loadings
            }
        }
    }
    else {
        load <- ic.results
    }
    if (is.null(colnames(load))) 
        colnames(load) <- paste("F", 1:ncol(load), sep = "")
    if (!is.null(choose)) 
        load <- load[, choose, drop = FALSE]
    nc <- dim(load)[2]
    nvar <- dim(load)[1]
    if (missing(pos)) 
        pos <- rep(1, nvar)
    ch.col = c("black", "blue", "red", "gray", "black", "blue", 
        "red", "gray")
    if (is.null(cluster)) {
        cluster <- rep(nc + 1, nvar)
        cluster <- apply(abs(load), 1, which.max)
        cluster[(apply(abs(load), 1, max) < cut)] <- nc + 1
    }
    "panel.points" <- function(x, y, pch = par("pch"), ...) {
        ymin <- min(y)
        ymax <- max(y)
        xmin <- min(x)
        xmax <- max(x)
        ylim <- c(min(ymin, xmin), max(ymax, xmax))
        xlim <- ylim
        if (show.points) 
            points(x, y, pch = pch, ylim = ylim, xlim = xlim, 
                ...)
        text(x, y, vnames, ...)
    }
    if (jiggle) 
        load <- jitter(load, amount = amount)
    if (nc > 2) {
        vnames <- labels
        pairs(load, pch = cluster + pch, col = ch.col[cluster], 
            bg = ch.col[cluster], lower.panel = panel.points, 
            upper.panel = panel.points, main = title, ...)
    }
    else {
        if (show.points) {
            plot(load, pch = cluster + pch, col = ch.col[cluster], 
                bg = ch.col[cluster], main = title, ...)
        }
        else {
            plot(load, pch = cluster + pch, col = ch.col[cluster], 
                bg = ch.col[cluster], main = title, type = "n", 
                ...)
            pos = NULL
        }
        abline(h = 0)
        abline(v = 0)
        if (is.null(labels)) 
            labels <- paste(1:nvar)
        text(load, labels, pos = pos, ...)
    }
}


scoreItems <- function (keys, items, totals = FALSE, ilabels = NULL, missing = TRUE, 
    impute = "median", delete = TRUE, min = NULL, max = NULL, 
    digits = 2, n.obs = NULL) 
{
    cl <- match.call()
    raw.data <- TRUE
    if (is.list(keys)) 
        keys <- make.keys(items, keys)
    keys <- as.matrix(keys)
    n.keys <- dim(keys)[2]
    n.items <- dim(keys)[1]
    abskeys <- abs(keys)
    keynames <- colnames(keys)
    num.item <- diag(t(abskeys) %*% abskeys)
    num.ob.item <- num.item
    if (!missing) 
        items <- na.omit(items)
    n.subjects <- dim(items)[1]
    if ((dim(items)[1] == dim(items)[2]) && !isCorrelation(items)) {
        warning("You have an equal number of rows and columns but do not seem to have  a correlation matrix.  I will treat this as a data matrix.")
    }
    if ((dim(items)[1] == dim(items)[2]) && isCorrelation(items)) {
        raw.data <- FALSE
        totals <- FALSE
        n.subjects <- 0
        C <- as.matrix(items)
        cov.scales <- t(keys) %*% C %*% keys
        cov.scales2 <- diag(t(abskeys) %*% C^2 %*% abskeys)
        response.freq <- NULL
    }
    else {
        if (!is.matrix(items)) {
            for (i in 1:n.items) {
                if (!is.numeric(items[[i]])) {
                  if (is.factor(unlist(items[[i]])) | is.character(unlist(items[[i]]))) {
                    items[[i]] <- as.numeric(items[[i]])
                    colnames(items)[i] <- paste0(colnames(items)[i], 
                      "*")
                  }
                  else {
                    items[[i]] <- NA
                  }
                }
            }
        }
        items <- as.matrix(items)
        response.freq <- response.frequencies(items)
        item.var <- apply(items, 2, sd, na.rm = TRUE)
        bad <- which((item.var == 0) | is.na(item.var))
        if ((length(bad) > 0) && delete) {
            for (baddy in 1:length(bad)) {
                warning("Item= ", colnames(items)[bad][baddy], 
                  " had no variance and was deleted from the data and the keys.")
            }
            items <- items[, -bad]
            keys <- as.matrix(keys[-bad, ])
            n.items <- n.items - length(bad)
            abskeys <- abs(keys)
            colnames(keys) <- keynames
        }
        item.means <- colMeans(items, na.rm = TRUE)
        if (is.null(min)) {
            min <- min(items, na.rm = TRUE)
        }
        if (is.null(max)) {
            max <- max(items, na.rm = TRUE)
        }
        miss.rep <- (is.na(items) + 0) %*% abs(keys)
        num.item <- diag(t(abskeys) %*% abskeys)
        num.ob.item <- num.item
        if (impute != "none") {
            miss <- which(is.na(items), arr.ind = TRUE)
            if (impute == "mean") {
                item.means <- colMeans(items, na.rm = TRUE)
                items[miss] <- item.means[miss[, 2]]
            }
            else {
                item.med <- apply(items, 2, median, na.rm = TRUE)
                items[miss] <- item.med[miss[, 2]]
            }
            scores <- items %*% keys
            C <- cov(items, use = "pairwise")
            cov.scales <- cov(scores, use = "pairwise")
            cov.scales2 <- diag(t(abskeys) %*% C^2 %*% abskeys)
        }
        else {
            scores <- matrix(NaN, ncol = n.keys, nrow = n.subjects)
            if (raw.data && totals == TRUE) 
                warning("Specifying totals = TRUE without imputation can lead to serious problems.  Are you sure?")
            for (scale in 1:n.keys) {
                pos.item <- items[, which(keys[, scale] > 0)]
                neg.item <- items[, which(keys[, scale] < 0)]
                neg.item <- max + min - neg.item
                sub.item <- cbind(pos.item, neg.item)
                if (!totals) {
                  scores[, scale] <- rowMeans(sub.item, na.rm = TRUE)
                }
                else {
                  scores[, scale] <- rowSums(sub.item, na.rm = TRUE)
                }
                rs <- rowSums(!is.na(sub.item))
                num.ob.item[scale] <- mean(rs[rs > 0])
            }
            C <- cov(items, use = "pairwise")
            cov.scales <- t(keys) %*% C %*% keys
            cov.scales2 <- diag(t(abskeys) %*% C^2 %*% abskeys)
            raw.data <- FALSE
        }
    }
    slabels <- colnames(keys)
    if (is.null(slabels)) {
        if (totals) {
            slabels <- paste("S", 1:n.keys, sep = "")
        }
        else {
            slabels <- paste("A", 1:n.keys, sep = "")
        }
    }
    item.var <- diag(C)
    var.scales <- diag(cov.scales)
    cor.scales <- cov2cor(cov.scales)
    sum.item.var <- item.var %*% abskeys
    sum.item.var2 <- item.var^2 %*% abskeys
    alpha.scale <- (var.scales - sum.item.var) * num.item/((num.item - 
        1) * var.scales)
    av.r <- alpha.scale/(num.item - alpha.scale * (num.item - 
        1))
    alpha.ob <- av.r * num.ob.item/(1 + (num.ob.item - 1) * av.r)
    colnames(alpha.scale) <- slabels
    alpha.scale[is.nan(alpha.scale)] <- 1
    Q = (2 * num.item^2/((num.item - 1)^2 * ((var.scales)^3))) * 
        (var.scales * (sum.item.var2 + sum.item.var^2) - 2 * 
            sum.item.var * cov.scales2)
    ase <- NULL
    if (raw.data) {
        item.cor <- cor(items, scores)
    }
    else {
        if (n.keys > 1) {
            item.cor <- C %*% keys %*% diag(1/sqrt(var.scales))/sqrt(item.var)
        }
        else {
            item.cor <- C %*% keys/sqrt(var.scales * item.var)
        }
    }
    colnames(item.cor) <- slabels
    c.smc <- smc(C, TRUE)
    diag(C) <- c.smc
    sum.smc <- c.smc %*% abskeys
    G6 <- (var.scales - sum.item.var + sum.smc)/var.scales
    corrected.var <- diag(t(keys) %*% C %*% keys)
    if (n.keys > 1) {
        item.rc <- (C %*% keys) %*% sqrt(diag(1/corrected.var))/sqrt(item.var)
    }
    else {
        item.rc <- C %*% keys/sqrt(corrected.var * item.var)
    }
    colnames(item.rc) <- slabels
    if (n.subjects > 0) {
        ase <- sqrt(Q/n.subjects)
    }
    else {
        if (!is.null(n.obs)) {
            ase <- sqrt(Q/n.obs)
        }
        else {
            ase = NULL
        }
    }
    if (is.null(ilabels)) {
        ilabels <- colnames(items)
    }
    if (is.null(ilabels)) {
        ilabels <- paste("I", 1:n.items, sep = "")
    }
    rownames(item.rc) <- ilabels
    if (raw.data) {
        correction <- (colSums(abs(keys) - (keys))/2) * (max + 
            min)
        scores <- scores + matrix(rep(correction, n.subjects), 
            byrow = TRUE, nrow = n.subjects)
        if (!totals) {
            if (n.keys > 1) {
                scores <- scores %*% diag(1/num.item)
            }
            else {
                scores <- scores/num.item
            }
        }
        colnames(scores) <- slabels
    }
    else {
        if (impute != "none") 
            scores <- NULL
    }
    scale.cor <- correct.cor(cor.scales, t(alpha.scale))
    rownames(alpha.scale) <- "alpha"
    rownames(av.r) <- "average.r"
    rownames(G6) <- "Lambda.6"
    sn <- av.r * num.item/(1 - av.r)
    rownames(sn) <- "Signal/Noise"
    if (!raw.data) {
        if (impute == "none") {
            if (!is.null(scores)) 
                colnames(scores) <- slabels
            results <- list(scores = scores, missing = miss.rep, 
                alpha = alpha.scale, av.r = av.r, sn = sn, n.items = num.item, 
                item.cor = item.cor, cor = cor.scales, corrected = scale.cor, 
                G6 = G6, item.corrected = item.rc, response.freq = response.freq, 
                raw = FALSE, alpha.ob = alpha.ob, num.ob.item = num.ob.item, 
                ase = ase, Call = cl)
        }
        else {
            results <- list(alpha = alpha.scale, av.r = av.r, 
                sn = sn, n.items = num.item, item.cor = item.cor, 
                cor = cor.scales, corrected = scale.cor, G6 = G6, 
                item.corrected = item.rc, response.freq = response.freq, 
                raw = FALSE, ase = ase, Call = cl)
        }
    }
    else {
        if (raw.data) {
            if (sum(miss.rep) > 0) {
                results <- list(scores = scores, missing = miss.rep, 
                  alpha = alpha.scale, av.r = av.r, sn = sn, 
                  n.items = num.item, item.cor = item.cor, cor = cor.scales, 
                  corrected = scale.cor, G6 = G6, item.corrected = item.rc, 
                  response.freq = response.freq, raw = TRUE, 
                  ase = ase, Call = cl)
            }
            else {
                results <- list(scores = scores, alpha = alpha.scale, 
                  av.r = av.r, sn = sn, n.items = num.item, item.cor = item.cor, 
                  cor = cor.scales, corrected = scale.cor, G6 = G6, 
                  item.corrected = item.rc, response.freq = response.freq, 
                  raw = TRUE, ase = ase, Call = cl)
            }
        }
    }
    class(results) <- c("psych", "score.items")
    return(results)
}


draw.tetra <- function (r, t1, t2, shade = TRUE) 
{
    binBvn <- function(rho, rc, cc) {
        row.cuts <- c(-Inf, rc, Inf)
        col.cuts <- c(-Inf, cc, Inf)
        P <- matrix(0, 2, 2)
        R <- matrix(c(1, rho, rho, 1), 2, 2)
        for (i in 1:2) {
            for (j in 1:2) {
                P[i, j] <- sadmvn(lower = c(row.cuts[i], col.cuts[j]), 
                  upper = c(row.cuts[i + 1], col.cuts[j + 1]), 
                  mean = rep(0, 2), varcov = R)
            }
        }
        P
    }
    def.par <- par(no.readonly = TRUE)
    if (missing(r)) 
        r <- 0.5
    if (missing(t1)) 
        t1 <- 1
    if (missing(t2)) 
        t2 <- 1
    segments = 101
    nf <- layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 
        1), c(1, 3), TRUE)
    par(mar = c(3, 3, 1, 1))
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (abs(r) > 0) {
        theta <- sign(r)/sqrt(2)
    }
    else {
        theta = 1/sqrt(2)
    }
    shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
        theta, -theta, theta), ncol = 2, byrow = TRUE)
    ellipse <- unit.circle %*% shape
    x <- t1
    y <- t2
    xrange <- c(-3, 3)
    yrange <- c(-3, 3)
    xloc = x + (3 - x)/2
    yloc <- y + (3 - y)/2
    plot(x, y, xlim = xrange, ylim = yrange, xlab = "X", ylab = "Y", 
        type = "n")
    lines(ellipse, type = "l")
    ellipse3 <- ellipse * 3
    lines(ellipse3, type = "l")
    abline(v = x)
    abline(h = y)
    if (shade) {
        poly <- matrix(c(rep(t1, segments + 1), rep(t2, segments + 
            1)), ncol = 2)
        poly[, 1] <- pmax(ellipse3[, 1], t1)
        poly[, 2] <- pmax(ellipse3[, 2], t2)
        polygon(poly, density = 10, angle = 90)
    }
    text(0, 0, paste("rho = ", r))
    HR <- 1 - pnorm(t1)
    SR <- 1 - pnorm(t2)
    P <- binBvn(r, HR, SR)
    ph <- phi(P)
    text(0, -0.3, paste("phi = ", round(ph, 2)))
    text(xloc, yloc + 0.1, expression(X > tau))
    text(xloc, yloc - 0.1, expression(Y > Tau))
    text(-xloc, yloc + 0.1, expression(X < tau))
    text(-xloc, yloc - 0.1, expression(Y > Tau))
    text(xloc, -yloc + 0.1, expression(X > tau))
    text(xloc, -yloc - 0.1, expression(Y < Tau))
    text(-xloc, -yloc + 0.1, expression(X < tau))
    text(-xloc, -yloc - 0.1, expression(Y < Tau))
    par(mar = c(0, 3, 1, 1))
    curve(dnorm(x), -3, 3, axes = FALSE)
    lines(c(x, x), c(0, dnorm(x)))
    text(xloc, dnorm(xloc) + 0.05, expression(X > tau))
    text(t1, dnorm(t1) + 0.05, expression(tau))
    if (shade) {
        xvals <- seq(t1, 3, 0.1)
        yvals <- dnorm(xvals)
        polygon(c(xvals, rev(xvals)), c(rep(0, length(xvals)), 
            dnorm(rev(xvals))), density = 10, angle = -45)
    }
    par(mar = c(3, 0, 1, 1))
    x1 <- seq(-3, 3, 6/segments)
    y1 <- dnorm(x1)
    plot(y1, x1, axes = FALSE, typ = "l")
    lines(c(0, dnorm(y)), c(y, y))
    text(0.1, yloc, expression(Y > Tau))
    text(dnorm(t2) + 0.05, t2, expression(Tau))
    if (shade) {
        yvals <- seq(t2, 3, 0.02)
        xvals <- dnorm(yvals)
        polygon(c(xvals, rev(xvals)), c(yvals, rep(t2, length(xvals))), 
            density = 10, angle = 45)
    }
    par(def.par)
}


read.clipboard.upper <- function (diag = TRUE, names = FALSE, ...) 
{
    MAC <- Sys.info()[1] == "Darwin"
    if (!MAC) {
        con <- file("clipboard")
    }
    else {
        con <- pipe("pbpaste")
    }
    xij <- scan(con, what = "char")
    close(con)
    m <- length(xij)
    d <- if (diag | names) 
        1
    else -1
    n <- floor((sqrt(1 + 8 * m) - d)/2)
    if (names) {
        name <- xij[1:n]
        xij <- xij[-c(1:n)]
    }
    xij <- as.numeric(xij)
    X <- diag(n)
    X[lower.tri(X, diag = diag)] <- xij
    diagonal <- diag(X)
    X <- t(X) + X
    diag(X) <- diagonal
    if (!names) 
        name <- paste("V", 1:n, sep = "")
    rownames(X) <- colnames(X) <- name
    return(X)
}


dfOrder <- function (object, columns) 
{
    nc <- length(columns)
    if (!is.data.frame(object)) 
        stop("I am sorry, I can only sort data.frames.")
    temp <- colnames(object)
    increase <- sign(columns)
    templist <- list()
    for (i in 1:nc) {
        templist[[i]] <- as.name(temp[abs(columns[i])])
        object[abs(columns[i])] <- increase[i] * object[abs(columns[i])]
    }
    ord <- with(object, do.call(order, templist))
    object <- object[ord, ]
    for (i in 1:nc) {
        object[abs(columns[i])] <- increase[i] * object[abs(columns[i])]
    }
    return(object)
}


d2r <- function (d) 
{
    d/sqrt(d^2 + 4)
}


VSS.simulate <- function (ncases = 1000, nvariables = 16, nfactors = 4, meanloading = 0.5, 
    dichot = FALSE, cut = 0) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    if (dichot) {
        items <- (items[, 1:nvariables] >= cut)
        items <- items + 0
    }
    return(items)
}


tableF <- function (x, y) 
{
    minx <- min(x, na.rm = TRUE)
    maxx <- max(x, na.rm = TRUE)
    miny <- min(y, na.rm = TRUE)
    maxy <- max(y, na.rm = TRUE)
    maxxy <- (maxx + (minx == 0)) * (maxy + (miny == 0))
    dims = c(maxx + 1 - min(1, minx), maxy + 1 - min(1, minx))
    bin <- x - minx + (y - miny) * (dims[1]) + max(1, minx)
    ans <- matrix(tabulate(bin, maxxy), dims)
    ans
}


dia.rect <- function (x, y = NULL, labels = NULL, cex = 1, xlim = c(0, 1), 
    ylim = c(0, 1), ...) 
{
    text(x = x, y = y, labels = labels, cex = cex, ...)
    xrange = (xlim[2] - xlim[1])
    yrange = (ylim[2] - ylim[1])
    xs <- 0.1 * xrange
    ys <- 0.1 * yrange
    len <- max(strwidth(labels, units = "user", cex = cex, ...), 
        strwidth("abc", units = "user", cex = cex, ...))/1.8
    vert <- max(strheight(labels, units = "user", cex = cex, 
        ...), strheight("ABC", units = "user", cex = cex, ...))/1
    rect(x - len, y - vert, x + len, y + vert)
    left <- c(x - len, y)
    right <- c(x + len, y)
    top <- c(x, y + vert)
    bottom <- c(x, y - vert)
    radius <- sqrt(len^2 + vert^2)
    dia.rect <- list(left = left, right = right, top = top, bottom = bottom, 
        center = c(x, y), radius = radius)
}


partial.r <- function (m, x, y) 
{
    cl <- match.call()
    if (dim(m)[1] != dim(m)[2]) {
        n.obs <- dim(m)[1]
        m <- cor(m, use = "pairwise")
    }
    if (!is.matrix(m)) 
        m <- as.matrix(m)
    nm <- dim(m)[1]
    t.mat <- matrix(0, ncol = nm, nrow = nm)
    xy <- c(x, y)
    numx <- length(x)
    numy <- length(y)
    nxy <- numx + numy
    for (i in 1:nxy) {
        t.mat[i, xy[i]] <- 1
    }
    reorder <- t.mat %*% m %*% t(t.mat)
    reorder[abs(reorder) > 1] <- NA
    X <- reorder[1:numx, 1:numx]
    Y <- reorder[1:numx, (numx + 1):nxy]
    phi <- reorder[(numx + 1):nxy, (numx + 1):nxy]
    phi.inv <- solve(phi)
    X.resid <- X - Y %*% phi.inv %*% t(Y)
    X.resid <- cov2cor(X.resid)
    colnames(X.resid) <- rownames(X.resid) <- colnames(m)[x]
    class(X.resid) <- c("psych", "partial.r")
    return(X.resid)
}


rmssd <- function (x, group = NULL, lag = 1, na.rm = TRUE) 
{
    return(sqrt(mssd(x, lag = lag, group = group, na.rm = na.rm)))
}


Yule2tetra <- function (Q, m, n = NULL, correct = TRUE) 
{
    if (!is.matrix(Q) && !is.data.frame(Q)) {
        result <- Yule2tet(Q, c(m[1], m[2]), n = n, correct = correct)
    }
    else {
        nvar <- nrow(Q)
        if (nvar != ncol(Q)) {
            stop("Matrix must be square")
        }
        if (length(m) != nvar) {
            stop("length of m must match the number of variables")
        }
        result <- Q
        for (i in 2:nvar) {
            for (j in 1:(i - 1)) {
                result[i, j] <- result[j, i] <- Yule2tet(Q[i, 
                  j], c(m[i], m[j]), n, correct = correct)
            }
        }
    }
    return(result)
}


polyserial <- function (x, y) 
{
    min.item <- min(y, na.rm = TRUE)
    max.item <- max(y, na.rm = TRUE)
    if (is.null(ncol(y))) {
        n.var <- 1
        n.cases <- length(y)
    }
    else {
        n.var <- ncol(y)
        n.cases <- dim(y)[1]
    }
    dummy <- matrix(rep(min.item:max.item, n.var), ncol = n.var)
    colnames(dummy) <- names(y)
    xdum <- rbind(y, dummy)
    frequency <- apply(xdum, 2, table)
    frequency <- t(frequency - 1)
    responses <- rowSums(frequency)
    frequency <- frequency/responses
    frequency <- t(apply(frequency, 1, cumsum))
    len <- dim(frequency)[2]
    tau <- dnorm(qnorm(frequency[, -len, drop = FALSE]))
    stau <- rowSums(tau)
    rxy <- cor(x, y, use = "pairwise")
    sdy <- apply(y, 2, sd, na.rm = TRUE)
    rps <- t(rxy) * sqrt((n.cases - 1)/n.cases) * sdy/stau
    rps[rps > 1] <- 1
    rps[rps < -1] <- -1
    return(rps)
}


plot.poly <- function (x, D, xlab, ylab, xlim, ylim, main, type = c("ICC", 
    "IIC", "test"), cut = 0.3, labels = NULL, keys = NULL, y2lab, 
    lncol = "black", ...) 
{
    item <- x
    byKeys <- FALSE
    if ((is.data.frame(x)) | (is.matrix(x))) {
        nf <- dim(x)[2] - 1
    }
    else {
        nf <- length(x$irt$difficulty)
    }
    if (!is.null(keys)) {
        nkeys = ncol(keys)
        if (nf < nkeys) {
            byKeys <- TRUE
            nf <- nkeys
        }
    }
    temp <- list()
    sumtemp <- list()
    x <- NULL
    nvar <- length(item$irt$discrimination[, 1])
    ncat <- dim(item$irt$difficulty[[1]])[2]
    if (length(lncol) < 2) 
        lncol <- rep(lncol, nvar)
    if (missing(type)) {
        type = "IIC"
    }
    if (missing(D)) {
        D <- 1.702
        if (missing(xlab)) 
            xlab <- "Latent Trait (normal scale)"
        if (missing(xlim)) {
            x <- seq(-3, 3, 0.1)
        }
        else {
            x <- seq(xlim[1], xlim[2], 0.1)
        }
        summaryx <- seq(-3, 3, 1)
    }
    if (D == 1) {
        if (missing(xlab)) 
            xlab <- "Latent Trait (logistic scale)"
    }
    if (missing(xlab)) 
        xlab <- "Latent Trait"
    if (is.null(x)) {
        x <- seq(-4, 4, 0.1)
        summaryx <- seq(-3, 3, 1)
    }
    if (is.null(labels)) {
        if (!is.null(rownames(item$irt$discrimination))) {
            labels = rownames(item$irt$discrimination)
        }
        else {
            labels <- 1:nvar
        }
    }
    lenx <- length(x)
    sumInfo <- matrix(NA, ncol = nvar, nrow = length(summaryx))
    for (f in 1:nf) {
        if (byKeys) {
            discrimination <- item$irt$discrimination[, 1]
        }
        else {
            discrimination <- item$irt$discrimination[, f]
        }
        if (!is.null(keys)) 
            discrimination <- discrimination * abs(keys[, f])
        if (any(is.nan(discrimination))) {
            bad <- which(is.nan(discrimination))
            discrimination[is.nan(discrimination)] <- max(discrimination, 
                na.rm = TRUE)
            warning("An discrimination  with a NaN value was replaced with the maximum discrimination for factor = ", 
                f, " and item ", labels[bad], "\nexamine the factor analysis object (fa)  to identify the Heywood case")
        }
        if (byKeys) {
            location = item$irt$difficulty[[1]]
        }
        else {
            location = item$irt$difficulty[[f]]
        }
        difficulty <- location[, 1:ncat]
        if (type == "ICC") {
            if (missing(main)) {
                main <- "Item parameters from factor analysis"
                main1 <- paste(main, "  ", f)
            }
            else {
                if (length(main) > 1) 
                  main1 <- main[f]
            }
            if (missing(ylab)) 
                ylab <- "Probability of Response"
            if (missing(ylim)) 
                ylim <- c(0, 1)
            for (i in 1:nvar) {
                if (abs(discrimination[i]) > cut) {
                  if (discrimination[i] > 0) {
                    plot(x, logistic(x, a = -D * discrimination[i], 
                      d = location[i, 1]), ylim = ylim, ylab = ylab, 
                      xlab = xlab, type = "l", main = main1, 
                      col = lncol[1], ...)
                    text(0, 0.7, labels[i])
                  }
                  else {
                    plot(x, logistic(x, a = D * discrimination[i], 
                      d = location[i, 1]), ylim = ylim, ylab = ylab, 
                      xlab = xlab, type = "l", main = main1, 
                      col = lncol[1], ...)
                    text(max(0), 0.7, paste("-", labels[i], sep = ""))
                  }
                  for (j in 2:(ncat)) {
                    if (discrimination[i] > 0) {
                      lines(x, (-logistic(x, a = D * discrimination[i], 
                        d = location[i, j]) + logistic(x, a = D * 
                        discrimination[i], d = location[i, j - 
                        1])), lty = c(1:6)[(j%%6) + 1], col = lncol[i])
                    }
                    else {
                      lines(x, (-logistic(x, a = -D * discrimination[i], 
                        d = location[i, j]) + logistic(x, a = -D * 
                        discrimination[i], d = location[i, j - 
                        1])), lty = c(1:6)[(j%%6) + 1], col = lncol[i])
                    }
                  }
                  if (discrimination[i] > 0) {
                    lines(x, (logistic(x, a = D * discrimination[i], 
                      d = location[i, ncat])), col = lncol[i])
                  }
                  else {
                    lines(x, (logistic(x, a = -D * discrimination[i], 
                      d = location[i, ncat])), col = lncol[i])
                  }
                }
            }
        }
        x <- as.matrix(x, ncol = 1)
        summaryx <- as.matrix(summaryx, ncol = 1)
        tInfo <- apply(x, 1, logisticInfo, a = discrimination, 
            d = sign(discrimination) * difficulty)
        tInfo <- array(unlist(tInfo), dim = c(nvar, ncat, length(x)))
        summtInfo <- apply(summaryx, 1, logisticInfo, a = discrimination, 
            d = sign(discrimination) * difficulty)
        summtInfo <- array(unlist(summtInfo), dim = c(nvar, ncat, 
            length(summaryx)))
        tInfo[is.nan(tInfo)] <- 0
        summtInfo[is.nan(summtInfo)] <- 0
        testInfo <- matrix(NA, ncol = nvar, nrow = length(x))
        sumInfo <- matrix(NA, ncol = nvar, nrow = length(summaryx))
        for (xi in 1:length(x)) {
            for (i in 1:nvar) {
                if (abs(discrimination[i]) > cut) {
                  testInfo[[xi, i]] <- sum(tInfo[i, , xi])
                }
                else {
                  testInfo[[xi, i]] <- 0
                }
            }
        }
        for (xi in 1:length(summaryx)) {
            for (i in 1:nvar) {
                if (abs(discrimination[i]) > cut) {
                  sumInfo[[xi, i]] <- sum(summtInfo[i, , xi])
                }
                else {
                  sumInfo[[xi, i]] <- 0
                }
            }
        }
        if (type == "test") {
            if (missing(main)) {
                main <- "Test information for factor "
                main1 <- paste(main, " ", f)
            }
            else {
                if (length(main) > 1) {
                  main1 <- main[f]
                }
                else {
                  main1 <- paste(main, "", f)
                }
            }
            if (missing(ylab)) 
                ylab <- "Test Information"
            if (missing(y2lab)) 
                y2lab <- "Reliability"
            rsInfo <- rowSums(testInfo)
            if (missing(ylim)) 
                ylim = c(0, max(rsInfo))
            op <- par(mar = c(5, 4, 4, 4))
            plot(x, rsInfo, typ = "l", ylim = ylim, ylab = ylab, 
                xlab = xlab, main = main1, col = lncol[1], ...)
            ax4 <- seq(0, ylim[2], ylim[2]/4)
            rel4 <- round(1 - 1/ax4, 2)
            rel4[1] <- NA
            axis(4, at = ax4, rel4)
            mtext(y2lab, side = 4, line = 2)
            op <- par(op)
        }
        else {
            if (type != "ICC") {
                if (missing(ylab)) 
                  ylab <- "Item Information"
                if (missing(main)) {
                  main1 <- paste("Item information from factor analysis for factor", 
                    f)
                }
                else {
                  if (length(main) > 1) {
                    main1 <- main[f]
                  }
                  else {
                    main1 <- main
                  }
                }
                if (missing(ylim)) 
                  ylim <- c(0, 1)
                ii <- 1
                while ((abs(discrimination[ii]) < cut) && (ii < 
                  nvar)) {
                  ii <- ii + 1
                }
                plot(x, testInfo[, ii], ylim = c(0, max(testInfo, 
                  na.rm = TRUE) + 0.03), ylab = ylab, xlab = xlab, 
                  type = "l", main = main1, col = lncol[1], ...)
                if (discrimination[ii] > 0) {
                  text(x[which.max(testInfo[, ii])], max(testInfo[, 
                    ii]) + 0.03, labels[ii])
                }
                else {
                  text(x[which.max(testInfo[, ii])], max(testInfo[, 
                    ii]) + 0.03, paste("-", labels[ii], sep = ""))
                }
                for (i in (ii + 1):nvar) {
                  if (abs(discrimination[i]) > cut) {
                    lines(x, testInfo[, i], lty = c(1:6)[(i%%6) + 
                      1], col = lncol[i])
                    if (discrimination[i] > 0) {
                      text(x[which.max(testInfo[, i])], max(testInfo[, 
                        i]) + 0.03, labels[i])
                    }
                    else {
                      text(x[which.max(testInfo[, i])], max(testInfo[, 
                        i]) + 0.03, paste("-", labels[i], sep = ""))
                    }
                  }
                }
            }
        }
        temp[[f]] <- testInfo
        sumInfo <- t(sumInfo)
        rownames(sumInfo) <- labels
        colnames(sumInfo) <- summaryx
        sumtemp[[f]] <- sumInfo
    }
    AUC <- matrix(NaN, ncol = nf, nrow = nvar)
    max.info <- matrix(NaN, ncol = nf, nrow = nvar)
    for (f in 1:nf) {
        AUC[, f] <- colSums(temp[[f]])
        max.info[, f] <- apply(temp[[f]], 2, which.max)
    }
    AUC <- AUC/lenx
    max.info <- (max.info - lenx/2) * 6/(lenx - 1)
    max.info[max.info < -2.9] <- NA
    if (byKeys) {
        colnames(AUC) <- colnames(max.info) <- colnames(keys)
    }
    else {
        colnames(AUC) <- colnames(max.info) <- colnames(item$irt$discrimination)
    }
    rownames(AUC) <- rownames(max.info) <- rownames(item$rho)
    result <- list(AUC = AUC, max.info = max.info, sumInfo = sumtemp)
    invisible(result)
    class(result) <- c("psych", "polyinfo")
    invisible(result)
}


reverse.code <- function (keys, items, mini = NULL, maxi = NULL) 
{
    if (is.vector(items)) {
        nvar <- 1
    }
    else {
        nvar <- dim(items)[2]
    }
    items <- as.matrix(items)
    if (is.null(maxi)) {
        colMax <- apply(items, 2, max, na.rm = TRUE)
    }
    else {
        colMax <- maxi
    }
    if (is.null(mini)) {
        colMin <- apply(items, 2, min, na.rm = TRUE)
    }
    else {
        colMin <- mini
    }
    colAdj <- colMax + colMin
    if (length(keys) < nvar) {
        temp <- keys
        if (is.character(temp)) 
            temp <- match(temp, colnames(items))
        keys <- rep(1, nvar)
        keys[temp] <- -1
    }
    keys.d <- diag(keys, nvar, nvar)
    items[is.na(items)] <- -9999
    reversed <- items %*% keys.d
    adj <- abs(keys * colAdj)
    adj[keys > 0] <- 0
    new <- t(adj + t(reversed))
    new[abs(new) > 999] <- NA
    colnames(new) <- colnames(items)
    colnames(new)[keys < 0] <- paste(colnames(new)[keys < 0], 
        "-", sep = "")
    return(new)
}


reflect <- function (f, flip = NULL) 
{
    rnames <- colnames(f$loadings)
    rnames[flip] <- paste(rnames[flip], "(R)", sep = "")
    flipper <- rep(1, ncol(f$loadings))
    flipper[flip] <- -1
    flip <- diag(flipper)
    colnames(flip) <- rownames(flip) <- rnames
    f$loadings <- f$loadings %*% flip
    if (!is.null(f$weights)) 
        f$weights <- f$weights %*% flip
    if (!is.null(f$scores)) 
        f$scores <- f$scores %*% flip
    if (!is.null(f$Phi)) 
        f$Phi <- flip %*% f$Phi %*% t(flip)
    return(f)
}


bestScales <- function (x, criteria, cut = 0.1, n.item = 10, overlap = FALSE, 
    dictionary = NULL, digits = 2) 
{
    findBad <- function(key, r) {
        ss <- abs(key) > 0
        rss <- r[ss, ss]
        if (any(is.na(rss))) {
            n.bad <- apply(rss, 1, function(x) sum(is.na(x)))
            key[names(which.max(n.bad))] <- 0
            findBad(key, r)
        }
        return(key)
    }
    short <- function(key, r) {
        kn <- names(key[abs(key[, 1]) > 0, 1])
        if (is.null(kn)) 
            kn <- names(which(abs(key[, 1]) > 0))
        cn <- colnames(key)
        ord <- order(abs(r[kn, cn]), decreasing = TRUE)
        kn <- kn[ord]
        result <- r[kn, cn, drop = FALSE]
        return(result)
    }
    nvar <- ncol(x)
    if (nrow(x) != nvar) {
        r <- cor(x, use = "pairwise")
    }
    else {
        r <- x
    }
    ny <- length(criteria)
    nc <- length(cut)
    ni <- length(n.item)
    ord.name <- NULL
    if (length(cut) == 1) 
        cut <- rep(cut, ny)
    if (length(n.item) == 1) 
        n.item <- rep(n.item, ny)
    if (ny > 1) {
        ord <- apply(abs(r[, criteria]), 2, order, decreasing = TRUE)
        for (i in 1:ny) {
            cut[i] <- max(cut[i], abs(r[criteria[i], ord[n.item[i] + 
                1, i]]))
            ord.name <- c(ord.name, rownames(r)[ord[1:n.item[i], 
                i]])
        }
    }
    else {
        ord <- order(abs(r[criteria, ]), decreasing = TRUE)
        for (i in 1:ny) {
            cut[i] <- max(cut[i], abs(r[ord[n.item[i] + 1], criteria]))
        }
    }
    key <- matrix(0, ncol = ny, nrow = nvar)
    key[t(r[criteria, ] >= cut)] <- 1
    key[t(r[criteria, ] <= -cut)] <- -1
    rownames(key) <- colnames(r)
    colnames(key) <- criteria
    c <- key
    if (!overlap) {
        key[criteria, criteria] <- 0
    }
    else {
        for (i in 1:ny) key[criteria[i], criteria[i]] <- 0
    }
    colnames(key) <- criteria
    if (any(is.na(r))) {
        for (i in 1:ny) {
            c[, i] <- colSums(key[, i] * r, na.rm = TRUE)
        }
        c <- t(c)
    }
    else {
        c <- t(key) %*% r
    }
    C <- c %*% key
    if (ny < 2) {
        re <- c[, criteria]/sqrt(C)
    }
    else {
        re <- diag(c[, criteria])/sqrt(diag(C))
    }
    ni <- colSums(abs(key))
    R <- cov2cor(C)
    short.key <- list()
    value <- list()
    for (i in 1:ny) {
        short.key[[criteria[i]]] <- short(key[, i, drop = FALSE], 
            r)
        if (!is.null(dictionary)) {
            if (!is.factor(dictionary)) {
                temp <- lookup(rownames(short.key[[criteria[i]]]), 
                  dictionary)
                value[[criteria[[i]]]] <- merge(short.key[[i]], 
                  temp, by = "row.names", all.x = TRUE, sort = FALSE)
                ord <- order(abs(value[[criteria[[i]]]][[criteria[[i]]]]), 
                  decreasing = TRUE)
                value[[criteria[[i]]]] <- value[[criteria[[i]]]][ord, 
                  ]
            }
        }
    }
    results <- list(r = re, n.items = ni, R = R, cut = cut, short.key = short.key, 
        value = value, key = key, ordered = ord.name)
    class(results) <- c("psych", "bestScales")
    return(results)
}


irt.tau <- function (x) 
{
    x <- as.matrix(x)
    nvar <- dim(x)[2]
    xmin <- min(x, na.rm = TRUE)
    xmax <- max(x, na.rm = TRUE)
    nvalues <- xmax - xmin + 1
    if (nvalues > 10) 
        stop("You have more than 10 categories for your items, polychoric is probably not needed")
    if (nvalues == 2) {
        tau <- -qnorm(colMeans(x, na.rm = TRUE))
        tau <- as.matrix(tau)
        rownames(tau) <- colnames(x)
    }
    else {
        if (nvalues > 10) 
            stop("You have more than 10 categories for your items, polychoric is probably not needed")
        xfreq <- apply(x - xmin + 1, 2, tabulate, nbins = nvalues)
        n.obs <- colSums(xfreq)
        xfreq <- t(t(xfreq)/n.obs)
        tau <- qnorm(apply(xfreq, 2, cumsum))[1:(nvalues - 1), 
            ]
        if (!is.matrix(tau)) 
            tau <- matrix(tau, ncol = nvar)
        rownames(tau) <- paste0(xmin:(xmax - 1))
        colnames(tau) <- colnames(x)
        if (dim(tau)[1] < dim(tau)[2]) 
            tau <- t(tau)
    }
    class(tau) <- c("psych", "tau")
    return(tau)
}


fa.rgraph <- function (fa.results, out.file = NULL, labels = NULL, cut = 0.3, 
    simple = TRUE, size = c(8, 6), node.font = c("Helvetica", 
        14), edge.font = c("Helvetica", 10), rank.direction = c("RL", 
        "TB", "LR", "BT"), digits = 1, main = "Factor Analysis", 
    graphviz = TRUE, ...) 
{
    if (!requireNamespace("Rgraphviz")) {
        stop("I am sorry, you need to have loaded the Rgraphviz package")
        nodes <- function() {
        }
        addEdge <- function() {
        }
        subGraph <- function() {
        }
    }
    Phi <- NULL
    if ((!is.matrix(fa.results)) && (!is.data.frame(fa.results))) {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) 
            Phi <- fa.results$Phi
    }
    else {
        factors <- fa.results
    }
    rank.direction <- match.arg(rank.direction)
    num.var <- dim(factors)[1]
    if (is.null(num.var)) {
        num.var <- length(factors)
        num.factors <- 1
    }
    else {
        num.factors <- dim(factors)[2]
    }
    if (simple) {
        k = 1
    }
    else {
        k <- num.factors
    }
    vars <- paste("V", 1:num.var, sep = "")
    fact <- paste("F", 1:num.factors, sep = "")
    clust.graph <- new("graphNEL", nodes = c(vars, fact), edgemode = "directed")
    graph.shape <- c(rep("box", num.var), rep("ellipse", num.factors))
    graph.rank <- c(rep("sink", num.var), rep("min", num.factors))
    names(graph.shape) <- nodes(clust.graph)
    names(graph.rank) <- nodes(clust.graph)
    edge.label <- rep("", num.var * k)
    edge.name <- rep("", num.var * k)
    names(edge.label) <- seq(1:num.var * k)
    edge.dir <- rep("forward", num.var * k)
    l <- factors
    if (num.factors == 1) {
        for (i in 1:num.var) {
            clust.graph <- addEdge(fact[1], vars[i], clust.graph, 
                1)
            edge.label[i] <- round(factors[i], digits)
            edge.name[i] <- paste(fact[1], "~", vars[i], sep = "")
        }
    }
    else {
        if (simple) {
            m1 <- matrix(apply(t(apply(l, 1, abs)), 1, which.max), 
                ncol = 1)
            for (i in 1:num.var) {
                clust.graph <- addEdge(fact[m1[i]], vars[i], 
                  clust.graph, 1)
                edge.label[i] <- round(factors[i, m1[i]], digits)
                edge.name[i] <- paste(fact[m1[i]], "~", vars[i], 
                  sep = "")
            }
        }
        else {
            k <- 1
            for (i in 1:num.var) {
                for (f in 1:num.factors) {
                  if (abs(factors[i, f]) > cut) {
                    clust.graph <- addEdge(fact[f], vars[i], 
                      clust.graph, 1)
                    edge.label[k] <- round(factors[i, f], digits)
                    edge.name[k] <- paste(fact[f], "~", vars[i], 
                      sep = "")
                    k <- k + 1
                  }
                }
            }
        }
    }
    if (!is.null(Phi)) {
        k <- num.var + 1
        for (f in 2:num.factors) {
            for (f1 in 1:(f - 1)) {
                if (Phi[f, f1] > cut) {
                  clust.graph <- addEdge(fact[f1], fact[f], clust.graph, 
                    1)
                  edge.label[k] <- round(Phi[f, f1], digits)
                  edge.name[k] <- paste(fact[f1], "~", fact[f], 
                    sep = "")
                  edge.dir[k] <- paste("both")
                  k <- k + 1
                }
            }
        }
    }
    nAttrs <- list()
    eAttrs <- list()
    if (!is.null(labels)) {
        var.labels <- c(labels, fact)
        names(var.labels) <- nodes(clust.graph)
        nAttrs$label <- var.labels
        names(edge.label) <- edge.name
    }
    names(edge.label) <- edge.name
    names(edge.dir) <- edge.name
    nAttrs$shape <- graph.shape
    nAttrs$rank <- graph.rank
    eAttrs$label <- edge.label
    eAttrs$dir <- edge.dir
    attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE), 
        graph = list(rankdir = rank.direction, fontsize = edge.font[2], 
            bgcolor = "white"))
    obs.var <- subGraph(vars, clust.graph)
    cluster.vars <- subGraph(fact, clust.graph)
    observed <- list(list(graph = obs.var, cluster = TRUE, attrs = c(rank = "sink")), 
        list(graph = cluster.vars, cluster = FALSE, attrs = c(rank = "source")))
    observed <- list(list(graph = obs.var, cluster = TRUE, attrs = c(rank = "sink")))
    plot(clust.graph, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
        attrs = attrs, subGList = observed, main = main)
    if (!is.null(out.file)) {
        toDotty(clust.graph, out.file, nodeAttrs = nAttrs, edgeAttrs = eAttrs, 
            attrs = attrs)
    }
    return(clust.graph)
}


logistic.grm <- function (x, d = 0, a = 1.5, c = 0, z = 1, r = 2, s = c(-1.5, 
    -0.5, 0.5, 1.5)) 
{
    if (r == 1) {
        p <- (1 - logistic(x, d = s[1], a = a, c = c, z = z))
    }
    else {
        if (r == (length(s) + 1)) {
            p <- logistic(x, d = s[r - 1], a = a, c = c, z = z)
        }
        else {
            p <- logistic(x, d = s[r - 1], a = a, c = c, z = z) - 
                logistic(x, d = s[r], a = a, c = c, z = z)
        }
    }
    p
}


pairwiseDescribe <- function (x, diagonal = FALSE, ...) 
{
    cp <- count.pairwise(x, diagonal = diagonal)
    cp <- as.vector(cp[lower.tri(cp)])
    describe(cp, ...)
}


vgQ.varimin <- function (L) 
{
    QL <- sweep(L^2, 2, colMeans(L^2), "-")
    list(Gq = L * QL, f = sqrt(sum(diag(crossprod(QL))))^2/4, 
        Method = "varimin")
}


principal <- function (r, nfactors = 1, residuals = FALSE, rotate = "varimax", 
    n.obs = NA, covar = FALSE, scores = TRUE, missing = FALSE, 
    impute = "median", oblique.scores = TRUE, method = "regression", 
    ...) 
{
    cl <- match.call()
    n <- dim(r)[2]
    if (!isCorrelation(r)) {
        raw <- TRUE
        n.obs <- dim(r)[1]
        if (scores) {
            x.matrix <- as.matrix(r)
            if (missing) {
                miss <- which(is.na(x.matrix), arr.ind = TRUE)
                if (impute == "mean") {
                  item.means <- colMeans(x.matrix, na.rm = TRUE)
                  x.matrix[miss] <- item.means[miss[, 2]]
                }
                else {
                  item.med <- apply(x.matrix, 2, median, na.rm = TRUE)
                  x.matrix[miss] <- item.med[miss[, 2]]
                }
            }
        }
        if (!covar) {
            r <- cor(r, use = "pairwise")
        }
        else r <- cov(r, use = "pairwise")
    }
    else {
        raw <- FALSE
        if (!is.matrix(r)) {
            r <- as.matrix(r)
        }
        sds <- sqrt(diag(r))
        if (!covar) 
            r <- r/(sds %o% sds)
    }
    if (!residuals) {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), fit = 0, fit.off = 0)
    }
    else {
        result <- list(values = c(rep(0, n)), rotation = rotate, 
            n.obs = n.obs, communality = c(rep(0, n)), loadings = matrix(rep(0, 
                n * n), ncol = n), residual = matrix(rep(0, n * 
                n), ncol = n), fit = 0, fit.off = 0)
    }
    if (any(is.na(r))) {
        bad <- TRUE
        tempr <- r
        wcl <- NULL
        while (bad) {
            wc <- table(which(is.na(tempr), arr.ind = TRUE))
            wcl <- c(wcl, as.numeric(names(which(wc == max(wc)))))
            tempr <- r[-wcl, -wcl]
            if (any(is.na(tempr))) {
                bad <- TRUE
            }
            else {
                bad <- FALSE
            }
        }
        cat("\nLikely variables with missing values are ", colnames(r)[wcl], 
            " \n")
        stop("I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue.\nPlease drop those variables and try again.")
    }
    eigens <- eigen(r)
    result$values <- eigens$values
    eigens$values[eigens$values < .Machine$double.eps] <- .Machine$double.eps
    loadings <- eigens$vectors %*% sqrt(diag(eigens$values, nrow = length(eigens$values)))
    if (nfactors > 0) {
        loadings <- loadings[, 1:nfactors]
    }
    else {
        nfactors <- n
    }
    if (nfactors > 1) {
        communalities <- rowSums(loadings^2)
    }
    else {
        communalities <- loadings^2
    }
    uniquenesses <- diag(r) - communalities
    names(communalities) <- colnames(r)
    if (nfactors > 1) {
        sign.tot <- vector(mode = "numeric", length = nfactors)
        sign.tot <- sign(colSums(loadings))
        sign.tot[sign.tot == 0] <- 1
        loadings <- loadings %*% diag(sign.tot)
    }
    else {
        if (sum(loadings) < 0) {
            loadings <- -as.matrix(loadings)
        }
        else {
            loadings <- as.matrix(loadings)
        }
        colnames(loadings) <- "PC1"
    }
    colnames(loadings) <- paste("PC", 1:nfactors, sep = "")
    rownames(loadings) <- rownames(r)
    Phi <- NULL
    rot.mat <- NULL
    if (rotate != "none") {
        if (nfactors > 1) {
            if (rotate == "varimax" | rotate == "Varimax" | rotate == 
                "quartimax" | rotate == "bentlerT" | rotate == 
                "geominT" | rotate == "targetT" | rotate == "bifactor" | 
                rotate == "TargetT" | rotate == "equamax" | rotate == 
                "varimin" | rotate == "specialT" | rotate == 
                "Promax" | rotate == "promax" | rotate == "cluster" | 
                rotate == "biquartimin" | rotate == "TargetQ" | 
                rotate == "specialQ") {
                Phi <- NULL
                colnames(loadings) <- paste("RC", 1:nfactors, 
                  sep = "")
                switch(rotate, varimax = {
                  rotated <- stats::varimax(loadings, ...)
                  loadings <- rotated$loadings
                  rot.mat <- rotated$rotmat
                }, Varimax = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::Varimax(loadings, ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, quartimax = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::quartimax(loadings, 
                    ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, bentlerT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::bentlerT(loadings, 
                    ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, geominT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::geominT(loadings, ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, targetT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rotated <- GPArotation::targetT(loadings, Tmat = diag(ncol(loadings)), 
                    ...)
                  loadings <- rotated$loadings
                  rot.mat <- t(solve(rotated$Th))
                }, bifactor = {
                  rot <- bifactor(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, TargetT = {
                  if (!requireNamespace("GPArotation")) {
                    stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
                  }
                  rot <- GPArotation::targetT(loadings, Tmat = diag(ncol(loadings)), 
                    ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, equamax = {
                  rot <- equamax(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, varimin = {
                  rot <- varimin(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, specialT = {
                  rot <- specialT(loadings, ...)
                  loadings <- rot$loadings
                  rot.mat <- t(solve(rot$Th))
                }, Promax = {
                  pro <- Promax(loadings, ...)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                  rot.mat <- pro$rotmat
                }, promax = {
                  pro <- stats::promax(loadings, ...)
                  loadings <- pro$loadings
                  rot.mat <- pro$rotmat
                  ui <- solve(rot.mat)
                  Phi <- cov2cor(ui %*% t(ui))
                }, cluster = {
                  loadings <- varimax(loadings, ...)$loadings
                  pro <- target.rot(loadings)
                  loadings <- pro$loadings
                  Phi <- pro$Phi
                  rot.mat <- pro$rotmat
                }, biquartimin = {
                  ob <- biquartimin(loadings, ...)
                  loadings <- ob$loadings
                  Phi <- ob$Phi
                  rot.mat <- t(solve(ob$Th))
                }, TargetQ = {
                  ob <- TargetQ(loadings, ...)
                  loadings <- ob$loadings
                  Phi <- ob$Phi
                  rot.mat <- t(solve(ob$Th))
                }, specialQ = {
                  ob <- specialQ(loadings, ...)
                  loadings <- ob$loadings
                  Phi <- ob$Phi
                  rot.mat <- t(solve(pro$Th))
                })
            }
            else {
                colnames(loadings) <- paste("TC", 1:nfactors, 
                  sep = "")
                if (rotate == "oblimin" | rotate == "quartimin" | 
                  rotate == "simplimax" | rotate == "geominQ" | 
                  rotate == "bentlerQ" | rotate == "targetQ") {
                  if (!requireNamespace("GPArotation")) {
                    warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
                    Phi <- NULL
                  }
                  else {
                    ob <- try(do.call(getFromNamespace(rotate, 
                      "GPArotation"), list(loadings, ...)))
                    if (class(ob) == as.character("try-error")) {
                      warning("The requested transformaton failed, Promax was used instead as an oblique transformation")
                      ob <- Promax(loadings)
                    }
                    loadings <- ob$loadings
                    Phi <- ob$Phi
                    rot.mat <- t(solve(ob$Th))
                  }
                }
                else {
                  message("Specified rotation not found, rotate='none' used")
                  colnames(loadings) <- paste("PC", 1:nfactors, 
                    sep = "")
                }
            }
        }
    }
    if (nfactors > 1) {
        ev.rotated <- diag(t(loadings) %*% loadings)
        ev.order <- order(ev.rotated, decreasing = TRUE)
        loadings <- loadings[, ev.order]
    }
    if (!is.null(Phi)) {
        Phi <- Phi[ev.order, ev.order]
    }
    signed <- sign(colSums(loadings))
    c.names <- colnames(loadings)
    signed[signed == 0] <- 1
    loadings <- loadings %*% diag(signed)
    colnames(loadings) <- c.names
    if (!is.null(Phi)) {
        Phi <- diag(signed) %*% Phi %*% diag(signed)
        colnames(Phi) <- rownames(Phi) <- c.names
    }
    class(loadings) <- "loadings"
    result$n.obs <- n.obs
    stats <- factor.stats(r, loadings, Phi, n.obs, fm = "pc")
    class(result) <- c("psych", "principal")
    result$fn <- "principal"
    result$loadings <- loadings
    result$Phi <- Phi
    result$Call <- cl
    result$communality <- communalities
    result$uniquenesses <- uniquenesses
    result$complexity <- stats$complexity
    result$chi <- stats$chi
    result$EPVAL <- stats$EPVAL
    result$R2 <- stats$R2
    result$objective <- stats$objective
    result$residual <- stats$residual
    result$rms <- stats$rms
    result$fit <- stats$fit
    result$fit.off <- stats$fit.off
    result$factors <- stats$factors
    result$dof <- stats$dof
    result$null.dof <- stats$null.dof
    result$null.model <- stats$null.model
    result$criteria <- stats$criteria
    result$STATISTIC <- stats$STATISTIC
    result$PVAL <- stats$PVAL
    result$weights <- stats$weights
    result$r.scores <- stats$r.scores
    result$rot.mat <- rot.mat
    if (!is.null(Phi) && oblique.scores) {
        result$Structure <- loadings %*% Phi
    }
    else {
        result$Structure <- loadings
    }
    if (scores && raw) {
        result$weights <- solve(r, result$Structure)
        result$scores <- scale(x.matrix, scale = !covar) %*% 
            result$weights
    }
    return(result)
}


sim.npl <- function (nvar = 5, n = 500, low = -3, high = 3, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1) 
{
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    if (is.null(a)) {
        a <- rep(1, nvar)
    }
    theta <- rnorm(n, mu, sd)
    item <- matrix(t(c + (z - c)/(1 + exp(a * t((-theta %+% t(d)))))), 
        n, nvar)
    item[] <- rbinom(n * nvar, 1, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(items = item, discrimination = a, difficulty = d, 
        gamma = c, zeta = z, theta = theta)
    return(result)
}


sim.npn <- function (nvar = 5, n = 500, low = -3, high = 3, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1) 
{
    if (is.null(d)) {
        d <- seq(low, high, (high - low)/(nvar - 1))
    }
    else {
        if (length(d) == 1) 
            d <- rep(d, nvar)
    }
    if (is.null(a)) {
        a <- rep(1, nvar)
    }
    theta <- rnorm(n, mu, sd)
    item <- matrix(t(c + (z - c) * pnorm(a * t(theta %+% t(-d)))), 
        n, nvar)
    item[] <- rbinom(n * nvar, 1, item)
    colnames(item) <- paste("V", 1:nvar, sep = "")
    result <- list(items = item, discrimination = a, difficulty = d, 
        gamma = c, zeta = z, theta = theta)
    return(result)
}


tenberge <- function (r) 
{
    n <- dim(r)[2]
    if (dim(r)[1] > n) {
        r <- cor(r, use = "pairwise")
    }
    vt <- sum(r)
    off <- r
    diag(off) <- 0
    sum.off <- sum(off)
    sumsq.off <- sum(off^2)
    lambda1 <- n * sum(off)/((n - 1) * vt)
    lambda2 <- (sum.off + sqrt(sumsq.off * n/(n - 1)))/vt
    lambda3 <- (sum.off + sqrt(sumsq.off + sqrt((n * sum(off^4)/(n - 
        1)))))/vt
    lambda4 <- (sum.off + sqrt(sumsq.off + sqrt(sum(off^4) + 
        sqrt((n * sum(off^8)/(n - 1))))))/vt
    return(list(mu0 = lambda1, mu1 = lambda2, mu2 = lambda3, 
        mu3 = lambda4))
}


make.congeneric <- function (loads = c(0.8, 0.7, 0.6, 0.5), N = 1000, err = NULL, 
    short = TRUE) 
{
    n <- length(loads)
    loading <- matrix(loads, nrow = n)
    error <- diag(1, nrow = n)
    if (!is.null(err)) {
        diag(error) <- err
    }
    else {
        diag(error) <- sqrt(1 - loading^2)
    }
    pattern <- cbind(loading, error)
    colnames(pattern) <- c("theta", paste("e", seq(1:n), sep = ""))
    rownames(pattern) <- c(paste("V", seq(1:n), sep = ""))
    model <- pattern %*% t(pattern)
    latent <- matrix(rnorm(N * (n + 1)), ncol = (n + 1))
    observed <- latent %*% t(pattern)
    colnames(latent) <- c("theta", paste("e", seq(1:n), sep = ""))
    if (short) {
        return(model)
    }
    else {
        result <- list(model = model, pattern = pattern, observed = observed, 
            latent = latent)
        return(result)
    }
}


error.dots <- function (x, var = NULL, se = NULL, group = NULL, sd = FALSE, 
    head = 12, tail = 12, sort = TRUE, decreasing = TRUE, main = NULL, 
    alpha = 0.05, eyes = FALSE, min.n = NULL, max.labels = 40, 
    labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pt.cex = cex, pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), 
    gcolor = par("fg"), lcolor = "gray", xlab = NULL, ylab = NULL, 
    xlim = NULL, ...) 
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (length(class(x)) > 1) {
        if (class(x)[1] == "psych") {
            obj <- class(x)[2]
            switch(obj, statsBy = {
                if (is.null(min.n)) {
                  se <- x$sd[, var]/sqrt(x$n[, var])
                  x <- x$mean[, var]
                } else {
                  se <- x$sd[, var]
                  n.obs <- x$n[, var]
                  x <- x$mean[, var]
                  if (sd) {
                    se <- x$sd[, var]
                  } else {
                    se <- se/sqrt(n.obs)
                  }
                  x <- subset(x, n.obs > min.n)
                  se <- subset(se, n.obs > min.n)
                  n.obs <- subset(n.obs, n.obs > min.n)
                }
            }, describe = {
                if (sd) {
                  se <- x$sd
                } else {
                  se <- x$se
                }
                labels <- rownames(x)
                x <- x$mean
                names(x) <- labels
            }, describeBy = {
                des <- x
                if (is.null(xlab)) xlab <- var
                var <- which(rownames(des[[1]]) == var)
                x <- se <- rep(NA, length(des))
                for (grp in 1:length(x)) {
                  x[grp] <- des[[grp]][["mean"]][var]
                  if (sd) {
                    se[grp] <- des[[grp]][["sd"]][var]
                  } else {
                    se[grp] <- des[[grp]][["se"]][var]
                  }
                }
                names(x) <- names(des)
                if (is.null(xlab)) xlab <- var
            })
        }
    }
    else {
        if (is.null(group)) {
            des <- describe(x)
            x <- des$mean
            if (sd) {
                se <- des$sd
            }
            else {
                se <- des$se
            }
            names(x) <- rownames(des)
        }
        else {
            if (is.null(xlab)) 
                xlab <- var
            des <- describeBy(x, group = group)
            x <- se <- rep(NA, length(des))
            names(x) <- names(des)
            var <- which(rownames(des[[1]]) == var)
            for (grp in 1:length(des)) {
                x[grp] <- des[[grp]][["mean"]][var]
                if (sd) {
                  se[grp] <- des[[grp]][["sd"]][var]
                }
                else {
                  se[grp] <- des[[grp]][["se"]][var]
                }
            }
        }
    }
    n.var <- length(x)
    if (sort) {
        ord <- order(x, decreasing = !decreasing)
    }
    else {
        ord <- n.var:1
    }
    x <- x[ord]
    se <- se[ord]
    temp <- temp.se <- rep(NA, min(head + tail, n.var))
    if ((head + tail) < n.var) {
        if (head > 0) {
            temp[1:head] <- x[1:head]
            temp.se[1:head] <- se[1:head]
            names(temp) <- names(x)[1:head]
        }
        if (tail > 0) {
            temp[(head + 1):(head + tail)] <- x[(length(x) - 
                tail + 1):length(x)]
            temp.se[(head + 1):(head + tail)] <- se[(length(x) - 
                tail + 1):length(x)]
            names(temp)[(head + 1):(head + tail)] <- names(x)[(length(x) - 
                tail + 1):length(x)]
        }
        x <- temp
        se <- temp.se
    }
    if (missing(main)) {
        if (sd) {
            main <- "means + standard deviation"
        }
        else {
            main = "Confidence Intervals around the mean"
        }
    }
    labels <- names(x)
    if (sd) {
        ci <- se
    }
    else {
        ci <- qnorm((1 - alpha/2)) * se
    }
    if (!is.null(ci) && is.null(xlim)) 
        xlim <- c(min(x - ci), max(x + ci))
    labels <- substr(labels, 1, max.labels)
    if (eyes) {
        ln <- seq(-3, 3, 0.1)
        rev <- (length(ln):1)
    }
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if (is.matrix(x)) {
        if (is.null(labels)) 
            labels <- rownames(x)
        if (is.null(labels)) 
            labels <- as.character(1L:nrow(x))
        labels <- rep_len(labels, n)
        if (is.null(groups)) 
            groups <- col(x, as.factor = TRUE)
        glabels <- levels(groups)
    }
    else {
        if (is.null(labels)) 
            labels <- names(x)
        glabels <- if (!is.null(groups)) 
            levels(groups)
        if (!is.vector(x)) {
            warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
            x <- as.numeric(x)
        }
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- o
        ylim <- c(0, n + 1)
    }
    else {
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
    if (!is.null(ci)) {
        if (!eyes) {
            segments(x - ci, y, x + ci, y, col = par("fg"), lty = par("lty"), 
                lwd = par("lwd"))
        }
    }
    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    if (eyes) {
        for (e in 1:(min(head + tail, n.var))) {
            catseye(x[e], y[e], ci[e]/qnorm(1 - alpha/2), alpha = alpha, 
                density = density)
        }
    }
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
    if (!is.null(group)) 
        result <- des
}


fa.parallel.poly <- function (x, n.iter = 10, SMC = TRUE, fm = "minres", correct = TRUE, 
    sim = FALSE, fa = "both", global = TRUE) 
{
    p <- 0.05
    cl <- match.call()
    .Deprecated("fa.parallel.poly", msg = "fa.parallel.poly is deprecated.  Please use the fa.parallel function with the cor='poly' option.")
    n.obs <- dim(x)[1]
    tx <- table(as.matrix(x))
    nx <- dim(tx)[1]
    if (dim(tx)[1] == 2) {
        tet <- tetrachoric(x, correct = correct)
        typ = "tet"
        if (sim) {
            tx.item <- matrix(apply(x, 2, table), ncol = ncol(x))
            px <- matrix(tx.item/colSums(tx.item), ncol = 2, 
                byrow = TRUE)
        }
    }
    else {
        tet <- mixed.cor(x, global = global)
        typ = "poly"
    }
    cat("\n")
    flush(stdout())
    r <- tet$rho
    f <- fa(r, n.obs = n.obs, SMC = SMC, covar = FALSE, fm = fm)
    f$Call <- cl
    fl <- f$loadings
    nvar <- dim(fl)[1]
    e.values <- list(pc = list(), fa = list())
    replicates <- list()
    rep.rots <- list()
    for (trials in 1:n.iter) {
        progressBar(trials, n.iter, "fa.parallel.poly")
        bad <- TRUE
        while (bad) {
            xs <- matrix(apply(x, 2, function(y) sample(y, n.obs, 
                replace = TRUE)), ncol = nvar)
            tets <- polychoric(xs, progress = FALSE, global = global)
            r <- tets$rho
            bad <- any(is.na(r))
        }
        values.samp <- eigen(tets$rho)$values
        e.values[["pc"]][[trials]] <- values.samp
        fs <- fa(r, n.obs = n.obs, SMC = SMC, covar = FALSE, 
            fm = fm)
        e.values[["fa"]][[trials]] <- fs$values
        if (sim && (typ == "tet")) {
            xsim <- matrix(apply(px, 1, function(x) sample(2, 
                n.obs, TRUE, prob = x)), ncol = nvar)
            rho.sim <- tetrachoric(xsim, correct = correct)
            values.sim <- eigen(rho.sim$rho)$values
            e.values[["pc.sim"]][[trials]] <- values.sim
            fsim <- fa(rho.sim$rho, n.obs = n.obs, SMC = SMC, 
                covar = FALSE, fm = fm)
            e.values[["fa.sim"]][[trials]] <- fsim$values
        }
    }
    cat("\n")
    ei.pc <- describe(matrix(unlist(e.values$pc), ncol = nvar, 
        byrow = TRUE))
    ei.fa <- describe(matrix(unlist(e.values$fa), ncol = nvar, 
        byrow = TRUE))
    fa.test <- which(!(f$values > ei.fa$mean))[1] - 1
    pc.test <- which(!(f$e.values > ei.pc$mean))[1] - 1
    if (sim && (typ == "tet")) {
        eis.pc <- describe(matrix(unlist(e.values$pc.sim), ncol = nvar, 
            byrow = TRUE))
        eis.fa <- describe(matrix(unlist(e.values$fa.sim), ncol = nvar, 
            byrow = TRUE))
    }
    else {
        eis.pc <- NULL
        eis.fa <- NULL
    }
    results <- list(rho = tet$rho, tau = tet$tau, n.obs = n.obs, 
        Call = cl, fa.values = f$values, pc.values = f$e.values, 
        pc.sim = ei.pc, fa.sim = ei.fa, pcs.sim = eis.pc, fas.sim = eis.fa, 
        nfact = fa.test, ncomp = pc.test)
    class(results) <- c("psych", "parallel")
    cat("\n See the graphic output for a description of the results\n")
    plot.poly.parallel(results, fa = fa)
    return(results)
}


make.hierarchical <- function (gload = NULL, fload = NULL, n = 0, raw = FALSE) 
{
    if (is.null(gload)) 
        gload = matrix(c(0.9, 0.8, 0.7), nrow = 3)
    if (is.null(fload)) {
        fload <- matrix(c(0.8, 0, 0, 0.7, 0, 0, 0.6, 0, 0, 0, 
            0.7, 0, 0, 0.6, 0, 0, 0.5, 0, 0, 0, 0.6, 0, 0, 0.5, 
            0, 0, 0.4), ncol = 3, byrow = TRUE)
    }
    fcor <- gload %*% t(gload)
    diag(fcor) <- 1
    model <- fload %*% fcor %*% t(fload)
    diag(model) <- 1
    nvar <- dim(fload)[1]
    colnames(model) <- rownames(model) <- paste("V", 1:nvar, 
        sep = "")
    if (n > 0) {
        eX <- eigen(model)
        model <- matrix(rnorm(nvar * n), n)
        model <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 0)), 
            nvar) %*% t(model))
        if (!raw) {
            model <- cor(model)
        }
    }
    make.hierarchical <- model
}


read.clipboard.lower <- function (diag = TRUE, names = FALSE, ...) 
{
    MAC <- Sys.info()[1] == "Darwin"
    if (!MAC) {
        con <- file("clipboard")
    }
    else {
        con <- pipe("pbpaste")
    }
    xij <- scan(con, what = "char")
    close(con)
    m <- length(xij)
    d <- if (diag | names) 
        1
    else -1
    n <- floor((sqrt(1 + 8 * m) - d)/2)
    if (names) {
        name <- xij[cumsum(1:n)]
        xij <- xij[-cumsum(seq(1:n))]
        d <- if (diag) 
            1
        else -1
        n <- floor((sqrt(1 + 8 * (m - n)) - d)/2)
    }
    xij <- as.numeric(xij)
    X <- diag(n)
    X[upper.tri(X, diag = diag)] <- xij
    diagonal <- diag(X)
    X <- t(X) + X
    diag(X) <- diagonal
    if (!names) 
        name <- paste("V", 1:n, sep = "")
    if (!names) 
        name <- paste("V", 1:n, sep = "")
    if (names && !diag) {
        rownames(X) <- colnames(X) <- c(name, paste("V", n, sep = ""))
    }
    else {
        rownames(X) <- colnames(X) <- name
    }
    return(X)
}


sim.circ <- function (nvar = 72, nsub = 500, circum = TRUE, xloading = 0.6, 
    yloading = 0.6, gloading = 0, xbias = 0, ybias = 0, categorical = FALSE, 
    low = -3, high = 3, truncate = FALSE, cutpoint = 0) 
{
    avloading <- (xloading + yloading)/2
    errorweight <- sqrt(1 - (avloading^2 + gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) * xloading + xbias
    truey <- rnorm(nsub) * yloading + ybias
    if (circum) {
        radia <- seq(0, 2 * pi, len = nvar + 1)
        rad <- radia[which(radia < 2 * pi)]
    }
    else rad <- c(rep(0, nvar/4), rep(pi/2, nvar/4), rep(pi, 
        nvar/4), rep(3 * pi/2, nvar/4))
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    trueitem <- outer(truex, cos(rad)) + outer(truey, sin(rad))
    item <- gloading * g + trueitem + errorweight * error
    if (categorical) {
        item = round(item)
        item[(item <= low)] <- low
        item[(item > high)] <- high
    }
    if (truncate) {
        item[item < cutpoint] <- 0
    }
    return(item)
}


omega <- function (m, nfactors = 3, fm = "minres", n.iter = 1, p = 0.05, 
    poly = FALSE, key = NULL, flip = TRUE, digits = 2, title = "Omega", 
    sl = TRUE, labels = NULL, plot = TRUE, n.obs = NA, rotate = "oblimin", 
    Phi = NULL, option = "equal", covar = FALSE, ...) 
{
    cl <- match.call()
    if (is.data.frame(m) || is.matrix(m)) {
        if (isCorrelation(m)) {
            if (is.na(n.obs) && (n.iter > 1)) 
                stop("You must specify the number of subjects if giving a correlation matrix")
        }
    }
    if (!is.data.frame(m) && !is.matrix(m)) {
        n.obs = m$n.obs
        if (poly) {
            pol <- list(rho = m$rho, tau = m$tau, n.obs = m$n.obs)
            m <- m$rho
        }
        else {
            m <- m$R
        }
    }
    else {
        if (poly) {
            pol <- polychoric(m)
            m <- pol$rho
            n.obs <- pol$n.obs
        }
    }
    om <- omegah(m = m, nfactors = nfactors, fm = fm, key = key, 
        flip = flip, digits = digits, title = title, sl = sl, 
        labels = labels, plot = plot, n.obs = n.obs, rotate = rotate, 
        Phi = Phi, option = option, covar = covar, ...)
    if (is.na(n.obs)) {
        n.obs <- om$stats$n.obs
    }
    replicates <- list()
    if (n.iter > 1) {
        for (trials in 1:n.iter) {
            if (dim(m)[1] == dim(m)[2]) {
                nvar <- dim(m)[1]
                eX <- eigen(m)
                m <- matrix(rnorm(nvar * n.obs), n.obs)
                m <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
                  0)), nvar) %*% t(m))
            }
            else {
                m <- m[sample(n.obs, n.obs, replace = TRUE), 
                  ]
            }
            if (poly) {
                pol <- polychoric(m)
                oms <- omegah(m = pol$rho, nfactors = nfactors, 
                  fm = fm, key = key, flip = flip, digits = digits, 
                  title = title, sl = sl, labels = labels, plot = plot, 
                  n.obs = pol$n.obs, rotate = rotate, Phi = Phi, 
                  option = option, ...)
            }
            else {
                oms <- omegah(m = m, nfactors = nfactors, fm = fm, 
                  key = key, flip = flip, digits = digits, title = title, 
                  sl = sl, labels = labels, plot = plot, n.obs = n.obs, 
                  rotate = rotate, Phi = Phi, option = option, 
                  ...)
            }
            replicates[[trials]] <- list(omega = oms$omega_h, 
                alpha = oms$alpha, omega.tot = oms$omega.tot, 
                G6 = oms$G6, omega.lim = oms$omega.lim)
        }
        replicates <- matrix(unlist(replicates), ncol = 5, byrow = TRUE)
        z.replicates <- cbind(fisherz(replicates[, 1:4]), replicates[, 
            5])
        means <- colMeans(z.replicates, na.rm = TRUE)
        sds <- apply(z.replicates, 2, sd, na.rm = TRUE)
        ci.lower <- means + qnorm(p/2) * sds
        ci.upper <- means + qnorm(1 - p/2) * sds
        ci <- data.frame(lower = ci.lower, upper = ci.upper)
        ci <- rbind(fisherz2r(ci[1:4, ]), ci[5, ])
        rownames(ci) <- c("omega_h", "alpha", "omega_tot", "G6", 
            "omega_lim")
        colnames(replicates) <- names(means) <- names(sds) <- rownames(ci)
        conf <- list(means = means, sds = sds, ci = ci, Call = cl, 
            replicates = replicates)
        om$Call = cl
        results <- list(om = om, ci = conf)
    }
    else {
        om$call = cl
        if (poly) {
            om$rho <- pol$rho
            om$tau <- pol$tau
            om$n.obs <- pol$n.obs
        }
        results <- om
    }
    class(results) <- c("psych", "omega")
    return(results)
}


shannon <- function (x, correct = FALSE, base = 2) 
{
    if (is.null(dim(x))) {
        t <- table(x)
        s <- sum(t)
        p <- t/s
        H <- -sum(p * log(p, base))
        if (correct) {
            Hmax <- -log(1/length(p), base)
            H <- H/Hmax
        }
    }
    else {
        H <- apply(x, 2, function(x) shannon(x, correct = correct, 
            base = base))
    }
    return(H)
}


headtail <- function (x, hlength = 4, tlength = 4, digits = 2, ellipsis = TRUE) 
{
    .Deprecated("headTail", msg = "headtail is deprecated.  Please use the headTail function")
    if (is.data.frame(x) | is.matrix(x)) {
        if (is.matrix(x)) 
            x <- data.frame(unclass(x))
        nvar <- dim(x)[2]
        dots <- rep("...", nvar)
        h <- data.frame(head(x, hlength))
        t <- data.frame(tail(x, tlength))
        for (i in 1:nvar) {
            if (is.numeric(h[1, i])) {
                h[i] <- round(h[i], digits)
                t[i] <- round(t[i], digits)
            }
            else {
                dots[i] <- NA
            }
        }
        if (ellipsis) {
            head.tail <- rbind(h, "..." = dots, t)
        }
        else {
            head.tail <- rbind(h, t)
        }
    }
    else {
        h <- head(x, hlength)
        t <- tail(x, tlength)
        if (ellipsis) {
            head.tail <- rbind(h, "...       ...", t)
        }
        else {
            head.tail <- rbind(h, t)
            head.tail <- as.matrix(head.tail)
        }
    }
    return(head.tail)
}


scoreIrt.2pl <- function (itemLists, items, correct = 0.5, messages = FALSE, 
    cut = 0.3, bounds = c(-4, 4), mod = "logistic") 
{
    nvar <- length(itemLists)
    select <- sub("-", "", unlist(itemLists))
    select <- select[!duplicated(select)]
    items <- items[select]
    smallFunction <- function(i, selection, correct, cut = cut, 
        bounds = bounds, mod = mod) {
        direction <- rep(1, length(selection[[i]]))
        neg <- grep("-", selection[[i]])
        direction[neg] <- -1
        select <- sub("-", "", selection[[i]])
        selectedItems <- as.matrix(items[select])
        if (!messages) {
            suppressMessages(stats <- irt.fa(selectedItems, correct = correct, 
                plot = FALSE, sort = FALSE))
        }
        else {
            stats <- irt.fa(selectedItems, correct = correct, 
                plot = FALSE, sort = FALSE)
        }
        flip <- sum(sign(stats$irt$discrimination * direction))
        if (flip < 0) 
            stats$irt$discrimination <- -stats$irt$discrimination
        scores <- scoreIrt(stats, selectedItems, cut = cut, bounds = bounds, 
            mod = mod)
        scores <- scores$theta
    }
    scoresList <- mcmapply(smallFunction, c(1:nvar), MoreArgs = list(selection = itemLists, 
        correct = correct, cut = cut, bounds = bounds, mod = mod))
    colnames(scoresList) <- names(itemLists)
    return(scoresList)
}


make.keys <- function (nvars, keys.list, item.labels = NULL, key.labels = NULL) 
{
    if (!is.null(ncol(nvars))) {
        item.labels <- colnames(nvars)
        nvars <- ncol(nvars)
    }
    else {
        if (!is.numeric(nvars)) {
            item.labels <- nvars
            nvars <- length(item.labels)
        }
    }
    nkeys <- length(keys.list)
    keys <- matrix(rep(0, nvars * nkeys), ncol = nkeys)
    for (i in 1:nkeys) {
        list.i <- unlist(keys.list[[i]])
        if ((is.character(list.i)) && !is.null(item.labels)) {
            neg <- grep("-", list.i)
            list.i <- sub("-", "", list.i)
            list.i <- match(list.i, item.labels)
            if (!any(is.na(neg))) 
                list.i[neg] <- -list.i[neg]
        }
        keys[abs(list.i), i] <- sign(list.i)
    }
    if (!is.null(key.labels)) {
        colnames(keys) <- key.labels
    }
    else {
        colnames(keys) <- names(keys.list)
    }
    if (!is.null(item.labels)) {
        rownames(keys) <- item.labels
    }
    return(keys)
}


YuleBonett <- function (x, c = 1, bonett = FALSE, alpha = 0.05) 
{
    stopifnot(prod(dim(x)) == 4 || length(x) == 4)
    if (is.vector(x)) {
        x <- matrix(x, 2)
    }
    p <- x/sum(x)
    C <- c
    a <- p[1, 1]
    b <- p[1, 2]
    c <- p[2, 1]
    d <- p[2, 2]
    Rs <- rowSums(p)
    Cs <- colSums(p)
    if (bonett) {
        C <- 0.5 - (0.5 - min(Rs, Cs))^2
    }
    ad <- (a * d)^C
    bc <- (b * c)^C
    Yule <- (ad - bc)/(ad + bc)
    Ystar <- w <- (x[1, 1] + 0.1) * (x[2, 2] + 0.1)/((x[2, 1] + 
        0.1) * (x[1, 2] + 0.1))
    Ystar <- (w^C - 1)/(w^C + 1)
    vlQ <- (1/(x[1, 1] + 0.1) + 1/(x[2, 2] + 0.1) + 1/(x[2, 1] + 
        0.1) + 1/(x[1, 2] + 0.1))
    vQ <- (C^2/4) * (1 - Ystar^2)^2 * vlQ
    tanhinv <- atanh(Ystar)
    upper <- tanh(atanh(Ystar) + qnorm(1 - alpha/2) * sqrt((C^2/4) * 
        vlQ))
    lower <- tanh(atanh(Ystar) - qnorm(1 - alpha/2) * sqrt((C^2/4) * 
        vlQ))
    result <- list(rho = Ystar, se = sqrt(vQ), upper = upper, 
        lower = lower)
    class(result) <- c("psych", "Yule")
    return(result)
}


kaiser <- function (f, rotate = "oblimin", m = 4, pro.m = 4) 
{
    if ((!is.matrix(f)) && (!is.data.frame(f))) {
        f <- as.matrix(f$loadings)
    }
    else {
        f <- as.matrix(f)
    }
    if (!requireNamespace("GPArotation")) 
        stop("GPArotation is required for the Kaiser normalization")
    h2 <- diag(f %*% t(f))
    weighted <- f/sqrt(h2)
    if (rotate == "Promax") {
        rotated <- Promax(weighted, m = pro.m)
    }
    else {
        rotated <- do.call(getFromNamespace(rotate, "GPArotation"), 
            list(weighted))
    }
    normalized <- rotated$loadings * sqrt(h2)
    rotated$loadings <- normalized
    class(rotated) <- c("psych", "fa")
    return(rotated)
}


varimin <- function (L, Tmat = diag(ncol(L)), normalize = FALSE, eps = 1e-05, 
    maxit = 1000) 
{
    if (requireNamespace("GPArotation")) {
        GPArotation::GPForth(A = L, Tmat = diag(ncol(L)), normalize = normalize, 
            eps = eps, maxit = maxit, method = "varimin")
    }
    else {
        stop("biquartimin requires GPArotation")
    }
}


phi.list <- function (nf, f.list, f.labels = NULL) 
{
    nkeys <- length(f.list)
    phi <- diag(1, nf, nf)
    for (i in 1:nkeys) {
        list.i <- unlist(f.list[[i]])
        phi[list.i, i] <- paste("r", letters[i], letters[list.i], 
            sep = "")
    }
    if (!is.null(f.labels)) {
        colnames(phi) <- f.labels
    }
    else {
        colnames(phi) <- paste("F", 1:nf, sep = "")
    }
    rownames(phi) <- colnames(phi)
    return(phi)
}


biquartimin <- function (L, Tmat = diag(ncol(L)), normalize = FALSE, eps = 1e-05, 
    maxit = 1000) 
{
    if (requireNamespace("GPArotation")) {
        GPArotation::GPFoblq(L, Tmat = Tmat, normalize = normalize, 
            eps = eps, maxit = maxit, method = "bimin")
    }
    else {
        stop("biquartimin requires GPArotation")
    }
}


setCor.diagram <- function (sc, main = "Regression model", digits = 2, show = TRUE, 
    ...) 
{
    beta <- round(sc$beta, digits)
    x.matrix <- round(sc$x.matrix, digits)
    y.matrix <- round(sc$y.matrix, digits)
    x.names <- rownames(sc$beta)
    y.names <- colnames(sc$beta)
    nx <- length(x.names)
    ny <- length(y.names)
    top <- max(nx, ny)
    xlim = c(-nx/3, 10)
    ylim = c(0, top)
    top <- max(nx, ny)
    x <- list()
    y <- list()
    x.scale <- top/(nx + 1)
    y.scale <- top/(ny + 1)
    plot(NA, xlim = xlim, ylim = ylim, main = main, axes = FALSE, 
        xlab = "", ylab = "")
    for (i in 1:nx) {
        x[[i]] <- dia.rect(3, top - i * x.scale, x.names[i])
    }
    for (j in 1:ny) {
        y[[j]] <- dia.rect(7, top - j * y.scale, y.names[j])
    }
    for (i in 1:nx) {
        for (j in 1:ny) {
            dia.arrow(x[[i]]$right, y[[j]]$left, labels = beta[i, 
                j], adj = 4 - j)
        }
    }
    if (nx > 1) {
        for (i in 2:nx) {
            for (k in 1:(i - 1)) {
                dia.curved.arrow(x[[i]]$left, x[[k]]$left, x.matrix[i, 
                  k], scale = -(abs(i - k)), both = TRUE)
            }
        }
    }
    if (ny > 1) {
        for (i in 2:ny) {
            for (k in 1:(i - 1)) {
                dia.curved.arrow(y[[i]]$right, y[[k]]$right, 
                  y.matrix[i, k], scale = (abs(i - k)))
            }
        }
    }
    for (i in 1:ny) {
        dia.self(y[[i]], side = 3, scale = 0.2)
    }
    if (show) {
        text((10 - nx/3)/2, 0, paste("unweighted matrix correlation  = ", 
            round(sc$Ruw, digits)))
    }
}


circadian.mean <- function (angle, data = NULL, hours = TRUE, na.rm = TRUE) 
{
    if (!is.null(data)) 
        angle <- data[, angle]
    if (hours) {
        angle <- angle * 2 * pi/24
    }
    x <- cos(angle)
    y <- sin(angle)
    if (is.vector(angle)) {
        mx <- mean(x, na.rm = na.rm)
        my <- mean(y, na.rm = na.rm)
    }
    else {
        mx <- colMeans(x, na.rm = na.rm)
        my <- colMeans(y, na.rm = na.rm)
    }
    mean.angle <- sign(my) * acos((mx)/sqrt(mx^2 + my^2))
    if (hours) {
        mean.angle <- mean.angle * 24/(2 * pi)
        mean.angle[mean.angle <= 0] <- mean.angle[mean.angle <= 
            0] + 24
    }
    return(mean.angle)
}


dia.self <- function (location, labels = NULL, scale = 0.8, side = 2, ...) 
{
    n <- 20
    if (side%%2 > 0) {
        scale <- scale * (location$right[1] - location$left[1])
    }
    else {
        scale <- scale * (location$top[2] - location$bottom[2])
    }
    if (side == 1) {
        x <- c(location$bottom[1] - scale/2, location$bottom[1], 
            location$bottom[1] + scale/2)
        y <- c(location$bottom[2], location$bottom[2] - scale, 
            location$bottom[2])
        sp <- spline(x, y, n = 20)
        lines(sp$x, sp$y)
        arrows(sp$x[3], sp$y[3], sp$x[1], sp$y[1], length = 2 * 
            abs(sp$x[3] - sp$x[1]))
        arrows(sp$x[n - 3], sp$y[n - 3], sp$x[n], sp$y[n], length = 2 * 
            abs(sp$x[3] - sp$x[1]))
        text(sp$x[n/2], sp$y[n/2] - scale, labels, ...)
    }
    if (side == 2) {
        x <- c(location$left[1], location$left[1] - scale, location$left[1])
        y <- c(location$left[2] - scale/2, location$left[2], 
            location$left[2] + scale/2)
        sp <- spline(y, x, n = 20)
        lines(sp$y, sp$x)
        arrows(sp$y[3], sp$x[3], sp$y[1], sp$x[1], length = 2 * 
            abs(sp$x[2] - sp$x[1]))
        arrows(sp$y[n - 3], sp$x[n - 3], sp$y[n], sp$x[n], length = 2 * 
            abs(sp$x[2] - sp$x[1]))
        text(sp$y[n/2] - scale, sp$x[n/2], labels, ...)
    }
    if (side == 3) {
        x <- c(location$top[1] - scale/2, location$top[1], location$top[1] + 
            scale/2)
        y <- c(location$top[2], location$top[2] + scale, location$top[2])
        sp <- spline(x, y, n = 20)
        lines(sp$x, sp$y)
        arrows(sp$x[3], sp$y[3], sp$x[1], sp$y[1], length = 2 * 
            abs(sp$x[3] - sp$x[1]))
        arrows(sp$x[n - 3], sp$y[n - 3], sp$x[n], sp$y[n], length = 2 * 
            abs(sp$x[3] - sp$x[1]))
        text(sp$x[n/2], sp$y[n/2] + scale, labels, ...)
    }
    if (side == 4) {
        x <- c(location$right[1], location$right[1] + scale, 
            location$right[1])
        y <- c(location$right[2] - scale/2, location$right[2], 
            location$right[2] + scale/2)
        sp <- spline(y, x, n = 20)
        lines(sp$y, sp$x)
        arrows(sp$y[3], sp$x[3], sp$y[1], sp$x[1], length = 2 * 
            abs(sp$x[3] - sp$x[1]))
        arrows(sp$y[n - 3], sp$x[n - 3], sp$y[n], sp$x[n], length = 2 * 
            abs(sp$x[3] - sp$x[1]))
        text(sp$y[n/2] + scale, sp$x[n/2], labels, ...)
    }
}


fa.stats <- function (r = NULL, f, phi = NULL, n.obs = NA, np.obs = NULL, 
    alpha = 0.1, fm = NULL) 
{
    cl <- match.call()
    conf.level <- alpha
    if ((!is.matrix(f)) && (!is.data.frame(f))) {
        if (is.null(r) && (!is.null(f$r))) 
            r <- f$r
        f <- as.matrix(f$loadings)
    }
    else {
        f <- as.matrix(f)
    }
    n <- dim(r)[2]
    if (dim(r)[1] != n) {
        n.obs = dim(r)[1]
        r <- cor(r, use = "pairwise")
    }
    if (is.data.frame(r)) 
        r <- as.matrix(r)
    nfactors <- dim(f)[2]
    if (is.null(phi)) {
        model <- f %*% t(f)
    }
    else {
        model <- f %*% phi %*% t(f)
    }
    residual <- r - model
    r2 <- sum(r * r)
    rstar2 <- sum(residual * residual)
    result <- list(residual = residual)
    result$dof <- dof <- n * (n - 1)/2 - n * nfactors + (nfactors * 
        (nfactors - 1)/2)
    r2.off <- r2 - tr(r)
    diag(residual) <- 0
    if (is.null(np.obs)) {
        rstar.off <- sum(residual^2)
        result$ENull <- r2.off * n.obs
        result$chi <- rstar.off * n.obs
        result$rms <- sqrt(rstar.off/(n * (n - 1)))
        result$nh <- n.obs
        if (result$dof > 0) {
            result$EPVAL <- pchisq(result$chi, result$dof, lower.tail = FALSE)
            result$crms <- sqrt(rstar.off/(2 * result$dof))
            result$EBIC <- result$chi - result$dof * log(n.obs)
            result$ESABIC <- result$chi - result$dof * log((n.obs + 
                2)/24)
        }
        else {
            result$EPVAL <- NA
            result$crms <- NA
            result$EBIC <- NA
            result$ESABIC <- NA
        }
    }
    else {
        rstar.off <- sum(residual^2 * np.obs)
        r2.off <- (r * r * np.obs)
        r2.off <- sum(r2.off) - tr(r2.off)
        result$chi <- rstar.off
        result$nh <- harmonic.mean(as.vector(np.obs))
        result$rms <- sqrt(rstar.off/(result$nh * n * (n - 1)))
        if (result$dof > 0) {
            result$EPVAL <- pchisq(result$chi, result$dof, lower.tail = FALSE)
            result$crms <- sqrt(rstar.off/(2 * result$nh * result$dof))
            result$EBIC <- result$chi - result$dof * log(result$nh)
            result$ESABIC <- result$chi - result$dof * log((result$nh + 
                2)/24)
        }
        else {
            result$EPVAL <- NA
            result$crms <- NA
            result$EBIC <- NA
            result$ESABIC <- NA
        }
    }
    result$fit <- 1 - rstar2/r2
    result$fit.off <- 1 - rstar.off/r2.off
    result$sd <- sd(as.vector(residual))
    result$factors <- nfactors
    result$complexity <- (apply(f, 1, function(x) sum(x^2)))^2/apply(f, 
        1, function(x) sum(x^4))
    diag(model) <- diag(r)
    model <- cor.smooth(model)
    r <- cor.smooth(r)
    m.inv.r <- try(solve(model, r), silent = TRUE)
    if (class(m.inv.r) == "try-error") {
        warning("the model inverse times the r matrix is singular, replaced with Identity matrix which means fits are wrong")
        m.inv.r <- diag(1, n, n)
    }
    if (is.na(n.obs)) {
        result$n.obs = NA
        result$PVAL = NA
    }
    else {
        result$n.obs = n.obs
    }
    result$dof <- n * (n - 1)/2 - n * nfactors + (nfactors * 
        (nfactors - 1)/2)
    result$objective <- sum(diag((m.inv.r))) - log(det(m.inv.r)) - 
        n
    if (is.infinite(result$objective)) {
        result$objective <- rstar2
        message("The determinant of the smoothed correlation was zero.\nThis means the objective function is not defined.\nChi square is based upon observed residuals.")
    }
    result$criteria <- c(objective = result$objective, NA, NA)
    if (!is.na(n.obs)) {
        result$STATISTIC <- chisq <- result$objective * ((n.obs - 
            1) - (2 * n + 5)/6 - (2 * nfactors)/3)
        if (!is.nan(result$STATISTIC)) 
            if (result$STATISTIC < 0) {
                result$STATISTIC <- 0
            }
        if (result$dof > 0) {
            result$PVAL <- pchisq(result$STATISTIC, result$dof, 
                lower.tail = FALSE)
        }
        else {
            result$PVAL <- NA
        }
    }
    result$Call <- cl
    F0 <- sum(diag((r))) - log(det(r)) - n
    if (is.infinite(F0)) {
        F0 <- r2
        message("The determinant of the smoothed correlation was zero.\nThis means the objective function is not defined for the null model either.\nThe Chi square is thus based upon observed correlations.")
    }
    Fm <- result$objective
    Mm <- Fm/(n * (n - 1)/2 - n * nfactors + (nfactors * (nfactors - 
        1)/2))
    M0 <- F0 * 2/(n * (n - 1))
    nm <- ((n.obs - 1) - (2 * n + 5)/6 - (2 * nfactors)/3)
    result$null.model <- F0
    result$null.dof <- n * (n - 1)/2
    if (!is.na(n.obs)) {
        result$null.chisq <- F0 * ((n.obs - 1) - (2 * n + 5)/6)
        result$TLI <- (M0 - Mm)/(M0 - 1/nm)
        if (is.numeric(result$TLI) & !is.nan(result$TLI) & (result$TLI > 
            1)) 
            result$F0 <- 1
        if (!is.null(result$objective) && (result$dof > 0) && 
            (!is.na(result$objective))) {
            RMSEA <- max((chisq/(result$dof * (n.obs - 1)) - 
                1/(n.obs - 1)), 0)
            tail <- conf.level/2
            N <- max <- n.obs
            df <- result$dof
            chi.sq.statistic <- chisq
            max <- max(max, chi.sq.statistic) + 2 * max
            while (max > 1) {
                res <- try(optimize(function(lam) (tail - pchisq(chi.sq.statistic, 
                  df, ncp = lam))^2, interval = c(0, max)), silent = TRUE)
                if (class(res) == "try-error") {
                  message("In factor.stats, I could not find the RMSEA upper bound . Sorry about that")
                  res <- NULL
                }
                if (is.null(res) || is.na(res$objective) || res$objective < 
                  0) {
                  max <- 0
                  warning("cannot find upper bound of RMSEA")
                  break
                }
                if (sqrt(res$objective) < tail/100) 
                  break
                max <- max/2
            }
            lam.U <- if (max <= 1) 
                NA
            else res$minimum
            max <- lam.U
            if (is.na(max)) 
                max <- N
            while (max > 1) {
                res <- try(optimize(function(lam) (1 - tail - 
                  pchisq(chi.sq.statistic, df, ncp = lam))^2, 
                  interval = c(0, max)), silent = TRUE)
                if (class(res) == "try-error") {
                  message("In factor.stats, I could not find the RMSEA lower bound. Sorry about that")
                  res <- NULL
                }
                if (is.null(res)) {
                  break
                }
                if (sqrt(res$objective) < tail/100) 
                  break
                max <- max/2
                if (is.na(res$objective) || res$objective < 0) {
                  max <- 0
                  warning("cannot find lower bound of RMSEA")
                  break
                }
            }
            lam.L <- if (max <= 1) 
                NA
            else res$minimum
            RMSEA.U <- sqrt(lam.U/((N) * df))
            RMSEA.L <- min(sqrt(lam.L/((N) * df)), RMSEA)
            if (!is.na(RMSEA.U) && RMSEA.U < RMSEA) 
                RMSEA.U <- NA
            if (!is.na(RMSEA.L) && RMSEA.L > RMSEA) 
                RMSEA.L <- NA
            result$RMSEA <- c(RMSEA, RMSEA.L, RMSEA.U, conf.level)
            names(result$RMSEA) <- c("RMSEA", "lower", "upper", 
                "confidence")
            result$BIC <- chisq - df * log(N)
            result$SABIC <- chisq - df * log((N + 2)/24)
        }
    }
    if (!is.null(phi)) 
        f <- f %*% phi
    r <- cor.smooth(r)
    w <- try(solve(r, f), silent = TRUE)
    if (class(w) == "try-error") {
        message("In factor.stats, the correlation matrix is singular, an approximation is used")
        ev <- eigen(r)
        if (is.complex(ev$values)) {
            warning("complex eigen values detected by factor stats, results are suspect")
        }
        else {
            ev$values[ev$values < .Machine$double.eps] <- 100 * 
                .Machine$double.eps
            r <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
            diag(r) <- 1
            w <- try(solve(r, f), silent = TRUE)
            if (class(w) == "try-error") {
                warning("In factor.stats, the correlation matrix is singular, and we could not calculate the beta weights for factor score estimates")
                w <- diag(1, dim(r)[1])
            }
        }
    }
    R2 <- diag(t(w) %*% f)
    if (is.null(fm)) {
        if (prod(R2) < 0) {
            message("In factor.stats: The factor scoring weights matrix is probably singular -- Factor score estimate results are likely incorrect.\n Try a different factor extraction method\n")
            R2[abs(R2) > 1] <- NA
            R2[R2 <= 0] <- NA
        }
        if ((max(R2, na.rm = TRUE) > (1 + .Machine$double.eps))) {
            message("The estimated weights for the factor scores are probably incorrect.  Try a different factor extraction method.")
        }
    }
    r.scores <- cov2cor(t(w) %*% r %*% w)
    result$r.scores <- r.scores
    result$R2 <- R2
    keys <- factor2cluster(f)
    covar <- t(keys) %*% r %*% keys
    if ((nfactors > 1) && (dim(covar)[2] > 1)) {
        sd.inv <- diag(1/sqrt(diag(covar)))
        cluster.correl <- sd.inv %*% covar %*% sd.inv
        valid <- t(f) %*% keys %*% sd.inv
        result$valid <- diag(valid)
        result$score.cor <- cluster.correl
    }
    else {
        sd.inv <- 1/sqrt(covar)
        if (dim(sd.inv)[1] == 1) 
            sd.inv <- diag(sd.inv)
        valid <- try(t(f) %*% keys * sd.inv)
        result$valid <- valid
    }
    result$weights <- w
    class(result) <- c("psych", "stats")
    return(result)
}


glb.algebraic <- function (Cov, LoBounds = NULL, UpBounds = NULL) 
{
    if (!requireNamespace("Rcsdp")) {
        stop("Rcsdp must be installed to find the glb.algebraic")
    }
    cl <- match.call()
    p <- dim(Cov)[2]
    if (dim(Cov)[1] != p) 
        Cov <- cov(Cov)
    if (any(t(Cov) != Cov)) 
        stop("'Cov' is not symmetric")
    if (is.null(LoBounds)) 
        LoBounds <- rep(0, ncol(Cov))
    if (is.null(UpBounds)) 
        UpBounds <- diag(Cov)
    if (any(LoBounds > UpBounds)) {
        stop("'LoBounds'<='UpBounds' violated")
    }
    if (length(LoBounds) != p) 
        stop("length(LoBounds) != dim(Cov)")
    if (length(UpBounds) != p) 
        stop("length(UpBounds)!=dim(Cov)")
    Var <- diag(Cov)
    opt = rep(1, p)
    C <- list(diag(Var) - Cov, -UpBounds, LoBounds)
    A <- vector("list", p)
    for (i in 1:p) {
        b <- rep(0, p)
        b[i] <- 1
        A[[i]] <- list(diag(b), -b, b)
    }
    K <- list(type = c("s", "l", "l"), size = rep(p, 3))
    result <- Rcsdp::csdp(C, A, opt, K)
    if (result$status >= 4 || result$status == 2) {
        warning("Failure of csdp, status of solution=", result$status)
        lb <- list(glb = NA, solution = NA, status = result$status, 
            Call = cl)
    }
    else {
        if (result$status != 0) {
            warning("status of solution=", result$status)
        }
        item.diag <- result$y
        names(item.diag) <- colnames(Cov)
        lb <- list(glb = (sum(Cov) - sum(Var) + sum(result$y))/sum(Cov), 
            solution = item.diag, status = result$status, Call = cl)
    }
    return(lb)
}


sim.parallel <- function (ntrials = 10, nvar = c(12, 24, 36, 48), nfact = c(1, 
    2, 3, 4, 6), n = c(200, 400)) 
{
    nvariables = nvar
    factors = nfact
    subjects = n
    result <- matrix(NaN, ncol = 7, nrow = ntrials * length(nvariables) * 
        length(subjects) * length(factors))
    k <- 1
    for (nfact in factors) {
        for (nvar in nvariables) {
            for (nsub in subjects) {
                for (trials in 1:ntrials) {
                  x <- sim.minor(nvar = nvar, nfact = nfact, 
                    n = nsub)$observed
                  fp <- fa.parallel(x)
                  fps <- fa.parallel(x, SMC = TRUE)
                  result[k, 1] <- nfact
                  result[k, 2] <- nvar
                  result[k, 3] <- trials
                  result[k, 4] <- fp$nfact
                  result[k, 5] <- fps$nfact
                  result[k, 6] <- fp$ncomp
                  result[k, 7] <- nsub
                  k <- k + 1
                }
            }
        }
    }
    colnames(result) <- c("factors", "nvar", "trials", "nfact", 
        "smc.fact", "ncomp", "nsub")
    return(result)
}


set.cor <- function (y, x, data, z = NULL, n.obs = NULL, use = "pairwise", 
    std = TRUE, square = FALSE, main = "Regression Models", plot = TRUE) 
{
    setCor(y = y, x = x, data = data, z = z, n.obs = n.obs, use = use, 
        std = std, square = square, main = main, plot = plot)
}


keys2list <- function (keys, sign = TRUE) 
{
    keys.list <- list()
    nkeys <- ncol(keys)
    for (i in 1:nkeys) {
        temp <- rownames(keys)[which(keys[, i] < 0)]
        if (sign && (length(temp) > 0)) 
            temp <- paste0("-", temp)
        keys.list[[i]] <- c(rownames(keys)[which(keys[, i] > 
            0)], temp)
    }
    names(keys.list) <- colnames(keys)
    keys.list
}


bi.bars <- function (x, grp, horiz, color, label = NULL, zero = FALSE, xlab, 
    ylab, ...) 
{
    if (missing(horiz)) 
        horiz <- TRUE
    if (missing(color)) 
        color <- c("blue", "red")
    if (horiz) {
        if (missing(xlab)) 
            xlab <- "Frequency"
        if (missing(ylab)) 
            ylab <- paste0(levels(grp))
    }
    else {
        if (missing(ylab)) 
            ylab <- "Frequency"
        if (missing(xlab)) 
            xlab <- paste0(levels(grp))
    }
    groups <- table(grp)
    max.val <- max(x, na.rm = TRUE)
    min.val <- min(x, na.rm = TRUE)
    gr1 <- (names(groups)[1])
    gr2 <- (names(groups)[2])
    g1 <- subset(x, grp == gr1)
    g2 <- subset(x, grp == gr2)
    t1 <- tabulate(g1 - min.val * zero, nbins = (max.val - min.val + 
        1))
    t2 <- tabulate(g2 - min.val * zero, nbins = (max.val - min.val + 
        1))
    m1 <- max(t1, t2)
    m2 <- max(t1, t2)
    xlim <- c(-m1, m2) * 1.04
    if (horiz) {
        xloc <- barplot(-t1, xlim = xlim, col = color[1], horiz = horiz, 
            xlab = xlab, ylab = ylab, axes = FALSE, axisnames = FALSE, 
            ...)
        barplot(t2, add = TRUE, col = color[2], horiz = horiz, 
            axes = FALSE, axisnames = FALSE, ...)
        box()
        if ((max.val - min.val) < 10) {
            if (is.null(label)) {
                axis(2, at = xloc + min.val * zero, labels = min.val:max.val, 
                  ...)
            }
            else {
                axis(2, at = xloc + min.val * zero, labels = label, 
                  las = 2, ...)
            }
        }
        else {
            at <- axTicks(2, usr = c(min.val, max.val))
            axis(2, at = at, labels = at + min.val * zero, las = 2, 
                ...)
        }
        atv <- axTicks(1)
        axis(1, at = atv, labels = abs(atv), ...)
    }
    else {
        ylim <- c(-m1, m2) * 1.04
        xloc <- barplot(-t1, ylim = ylim, col = color[1], horiz = horiz, 
            xlab = xlab, ylab = ylab, axes = FALSE, ...)
        barplot(t2, add = TRUE, col = color[2], horiz = horiz, 
            axes = FALSE, ...)
        box()
        atv <- axTicks(2)
        axis(2, at = atv, labels = abs(atv), las = 2, ...)
        if ((max.val - min.val) < 10) {
            if (is.null(label)) {
                axis(1, at = xloc, labels = min.val:max.val, 
                  ...)
            }
            else {
                axis(1, at = xloc, labels = label, ...)
            }
        }
        else {
            at <- axTicks(1, usr = c(min.val, max.val))
            axis(1, at = at, labels = at + min.val * zero, ...)
        }
    }
}


circ.sim.plot <- function (x.df) 
{
    with(x.df, {
        symb <- c(21, 22, 20)
        colors <- c("black", "blue", "red")
        op <- par(mfrow = c(2, 2))
        plot(c.gap, c.RT, xlim = c(0, 0.5), ylim = c(0, 1), pch = symb[1], 
            col = colors[1], xlab = "Gap Test", ylab = "Rotation Test", 
            main = "Gap x Rotation")
        points(s.gap, s.RT, pch = symb[2], col = colors[2])
        points(e.gap, e.RT, pch = symb[3], col = colors[3])
        plot(c.gap, c.fisher, xlim = c(0, 0.5), ylim = c(0, 0.5), 
            pch = symb[1], col = colors[1], xlab = "Gap Test", 
            ylab = "Fisher Test", main = "Gap x Fisher")
        points(s.gap, s.fisher, pch = symb[2], col = colors[2])
        points(e.gap, e.fisher, pch = symb[3], col = colors[3])
        plot(c.fisher, c.RT, xlim = c(0, 0.5), ylim = c(0, 1), 
            pch = symb[1], col = colors[1], xlab = "Fisher Test", 
            ylab = "Rotation Test", main = "Fisher x Rotation")
        points(s.gap, s.RT, pch = symb[2], col = colors[2])
        points(e.gap, e.RT, pch = symb[3], col = colors[3])
        boxplot(x.df, main = " Box Plot of all tests")
        title(main = "Circumplex Tests for Circumplex, Ellipsoid, and Simple Structure", 
            outer = TRUE, line = -1)
    })
    op <- par(mfrow = c(1, 1))
}


schmid <- function (model, nfactors = 3, fm = "minres", digits = 2, rotate = "oblimin", 
    n.obs = NA, option = "equal", Phi = NULL, covar = FALSE, 
    ...) 
{
    cl <- match.call()
    if (!requireNamespace("GPArotation")) {
        stop("I am sorry, you need to have the  GPArotation package installed")
    }
    if (is.list(model)) {
        if ((class(model)[1] == "psych") && (class(model)[2] == 
            "fa")) {
            Phi <- model$Phi
            model <- model$loadings
        }
        else {
            stop("input is a list, but is not from of class 'fa' ")
        }
    }
    if (is.null(Phi)) {
        normal.case <- TRUE
        nvar <- dim(model)[2]
        if (dim(model)[1] != dim(model)[2]) {
            n.obs <- dim(model)[1]
            if (covar) {
                model <- cov(model, use = "pairwise")
            }
            else {
                model <- cor(model, use = "pairwise")
            }
        }
        if (fm == "pc") {
            fact <- principal(model, nfactors, n.obs = n.obs, 
                ...)
            fm <- "minres"
            message("The higher order factor is found using minres -- see the notes")
        }
        else {
            if ((fm == "pa") | (fm == "minres") | (fm == "wls") | 
                (fm == "minres") | (fm == "ml") | (fm == "mle") | 
                (fm == "gls") | (fm == "minchi") | (fm == "minrank")) {
                fact <- fa(model, nfactors, n.obs = n.obs, rotate = "varimax", 
                  fm = fm, covar = covar)
            }
            else {
                stop("The method of factor extraction you specified is not available")
            }
        }
        orth.load <- loadings(fact)
    }
    else {
        model <- as.matrix(model)
        Phi <- as.matrix(Phi)
        fact <- model %*% Phi
        orth.load <- fact
        ev <- eigen(Phi)
        orth.load <- model %*% ev$vector %*% sqrt(diag(ev$values))
        colnames(orth.load) <- colnames(Phi)
        nfactors <- dim(fact)[2]
        normal.case <- FALSE
    }
    colnames(orth.load) <- paste("F", 1:nfactors, sep = "")
    if (nfactors == 1) {
        message("Omega_h for 1 factor is not meaningful, just omega_t")
        obminfact <- list(loadings = orth.load)
        factr <- 1
    }
    else {
        switch(rotate, simplimax = {
            obminfact <- GPArotation::simplimax(orth.load)
        }, promax = {
            pro <- kaiser(orth.load, rotate = "Promax", ...)
            obminfact <- pro
            rot.mat <- pro$rotmat
            Phi <- pro$Phi
        }, Promax = {
            obminfact <- Promax(orth.load)
            rotmat <- obminfact$rotmat
            Phi <- obminfact$Phi
        }, TargetQ = {
            obminfact <- do.call(rotate, list(orth.load, ...))
            loadings <- obminfact$loadings
            Phi <- obminfact$Phi
        }, cluster = {
            obminfact <- varimax(orth.load)
            obminfact <- target.rot(obminfact, ...)
            loadings <- obminfact$loadings
            Phi <- obminfact$Phi
        }, target = {
            obminfact <- varimax(orth.load)
            obminfact <- target.rot(obminfact, ...)
            loadings <- obminfact$loadings
            Phi <- obminfact$Phi
        }, oblimin = {
            obminfact <- try(GPArotation::oblimin(orth.load))
            if (class(obminfact) == as.character("try-error")) {
                obminfact <- Promax(orth.load)
                message("\nThe oblimin solution failed, Promax used instead.\n")
                rotmat <- obminfact$rotmat
                Phi <- obminfact$Phi
            }
        }, geominQ = {
            obminfact <- try(GPArotation::geominQ(orth.load))
            if (class(obminfact) == as.character("try-error")) {
                obminfact <- Promax(orth.load)
                message("\nThe geominQ solution failed, Promax used instead.\n")
                rotmat <- obminfact$rotmat
                Phi <- obminfact$Phi
            }
        }, bentlerQ = {
            obminfact <- try(GPArotation::bentlerQ(orth.load))
            if (class(obminfact) == as.character("try-error")) {
                obminfact <- Promax(orth.load)
                message("\nThe bentlerQ solution failed, Promax used instead.\n")
                rotmat <- obminfact$rotmat
                Phi <- obminfact$Phi
            }
        }, targetQ = {
            obminfact <- try(GPArotation::targetQ(orth.load, 
                ...))
            if (class(obminfact) == as.character("try-error")) {
                obminfact <- Promax(orth.load)
                message("\nThe targetQ solution failed, Promax used instead.\n")
                rotmat <- obminfact$rotmat
                Phi <- obminfact$Phi
            }
        }, biquartimin = {
            obminfact <- biquartimin(orth.load, ...)
            loadings <- obminfact$loadings
            Phi <- obminfact$Phi
            rot.mat <- t(solve(obminfact$Th))
        })
    }
    if (nfactors > 1) 
        rownames(obminfact$loadings) <- attr(model, "dimnames")[[1]]
    if (!normal.case) {
        fload <- model
        factr <- Phi
        model <- fload %*% Phi %*% t(fload)
        diag(model) <- 1
    }
    else {
        fload <- obminfact$loadings
        factr <- obminfact$Phi
    }
    if (nfactors == 1) {
        gload <- c(1)
        warning("Omega_h and Omega_asymptotic are not meaningful with one factor")
    }
    else {
        colnames(factr) <- rownames(factr) <- paste("F", 1:nfactors, 
            sep = "")
        if (nfactors > 2) {
            gfactor <- fa(factr, fm = fm)
            gload <- loadings(gfactor)
        }
        else {
            gload <- c(NA, NA)
            if (option == "equal") {
                gload[1] <- sqrt(abs(factr[1, 2]))
                gload[2] <- sign(factr[1, 2]) * sqrt(abs(factr[1, 
                  2]))
                message("\nThree factors are required for identification -- general factor loadings set to be equal. \nProceed with caution. \nThink about redoing the analysis with alternative values of the 'option' setting.\n")
            }
            else {
                if (option == "first") {
                  gload[1] <- 1
                  gload[2] <- (factr[1, 2])
                  message("\nThree factors are required for identification -- general factor loading set to be 1 for group factor 1. \nProceed with caution. \nThink about redoing the analysis with alternative values of the 'option' setting.\n")
                }
                else {
                  gload[2] <- 1
                  gload[1] <- (factr[1, 2])
                  message("\nThree factors are required for identification -- general factor loadings are set to be 1 for group factor 2.\nProceed with caution. \nThink about redoing the analysis with alternative values of the 'option' setting.\n")
                }
            }
        }
    }
    gprimaryload <- fload %*% gload
    colnames(gprimaryload) <- "g"
    h2 <- diag(orth.load %*% t(orth.load))
    u2 <- diag(model) - h2
    uniq <- diag(model) - fload^2
    guniq <- diag(model) - gprimaryload^2
    Ig <- diag(drop(gload))
    primeload <- fload %*% Ig
    g.percent <- gprimaryload^2/h2
    colnames(g.percent) <- "p2"
    uniq2 <- diag(model) - uniq - primeload^2
    uniq2[uniq2 < 0] <- 0
    sm <- sign(fload) * sqrt(uniq2)
    colnames(sm) <- paste("F", 1:nfactors, "*", sep = "")
    if (!is.null(Phi)) {
        result <- list(sl = cbind(gprimaryload, sm, h2, u2, p = g.percent), 
            orthog = orth.load, oblique = fload, phi = factr, 
            gloading = gload, Call = cl)
    }
    else {
        result <- list(sl = cbind(gprimaryload, sm, h2, u2, p = g.percent), 
            orthog = orth.load, oblique = fload, phi = factr, 
            gloading = gload, dof = fact$dof, objective = fact$criteria[1], 
            STATISTIC = fact$STATISTIC, PVAL = fact$PVAL, RMSEA = fact$RMSEA, 
            BIC = fact$BIC, rms = fact$rms, crms = fact$crms, 
            n.obs = n.obs, scores = fact$scores, Call = cl)
    }
    class(result) <- c("psych", "schmid")
    return(result)
}


corr.test <- function (x, y = NULL, use = "pairwise", method = "pearson", 
    adjust = "holm", alpha = 0.05, ci = TRUE) 
{
    cl <- match.call()
    if (is.null(y)) {
        r <- cor(x, use = use, method = method)
        sym <- TRUE
        n <- t(!is.na(x)) %*% (!is.na(x))
    }
    else {
        r <- cor(x, y, use = use, method = method)
        sym = FALSE
        n <- t(!is.na(x)) %*% (!is.na(y))
    }
    if ((use == "complete") | (min(n) == max(n))) 
        n <- min(n)
    t <- (r * sqrt(n - 2))/sqrt(1 - r^2)
    p <- 2 * (1 - pt(abs(t), (n - 2)))
    se <- sqrt((1 - r * r)/(n - 2))
    nvar <- ncol(r)
    p[p > 1] <- 1
    if (adjust != "none") {
        if (is.null(y)) {
            lp <- upper.tri(p)
            pa <- p[lp]
            pa <- p.adjust(pa, adjust)
            p[upper.tri(p, diag = FALSE)] <- pa
        }
        else {
            p[] <- p.adjust(p, adjust)
        }
    }
    z <- fisherz(r[lower.tri(r)])
    if (ci) {
        if (min(n) < 4) {
            warning("Number of subjects must be greater than 3 to find confidence intervals.")
        }
        alpha <- 1 - alpha/2
        dif <- qnorm(alpha)
        if (sym) {
            if (is.matrix(n)) {
                se <- 1/sqrt(n[lower.tri(n)] - 3)
            }
            else {
                se <- 1/sqrt(n - 3)
            }
            lower <- fisherz2r(z - dif * se)
            upper <- fisherz2r(z + dif * se)
            ci <- data.frame(lower = lower, r = r[lower.tri(r)], 
                upper = upper, p = p[lower.tri(p)])
            cnR <- abbreviate(colnames(r), minlength = 5)
            k <- 1
            for (i in 1:(nvar - 1)) {
                for (j in (i + 1):nvar) {
                  rownames(ci)[k] <- paste(cnR[i], cnR[j], sep = "-")
                  k <- k + 1
                }
            }
        }
        else {
            z <- fisherz(r)
            se <- 1/sqrt(n - 3)
            lower <- as.vector(fisherz2r(z - dif * se))
            upper <- as.vector(fisherz2r(z + dif * se))
            ci <- data.frame(lower = lower, r = as.vector(r), 
                upper = upper, p = as.vector(p))
            cnR <- abbreviate(rownames(r), minlength = 5)
            cnC <- abbreviate(colnames(r), minlength = 5)
            k <- 1
            for (i in 1:ncol(y)) {
                for (j in 1:ncol(x)) {
                  rownames(ci)[k] <- paste(cnR[j], cnC[i], sep = "-")
                  k <- k + 1
                }
            }
        }
    }
    else {
        ci <- NULL
    }
    result <- list(r = r, n = n, t = t, p = p, se = se, adjust = adjust, 
        sym = sym, ci = ci, Call = cl)
    class(result) <- c("psych", "corr.test")
    return(result)
}


char2numeric <- function (x) 
{
    nvar <- ncol(x)
    for (i in 1:nvar) {
        if (!is.numeric(x[[i]])) {
            if (is.factor(unlist(x[[i]])) | is.character(unlist(x[[i]]))) {
                x[[i]] <- as.numeric(x[[i]])
            }
            else {
                x[[i]] <- NA
            }
        }
    }
    invisible(x)
}


sim.poly <- function (nvar = 5, n = 500, low = -2, high = 2, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, cat = 5, mod = "logistic") 
{
    if (mod == "normal") {
        result <- sim.poly.npn(nvar, n, low, high, a, c, z, d, 
            mu, sd, cat)
    }
    else {
        result <- sim.poly.npl(nvar, n, low, high, a, c, z, d, 
            mu, sd, cat)
    }
    return(result)
}


statsBy <- function (data, group, cors = FALSE, cor = "cor", method = "pearson", 
    use = "pairwise", poly = FALSE, na.rm = TRUE) 
{
    cl <- match.call()
    valid <- function(x) {
        sum(!is.na(x))
    }
    pairwise <- function(x) {
        n <- t(!is.na(x)) %*% (!is.na(x))
        n
    }
    gr <- which(colnames(data) %in% group)
    z1 <- data[, group]
    z <- z1
    cnames <- colnames(data)
    for (i in 1:ncol(data)) {
        if (is.factor(data[, i]) || is.logical(data[, i])) {
            data[, i] <- as.numeric(data[, i])
        }
    }
    xvals <- list()
    temp <- by(data, z, colMeans, na.rm = na.rm)
    rownn <- lapply(temp, is.null)
    if (sum(as.integer(rownn)) > 0) {
        rown <- names(temp)[-which(rownn == TRUE)]
    }
    else {
        rown <- names(temp)
    }
    xvals$mean <- t(matrix(unlist(temp), nrow = ncol(data)))
    xvals$sd <- t(matrix(unlist(by(data, z, function(x) sapply(x, 
        sd, na.rm = na.rm))), nrow = ncol(data)))
    xvals$n <- t(matrix(unlist(by(data, z, function(x) sapply(x, 
        valid))), nrow = ncol(data)))
    colnames(xvals$mean) <- colnames(xvals$sd) <- colnames(xvals$n) <- colnames(data)
    rownames(xvals$mean) <- rownames(xvals$sd) <- rownames(xvals$n) <- rown
    nH <- harmonic.mean(xvals$n)
    nG <- nrow(xvals$mean)
    GM <- colSums(xvals$mean * xvals$n, na.rm = na.rm)/colSums(xvals$n, 
        na.rm = na.rm)
    MSb <- colSums(xvals$n * t((t(xvals$mean) - GM)^2), na.rm = na.rm)/(nG - 
        1)
    MSw <- colSums(xvals$sd^2 * (xvals$n - 1), na.rm = na.rm)/(colSums(xvals$n - 
        1))
    xvals$F <- MSb/MSw
    N <- colSums(xvals$n)
    npr <- (colSums(xvals$n - 1) + nrow(xvals$n))/(nrow(xvals$n))
    xvals$ICC1 <- (MSb - MSw)/(MSb + MSw * (npr - 1))
    xvals$ICC2 <- (MSb - MSw)/(MSb)
    if (cors) {
        if (poly) {
            cor <- "poly"
        }
        switch(cor, cor = {
            r <- by(data, z, function(x) cor(x[-gr], use = use, 
                method = method))
        }, cov = {
            r <- by(data, z, function(x) cov(x[-gr], use = use))
            covar <- TRUE
        }, tet = {
            r <- by(data, z, function(x) tetrachoric(x[-gr])$rho)
        }, poly = {
            r <- by(data, z, function(x) polychoric(x[-gr])$rho)
        }, mixed = {
            r <- by(data, z, function(x) mixed.cor(x[-gr])$rho)
        })
        nvars <- ncol(r[[1]])
        xvals$r <- r
        lower <- lapply(r, function(x) x[lower.tri(x)])
        xvals$within <- t(matrix(unlist(lower), nrow = nvars * 
            (nvars - 1)/2))
        cnR <- abbreviate(cnames[-gr], minlength = 5)
        k <- 1
        colnames(xvals$within) <- paste("V", 1:ncol(xvals$within))
        for (i in 1:(nvars - 1)) {
            for (j in (i + 1):nvars) {
                colnames(xvals$within)[k] <- paste(cnR[i], cnR[j], 
                  sep = "-")
                k <- k + 1
            }
        }
        wt <- by(data, z, function(x) count.pairwise(x[-gr]))
        lower.wt <- t(matrix(unlist(lapply(wt, function(x) x[lower.tri(x)])), 
            nrow = nvars * (nvars - 1)/2))
        lower.wt <- t(t(lower.wt)/colSums(lower.wt, na.rm = TRUE))
        pool <- colSums(lower.wt * xvals$within, na.rm = TRUE)
        pool.sd <- apply(xvals$within, 2, FUN = sd, na.rm = TRUE)
        xvals$pooled <- matrix(0, nvars, nvars)
        xvals$pooled[lower.tri(xvals$pooled)] <- pool
        xvals$pooled <- xvals$pooled + t(xvals$pooled)
        diag(xvals$pooled) <- 1
        xvals$sd.r <- matrix(NaN, nvars, nvars)
        xvals$sd.r[lower.tri(xvals$sd.r)] <- pool.sd
        xvals$sd.r[upper.tri(xvals$sd.r)] <- pool.sd
        colnames(xvals$pooled) <- rownames(xvals$pooled) <- cnames[-gr]
    }
    nvar <- ncol(data) - length(group)
    if (poly) 
        cor <- "poly"
    switch(cor, cor = {
        xvals$raw <- cor(data, use = use, method = method)
    }, cov = {
        xvals$raw <- cov(data, use = use)
        covar <- TRUE
    }, poly = {
        xvals$raw <- polychoric(data)$rho
    }, tet = {
        xvals$raw <- tetrachoric(data)$rho
    }, mixed = {
        xvals$raw <- mixed.cor(data)$rho
    })
    new.data <- as.matrix(merge(xvals$mean, data, by = group, 
        suffixes = c(".bg", "")))
    new.data <- new.data[, (length(group) + 1):ncol(new.data)]
    diffs <- new.data[, (nvar + 1):ncol(new.data)] - new.data[, 
        1:nvar]
    colnames(diffs) <- paste(colnames(new.data)[(nvar + 1):ncol(new.data)], 
        ".wg", sep = "")
    xvals$rbg <- cor(new.data[, 1:nvar], use = "pairwise", method = method)
    t <- (xvals$rbg * sqrt(nG - 2))/sqrt(1 - xvals$rbg^2)
    if (nG > 2) {
        xvals$pbg <- 2 * (1 - pt(abs(t), (nG - 2)))
    }
    else {
        xvals$pbg <- NA
    }
    if (cor %in% c("tet", "poly", "mixed", "mixed.cor")) 
        cor <- "cor"
    switch(cor, cor = {
        xvals$rwg <- cor(diffs, use = use, method = method)
    }, cov = {
        xvals$rwg <- cov(diffs, use = use)
        covar <- TRUE
    })
    xvals$nw <- pairwise(diffs)
    rwg <- cov2cor(xvals$rwg)
    t <- (rwg * sqrt(xvals$nw - 2))/sqrt(1 - rwg^2)
    xvals$pwg <- 2 * (1 - pt(abs(t), (N - nG - 2)))
    xvals$etabg <- diag(cor(new.data[, 1:(nvar)], new.data[, 
        (nvar + 1):ncol(new.data)], use = "pairwise", method = method))
    xvals$etawg <- diag(cor(new.data[, (nvar + 1):ncol(new.data)], 
        diffs, use = "pairwise", method = method))
    names(xvals$etabg) <- colnames(xvals$rbg)
    xvals$nwg <- N - nG
    xvals$nG <- nG
    xvals$Call <- cl
    statsBy <- xvals
    class(statsBy) <- c("psych", "statsBy")
    return(statsBy)
}


circadian.F <- function (angle, group, data = NULL, hours = TRUE, na.rm = TRUE) 
{
    if (!is.null(data)) {
        angle <- data[, angle]
        group <- data[, group]
    }
    stats <- by(angle, group, circadian.stats)
    all <- circadian.stats(angle)
    allR <- all$n * all$R
    nR <- lapply(stats, function(x) x$n * x$R)
    ngroups <- length(nR)
    within <- matrix(unlist(nR), ncol = ngroups)
    rownames(within) <- rownames(all)
    SSb <- rowSums(within) - allR
    SSw <- all$n - rowSums(within)
    dfw <- all$n - ngroups
    MSw <- SSw/dfw
    dfb = ngroups - 1
    MSb <- SSb/dfb
    F <- MSb/MSw
    prob <- 1 - pf(F, dfb, dfw)
    F.df <- data.frame(SSb = SSb, dfb = dfb, MSb = MSb, SSw = SSw, 
        dfw = dfw, MSw = MSw, F = F, prob = prob)
    result <- list(pooled = all, bygroup = stats, F = F.df)
    class(result) <- c("psych", "circadian")
    return(result)
}


corFiml <- function (x, covar = FALSE, show = FALSE) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    Mp <- getMissingPatterns(x)
    if (length(Mp$empty.idx) > 0L) {
        x <- x[-Mp$empty.idx, , drop = FALSE]
    }
    mpat <- getMissingPatternStats(X = x, Mp = Mp)
    if (show) {
        return(Mp$pat)
    }
    else {
        moments <- estimate.moments.fiml(X = x, M = mpat)
        colnames(moments$sigma) <- rownames(moments$sigma) <- colnames(x)
        cor <- cov2cor(moments$sigma)
        if (covar) {
            return(list(mean = moments$mu, cor = cor, cov = moments$sigma, 
                fx = moments$fx))
        }
        else {
            return(cor)
        }
    }
}


factor.stats <- function (r = NULL, f, phi = NULL, n.obs = NA, np.obs = NULL, 
    alpha = 0.1, fm = NULL) 
{
    fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, 
        alpha = alpha, fm = fm)
}


matReg <- function (x, y, C, n.obs = 0) 
{
    numx <- length(x)
    df <- n.obs - 1 - numx
    Cr <- cov2cor(C)
    if (numx == 1) {
        beta <- solve(C[x, x, drop = FALSE], (C[x, y, drop = FALSE]))
        colnames(beta) <- y
    }
    else {
        beta <- solve(C[x, x], (C[x, y]))
    }
    if (!is.matrix(beta)) {
        beta <- matrix(beta, nrow = length(beta))
    }
    if (is.character(x)) {
        rownames(beta) <- x
    }
    else {
        rownames(beta) <- colnames(C)[x]
    }
    if (is.character(y)) {
        colnames(beta) <- y
    }
    else {
        colnames(beta) <- colnames(C)[y]
    }
    R2 <- colSums(beta * C[x, y])/diag(C[y, y, drop = FALSE])
    uniq <- 1 - (1 - 1/diag(solve(Cr[x, x, drop = FALSE])))
    if (n.obs > 2) {
        se <- (sqrt((1 - R2)/(n.obs - 1 - numx)) %*% t(sqrt(1/uniq)))
        se <- t(se * sqrt(diag(C[y, y, drop = FALSE])) %*% t(sqrt(1/diag(C[x, 
            x, drop = FALSE]))))
        colnames(se) <- colnames(beta)
    }
    else {
        se <- NA
    }
    if (!any(is.na(se))) {
        tvalue <- beta/se
        prob <- 2 * (1 - pt(abs(tvalue), df))
    }
    else {
        tvalue <- prob <- NA
    }
    result <- list(beta = beta, se = se, t = tvalue, prob = prob, 
        R2 = R2)
    return(result)
}


faBy <- function (stats, nfactors = 1, rotate = "oblimin", fm = "minres", 
    free = TRUE, all = FALSE, min.n = 12, quant = 0.1, ...) 
{
    if (class(stats)[2] != "statsBy") 
        stop("Please run statsBy first")
    cl <- match.call()
    fo.orth <- fa(stats$pooled, nfactors = nfactors, rotate = "none", 
        fm = fm)
    fo.rotated <- fa(stats$pooled, nfactors = nfactors, rotate = rotate, 
        fm = fm, ...)
    fl <- fo.rotated$loadings
    f <- list()
    ngroups <- stats$nG
    nvar <- ncol(stats$r[[1]])
    stats$r <- pickgood(stats, min.n = min.n)
    replicateslist <- lapply(stats$r, function(X, ...) {
        if (!is.null(X)) {
            if (!free && (nfactors > 1)) {
                fs <- try(fac(X, nfactors = nfactors, rotate = "none", 
                  scores = "none", ...))
                fs$loadings <- faMatch(fo.orth, fs)$loadings
                fs <- TargetQ(fs$loadings, Target = list(fl))
                fs <- faMatch(fl, fs)
            }
            else {
                fs <- try(fac(X, nfactors = nfactors, rotate = rotate, 
                  scores = "none", ...))
                fs$loadings <- faMatch(fl, fs)$loadings
            }
            if (!is.null(fs$Phi)) {
                phis <- fs$Phi
                if (all) {
                  replicates <- list(fa = fs, loadings = (fs$loadings), 
                    phis = phis, vloadings = as.vector(fs$loadings), 
                    vphis = phis[lower.tri(phis)])
                }
                else {
                  replicates <- list(loadings = fs$loadings, 
                    phis = phis, vloadings = as.vector(fs$loadings), 
                    vphis = phis[lower.tri(phis)])
                }
            }
        }
    })
    fabygroups <- lapply(replicateslist, function(X) X$vloadings)
    notnullgroup <- unlist(lapply(fabygroups, function(x) !is.null(x)))
    namesbygroup <- names(fabygroups)[notnullgroup]
    fabygroups <- matrix(unlist(lapply(replicateslist, function(X) X$vloadings)), 
        ncol = nvar * nfactors, byrow = TRUE)
    num.groups <- nrow(fabygroups)
    means <- colMeans(fabygroups, na.rm = TRUE)
    sds <- apply(fabygroups, 2, sd, na.rm = TRUE)
    quants.low <- apply(fabygroups, 2, quantile, quant)
    quants.high <- apply(fabygroups, 2, quantile, 1 - quant)
    fnames <- colnames(fo.rotated$loadings)[1:nfactors]
    vnames <- rownames(fo.rotated$loadings)
    faby.sum <- matrix(c(as.vector(fl), means, sds, quants.low, 
        quants.high), ncol = 5)
    colnames(faby.sum) <- c("Pooled", "mean", "sd", "low", "high")
    rownames(faby.sum) <- paste(rep(vnames, nfactors))
    faby <- t(fabygroups)
    colnames(faby) <- c(paste0("gr-", namesbygroup))
    rownames(faby) <- paste(rep(vnames, nfactors), "-", rep(fnames, 
        each = nvar))
    if (!is.null(fo.rotated$Phi)) {
        vphis <- matrix(unlist(lapply(replicateslist, function(X) X$vphis)), 
            nrow = num.groups, byrow = TRUE)
        means.phis <- colMeans(vphis)
        sds.phis <- apply(vphis, 2, sd, na.rm = TRUE)
        phis.low <- apply(vphis, 2, quantile, quant)
        phis.high <- apply(vphis, 2, quantile, 1 - quant)
        phiby.sum <- matrix(c(fo.rotated$Phi[lower.tri(fo.rotated$Phi)], 
            means.phis, sds.phis, phis.low, phis.high), ncol = 5)
        phiby <- (matrix(c(fo.rotated$Phi[lower.tri(fo.rotated$Phi)], 
            means.phis, sds.phis, phis.low, phis.high, t(vphis)), 
            ncol = (num.groups + 5), byrow = FALSE))
        colnames(phiby) <- c("Total", "Mean", "sd", "low", "high", 
            paste0("gr-", namesbygroup))
        rownames(phiby) <- 1:(nfactors * (nfactors - 1)/2)
        k <- 1
        for (fi in 1:(nfactors - 1)) {
            for (fj in (fi + 1):(nfactors)) {
                rownames(phiby)[k] <- paste(fnames[fi], "-", 
                  fnames[fj], sep = "")
                k <- k + 1
            }
        }
    }
    meanloading <- matrix(means, ncol = nfactors)
    colnames(meanloading) <- fnames
    rownames(meanloading) <- vnames
    phis <- matrix(0, nfactors, nfactors)
    phis[lower.tri(phis)] <- means.phis
    phis <- phis + t(phis)
    diag(phis) <- 1
    colnames(phis) <- rownames(phis) <- fnames
    if (all) {
        faBy <- list(fa = lapply(replicateslist, function(X) X$fa), 
            loadings = faby, Phi = phiby, Call = cl)
    }
    else {
        faBy <- list(mean.loading = meanloading, mean.Phi = phis, 
            faby.sum = faby.sum, Phi.sum = phiby.sum, loadings = t(faby), 
            Phi = t(phiby), nfactors = nfactors, quant, Call = cl)
    }
    class(faBy) <- c("psych", "faBy")
    return(faBy)
}


statsBy.boot <- function (data, group, ntrials = 10, cors = FALSE, replace = TRUE, 
    method = "pearson") 
{
    cl <- match.call()
    result <- vector("list", ntrials)
    for (i in 1:ntrials) {
        progressBar(i, ntrials, "statsBy")
        data[, group] <- sample(data[, group], size = nrow(data), 
            replace = replace)
        result[[i]] <- statsBy(data, group, cors = cors, method = method)
    }
    return(result)
}


ellipses <- function (x, y = NULL, add = FALSE, smooth = TRUE, lm = FALSE, 
    data = TRUE, n = 2, span = 2/3, iter = 3, col = "red", xlab = NULL, 
    ylab = NULL, ...) 
{
    done = FALSE
    segments = 51
    if ((is.matrix(x)) | (is.data.frame(x))) {
        if (dim(x)[2] > 2) {
            pairs.panels(x)
            done = TRUE
        }
        else {
            if (is.null(xlab)) 
                xlab = colnames(x)[1]
            if (is.null(ylab)) 
                ylab = colnames(x)[2]
            y <- x[, 2]
            x <- x[, 1]
        }
    }
    else {
        if ((!is.vector(x)) | (!is.vector(y))) {
            stop("x and y must be vectors")
        }
    }
    if (!done) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = (cor(x, y, use = "pairwise"))
        if (is.null(xlab)) 
            xlab = colnames(x)
        if (is.null(ylab)) 
            ylab = colnames(y)
        angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
        if (abs(r) > 0) 
            theta <- sign(r)/sqrt(2)
        else theta = 1/sqrt(2)
        shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
            theta, -theta, theta), ncol = 2, byrow = TRUE)
        ellipse <- unit.circle %*% shape
        ellipse[, 1] <- ellipse[, 1] * xs + xm
        ellipse[, 2] <- ellipse[, 2] * ys + ym
        if (add) {
            lines(ellipse, col = col, ...)
            if (data) {
                points(xm, ym, pch = 20, cex = 1.5, col = col)
            }
        }
        else {
            plot(x, y, xlab = xlab, ylab = ylab, ...)
            points(xm, ym, pch = 20, cex = 1.5, col = col)
            lines(ellipse, type = "l", col = col, ...)
        }
        if (smooth) {
            ok <- is.finite(x) & is.finite(y)
            if (any(ok)) 
                lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                  col = col, ...)
        }
        if (lm) {
            ok <- is.finite(x) & is.finite(y)
            if (any(ok)) 
                abline(lm(y[ok] ~ x[ok]), col = col, lty = "dashed", 
                  ...)
        }
        if (n > 1) {
            ellipse <- unit.circle %*% shape
            ellipse[, 1] <- ellipse[, 1] * 2 * xs + xm
            ellipse[, 2] <- ellipse[, 2] * 2 * ys + ym
            lines(ellipse, col = col, ...)
        }
    }
}


YuleCor <- function (x, c = 1, bonett = FALSE, alpha = 0.05) 
{
    cl <- match.call()
    nvar <- ncol(x)
    rho <- matrix(NA, nvar, nvar)
    ci <- matrix(NA, nvar, nvar)
    zp <- matrix(NA, nvar, nvar)
    for (i in 1:nvar) {
        for (j in 1:i) {
            YB <- YuleBonett(table(x[, i], x[, j]), c = c, bonett = bonett, 
                alpha = alpha)
            rho[i, j] <- rho[j, i] <- YB$rho
            ci[i, j] <- YB$lower
            ci[j, i] <- YB$upper
            zp[i, j] <- YB$rho/YB$se
            zp[j, i] <- 1 - pnorm(zp[i, j])
        }
    }
    colnames(rho) <- rownames(rho) <- colnames(ci) <- rownames(ci) <- colnames(x)
    result <- list(rho = rho, ci = ci, zp = zp, Call = cl)
    class(result) <- c("psych", "yule")
    return(result)
}


sim.correlation <- function (R, n = 1000, data = FALSE) 
{
    eX <- eigen(R)
    nvar <- ncol(R)
    observed <- matrix(rnorm(nvar * n), n, nvar)
    observed <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 0)), 
        nvar) %*% t(observed))
    colnames(observed) <- colnames(R)
    if (data) {
        result <- observed
    }
    else {
        result <- cor(observed)
    }
    return(result)
}


p.rep.r <- function (r, n, twotailed = TRUE) 
{
    dprime <- 2 * r/sqrt(1 - r^2)
    sigmad <- sqrt(4/(n - 4))
    z <- dprime/sigmad
    p <- 1 - pnorm(z)
    p.rep <- pnorm(qnorm((1 - p))/sqrt(2))
    if (twotailed) 
        p <- 2 * p
    return(list(p.rep = p.rep, dprime = dprime, prob = p))
}


nfactors <- function (x, n = 20, rotate = "varimax", diagonal = FALSE, fm = "minres", 
    n.obs = NULL, title = "Number of Factors", pch = 16, use = "pairwise", 
    cor = "cor", ...) 
{
    vs <- vss(x = x, n = n, rotate = rotate, diagonal = diagonal, 
        fm = fm, n.obs = n.obs, plot = FALSE, title = title, 
        use = use, cor = cor, ...)
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    op <- par(mfrow = c(2, 2))
    x <- vs$vss.stats
    n = dim(x)
    plot(x$cfit.1, ylim = c(0, 1), typ = "b", ylab = "Very Simple Structure Fit", 
        xlab = "Number of Factors", main = "Very Simple Structure", 
        pch = 49)
    lines(x$cfit.1)
    x$cfit.2[1] <- NA
    x$cfit.3[1] <- NA
    x$cfit.3[2] <- NA
    lines(x$cfit.2)
    points(x$cfit.2, pch = 50)
    lines(x$cfit.3)
    points(x$cfit.3, pch = 51)
    plot(vs$vss.stats[, "complex"], xlab = "Number of factors", 
        ylab = "Complexity", typ = "b", main = "Complexity", 
        pch = pch, ...)
    plot(vs$vss.stats[, "eBIC"], xlab = "Number of factors", 
        ylab = "Empirical BIC", typ = "b", main = "Empirical BIC", 
        pch = pch, ...)
    plot(vs$vss.stats[, "SRMR"], xlab = "Number of factors", 
        ylab = "SRMR", typ = "b", main = "Root Mean Residual", 
        pch = pch, ...)
    results <- list(title = title, map = vs$map, vss.stats = vs$vss.stats[, 
        1:16], call = vs$call)
    class(results) <- c("psych", "vss")
    return(results)
}


p.rep.t <- function (t, df, df2 = NULL, twotailed = TRUE) 
{
    if (is.null(df2)) {
        dprime = 2 * t/sqrt(df)
        nc <- 1
    }
    else {
        n1 <- df + 1
        n2 <- df2 + 1
        df <- df + df2
        nc <- ((n1 + n2)/2)/((2 * n1 * n2)/(n1 + n2))
        dprime <- (2 * t/sqrt(df)) * sqrt(nc)
    }
    p <- pt(t, df, lower.tail = FALSE)
    r.equiv <- sqrt(t^2/(t^2 + df))
    if (twotailed) 
        p <- 2 * p
    p.rep <- pnorm(qnorm((1 - p))/sqrt(2))
    return(list(p.rep = p.rep, dprime = dprime, prob = p, r.equiv = r.equiv))
}


correct.cor <- function (x, y) 
{
    n = dim(x)[1]
    {
        diag(x) <- y
        if (n > 1) {
            for (i in 2:n) {
                k = i - 1
                for (j in 1:k) {
                  x[j, i] <- x[j, i]/sqrt(y[i] * y[j])
                }
            }
        }
        return(x)
    }
}


circadian.stats <- function (angle, data = NULL, hours = TRUE, na.rm = TRUE) 
{
    cl <- match.call()
    means <- circadian.mean(angle = angle, data = data, hours = hours, 
        na.rm = na.rm)
    csd <- circadian.sd(angle = angle, hours = hours, na.rm = na.rm)
    if (!is.null(data)) 
        angle <- data[, angle]
    if (length(means) > 1) {
        n.obs <- colSums(!is.na(angle))
    }
    else {
        n.obs <- sum(!is.na(angle))
    }
    R <- csd$R
    if (hours) {
        sd <- csd$sd * 24/(2 * pi)
    }
    else {
        sd <- csd$sd
    }
    z <- n.obs * R^2
    p <- exp(-z)
    result <- data.frame(n = n.obs, mean = means, sd = sd, R, 
        z = z, p = p)
    class(result) <- c("psych", "circadian", "data.frame")
    return(result)
}


mat.sort <- function (m, f = NULL) 
{
    if (is.null(f)) {
        f <- fa(m)
    }
    if (is.list(f) && (!is.null(loadings(f)))) {
        load <- loadings(f)
    }
    else {
        load <- f
    }
    load <- as.matrix(load)
    nitems <- NROW(load)
    nfactors <- NCOL(load)
    loads <- data.frame(item = seq(1:nitems), cluster = rep(0, 
        nitems), unclass(load))
    loads$cluster <- apply(abs(load), 1, which.max)
    ord <- sort(loads$cluster, index.return = TRUE)
    loads[1:nitems, ] <- loads[ord$ix, ]
    rownames(loads)[1:nitems] <- rownames(loads)[ord$ix]
    items <- table(loads$cluster)
    first <- 1
    item <- loads$item
    for (i in 1:length(items)) {
        if (items[i] > 0) {
            last <- first + items[i] - 1
            ord <- sort(abs(loads[first:last, i + 2]), decreasing = TRUE, 
                index.return = TRUE)
            loads[first:last, 3:(nfactors + 2)] <- load[item[ord$ix + 
                first - 1], ]
            loads[first:last, 1] <- item[ord$ix + first - 1]
            rownames(loads)[first:last] <- rownames(loads)[ord$ix + 
                first - 1]
            first <- first + items[i]
        }
    }
    item.order <- loads[, 1]
    m <- m[item.order, item.order]
    return(m)
}


read.file.csv <- function (f = NULL, header = TRUE, ...) 
{
    if (missing(f)) 
        f <- file.choose()
    read.table(f, header = header, sep = ",", ...)
}


read.clipboard.tab <- function (header = TRUE, sep = "\t", ...) 
{
    MAC <- Sys.info()[1] == "Darwin"
    if (!MAC) {
        if (header) 
            read.clipboard <- read.table(file("clipboard"), header = TRUE, 
                sep, ...)
        else read.clipboard <- read.table(file("clipboard"), 
            sep = sep, ...)
    }
    else {
        if (header) 
            read.clipboard <- read.table(pipe("pbpaste"), header = TRUE, 
                sep, ...)
        else read.clipboard <- read.table(pipe("pbpaste"), sep = sep, 
            ...)
    }
}


mat.regress <- function (y, x, data, z = NULL, n.obs = NULL, use = "pairwise", 
    square = FALSE) 
{
    message("mat.regress has been replaced by setCor, please change your call")
    setCor(y, x, data, z = NULL, n.obs = NULL, use = "pairwise", 
        square = FALSE)
}


guttman <- function (r, key = NULL) 
{
    cl <- match.call()
    .Deprecated("splitHalf", msg = "Guttman has been deprecated.  The use of the splitHalf function is recommended")
    nvar <- dim(r)[2]
    if (dim(r)[1] != dim(r)[2]) {
        r <- cor(r, use = "pairwise")
    }
    else {
        if (!is.matrix(r)) 
            r <- as.matrix(r)
        r <- cov2cor(r)
    }
    if (is.null(colnames(r))) {
        rownames(r) <- colnames(r) <- paste("V", 1:nvar, sep = "")
    }
    if (!is.null(key)) {
        key <- as.vector(key)
        r <- diag(key) %*% r %*% diag(key)
        flip <- FALSE
    }
    else {
        key <- rep(1, nvar)
    }
    m <- (1 - r)/2
    diag(m) <- 1
    m.names <- colnames(r)
    colnames(m) <- m.names
    signkey <- strtrim(key, 1)
    signkey[signkey == "1"] <- ""
    m.names <- paste(m.names, signkey, sep = "")
    colnames(m) <- rownames(m) <- m.names
    if (nvar < 3) {
        message("These estimates are not really meaningful if you have less than 3 items, \n Try running the alpha function instead")
        stop()
    }
    best <- splitHalf(r)
    f1 <- fa(r, SMC = FALSE)
    load <- f1$loadings
    ord.load <- order(load)
    key.fa <- matrix(rep(0, nvar * 2), ncol = 2)
    for (i in 1:nvar) {
        key.fa[ord.load[i], 1] <- i%%2
        key.fa[ord.load[i], 2] <- 1 - key.fa[ord.load[i], 1]
    }
    f2 <- fa(r, 2, SMC = FALSE)
    load <- f2$loadings
    key.fa2 <- matrix(rep(0, nvar * 2), ncol = 2)
    key.fa2[, 1] <- (abs(load[, 1]) > abs(load[, 2])) + 0
    key.fa2[, 2] <- 1 - key.fa2[, 1]
    ev <- eigen(r)$values
    e <- ev[1]
    alpha.pc <- (1 - 1/e) * nvar/(nvar - 1)
    r.pc <- 2 * ev[1]/(ev[1] + ev[2]) - 1
    r.pc <- r.pc * alpha.pc
    beta.pc <- 2 * r.pc/(1 + r.pc)
    Vt <- sum.r <- sum(r)
    tr.r <- tr(r)
    lambda.1 <- 1 - tr.r/Vt
    off <- r
    diag(off) <- 0
    sum.off <- sum(off)
    sumsq.off <- sum(off^2)
    lambda.2 <- (sum.off + sqrt(sumsq.off * nvar/(nvar - 1)))/Vt
    lambda.3 <- nvar * lambda.1/(nvar - 1)
    sum.smc <- sum(smc(r))
    lambda.6 <- (sum.r + sum.smc - sum(diag(r)))/Vt
    c.co <- colSums(r^2) - diag(r^2)
    c.co.max <- max(c.co)
    lambda.5 <- lambda.1 + 2 * sqrt(c.co.max)/Vt
    lambda.5p <- lambda.1 + (nvar)/(nvar - 1) * 2 * sqrt(c.co.max)/Vt
    sum.smc <- sum(smc(r))
    glb <- glb.fa(r)$glb
    beta <- best$minrb
    if (beta < 0) 
        beta <- 0
    gamma <- (sum.r + sum.smc - sum(diag(r)))/Vt
    tenberg <- tenberge(r)
    result <- list(lambda.1 = lambda.1, lambda.2 = lambda.2, 
        lambda.3 = lambda.3, lambda.4 = best$maxrb, lambda.5 = lambda.5, 
        lambda.5p = lambda.5p, lambda.6 = lambda.6, alpha.pc = alpha.pc, 
        glb = glb, tenberge = tenberg, r.pc = r.pc, beta.pc = beta.pc, 
        beta = beta, Call = cl)
    class(result) <- c("psych", "guttman")
    return(result)
}


sim.congeneric <- function (loads = c(0.8, 0.7, 0.6, 0.5), N = NULL, err = NULL, 
    short = TRUE, categorical = FALSE, low = -3, high = 3, cuts = NULL) 
{
    n <- length(loads)
    loading <- matrix(loads, nrow = n)
    error <- diag(1, nrow = n)
    if (!is.null(err)) {
        diag(error) <- err
    }
    else {
        diag(error) <- sqrt(1 - loading^2)
    }
    pattern <- cbind(loading, error)
    colnames(pattern) <- c("theta", paste("e", seq(1:n), sep = ""))
    rownames(pattern) <- c(paste("V", seq(1:n), sep = ""))
    model <- pattern %*% t(pattern)
    if (!is.null(N)) {
        latent <- matrix(rnorm(N * (n + 1)), ncol = (n + 1))
        observed <- latent %*% t(pattern)
        if (is.null(cuts)) {
            if (categorical) {
                observed = round(observed)
                observed[(observed <= low)] <- low
                observed[(observed > high)] <- high
            }
        }
        else {
            temp <- observed
            ncuts <- length(cuts)
            temp[(observed <= cuts[1])] <- 1
            if (ncuts > 1) {
                for (nc in 2:ncuts) {
                  temp[(observed > cuts[nc - 1]) & (observed <= 
                    cuts[nc])] <- nc
                }
            }
            temp[(observed > cuts[ncuts])] <- ncuts + 1
            observed <- temp - 1
        }
        colnames(latent) <- c("theta", paste("e", seq(1:n), sep = ""))
        if (short) 
            model <- cor(observed)
    }
    if (short) {
        return(model)
    }
    else {
        if (!is.null(N)) {
            result <- list(model = model, pattern = pattern, 
                r = cor(observed), latent = latent, observed = observed, 
                N = N)
        }
        else {
            result <- model
        }
        class(result) <- c("psych", "sim")
        return(result)
    }
}


r.test <- function (n, r12, r34 = NULL, r23 = NULL, r13 = NULL, r14 = NULL, 
    r24 = NULL, n2 = NULL, pooled = TRUE, twotailed = TRUE) 
{
    cl <- match.call()
    if (is.null(r34) & is.null(r13) & is.null(r23)) {
        t <- r12 * sqrt(n - 2)/sqrt(1 - r12^2)
        p <- 1 - pt(abs(t), n - 2)
        if (twotailed) 
            p <- 2 * p
        ci <- r.con(r12, n)
        result <- list(Call = cl, Test = "Test of significance of a  correlation", 
            t = t, p = p, ci = ci)
    }
    else {
        if (is.null(r23)) {
            xy.z <- 0.5 * log((1 + r12)/(1 - r12))
            xz.z <- 0.5 * log((1 + r34)/(1 - r34))
            if (is.null(n2)) 
                n2 <- n
            se.diff.r <- sqrt(1/(n - 3) + 1/(n2 - 3))
            diff <- xy.z - xz.z
            z <- abs(diff/se.diff.r)
            p <- (1 - pnorm(z))
            if (twotailed) 
                p <- 2 * p
            result <- list(Call = cl, Test = "Test of difference between two independent correlations", 
                z = z, p = p)
        }
        else {
            if (is.null(r14)) {
                if (!is.null(r34)) {
                  if (is.null(r13)) {
                    r13 <- r34
                  }
                }
                diff <- r12 - r13
                determin = 1 - r12 * r12 - r23 * r23 - r13 * 
                  r13 + 2 * r12 * r23 * r13
                av = (r12 + r13)/2
                cube = (1 - r23) * (1 - r23) * (1 - r23)
                t2 = diff * sqrt((n - 1) * (1 + r23)/(((2 * (n - 
                  1)/(n - 3)) * determin + av * av * cube)))
                p <- pt(abs(t2), n - 3, lower.tail = FALSE)
                if (twotailed) 
                  p <- 2 * p
                cl <- paste("r.test(n = ", n, ",  r12 = ", r12, 
                  ",  r23 = ", r23, ",  r13 = ", r13, ")")
                result <- list(Call = cl, Test = "Test of difference between two correlated  correlations", 
                  t = t2, p = p)
            }
            else {
                z12 <- fisherz(r12)
                z34 <- fisherz(r34)
                pooledr <- (r12 + r34)/2
                if (pooled) {
                  r1234 = 1/2 * ((r13 - pooledr * r23) * (r24 - 
                    r23 * pooledr) + (r14 - r13 * pooledr) * 
                    (r23 - pooledr * r13) + (r13 - r14 * pooledr) * 
                    (r24 - pooledr * r14) + (r14 - pooledr * 
                    r24) * (r23 - r24 * pooledr))
                  z1234 <- r1234/((1 - pooledr^2) * (1 - pooledr^2))
                }
                else {
                  r1234 = 1/2 * ((r13 - r12 * r23) * (r24 - r23 * 
                    r34) + (r14 - r13 * r34) * (r23 - r12 * r13) + 
                    (r13 - r14 * r34) * (r24 - r12 * r14) + (r14 - 
                    r12 * r24) * (r23 - r24 * r34))
                  z1234 <- r1234/((1 - r12^2) * (1 - r34^2))
                }
                ztest <- (z12 - z34) * sqrt(n - 3)/sqrt(2 * (1 - 
                  z1234))
                z <- ztest
                p <- (1 - pnorm(abs(z)))
                if (twotailed) 
                  p <- 2 * p
                result <- list(Call = cl, Test = "Test of difference between two dependent correlations", 
                  z = z, p = p)
            }
        }
    }
    class(result) <- c("psych", "r.test")
    return(result)
}


table2df <- function (x, count = NULL, labs = NULL) 
{
    if (!is.null(count)) {
        xm.df <- bigtable2matrix(x, count, labs)
    }
    else {
        n <- sum(x)
        nrows <- dim(x)[1]
        ncol <- dim(x)[2]
        rowval <- as.numeric(rownames(x))
        colval <- as.numeric(colnames(x))
        xm <- matrix(NaN, nrow = n, ncol = 2)
        k <- 1
        for (rows in 1:nrows) {
            for (cols in 1:ncol) {
                case <- x[rows, cols]
                if (case > 0) {
                  for (cases in 1:case) {
                    xm[k, 1] <- rowval[rows]
                    xm[k, 2] <- colval[cols]
                    k <- k + 1
                  }
                }
            }
        }
        if (!is.null(labs)) 
            colnames(xm) <- labs
        xm.df <- data.frame(xm)
    }
    return(xm.df)
}


phi2poly <- function (ph, cp, cc, n = NULL, correct = TRUE) 
{
    r.marg <- rep(0, 2)
    c.marg <- rep(0, 2)
    p <- array(rep(0, 4), dim = c(2, 2))
    r.marg[1] <- cp
    r.marg[2] <- 1 - cp
    c.marg[1] <- cc
    c.marg[2] <- 1 - cc
    p[1, 1] <- r.marg[1] * c.marg[1] + ph * sqrt(prod(r.marg, 
        c.marg))
    p[2, 2] <- r.marg[2] * c.marg[2] + ph * sqrt(prod(r.marg, 
        c.marg))
    p[1, 2] <- r.marg[1] * c.marg[2] - ph * sqrt(prod(r.marg, 
        c.marg))
    p[2, 1] <- r.marg[2] * c.marg[1] - ph * sqrt(prod(r.marg, 
        c.marg))
    if (!is.null(n)) 
        p <- p * n
    result <- tetrachoric(p, correct = correct)$rho
    return(result)
}


interbattery <- function (r, varsX, varsY, nfX = 1, nfY = 1, n.obs = NULL, cor = "cor", 
    use = "pairwise", weight = NULL) 
{
    cl <- match.call()
    vars <- c(varsX, varsY)
    if (is.null(n.obs)) 
        n.obs <- NA
    if (ncol(r) < nrow(r)) {
        n.obs <- nrow(r)
        switch(cor, cor = {
            r <- cor(r, use = use)
        }, cov = {
            r <- cov(r, use = use)
            covar <- TRUE
        }, tet = {
            r <- tetrachoric(r)$rho
        }, poly = {
            r <- polychoric(r)$rho
        }, mixed = {
            r <- mixed.cor(r, use = use)$rho
        }, Yuleb = {
            r <- YuleCor(r, , bonett = TRUE)$rho
        }, YuleQ = {
            r <- YuleCor(r, 1)$rho
        }, YuleY = {
            r <- YuleCor(r, 0.5)$rho
        })
    }
    varnames <- colnames(r)[vars]
    R <- r[varnames, varnames]
    r12 <- r[varsX, varsY]
    H1 <- r12 %*% t(r12)
    E1 <- eigen(H1)
    W1 <- E1$vectors[, 1:nfX, drop = FALSE]
    gamma1 <- sqrt(E1$values[1:nfX, drop = FALSE])
    A1 <- W1 %*% diag(sqrt(gamma1), ncol = nfX)
    W2 <- t(r12) %*% W1 %*% diag(1/gamma1, ncol = nfX)
    A2 <- W2 %*% diag(sqrt(gamma1))
    As <- colSums(sign(A1))
    flip <- diag(sign(As), ncol = nfX)
    A1 <- A1 %*% flip
    As <- colSums(sign(A2))
    flip <- diag(sign(As), ncol = nfX)
    A2 <- A2 %*% flip
    colnames(A1) <- colnames(A2) <- paste0("IB", 1:ncol(A1))
    rownames(A1) <- rownames(r12)
    return(list(A1 = A1, A2 = A2, loadings = rbind(A1, A2), Call = cl))
}


summary.psych <- function (object, digits = 2, items = FALSE, ...) 
{
    if (length(class(object)) > 1) {
        value <- class(object)[2]
    }
    if (value == "principal") 
        value <- "fa"
    if (value == "score.items") 
        value <- "scores"
    if (value == "cluster.loadings") 
        value <- "cluster.cor"
    if (value == "mat.regress") 
        value <- "mat.reg"
    if (value == "set.cor") 
        value <- "setCor"
    if (value == "mat.reg") 
        value <- "setCor"
    switch(value, iclust = {
        cat("ICLUST (Item Cluster Analysis)")
        cat("Call: ")
        print(object$call)
        cat(object$title, "\n")
        cat("\nPurified Alpha:\n")
        print(object$purified$alpha, digits)
        cat("\n Guttman Lambda6* \n")
        print(object$G6, digits)
        cat("\nOriginal Beta:\n")
        print(object$beta, digits)
        cat("\nCluster size:\n")
        print(object$purified$size, digits)
        if (!is.null(object$purified$cor)) {
            cat("\nPurified scale intercorrelations\n reliabilities on diagonal\n correlations corrected for attenuation above diagonal: \n")
            print(object$purified$corrected, digits)
        }
    }, omega = {
        cat(object$title, "\n")
        cat("Alpha:                ", round(object$alpha, digits), 
            "\n")
        cat("G.6:                  ", round(object$G6, digits), 
            "\n")
        cat("Omega Hierarchical:   ", round(object$omega_h, digits), 
            "\n")
        cat("Omega H asymptotic:   ", round(object$omega.lim, 
            digits), "\n")
        cat("Omega Total           ", round(object$omega.tot, 
            digits), "\n")
        numfactors <- dim(object$schmid$sl)[2] - 3
        eigenvalues <- diag(t(object$schmid$sl[, 1:numfactors]) %*% 
            object$schmid$sl[, 1:numfactors])
        cat("\nWith eigenvalues of:\n")
        print(eigenvalues, digits = 2)
        maxmin <- max(eigenvalues[2:numfactors])/min(eigenvalues[2:numfactors])
        gmax <- eigenvalues[1]/max(eigenvalues[2:numfactors])
        cat("The degrees of freedom for the model is", object$schmid$dof, 
            " and the fit was ", round(object$schmid$objective, 
                digits), "\n")
        if (!is.na(object$schmid$n.obs)) {
            cat("The number of observations was ", object$schmid$n.obs, 
                " with Chi Square = ", round(object$schmid$STATISTIC, 
                  digits), " with prob < ", round(object$schmid$PVAL, 
                  digits), "\n")
        }
        if (!is.null(object$stats$rms)) {
            cat("\nThe root mean square of the residuals is ", 
                round(object$stats$rms, digits), "\n")
        }
        if (!is.null(object$stats$crms)) {
            cat("The df corrected root mean square of the residuals is ", 
                round(object$stats$crms, digits), "\n")
        }
        if (!is.null(object$schmid$RMSEA)) {
            cat("\nRMSEA and the ", object$schmid$RMSEA[4], "confidence intervals are ", 
                round(object$schmid$RMSEA[1:3], digits + 1))
        }
        if (!is.null(object$schmid$BIC)) {
            cat("\nBIC = ", round(object$schmid$BIC, digits))
        }
        if (!is.null(object$ECV)) cat("Explained Common Variance of the general factor = ", 
            round(object$ECV, digits), "\n")
        cat("\n Total, General and Subset omega for each subset\n")
        colnames(object$omega.group) <- c("Omega total for total scores and subscales", 
            "Omega general for total scores and subscales ", 
            "Omega group for total scores and subscales")
        print(round(t(object$omega.group), digits))
    }, scores = {
        cat("Call: ")
        print(object$Call)
        if (object$raw) {
            cat("\nScale intercorrelations corrected for attenuation \n raw correlations below the diagonal, (unstandardized) alpha on the diagonal \n corrected correlations above the diagonal:\n")
        } else {
            cat("\nScale intercorrelations corrected for attenuation \n raw correlations below the diagonal, (standardized) alpha on the diagonal \n corrected correlations above the diagonal:\n")
        }
        print(object$corrected, digits)
        result <- object$corrected
    }, overlap = {
        cat("Call: ")
        print(object$Call)
        cat("\nScale intercorrelations adjusted for item overlap")
        cat("\nScale intercorrelations corrected for attenuation \n raw correlations (corrected for overlap) below the diagonal, (standardized) alpha on the diagonal \n corrected (for overlap and reliability) correlations above the diagonal:\n")
        print(object$corrected, digits)
        result <- object$corrected
    }, vss = {
        if (object$title != "Very Simple Structure") {
            cat("\nVery Simple Structure of ", object$title, 
                "\n")
        } else {
            cat("\nVery Simple Structure\n")
        }
        cat("VSS complexity 1 achieves a maximimum of ")
        vss.max <- round(max(object$cfit.1), digits)
        cat(vss.max, " with ", which.max(object$cfit.1), " factors\n")
        cat("VSS complexity 2 achieves a maximimum of ")
        vss.max <- round(max(object$cfit.2), digits)
        cat(vss.max, " with ", which.max(object$cfit.2), " factors\n")
        cat("\nThe Velicer MAP criterion achieves a minimum of ")
        vss.map <- round(max(object$map), digits)
        cat(vss.map, " with ", which.min(object$map), " factors\n ")
    }, cluster.cor = {
        cat("Call: ")
        print(object$Call)
        cat("\nScale intercorrelations corrected for attenuation \n raw correlations below the diagonal, (standardized) alpha on the diagonal \n corrected correlations above the diagonal:\n")
        print(object$corrected, digits)
        result <- object$corrected
    }, esem = {
        cat("\nExploratory Structural Equation Modeling  with Call: ")
        print(object$Call)
        nfactors <- dim(object$loadings)[2]
        objective <- object$criteria[1]
        if (!is.null(objective)) {
            cat("\nTest of the hypothesis that", nfactors, if (nfactors == 
                1) "factor is" else "factors are", "sufficient.")
            cat("\nThe degrees of freedom for the model is", 
                object$dof, " and the objective function was ", 
                round(objective, digits), "\n")
            if (!is.na(object$n.obs)) {
                cat("The number of observations was ", object$n.obs, 
                  " with Chi Square = ", round(object$STATISTIC, 
                    digits), " with prob < ", signif(object$PVAL, 
                    digits), "\n")
            }
        }
        if (!is.null(object$rms)) {
            cat("\nThe root mean square of the residuals (RMSA) is ", 
                round(object$rms, digits), "\n")
        }
        if (!is.null(object$crms)) {
            cat("The df corrected root mean square of the residuals is ", 
                round(object$crms, digits), "\n")
        }
        if (!is.null(object$TLI)) {
            cat("\nTucker Lewis Index of factoring reliability = ", 
                round(object$TLI, digits + 1))
        }
        if (!is.null(object$RMSEA)) {
            cat("\nRMSEA index = ", round(object$RMSEA[1], digits + 
                1), " and the", (1 - object$RMSEA[4]) * 100, 
                "% confidence intervals are ", round(object$RMSEA[2:3], 
                  digits + 1))
        }
        if (!is.null(object$BIC)) {
            cat("\nBIC = ", round(object$BIC, digits))
        }
        if (!is.null(object$Phi)) {
            colnames(object$Phi) <- rownames(object$Phi) <- colnames(object$loadings)
            print(round(object$Phi, digits))
        }
    }, fa = {
        cat("\nFactor analysis with Call: ")
        print(object$Call)
        nfactors <- dim(object$loadings)[2]
        objective <- object$criteria[1]
        if (!is.null(objective)) {
            cat("\nTest of the hypothesis that", nfactors, if (nfactors == 
                1) "factor is" else "factors are", "sufficient.")
            cat("\nThe degrees of freedom for the model is", 
                object$dof, " and the objective function was ", 
                round(objective, digits), "\n")
            if (!is.na(object$n.obs)) {
                cat("The number of observations was ", object$n.obs, 
                  " with Chi Square = ", round(object$STATISTIC, 
                    digits), " with prob < ", signif(object$PVAL, 
                    digits), "\n")
            }
        }
        if (!is.null(object$rms)) {
            cat("\nThe root mean square of the residuals (RMSA) is ", 
                round(object$rms, digits), "\n")
        }
        if (!is.null(object$crms)) {
            cat("The df corrected root mean square of the residuals is ", 
                round(object$crms, digits), "\n")
        }
        if (!is.null(object$TLI)) {
            cat("\nTucker Lewis Index of factoring reliability = ", 
                round(object$TLI, digits + 1))
        }
        if (!is.null(object$RMSEA)) {
            cat("\nRMSEA index = ", round(object$RMSEA[1], digits + 
                1), " and the", (1 - object$RMSEA[4]) * 100, 
                "% confidence intervals are ", round(object$RMSEA[2:3], 
                  digits + 1))
        }
        if (!is.null(object$BIC)) {
            cat("\nBIC = ", round(object$BIC, digits))
        }
        if (!is.null(object$Phi)) {
            if (object$fn == "principal") {
                cat("\n With component correlations of \n")
            } else {
                cat("\n With factor correlations of \n")
            }
            colnames(object$Phi) <- rownames(object$Phi) <- colnames(object$loadings)
            print(round(object$Phi, digits))
        }
    }, faBy = {
        cat("\nFactor analysis within groups with Call: ")
        print(object$Call)
        print(object$mean.loading, digits = digits)
        print(object$mean.phi, digits = digits)
    }, items = {
        if (omega) {
            cat("\nSchmid Leiman Factor loadings:\n")
            print(object$schmid$sl)
            numfactors <- dim(object$schmid$sl)[2] - 2
            eigenvalues <- diag(t(object$schmid$sl[, 1:numfactors]) %*% 
                object$schmid$sl[, 1:numfactors])
            cat("\nWith eigenvalues of:\n")
            print(eigenvalues, digits = digits)
        }
        if (!is.null(object$item.cor)) {
            cat("\nItem by scale correlations:\n")
            print(object$item.cor, digits)
        }
        if (!is.null(object$p.sorted$sorted)) {
            cat("\nItem by Cluster Structure matrix:\n")
            print(object$p.sorted$sorted, digits)
        }
        if (!is.null(object$purified$pattern)) {
            cat("\nItem by Cluster Pattern matrix:\n")
            print(object$purified$pattern, digits)
        }
        if (vss) {
            cat("\nVelicer MAP\n")
            print(object$map, digits)
            cat("\nVery Simple Structure Complexity 1\n")
            print(object$cfit.1, digits)
            cat("\nVery Simple Structure Complexity 2\n")
            print(object$cfit.2, digits)
        }
    }, alpha = {
        cat("\nReliability analysis ", object$title, " \n")
        print(object$total, digits = digits)
    }, setCor = {
        if (object$raw) {
            cat("\nMultiple Regression from raw data \n")
        } else {
            cat("\nMultiple Regression from matrix input \n")
        }
        print(object$Call)
        cat("\nMultiple Regression from matrix input \n")
        cat("\nBeta weights \n")
        print(object$beta, digits)
        cat("\nMultiple R \n")
        print(object$R, digits)
        cat("\nMultiple R2 \n")
        print(object$R2, digits)
        cat("\nCohen's set correlation R2 \n")
        print(object$Rset, digits)
        cat("\nSquared Canonical Correlations\n")
        print(object$cancor2, digits)
    }, irt.fa = {
        cat("\nItem Response Theory using factor analysis with Call: ")
        print(object$Call)
        nfactors <- dim(object$fa$loadings)[2]
        objective <- object$fa$criteria[1]
        if (!is.null(objective)) {
            cat("\nTest of the hypothesis that", nfactors, if (nfactors == 
                1) "factor is" else "factors are", "sufficient.")
            cat("\nThe degrees of freedom for the model is", 
                object$fa$dof, " and the objective function was ", 
                round(objective, digits), "\n")
            if (!is.na(object$fa$n.obs)) {
                cat("The number of observations was ", object$fa$n.obs, 
                  " with Chi Square = ", round(object$fa$STATISTIC, 
                    digits), " with prob < ", signif(object$fa$PVAL, 
                    digits), "\n")
            }
            if (!is.null(object$fa$rms)) {
                cat("\nThe root mean square of the residuals (RMSA) is ", 
                  round(object$fa$rms, digits), "\n")
            }
            if (!is.null(object$fa$crms)) {
                cat("The df corrected root mean square of the residuals is ", 
                  round(object$fa$crms, digits), "\n")
            }
            if (!is.null(object$fa$TLI)) cat("\nTucker Lewis Index of factoring reliability = ", 
                round(object$fa$TLI, digits + 1))
        }
        if (!is.null(object$fa$RMSEA)) {
            cat("\nRMSEA index = ", round(object$fa$RMSEA[1], 
                digits + 1), " and the", (1 - object$fa$RMSEA[4]) * 
                100, "% confidence intervals are ", round(object$fa$RMSEA[2:3], 
                digits + 1))
        }
        if (!is.null(object$fa$BIC)) {
            cat("\nBIC = ", round(object$fa$BIC, digits))
        }
        if (!is.null(object$fa$Phi)) {
            if (object$fa$fn == "principal") {
                cat("\n With component correlations of \n")
            } else {
                cat("\n With factor correlations of \n")
            }
            colnames(object$fa$Phi) <- rownames(object$fa$Phi) <- colnames(object$fa$loadings)
            print(round(object$fa$Phi, digits))
        }
    }, describeData = {
        cat("n.obs = ", object$n.obs, "of which ", object$complete.cases, 
            " are complete cases. Number of variables = ", object$nvar, 
            " of which all are numeric is ", object$all.numeric, 
            "\n")
    })
}


scrub <- function (x, where, min, max, isvalue, newvalue) 
{
    if (missing(min)) 
        min <- -Inf
    if (missing(max)) 
        max <- Inf
    if (missing(isvalue)) 
        isvalue <- Inf
    if (missing(where)) 
        where <- 1:dim(x)[2]
    maxlength <- max(length(isvalue), length(min), length(max), 
        length(where))
    if (missing(newvalue)) 
        newvalue <- rep(NA, maxlength)
    if (length(min) == 1) 
        min <- rep(min, ncol(x))
    if (length(max) == 1) 
        max <- rep(max, ncol(x))
    if (length(where) == 1) 
        where <- rep(where, maxlength)
    if (length(isvalue) == 1) 
        isvalue <- rep(isvalue, maxlength)
    if (length(newvalue) == 1) 
        newvalue <- rep(newvalue, maxlength)
    for (k in 1:maxlength) {
        i <- where[k]
        x[(!is.na(x[, i]) & (x[, i] < min[k])), i] <- newvalue[k]
        x[(!is.na(x[, i]) & (x[, i] > max[k])), i] <- newvalue[k]
        x[(!is.na(x[, i]) & (x[, i] == isvalue[k])), i] <- newvalue[k]
    }
    return(x)
}


Yule2phi <- function (Q, m, n = NULL) 
{
    t <- Yule.inv(Q, m, n = n)
    return(phi(t, digits = 15))
}


levels2numeric <- function (x) 
{
    n.var <- ncol(x)
    for (item in 1:n.var) {
        if (is.factor(x[, item])) 
            x[, item] <- as.numeric(x[, item])
    }
    invisible(x)
}


sim.VSS <- function (ncases = 1000, nvariables = 16, nfactors = 4, meanloading = 0.5, 
    dichot = FALSE, cut = 0) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    if (dichot) {
        items <- (items[, 1:nvariables] >= cut)
        items <- items + 0
    }
    return(items)
}


circadian.sd <- function (angle, data = NULL, hours = TRUE, na.rm = TRUE) 
{
    if (!is.null(data)) 
        angle <- data[, angle]
    if (hours) {
        angle <- angle * 2 * pi/24
    }
    nvar <- dim(angle)[2]
    if (is.null(nvar)) 
        nvar <- 1
    x <- cos(angle)
    y <- sin(angle)
    if (nvar > 1) {
        mx <- colSums(x, na.rm = na.rm)
        my <- colSums(y, na.rm = na.rm)
        n.obs <- colSums(!is.na(angle))
    }
    else {
        mx <- sum(x, na.rm = na.rm)
        my <- sum(y, na.rm = na.rm)
        n.obs <- sum(!is.na(angle))
    }
    R <- sqrt(mx^2 + my^2)/n.obs
    mean.angle <- sign(my) * acos((mx/n.obs)/R)
    Rvar <- 1 - R
    sd <- sqrt(-2 * log(R))
    if (hours) {
        Rvar <- Rvar * 24/(pi * 2)
    }
    return(list(Rvar = Rvar, sd = sd, R = R))
}


p.rep.f <- function (F, df2, twotailed = FALSE) 
{
    p <- pf(F, 1, df2, lower.tail = FALSE)
    dprime = sqrt(4 * F/(df2))
    if (twotailed) 
        p <- 2 * p
    p.rep <- pnorm(qnorm(1 - p)/sqrt(2))
    if (twotailed) 
        p <- 2 * p
    r.equiv <- sqrt(F/(F + df2))
    return(list(p.rep = p.rep, dprime = dprime, prob = p, r.equiv = r.equiv))
}


pca <- function (r, nfactors = 1, residuals = FALSE, rotate = "varimax", 
    n.obs = NA, covar = FALSE, scores = TRUE, missing = FALSE, 
    impute = "median", oblique.scores = TRUE, method = "regression", 
    ...) 
{
    principal(r = r, nfactors = nfactors, residuals = residuals, 
        rotate = rotate, n.obs = n.obs, covar = covar, scores = scores, 
        missing = missing, impute = impute, oblique.scores = oblique.scores, 
        method = method, ...)
}


ICLUST <- function (r.mat, nclusters = 0, alpha = 3, beta = 1, beta.size = 4, 
    alpha.size = 3, correct = TRUE, correct.cluster = TRUE, reverse = TRUE, 
    beta.min = 0.5, output = 1, digits = 2, labels = NULL, cut = 0, 
    n.iterations = 0, title = "ICLUST", plot = TRUE, weighted = TRUE, 
    cor.gen = TRUE, SMC = TRUE, purify = TRUE, diagonal = FALSE) 
{
    iclust(r.mat, nclusters, alpha, beta, beta.size, alpha.size, 
        correct, correct.cluster, reverse, beta.min, output, 
        digits, labels, cut, n.iterations, title, plot, weighted, 
        cor.gen, SMC, purify, diagonal)
}


item.dichot <- function (nvar = 72, nsub = 500, circum = FALSE, xloading = 0.6, 
    yloading = 0.6, gloading = 0, xbias = 0, ybias = 0, low = 0, 
    high = 0) 
{
    avloading <- (xloading + yloading)/2
    errorweight <- sqrt(1 - (avloading^2 + gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) * xloading + xbias
    truey <- rnorm(nsub) * yloading + ybias
    if (circum) {
        radia <- seq(0, 2 * pi, len = nvar + 1)
        rad <- radia[which(radia < 2 * pi)]
    }
    else rad <- c(rep(0, nvar/4), rep(pi/2, nvar/4), rep(pi, 
        nvar/4), rep(3 * pi/2, nvar/4))
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    trueitem <- outer(truex, cos(rad)) + outer(truey, sin(rad))
    item <- gloading * g + trueitem + errorweight * error
    nvar2 <- nvar/2
    iteml <- (item[, (1:nvar2) * 2 - 1] >= low)
    itemh <- (item[, (1:nvar2) * 2] >= high)
    item <- cbind(iteml, itemh) + 0
    return(item)
}


tr <- function (m) 
{
    if (!is.matrix(m) | (dim(m)[1] != dim(m)[2])) 
        stop("m must be a square matrix")
    return(sum(diag(m), na.rm = TRUE))
}


corr.p <- function (r, n, adjust = "holm", alpha = 0.05) 
{
    cl <- match.call()
    if (missing(n)) 
        stop("The number of subjects must be specified")
    sym <- FALSE
    t <- (r * sqrt(n - 2))/sqrt(1 - r^2)
    p <- 2 * (1 - pt(abs(t), (n - 2)))
    p[p > 1] <- 1
    if (adjust != "none") {
        if (isSymmetric(unclass(p))) {
            sym <- TRUE
            lp <- upper.tri(p)
            pa <- p[lp]
            pa <- p.adjust(pa, adjust)
            p[upper.tri(p, diag = FALSE)] <- pa
        }
        else {
            p[] <- p.adjust(p, adjust)
            sym <- FALSE
        }
    }
    nvar <- ncol(r)
    if (sym) {
        z <- fisherz(r[lower.tri(r)])
    }
    else {
        z <- fisherz(r)
    }
    if (min(n) < 4) {
        warning("Number of subjects must be greater than 3 to find confidence intervals.")
    }
    if (is.matrix(n)) {
        se <- 1/sqrt(n[lower.tri(n)] - 3)
    }
    else {
        se <- 1/sqrt(n - 3)
    }
    alpha <- 1 - alpha/2
    dif <- qnorm(alpha)
    lower <- fisherz2r(z - dif * se)
    upper <- fisherz2r(z + dif * se)
    if (sym) {
        ci <- data.frame(lower = lower, r = r[lower.tri(r)], 
            upper = upper, p = p[lower.tri(p)])
    }
    else {
        ci <- data.frame(lower = as.vector(lower), r = as.vector(r), 
            upper = as.vector(upper), p = as.vector(p))
    }
    cnR <- abbreviate(colnames(r), minlength = 5)
    rnR <- abbreviate(rownames(r), minlength = 5)
    if (sym) {
        k <- 1
        for (i in 2:nvar) {
            for (j in 1:(i - 1)) {
                rownames(ci)[k] <- paste(cnR[j], cnR[i], sep = "-")
                k <- k + 1
            }
        }
    }
    else {
        k <- 1
        for (i in 1:ncol(r)) {
            for (j in 1:nrow(r)) {
                rownames(ci)[k] <- paste(rnR[j], cnR[i], sep = "-")
                k <- k + 1
            }
        }
    }
    result <- list(r = r, n = n, t = t, p = p, sym = sym, adjust = adjust, 
        ci = ci, Call = cl)
    class(result) <- c("psych", "corr.p")
    return(result)
}


esem <- function (r, varsX, varsY, nfX = 1, nfY = 1, n.obs = NULL, fm = "minres", 
    rotate = "oblimin", plot = TRUE, cor = "cor", use = "pairwise", 
    weight = NULL, ...) 
{
    cl <- match.call()
    vars <- c(varsX, varsY)
    if (is.null(n.obs)) 
        n.obs <- NA
    if (ncol(r) < nrow(r)) {
        n.obs <- nrow(r)
        switch(cor, cor = {
            r <- cor(r, use = use)
        }, cov = {
            r <- cov(r, use = use)
            covar <- TRUE
        }, wtd = {
            r <- cor.wt(r, w = weight)$r
        }, tet = {
            r <- tetrachoric(r)$rho
        }, poly = {
            r <- polychoric(r)$rho
        }, mixed = {
            r <- mixed.cor(r, use = use)$rho
        }, Yuleb = {
            r <- YuleCor(r, , bonett = TRUE)$rho
        }, YuleQ = {
            r <- YuleCor(r, 1)$rho
        }, YuleY = {
            r <- YuleCor(r, 0.5)$rho
        })
    }
    varnames <- colnames(r)[vars]
    R <- r[varnames, varnames]
    nX <- length(varsX)
    nY <- length(varsY)
    df1 <- nX * (nX - 1)/2 - nfX * nX + nfX * (nfX - 1)/2
    df2 <- nY * (nY - 1)/2 - nfY * nY + nfY * (nfY - 1)/2
    f1 <- fa.extend(R, nfX, ov = varsX, ev = varsY, fm = fm, 
        rotate = rotate, ...)
    loads1 <- f1$loadings[varnames, , drop = FALSE]
    S1 <- f1$Structure[varnames, , drop = FALSE]
    if (!is.null(ncol(S1))) 
        colnames(loads1) <- colnames(S1) <- paste0("X", 1:ncol(loads1))
    Phi1 <- f1$Phi
    f2 <- fa.extend(R, nfY, ov = varsY, ev = varsX, fm = fm, 
        rotate = rotate, ...)
    loads2 <- f2$loadings[varnames, , drop = FALSE]
    S2 <- f2$Structure[varnames, , drop = FALSE]
    if (!is.null(ncol(S2))) {
        colnames(loads2) <- colnames(S2) <- paste0("Y", 1:ncol(loads2))
    }
    Phi2 <- f2$Phi
    f12 <- cbind(loads1, loads2)
    S12 <- cbind(S1, S2)
    S12 <- as.matrix(S12)
    Phi <- t(S12) %*% solve(R) %*% S12
    loadsX <- f1$loadings[colnames(R)[varsX], , drop = FALSE]
    loadsY <- f2$loadings[colnames(R)[varsY], , drop = FALSE]
    diag(Phi) <- 1
    if (FALSE) {
        if (!is.null(Phi1)) 
            Phi[1:nfX, 1:nfX] <- Phi1
        if (!is.null(Phi2)) 
            Phi[(nfX + 1):(nfX + nfY), (nfX + 1):(nfX + nfY)] <- Phi2
    }
    result <- esem.stats(R, f12, S12, Phi, n.obs = n.obs)
    result$n.obs <- n.obs
    result$loadings <- f12
    result$Structure <- S12
    result$loadsX <- loadsX
    result$loadsY <- loadsY
    result$PhiX <- Phi1
    result$PhiY <- Phi2
    result$esem.dof <- df1 + df2
    result$fm <- fm
    result$fx <- f1$fo
    result$fy <- f2$fo
    result$Phi <- Phi
    result$Call <- cl
    class(result) <- c("psych", "esem")
    if (plot) 
        esem.diagram(result)
    return(result)
}


g2r <- function (g, df, n) 
{
    g/sqrt(g^2 + 4 * df/n)
}


plot.psych <- function (x, labels = NULL, ...) 
{
    result <- NULL
    vss <- iclust <- omega <- fa <- irt.fa <- irt.poly <- principal <- parallel <- set.cor <- residuals <- FALSE
    if (length(class(x)) > 1) {
        if (class(x)[2] == "irt.fa") 
            irt.fa <- TRUE
        if (class(x)[2] == "irt.poly") 
            irt.poly <- TRUE
        if (class(x)[2] == "vss") 
            vss <- TRUE
        if (class(x)[2] == "iclust") 
            iclust <- TRUE
        if (class(x)[2] == "fa") 
            fa <- TRUE
        if (class(x)[2] == "principal") 
            principal <- TRUE
        if (class(x)[2] == "vss") 
            vss <- TRUE
        if (class(x)[2] == "omega") 
            omega <- TRUE
        if (class(x)[2] == "parallel") 
            parallel <- TRUE
        if (class(x)[2] == "set.cor") 
            set.cor <- TRUE
        if (class(x)[2] == "residuals") 
            residuals <- TRUE
    }
    switch(class(x)[2], vss = {
        n = dim(x)
        symb = c(49, 50, 51, 52)
        plot(x$cfit.1, ylim = c(0, 1), type = "b", ylab = "Very Simple Structure Fit", 
            xlab = "Number of Factors", pch = 49)
        x$cfit.3 <- x$vss.stats$cfit.3
        x$cfit.4 <- x$vss.stats$cfit.4
        title <- x$title
        title(main = title)
        x$cfit.2[1] <- NA
        x$cfit.3[1] <- NA
        x$cfit.3[2] <- NA
        x$cfit.4[1] <- NA
        x$cfit.4[2] <- NA
        x$cfit.4[3] <- NA
        lines(x$cfit.2)
        points(x$cfit.2, pch = 50)
        lines(x$cfit.3)
        points(x$cfit.3, pch = symb[3])
        lines(x$cfit.4)
        points(x$cfit.4, pch = symb[4])
    }, iclust = {
        op <- par(no.readonly = TRUE)
        cut <- 0
        if (iclust) {
            load <- x$loadings
            cat("Use ICLUST.diagram to see the  hierarchical structure\n")
        } else {
            load <- x$schmid$orthog
            cat("Use omega.diagram to see the  hierarchical structure\n")
        }
        nc <- dim(load)[2]
        nvar <- dim(load)[1]
        ch.col = c("black", "blue", "red", "gray", "black", "blue", 
            "red", "gray")
        cluster <- rep(nc + 1, nvar)
        cluster <- apply(abs(load), 1, which.max)
        cluster[(apply(abs(load), 1, max) < cut)] <- nc + 1
        if (nc > 2) {
            pairs(load, pch = cluster + 19, col = ch.col[cluster], 
                bg = ch.col[cluster])
        } else {
            plot(load, pch = cluster + 20, col = ch.col[cluster], 
                bg = ch.col[cluster], ...)
            abline(h = 0)
            abline(v = 0)
        }
        if (is.null(labels)) labels <- paste(1:nvar)
        if (nc < 3) text(load, labels, pos = 1)
        par(op)
    }, omega = {
        op <- par(no.readonly = TRUE)
        cut <- 0
        if (iclust) {
            load <- x$loadings
            cat("Use ICLUST.diagram to see the  hierarchical structure\n")
        } else {
            load <- x$schmid$orthog
            cat("Use omega.diagram to see the  hierarchical structure\n")
        }
        nc <- dim(load)[2]
        nvar <- dim(load)[1]
        ch.col = c("black", "blue", "red", "gray", "black", "blue", 
            "red", "gray")
        cluster <- rep(nc + 1, nvar)
        cluster <- apply(abs(load), 1, which.max)
        cluster[(apply(abs(load), 1, max) < cut)] <- nc + 1
        if (nc > 2) {
            pairs(load, pch = cluster + 19, cex = 1.5, col = ch.col[cluster], 
                bg = ch.col[cluster])
        } else {
            plot(load, pch = cluster + 20, col = ch.col[cluster], 
                bg = ch.col[cluster], ...)
            abline(h = 0)
            abline(v = 0)
        }
        if (is.null(labels)) labels <- paste(1:nvar)
        if (nc < 3) text(load, labels, pos = 1)
        par(op)
    }, set.cor = {
        plot(x$cancor2, typ = "b", ylab = "Squared Canonical Correlation", 
            xlab = "Canonical variate", main = "Scree of canonical correlations", 
            ylim = c(0, 1), ...)
    }, irt.fa = {
        result <- plot.irt(x, labels = labels, ...)
    }, irt.poly = {
        result <- plot.poly(x, labels = labels, ...)
    }, fa = {
        fa.plot(x, labels = labels, ...)
    }, principal = {
        fa.plot(x, labels = labels, ...)
    }, parallel = {
        plot.fa.parallel(x, ...)
    }, residuals = {
        plot.residuals(x, ...)
    })
    if (!is.null(result)) {
        class(result) <- c("psych", "polyinfo")
        invisible(result)
    }
}


test.psych <- function (first = 1, last = 5, short = TRUE, all = FALSE, fapc = FALSE) 
{
    s1 <- datasets::USArrests
    s2 <- datasets::attitude
    s3 <- datasets::Harman23.cor$cov
    s4 <- datasets::Harman74.cor$cov
    s5 <- datasets::ability.cov$cov
    d5 <- diag(1/sqrt(diag(s5)))
    s5 <- d5 %*% s5 %*% d5
    datasets <- list(s1, s2, s3, s4, s5)
    out <- list()
    for (i in first:last) {
        test.data <- datasets[[i]]
        pc <- principal(test.data)
        pc2 <- principal(test.data, 2)
        if (i < 3) {
            fa2 <- fa(test.data, 2)
            fp <- fa.parallel(test.data)
            vss2 <- VSS(test.data)
            vsspc <- VSS(test.data, fm = "pc")
        }
        else {
            fa2 <- fa(test.data, 2, n.obs = 200)
            cluster.plot(fa2)
            fp <- fa.parallel(test.data, n.obs = 200)
            vss2 <- VSS(test.data, n.obs = 200)
            vsspc <- VSS(test.data, fm = "pc", n.obs = 200)
        }
        ic <- ICLUST(test.data, plot = FALSE)
        if (requireNamespace("GPArotation")) {
            om <- omega(test.data, plot = FALSE)
        }
        else {
            warning("Omega requires the GPArotation package to be loaded")
            om <- NULL
        }
        fc <- factor.congruence(pc2, fa2)
        d <- describe(test.data)
        keys <- matrix(rep(0, dim(test.data)[2] * 2), ncol = 2)
        keys[, 1] <- 1
        keys[1:3, 2] <- 1
        if (dim(test.data)[1] != dim(test.data)[2]) {
            test.score <- scoreItems(keys, test.data)
        }
        else {
            test.score <- cluster.cor(keys, test.data)
        }
        out <- list(out, paste("test", i), pc, pc2, fa2, fp, 
            ic, om, fc, vss2, vsspc, d, test.score)
    }
    cat("\n Testing cor plot and sim.item\n")
    set.seed(42)
    simple <- sim.item(nvar = 24)
    circ <- sim.circ(nvar = 24)
    cor.plot(cor(circ), colors = TRUE, zlim = c(-1, 1), main = "24 variables in a circumplex")
    simple.par <- fa.parallel(simple)
    fa.simple <- fa(simple, 2)
    cor.plot(fa.simple, TRUE, n = 4)
    fa.simple.keys <- factor2cluster(fa.simple)
    simple.scores <- scoreItems(fa.simple.keys, simple)
    pairs.panels(simple.scores$scores)
    cat("\n Test of sim.VSS\n")
    f4 <- sim.VSS()
    psych.d <- NULL
    cong <- sim.congeneric()
    if (all) {
        cat("\n Test of a singular matrix\n")
        IRIS <- datasets::iris[, 1:4]
        IRIS[, 5] <- datasets::iris[, 1] + datasets::iris[, 2]
        f.iris <- fa(IRIS, 5, scores = TRUE)
        p.iris <- principal(IRIS, 5, scores = TRUE)
    }
    cluster.plot(fa(sim.circ(nvar = 24), nfactors = 2), title = "two circumplex factors")
    pairs.panels(cong)
    if (FALSE) {
        fa.graph(fa(item.sim(16), 2), title = "Principal factor of a simple structure")
        ic.out <- ICLUST(s4, title = "ICLUST of 24 Mental abilities")
        v9 <- omega(sim.hierarchical(), title = "Omega with Schmid Leihman")
        omega.graph(v9, sl = FALSE, title = "Omega with hierarchical factors")
        X6 <- matrix(c("a", "b", "c", rep(0, 6), "d", "e", "f"), 
            nrow = 6)
        colnames(X6) <- c("L1", "L2")
        rownames(X6) <- c("x1", "x2", "x3", "x4", "x5", "x6")
        Y3 <- matrix(c("u", "w", "z"), ncol = 1)
        colnames(Y3) <- "Y"
        rownames(Y3) <- c("y1", "y2", "y3")
        phi21 <- matrix(c(1, 0, "r1", 0, 1, "r2", 0, 0, 1), ncol = 3)
        colnames(phi21) <- rownames(phi21) <- c("L1", "L2", "Y")
        structure.graph(X6, phi21, Y3, title = "Symbolic structural model")
    }
    else {
        warning("fa.graph, omega.graph, ICLUST.rgraph, structure.graph require Rgraphviz and were not tested")
    }
    fa.diagram(fa(item.sim(16), nfactors = 2))
    ic.out <- ICLUST(s4, title = "ICLUST of 24 Mental abilities")
    v9 <- omega(sim.hierarchical(), title = "Omega with Schmid Leihman")
    omega.diagram(v9, sl = FALSE, main = "Omega with hierarchical factors")
    X6 <- matrix(c("a", "b", "c", rep(0, 6), "d", "e", "f"), 
        nrow = 6)
    colnames(X6) <- c("L1", "L2")
    rownames(X6) <- c("x1", "x2", "x3", "x4", "x5", "x6")
    Y3 <- matrix(c("u", "w", "z"), ncol = 1)
    colnames(Y3) <- "Y"
    rownames(Y3) <- c("y1", "y2", "y3")
    phi21 <- matrix(c(1, 0, "r1", 0, 1, "r2", 0, 0, 1), ncol = 3)
    colnames(phi21) <- rownames(phi21) <- c("L1", "L2", "Y")
    example.model <- structure.diagram(X6, phi21, Y3, main = "Symbolic structural model")
    R <- cor(sim.item(16))
    ss <- c(1, 3, 5, 7, 9, 11, 13, 15)
    f <- fa(R[ss, ss], 2)
    foe <- fa.extension(R[ss, -ss], f)
    fa.diagram(fa.results = f, fe.results = foe)
    if (fapc) {
        data1 <- psych::bfi
        f3 <- fa(data1[1:15], 3, n.iter = 5)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "Varimax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "varimax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "quartimax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "bentlerT")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "geominT")
        Target <- matrix(c(rep(1, 5), rep(0, 15), rep(1, 5), 
            rep(0, 15), rep(1, 5)), ncol = 3)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "targetT", 
            Target = Target)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "bifactor")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "TargetT", 
            Target = Target)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "equamax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "varimin")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "Promax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "promax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "cluster")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "biquartimin")
        Targ <- make.keys(15, list(f1 = 1:5, f2 = 6:10, f3 = 11:15))
        Targ <- scrub(Targ, isvalue = 1)
        Targ <- list(Targ)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "TargetQ", 
            Target = Targ)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "oblimin")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "quartimin")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "simplimax")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "geominQ")
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "targetQ", 
            Target = Target)
        f3 <- fa(data1[1:15], 3, n.iter = 5, rotate = "bentlerQ")
        data2 <- psych::ability
        f1 <- fa(data2)
        fpoly <- fa(data2[1:10], 2, n.iter = 5, cor = "poly")
        f1 <- fa(data2, n.iter = 4)
        f1p <- fa(data2, n.iter = 4, cor = "tet")
        p3 <- principal(data1[1:15], 3, n.iter = 1)
        p3 <- principal(data1[1:15], 3, rotate = "Varimax")
        p3 <- principal(data1[1:15], 3, rotate = "varimax")
        p3 <- principal(data1[1:15], 3, rotate = "quartimax")
        p3 <- principal(data1[1:15], 3, rotate = "bentlerT")
        p3 <- principal(data1[1:15], 3, rotate = "geominT")
        Target <- matrix(c(rep(1, 5), rep(0, 15), rep(1, 5), 
            rep(0, 15), rep(1, 5)), ncol = 3)
        p3 <- principal(data1[1:15], 3, rotate = "targetT", Target = Target)
        p3 <- principal(data1[1:15], 3, rotate = "TargetT", Target = Target)
        p3 <- principal(data1[1:15], 3, rotate = "bifactor")
        p3 <- principal(data1[1:15], 3, rotate = "varimin")
        p3 <- principal(data1[1:15], 3, rotate = "bentlerT")
        p3 <- principal(data1[1:15], 3, rotate = "geominT")
        p3 <- principal(data1[1:15], 3, rotate = "equamax")
        p3 <- principal(data1[1:15], 3, rotate = "Promax")
        p3 <- principal(data1[1:15], 3, rotate = "promax")
        p3 <- principal(data1[1:15], 3, rotate = "cluster")
        p3 <- principal(data1[1:15], 3, rotate = "biquartimin")
        p3 <- principal(data1[1:15], 3, rotate = "equamax")
        Targ <- make.keys(15, list(f1 = 1:5, f2 = 6:10, f3 = 11:15))
        Targ <- scrub(Targ, isvalue = 1)
        Targ <- list(Targ)
        p3 <- principal(data1[1:15], 3, rotate = "TargetQ", Target = Targ)
        p3 <- principal(data1[1:15], 3, rotate = "oblimin")
        p3 <- principal(data1[1:15], 3, rotate = "quartimin")
        p3 <- principal(data1[1:15], 3, rotate = "simplimax")
        p3 <- principal(data1[1:15], 3, rotate = "geominQ")
        p3 <- principal(data1[1:15], 3, rotate = "biquartimin")
        p3 <- principal(data1[1:15], 3, rotate = "targetQ", Target = Target)
        p3 <- principal(data1[1:15], 3, rotate = "bentlerQ")
        fpoly <- principal(data1[1:10], 2, cor = "poly")
        f1 <- principal(data2)
        f1p <- principal(data2, cor = "tet")
    }
    out <- list(out, fa.simple, psych.d)
    if (!short) {
        return(out)
    }
}


circular.mean <- function (angle, na.rm = TRUE) 
{
    x <- cos(angle)
    y <- sin(angle)
    if (is.vector(angle)) {
        mx <- mean(x, na.rm = na.rm)
        my <- mean(y, na.rm = na.rm)
    }
    else {
        mx <- colMeans(x, na.rm = na.rm)
        my <- colMeans(y, na.rm = na.rm)
    }
    mean.angle <- sign(my) * acos((mx)/sqrt(mx^2 + my^2))
    return(mean.angle)
}


pairs.panels <- function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE, 
    digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE, 
    jiggle = FALSE, factor = 2, hist.col = "cyan", show.points = TRUE, 
    rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, ...) 
{
    "panel.hist.density" <- function(x, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5))
        h <- hist(x, breaks = breaks, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
        if (density) {
            tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd", 
                adjust = 1.2), silent = TRUE)
            if (class(tryd) != "try-error") {
                d$y <- d$y/max(d$y)
                lines(d)
            }
        }
        if (rug) 
            rug(x)
    }
    "panel.cor" <- function(x, y, digits = 2, prefix = "", ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        if (is.null(wt)) {
            r <- cor(x, y, use = "pairwise", method = method)
        }
        else {
            r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1, 
                2]
        }
        txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        cex <- cex.cor * 0.8/strwidth(txt)
        if (scale) {
            cex1 <- cex * abs(r)
            if (cex1 < 0.25) 
                cex1 <- 0.25
            text(0.5, 0.5, txt, cex = cex1)
        }
        else {
            text(0.5, 0.5, txt, cex = cex)
        }
    }
    "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red", 
        span = 2/3, iter = 3, ...) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        if (show.points) 
            points(x, y, pch = pch, ...)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                col = col.smooth, ...)
        panel.ellipse1(xm, ym, xs, ys, r, col.smooth = col.smooth, 
            ...)
    }
    "panel.smoother.no.noellipse" <- function(x, y, pch = par("pch"), 
        col.smooth = "red", span = 2/3, iter = 3, ...) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        if (show.points) 
            points(x, y, pch = pch, ...)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                col = col.smooth, ...)
    }
    "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red", 
        ...) {
        ymin <- min(y)
        ymax <- max(y)
        xmin <- min(x)
        xmax <- max(x)
        ylim <- c(min(ymin, xmin), max(ymax, xmax))
        xlim <- ylim
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        points(x, y, pch = pch, ylim = ylim, xlim = xlim, ...)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
            abline(lm(y[ok] ~ x[ok]), col = col.lm, ...)
    }
    "panel.lm.ellipse" <- function(x, y, pch = par("pch"), col.lm = "red", 
        ...) {
        ymin <- min(y)
        ymax <- max(y)
        xmin <- min(x)
        xmax <- max(x)
        ylim <- c(min(ymin, xmin), max(ymax, xmax))
        xlim <- ylim
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        points(x, y, pch = pch, ylim = ylim, xlim = xlim, ...)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
            abline(lm(y[ok] ~ x[ok]), col = col.lm, ...)
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        panel.ellipse1(xm, ym, xs, ys, r, col.smooth = col.lm, 
            ...)
    }
    "panel.ellipse1" <- function(x = 0, y = 0, xs = 1, ys = 1, 
        r = 0, col.smooth, add = TRUE, segments = 51, ...) {
        angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
        if (!is.na(r)) {
            if (abs(r) > 0) 
                theta <- sign(r)/sqrt(2)
            else theta = 1/sqrt(2)
            shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                theta, -theta, theta), ncol = 2, byrow = TRUE)
            ellipse <- unit.circle %*% shape
            ellipse[, 1] <- ellipse[, 1] * xs + x
            ellipse[, 2] <- ellipse[, 2] * ys + y
            points(x, y, pch = 19, col = col.smooth, cex = 1.5)
            lines(ellipse, ...)
        }
    }
    "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red", 
        ...) {
        segments = 51
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        if (show.points) 
            points(x, y, pch = pch, ...)
        angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
        if (!is.na(r)) {
            if (abs(r) > 0) 
                theta <- sign(r)/sqrt(2)
            else theta = 1/sqrt(2)
            shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                theta, -theta, theta), ncol = 2, byrow = TRUE)
            ellipse <- unit.circle %*% shape
            ellipse[, 1] <- ellipse[, 1] * xs + xm
            ellipse[, 2] <- ellipse[, 2] * ys + ym
            points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
            if (ellipses) 
                lines(ellipse, ...)
        }
    }
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    if (missing(cex.cor)) 
        cex.cor <- 1
    for (i in 1:ncol(x)) {
        if (is.character(x[[i]])) {
            x[[i]] <- as.numeric(as.factor(x[[i]]))
            colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
        }
    }
    if (!lm) {
        if (smooth) {
            if (ellipses) {
                pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
                  lower.panel = panel.smoother, pch = pch, ...)
            }
            else {
                pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
                  lower.panel = panel.smoother.no.noellipse, 
                  pch = pch, ...)
            }
        }
        else {
            pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
                lower.panel = panel.ellipse, pch = pch, ...)
        }
    }
    else {
        if (!cor) {
            if (ellipses) {
                pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm.ellipse, 
                  lower.panel = panel.lm.ellipse, pch = pch, 
                  ...)
            }
            else {
                pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm, 
                  lower.panel = panel.lm, pch = pch, ...)
            }
        }
        else {
            if (ellipses) {
                pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
                  lower.panel = panel.lm.ellipse, pch = pch, 
                  ...)
            }
            else {
                pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
                  lower.panel = panel.lm, pch = pch, ...)
            }
        }
    }
}


winsor.var <- function (x, trim = 0.2, na.rm = TRUE) 
{
    if (is.vector(x)) {
        ans <- win.var(x, trim = trim, na.rm = na.rm)
    }
    else {
        if (is.matrix(x) | is.data.frame(x)) {
            ans <- apply(x, 2, win.var, trim = trim, na.rm = na.rm)
        }
    }
    return(ans)
}


chi2r <- function (chi, n) 
{
    sqrt(chi/n)
}


con2cat <- function (old, cuts = c(0, 1, 2, 3), where) 
{
    new <- old
    nc <- length(cuts)
    if (missing(where)) 
        where <- 1:ncol(old)
    lw <- length(where)
    if (is.matrix(cuts)) {
        mcuts <- cuts
    }
    else {
        mcuts <- matrix(rep(cuts, lw), nrow = lw, byrow = TRUE)
    }
    vwhere <- as.vector(where)
    for (w in 1:lw) {
        where <- vwhere[w]
        cuts <- mcuts[w, ]
        nc <- length(cuts)
        if (nc < 2) {
            new[(!is.na(new[, where]) & (old[, where] <= cuts)), 
                where] <- 0
            new[(!is.na(new[, where]) & (old[, where] > cuts)), 
                where] <- 1
        }
        else {
            new[(!is.na(new[, where]) & (old[, where] <= cuts[1])), 
                where] <- 0
            for (i in (2:nc)) {
                new[(!is.na(new[, where]) & (old[, where] > cuts[i - 
                  1])), where] <- i - 1
            }
            new[(!is.na(new[, where]) & (old[, where] > cuts[nc])), 
                where] <- nc
        }
    }
    new
}


phi2poly.matrix <- function (x, v) 
{
    .Deprecated("phi2tetra", msg = "This function has been replaced by phi2tetra")
}


thurstone <- function (x, ranks = FALSE, digits = 2) 
{
    cl <- match.call()
    if (ranks) {
        choice <- choice.mat(x)
    }
    else {
        if (is.matrix(x)) 
            choice <- x
        choice <- as.matrix(x)
    }
    scale.values <- colMeans(qnorm(choice)) - min(colMeans(qnorm(choice)))
    model <- pnorm(-scale.values %+% t(scale.values))
    error <- model - choice
    fit <- 1 - (sum(error * error)/sum(choice * choice))
    result <- list(scale = round(scale.values, digits), GF = fit, 
        residual = error, Call = cl)
    class(result) <- c("psych", "thurstone")
    return(result)
}


dia.cone <- function (x = 0, y = -2, theta = 45, arrow = TRUE, curves = TRUE, 
    add = FALSE, labels = NULL, xlim = c(-1, 1), ylim = c(-1, 
        1), ...) 
{
    segments = 51
    extend = 1.1
    xrange = 2
    height = xrange
    xs <- tan(theta * pi/180) * height
    ys = 0.3 * xs
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- unit.circle
    ellipse[, 1] <- ellipse[, 1] * xs + x
    ellipse[, 2] <- ellipse[, 2] * ys + y + height
    if (!add) {
        plot.new()
        plot.window(xlim = xlim * 2, ylim = ylim, ...)
    }
    lines(ellipse, ...)
    if (arrow) {
        arrows(x, y, (x - xs), y + height, lty = "dashed")
        arrows(x, y, (x + xs), y + height, lty = "dashed")
        arrows(x, y, x, y + extend^2 * height)
    }
    else {
        coords <- matrix(c(x, x - xs, y, y + height), 2, 2)
        lines(coords)
        coords <- matrix(c(x, x + xs, y, y + height), 2, 2)
        lines(coords)
    }
    if (curves) {
        dia.curve(c(x, y + height/3), c(x - xs/3, y + height/3), 
            scale = 0.2, labels = labels[1])
        dia.curve(c(x, y + height/3), c(x + xs/3, y + height/3), 
            scale = 0.2, labels = labels[2])
        dia.curve(c(x - xs/2, y + height/2), c(x + xs/2, y + 
            height/2), scale = 0.3, labels = labels[3])
    }
}


fa.diagram <- function (fa.results, Phi = NULL, fe.results = NULL, sort = TRUE, 
    labels = NULL, cut = 0.3, simple = TRUE, errors = FALSE, 
    g = FALSE, digits = 1, e.size = 0.05, rsize = 0.15, side = 2, 
    main, cex = NULL, marg = c(0.5, 0.5, 1, 0.5), adj = 1, ...) 
{
    old.par <- par(mar = marg)
    on.exit(par(old.par))
    col <- c("black", "red")
    if (missing(main)) 
        if (is.null(fe.results)) {
            main <- "Factor Analysis"
        }
        else {
            main <- "Factor analysis and extension"
        }
    if (!is.matrix(fa.results) && !is.null(fa.results$fa) && 
        is.list(fa.results$fa)) 
        fa.results <- fa.results$fa
    if (is.null(cex)) 
        cex <- 1
    if (sort) {
        fa.results <- fa.sort(fa.results)
        if (!is.null(fe.results)) {
            fe.results <- fa.sort(fe.results)
        }
    }
    if ((!is.matrix(fa.results)) && (!is.data.frame(fa.results))) {
        factors <- as.matrix(fa.results$loadings)
        if (!is.null(fa.results$Phi)) {
            Phi <- fa.results$Phi
        }
        else {
            if (!is.null(fa.results$cor)) {
                Phi <- fa.results$cor
            }
        }
    }
    else {
        factors <- fa.results
    }
    nvar <- dim(factors)[1]
    if (is.null(nvar)) {
        nvar <- length(factors)
        num.factors <- 1
    }
    else {
        num.factors <- dim(factors)[2]
    }
    nvar <- dim(factors)[1]
    e.size = e.size * 16 * cex/nvar
    if (is.null(nvar)) {
        nvar <- length(factors)
        num.factors <- 1
    }
    else {
        num.factors <- dim(factors)[2]
    }
    if (is.null(rownames(factors))) {
        rownames(factors) <- paste("V", 1:nvar, sep = "")
    }
    if (is.null(colnames(factors))) {
        colnames(factors) <- paste("F", 1:num.factors, sep = "")
    }
    var.rect <- list()
    fact.rect <- list()
    max.len <- max(nchar(rownames(factors))) * rsize
    x.max <- max((nvar + 1), 6)
    limx = c(-max.len/2, x.max)
    n.evar <- 0
    if (!is.null(fe.results)) {
        n.evar <- dim(fe.results$loadings)[1]
        limy <- c(0, max(nvar + 1, n.evar + 1))
    }
    else {
        limy = c(0, nvar + 1)
    }
    top <- max(nvar, n.evar) + 1
    plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
        axes = FALSE, ylab = "", xlab = "", main = main, ...)
    max.len <- max(strwidth(rownames(factors)), strwidth("abc"))/1.8
    limx = c(-max.len/2, x.max)
    cex <- min(cex, 20/x.max)
    if (g) {
        left <- 0.3 * x.max
        middle <- 0.6 * x.max
        gf <- 2
    }
    else {
        left <- 0
        middle <- 0.5 * x.max
        gf <- 1
    }
    for (v in 1:nvar) {
        var.rect[[v]] <- dia.rect(left, top - v - max(0, n.evar - 
            nvar)/2, rownames(factors)[v], xlim = limx, ylim = limy, 
            cex = cex, ...)
    }
    f.scale <- (top)/(num.factors + 1)
    f.shift <- max(nvar, n.evar)/num.factors
    if (g) {
        fact.rect[[1]] <- dia.ellipse(-max.len/2, top/2, colnames(factors)[1], 
            xlim = limx, ylim = limy, e.size = e.size, cex = cex, 
            ...)
        for (v in 1:nvar) {
            if (simple && (abs(factors[v, 1]) == max(abs(factors[v, 
                ]))) && (abs(factors[v, 1]) > cut) | (!simple && 
                (abs(factors[v, 1]) > cut))) {
                dia.arrow(from = fact.rect[[1]], to = var.rect[[v]]$left, 
                  labels = round(factors[v, 1], digits), col = ((sign(factors[v, 
                    1]) < 0) + 1), lty = ((sign(factors[v, 1]) < 
                    0) + 1))
            }
        }
    }
    for (f in gf:num.factors) {
        fact.rect[[f]] <- dia.ellipse(left + middle, (num.factors + 
            gf - f) * f.scale, colnames(factors)[f], xlim = limx, 
            ylim = limy, e.size = e.size, cex = cex, ...)
        for (v in 1:nvar) {
            if (simple && (abs(factors[v, f]) == max(abs(factors[v, 
                ]))) && (abs(factors[v, f]) > cut) | (!simple && 
                (abs(factors[v, f]) > cut))) {
                dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$right, 
                  labels = round(factors[v, f], digits), col = ((sign(factors[v, 
                    f]) < 0) + 1), lty = ((sign(factors[v, f]) < 
                    0) + 1), adj = f%%adj + 1, cex = cex)
            }
        }
    }
    if (!is.null(Phi) && (ncol(Phi) > 1)) {
        for (i in 2:num.factors) {
            for (j in 1:(i - 1)) {
                if (abs(Phi[i, j]) > cut) {
                  dia.curve(from = fact.rect[[j]]$right, to = fact.rect[[i]]$right, 
                    labels = round(Phi[i, j], digits), scale = (i - 
                      j), cex = cex, ...)
                }
            }
        }
    }
    if (errors) {
        for (v in 1:nvar) {
            dia.self(location = var.rect[[v]], scale = 0.5, side = side)
        }
    }
    if (!is.null(fe.results)) {
        e.loadings <- fe.results$loadings
        for (v in 1:n.evar) {
            var.rect[[v]] <- dia.rect(x.max, top - v - max(0, 
                nvar - n.evar)/2, rownames(e.loadings)[v], xlim = limx, 
                ylim = limy, cex = cex, ...)
            for (f in 1:num.factors) {
                if (simple && (abs(e.loadings[v, f]) == max(abs(e.loadings[v, 
                  ]))) && (abs(e.loadings[v, f]) > cut) | (!simple && 
                  (abs(e.loadings[v, f]) > cut))) {
                  dia.arrow(from = fact.rect[[f]], to = var.rect[[v]]$left, 
                    labels = round(e.loadings[v, f], digits), 
                    col = ((sign(e.loadings[v, f]) < 0) + 1), 
                    lty = ((sign(e.loadings[v, f]) < 0) + 1), 
                    adj = f%%adj + 1)
                }
            }
        }
    }
}


radar <- function (x, labels = NULL, center = FALSE, connect = FALSE, 
    scale = 1, ncolors = 31, fill = FALSE, add = FALSE, linetyp = "solid", 
    main = "Radar Plot", ...) 
{
    nvar <- length(x)
    if (is.null(labels)) 
        labels <- paste("V", 1:nvar, sep = "")
    SEGMENTS <- 48
    if (ncolors < 2) {
        colors <- FALSE
    }
    else {
        colors <- TRUE
    }
    angles <- (0:SEGMENTS) * 2 * pi/SEGMENTS
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!add) {
        plot(unit.circle, typ = "l", asp = 1, axes = FALSE, xlab = "", 
            ylab = "", main = main)
        lines(unit.circle * 0.25, typ = "l", lty = "dotted", 
            col = "red")
        lines(unit.circle * 0.5, typ = "l", lty = "dotted")
        lines(unit.circle * 0.75, typ = "l", lty = "dotted", 
            col = "blue")
    }
    if (colors) {
        gr <- colorRampPalette(c("red", "white", "blue"))
        colramp <- gr(ncolors)
    }
    else {
        colramp <- grey((ncolors:0)/ncolors)
    }
    for (c in 1:nvar) {
        nx <- (c - 1) * SEGMENTS/nvar + 1
        if (center) {
            x0 <- unit.circle[nx, 1] * 0.5
            y0 <- unit.circle[nx, 2] * 0.5
        }
        else {
            x0 <- 0
            y0 <- 0
        }
        scaler <- (x[c] * scale/2 + 0.5)
        x1 <- unit.circle[nx, 1]
        y1 <- unit.circle[nx, 2]
        Lx <- c(x0, x1) * scaler
        Ly <- c(y0, y1) * scaler
        if (c == 1) {
            Oldx <- unit.circle[(nvar - 1) * SEGMENTS/nvar + 
                1, 1] * (x[nvar] * scale/2 + 0.5)
            Oldy <- unit.circle[(nvar - 1) * SEGMENTS/nvar + 
                1, 2] * (x[nvar] * scale/2 + 0.5)
        }
        if (colors) {
            if (scaler < 0.5) {
                col = "red"
            }
            else {
                col = "blue"
            }
            lines(Lx, Ly, col = col, ...)
        }
        else {
            lines(Lx, Ly, ...)
        }
        if (connect) {
            lines(c(Oldx, x1 * scaler), c(Oldy, y1 * scaler), 
                lty = linetyp)
        }
        if (fill) {
            polygon(c(0, Oldx, x1 * scaler, 0), c(0, Oldy, y1 * 
                scaler, 0), col = colramp[ceiling(scaler * ncolors)], 
                ...)
        }
        Oldx <- x1 * scaler
        Oldy <- y1 * scaler
        text(x1 * 1.05, y1 * 1.05, labels[c])
    }
}


polydi <- function (p, d, taup, taud, global = TRUE, ML = FALSE, std.err = FALSE, 
    weight = NULL, progress = TRUE, na.rm = TRUE, delete = TRUE, 
    correct = 0.5) 
{
    myfun <- function(x, i, j, correct, taup, taud, gminx, gmaxx, 
        gminy, gmaxy, np) {
        polyc(x[, i], x[, j], taup[, i], taud[j - np], global = TRUE, 
            weight = weight, correct = correct, gminx = gminx, 
            gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy)
    }
    matpLower <- function(x, np, nd, taup, taud, gminx, gmaxx, 
        gminy, gmaxy) {
        k <- 1
        il <- vector()
        jl <- vector()
        for (i in 1:np) {
            for (j in 1:nd) {
                il[k] <- i
                jl[k] <- j
                k <- k + 1
            }
        }
        poly <- mcmapply(function(i, j) myfun(x, i, j, correct = correct, 
            taup = taup, taud = taud, gminx = gminx, gmaxx = gmaxx, 
            gminy = gminy, gmaxy = gmaxy, np = np), il, jl + 
            np)
        mat <- matrix(np, nd)
        mat <- as.numeric(poly[1, ])
        return(mat)
    }
    if (!is.null(weight)) {
        if (length(weight) != nrow(x)) {
            stop("length of the weight vector must match the number of cases")
        }
    }
    cl <- match.call()
    np <- dim(p)[2]
    nd <- dim(d)[2]
    if (is.null(np)) 
        np <- 1
    if (is.null(nd)) 
        nd <- 1
    nsub <- dim(p)[1]
    p <- as.matrix(p)
    d <- as.matrix(d)
    pt <- table(p)
    nvalues <- max(p, na.rm = TRUE) - min(p, na.rm = TRUE) + 
        1
    dt <- table(d)
    if (length(dt) != 2) 
        stop("You did not supply a dichotomous variable")
    if (nvalues > 8) 
        stop("You have more than 8 categories for your items, polychoric is probably not needed")
    item.var <- apply(p, 2, sd, na.rm = na.rm)
    bad <- which((item.var <= 0) | is.na(item.var))
    if ((length(bad) > 0) & delete) {
        for (baddy in 1:length(bad)) {
            message("Item = ", colnames(p)[bad][baddy], " had no variance and was deleted")
        }
        p <- p[, -bad]
        np <- np - length(bad)
    }
    pmin <- apply(p, 2, function(x) min(x, na.rm = TRUE))
    gminx <- min(pmin)
    p <- t(t(p) - gminx + 1)
    dmin <- apply(d, 2, function(x) min(x, na.rm = TRUE))
    gminy <- min(dmin)
    d <- t(t(d) - gminy + 1)
    pmax <- apply(p, 2, function(x) max(x, na.rm = TRUE))
    gmaxx <- max(pmax)
    if (min(pmax) != max(pmax)) {
        message("The items do not have an equal number of response alternatives, I am using the largest number of categories in the polytomous set")
    }
    gmaxy <- max(apply(d, 2, function(x) max(x, na.rm = TRUE)))
    pfreq <- apply(p, 2, tabulate, nbins = nvalues)
    n.obs <- colSums(pfreq)
    pfreq <- t(t(pfreq)/n.obs)
    taup <- qnorm(apply(pfreq, 2, cumsum))[1:(nvalues - 1), ]
    rownames(taup) <- names(pt)[1:(nvalues - 1)]
    colnames(taup) <- colnames(p)
    dfreq <- apply(d, 2, tabulate, nbins = 2)
    if (nd < 2) {
        n.obsd <- sum(dfreq)
    }
    else {
        n.obsd <- colSums(dfreq)
    }
    dfreq <- t(t(dfreq)/n.obsd)
    taud <- qnorm(apply(dfreq, 2, cumsum))
    mat <- matrix(0, np, nd)
    rownames(mat) <- colnames(p)
    colnames(mat) <- colnames(d)
    x <- cbind(p, d)
    mat <- matpLower(x, np, nd, taup, taud, gminx, gmaxx, gminy, 
        gmaxy)
    mat <- matrix(mat, np, nd, byrow = TRUE)
    rownames(mat) <- colnames(p)
    colnames(mat) <- colnames(d)
    taud <- t(taud)
    result <- list(rho = mat, tau = taud, n.obs = nsub, Call = cl)
    class(result) <- c("psych", "polydi")
    return(result)
}


plot.irt <- function (x, xlab, ylab, main, D, type = c("ICC", "IIC", "test"), 
    cut = 0.3, labels = NULL, keys = NULL, xlim, ylim, y2lab, 
    lncol = "black", ...) 
{
    if (class(x)[2] == "irt.poly") {
        if (missing(type)) 
            type = "IIC"
        plot.poly(x = x, D = D, xlab = xlab, ylab = ylab, xlim = xlim, 
            ylim = ylim, main = main, type = type, cut = cut, 
            labels = labels, keys = keys, y2lab = y2lab, lncol = lncol, 
            ...)
    }
    else {
        item <- x
        temp <- list()
        sumtemp <- list()
        byKeys <- FALSE
        if ((is.data.frame(x)) | (is.matrix(x))) {
            nf <- dim(x)[2] - 1
        }
        else {
            nf <- length(x$irt$difficulty)
        }
        if (!is.null(keys)) {
            nkeys = ncol(keys)
            if (nf < nkeys) {
                byKeys <- TRUE
                nf <- nkeys
            }
        }
        for (f in 1:nf) {
            if ((is.data.frame(item)) | (is.matrix(item))) {
                if (byKeys) {
                  discrimination <- item[, 1]
                }
                else {
                  discrimination <- item[, f]
                }
                if (byKeys) {
                  location <- item[, 2]
                }
                else {
                  location <- item[, f + 1]
                }
            }
            else {
                if (byKeys) {
                  discrimination = item$irt$discrimination[, 
                    1]
                }
                else {
                  discrimination = item$irt$discrimination[, 
                    f]
                }
                if (!is.null(keys)) 
                  discrimination <- discrimination * abs(keys[, 
                    f])
                if (byKeys) {
                  location = item$irt$difficulty[[1]]
                }
                else {
                  location = item$irt$difficulty[[f]]
                }
            }
            x <- NULL
            nvar <- length(discrimination)
            if (is.null(labels)) {
                if (!is.null(rownames(item$irt$discrimination))) {
                  labels = rownames(item$irt$discrimination)
                }
                else {
                  labels <- 1:nvar
                }
            }
            if (missing(type)) {
                type = "IIC"
            }
            if (missing(D)) {
                D <- 1.702
                if (missing(xlab)) 
                  xlab <- "Latent Trait (normal scale)"
                x <- seq(-3, 3, 0.1)
                summaryx <- seq(-3, 3, 1)
            }
            if (D == 1) {
                if (missing(xlab)) 
                  xlab <- "Latent Trait (logistic scale)"
            }
            if (missing(xlab)) 
                xlab <- "Latent Trait"
            if (is.null(x)) {
                x <- seq(-4, 4, 0.1)
                summaryx <- seq(-3, 3, 1)
            }
            lenx <- length(x)
            sumInfo <- matrix(NA, ncol = nvar, nrow = length(summaryx))
            summaryx <- as.matrix(summaryx, ncol = 1)
            if (type == "ICC") {
                summtInfo <- NULL
                if (missing(main)) 
                  main <- "Item parameters from factor analysis"
                if (missing(ylab)) 
                  ylab <- "Probability of Response"
                if (length(lncol) < 2) 
                  lncol <- rep(lncol, nvar)
                ii <- 1
                while ((abs(discrimination[ii]) < cut) && (ii < 
                  nvar)) {
                  ii <- ii + 1
                }
                plot(x, logistic(x, a = discrimination[ii] * 
                  D, d = location[ii]), ylim = c(0, 1), ylab = ylab, 
                  xlab = xlab, type = "l", main = main, col = lncol[1], 
                  ...)
                text(location[ii], 0.53, labels[ii])
                for (i in (ii + 1):nvar) {
                  if (abs(discrimination[i]) > cut) {
                    lines(x, logistic(x, a = discrimination[i] * 
                      D, d = location[i]), lty = c(1:6)[(i%%6) + 
                      1], col = lncol[i], ...)
                    text(location[i], 0.53, labels[i])
                  }
                }
            }
            else {
                tInfo <- matrix(0, ncol = nvar, nrow = length(x))
                for (i in 1:nvar) {
                  if (abs(discrimination[i]) > cut) {
                    tInfo[, i] <- logisticInfo(x, a = discrimination[i] * 
                      D, d = location[i])
                    sumInfo[, i] <- logisticInfo(summaryx, a = discrimination[i] * 
                      D, d = location[i])
                  }
                  else {
                    tInfo[, i] <- 0
                    sumInfo[, i] <- 0
                  }
                }
                AUC <- colSums(tInfo)
                max.info <- apply(tInfo, 2, which.max)
                if (type == "test") {
                  if (missing(main)) 
                    main <- "Test information -- item parameters from factor analysis"
                  if (missing(y2lab)) 
                    y2lab <- "Reliability"
                  testInfo <- rowSums(tInfo)
                  if (missing(ylab)) 
                    ylab <- "Test Information"
                  if (missing(xlab)) 
                    xlab <- "Latent Trait (normal scale)"
                  if (length(lncol) < 2) 
                    lncol <- rep(lncol, nvar)
                  op <- par(mar = c(5, 4, 4, 4))
                  plot(x, testInfo, typ = "l", ylim = c(0, max(testInfo)), 
                    ylab = ylab, xlab = xlab, main = main, col = lncol[1], 
                    ...)
                  ax4 <- seq(0, max(testInfo), max(testInfo)/4)
                  rel4 <- round(1 - 1/ax4, 2)
                  rel4[1] <- NA
                  axis(4, at = ax4, rel4)
                  mtext(y2lab, side = 4, line = 2)
                  op <- par(op)
                }
                else {
                  if (missing(ylab)) 
                    ylab <- "Item Information"
                  if (missing(main)) 
                    main <- "Item information from factor analysis"
                  if (length(lncol) < 2) 
                    lncol <- rep(lncol, nvar)
                  ii <- 1
                  while ((abs(discrimination[ii]) < cut) && (ii < 
                    nvar)) {
                    ii <- ii + 1
                  }
                  if (missing(ylim)) {
                    ylimit = c(0, max(tInfo) + 0.03)
                  }
                  else {
                    ylimit <- ylim
                  }
                  plot(x, logisticInfo(x, a = discrimination[ii] * 
                    D, d = location[ii]), ylim = ylimit, ylab = ylab, 
                    xlab = xlab, type = "l", main = main, col = lncol[1], 
                    ...)
                  text(location[ii], max(tInfo[, ii]) + 0.03, 
                    labels[ii])
                  for (i in (ii + 1):nvar) {
                    if (abs(discrimination[i]) > cut) {
                      lines(x, logisticInfo(x, a = discrimination[i] * 
                        D, d = location[i]), lty = c(1:6)[(i%%6) + 
                        1], col = lncol[i])
                      text(location[i], max(tInfo[, i]) + 0.02, 
                        labels[i])
                    }
                  }
                }
                if (type != "ICC") {
                  temp[[f]] <- list(AUC = AUC, max.info = max.info)
                  sumInfo <- t(sumInfo)
                  colnames(sumInfo) <- summaryx
                  rownames(sumInfo) <- rownames(item$rho)
                  sumtemp[[f]] <- sumInfo
                }
            }
            devAskNewPage(ask = TRUE)
        }
        devAskNewPage(ask = FALSE)
        if (type != "ICC") {
            AUC <- matrix(NA, ncol = nf, nrow = nvar)
            max.info <- matrix(NA, ncol = nf, nrow = nvar)
            for (f in 1:nf) {
                AUC[, f] <- temp[[f]]$AUC
                max.info[, f] <- temp[[f]]$max.info
            }
            AUC <- AUC/lenx
            max.info <- (max.info - lenx/2) * 6/(lenx - 1)
            max.info[max.info < -2.9] <- NA
            if (byKeys) {
                colnames(AUC) <- colnames(max.info) <- colnames(keys)
            }
            else {
                colnames(AUC) <- colnames(max.info) <- colnames(item$irt$discrimination)
            }
            rownames(AUC) <- rownames(max.info) <- rownames(item$rho)
            result <- list(AUC = AUC, max.info = max.info, sumInfo = sumtemp)
            invisible(result)
            class(result) <- c("psych", "polyinfo")
            invisible(result)
        }
    }
}


rangeCorrection <- function (r, sdu, sdr, sdxu = NULL, sdxr = NULL, case = 2) 
{
    if (!is.null(sdxu)) 
        case <- 4
    switch(case, {
        result <- sqrt(1 - (sdr^2/sdu^2) * (1 - r^2))
    }, {
        result <- (r * sdu/(sdr * sqrt(1 - r^2 + r^2 * (sdu^2/sdr^2))))
    }, {
        result <- NULL
    }, {
        result <- r * (sdr/sdu) * (sdxr/sdxu) + sqrt((1 - (sdr/sdu)^2) * 
            (1 - (sdxr/sdxu)^2))
    })
    return(result)
}


test.all <- function (pl, package = "psych", dependencies = c("Depends", 
    "Imports", "LinkingTo"), find = FALSE, skip = NULL) 
{
    if (find) {
        pl <- tools::dependsOnPkgs(package, dependencies = dependencies)
        if (!is.null(skip) && skip %in% pl) {
            pl <- pl[-which(skip == pl)]
        }
    }
    np <- length(pl)
    if (np > 0) {
        for (i in 1:np) {
            p <- pl[i]
            test <- require(p, character.only = TRUE)
            if (!test) {
                cat("\nCould not find package ", p, "\n")
                next
            }
            cat(paste("\nNow testing package ", p))
            ob <- paste("package", p, sep = ":")
            ol <- objects(ob)
            nf <- length(ol)
            options(example.ask = FALSE)
            for (i in 1:nf) {
                fn <- as.character(ol[[i]])
                example(topic = fn, package = p, character.only = TRUE)
            }
            detach(ob, character.only = TRUE)
        }
    }
    else {
        cat("\nNo dependencies for package ", package)
    }
}


omegaSem <- function (m, nfactors = 3, fm = "minres", key = NULL, flip = TRUE, 
    digits = 2, title = "Omega", sl = TRUE, labels = NULL, plot = TRUE, 
    n.obs = NA, rotate = "oblimin", Phi = NULL, option = "equal", 
    lavaan = TRUE, ...) 
{
    if (lavaan) {
        if (!requireNamespace("lavaan")) 
            stop("You must have the lavaan package installed to use omegaSem")
    }
    else {
        if (!requireNamespace("sem")) 
            stop("You must have the sem package installed to use omegaSem")
    }
    if (!sl) {
        warning("OmegaSem only works for Bifactor models, sl set to TRUE ")
        sl <- TRUE
    }
    cl <- match.call()
    om <- omega(m = m, nfactors = nfactors, fm = fm, key = key, 
        flip = flip, digits = digits, title = title, sl = sl, 
        labels = labels, plot = plot, n.obs = n.obs, rotate = rotate, 
        Phi = Phi, option = option, ...)
    if (lavaan) {
        sem.model <- om$model$lavaan
    }
    else {
        sem.model <- om$model$sem
    }
    if (is.na(n.obs)) {
        n.obs <- om$gstats$n.obs
    }
    if (dim(m)[1] != dim(m)[2]) {
        n.obs <- dim(m)[1]
        m <- cor(m, use = "pairwise")
    }
    else {
        m <- cov2cor(as.matrix(m))
    }
    nvar <- dim(m)[2]
    if (is.na(n.obs)) {
        message("Number of observations not specified. Arbitrarily set to 500")
        n.obs <- 500
    }
    if (is.null(colnames(m))) {
        rownames(m) <- colnames(m) <- paste("V", 1:nvar, sep = "")
    }
    m.names <- colnames(m)
    if (lavaan) {
        if (!requireNamespace("lavaan")) 
            stop("You must have the lavaan package installed to use omegaSem")
    }
    else {
        if (!requireNamespace("sem")) 
            stop("You must have the sem package installed to use omegaSem")
    }
    if (lavaan) {
        sem.om <- lavaan::cfa(sem.model, sample.cov = m, sample.nobs = n.obs, 
            orthogonal = TRUE, std.lv = TRUE)
    }
    else {
        sem.om <- sem::sem(sem.model, m, n.obs)
    }
    omega.efa <- omegaFromSem(sem.om, m, flip = flip)
    results <- list(omegaSem = om, omega.efa = omega.efa, sem = sem.om, 
        Call = cl)
    class(results) <- c("psych", "omegaSem")
    return(results)
}


anova.psych <- function (object, object2, ...) 
{
    name.1 <- deparse(substitute(object))
    name.2 <- deparse(substitute(object2))
    object1 <- object
    if (class(object1)[2] == "omega") 
        object1 <- object1$schmid
    if (class(object2)[2] == "omega") 
        object2 <- object2$schmid
    chi.1 <- object1$STATISTIC
    dof.1 <- object1$dof
    BIC.1 <- object1$BIC
    echi.1 <- object1$chi
    chi.2 <- object2$STATISTIC
    dof.2 <- object2$dof
    BIC.2 <- object2$BIC
    echi.2 <- object2$chi
    if (is.null(echi.1)) 
        echi.1 <- NA
    if (is.null(echi.2)) 
        echi.2 <- NA
    if (is.null(chi.1)) {
        stop("You do not seem to have chi square values for  ", 
            name.1, "\nPerhaps you did not specify the sample size when you did the analysis?")
    }
    if (is.null(chi.2)) {
        stop("You do not seem to have chi square values for  ", 
            name.2, "\nPerhaps you did not specify the sample size when you did the analysis?")
    }
    delta.df <- abs(dof.1 - dof.2)
    delta.chi <- abs(chi.1 - chi.2)
    delta.echi <- abs(echi.1 - echi.2)
    test.chi <- delta.chi/delta.df
    test.echi <- delta.echi/delta.df
    delta.BIC <- (BIC.2 - BIC.1)
    table <- data.frame(c(dof.1, dof.2), c(chi.1, chi.2), c(NA, 
        delta.df), c(NA, delta.chi), c(NA, pchisq(delta.chi, 
        delta.df, lower.tail = FALSE)), c(echi.1, echi.2), c(NA, 
        delta.echi), c(NA, pchisq(delta.echi, delta.df, lower.tail = FALSE)), 
        c(BIC.1, BIC.2), c(NA, delta.BIC))
    names(table) <- c("Model Df", "ML Chisq", "Delta Df", "Delta Chisq", 
        "Pr(> Delta Chisq)", "Emp Chisq", " Delta Emp Chisq", 
        "Pr(> Emp.Delta Chisq)", "BIC", "Delta BIC")
    if (is.na(delta.echi)) {
        table <- data.frame(c(dof.1, dof.2), c(chi.1, chi.2), 
            c(NA, delta.df), c(NA, delta.chi), c(NA, pchisq(delta.chi, 
                delta.df, lower.tail = FALSE)), c(BIC.1, BIC.2), 
            c(NA, delta.BIC))
        names(table) <- c("Model Df", "ML Chisq", "Delta Df", 
            "Delta Chisq", "Pr(> Delta Chisq)", "BIC", "Delta BIC")
    }
    rownames(table) <- c(name.1, name.2)
    structure(table, heading = c("ANOVA Test for Difference Between Models", 
        ""), class = c("anova", "data.frame"))
}


cta <- function (n = 3, t = 5000, cues = NULL, act = NULL, inhibit = NULL, 
    expect = NULL, consume = NULL, tendency = NULL, tstrength = NULL, 
    type = "both", fast = 2, compare = FALSE, learn = TRUE, reward = NULL) 
{
    tf <- function(tendency, cues, step, expect, act, consume) {
        tf <- tendency + cues %*% step %*% expect - act %*% step %*% 
            consume
    }
    af <- function(act, tendency, step, tstrength, inhibit) {
        af <- tendency %*% step %*% tstrength + act - act %*% 
            step %*% inhibit
    }
    ef <- function(expect, act, step, consume, reward) {
        if (learn) {
            which.act <- which.max(act)
            if (old.act != which.act) {
                diag(temp) <- act %*% reward
                expect <- expect + temp
                expect <- expect * 1/tr(expect)
                old.act <- which.act
            }
        }
        ef <- expect
    }
    temp <- matrix(0, n, n)
    if (n > 4) {
        colours <- rainbow(n)
    }
    else {
        colours <- c("blue", "red", "black", "green")
    }
    stepsize <- 0.05
    tendency.start <- tendency
    act.start <- act
    expect.start <- expect
    if (is.null(cues)) {
        cues <- 2^(n - 1:n)
    }
    if (is.null(inhibit)) {
        inhibit <- matrix(1, ncol = n, nrow = n)
        diag(inhibit) <- 0.05
    }
    if (is.null(tstrength)) 
        tstrength <- diag(1, n)
    if (n > 1) {
        colnames(inhibit) <- rownames(inhibit) <- paste("A", 
            1:n, sep = "")
    }
    if (is.null(consume)) {
        consume <- diag(0.03, ncol = n, nrow = n)
    }
    step <- diag(stepsize, n)
    if (is.null(expect)) 
        expect <- diag(1, n)
    if (is.null(tendency.start)) {
        tendency <- rep(0, n)
    }
    else {
        tendency <- tendency.start
    }
    if (is.null(act.start)) {
        act <- cues
    }
    else {
        act <- act.start
    }
    if (is.null(reward)) {
        reward <- matrix(0, n, n)
        diag(reward) <- c(rep(0, n - 1), 0.05)
    }
    else {
        temp1 <- reward
        reward <- matrix(0, n, n)
        diag(reward) <- temp1
    }
    maxact <- minact <- mintendency <- maxtendency <- 0
    counts <- rep(0, n)
    transitions <- matrix(0, ncol = n, nrow = n)
    frequency <- matrix(0, ncol = n, nrow = n)
    colnames(frequency) <- paste("T", 1:n, sep = "")
    rownames(frequency) <- paste("F", 1:n, sep = "")
    old.act <- which.max(act)
    for (i in 1:t) {
        tendency <- tf(tendency, cues, step, expect, act, consume)
        act <- af(act, tendency, step, tstrength, inhibit)
        act[act < 0] <- 0
        expect <- ef(expect, act, step, consume, reward)
        maxact <- max(maxact, act)
        minact <- min(minact, act)
        maxtendency <- max(maxtendency, tendency)
        mintendency <- min(mintendency, tendency)
        which.act <- which.max(act)
        counts[which.act] <- counts[which.act] + 1
        transitions[old.act, which.act] <- transitions[old.act, 
            which.act] + 1
        if (old.act != which.act) {
            frequency[old.act, which.act] <- frequency[old.act, 
                which.act] + 1
            frequency[which.act, which.act] <- frequency[which.act, 
                which.act] + 1
        }
        old.act <- which.act
    }
    plots <- 1
    action <- FALSE
    if (type != "none") {
        if (type == "state") {
            op <- par(mfrow = c(1, 1))
            if (is.null(tendency.start)) {
                tendency <- rep(0, n)
            }
            else {
                tendency <- tendency.start
            }
            if (is.null(act.start)) {
                act <- cues
            }
            else {
                act <- act.start
            }
            plot(tendency[1], tendency[2], xlim = c(mintendency, 
                maxtendency), ylim = c(mintendency, maxtendency), 
                col = "black", main = "State diagram", xlab = "tendency 1", 
                ylab = "tendency 2")
            for (i in 1:t) {
                tendency <- tf(tendency, cues, step, expect, 
                  act, consume)
                act <- af(act, tendency, step, tstrength, inhibit)
                act[act < 0] <- 0
                if (!(i%%fast)) 
                  points(tendency[1], tendency[2], col = "black", 
                    pch = 20, cex = 0.2)
            }
        }
        else {
            if (type == "both") {
                if (compare) {
                  op <- par(mfrow = c(2, 2))
                }
                else {
                  op <- par(mfrow = c(2, 1))
                }
                plots <- 2
            }
            else {
                op <- par(mfrow = c(1, 1))
            }
            if (type == "action") {
                action <- TRUE
            }
            else {
                if (type == "tendencyd") 
                  action <- FALSE
            }
            for (k in 1:plots) {
                if (is.null(tendency.start)) {
                  tendency <- rep(0, n)
                }
                else {
                  tendency <- tendency.start
                }
                if (is.null(act.start)) {
                  act <- cues
                }
                else {
                  act <- act.start
                }
                if (is.null(expect.start)) {
                  expect <- diag(1, n)
                }
                else {
                  expect <- expect.start
                }
                if (action) 
                  plot(rep(1, n), act, xlim = c(0, t), ylim = c(minact, 
                    maxact), xlab = "time", ylab = "action", 
                    main = "Actions over time")
                else plot(rep(1, n), tendency, xlim = c(0, t), 
                  ylim = c(mintendency, maxtendency), xlab = "time", 
                  ylab = "action tendency", main = "Action tendencies over time")
                for (i in 1:t) {
                  tendency <- tf(tendency, cues, step, expect, 
                    act, consume)
                  act <- af(act, tendency, step, tstrength, inhibit)
                  act[act < 0] <- 0
                  maxact <- max(maxact, act)
                  minact <- min(minact, act)
                  maxtendency <- max(maxtendency, tendency)
                  mintendency <- min(mintendency, tendency)
                  which.act <- which.max(act)
                  counts[which.act] <- counts[which.act] + 1
                  transitions[old.act, which.act] <- transitions[old.act, 
                    which.act] + 1
                  if (old.act != which.act) {
                    frequency[old.act, which.act] <- frequency[old.act, 
                      which.act] + 1
                    expect <- ef(expect, act, step, consume, 
                      reward)
                  }
                  old.act <- which.act
                  if (!(i%%fast)) {
                    if (action) 
                      points(rep(i, n), act, col = colours, cex = 0.2)
                    else points(rep(i, n), tendency, col = colours, 
                      cex = 0.2)
                  }
                }
                action <- TRUE
            }
        }
    }
    results <- list(cues = cues, expectancy = expect, strength = tstrength, 
        inihibition = inhibit, consumation = consume, reinforcement = reward, 
        time = counts, frequency = frequency, tendency = tendency, 
        act = act)
    return(results)
}


`%+%` <- function (x, y) 
{
    if (!is.matrix(x)) {
        if (is.vector(x)) {
            x <- as.matrix(x)
        }
        else stop("x must be either a vector or a matrix")
    }
    if (!is.matrix(y)) {
        if (is.vector(y)) {
            y <- as.matrix(y)
        }
        else stop("y must be either a vector or a matrix")
    }
    n.x <- dim(x)[1]
    n.y <- dim(y)[2]
    n.k <- dim(x)[2]
    if (n.k != dim(y)[1]) {
        warning("Matrices should be comparable")
    }
    x <- rowSums(x, na.rm = FALSE)
    y <- colSums(y, na.rm = FALSE)
    one <- as.vector(rep(1, n.y))
    one.y <- as.vector(rep(1, n.x))
    xy <- x %*% t(one) + t(y %*% t(one.y))
    return(xy)
}


mixed.cor <- function (x = NULL, p = NULL, d = NULL, smooth = TRUE, correct = 0.5, 
    global = TRUE, ncat = 8, use = "pairwise", method = "pearson", 
    weight = NULL) 
{
    cl <- match.call()
    organize <- FALSE
    if (!is.null(x) && is.null(p) && is.null(d)) {
        organize <- TRUE
        nvar <- ncol(x)
        x <- as.matrix(x)
        tab <- apply(x, 2, function(x) table(x))
        if (is.list(tab)) {
            len <- lapply(tab, function(x) length(x))
        }
        else {
            len <- dim(tab)[1]
        }
        dvars <- subset(1:nvar, len == 2)
        pvars <- subset(1:nvar, ((len > 2) & (len <= ncat)))
        cvars <- subset(1:nvar, (len > ncat))
        if (length(dvars) > 0) {
            d <- matrix(x[, dvars], ncol = length(dvars))
            colnames(d) <- colnames(x)[dvars]
        }
        else {
            d <- NULL
        }
        if (length(pvars) > 0) {
            p <- matrix(x[, pvars], ncol = length(pvars))
            colnames(p) <- colnames(x)[pvars]
            tab <- table(p)
            if (length(tab) > ncat) 
                stop("I tried to figure out which where continuous and which were polytomous, but failed.  Please try again by specifying x, p, and d.")
            ok <- apply(p, 2, function(x) {
                if (length(table(x)) != (max(x, na.rm = TRUE) - 
                  min(x, na.rm = TRUE) + 1)) {
                  FALSE
                }
                else {
                  TRUE
                }
            })
            if (any(!ok)) {
                bad <- which(!ok)
                cat("\n Some polytomous variables have fewer categories than they should.  Please check your data.  \nPotential bad items are ", 
                  colnames(p)[bad], "\n")
                stop("\nI am stopping because of the problem with polytomous data")
            }
        }
        else {
            p <- NULL
        }
        if (length(cvars) > 0) {
            cont <- matrix(x[, cvars], ncol = length(cvars))
            colnames(cont) <- colnames(x)[cvars]
        }
        else {
            cont <- NULL
        }
        Rho <- mixed.cor1(cont, p, d, smooth = smooth, global = global, 
            correct = correct, use = use, method = method, weight = weight)
        oldorder <- c(cvars, pvars, dvars)
        ord <- order(oldorder)
        Rho$rho <- Rho$rho[ord, ord]
    }
    else {
        Rho <- mixed.cor1(x = x, p = p, d = d, smooth = smooth, 
            global = global, correct = correct, use = use, method = method, 
            weight = weight)
    }
    Rho$Call <- cl
    return(Rho)
}


error.bars.tab <- function (t, way = "columns", raw = FALSE, col = c("blue", "red"), 
    ...) 
{
    rnames <- rownames(t)
    cnames <- colnames(t)
    t <- as.matrix(t)
    switch(way, columns = {
        p <- t(t(t)/colSums(t))
        if (!raw) {
            standard.error <- t(sqrt(t(p * (1 - p))/colSums(t)))
        } else {
            standard.error <- t(sqrt(t(p * (1 - p)) * colSums(t)))
        }
    }, rows = {
        t <- as.matrix(t)
        p <- t/rowSums(t)
        if (!raw) {
            standard.error <- sqrt(p * (1 - p)/rowSums(t))
        } else {
            standard.error <- sqrt(p * (1 - p) * rowSums(t))
        }
    }, both = {
        p <- t(t(t)/sum(t))
        if (!raw) {
            standard.error <- t(sqrt(t(p * (1 - p))/sum(t)))
        } else {
            standard.error <- t(sqrt(t(p * (1 - p)) * sum(t)))
        }
    })
    colnames(p) <- colnames(t)
    rownames(p) <- rownames(t)
    nv <- ncol(t)
    ng <- nrow(t)
    if (raw) {
        p <- t
    }
    stats <- data.frame(mean = as.vector(p), se = as.vector(standard.error))
    rownames(stats) <- paste(rnames, rep(cnames, each = ng))
    space <- rep(0.1, nv * ng)
    for (i in 1:(nv - 1)) {
        space[ng * i + 1] <- 1
    }
    error.bars(stats = stats, bars = TRUE, space = space, density = c(20, 
        -10, 20, -10), col = col, ...)
    invisible(list(p = p, stats = stats))
}


structure.sem <- function (fx, Phi = NULL, fy = NULL, out.file = NULL, labels = NULL, 
    cut = 0.3, errors = TRUE, simple = TRUE, regression = FALSE) 
{
    xmodel <- fx
    ymodel <- fy
    if (!is.null(class(xmodel)) && (length(class(xmodel)) > 1)) {
        if (class(xmodel)[1] == "psych" && class(xmodel)[2] == 
            "omega") {
            Phi <- xmodel$schmid$phi
            xmodel <- xmodel$schmid$oblique
        }
        else {
            if (class(xmodel)[1] == "psych" && ((class(xmodel)[2] == 
                "fa") | (class(xmodel)[2] == "principal"))) {
                if (!is.null(xmodel$Phi)) 
                  Phi <- xmodel$Phi
                xmodel <- as.matrix(xmodel$loadings)
            }
        }
    }
    else {
        if (!is.matrix(xmodel) & !is.data.frame(xmodel) & !is.vector(xmodel)) {
            if (!is.null(xmodel$Phi)) 
                Phi <- xmodel$Phi
            xmodel <- as.matrix(xmodel$loadings)
        }
        else {
            xmodel <- xmodel
        }
    }
    digits <- 2
    if (!is.matrix(xmodel)) {
        factors <- as.matrix(xmodel)
    }
    else {
        factors <- xmodel
    }
    num.y <- 0
    num.var <- num.xvar <- dim(factors)[1]
    if (is.null(num.var)) {
        num.var <- length(factors)
        num.factors <- 1
    }
    else {
        num.factors <- dim(factors)[2]
    }
    num.xfactors <- num.factors
    if (is.null(labels)) {
        vars <- xvars <- rownames(xmodel)
    }
    else {
        xvars <- vars <- labels
    }
    if (is.null(vars)) {
        vars <- xvars <- paste("x", 1:num.var, sep = "")
    }
    fact <- colnames(xmodel)
    if (is.null(fact)) {
        fact <- paste("X", 1:num.factors, sep = "")
    }
    num.yfactors <- 0
    if (!is.null(ymodel)) {
        if (is.list(ymodel) & !is.data.frame(ymodel)) {
            ymodel <- as.matrix(ymodel$loadings)
        }
        else {
            ymodel <- ymodel
        }
        if (!is.matrix(ymodel)) {
            y.factors <- as.matrix(ymodel)
        }
        else {
            y.factors <- ymodel
        }
        num.y <- dim(y.factors)[1]
        if (is.null(num.y)) {
            num.y <- length(ymodel)
            num.yfactors <- 1
        }
        else {
            num.yfactors <- dim(y.factors)[2]
        }
        yvars <- rownames(ymodel)
        if (is.null(yvars)) {
            yvars <- paste("y", 1:num.y, sep = "")
        }
        if (is.null(labels)) {
            vars <- c(xvars, yvars)
        }
        else {
            yvars <- labels[(num.xvar + 1):(num.xvar + num.y)]
        }
        vars <- c(vars, yvars)
        yfact <- colnames(ymodel)
        if (is.null(yfact)) {
            yfact <- paste("Y", 1:num.yfactors, sep = "")
        }
        fact <- c(fact, yfact)
        num.var <- num.xvar + num.y
        num.factors <- num.xfactors + num.yfactors
    }
    sem <- matrix(rep(NA), 6 * (num.var * num.factors + num.factors), 
        ncol = 3)
    colnames(sem) <- c("Path", "Parameter", "Value")
    if (!regression) {
        k <- num.factors
        if (num.xfactors == 1) {
            for (i in 1:num.xvar) {
                sem[i, 1] <- paste(fact[1], "->", vars[i], sep = "")
                if (is.numeric(factors[i])) {
                  sem[i, 2] <- vars[i]
                }
                else {
                  sem[i, 2] <- factors[i]
                }
            }
            k <- num.xvar + 1
        }
        else {
            k <- 1
            for (i in 1:num.xvar) {
                for (f in 1:num.xfactors) {
                  if ((!is.numeric(factors[i, f]) && (factors[i, 
                    f] != "0")) || ((is.numeric(factors[i, f]) && 
                    abs(factors[i, f]) > cut))) {
                    sem[k, 1] <- paste(fact[f], "->", vars[i], 
                      sep = "")
                    if (is.numeric(factors[i, f])) {
                      sem[k, 2] <- paste("F", f, vars[i], sep = "")
                    }
                    else {
                      sem[k, 2] <- factors[i, f]
                    }
                    k <- k + 1
                  }
                }
            }
        }
        if (errors) {
            for (i in 1:num.xvar) {
                sem[k, 1] <- paste(vars[i], "<->", vars[i], sep = "")
                sem[k, 2] <- paste("x", i, "e", sep = "")
                k <- k + 1
            }
        }
    }
    else {
        if (title == "Structural model") 
            title <- "Regression model"
        k <- num.var + 1
        yvars <- "Y1"
    }
    if (!is.null(ymodel)) {
        if (num.yfactors == 1) {
            for (i in 1:num.y) {
                sem[k, 1] <- paste(fact[1 + num.xfactors], "->", 
                  yvars[i], sep = "")
                if (is.numeric(y.factors[i])) {
                  sem[k, 2] <- paste("Fy", yvars[i], sep = "")
                }
                else {
                  sem[k, 2] <- y.factors[i]
                }
                k <- k + 1
            }
        }
        else {
            for (i in 1:num.y) {
                for (f in 1:num.yfactors) {
                  if ((!is.numeric(y.factors[i, f]) && (y.factors[i, 
                    f] != "0")) || ((is.numeric(y.factors[i, 
                    f]) && abs(y.factors[i, f]) > cut))) {
                    sem[k, 1] <- paste(fact[f + num.xfactors], 
                      "->", vars[i + num.xvar], sep = "")
                    if (is.numeric(y.factors[i, f])) {
                      sem[k, 2] <- paste("Fy", f, vars[i + num.xvar], 
                        sep = "")
                    }
                    else {
                      sem[k, 2] <- y.factors[i, f]
                    }
                    k <- k + 1
                  }
                }
            }
        }
        if (errors) {
            for (i in 1:num.y) {
                sem[k, 1] <- paste(vars[i + num.xvar], "<->", 
                  vars[i + num.xvar], sep = "")
                sem[k, 2] <- paste("y", i, "e", sep = "")
                k <- k + 1
            }
        }
    }
    if (!is.null(labels)) {
        var.labels <- c(labels, fact)
    }
    if (!regression) {
        if (!is.null(Phi)) {
            if (!is.matrix(Phi)) 
                Phi <- matrix(c(1, Phi, 0, 1), ncol = 2)
            if (num.xfactors > 1) {
                for (i in 2:num.xfactors) {
                  for (j in 1:(i - 1)) {
                    if ((!is.numeric(Phi[i, j]) && (Phi[i, j] != 
                      "0")) || ((is.numeric(Phi[i, j]) && abs(Phi[i, 
                      j]) > cut))) {
                      if (Phi[i, j] != Phi[j, i]) {
                        sem[k, 1] <- paste(fact[j], "->", fact[i], 
                          sep = "")
                        if (is.numeric(Phi[i, j])) {
                          sem[k, 2] <- paste("rF", j, "F", i, 
                            sep = "")
                        }
                        else {
                          sem[k, 2] <- Phi[i, j]
                        }
                      }
                      else {
                        sem[k, 1] <- paste(fact[i], "<->", fact[j], 
                          sep = "")
                        if (is.numeric(Phi[i, j])) {
                          sem[k, 2] <- paste("rF", i, "F", j, 
                            sep = "")
                        }
                        else {
                          sem[k, 2] <- Phi[i, j]
                        }
                      }
                      k <- k + 1
                    }
                  }
                }
            }
        }
        if (!is.null(ymodel)) {
            for (i in 1:num.xfactors) {
                for (j in 1:num.yfactors) {
                  if ((!is.numeric(Phi[i, j + num.xfactors]) && 
                    (Phi[i, j + num.xfactors] != "0")) || ((is.numeric(Phi[i, 
                    j + num.xfactors]) && abs(Phi[i, j + num.xfactors]) > 
                    cut))) {
                    sem[k, 1] <- paste(fact[i], "->", fact[j + 
                      num.xfactors], sep = "")
                    if (is.numeric(Phi[i, j + num.xfactors])) {
                      sem[k, 2] <- paste("rX", i, "Y", j, sep = "")
                    }
                    else {
                      sem[k, 2] <- Phi[i, j + num.xfactors]
                    }
                    k <- k + 1
                  }
                }
            }
        }
    }
    else {
        if (!is.null(Phi)) {
            if (!is.matrix(Phi)) 
                Phi <- matrix(c(1, Phi, 0, 1), ncol = 2)
            for (i in 2:num.xvar) {
                for (j in 1:(i - 1)) {
                  if (Phi[i, j] != Phi[j, i]) {
                    edge.dir[k] <- "back"
                  }
                  else {
                    edge.dir[k] <- "both"
                  }
                  k <- k + 1
                }
            }
        }
    }
    for (f in 1:num.factors) {
        sem[k, 1] <- paste(fact[f], "<->", fact[f], sep = "")
        sem[k, 3] <- "1"
        k <- k + 1
    }
    model = sem[1:(k - 1), ]
    class(model) <- "mod"
    return(model)
}


eigen.loadings <- function (x) 
{
    if (!is.null(x$vector)) {
        ans <- x$vectors %*% sqrt(diag(x$values))
        colnames(ans) <- rownames(ans) <- rownames(x$vector)
        return(ans)
    }
    else if (!is.null(x$loadings)) {
        ans <- x$loadings %*% diag(x$sdev)
        rownames(ans) <- rownames(x$loadings)
        colnames(ans) <- colnames(x$loadings)
        return(ans)
    }
}


factor.model <- function (f, Phi = NULL, U2 = TRUE) 
{
    if (!is.matrix(f)) 
        f <- as.matrix(f)
    if (is.null(Phi)) {
        Phi <- diag(1, dim(f)[2])
    }
    if (!is.matrix(Phi)) {
        Phi <- as.matrix(Phi)
    }
    if (!U2) 
        diag(Phi) <- 1
    result <- f %*% Phi %*% t(f)
    if (!U2) 
        diag(result) <- 1
    return(result)
}


Yule <- function (x, Y = FALSE) 
{
    stopifnot(prod(dim(x)) == 4 || length(x) == 4)
    if (is.vector(x)) {
        x <- matrix(x, 2)
    }
    a <- x[1, 1]
    b <- x[1, 2]
    c <- x[2, 1]
    d <- x[2, 2]
    if (Y) {
        Yule <- (sqrt(a * d) - sqrt(b * c))/(sqrt(a * d) + sqrt(b * 
            c))
    }
    else {
        Yule <- (a * d - b * c)/(a * d + b * c)
    }
    return(Yule)
}


interp.quartiles <- function (x, w = 1, na.rm = TRUE) 
{
    q <- c(0.25, 0.5, 0.75)
    if (is.vector(x)) {
        im <- interp.quart(x, w, na.rm = na.rm)
        names(im) <- c("Q1", "Median", "Q3")
    }
    else {
        nvar <- dim(x)[2]
        im <- matrix(NA, ncol = 3, nrow = nvar)
        for (i in 1:nvar) {
            im[i, ] <- interp.quart(x[, i], w, na.rm = na.rm)
        }
        rownames(im) <- colnames(x)
        colnames(im) <- c("Q1", "Median", "Q3")
    }
    return(im)
}


cortest.normal <- function (R1, R2 = NULL, n1 = NULL, n2 = NULL, fisher = TRUE) 
{
    cl <- match.call()
    if (dim(R1)[1] != dim(R1)[2]) {
        n1 <- dim(R1)[1]
        message("R1 was not square, finding R from data")
        R1 <- cor(R1, use = "pairwise")
    }
    if (!is.matrix(R1)) 
        R1 <- as.matrix(R1)
    p <- dim(R1)[2]
    if (is.null(n1)) {
        n1 <- 100
        warning("n not specified, 100 used")
    }
    if (is.null(R2)) {
        if (fisher) {
            R <- 0.5 * log((1 + R1)/(1 - R1))
            R <- R * R
        }
        else {
            R <- R1 * R1
        }
        diag(R) <- 0
        E <- (sum(R * lower.tri(R)))
        chisq <- E * (n1 - 3)
        df <- p * (p - 1)/2
        p.val <- pchisq(chisq, df, lower.tail = FALSE)
    }
    else {
        if (dim(R2)[1] != dim(R2)[2]) {
            n2 <- dim(R2)[1]
            message("R2 was not square, finding R from data")
            R2 <- cor(R2, use = "pairwise")
        }
        if (!is.matrix(R2)) 
            R2 <- as.matrix(R2)
        if (fisher) {
            R1 <- 0.5 * log((1 + R1)/(1 - R1))
            R2 <- 0.5 * log((1 + R2)/(1 - R2))
            diag(R1) <- 0
            diag(R2) <- 0
        }
        R <- R1 - R2
        R <- R * R
        if (is.null(n2)) 
            n2 <- n1
        n <- (n1 * n2)/(n1 + n2)
        E <- (sum(R * lower.tri(R)))
        chisq <- E * (n - 3)
        df <- p * (p - 1)/2
        p.val <- pchisq(chisq, df, lower.tail = FALSE)
    }
    result <- list(chi2 = chisq, prob = p.val, df = df, Call = cl)
    class(result) <- c("psych", "cortest")
    return(result)
}


outlier <- function (x, plot = TRUE, bad = 5, na.rm = TRUE, xlab, ylab, 
    ...) 
{
    if (missing(xlab)) 
        xlab <- expression("Quantiles of " * ~chi^2)
    if (missing(ylab)) 
        ylab <- expression("Mahalanobis " * D^2)
    rn <- rownames(x)
    nvar <- ncol(x)
    n.obs <- nrow(x)
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    nvar <- ncol(x)
    Sx <- cov(x, use = "pairwise")
    Sx.inv <- solve(Sx)
    x <- scale(x, scale = FALSE)
    D2 <- t(apply(x, 1, function(xx) colSums(xx * Sx.inv, na.rm = TRUE)))
    D2 <- rowSums(D2 * x, na.rm = TRUE)
    names(D2) <- rn
    if (plot) {
        Chi2 <- qchisq(ppoints(n.obs), df = nvar)
        qqplot(Chi2, D2, main = expression("Q-Q plot of Mahalanobis" * 
            ~D^2 * " vs. quantiles of" * ~chi[nvar]^2), xlab = xlab, 
            ylab = ylab, ...)
        abline(0, 1, col = "gray")
        worst <- order(D2, decreasing = TRUE)
        text(Chi2[n.obs:(n.obs - bad + 1)], D2[worst[1:bad]], 
            names(D2)[worst[1:bad]], pos = 3, ...)
    }
    return(D2)
}


irt.0p <- function (items) 
{
    possible <- dim(items)[2]
    raw <- rowMeans(items, na.rm = TRUE)
    ave <- raw
    valid <- rowSums(!is.na(items))
    ave[(!is.na(ave)) & (ave < 1e-04)] <- 0.5/(possible)
    ave[(!is.na(ave)) & (ave > 0.9999)] <- (possible - 0.5)/possible
    theta <- -log((1/ave) - 1)
    irt.0p <- matrix(c(raw, theta, valid), ncol = 3)
    colnames(irt.0p) <- c("raw", "theta0", "valid")
    return(irt.0p)
}


biserial <- function (x, y) 
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    nx <- dim(x)[2]
    ny <- dim(y)[2]
    if (is.null(nx)) 
        nx <- 1
    if (is.null(ny)) 
        ny <- 1
    mat <- matrix(NaN, nrow = ny, ncol = nx)
    colnames(mat) <- colnames(x)
    rownames(mat) <- colnames(y)
    for (i in 1:ny) {
        progressBar(i * (i - 1)/2, ny^2/2, "Biserial")
        for (j in 1:nx) {
            mat[i, j] <- biserialc(x[, j], y[, i], j, i)
        }
    }
    flush(stdout())
    cat("\n")
    return(mat)
}


cluster.plot <- function (ic.results, cluster = NULL, cut = 0, labels = NULL, 
    title = "Cluster plot", pch = 18, pos, show.points = TRUE, 
    choose = NULL, ...) 
{
    if (!is.matrix(ic.results)) {
        if (!is.null(class(ic.results))) {
            if (class(ic.results)[1] == "kmeans") {
                load <- t(ic.results$centers)
            }
            else {
                load <- ic.results$loadings
            }
        }
    }
    else {
        load <- ic.results
    }
    if (!is.null(choose)) 
        load <- load[, choose, drop = FALSE]
    nc <- dim(load)[2]
    nvar <- dim(load)[1]
    "panel.points" <- function(x, y, pch = par("pch"), ...) {
        ymin <- min(y)
        ymax <- max(y)
        xmin <- min(x)
        xmax <- max(x)
        ylim <- c(min(ymin, xmin), max(ymax, xmax))
        xlim <- ylim
        if (show.points) 
            points(x, y, pch = pch, ylim = ylim, xlim = xlim, 
                ...)
        text(x, y, vnames, ...)
    }
    if (missing(pos)) 
        pos <- rep(1, nvar)
    ch.col = c("black", "blue", "red", "gray", "black", "blue", 
        "red", "gray")
    if (is.null(cluster)) {
        cluster <- rep(nc + 1, nvar)
        cluster <- apply(abs(load), 1, which.max)
        cluster[(apply(abs(load), 1, max) < cut)] <- nc + 1
    }
    if (nc > 2) {
        vnames <- labels
        pairs(load, pch = cluster + pch, col = ch.col[cluster], 
            bg = ch.col[cluster], main = title, lower.panel = panel.points, 
            upper.panel = panel.points, ...)
    }
    else {
        if (show.points) {
            plot(load, pch = cluster + pch, col = ch.col[cluster], 
                bg = ch.col[cluster], main = title, ...)
        }
        else {
            plot(load, pch = cluster + pch, col = ch.col[cluster], 
                bg = ch.col[cluster], main = title, type = "n", 
                ...)
            pos = NULL
        }
        abline(h = 0)
        abline(v = 0)
    }
    if (is.null(labels)) 
        labels <- paste(1:nvar)
    if (nc < 3) 
        text(load, labels, pos = pos, ...)
}


VSS.scree <- function (rx, main = "scree plot") 
{
    nvar <- dim(rx)[2]
    if (nvar != dim(rx)[1]) {
        rx <- cor(rx, use = "pairwise")
    }
    values <- eigen(rx)$values
    plot(values, type = "b", main = main, ylab = "Eigen values of components", 
        xlab = " component number")
    abline(h = 1)
}


circadian.reliability <- function (angle, x = NULL, code = NULL, data = NULL, min = 16, 
    oddeven = FALSE, hours = TRUE, period = 24, plot = FALSE, 
    opti = FALSE, na.rm = TRUE) 
{
    cl <- match.call()
    if (!is.null(data)) {
        if (is.character(angle)) 
            angle <- which(colnames(data) == angle)
        if (!is.null(code)) {
            if (is.character(code)) 
                codeloc <- which(colnames(data) == code)
            x <- data[, c(angle, x, codeloc)]
        }
        else {
            x <- data[, c(angle, x)]
        }
        angle <- x[1]
        x <- x[-1]
    }
    else {
        if (is.null(x) && is.null(code)) {
            x <- angle
            angle <- angle[, 1]
        }
        else {
            x <- cbind(angle, x)
            angle <- x[1]
            x <- x[-1]
        }
    }
    if (hours) {
        angle <- angle * 2 * pi/period
        x <- cbind(angle, x)
    }
    n.obs <- dim(x)[1]
    if (is.null(code)) {
        fit <- cosinor.rel(angle, x, period = period, na.rm = na.rm)
        m.resp <- mean(x[, 1])
        s.resp <- sd(x[, 1])
    }
    else {
        fit.list <- by(x, x[, code], function(x) cosinor.rel(angle = x[1], 
            x = x[-c(1, which(colnames(x) == code))], min = min, 
            oddeven = oddeven, na.rm = na.rm))
        ncases <- length(fit.list)
        fit <- matrix(unlist(fit.list), nrow = ncases, byrow = TRUE)
        colnames(fit) <- c(paste(colnames(x)[-c(1, which(colnames(x) == 
            code))], "phase1", sep = "."), paste(colnames(x)[-c(1, 
            which(colnames(x) == code))], "phase2", sep = "."), 
            paste(colnames(x)[-c(1, which(colnames(x) == code))], 
                "fit1", sep = "."), paste(colnames(x)[-c(1, which(colnames(x) == 
                code))], "fit2", sep = "."))
        rownames(fit) <- names(fit.list)
    }
    nvar <- ncol(fit)/4
    r <- circadian.cor(fit[, 1:(nvar * 2)])
    r.fit <- cor(fit[, (nvar * 2 + 1):ncol(fit)], use = "pairwise")
    splithalf <- split.fit <- rep(NA, nvar)
    for (i in 1:nvar) {
        splithalf[i] <- r[i, (nvar + i)]
        split.fit[i] <- r.fit[i, (nvar + i)]
    }
    rel <- splithalf * 2/(1 + splithalf)
    fit.rel <- split.fit * 2/(1 + split.fit)
    names(rel) <- paste(colnames(x)[-c(1, which(colnames(x) == 
        code))])
    names(fit.rel) <- paste(colnames(x)[-c(1, which(colnames(x) == 
        code))])
    split.F <- circadian.split.F(fit)
    result <- list(phase.rel = rel, fit.rel = fit.rel, split.F = split.F, 
        splits = fit, Call = cl)
    class(result) <- c("psych", "circadian", "reliability")
    return(result)
}


congeneric.sim <- function (loads = c(0.8, 0.7, 0.6, 0.5), N = NULL, err = NULL, 
    short = TRUE) 
{
    n <- length(loads)
    loading <- matrix(loads, nrow = n)
    error <- diag(1, nrow = n)
    if (!is.null(err)) {
        diag(error) <- err
    }
    else {
        diag(error) <- sqrt(1 - loading^2)
    }
    pattern <- cbind(loading, error)
    colnames(pattern) <- c("theta", paste("e", seq(1:n), sep = ""))
    rownames(pattern) <- c(paste("V", seq(1:n), sep = ""))
    model <- pattern %*% t(pattern)
    if (!is.null(N)) {
        latent <- matrix(rnorm(N * (n + 1)), ncol = (n + 1))
        observed <- latent %*% t(pattern)
        colnames(latent) <- c("theta", paste("e", seq(1:n), sep = ""))
        if (short) 
            model <- cor(observed)
    }
    if (short) {
        return(model)
    }
    else {
        result <- list(model = model, pattern = pattern, r = cor(observed), 
            latent = latent, observed = observed, N = N)
        class(result) <- c("psych", "sim")
        return(result)
    }
}


factor.congruence <- function (x, y = NULL, digits = 2, use = NULL, structure = FALSE) 
{
    fa.congruence(x = x, y = y, digits = digits, use = use, structure = structure)
}


scoreIrt.1pl <- function (keys.list, items, correct = 0.5, messages = FALSE, 
    cut = 0.3, bounds = c(-4, 4), mod = "logistic") 
{
    select <- sub("-", "", unlist(keys.list))
    select <- select[!duplicated(select)]
    items <- items[select]
    nf <- length(keys.list)
    fix <- is.numeric(keys.list[[1]])
    smallFunction <- function(i, keys.list, correct, cut = cut, 
        bounds = bounds, mod = mod) {
        list.i <- keys.list[[i]]
        keys <- rep(1, length(list.i))
        neg <- grep("-", list.i)
        keys[neg] <- -1
        select <- sub("-", "", list.i)
        selectedItems <- as.matrix(items[select])
        stats <- irt.tau(selectedItems)
        scores <- scoreIrt(stats, selectedItems, keys, cut = cut, 
            bounds = bounds, mod = mod)
        scores <- scores[, 1]
    }
    scoresList <- mcmapply(smallFunction, c(1:nf), MoreArgs = list(keys.list = keys.list, 
        correct = correct, cut = cut, bounds = bounds, mod = mod))
    colnames(scoresList) <- names(keys.list)
    return(scoresList)
}


r.con <- function (rho, n, p = 0.95, twotailed = TRUE) 
{
    z <- fisherz(rho)
    if (n < 4) {
        stop("number of subjects must be greater than 3")
    }
    se <- 1/sqrt(n - 3)
    p <- 1 - p
    if (twotailed) 
        p <- p/2
    dif <- qnorm(p)
    zlow <- z + dif * se
    zhigh <- z - dif * se
    ci <- c(zlow, zhigh)
    ci <- fisherz2r(ci)
    return(ci)
}


ICLUST.sort <- function (ic.load, cut = 0, labels = NULL, keys = FALSE, clustsort = TRUE) 
{
    if (is.matrix(ic.load)) {
        loadings <- ic.load
        pattern <- as.matrix(loadings)
    }
    else {
        loadings <- ic.load$loadings
        pattern <- as.matrix(ic.load$pattern)
    }
    nclust <- dim(loadings)[2]
    nitems <- dim(loadings)[1]
    loadings <- as.matrix(loadings)
    loadings <- unclass(loadings)
    if (nclust > 1) {
        eigenvalue <- diag(t(pattern) %*% loadings)
        evorder <- order(eigenvalue, decreasing = TRUE)
        if (clustsort) 
            loadings <- loadings[, evorder]
    }
    if (length(labels) == 0) {
        var.labels <- rownames(loadings)
    }
    else {
        var.labels = labels
    }
    if (length(var.labels) == 0) {
        var.labels = paste("V", seq(1:nitems), sep = "")
    }
    loads <- data.frame(item = seq(1:nitems), content = var.labels, 
        cluster = rep(0, nitems), loadings)
    loads$cluster <- apply(abs(loadings), 1, which.max)
    for (i in 1:nitems) {
        if (abs(loadings[i, loads$cluster[i]]) < cut) {
            loads$cluster[i] <- nclust + 1
        }
    }
    ord <- sort(loads$cluster, index.return = TRUE)
    loads[1:nitems, ] <- loads[ord$ix, ]
    rownames(loads)[1:nitems] <- rownames(loads)[ord$ix]
    items <- c(table(loads$cluster), 1)
    if (length(items) < (nclust + 1)) {
        items <- rep(0, (nclust + 1))
        for (i in 1:nclust + 1) {
            items[i] <- sum(loads$cluster == i)
        }
    }
    first <- 1
    for (i in 1:nclust) {
        if (items[i] > 0) {
            last <- first + items[i] - 1
            ord <- sort(abs(loads[first:last, i + 3]), decreasing = TRUE, 
                index.return = TRUE)
            loads[first:last, ] <- loads[ord$ix + first - 1, 
                ]
            rownames(loads)[first:last] <- rownames(loads)[ord$ix + 
                first - 1]
            first <- first + items[i]
        }
    }
    if (first < nitems) 
        loads[first:nitems, "cluster"] <- 0
    if (keys) {
        result <- list(sorted = loads, clusters = factor2cluster(loadings))
    }
    else result <- list(sorted = loads)
    class(result) <- c("psych", "iclust.sort")
    return(result)
}


spider <- function (y, x, data, labels = NULL, rescale = FALSE, center = FALSE, 
    connect = TRUE, overlay = FALSE, scale = 1, ncolors = 31, 
    fill = FALSE, main = NULL, ...) 
{
    if (is.null(labels)) 
        labels <- colnames(data)[x]
    if (rescale) {
        data <- scale(data)/3
    }
    if (length(y) == 1) {
        if (!is.null(main)) {
            main = main
        }
        else {
            main <- colnames(data)[y]
        }
        radar(data[y, x], labels = labels, center = center, connect = connect, 
            scale = scale, ncolors = ncolors, fill = fill, main = main, 
            ...)
    }
    else {
        nvar <- length(y)
        for (i in 1:nvar) {
            if (!is.null(main)) {
                title = main[y[i]]
            }
            else {
                title <- colnames(data)[y[i]]
            }
            if (overlay) {
                if (i == 1) {
                  radar(data[y[i], x], labels = labels, center = center, 
                    connect = connect, scale = scale, ncolors = ncolors, 
                    fill = fill, main = title, ...)
                }
                else {
                  radar(data[y[i], x], labels = labels, center = center, 
                    connect = connect, scale = scale, ncolors = ncolors, 
                    fill = fill, add = TRUE, linetyp = nvar%%6 + 
                      2, main = title, ...)
                }
            }
            else {
                radar(data[y[i], x], labels = labels, center = center, 
                  connect = connect, scale = scale, ncolors = ncolors, 
                  fill = fill, main = title, ...)
            }
        }
    }
}


geometric.mean <- function (x, na.rm = TRUE) 
{
    if (is.null(nrow(x))) {
        exp(mean(log(x), na.rm = TRUE))
    }
    else {
        exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
}


keys.lookup <- function (keys.list, dictionary) 
{
    if (is.list(keys.list)) {
        items <- sub("-", "", unlist(keys.list))
        f <- make.keys(items, keys.list)
    }
    keys.list <- fa.sort(f)
    contents <- lookup(rownames(f), y = dictionary)
    rownames(contents)[rowSums(f) < 0] <- paste0(rownames(contents)[rowSums(f) < 
        0], "-")
    return(contents)
}


fa.parallel <- function (x, n.obs = NULL, fm = "minres", fa = "both", main = "Parallel Analysis Scree Plots", 
    n.iter = 20, error.bars = FALSE, se.bars = TRUE, SMC = FALSE, 
    ylabel = NULL, show.legend = TRUE, sim = TRUE, quant = 0.95, 
    cor = "cor", use = "pairwise") 
{
    cl <- match.call()
    ci <- 1.96
    arrow.len <- 0.05
    nsub <- dim(x)[1]
    nvariables <- dim(x)[2]
    resample <- TRUE
    if ((isCorrelation(x)) && !sim) {
        warning("You specified a correlation matrix, but asked to just resample (sim was set to FALSE).  This is impossible, so sim is set to TRUE")
        sim <- TRUE
        resample <- FALSE
    }
    if (!is.null(n.obs)) {
        nsub <- n.obs
        rx <- x
        resample <- FALSE
        if (dim(x)[1] != dim(x)[2]) {
            warning("You specified the number of subjects, implying a correlation matrix, but do not have a correlation matrix, correlations found ")
            switch(cor, cor = {
                rx <- cor(x, use = use)
            }, cov = {
                rx <- cov(x, use = use)
                covar <- TRUE
            }, tet = {
                rx <- tetrachoric(x)$rho
            }, poly = {
                rx <- polychoric(x)$rho
            }, mixed = {
                rx <- mixed.cor(x, use = use)$rho
            }, Yuleb = {
                rx <- YuleCor(x, , bonett = TRUE)$rho
            }, YuleQ = {
                rx <- YuleCor(x, 1)$rho
            }, YuleY = {
                rx <- YuleCor(x, 0.5)$rho
            })
            if (!sim) {
                warning("You specified a correlation matrix, but asked to just resample (sim was set to FALSE).  This is impossible, so sim is set to TRUE")
                sim <- TRUE
                resample <- FALSE
            }
        }
    }
    else {
        if (isCorrelation(x)) {
            warning("It seems as if you are using a correlation matrix, but have not specified the number of cases. The number of subjects is arbitrarily set to be 100  ")
            rx <- x
            nsub = 100
            n.obs = 100
            resample <- FALSE
        }
        else {
            switch(cor, cor = {
                rx <- cor(x, use = use)
            }, cov = {
                rx <- cov(x, use = use)
                covar <- TRUE
            }, tet = {
                rx <- tetrachoric(x)$rho
            }, poly = {
                rx <- polychoric(x)$rho
            }, mixed = {
                rx <- mixed.cor(x, use = use)$rho
            }, Yuleb = {
                rx <- YuleCor(x, , bonett = TRUE)$rho
            }, YuleQ = {
                rx <- YuleCor(x, 1)$rho
            }, YuleY = {
                rx <- YuleCor(x, 0.5)$rho
            })
        }
    }
    valuesx <- eigen(rx)$values
    if (SMC) {
        diag(rx) <- smc(rx)
        fa.valuesx <- eigen(rx)$values
    }
    else {
        fa.valuesx <- fa(rx, fm = fm, warnings = FALSE)$values
    }
    temp <- list(samp = vector("list", n.iter), samp.fa = vector("list", 
        n.iter), sim = vector("list", n.iter), sim.fa = vector("list", 
        n.iter))
    templist <- mclapply(1:n.iter, function(XX) {
        if (is.null(n.obs)) {
            bad <- TRUE
            while (bad) {
                sampledata <- matrix(apply(x, 2, function(y) sample(y, 
                  nsub, replace = TRUE)), ncol = nvariables)
                switch(cor, cor = {
                  C <- cor(sampledata, use = use)
                }, cov = {
                  C <- cov(sampledata, use = use)
                  covar <- TRUE
                }, tet = {
                  C <- tetrachoric(sampledata)$rho
                }, poly = {
                  C <- polychoric(sampledata)$rho
                }, mixed = {
                  C <- mixed.cor(sampledata, use = use)$rho
                }, Yuleb = {
                  C <- YuleCor(sampledata, , bonett = TRUE)$rho
                }, YuleQ = {
                  C <- YuleCor(sampledata, 1)$rho
                }, YuleY = {
                  C <- YuleCor(sampledata, 0.5)$rho
                })
                bad <- any(is.na(C))
            }
            values.samp <- eigen(C)$values
            temp[["samp"]] <- values.samp
            if (fa != "pc") {
                if (SMC) {
                  sampler <- C
                  diag(sampler) <- smc(sampler)
                  temp[["samp.fa"]] <- eigen(sampler)$values
                }
                else {
                  temp[["samp.fa"]] <- fa(C, fm = fm, SMC = FALSE, 
                    warnings = FALSE)$values
                }
            }
        }
        if (sim) {
            simdata = matrix(rnorm(nsub * nvariables), nrow = nsub, 
                ncol = nvariables)
            sim.cor <- cor(simdata)
            temp[["sim"]] <- eigen(sim.cor)$values
            if (fa != "pc") {
                if (SMC) {
                  diag(sim.cor) <- smc(sim.cor)
                  temp[["sim.fa"]] <- eigen(sim.cor)$values
                }
                else {
                  fa.values.sim <- fa(sim.cor, fm = fm, SMC = FALSE, 
                    warnings = FALSE)$values
                  temp[["sim.fa"]] <- fa.values.sim
                }
            }
        }
        replicates <- list(samp = temp[["samp"]], samp.fa = temp[["samp.fa"]], 
            sim = temp[["sim"]], sim.fa = temp[["sim.fa"]])
    })
    if (is.null(ylabel)) {
        if (fa != "pc") {
            ylabel <- "eigenvalues of principal components and factor analysis"
        }
        else {
            ylabel <- "eigen values of principal components"
        }
    }
    values <- t(matrix(unlist(templist), ncol = n.iter))
    values.sim.mean = colMeans(values, na.rm = TRUE)
    if (!missing(quant)) {
        values.ci = apply(values, 2, function(x) quantile(x, 
            quant))
    }
    else {
        values.ci <- values.sim.mean
    }
    if (se.bars) {
        values.sim.se <- apply(values, 2, sd, na.rm = TRUE)/sqrt(n.iter)
    }
    else {
        values.sim.se <- apply(values, 2, sd, na.rm = TRUE)
    }
    ymax <- max(valuesx, values.sim.mean)
    sim.pcr <- sim.far <- NA
    switch(fa, pc = {
        plot(valuesx, type = "b", main = main, ylab = ylabel, 
            ylim = c(0, ymax), xlab = "Component Number", pch = 4, 
            col = "blue")
        if (resample) {
            sim.pcr <- values.sim.mean[1:nvariables]
            sim.pcr.ci <- values.ci[1:nvariables]
            sim.se.pcr <- values.sim.se[1:nvariables]
            points(sim.pcr, type = "l", lty = "dashed", pch = 4, 
                col = "red")
        } else {
            sim.pcr <- NA
            sim.se.pc <- NA
        }
        if (sim) {
            if (resample) {
                sim.pc <- values.sim.mean[(nvariables + 1):(2 * 
                  nvariables)]
                sim.pc.ci <- values.ci[(nvariables + 1):(2 * 
                  nvariables)]
                sim.se.pc <- values.sim.se[(nvariables + 1):(2 * 
                  nvariables)]
            } else {
                sim.pc <- values.sim.mean[1:nvariables]
                sim.pc.ci <- values.ci[1:nvariables]
                sim.se.pc <- values.sim.se[1:nvariables]
            }
            points(sim.pc, type = "l", lty = "dotted", pch = 4, 
                col = "red")
            pc.test <- which(!(valuesx > sim.pc.ci))[1] - 1
        } else {
            sim.pc <- NA
            sim.pc.ci <- NA
            sim.se.pc <- NA
            pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 1
        }
        fa.test <- NA
        sim.far <- NA
        sim.fa <- NA
    }, fa = {
        ylabel <- "eigen values of principal factors"
        plot(fa.valuesx, type = "b", main = main, ylab = ylabel, 
            ylim = c(0, ymax), xlab = "Factor Number", pch = 2, 
            col = "blue")
        sim.se.pc <- NA
        if (resample) {
            sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                nvariables)]
            sim.far.ci <- values.ci[(nvariables + 1):(2 * nvariables)]
            sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                nvariables)]
            points(sim.far, type = "l", lty = "dashed", pch = 2, 
                col = "red")
        }
        if (sim) {
            if (resample) {
                sim.fa <- values.sim.mean[(3 * nvariables + 1):(4 * 
                  nvariables)]
                sim.fa.ci <- values.ci[(3 * nvariables + 1):(4 * 
                  nvariables)]
                sim.se.fa <- values.sim.se[(3 * nvariables + 
                  1):(4 * nvariables)]
            } else {
                sim.fa <- values.sim.mean[(nvariables + 1):(2 * 
                  nvariables)]
                sim.fa.ci <- values.sim.mean[(nvariables + 1):(2 * 
                  nvariables)]
                sim.se.fa <- values.sim.se[(nvariables + 1):(2 * 
                  nvariables)]
                sim.far <- NA
                sim.far.ci <- NA
                sim.se.far <- NA
            }
            points(sim.fa, type = "l", lty = "dotted", pch = 2, 
                col = "red")
            fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
                1
        } else {
            sim.fa <- NA
            fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
                1
        }
        sim.pc <- NA
        sim.pcr <- NA
        sim.se.pc <- NA
        pc.test <- NA
    }, both = {
        plot(valuesx, type = "b", main = main, ylab = ylabel, 
            ylim = c(0, ymax), xlab = "Factor/Component Number", 
            pch = 4, col = "blue")
        points(fa.valuesx, type = "b", pch = 2, col = "blue")
        if (sim) {
            if (resample) {
                sim.pcr <- values.sim.mean[1:nvariables]
                sim.pcr.ci <- values.ci[1:nvariables]
                sim.se.pcr <- values.sim.se[1:nvariables]
                sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                  nvariables)]
                sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                  nvariables)]
                sim.far.ci <- values.ci[(nvariables + 1):(2 * 
                  nvariables)]
                sim.pc <- values.sim.mean[(2 * nvariables + 1):(3 * 
                  nvariables)]
                sim.pc.ci <- values.ci[(2 * nvariables + 1):(3 * 
                  nvariables)]
                sim.se.pc <- values.sim.se[(2 * nvariables + 
                  1):(3 * nvariables)]
                sim.fa <- values.sim.mean[(3 * nvariables + 1):(4 * 
                  nvariables)]
                sim.fa.ci <- values.ci[(3 * nvariables + 1):(4 * 
                  nvariables)]
                sim.se.fa <- values.sim.se[(3 * nvariables + 
                  1):(4 * nvariables)]
                pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 
                  1
                fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
                  1
            } else {
                sim.pc <- values.sim.mean[1:nvariables]
                sim.pc.ci <- values.ci[1:nvariables]
                sim.se.pc <- values.sim.se[1:nvariables]
                sim.fa <- values.sim.mean[(nvariables + 1):(2 * 
                  nvariables)]
                sim.fa.ci <- values.ci[(nvariables + 1):(2 * 
                  nvariables)]
                sim.se.fa <- values.sim.se[(nvariables + 1):(2 * 
                  nvariables)]
                pc.test <- which(!(valuesx > sim.pc.ci))[1] - 
                  1
                fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
                  1
            }
            points(sim.pc, type = "l", lty = "dotted", pch = 4, 
                col = "red")
            points(sim.fa, type = "l", lty = "dotted", pch = 4, 
                col = "red")
            points(sim.pcr, type = "l", lty = "dashed", pch = 2, 
                col = "red")
            points(sim.far, type = "l", lty = "dashed", pch = 2, 
                col = "red")
            pc.test <- which(!(valuesx > sim.pc.ci))[1] - 1
            fa.test <- which(!(fa.valuesx > sim.fa.ci))[1] - 
                1
        } else {
            sim.pcr <- values.sim.mean[1:nvariables]
            sim.pcr.ci <- values.ci[1:nvariables]
            sim.se.pcr <- values.sim.se[1:nvariables]
            sim.far <- values.sim.mean[(nvariables + 1):(2 * 
                nvariables)]
            sim.far.ci <- values.ci[(nvariables + 1):(2 * nvariables)]
            sim.se.far <- values.sim.se[(nvariables + 1):(2 * 
                nvariables)]
            sim.fa <- NA
            sim.pc <- NA
            sim.se.fa <- NA
            sim.se.pc <- NA
            pc.test <- which(!(valuesx > sim.pcr.ci))[1] - 1
            fa.test <- which(!(fa.valuesx > sim.far.ci))[1] - 
                1
        }
        if (resample) {
            points(sim.pcr, type = "l", lty = "dashed", pch = 4, 
                col = "red")
            points(sim.far, type = "l", lty = "dashed", pch = 4, 
                col = "red")
        }
    })
    if (error.bars) {
        if (!any(is.na(sim.pc))) {
            for (i in 1:length(sim.pc)) {
                ycen <- sim.pc[i]
                yse <- sim.se.pc[i]
                arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                  length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                  lty = NULL, lwd = par("lwd"), xpd = NULL)
            }
        }
        if (!any(is.na(sim.pcr))) {
            for (i in 1:length(sim.pcr)) {
                ycen <- sim.pcr[i]
                yse <- sim.se.pcr[i]
                arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                  length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                  lty = NULL, lwd = par("lwd"), xpd = NULL)
            }
        }
        if (!any(is.na(sim.fa))) {
            for (i in 1:length(sim.fa)) {
                ycen <- sim.fa[i]
                yse <- sim.se.fa[i]
                arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                  length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                  lty = NULL, lwd = par("lwd"), xpd = NULL)
            }
        }
        if (!any(is.na(sim.far))) {
            for (i in 1:length(sim.far)) {
                ycen <- sim.far[i]
                yse <- sim.se.far[i]
                arrows(i, ycen - ci * yse, i, ycen + ci * yse, 
                  length = arrow.len, angle = 90, code = 3, col = par("fg"), 
                  lty = NULL, lwd = par("lwd"), xpd = NULL)
            }
        }
    }
    if (show.legend) {
        if (is.null(n.obs)) {
            switch(fa, both = {
                if (sim) {
                  legend("topright", c("  PC  Actual Data", "  PC  Simulated Data", 
                    " PC  Resampled Data", "  FA  Actual Data", 
                    "  FA  Simulated Data", " FA  Resampled Data"), 
                    col = c("blue", "red", "red", "blue", "red", 
                      "red"), pch = c(4, NA, NA, 2, NA, NA), 
                    text.col = "green4", lty = c("solid", "dotted", 
                      "dashed", "solid", "dotted", "dashed"), 
                    merge = TRUE, bg = "gray90")
                } else {
                  legend("topright", c("  PC  Actual Data", " PC  Resampled Data", 
                    "  FA  Actual Data", " FA  Resampled Data"), 
                    col = c("blue", "red", "blue", "red"), pch = c(4, 
                      NA, 2, NA, NA), text.col = "green4", lty = c("solid", 
                      "dashed", "solid", "dashed"), merge = TRUE, 
                    bg = "gray90")
                }
            }, pc = {
                if (sim) {
                  legend("topright", c("  PC  Actual Data", "  PC  Simulated Data", 
                    " PC  Resampled Data"), col = c("blue", "red", 
                    "red", "blue", "red", "red"), pch = c(4, 
                    NA, NA, 2, NA, NA), text.col = "green4", 
                    lty = c("solid", "dotted", "dashed", "solid", 
                      "dotted", "dashed"), merge = TRUE, bg = "gray90")
                } else {
                  legend("topright", c("  PC  Actual Data", " PC  Resampled Data"), 
                    col = c("blue", "red", "red", "blue", "red", 
                      "red"), pch = c(4, NA, NA, 2, NA, NA), 
                    text.col = "green4", lty = c("solid", "dashed", 
                      "solid", "dotted", "dashed"), merge = TRUE, 
                    bg = "gray90")
                }
            }, fa = {
                if (sim) {
                  legend("topright", c("  FA  Actual Data", "  FA  Simulated Data", 
                    " FA  Resampled Data"), col = c("blue", "red", 
                    "red", "blue", "red", "red"), pch = c(4, 
                    NA, NA, 2, NA, NA), text.col = "green4", 
                    lty = c("solid", "dotted", "dashed", "solid", 
                      "dotted", "dashed"), merge = TRUE, bg = "gray90")
                } else {
                  legend("topright", c("  FA  Actual Data", " FA  Resampled Data"), 
                    col = c("blue", "red", "red", "blue", "red", 
                      "red"), pch = c(4, NA, NA, 2, NA, NA), 
                    text.col = "green4", lty = c("solid", "dashed", 
                      "solid", "dotted", "dashed"), merge = TRUE, 
                    bg = "gray90")
                }
            })
        }
        else {
            switch(fa, both = {
                legend("topright", c("PC  Actual Data", " PC  Simulated Data", 
                  "FA  Actual Data", " FA  Simulated Data"), 
                  col = c("blue", "red", "blue", "red"), pch = c(4, 
                    NA, 2, NA), text.col = "green4", lty = c("solid", 
                    "dotted", "solid", "dotted"), merge = TRUE, 
                  bg = "gray90")
            }, pc = {
                legend("topright", c("PC  Actual Data", " PC  Simulated Data"), 
                  col = c("blue", "red", "blue", "red"), pch = c(4, 
                    NA, 2, NA), text.col = "green4", lty = c("solid", 
                    "dotted", "solid", "dotted"), merge = TRUE, 
                  bg = "gray90")
            }, fa = {
                legend("topright", c("FA  Actual Data", " FA  Simulated Data"), 
                  col = c("blue", "red", "blue", "red"), pch = c(4, 
                    NA, 2, NA), text.col = "green4", lty = c("solid", 
                    "dotted", "solid", "dotted"), merge = TRUE, 
                  bg = "gray90")
            })
        }
    }
    colnames(values) <- paste0("Sim", 1:ncol(values))
    if (fa != "pc") 
        abline(h = 1)
    results <- list(fa.values = fa.valuesx, pc.values = valuesx, 
        pc.sim = sim.pc, pc.simr = sim.pcr, fa.sim = sim.fa, 
        fa.simr = sim.far, nfact = fa.test, ncomp = pc.test, 
        Call = cl)
    if (fa == "pc") {
        colnames(values)[1:nvariables] <- paste0("C", 1:nvariables)
    }
    else {
        colnames(values)[1:(2 * nvariables)] <- c(paste0("C", 
            1:nvariables), paste0("F", 1:nvariables))
        if (sim) {
            if (resample) 
                colnames(values)[(2 * nvariables + 1):ncol(values)] <- c(paste0("CSim", 
                  1:nvariables), paste0("Fsim", 1:nvariables))
        }
        results$nfact <- fa.test
    }
    results$ncomp <- pc.test
    results$values <- values
    cat("Parallel analysis suggests that ")
    cat("the number of factors = ", fa.test, " and the number of components = ", 
        pc.test, "\n")
    class(results) <- c("psych", "parallel")
    return(invisible(results))
}


target.rot <- function (x, keys = NULL) 
{
    if (!is.matrix(x) & !is.data.frame(x)) {
        if (!is.null(x$loadings)) 
            x <- as.matrix(x$loadings)
    }
    else {
        x <- x
    }
    if (ncol(x) < 2) 
        return(x)
    dn <- dimnames(x)
    if (is.null(keys)) {
        Q <- factor2cluster(x)
    }
    else {
        Q <- keys
    }
    Q <- as.matrix(Q)
    if (dim(Q)[2] < 2) {
        stop("Cluster structure produces 1 cluster. Rotation is not meaningful with less than 2 factors")
    }
    U <- lm.fit(x, Q)$coefficients
    d <- diag(solve(t(U) %*% U))
    U <- U %*% diag(sqrt(d))
    dimnames(U) <- NULL
    z <- x %*% U
    ui <- solve(U)
    Phi <- ui %*% t(ui)
    dimnames(z) <- dn
    class(z) <- "loadings"
    result <- list(loadings = z, rotmat = U, Phi = Phi)
    class(result) <- c("psych", "fa")
    return(result)
}


paired.r <- function (xy, xz, yz = NULL, n, n2 = NULL, twotailed = TRUE) 
{
    cl <- match.call()
    if (!is.null(yz)) {
        diff <- xy - xz
        determin = 1 - xy * xy - xz * xz - yz * yz + 2 * xy * 
            xz * yz
        av = (xy + xz)/2
        cube = (1 - yz) * (1 - yz) * (1 - yz)
        t2 = diff * sqrt((n - 1) * (1 + yz)/(((2 * (n - 1)/(n - 
            3)) * determin + av * av * cube)))
        p <- pt(abs(t2), n - 2, lower.tail = FALSE)
        if (twotailed) 
            p <- 2 * p
        value <- list(test = "test of difference between two correlated  correlations", 
            t = t2, p = p, Call = cl)
    }
    else {
        xy.z <- 0.5 * log((1 + xy)/(1 - xy))
        xz.z <- 0.5 * log((1 + xz)/(1 - xz))
        if (is.null(n2)) 
            n2 <- n
        se.diff.r <- sqrt(1/(n - 3) + 1/(n2 - 3))
        diff <- xy.z - xz.z
        z <- abs(diff/se.diff.r)
        p <- (1 - pnorm(z))
        if (twotailed) 
            p <- 2 * p
        value <- list(test = "test of difference between two independent correlations", 
            z = z, p = p, Call = cl)
    }
    class(value) <- c("psych", "paired.r")
    return(value)
}


fa.congruence <- function (x, y = NULL, digits = 2, use = NULL, structure = FALSE) 
{
    if (is.null(y) && is.list(x)) {
        n <- length(x)
        for (i in 1:n) {
            xi <- x[[i]]
            if (length(class(xi)) > 1) {
                if (class(xi)[2] == "omega") {
                  xi <- xi$schmid$sl
                  xi <- as.matrix(xi[, 1:(ncol(xi) - 2)])
                }
            }
            if (!(is.matrix(xi))) {
                if (!is.null(xi$loadings)) {
                  if (structure) {
                    xi <- xi$Structure
                  }
                  else {
                    xi <- xi$loadings
                  }
                }
                else {
                  xi <- as.matrix(xi)
                }
            }
            if (i == 1) {
                xg <- xi
            }
            else {
                xg <- cbind(xg, xi)
            }
        }
        x <- xg
        if (is.null(y)) 
            y <- xg
    }
    else {
        if (length(class(x)) > 1) {
            if (class(x)[2] == "omega") {
                x <- x$schmid$sl
                x <- as.matrix(x[, 1:(ncol(x) - 2)])
            }
        }
        if (length(class(y)) > 1) {
            if (class(y)[2] == "omega") {
                y <- y$schmid$sl
                y <- as.matrix(y[, 1:(ncol(y) - 2)])
            }
        }
        if (!is.matrix(x)) {
            if (!is.null(x$loadings)) {
                if (structure) {
                  x <- x$Structure
                }
                else {
                  x <- x$loadings
                }
            }
            else {
                x <- as.matrix(x)
            }
        }
        if (!is.matrix(y)) {
            if (!is.null(y$loadings)) {
                if (structure) {
                  y <- y$Structure
                }
                else {
                  y <- y$loadings
                }
            }
            else {
                y <- as.matrix(y)
            }
        }
    }
    if (any(is.na(x) | any(is.na(y)))) {
        warning("Some loadings were missing.")
        if (!is.null(use)) {
            message("Analysis is  done on complete cases")
            if (any(is.na(x))) {
                xc <- x[complete.cases(x), ]
                y <- y[complete.cases(x), ]
                x <- xc
            }
            if (any(is.na(y))) {
                yc <- y[complete.cases(y), ]
                x <- x[complete.cases(y), ]
                y <- yc
            }
        }
        else {
            warning("Check your data or rerun with the  use = complete option")
        }
    }
    nx <- dim(x)[2]
    ny <- dim(y)[2]
    cross <- t(y) %*% x
    sumsx <- sqrt(1/diag(t(x) %*% x))
    sumsy <- sqrt(1/diag(t(y) %*% y))
    result <- matrix(rep(0, nx * ny), ncol = nx)
    result <- round(sumsy * (cross * rep(sumsx, each = ny)), 
        digits)
    return(t(result))
}


cortest <- function (R1, R2 = NULL, n1 = NULL, n2 = NULL, fisher = TRUE, 
    cor = TRUE) 
{
    cl <- match.call()
    if ((dim(R1)[1] != dim(R1)[2]) & cor) {
        n1 <- dim(R1)[1]
        message("R1 was not square, finding R from data")
        R1 <- cor(R1, use = "pairwise")
    }
    if (!is.matrix(R1)) 
        R1 <- as.matrix(R1)
    p <- dim(R1)[2]
    if (is.null(n1)) {
        n1 <- 100
        warning("n not specified, 100 used")
    }
    if (is.null(R2)) {
        if (fisher) {
            R <- 0.5 * log((1 + R1)/(1 - R1))
            R2 <- R * R
        }
        else {
            R2 <- R1 * R1
        }
        if (cor) {
            diag(R2) <- 0
            E <- (sum(R2 * lower.tri(R2)))
            z <- sum(R * lower.tri(R))
            df <- p * (p - 1)/2
        }
        else {
            E <- sum(R2)
            z <- sum(R1)
            df <- ncol(R1) * nrow(R1)
        }
        chisq <- E * (n1 - 3)
        z <- z/sqrt(n1 - 3)
        p.val <- pchisq(chisq, df, lower.tail = FALSE)
    }
    else {
        if ((dim(R2)[1] != dim(R2)[2]) & cor) {
            n2 <- dim(R2)[1]
            message("R2 was not square, finding R from data")
            R2 <- cor(R2, use = "pairwise")
        }
        if (!is.matrix(R2)) 
            R2 <- as.matrix(R2)
        if (fisher) {
            R1 <- 0.5 * log((1 + R1)/(1 - R1))
            R2 <- 0.5 * log((1 + R2)/(1 - R2))
            if (cor) {
                diag(R1) <- 0
                diag(R2) <- 0
            }
        }
        R <- R1 - R2
        R2 <- R * R
        if (is.null(n2)) 
            n2 <- n1
        n <- (n1 * n2)/(n1 + n2)
        if (cor) {
            E <- (sum(R2 * lower.tri(R2)))
            chisq <- E * (n - 3)
            df <- p * (p - 1)/2
            z <- sum(R2 * lower.tri(R2))/sqrt(n - 3)
        }
        else {
            E <- sum(R2)
            chisq <- E * (n - 3)
            df <- ncol(R2) * nrow(R2)
            z <- sum(R2)/sqrt(n - 3)
        }
        p.val <- pchisq(chisq, df, lower.tail = FALSE)
    }
    if (is.null(n2)) 
        z <- NULL
    result <- list(chi2 = chisq, prob = p.val, df = df, z = z, 
        Call = cl)
    class(result) <- c("psych", "cortest")
    return(result)
}


fa.organize <- function (fa.results, o = NULL, i = NULL, cn = NULL) 
{
    if (!is.null(o)) {
        fa.results$loadings <- fa.results$loadings[, o]
        fa.results$Structure <- fa.results$Structure[, o]
        fa.results$weights <- fa.results$weights[, o]
        fa.results$valid <- fa.results$valid[o]
        fa.results$score.cor <- fa.results$score.cor[o, o]
        fa.results$r.scores <- fa.results$r.scores[o, o]
        fa.results$R2 <- fa.results$R2[o]
        if (!is.null(cn)) {
            colnames(fa.results$loadings) <- cn
        }
        fa.results$Phi <- fa.results$Phi[o, o]
    }
    if (!is.null(i)) {
        fa.results$loadings <- fa.results$loadings[i, ]
        fa.results$Structure <- fa.results$Structure[i, ]
        fa.results$weights <- fa.results$weights[i, ]
        fa.results$complexity = fa.results$complexity[i]
        fa.results$uniquenesses <- fa.results$uniquenesses[i]
    }
    return(fa.results)
}


error.crosses <- function (x, y, labels = NULL, main = NULL, xlim = NULL, ylim = NULL, 
    xlab = NULL, ylab = NULL, pos = NULL, offset = 1, arrow.len = 0.2, 
    alpha = 0.05, sd = FALSE, add = FALSE, colors = NULL, col.arrows = NULL, 
    col.text = NULL, ...) 
{
    if (is.vector(x)) {
        x <- describe(x)
    }
    xmin <- min(x$mean)
    xmax <- max(x$mean)
    if (sd) {
        max.sex <- max(x$sd, na.rm = TRUE)
        if (is.null(xlim)) {
            xlim = c(xmin - max.sex, xmax + max.sex)
        }
    }
    else {
        max.sex <- max(x$se, na.rm = TRUE)
    }
    if (is.vector(y)) {
        y <- describe(y)
    }
    ymin <- min(y$mean)
    ymax <- max(y$mean)
    if (sd) {
        max.sey <- max(y$sd, na.rm = TRUE)
        if (is.null(ylim)) {
            ylim = c(ymin - max.sey, ymax + max.sey)
        }
    }
    else {
        max.sey <- max(y$se, na.rm = TRUE)
    }
    if (is.null(xlim)) 
        xlim = c(xmin - 2 * max.sex, xmax + 2 * max.sex)
    if (is.null(ylim)) 
        ylim = c(ymin - 2 * max.sey, ymax + 2 * max.sey)
    if (is.null(main)) {
        if (!sd) {
            main = paste((1 - alpha) * 100, "% confidence limits", 
                sep = "")
        }
        else {
            main = paste("Means and standard deviations")
        }
    }
    if (is.null(xlab)) 
        xlab <- "Group 1"
    if (is.null(ylab)) 
        ylab <- "Group 2"
    if (is.null(colors)) 
        colors <- "black"
    if (is.null(col.arrows)) 
        col.arrows <- colors
    if (is.null(col.text)) 
        col.text <- colors
    if (!add) 
        plot(x$mean, y$mean, xlim = xlim, ylim = ylim, xlab = xlab, 
            ylab = ylab, main = main, col = colors, ...)
    cix <- qt(1 - alpha/2, x$n - 1)
    ciy <- qt(1 - alpha/2, y$n - 1)
    z <- dim(x)[1]
    if (sd) {
        x$se <- x$sd
        y$se <- y$sd
        cix <- ciy <- rep(1, z)
    }
    if (is.null(pos)) {
        locate <- rep(1, z)
    }
    else {
        locate <- pos
    }
    if (is.null(labels)) {
        labels <- rownames(x)
    }
    if (is.null(labels)) {
        lab <- paste("V", 1:z, sep = "")
    }
    else {
        lab <- labels
    }
    if (length(col.arrows) < z) {
        col.arrows <- rep(col.arrows, z)
    }
    if (length(col.text) < z) {
        col.text <- rep(col.text, z)
    }
    for (i in 1:z) {
        xcen <- x$mean[i]
        ycen <- y$mean[i]
        xse <- x$se[i]
        yse <- y$se[i]
        arrows(xcen - cix[i] * xse, ycen, xcen + cix[i] * xse, 
            ycen, length = arrow.len, angle = 90, code = 3, col = col.arrows[i], 
            lty = NULL, lwd = par("lwd"), xpd = NULL)
        arrows(xcen, ycen - ciy[i] * yse, xcen, ycen + ciy[i] * 
            yse, length = arrow.len, angle = 90, code = 3, col = col.arrows[i], 
            lty = NULL, lwd = par("lwd"), xpd = NULL)
        text(xcen, ycen, labels = lab[i], pos = locate[i], offset = offset, 
            col = col.text[i], ...)
    }
}


psych <- function () 
{
}


irt.1p <- function (delta, items) 
{
    irt <- function(x, delta, scores) {
        fit <- -1 * (log(scores/(1 + exp(delta - x)) + (1 - scores)/(1 + 
            exp(x - delta))))
        mean(fit, na.rm = TRUE)
    }
    delta <- delta
    items <- items
    num <- dim(items)[1]
    fit <- matrix(NA, num, 2)
    total <- rowMeans(items, na.rm = TRUE)
    count <- rowSums(!is.na(items))
    for (i in 1:num) {
        if (count[i] > 0) {
            myfit <- optimize(irt, c(-4, 4), delta = delta, scores = items[i, 
                ])
            fit[i, 1] <- myfit$minimum
            fit[i, 2] <- myfit$objective
        }
        else {
            fit[i, 1] <- NA
            fit[i, 2] <- NA
        }
    }
    irt.1p <- data.frame(total, theta = fit[, 1], fit = fit[, 
        2], count)
}


omegaFromSem <- function (fit, m = NULL, flip = TRUE, plot = TRUE) 
{
    if (class(fit)[1] == "lavaan") {
        fx <- fit@Model@GLIST$lambda
        m <- cov2cor(as.matrix(fit@SampleStats@cov[[1]]))
        Fit <- fit@Fit@test
        sem <- "lavaan"
        nvar <- nrow(fx)
        n.fact <- ncol(fx)
        g <- fx[, 1]
        rn <- fit@Data@ov.names[[1]]
        cfa.loads <- fx
    }
    else {
        sem <- "sem"
        nvar <- dim(m)[1]
        n.fact <- dim(fit$A)[2] - nvar
        rn <- rownames(m)
        g <- fit$A[1:nvar, nvar + n.fact]
        Fit <- NULL
        cfa.loads <- cbind(g, fit$A[1:nvar, (nvar + 1):(nvar + 
            n.fact - 1)])
    }
    if (flip) {
        flipper <- rep(1, nvar)
        flipper[g < 0] <- -1
        signkey <- strtrim(flipper, 1)
        signkey[signkey == "1"] <- ""
        rn <- paste(rn, signkey, sep = "")
        flipper <- diag(flipper)
        m <- flipper %*% m %*% t(flipper)
        g <- flipper %*% g
        cfa.loads <- flipper %*% cfa.loads
        countneg <- colSums(cfa.loads < 0)
        for (i in 1:n.fact) {
            if (countneg[i] > 0) 
                cfa.loads[, i] <- -cfa.loads[, i]
        }
    }
    w <- solve(m, cfa.loads)
    rownames(cfa.loads) <- rn
    rownames(w) <- rn
    gR2 <- diag(t(w) %*% cfa.loads)
    Vt <- sum(m)
    omh.sem <- sum(g)^2/Vt
    h2 <- sum(rowSums(cfa.loads^2))
    uniq <- tr(m) - h2
    omt.sem <- (Vt - uniq)/Vt
    omg <- omgo <- omt <- rep(NA, n.fact)
    sub <- apply(cfa.loads, 1, function(x) which.max(abs(x[2:(n.fact)])))
    grs <- 0
    for (group in (1:n.fact)) {
        groupi <- which(sub == group)
        if (length(groupi) > 0) {
            Vgr <- sum(m[groupi, groupi])
            gr <- sum(cfa.loads[groupi, (group + 1)])
            grs <- grs + gr^2
            omg[group + 1] <- gr^2/Vgr
            omgo[group + 1] <- sum(cfa.loads[groupi, 1])^2/Vgr
            omt[group + 1] <- (gr^2 + sum(cfa.loads[groupi, 1])^2)/Vgr
        }
    }
    omgo[1] <- sum(cfa.loads[, 1])^2/sum(m)
    omg[1] <- grs/sum(m)
    omt[1] <- omt.sem
    om.group <- data.frame(total = omt, general = omgo, group = omg)
    class(cfa.loads) <- "loadings"
    results <- list(omega = omh.sem, omega.tot = omt.sem, cfa.loads = cfa.loads, 
        gR2 = gR2, omega.group = om.group, Fit = Fit, sem = sem)
    class(results) <- c("psych", "omegaSem")
    if (plot) 
        omega.diagram(results, sort = FALSE)
    return(results)
}


violinBy <- function (x, grp = NULL, grp.name = NULL, ylab = "Observed", 
    xlab = "", main = "Density plot", density = 20, restrict = TRUE, 
    xlim = NULL, add = FALSE, col = NULL, pch = 20, scale = NULL, 
    ...) 
{
    SCALE = 0.3
    count.valid <- function(x) {
        sum(!is.na(x))
    }
    if (missing(col)) {
        col <- c("blue", "red")
    }
    nvar <- nvarg <- ncol(x)
    if (!is.null(grp)) {
        if (!is.data.frame(grp) && !is.list(grp) && (length(grp) < 
            NROW(x))) 
            grp <- x[, grp]
        Qnt <- apply(x, 2, function(xx) by(xx, grp, quantile, 
            prob = c(0, 1, 0.5, 0.25, 0.75), na.rm = TRUE))
        meanX <- apply(x, 2, function(xx) by(xx, grp, mean, na.rm = TRUE))
        nX <- apply(x, 2, function(xx) by(xx, grp, count.valid))
        meanX <- matrix(unlist(meanX))
        Qnt <- matrix(unlist(Qnt), nrow = 5)
        ngrp <- ncol(Qnt)/nvar
        nvarg <- ncol(Qnt)
    }
    else {
        Qnt <- apply(x, 2, quantile, prob = c(0, 1, 0.5, 0.25, 
            0.75), na.rm = TRUE)
        meanX <- apply(x, 2, mean, na.rm = TRUE)
    }
    minx <- Qnt[1, ]
    maxx <- Qnt[2, ]
    medx <- Qnt[3, ]
    Q25 <- Qnt[4, ]
    Q75 <- Qnt[5, ]
    rangex <- apply(x, 2, range, na.rm = TRUE)
    names <- colnames(x)
    tot.n.obs <- nrow(x)
    if (!is.null(grp)) {
        if (missing(grp.name)) 
            grp.name <- 1:ngrp
        names <- paste(rep(names, each = ngrp), grp.name[1:ngrp], 
            sep = " ")
        col <- rep(col, nvar * ngrp)
    }
    d <- list(nvar)
    if (missing(xlim)) 
        xlim <- c(0.5, nvarg + 0.5)
    for (i in 1:nvar) {
        if (!is.null(grp)) {
            if (restrict) {
                d[[i]] <- by(x[, i], grp, function(xx) density(xx, 
                  na.rm = TRUE, from = rangex[1, i], to = rangex[2, 
                    i]))
            }
            else {
                d[[i]] <- by(x[, i], grp, function(xx) density(xx, 
                  na.rm = TRUE))
            }
        }
        else {
            if (restrict) {
                d[[i]] <- density(x[, i], na.rm = TRUE, from = minx[i], 
                  to = maxx[i])
            }
            else {
                d[[i]] <- density(x[, i], na.rm = TRUE)
            }
        }
    }
    if (!add) {
        plot(meanX, ylim = c(min(minx), max(maxx)), xlim = xlim, 
            axes = FALSE, xlab = xlab, ylab = ylab, main = main, 
            pch = pch, ...)
        axis(1, 1:nvarg, names)
        axis(2)
        box()
    }
    if (!is.null(grp)) 
        d <- unlist(d, recursive = FALSE)
    rev <- (length(d[[1]]$y):1)
    for (i in 1:nvarg) {
        if (!is.null(scale)) {
            width <- scale * sqrt(nX[[i]]/tot.n.obs)/max(d[[i]]$y)
        }
        else {
            width <- SCALE/max(d[[i]]$y)
        }
        polygon(width * c(-d[[i]]$y, d[[i]]$y[rev]) + i, c(d[[i]]$x, 
            d[[i]]$x[rev]), density = density, col = col[i], 
            ...)
        dmd <- max(which(d[[i]]$x < medx[i]))
        d25 <- max(which(d[[i]]$x <= Q25[i]))
        d75 <- max(which(d[[i]]$x <= Q75[i]))
        segments(x0 = width * d[[i]]$y[dmd] + i, y0 = d[[i]]$x[dmd], 
            x1 = -width * d[[i]]$y[dmd] + i, y1 = d[[i]]$x[dmd], 
            lwd = 2)
        segments(x0 = width * d[[i]]$y[d25] + i, y0 = d[[i]]$x[d25], 
            x1 = -width * d[[i]]$y[d25] + i, y1 = d[[i]]$x[d25])
        segments(x0 = width * d[[i]]$y[d75] + i, y0 = d[[i]]$x[d75], 
            x1 = -width * d[[i]]$y[d75] + i, y1 = d[[i]]$x[d75])
    }
}


moderate.diagram <- function (medi, digits = 2, ylim = c(2, 8), main = "Moderation model", 
    ...) 
{
    xlim = c(0, 10)
    plot(NA, xlim = xlim, ylim = ylim, main = main, axes = FALSE, 
        xlab = "", ylab = "")
    var.names <- rownames(medi$direct)
    x.names <- rownames(medi$direct)
    y.names <- colnames(medi$direct)
    beta <- round(medi$direct, digits)
    nx <- length(x.names)
    ny <- length(y.names)
    top <- max(nx, ny)
    xlim = c(-nx/3, 10)
    ylim = c(0, top)
    top <- max(nx, ny)
    x <- list()
    y <- list()
    x.scale <- top/(nx + 1)
    y.scale <- top/(ny + 1)
    plot(NA, xlim = xlim, ylim = ylim, main = main, axes = FALSE, 
        xlab = "", ylab = "")
    for (i in 1:nx) {
        x[[i]] <- dia.rect(2, top - i * x.scale, x.names[i])
    }
    for (j in 1:ny) {
        y[[j]] <- dia.rect(7, top - j * y.scale, y.names[j])
    }
    y[[1]] <- dia.rect(7, top - y.scale, y.names[1])
    for (j in 1:ny) {
        for (i in 1:nx) {
            dia.arrow(x[[i]]$right, y[[j]]$left, labels = beta[i, 
                1], adj = 2)
        }
    }
    if (nx > 1) {
        for (i in 2:nx) {
            for (k in 1:(i - 1)) {
                dia.curved.arrow(x[[i]]$left, x[[k]]$left, round(medi$C[i, 
                  k], 2), scale = -(abs(k - i)), both = TRUE)
            }
        }
    }
}


Yule.inv <- function (Q, m, n = NULL) 
{
    if (length(m) > 2) {
        R1 <- m[1]
        R2 <- m[2]
        C1 <- m[3]
        C2 <- m[4]
    }
    else {
        if (is.null(n)) {
            n <- 1
        }
        R1 <- m[1]
        R2 <- n - R1
        C1 <- m[2]
        C2 <- n - C1
    }
    f <- function(x) {
        (Q - Yule(c(x, R1 - x, C1 - x, R2 - C1 + x)))^2
    }
    xval <- optimize(f, c(min(R1 - min(C2, R1), C1 - min(R2, 
        C1)), min(R1, C1)))
    R <- matrix(ncol = 2, nrow = 2)
    x <- xval$minimum
    R[1, 1] <- x
    R[1, 2] <- R1 - x
    R[2, 1] <- C1 - x
    R[2, 2] <- R2 - C1 + x
    return(R)
}


fa2latex <- function (f, digits = 2, rowlabels = TRUE, apa = TRUE, short.names = FALSE, 
    cumvar = FALSE, cut = 0, big = 0.3, alpha = 0.05, font.size = "scriptsize", 
    heading = "A factor analysis table from the psych package in R", 
    caption = "fa2latex", label = "default", silent = FALSE, 
    file = NULL, append = FALSE) 
{
    if (class(f)[2] == "fa.ci") {
        if (is.null(f$cip)) {
            px <- f$cis$p
        }
        else {
            px <- f$cip
        }
    }
    else {
        px <- NULL
    }
    x <- unclass(f$loadings)
    if (!is.null(f$Phi)) {
        Phi <- f$Phi
    }
    else {
        Phi <- NULL
    }
    nfactors <- ncol(x)
    if (nfactors > 1) {
        if (is.null(Phi)) {
            h2 <- rowSums(x^2)
        }
        else {
            h2 <- diag(x %*% Phi %*% t(x))
        }
    }
    else {
        h2 <- x^2
    }
    u2 <- 1 - h2
    vtotal <- sum(h2 + u2)
    if (cut > 0) 
        x[abs(x) < cut] <- NA
    if (!is.null(f$complexity)) {
        x <- data.frame(x, h2 = h2, u2 = u2, com = f$complexity)
    }
    else {
        x <- data.frame(x, h2 = h2, u2 = u2)
    }
    nvar <- dim(x)[2]
    comment <- paste("% Called in the psych package ", match.call())
    header <- paste("\\begin{table}[htpb]", "\\caption{", caption, 
        "}\n\\begin{center}\n\\begin{", font.size, "} \n\\begin{tabular}", 
        sep = "")
    header <- c(header, "{l", rep("r", nvar), "}\n")
    if (apa) 
        header <- c(header, "\\multicolumn{", nvar, "}{l}{", 
            heading, "}", "\\cr \n \\hline ")
    if (apa) {
        footer <- paste(" \\hline ")
    }
    footer <- paste(footer, "\n\\end{tabular}\n\\end{", font.size, 
        "}\n\\end{center}\n\\label{", label, "}\n\\end{table} \n\n", 
        sep = "")
    x <- round(x, digits = digits)
    cname <- colnames(x)
    if (short.names) 
        cname <- 1:nvar
    names1 <- paste(cname[1:(nvar - 1)], " & ")
    lastname <- paste(cname[nvar], "\\cr \n")
    if (apa) {
        allnames <- c("Variable  &  ", names1, lastname, " \\hline \n")
    }
    else {
        allnames <- c("  &  ", names1, lastname, "\\cr \n")
    }
    fx <- format(x, drop0trailing = FALSE)
    {
        if (!is.null(px) && (cut == 0)) {
            temp <- fx[1:nfactors]
            temp[px < alpha] <- paste("\\bf{", temp[px < alpha], 
                "}", sep = "")
            fx[1:nfactors] <- temp
        }
        if (big > 0) {
            temp <- fx[1:nfactors]
            x <- x[1:nfactors]
            temp[!is.na(x) & (abs(x) > big)] <- paste("\\bf{", 
                temp[!is.na(x) & (abs(x) > big)], "}", sep = "")
            fx[1:nfactors] <- temp
        }
        value <- apply(fx, 1, paste, collapse = "  &  ")
        value <- gsub("NA", "  ", value, fixed = TRUE)
        if (rowlabels) 
            value <- {
                paste(sanitize.latex(names(value)), "  & ", value)
            }
        else {
            paste("  &  ", value)
        }
        values <- paste(value, "\\cr", "\n")
        if (!silent) {
            cat(comment, "\n")
            cat(header)
            cat(allnames)
            cat(values)
        }
        x <- f$loadings
        nvar <- nrow(x)
        if (is.null(Phi)) {
            if (nfactors > 1) {
                vx <- colSums(x^2)
            }
            else {
                vx <- diag(t(x) %*% x)
                vx <- vx * nvar/vtotal
            }
        }
        else {
            vx <- diag(Phi %*% t(x) %*% x)
            vx <- vx * nvar/vtotal
        }
        vx <- round(vx, digits)
        loads <- c("\\hline \\cr SS loadings &", paste(vx, " & ", 
            sep = ""), "\\cr  \n")
        if (!silent) {
            cat(loads)
        }
        summ <- NULL
        if (cumvar) {
            provar <- round(vx/nvar, digits)
            summ <- c("Proportion Var &", paste(provar, "  & ", 
                sep = ""), "\\cr \n")
            if (nfactors > 1) {
                cumvar <- round(cumsum(vx/nvar), digits)
                cumfavar <- round(cumsum(vx/sum(vx)), digits = digits)
                summ <- c(summ, "Cumulative Var & ", paste(cumvar, 
                  " & ", sep = ""), "\\cr \n", "Cum. factor Var & ", 
                  paste(round(cumsum(vx/sum(vx)), digits = digits), 
                    "  & ", sep = ""), "\\cr \n")
            }
            if (!silent) {
                cat(summ)
            }
        }
        loads <- c(loads, summ)
        if (!is.null(Phi)) {
            summ <- c("\\cr \n            \\hline \\cr \n")
            if (!silent) {
                cat(summ)
            }
            Phi <- round(Phi, digits)
            phi <- format(Phi, nsmall = digits)
            phi <- apply(phi, 1, paste, collapse = " & ")
            phi <- paste(colnames(x), "  &", phi)
            phi <- paste(phi, "\\cr", "\n")
            loads <- c(loads, summ, phi)
            if (!silent) {
                cat(phi)
            }
        }
        if (!silent) {
            cat(footer)
        }
    }
    values <- c(values, loads)
    result <- c(header, allnames, values, footer)
    if (!is.null(file)) 
        write.table(result, file = file, row.names = FALSE, col.names = FALSE, 
            quote = FALSE, append = append)
    invisible(result)
}


sim.poly.ideal <- function (nvar = 5, n = 500, low = -2, high = 2, a = NULL, c = 0, 
    z = 1, d = NULL, mu = 0, sd = 1, cat = 5, mod = "logistic") 
{
    if (mod == "normal") {
        result <- sim.poly.ideal.npn(nvar, n, low, high, a, c, 
            z, d, mu, sd, cat)
    }
    else {
        result <- sim.poly.ideal.npl(nvar, n, low, high, a, c, 
            z, d, mu, sd, cat)
    }
    return(result)
}


interp.qplot.by <- function (y, x, w = 1, na.rm = TRUE, xlab = "group", ylab = "dependent", 
    ylim = NULL, arrow.len = 0.05, typ = "b", add = FALSE, ...) 
{
    z <- by(y, x, interp.quartiles)
    zname <- names(z)
    z <- matrix(unlist(z), ncol = 3, byrow = TRUE)
    rownames(z) <- zname
    colnames(z) <- c("Q1", "Median", "Q3")
    xv <- as.numeric(zname)
    ymin <- min(y)
    ymax <- max(y)
    if (is.null(ylim)) {
        ylim <- c(ymin, ymax)
    }
    if (add) {
        points(xv, z[, 2], typ = typ, ...)
    }
    else {
        plot(xv, z[, 2], ylim = ylim, xlab = xlab, ylab = ylab, 
            typ = typ, ...)
    }
    lenx <- length(xv)
    for (i in 1:lenx) {
        xlow <- z[i, 1]
        xcen <- z[i, 2]
        xup <- z[i, 3]
        arrows(xv[i], xlow, xv[i], xup, length = arrow.len, angle = 90, 
            code = 3, lty = NULL, lwd = par("lwd"), xpd = NULL, 
            ...)
    }
}


write.file.csv <- function (x, f = NULL, row.names = FALSE, ...) 
{
    if (missing(f)) 
        f <- file.choose(TRUE)
    write.table(x, f, sep = ",", row.names = row.names, ...)
}


sim.item <- function (nvar = 72, nsub = 500, circum = FALSE, xloading = 0.6, 
    yloading = 0.6, gloading = 0, xbias = 0, ybias = 0, categorical = FALSE, 
    low = -3, high = 3, truncate = FALSE, cutpoint = 0) 
{
    avloading <- (xloading + yloading)/2
    errorweight <- sqrt(1 - (avloading^2 + gloading^2))
    g <- rnorm(nsub)
    truex <- rnorm(nsub) * xloading + xbias
    truey <- rnorm(nsub) * yloading + ybias
    if (circum) {
        radia <- seq(0, 2 * pi, len = nvar + 1)
        rad <- radia[which(radia < 2 * pi)]
    }
    else rad <- c(rep(0, nvar/4), rep(pi/2, nvar/4), rep(pi, 
        nvar/4), rep(3 * pi/2, nvar/4))
    error <- matrix(rnorm(nsub * (nvar)), nsub)
    trueitem <- outer(truex, cos(rad)) + outer(truey, sin(rad))
    item <- gloading * g + trueitem + errorweight * error
    if (categorical) {
        item = round(item)
        item[(item <= low)] <- low
        item[(item > high)] <- high
    }
    if (truncate) {
        item[item < cutpoint] <- 0
    }
    colnames(item) <- paste("V", 1:nvar, sep = "")
    return(item)
}


iclust <- function (r.mat, nclusters = 0, alpha = 3, beta = 1, beta.size = 4, 
    alpha.size = 3, correct = TRUE, correct.cluster = TRUE, reverse = TRUE, 
    beta.min = 0.5, output = 1, digits = 2, labels = NULL, cut = 0, 
    n.iterations = 0, title = "ICLUST", plot = TRUE, weighted = TRUE, 
    cor.gen = TRUE, SMC = TRUE, purify = TRUE, diagonal = FALSE) 
{
    cl <- match.call()
    if (is.null(labels)) {
        labels <- colnames(r.mat)
    }
    else {
        if ((length(labels) == 1) && (!labels)) 
            labels <- NULL
    }
    ICLUST.debug <- FALSE
    ICLUST.options <- list(n.clus = nclusters, alpha = alpha, 
        beta = beta, beta.size = beta.size, alpha.size = alpha.size, 
        correct = correct, correct.cluster = correct.cluster, 
        reverse = reverse, beta.min = beta.min, output = output, 
        digits = digits, weighted = weighted, cor.gen = cor.gen, 
        SMC = SMC)
    if (dim(r.mat)[1] != dim(r.mat)[2]) {
        r.mat <- cor(r.mat, use = "pairwise")
    }
    else {
        r.mat <- cov2cor(r.mat)
    }
    if (!is.matrix(r.mat)) {
        r.mat <- as.matrix(r.mat)
    }
    nvar <- dim(r.mat)[2]
    if (nvar < 3) {
        message("Cluster analysis of items is only meaningful for more than 2 variables. Otherwise, you will find one cluster that is just the composite of the two.  Beta = Alpha = 2*r/(1+r).  Have you made a mistake? \n Try calling the alpha function to give some trivial statistics.")
        stop()
    }
    if (is.null(colnames(r.mat))) {
        colnames(r.mat) <- paste("V", 1:nvar, sep = "")
    }
    if (is.null(rownames(r.mat))) {
        rownames(r.mat) <- paste("V", 1:nvar, sep = "")
    }
    if (any(is.na(r.mat))) {
        bad <- TRUE
        tempr <- r.mat
        wcl <- NULL
        while (bad) {
            wc <- table(which(is.na(tempr), arr.ind = TRUE))
            wcl <- c(wcl, as.numeric(names(which(wc == max(wc)))))
            tempr <- r.mat[-wcl, -wcl]
            if (any(is.na(tempr))) {
                bad <- TRUE
            }
            else {
                bad <- FALSE
            }
        }
        cat("\nLikely variables with missing values are ", colnames(r.mat)[wcl], 
            " \n")
        stop("I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue.\nPlease drop those variables and try again.")
    }
    if (ICLUST.options$SMC) {
        smc.items <- smc(r.mat)
    }
    else {
        smc.items <- rep(1, nvar)
    }
    iclust.results <- ICLUST.cluster(r.mat, ICLUST.options, smc.items)
    loads <- cluster.loadings(iclust.results$clusters, r.mat, 
        SMC = SMC)
    if (is.matrix(iclust.results$clusters)) {
        eigenvalue <- diag(t(loads$pattern) %*% loads$loading)
        sorted.cluster.keys.ord <- order(eigenvalue, decreasing = TRUE)
        sorted.cluster.keys <- iclust.results$clusters[, sorted.cluster.keys.ord]
        loads <- cluster.loadings(sorted.cluster.keys, r.mat, 
            SMC = SMC)
        iclust.results$clusters <- sorted.cluster.keys
        cluster.beta <- iclust.results$results[colnames(sorted.cluster.keys), 
            "beta"]
        names(cluster.beta) <- colnames(sorted.cluster.keys)
    }
    else {
        sorted.cluster.keys <- iclust.results$clusters
    }
    fits <- cluster.fit(r.mat, as.matrix(loads$loadings), iclust.results$clusters, 
        diagonal)
    sorted <- ICLUST.sort(ic.load = loads, labels = labels, cut = cut)
    if (is.matrix(sorted.cluster.keys)) {
        cluster.beta <- iclust.results$results[colnames(sorted.cluster.keys), 
            "beta"]
        names(cluster.beta) <- colnames(sorted.cluster.keys)
    }
    else {
        number.of.clusters <- dim(iclust.results$results)[1]
        cluster.beta <- iclust.results$results[number.of.clusters, 
            "beta"]
    }
    clusters <- as.matrix(iclust.results$clusters)
    if (dim(clusters)[2] == 0) {
        warning("no items meet the specification time1")
    }
    old.clusters <- clusters
    old.fit <- fits$clusterfit
    if (ICLUST.debug) {
        print(paste("clusters ", clusters))
    }
    if (purify) {
        clusters <- factor2cluster(loads, cut = cut)
        clusters <- as.matrix(clusters)
    }
    if (dim(clusters)[2] == 0) {
        warning("no items meet the specification stage 2", immediate. = TRUE)
    }
    if (ICLUST.debug) {
        print(paste("clusters ", clusters))
        print(paste("loads ", loads))
    }
    loads <- cluster.loadings(clusters, r.mat, SMC = SMC)
    if (n.iterations > 0) {
        for (steps in 1:n.iterations) {
            loads <- cluster.loadings(clusters, r.mat, SMC = SMC)
            clusters <- factor2cluster(loads, cut = cut)
            if (dim(clusters)[2] != dim(old.clusters)[2]) {
                change <- 999
                loads <- cluster.loadings(clusters, r.mat, SMC = SMC)
            }
            else {
                change <- sum(abs(clusters) - abs(old.clusters))
            }
            fit <- cluster.fit(r.mat, as.matrix(loads$loadings), 
                clusters, diagonal)
            old.clusters <- clusters
            print(paste("iterations ", steps, " change in clusters ", 
                change, "current fit ", fit$clusterfit))
            if ((abs(change) < 1) | (fit$clusterfit <= old.fit)) {
                break
            }
            old.fit <- fit$cluster.fit
        }
    }
    p.fit <- cluster.fit(r.mat, as.matrix(loads$loadings), clusters, 
        diagonal)
    p.sorted <- ICLUST.sort(ic.load = loads, labels = labels, 
        cut = cut, keys = TRUE)
    purified <- cluster.cor(p.sorted$clusters, r.mat, SMC = SMC, 
        item.smc = smc.items)
    class(loads$loadings) <- "loading"
    result <- list(title = title, clusters = iclust.results$clusters, 
        corrected = loads$corrected, loadings = loads$loadings, 
        pattern = loads$pattern, G6 = loads$G6, fit = fits, results = iclust.results$results, 
        cor = loads$cor, Phi = loads$cor, alpha = loads$alpha, 
        beta = cluster.beta, av.r = loads$av.r, size = loads$size, 
        sorted = sorted, p.fit = p.fit, p.sorted = p.sorted, 
        purified = purified, purify = purify, call = cl)
    if (plot) 
        iclust.diagram(result, labels = labels, main = title, 
            digits = digits)
    class(result) <- c("psych", "iclust")
    return(result)
}


cosinor <- function (angle, x = NULL, code = NULL, data = NULL, hours = TRUE, 
    period = 24, plot = FALSE, opti = FALSE, na.rm = TRUE) 
{
    if (!is.null(data)) {
        if (is.matrix(data)) 
            data <- data.frame(data)
        if (is.character(angle)) 
            angle <- which(colnames(data) == angle)
        if (!is.null(code)) {
            if (is.character(code)) 
                codeloc <- which(colnames(data) == code)
            x <- data[, c(angle, x, codeloc)]
        }
        else {
            x <- data[, c(angle, x)]
        }
        angle <- x[1]
        x <- x[-1]
    }
    else {
        if (is.null(x) && is.null(code)) {
            angle <- data.frame(angle)
            x <- angle
            angle <- angle[, 1]
        }
        else {
            x <- data.frame(x)
            x <- cbind(angle, x)
            angle <- x[1]
            x <- x[-1]
        }
    }
    if (hours) {
        angle <- angle * 2 * pi/period
        x <- cbind(angle, x)
    }
    nvar <- dim(x)[2]
    if (is.null(code)) {
        fit <- cosinor1(angle, x[-1], period = period, opti = opti, 
            na.rm = na.rm)
        m.resp <- mean(x[, 1])
        s.resp <- sd(x[, 1])
        if (plot) {
        }
    }
    else {
        fit.list <- by(x, x[code], function(x) cosinor1(angle = x[1], 
            x = x[-c(1, which(colnames(x) == code))], period = period, 
            opti = opti, na.rm = na.rm))
        ncases <- length(fit.list)
        fit <- matrix(unlist(fit.list), nrow = ncases, byrow = TRUE)
        colnames(fit) <- c(paste(colnames(x)[-c(1, which(colnames(x) == 
            code))], "phase", sep = "."), paste(colnames(x)[-c(1, 
            which(colnames(x) == code))], "fit", sep = "."), 
            paste(colnames(x)[-c(1, which(colnames(x) == code))], 
                "amp", sep = "."), paste(colnames(x)[-c(1, which(colnames(x) == 
                code))], "sd", sep = "."), paste(colnames(x)[-c(1, 
                which(colnames(x) == code))], "mean", sep = "."), 
            paste(colnames(x)[-c(1, which(colnames(x) == code))], 
                "intercept", sep = "."))
        rownames(fit) <- names(fit.list)
    }
    x <- NA
    return(fit)
}


irt.2p <- function (delta, beta, items) 
{
    irt.2par <- function(x, delta, beta, scores) {
        fit <- -1 * (log(scores/(1 + exp(beta * (delta - x))) + 
            (1 - scores)/(1 + exp(beta * (x - delta)))))
        mean(fit, na.rm = TRUE)
    }
    num <- dim(items)[1]
    fit <- matrix(NaN, num, 2)
    total <- rowMeans(items, na.rm = TRUE)
    count <- rowSums(!is.na(items))
    for (i in 1:num) {
        if (count[i] > 0) {
            myfit <- optimize(irt.2par, c(-4, 4), beta = beta, 
                delta = delta, scores = items[i, ])
            fit[i, 1] <- myfit$minimum
            fit[i, 2] <- myfit$objective
        }
        else {
            fit[i, 1] <- NA
            fit[i, 2] <- NA
        }
    }
    irt.2p <- data.frame(total, theta = fit[, 1], fit = fit[, 
        2], count)
}


tetrachoric <- function (x, y = NULL, correct = 0.5, smooth = TRUE, global = TRUE, 
    weight = NULL, na.rm = TRUE, delete = TRUE) 
{
    cl <- match.call()
    if (!is.matrix(x) && !is.data.frame(x)) {
        if (length(x) == 4) {
            x <- matrix(x, 2, 2)
        }
        else {
            if (length(x) == 3) {
                x <- pqr(x)
            }
            else {
                stop("Data must be either a 1 x 4 vector, a 2 x 2 matrix, a comorbidity table, or a data.frame/matrix of data")
            }
        }
    }
    nvar <- dim(x)[2]
    n.obs <- dim(x)[1]
    if (!is.null(weight)) {
        if (length(weight) != n.obs) 
            stop("The number of weights must match the number of observations")
    }
    if (n.obs == nvar) {
        result <- tetrac(x, correct = correct, i = 1, j = 1, 
            global = FALSE)
    }
    else {
        item.var <- apply(x, 2, sd, na.rm = na.rm)
        bad <- which((item.var <= 0) | is.na(item.var))
        if ((length(bad) > 0) & delete) {
            for (baddy in 1:length(bad)) {
                warning("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted")
            }
            x <- x[, -bad]
            nvar <- nvar - length(bad)
        }
        result <- tetra.mat(x, y = y, correct = correct, smooth = smooth, 
            global = global, weight = weight)
    }
    result$Call <- cl
    class(result) <- c("psych", "tetra")
    return(result)
}


harmonic.mean <- function (x, na.rm = TRUE) 
{
    if (is.null(nrow(x))) {
        1/mean(1/x, na.rm = na.rm)
    }
    else {
        1/(apply(1/x, 2, mean, na.rm = na.rm))
    }
}


error.bars.by <- function (x, group, by.var = FALSE, x.cat = TRUE, ylab = NULL, 
    xlab = NULL, main = NULL, ylim = NULL, xlim = NULL, eyes = TRUE, 
    alpha = 0.05, sd = FALSE, labels = NULL, v.labels = NULL, 
    pos = NULL, arrow.len = 0.05, add = FALSE, bars = FALSE, 
    within = FALSE, colors = c("black", "blue", "red"), lty, 
    lines = TRUE, legend = 0, pch, density = -10, ...) 
{
    if (!lines) {
        typ <- "p"
    }
    else {
        typ <- "b"
    }
    n.color <- length(colors)
    nvar <- ncol(x)
    if (is.null(nvar)) 
        nvar <- 1
    if (by.var & (nvar > n.color)) {
        colors <- rainbow(nvar)
    }
    if (!missing(density)) {
        col12 <- col2rgb(colors, TRUE)/255
        colors <- rgb(col12[1, ], col12[2, ], col12[3, ], 0.5)
        n.color <- nvar
    }
    if (missing(lty)) 
        lty <- 1:8
    legend.location <- c("bottomright", "bottom", "bottomleft", 
        "left", "topleft", "top", "topright", "right", "center", 
        "none")
    all.stats <- describe(x)
    min.x <- min(all.stats$min, na.rm = TRUE)
    max.x <- max(all.stats$max, na.rm = TRUE)
    max.se <- max(all.stats$se, na.rm = TRUE)
    if (sd) 
        max.se <- max(all.stats$sd, na.rm = TRUE)
    if (is.null(ylim)) {
        if (is.na(max.x) | is.na(max.se) | is.na(min.x) | is.infinite(max.x) | 
            is.infinite(min.x) | is.infinite(max.se)) {
            ylim = c(0, 1)
        }
        else {
            if (sd) {
                ylim <- c(min.x - max.se, max.x + max.se)
            }
            else {
                ylim = c(min.x - 2 * max.se, max.x + 2 * max.se)
            }
        }
    }
    if (is.null(main)) {
        if (sd) {
            main <- paste("Means + Standard Deviations")
        }
        else {
            main <- paste(1 - alpha, "% confidence limits", sep = "")
        }
    }
    if (bars) {
        group.stats <- describeBy(x, group, mat = TRUE)
        n.var <- dim(all.stats)[1]
        n.group <- length(group.stats[[1]])/n.var
        group.means <- matrix(group.stats$mean, ncol = n.group, 
            byrow = TRUE)
        if (missing(pch)) 
            pch <- seq(15, (15 + n.group))
        if (sd) {
            group.se <- matrix(group.stats$sd, ncol = n.group, 
                byrow = TRUE)
        }
        else {
            group.se <- matrix(group.stats$se, ncol = n.group, 
                byrow = TRUE)
        }
        group.n <- matrix(group.stats$n, ncol = n.group, byrow = TRUE)
        if (within) {
            group.smc <- matrix(unlist(by(x, group, smc)), nrow = n.group, 
                byrow = TRUE)
            group.sd <- matrix(group.stats$sd, ncol = n.group, 
                byrow = TRUE)
            if (sd) {
                group.se <- sqrt(group.se^2 * (1 - group.smc))
            }
            else {
                group.se <- sqrt(group.sd^2 * (1 - group.smc)/group.n)
            }
        }
        rownames(group.means) <- rownames(all.stats)
        if (is.null(labels)) {
            colnames(group.means) <- paste("Group", 1:n.group)
        }
        else {
            colnames(group.means) <- labels
        }
        if (is.null(ylab)) 
            ylab <- "Dependent Variable"
        if (missing(ylim)) 
            ylim = c(0, max.x + 2 * max.se)
        if (by.var) {
            if (is.null(xlab)) 
                xlab <- "Variables"
            mp <- barplot(t(group.means), beside = TRUE, ylab = ylab, 
                xlab = xlab, ylim = ylim, main = main, col = colors, 
                ...)
            for (i in 1:n.var) {
                for (j in 1:n.group) {
                  xcen <- group.means[i, j]
                  xse <- group.se[i, j]
                  if (sd) {
                    ci <- 1
                  }
                  else {
                    ci <- qt(1 - alpha/2, group.n[i, j])
                  }
                  if (is.finite(xse) && xse > 0) 
                    arrows(mp[j, i], xcen - ci * xse, mp[j, i], 
                      xcen + ci * xse, length = arrow.len, angle = 90, 
                      code = 3, col = par("fg"), lty = NULL, 
                      lwd = par("lwd"), xpd = NULL)
                }
            }
        }
        else {
            if (is.null(xlab)) 
                xlab <- "Grouping Variable"
            mp <- barplot(group.means, beside = TRUE, ylab = ylab, 
                xlab = xlab, ylim = ylim, main = main, col = colors, 
                ...)
            for (i in 1:n.var) {
                for (j in 1:n.group) {
                  xcen <- group.means[i, j]
                  xse <- group.se[i, j]
                  if (sd) {
                    ci <- 1
                  }
                  else {
                    ci <- qt(1 - alpha/2, group.n[i, j])
                  }
                  if (is.finite(xse) && xse > 0) 
                    arrows(mp[i, j], xcen - ci * xse, mp[i, j], 
                      xcen + ci * xse, length = arrow.len, angle = 90, 
                      code = 3, col = par("fg"), lty = NULL, 
                      lwd = par("lwd"), xpd = NULL)
                }
            }
        }
        axis(2, ...)
        box()
        if (legend > 0) {
            if (!is.null(v.labels)) {
                lab <- v.labels
            }
            else {
                lab <- paste("V", 1:n.var, sep = "")
            }
            legend(legend.location[legend], lab, col = colors[(1:n.color)], 
                pch = pch[1:n.var], text.col = "green4", lty = lty[1:n.var], 
                merge = TRUE, bg = "gray90")
        }
    }
    else {
        group.stats <- describeBy(x, group)
        n.group <- length(group.stats)
        n.var <- ncol(x)
        if (is.null(n.var)) 
            n.var <- 1
        if (missing(pch)) 
            pch <- seq(15, (15 + n.group))
        if (missing(lty)) 
            lty <- 1:8
        if (within) {
            group.smc <- by(x, group, smc)
        }
        z <- dim(x)[2]
        if (is.null(z)) 
            z <- 1
        if (is.null(ylab)) 
            ylab <- "Dependent Variable"
        if (!by.var) {
            if (is.null(xlab)) 
                xlab <- "Independent Variable"
            for (g in 1:n.group) {
                x.stats <- group.stats[[g]]
                if (within) {
                  x.smc <- group.smc[[g]]
                  if (sd) {
                    x.stats.$se <- sqrt(x.stats$sd^2 * (1 - x.smc))
                  }
                  else {
                    x.stats$se <- sqrt((x.stats$sd^2 * (1 - x.smc))/x.stats$n)
                  }
                }
                if (missing(xlim)) 
                  xlim <- c(0.5, n.var + 0.5)
                if (!add) {
                  plot(x.stats$mean, ylim = ylim, xlim = xlim, 
                    xlab = xlab, ylab = ylab, main = main, typ = typ, 
                    lty = (lty[((g - 1)%%8 + 1)]), axes = FALSE, 
                    col = colors[(g - 1)%%n.color + 1], pch = pch[g], 
                    ...)
                  axis(1, 1:z, colnames(x), ...)
                  axis(2, ...)
                  box()
                }
                else {
                  points(x.stats$mean, typ = typ, lty = lty[((g - 
                    1)%%8 + 1)], col = colors[(g - 1)%%n.color + 
                    1], pch = pch[g])
                }
                if (!is.null(labels)) {
                  lab <- labels
                }
                else {
                  lab <- paste("V", 1:z, sep = "")
                }
                if (length(pos) == 0) {
                  locate <- rep(1, z)
                }
                else {
                  locate <- pos
                }
                if (length(labels) == 0) 
                  lab <- rep("", z)
                else lab <- labels
                for (i in 1:z) {
                  xcen <- x.stats$mean[i]
                  if (sd) {
                    xse <- x.stats$sd[i]
                  }
                  else {
                    xse <- x.stats$se[i]
                  }
                  if (sd) {
                    ci <- 1
                  }
                  else {
                    if (x.stats$n[i] > 1) {
                      ci <- qt(1 - alpha/2, x.stats$n[i] - 1)
                    }
                    else {
                      ci <- 0
                    }
                  }
                  if (is.finite(xse) & xse > 0) {
                    arrows(i, xcen - ci * xse, i, xcen + ci * 
                      xse, length = arrow.len, angle = 90, code = 3, 
                      col = colors[(g - 1)%%n.color + 1], lty = NULL, 
                      lwd = par("lwd"), xpd = NULL)
                    if (eyes) {
                      catseyes(i, xcen, xse, x.stats$n[i], alpha = alpha, 
                        density = density, col = colors[(g - 
                          1)%%n.color + 1])
                    }
                  }
                }
                add <- TRUE
            }
            if (legend > 0) {
                if (!is.null(labels)) {
                  lab <- labels
                }
                else {
                  lab <- paste("G", 1:n.group, sep = "")
                }
                legend(legend.location[legend], lab, col = colors[(1:n.color)], 
                  pch = pch[1:n.group], text.col = "green4", 
                  lty = lty[1:8], merge = TRUE, bg = "gray90")
            }
        }
        else {
            if (is.null(xlab)) 
                xlab <- "Grouping Variable"
            n.vars <- dim(x)[2]
            if (is.null(n.vars)) 
                n.vars <- 1
            var.means <- matrix(NaN, nrow = n.vars, ncol = n.group)
            var.se <- matrix(NA, nrow = n.vars, ncol = n.group)
            for (g in 1:n.group) {
                var.means[, g] <- group.stats[[g]]$mean
                if (sd) {
                  var.se[, g] <- group.stats[[g]]$sd
                }
                else {
                  var.se[, g] <- group.stats[[g]]$se
                }
            }
            if (x.cat) {
                x.values <- 1:n.group
            }
            else {
                x.values <- as.numeric(names(group.stats))
            }
            for (i in 1:n.vars) {
                if (!add) {
                  if (missing(xlim)) 
                    xlim <- c(0.5, n.group + 0.5)
                  plot(x.values, var.means[1, ], ylim = ylim, 
                    xlim = xlim, xlab = xlab, ylab = ylab, main = main, 
                    typ = typ, axes = FALSE, lty = lty[1], pch = pch[1], 
                    col = colors[(i - 1)%%n.color + 1], ...)
                  if (x.cat) {
                    axis(1, 1:n.group, unlist(dimnames(group.stats)), 
                      ...)
                  }
                  else {
                    axis(1)
                  }
                  axis(2, ...)
                  box()
                  add <- TRUE
                }
                else {
                  points(x.values, var.means[i, ], typ = typ, 
                    lty = lty[((i - 1)%%8 + 1)], col = colors[(i - 
                      1)%%n.color + 1], pch = pch[i], ...)
                }
                if (!is.null(labels)) {
                  lab <- labels
                }
                else {
                  lab <- paste("G", 1:z, sep = "")
                }
                if (length(pos) == 0) {
                  locate <- rep(1, z)
                }
                else {
                  locate <- pos
                }
                if (length(labels) == 0) 
                  lab <- rep("", z)
                else lab <- labels
                for (g in 1:n.group) {
                  xcen <- var.means[i, g]
                  xse <- var.se[i, g]
                  if (sd) {
                    ci <- rep(1, n.group)
                  }
                  else {
                    ci <- qt(1 - alpha/2, group.stats[[g]]$n - 
                      1)
                  }
                  if (x.cat) {
                    arrows(g, xcen - ci[i] * xse, g, xcen + ci[i] * 
                      xse, length = arrow.len, angle = 90, code = 3, 
                      col = colors[(i - 1)%%n.color + 1], lty = NULL, 
                      lwd = par("lwd"), xpd = NULL)
                    if (eyes) {
                      catseyes(g, xcen, xse, group.stats[[g]]$n[i], 
                        alpha = alpha, density = density, col = colors[(i - 
                          1)%%n.color + 1])
                    }
                  }
                  else {
                    arrows(x.values[g], xcen - ci[i] * xse, x.values[g], 
                      xcen + ci[i] * xse, length = arrow.len, 
                      angle = 90, code = 3, col = colors[(i - 
                        1)%%n.color + 1], lty = NULL, lwd = par("lwd"), 
                      xpd = NULL)
                    if (eyes) {
                      catseyes(x.values[g], xcen, xse, x.stats$n[i], 
                        alpha = alpha, density = density, col = colors[(i - 
                          1)%%n.color + 1])
                    }
                  }
                }
            }
            if (legend > 0) {
                if (!is.null(labels)) {
                  lab <- labels
                }
                else {
                  lab <- paste("V", 1:z, sep = "")
                }
                legend(legend.location[legend], lab, col = colors[(1:n.color)], 
                  pch = pch[1:n.vars], text.col = "green4", lty = lty[1:8], 
                  merge = TRUE, bg = "gray90")
            }
        }
    }
}


equamax <- function (L, Tmat = diag(ncol(L)), eps = 1e-05, maxit = 1000) 
{
    kappa = ncol(L)/(2 * nrow(L))
    if (requireNamespace("GPArotation")) {
        GPArotation::cfT(L, Tmat = diag(ncol(L)), eps = eps, 
            maxit = maxit)
    }
    else {
        stop("biquartimin requires GPArotation")
    }
}


r2d <- function (rho) 
{
    2 * rho/sqrt(1 - rho^2)
}


scoreIrt <- function (stats = NULL, items, keys = NULL, cut = 0.3, bounds = c(-4, 
    4), mod = "logistic") 
{
    if (!is.null(keys) && is.list(keys)) {
        select <- sub("-", "", unlist(keys))
        items <- items[select]
        keys <- make.keys(items, keys)
    }
    if (!is.null(keys) && (is.vector(keys))) 
        keys <- matrix(keys)
    if (length(class(stats)) > 1) {
        if (!is.null(keys) && is.vector(keys)) 
            keys <- as.matrix(keys)
        switch(class(stats)[2], irt.poly = {
            scores <- score.irt.poly(stats$irt, items, keys, 
                cut, bounds = bounds, mod = mod)
        }, irt.fa = {
            scores <- score.irt.2(stats$irt, items, keys, cut, 
                bounds = bounds, mod = mod)
        }, fa = {
            tau <- irt.tau(items)
            nf <- dim(stats$loadings)[2]
            diffi <- list()
            for (i in 1:nf) {
                diffi[[i]] <- tau/sqrt(1 - stats$loadings[, i]^2)
            }
            discrim <- stats$loadings/sqrt(1 - stats$loadings^2)
            class(diffi) <- NULL
            class(discrim) <- NULL
            new.stats <- list(difficulty = diffi, discrimination = discrim)
            scores <- score.irt.poly(new.stats, items, keys, 
                cut, bounds = bounds)
        }, tau = {
            tau <- stats
            if (is.matrix(keys)) {
                nf <- dim(keys)[2]
            } else {
                nf <- 1
            }
            diffi <- list()
            for (i in 1:nf) {
                diffi[[i]] <- tau
            }
            discrim <- keys
            class(diffi) <- NULL
            class(discrim) <- NULL
            new.stats <- list(difficulty = diffi, discrimination = discrim)
            if (dim(tau)[2] == 1) {
                scores <- score.irt.2(stats = new.stats, items = items, 
                  keys = keys, cut = cut, bounds = bounds)
            } else {
                scores <- score.irt.poly(stats = new.stats, items = items, 
                  keys = keys, cut = cut, bounds = bounds)
            }
        })
    }
    else {
        tau <- irt.tau(items)
        if (is.matrix(keys)) {
            nf <- dim(keys)[2]
        }
        else {
            nf <- 1
        }
        diffi <- list()
        for (i in 1:nf) {
            diffi[[i]] <- tau
        }
        if (!is.null(keys)) {
            discrim <- keys
        }
        else {
            stop("I am sorry, you specified  tau  but not  keys.")
        }
        class(diffi) <- NULL
        class(discrim) <- NULL
        new.stats <- list(difficulty = diffi, discrimination = discrim)
        if (dim(tau)[2] == 1) {
            scores <- score.irt.2(stats = new.stats, items = items, 
                keys = keys, cut = cut, bounds = bounds)
        }
        else {
            scores <- score.irt.poly(stats = new.stats, items = items, 
                keys = keys, cut = cut, bounds = bounds)
        }
    }
    scores <- data.frame(scores)
    if (!is.null(keys)) {
        colnames(scores) <- c(paste(colnames(keys), "theta", 
            sep = "-"), paste(colnames(keys), "total", sep = "-"), 
            paste(colnames(keys), "fit", sep = "-"))
    }
    return(scores)
}


cluster2keys <- function (c) 
{
    if (class(c)[1] == "kmeans") {
        c <- c$cluster
        p <- max(c)
        v <- length(c)
        keys <- matrix(rep(0, p * v), ncol = p)
        for (i in 1:v) {
            keys[i, c[i]] <- 1
        }
    }
    else {
        if (length(class(c)) > 1) {
            if (class(c)[2] == "iclust") {
                keys <- factor2cluster(c)
            }
        }
    }
    return(keys)
}


response.frequencies <- function (items, max = 10, uniqueitems = NULL) 
{
    min.item <- min(items, na.rm = TRUE)
    max.item <- max(items, na.rm = TRUE)
    if (is.null(uniqueitems)) 
        uniqueitems <- unique(as.vector(unlist(items)))
    if ((max.item - min.item > max) || (nlevels(factor(items[, 
        1])) > max) || length(uniqueitems) > max) {
        frequency <- NULL
    }
    else {
        n.var <- dim(items)[2]
        n.cases <- dim(items)[1]
        dummy <- matrix(rep(uniqueitems, n.var), ncol = n.var)
        colnames(dummy) <- names(items)
        xdum <- rbind(items, dummy)
        frequency <- apply(xdum, 2, table)
        frequency <- t(frequency - 1)
        responses <- rowSums(frequency)
        frequency <- frequency/responses
        miss <- 1 - responses/n.cases
        frequency <- cbind(frequency, miss)
    }
    return(frequency)
}


het.diagram <- function (r, levels, cut = 0.3, digits = 2, both = TRUE, main = "Heterarchy diagram", 
    l.cex, gap.size, ...) 
{
    col = c("black", "red")
    nlevels <- length(levels)
    if (missing(gap.size)) 
        gap.size <- 0.4/nlevels
    if (missing(l.cex)) 
        l.cex <- 1
    nvar <- max(unlist(lapply(levels, length)))
    lowest <- rownames(r)[levels[[1]]]
    xlim <- c(-0.25, (nlevels - 0.75))
    ylim <- c(0.25, nvar)
    plot(0, type = "n", frame.plot = FALSE, axes = FALSE, xlim = xlim, 
        ylim = ylim, ylab = "", xlab = "", main = main, ...)
    x <- 0
    if (!is.null(names(levels))) {
        text(x, nvar, names(levels)[x + 1])
    }
    nlevel.i <- length(levels[[1]])
    lower <- list(nlevel.i)
    spacing <- (nvar)/(nlevel.i + 1)
    for (y in 1:nlevel.i) {
        lower[[y]] <- dia.rect(x, y * spacing, lowest[y], xlim = xlim, 
            ylim = ylim, ...)
    }
    names.i <- lowest
    for (i in 2:(nlevels)) {
        nlevel.i <- length(levels[[i]])
        level.i <- list(nlevel.i)
        names.next <- rownames(r)[levels[[i]]]
        x <- i - 1
        if (!is.null(names(levels))) {
            text(x, nvar, names(levels)[i])
        }
        spacing <- (nvar)/(nlevel.i + 1)
        for (y in 1:nlevel.i) {
            level.i[[y]] <- dia.rect(x, y * spacing, rownames(r)[levels[[i]][y]], 
                xlim = xlim, ylim = ylim, ...)
            for (j in 1:length(levels[[i - 1]])) {
                if (abs(r[names.i[j], names.next[y]]) > cut) {
                  dia.arrow(from = level.i[[y]]$left, to = lower[[j]]$right, 
                    labels = round(r[names.i[j], names.next[y]], 
                      digits), both = both, adj = y%%3 + 1, col = (sign(r[names.i[j], 
                      names.next[y]] < 0) + 1), lty = (sign(r[names.i[j], 
                      names.next[y]] < 0) + 1), l.cex = l.cex, 
                    gap.size = gap.size, ...)
                }
            }
        }
        names.i <- names.next
        lower <- level.i
    }
}


KMO <- function (r) 
{
    cl <- match.call()
    if (!isCorrelation(r)) 
        r <- cor(r, use = "pairwise")
    Q <- try(solve(r))
    if (class(Q) == as.character("try-error")) {
        message("matrix is not invertible, image not found")
        Q <- r
    }
    S2 <- diag(1/diag(Q))
    IC <- S2 %*% Q %*% S2
    Q <- Image <- cov2cor(Q)
    diag(Q) <- 0
    diag(r) <- 0
    sumQ2 <- sum(Q^2)
    sumr2 <- sum(r^2)
    MSA <- sumr2/(sumr2 + sumQ2)
    MSAi <- colSums(r^2)/(colSums(r^2) + colSums(Q^2))
    results <- list(MSA = MSA, MSAi = MSAi, Image = Image, ImCov = IC, 
        Call = cl)
    class(results) <- c("psych", "KMO")
    return(results)
}


ICC <- function (x, missing = TRUE, alpha = 0.05) 
{
    cl <- match.call()
    if (is.matrix(x)) 
        x <- data.frame(x)
    n.obs.original <- dim(x)[1]
    if (missing) {
        x1 <- try(na.fail(x))
        if (class(x1) == as.character("try-error")) {
            x <- na.omit(x)
            n.obs <- dim(x)[1]
            stop("missing data were found for ", n.obs.original - 
                n.obs, " cases \n Try again with na.omit  or set missing= FALSE and proceed at your own risk.")
        }
    }
    n.obs <- dim(x)[1]
    if (n.obs < n.obs.original) 
        message("Warning, missing data were found for ", n.obs.original - 
            n.obs, " cases")
    nj <- dim(x)[2]
    x.s <- stack(x)
    x.df <- data.frame(x.s, subs = rep(paste("S", 1:n.obs, sep = ""), 
        nj))
    aov.x <- aov(values ~ subs + ind, data = x.df)
    s.aov <- summary(aov.x)
    stats <- matrix(unlist(s.aov), ncol = 3, byrow = TRUE)
    MSB <- stats[3, 1]
    MSW <- (stats[2, 2] + stats[2, 3])/(stats[1, 2] + stats[1, 
        3])
    MSJ <- stats[3, 2]
    MSE <- stats[3, 3]
    ICC1 <- (MSB - MSW)/(MSB + (nj - 1) * MSW)
    ICC2 <- (MSB - MSE)/(MSB + (nj - 1) * MSE + nj * (MSJ - MSE)/n.obs)
    ICC3 <- (MSB - MSE)/(MSB + (nj - 1) * MSE)
    ICC12 <- (MSB - MSW)/(MSB)
    ICC22 <- (MSB - MSE)/(MSB + (MSJ - MSE)/n.obs)
    ICC32 <- (MSB - MSE)/MSB
    F11 <- MSB/MSW
    df11n <- n.obs - 1
    df11d <- n.obs * (nj - 1)
    p11 <- 1 - pf(F11, df11n, df11d)
    F21 <- MSB/MSE
    df21n <- n.obs - 1
    df21d <- (n.obs - 1) * (nj - 1)
    p21 <- 1 - pf(F21, df21n, df21d)
    F31 <- F21
    results <- data.frame(matrix(NA, ncol = 8, nrow = 6))
    colnames(results) <- c("type", "ICC", "F", "df1", "df2", 
        "p", "lower bound", "upper bound")
    rownames(results) <- c("Single_raters_absolute", "Single_random_raters", 
        "Single_fixed_raters", "Average_raters_absolute", "Average_random_raters", 
        "Average_fixed_raters")
    results[1, 1] = "ICC1"
    results[2, 1] = "ICC2"
    results[3, 1] = "ICC3"
    results[4, 1] = "ICC1k"
    results[5, 1] = "ICC2k"
    results[6, 1] = "ICC3k"
    results[1, 2] = ICC1
    results[2, 2] = ICC2
    results[3, 2] = ICC3
    results[4, 2] = ICC12
    results[5, 2] = ICC22
    results[6, 2] = ICC32
    results[1, 3] <- results[4, 3] <- F11
    results[2, 3] <- F21
    results[3, 3] <- results[6, 3] <- results[5, 3] <- F31 <- F21
    results[5, 3] <- F21
    results[1, 4] <- results[4, 4] <- df11n
    results[1, 5] <- results[4, 5] <- df11d
    results[1, 6] <- results[4, 6] <- p11
    results[2, 4] <- results[3, 4] <- results[5, 4] <- results[6, 
        4] <- df21n
    results[2, 5] <- results[3, 5] <- results[5, 5] <- results[6, 
        5] <- df21d
    results[2, 6] <- results[5, 6] <- results[3, 6] <- results[6, 
        6] <- p21
    F1L <- F11/qf(1 - alpha/2, df11n, df11d)
    F1U <- F11 * qf(1 - alpha/2, df11d, df11n)
    L1 <- (F1L - 1)/(F1L + (nj - 1))
    U1 <- (F1U - 1)/(F1U + nj - 1)
    F3L <- F31/qf(1 - alpha/2, df21n, df21d)
    F3U <- F31 * qf(1 - alpha/2, df21d, df21n)
    results[1, 7] <- L1
    results[1, 8] <- U1
    results[3, 7] <- (F3L - 1)/(F3L + nj - 1)
    results[3, 8] <- (F3U - 1)/(F3U + nj - 1)
    results[4, 7] <- 1 - 1/F1L
    results[4, 8] <- 1 - 1/F1U
    results[6, 7] <- 1 - 1/F3L
    results[6, 8] <- 1 - 1/F3U
    Fj <- MSJ/MSE
    vn <- (nj - 1) * (n.obs - 1) * ((nj * ICC2 * Fj + n.obs * 
        (1 + (nj - 1) * ICC2) - nj * ICC2))^2
    vd <- (n.obs - 1) * nj^2 * ICC2^2 * Fj^2 + (n.obs * (1 + 
        (nj - 1) * ICC2) - nj * ICC2)^2
    v <- vn/vd
    F3U <- qf(1 - alpha/2, n.obs - 1, v)
    F3L <- qf(1 - alpha/2, v, n.obs - 1)
    L3 <- n.obs * (MSB - F3U * MSE)/(F3U * (nj * MSJ + (nj * 
        n.obs - nj - n.obs) * MSE) + n.obs * MSB)
    results[2, 7] <- L3
    U3 <- n.obs * (F3L * MSB - MSE)/(nj * MSJ + (nj * n.obs - 
        nj - n.obs) * MSE + n.obs * F3L * MSB)
    results[2, 8] <- U3
    L3k <- L3 * nj/(1 + L3 * (nj - 1))
    U3k <- U3 * nj/(1 + U3 * (nj - 1))
    results[5, 7] <- L3k
    results[5, 8] <- U3k
    results[, 2:8] <- results[, 2:8]
    result <- list(results = results, summary = s.aov, stats = stats, 
        MSW = MSW, Call = cl, n.obs = n.obs, n.judge = nj)
    class(result) <- c("psych", "ICC")
    return(result)
}


cluster.loadings <- function (keys, r.mat, correct = TRUE, SMC = TRUE) 
{
    cl <- match.call()
    if (!is.matrix(keys)) {
        keys <- as.matrix(keys)
    }
    r.mat[is.na(r.mat)] <- -9999999
    item.sd <- sqrt(diag(r.mat))
    item.covar <- r.mat %*% keys
    covar <- t(keys) %*% item.covar
    var <- diag(covar)
    sd.inv <- 1/sqrt(var)
    if (SMC) {
        r.smc <- smc(r.mat)
        r.smc[r.smc < 0] <- 1
        diag(r.mat) <- r.smc
    }
    else {
        diag(r.mat) <- 0
        item.max <- apply(r.mat, 1, max)
        diag(r.mat) <- item.max
    }
    c.item.covar <- r.mat %*% keys
    c.covar <- t(keys) %*% c.item.covar
    c.var <- diag(c.covar)
    G6 <- c.var/var
    n.keys <- dim(keys)[2]
    if (n.keys > 1) {
        c.item.cor <- c.item.covar %*% sqrt(diag(1/c.var))/item.sd
    }
    else {
        c.item.cor <- c.item.covar/sqrt(c.var * item.sd)
    }
    key.count <- diag(t(keys) %*% keys)
    if (correct) {
        cluster.correct <- diag((key.count/(key.count - 1)))
        for (i in 1:dim(keys)[2]) {
            if (key.count[i] < 2) {
                cluster.correct[i, i] <- 1
            }
            else {
                cluster.correct[i, i] <- key.count[i]/(key.count[i] - 
                  1)
                item.covar[, i] <- item.covar[, i] - keys[, i]
            }
        }
        correction.factor <- keys %*% cluster.correct
        correction.factor[correction.factor < 1] <- 1
        item.covar <- item.covar * correction.factor
    }
    ident.sd <- diag(sd.inv, ncol = length(sd.inv))
    c.correl <- ident.sd %*% covar %*% ident.sd
    p.loading <- try(c.item.cor %*% solve(c.correl))
    if (class(p.loading) == "try-error") {
        message("the correlation matrix was singular, pattern loadings not found, proceed with caution")
        p.loading <- c.item.cor
    }
    c.item.cor[abs(c.item.cor) > 99999] <- NA
    c.correl[abs(c.correl) > 99999] <- NA
    key.alpha <- ((var - key.count)/var) * (key.count/(key.count - 
        1))
    key.alpha[is.nan(key.alpha)] <- 1
    key.alpha[!is.finite(key.alpha)] <- 1
    key.av.r <- key.alpha/(key.count - key.alpha * (key.count - 
        1))
    colnames(c.item.cor) <- colnames(keys)
    colnames(p.loading) <- colnames(keys)
    colnames(c.correl) <- colnames(keys)
    rownames(c.correl) <- colnames(keys)
    rownames(c.item.cor) <- rownames(r.mat)
    if (ncol(keys) > 1) {
        cluster.corrected <- correct.cor(c.correl, t(key.alpha))
    }
    else {
        cluster.corrected <- c.correl
    }
    results <- list(loadings = c.item.cor, pattern = p.loading, 
        cor = c.correl, corrected = cluster.corrected, sd = sqrt(var), 
        alpha = key.alpha, av.r = key.av.r, size = key.count, 
        G6 = G6, Call = cl)
    class(results) <- c("psych", "cluster.loadings")
    return(results)
}


psych.misc <- function () 
{
}


ICC2latex <- function (icc, digits = 2, rowlabels = TRUE, apa = TRUE, ci = TRUE, 
    font.size = "scriptsize", big.mark = NULL, drop.na = TRUE, 
    heading = "A table from the psych package in R", caption = "ICC2latex", 
    label = "default", char = FALSE, silent = FALSE, file = NULL, 
    append = FALSE) 
{
    if ((length(class(icc)) < 2) | (class(icc)[2] != "ICC")) 
        icc <- ICC(icc)
    x <- icc$results
    nvar <- dim(x)[2]
    rname <- rownames(x)
    comment <- paste("%", match.call())
    header <- paste("\\begin{", font.size, "} \\begin{table}[[htpb]", 
        "\\caption{", caption, "}\n\\begin{tabular}", sep = "")
    if (rowlabels) {
        header <- c(header, "{l", rep("r", (nvar)), "}\n")
    }
    else {
        header <- c(header, "{", rep("r", (nvar + 1)), "}\n")
    }
    if (apa) {
        header <- c(header, "\\multicolumn{", 5, "}{l}{", heading, 
            "}", "\\cr \n \\hline ")
        footer <- paste(" \\hline \\cr \\multicolumn{ 5 }{c}{   Number of subjects = ", 
            icc$n.obs, "Number of raters = ", icc$n.judge, "}")
    }
    else {
        footer <- NULL
    }
    footer <- paste(footer, "\n\\end{tabular}\n\\label{", label, 
        "}\n\\end{table} \n\\end{", font.size, "}\n", sep = "")
    x[2:nvar] <- try(round(x[2:nvar], digits = digits))
    cname <- colnames(x)
    if (!ci) 
        nvar <- nvar - 2
    names1 <- paste(cname[1:(nvar - 1)], " & ")
    lastname <- paste(cname[nvar], "\\cr \n")
    if (apa) {
        allnames <- c("Variable  &  ", names1, lastname, " \\hline \n")
    }
    else {
        if (rowlabels) {
            allnames <- c("  &  ", names1, lastname, "\\cr \n")
        }
        else {
            allnames <- c(names1, lastname, "\\cr \n")
        }
    }
    if (!char) {
        if (is.null(big.mark)) {
            x <- format(x[1:nvar], drop0trailing = FALSE)
        }
        else {
            x <- prettyNum(x, big.mark = ",", drop0trailing = FALSE)
        }
    }
    value <- apply(x, 1, paste, collapse = "  &  ")
    if (rowlabels) {
        value <- paste(sanitize.latex(rname), "  & ", value)
    }
    else {
        value <- paste("  & ", value)
    }
    values <- paste(value, "\\cr", "\n")
    if (drop.na) 
        values <- gsub("NA", "  ", values, fixed = TRUE)
    if (!silent) {
        cat(comment, "\n")
        cat(header)
        cat(allnames)
        cat(values)
        cat(footer)
    }
    result <- c(header, allnames, values, footer)
    if (!is.null(file)) 
        write.table(result, file = file, row.names = FALSE, col.names = FALSE, 
            quote = FALSE, append = append)
    invisible(result)
}


glb.fa <- function (r, key = NULL) 
{
    cl <- match.call()
    nvar <- dim(r)[2]
    if (dim(r)[1] != dim(r)[2]) {
        r <- cor(r, use = "pairwise")
    }
    else {
        if (!is.matrix(r)) 
            r <- as.matrix(r)
        r <- cov2cor(r)
    }
    if (is.null(colnames(r))) {
        rownames(r) <- colnames(r) <- paste("V", 1:nvar, sep = "")
    }
    if (!is.null(key)) {
        key <- as.vector(key)
        r <- diag(key) %*% r %*% diag(key)
        flip <- FALSE
    }
    else {
        key <- rep(1, nvar)
    }
    nv <- dim(r)[1]
    f1 <- fa(r)
    nf <- length(which(f1$values > 0))
    df <- nv * (nv - 1)/2 - nf * nv + nf * (nf - 1)/2
    if (df < 0) 
        nf <- nf - 1
    fn <- fa(r, nf, rotate = "none")
    rr <- r
    diag(rr) <- fn$communality
    glb <- sum(rr)/sum(r)
    return(list(glb = glb, communality = fn$communality, numf = nf, 
        Call = cl))
}


read.clipboard.csv <- function (header = TRUE, sep = ",", ...) 
{
    MAC <- Sys.info()[1] == "Darwin"
    if (!MAC) {
        if (header) 
            read.clipboard <- read.table(file("clipboard"), header = TRUE, 
                sep, ...)
        else read.clipboard <- read.table(file("clipboard"), 
            sep = sep, ...)
    }
    else {
        if (header) 
            read.clipboard <- read.table(pipe("pbpaste"), header = TRUE, 
                sep, ...)
        else read.clipboard <- read.table(pipe("pbpaste"), sep = sep, 
            ...)
    }
}


fa2irt <- function (f, rho, plot = TRUE, n.obs = NULL) 
{
    cl <- match.call()
    tau <- rho$tau
    if (!is.null(f$order)) {
        if (!is.null(ncol(tau))) {
            tau <- tau[f$order, ]
        }
        else {
            tau <- tau[f$order]
        }
    }
    r <- rho$rho
    nf <- ncol(f$loadings)
    diffi <- list()
    for (i in 1:nf) {
        diffi[[i]] <- tau/sqrt(1 - f$loadings[, i]^2)
    }
    discrim <- f$loadings/sqrt(1 - f$loadings^2)
    if (any(is.nan(discrim))) {
        bad <- which(is.nan(discrim), arr.ind = TRUE)
        if (length(bad) > 0) {
            warning("An discrimination  with a NaN value was replaced with the maximum discrimination for item(s)  ", 
                bad, "\nexamine the factor analysis object (fa)  to identify the Heywood case")
        }
        for (i in 1:nf) {
            discrimin[is.nan(discrim[, i])] <- max(discrimin[, 
                i], na.rm = TRUE)
        }
    }
    irt <- list(difficulty = diffi, discrimination = discrim)
    result <- list(irt = irt, fa = f, rho = r, tau = tau, n.obs = n.obs, 
        Call = cl)
    if (class(rho)[2] == "poly") {
        class(result) <- c("psych", "irt.poly")
    }
    else {
        class(result) <- c("psych", "irt.fa")
    }
    if (plot) {
        pr <- plot(result)
        result$plot <- pr
    }
    return(result)
}


score.alpha <- function (keys, items, labels = NULL, totals = TRUE, digits = 2) 
{
    .Deprecated("score.alpha", msg = "score.alpha is deprecated.  Please use the scoreItems function")
    keys <- as.matrix(keys)
    items <- as.matrix(items)
    scores <- items %*% keys
    if (length(labels) > 0) {
        colnames(scores) <- labels
    }
    abskeys <- abs(keys)
    item.var <- diag(var(items, use = "pairwise"))
    cov.scales <- cov(scores, use = "pairwise")
    var.scales <- diag(cov.scales)
    cor.scales <- cor(scores, use = "pairwise")
    sum.item.var <- item.var %*% abskeys
    num.item <- diag(t(abskeys) %*% abskeys)
    alpha.scale <- (var.scales - sum.item.var) * num.item/((num.item - 
        1) * var.scales)
    if (length(labels) > 0) {
        colnames(alpha.scale) <- labels
    }
    av.r <- alpha.scale/(num.item - alpha.scale * (num.item - 
        1))
    item.cor <- cor(items, scores, use = "pairwise")
    if (!totals) 
        scores <- scores/num.item
    results <- list(scores = scores, alpha = round(alpha.scale, 
        digits), av.r = round(av.r, digits), n.items = num.item, 
        cor = round(cor.scales, digits), item.cor = round(item.cor, 
            digits))
    class(results) <- "psych"
    return(results)
}


fa.extend <- function (r, nfactors = 1, ov = NULL, ev = NULL, n.obs = NA, 
    np.obs = NULL, correct = TRUE, rotate = "oblimin", SMC = TRUE, 
    warnings = TRUE, fm = "minres", alpha = 0.1, omega = FALSE, 
    ...) 
{
    cl <- match.call()
    nv <- c(ov, ev)
    if (nrow(r) > ncol(r)) {
        if (omega) {
            fo <- omega(r[, ov], nfactors = nfactors, rotate = rotate, 
                SMC = SMC, warnings = warnings, fm = fm, alpha = alpha, 
                ...)
        }
        else {
            fo <- fa(r[, ov], nfactors = nfactors, rotate = rotate, 
                SMC = SMC, warnings = warnings, fm = fm, alpha = alpha, 
                ...)
        }
        n.obs <- nrow(r)
        np.obs.r <- count.pairwise(r)[nv, nv]
        np.obs <- np.obs.r[ov, ov]
        r <- cor(r, use = "pairwise")
    }
    else {
        R <- r[ov, ov]
        np.obs.r <- np.obs
        if (omega) {
            fo <- omega(R, nfactors = nfactors, n.obs = n.obs, 
                rotate = rotate, SMC = SMC, warnings = warnings, 
                fm = fm, alpha = alpha, np.obs = np.obs[ov, ov], 
                ...)
        }
        else {
            fo <- fa(R, nfactors = nfactors, n.obs = n.obs, rotate = rotate, 
                SMC = SMC, warnings = warnings, fm = fm, alpha = alpha, 
                np.obs = np.obs[ov, ov], ...)
        }
    }
    Roe <- r[ov, ev, drop = FALSE]
    fe <- fa.extension(Roe, fo, correct = correct)
    if (omega) 
        fo$loadings <- fo$schmid$sl[, 1:(ncol(fo$schmid$sl) - 
            3)]
    foe <- rbind(fo$loadings, fe$loadings)
    if (omega) 
        oblique <- rbind(fo$schmid$oblique, fe$oblique)
    if (is.na(n.obs) && !is.null(np.obs)) 
        n.obs <- max(as.vector(np.obs))
    result <- factor.stats(r[nv, nv], foe, fo$Phi, n.obs, np.obs.r, 
        alpha = alpha)
    if (omega) 
        result$schmid$sl <- foe
    result$rotation <- rotate
    result$loadings <- foe
    if (nfactors > 1) {
        if (is.null(fo$Phi)) {
            h2 <- rowSums(foe^2)
        }
        else {
            h2 <- diag(foe %*% fo$Phi %*% t(foe))
        }
    }
    else {
        h2 <- foe^2
    }
    result$communality <- h2
    result$fm <- fm
    result$fo = fo
    if (omega) {
        result$schmid$sl <- foe
        result$schmid$gloading <- fo$schmid$gloading
        result$schmid$oblique <- oblique
    }
    if (is.null(fo$Phi)) {
        result$Structure <- foe
    }
    else {
        result$Structure <- foe %*% fo$Phi
    }
    result$fe = fe
    result$Phi = fo$Phi
    result$fn = "fa"
    result$Call = cl
    class(result) <- c("psych", "extend")
    return(result)
}


scree <- function (rx, factors = TRUE, pc = TRUE, main = "Scree plot", 
    hline = NULL, add = FALSE) 
{
    cl <- match.call()
    nvar <- dim(rx)[2]
    if (nvar != dim(rx)[1]) {
        rx <- cor(rx, use = "pairwise")
    }
    if (pc) {
        values <- eigen(rx)$values
        if (factors) {
            ylab = "Eigen values of factors and components"
            xlab = "factor or component number"
        }
        else {
            ylab = "Eigen values of components"
            xlab = " component number"
        }
    }
    else {
        values <- fa(rx)$values
        ylab = "Eigen values of factors"
        xlab = " factor number"
        factors <- FALSE
    }
    max.value <- max(values)
    if (!add) {
        plot(values, type = "b", main = main, pch = 16, ylim = c(0, 
            max.value), ylab = ylab, xlab = xlab)
    }
    else {
        points(values, type = "b", , pch = 16)
    }
    if (factors) {
        fv <- fa(rx)$values
        points(fv, type = "b", pch = 21, lty = "dotted")
    }
    else {
        fv <- NULL
    }
    if (is.null(hline)) {
        abline(h = 1)
    }
    else {
        abline(h = hline)
    }
    if (factors & pc) {
        legend("topright", c("PC ", "FA"), pch = c(16, 21), text.col = "green4", 
            lty = c("solid", "dotted"), merge = TRUE, bg = "gray90")
    }
    if (pc) {
        results <- list(fv = fv, pcv = values, call = cl)
    }
    else {
        results <- list(fv = values, pcv = NULL, call = cl)
    }
    class(results) <- c("psych", "scree")
    invisible(results)
}




## Package Data

Bechtoldt <- psych::Bechtoldt		## Seven data sets showing a bifactor solution.

Bechtoldt.1 <- psych::Bechtoldt.1		## Seven data sets showing a bifactor solution.

Bechtoldt.2 <- psych::Bechtoldt.2		## Seven data sets showing a bifactor solution.

Dwyer <- psych::Dwyer		## 8 cognitive variables used by Dwyer for an example.

Gleser <- psych::Gleser		## Example data from Gleser, Cronbach and Rajaratnam (1965) to show basic principles of generalizability theory.

Gorsuch <- psych::Gorsuch		## Example data set from Gorsuch (1997) for an example factor extension.

Harman.5 <- psych::Harman.5		## 5 socio-economic variables from Harman (1967)

Harman.8 <- psych::Harman.8		## Correlations of eight physical variables (from Harman, 1966)

Harman.political <- psych::Harman.political		## Eight political variables used by Harman (1967) as example 8.17

Holzinger <- psych::Holzinger		## Seven data sets showing a bifactor solution.

Holzinger.9 <- psych::Holzinger.9		## Seven data sets showing a bifactor solution.

Reise <- psych::Reise		## Seven data sets showing a bifactor solution.

Schmid <- psych::Schmid		## 12 variables created by Schmid and Leiman to show the Schmid-Leiman Transformation

Schutz <- psych::Schutz		## The Schutz correlation matrix example from Shapiro and ten Berge

Thurstone <- psych::Thurstone		## Seven data sets showing a bifactor solution.

Thurstone.33 <- psych::Thurstone.33		## Seven data sets showing a bifactor solution.

Tucker <- psych::Tucker		## 9 Cognitive variables discussed by Tucker and Lewis (1973)

ability <- psych::ability		## 16 ability items scored as correct or incorrect.

affect <- psych::affect		## Two data sets of affect and arousal scores as a function of personality and movie conditions

bfi <- psych::bfi		## 25 Personality items representing 5 factors

bfi.dictionary <- psych::bfi.dictionary		## 25 Personality items representing 5 factors

blot <- psych::blot		## Bond's Logical Operations Test - BLOT

burt <- psych::burt		## 11 emotional variables from Burt (1915)

cattell <- psych::cattell		## 12 cognitive variables from Cattell (1963)

cities <- psych::cities		## Distances between 11 US cities

cubits <- psych::cubits		## Galton's example of the relationship between height and 'cubit' or forearm length

cushny <- psych::cushny		## A data set from Cushny and Peebles (1905) on the effect of three drugs on hours of sleep, used by Student (1908)

epi <- psych::epi		## Eysenck Personality Inventory (EPI) data for 3570 participants

epi.bfi <- psych::epi.bfi		## 13 personality scales from the Eysenck Personality Inventory and Big 5 inventory

epi.dictionary <- psych::epi.dictionary		## Eysenck Personality Inventory (EPI) data for 3570 participants

galton <- psych::galton		## Galton's Mid parent child height data

heights <- psych::heights		## A data.frame of the Galton (1888) height and cubit data set.

income <- psych::income		## US family income from US census 2008

iqitems <- psych::iqitems		## 16 multiple choice IQ items

msq <- psych::msq		## 75 mood items from the Motivational State Questionnaire for 3896 participants

neo <- psych::neo		## NEO correlation matrix from the NEO_PI_R manual

peas <- psych::peas		## Galton's Peas

sat.act <- psych::sat.act		## 3 Measures of ability: SATV, SATQ, ACT

withinBetween <- psych::withinBetween		## An example of the distinction between within group and between group correlations



## Package Info

.skeleton_package_title = "Procedures for Psychological, Psychometric, and PersonalityResearch"

.skeleton_package_version = "1.6.12"

.skeleton_package_depends = ""

.skeleton_package_imports = "mnormt,parallel,stats,graphics,grDevices,methods,foreign,lattice,nlme"


## Internal

.skeleton_version = 5


## EOF