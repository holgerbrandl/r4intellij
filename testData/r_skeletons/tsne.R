##
## Exported symobls in package `tsne`
##

## Exported package methods

tsne <- function (X, initial_config = NULL, k = 2, initial_dims = 30, 
    perplexity = 30, max_iter = 1000, min_cost = 0, epoch_callback = NULL, 
    whiten = TRUE, epoch = 100) 
{
    if ("dist" %in% class(X)) {
        n = attr(X, "Size")
    }
    else {
        X = as.matrix(X)
        X = X - min(X)
        X = X/max(X)
        initial_dims = min(initial_dims, ncol(X))
        if (whiten) 
            X <- .whiten(as.matrix(X), n.comp = initial_dims)
        n = nrow(X)
    }
    momentum = 0.5
    final_momentum = 0.8
    mom_switch_iter = 250
    epsilon = 500
    min_gain = 0.01
    initial_P_gain = 4
    eps = 2^(-52)
    if (!is.null(initial_config) && is.matrix(initial_config)) {
        if (nrow(initial_config) != n | ncol(initial_config) != 
            k) {
            stop("initial_config argument does not match necessary configuration for X")
        }
        ydata = initial_config
        initial_P_gain = 1
    }
    else {
        ydata = matrix(rnorm(k * n), n)
    }
    P = .x2p(X, perplexity, 1e-05)$P
    P = 0.5 * (P + t(P))
    P[P < eps] <- eps
    P = P/sum(P)
    P = P * initial_P_gain
    grads = matrix(0, nrow(ydata), ncol(ydata))
    incs = matrix(0, nrow(ydata), ncol(ydata))
    gains = matrix(1, nrow(ydata), ncol(ydata))
    for (iter in 1:max_iter) {
        if (iter%%epoch == 0) {
            cost = sum(apply(P * log((P + eps)/(Q + eps)), 1, 
                sum))
            message("Epoch: Iteration #", iter, " error is: ", 
                cost)
            if (cost < min_cost) 
                break
            if (!is.null(epoch_callback)) 
                epoch_callback(ydata)
        }
        sum_ydata = apply(ydata^2, 1, sum)
        num = 1/(1 + sum_ydata + sweep(-2 * ydata %*% t(ydata), 
            2, -t(sum_ydata)))
        diag(num) = 0
        Q = num/sum(num)
        if (any(is.nan(num))) 
            message("NaN in grad. descent")
        Q[Q < eps] = eps
        stiffnesses = 4 * (P - Q) * num
        for (i in 1:n) {
            grads[i, ] = apply(sweep(-ydata, 2, -ydata[i, ]) * 
                stiffnesses[, i], 2, sum)
        }
        gains = ((gains + 0.2) * abs(sign(grads) != sign(incs)) + 
            gains * 0.8 * abs(sign(grads) == sign(incs)))
        gains[gains < min_gain] = min_gain
        incs = momentum * incs - epsilon * (gains * grads)
        ydata = ydata + incs
        ydata = sweep(ydata, 2, apply(ydata, 2, mean))
        if (iter == mom_switch_iter) 
            momentum = final_momentum
        if (iter == 100 && is.null(initial_config)) 
            P = P/4
    }
    ydata
}




## Package Data

# none


## Package Info

.skeleton_package_title = "T-Distributed Stochastic Neighbor Embedding for R (t-SNE)"

.skeleton_package_version = "0.1-3"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF