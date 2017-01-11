return(switch(kernel,
                      gaussian = 1/(2*sqrt(pi)),
                      rectangular = sqrt(3)/6,
                      triangular = sqrt(6)/9,
                      epanechnikov = 3/(5*sqrt(5)),
                      biweight = 5*sqrt(7)/49,
                      cosine = 3/4*sqrt(1/3 - 2/pi^2),
                      optcosine = sqrt(1-8/pi^2)*pi^2/16
                      ))