a[,] # unclear usage but valid R code to acces complete dataframe

a[,1]

b[,,,1]

c[1,1]

d[1,1,232]

e[-1,]

f[-1,,]

g[,,,,,-1,,]


# also test function call with tailing comma here
sum(,1)

## todo fix test to ensure that parser fails for this one (which does not make sense)
#sum(1,) # should not work
