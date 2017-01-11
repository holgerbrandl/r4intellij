
readinteger <- function()
{
  n <- readline(prompt="Enter an integer: ")
  return(as.integer(n))
}

print(readinteger())