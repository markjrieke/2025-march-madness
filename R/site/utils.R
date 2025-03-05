increment_pid <- function(x) {
  
  (x + (x %% 2)) / 2
  
}

route_pid <- function(x) {
  
  if (length(x) == 1) {
    return(x)
  }
  
  for (i in 2:length(x)) {
    x[i] <- increment_pid(x[i-1])
  }
  
  return(x)
  
}
