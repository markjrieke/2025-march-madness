#' Find what a team's position in the next round is based on their position in
#' this round
#' 
#' @param x A team's current position (pid). Should be in the range 1:64.
increment_pid <- function(x) {
  
  (x + (x %% 2)) / 2
  
}

#' Route a team's path through the tournament based on their initial position
#' 
#' @param x A vector where `x[1]` is the team's initial position (pid) in the 
#'        bracket and all other values are `0`.
route_pid <- function(x) {
  
  if (length(x) == 1) {
    return(x)
  }
  
  for (i in 2:length(x)) {
    x[i] <- increment_pid(x[i-1])
  }
  
  return(x)
  
}
