towardsFood2 <- function(snake,food,x,y) {
  bodycheck <- function(newhead) {
    if(newhead[1] < 1 || newhead[1] > x || newhead[2] < 1 || newhead[2] > y) return(FALSE)
    return(sum(sapply(1:length(snake), function(x){identical(snake[[x]], newhead)})) == 0)
  }
  rad <- atan2(food[2]- snake[[1]][2], food[1] - snake[[1]][1]) / pi + 0.25
  if(rad > 1) rad <- rad-2
  v <- as.numeric(cut(rad, breaks=c(-1,-0.5,0,0.5,1)))
  nh <- snake[[1]] + list(c(-1,0), c(0,-1), c(1,0), c(0,1))[[v]]
  while(!bodycheck(nh)){
    v <- v + 1
    if(v > 4) v <- 1
    nh <- snake[[1]] + list(c(-1,0), c(0,-1), c(1,0), c(0,1))[[v]]
  }
  return(c("h","j", "l", "k")[v])
}