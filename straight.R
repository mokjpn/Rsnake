straight <- function(snake,food,x,y) {
  bodycheck <- function(newhead) {
    if(newhead[1] < 1 || newhead[1] > x || newhead[2] < 1 || newhead[2] > y) return(FALSE)
    return(sum(sapply(1:length(snake), function(x){identical(snake[[x]], newhead)})) == 0)
  }
  yd <- food[2]- snake[[1]][2]
  xd <- food[1]- snake[[1]][1]
  if(abs(xd) > 1 || abs(xd) > abs(yd)) {
    if(xd < 0)
      v <- 1
    else
      v <- 3
  } else {
    if(yd < 0)
      v <- 2
    else
      v <- 4
  }
  nh <- snake[[1]] + list(c(-1,0), c(0,-1), c(1,0), c(0,1))[[v]]
  while(!bodycheck(nh)){
    v <- v + 1
    if(v > 4) v <- 1
    nh <- snake[[1]] + list(c(-1,0), c(0,-1), c(1,0), c(0,1))[[v]]
  }
  return(c("h","j", "l", "k")[v])
}