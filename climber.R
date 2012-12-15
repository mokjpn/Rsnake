climber <- function(snake,food,x,y){
  bodycheck <- function(newhead) {
    if(newhead[1] < 1 || newhead[1] > x || newhead[2] < 1 || newhead[2] > y) return(FALSE)
    return(sum(sapply(1:length(snake), function(x){identical(snake[[x]], newhead)})) == 0)
  }
  v <- round(rnorm(1,3,0.5))
  if(v<1) v<-1; if(v>4) v<-4 
  nh <- snake[[1]] + list(c(-1,0), c(0,-1), c(0,1), c(1,0))[[v]]
  while(!bodycheck(nh)){
    v <- v + 1
    if(v > 4) v <- 1
    nh <- snake[[1]] + list(c(-1,0), c(0,-1), c(0,1), c(1,0))[[v]]
  }
  return(c("h","j","k","l")[v])
}
