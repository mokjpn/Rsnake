towardsFood <- function(snake,food,x,y) {
  rad <- atan2(food[2]- snake[[1]][2], food[1] - snake[[1]][1]) / pi + 0.25
  if(rad > 1) rad <- rad-2
  v <- as.numeric(cut(rad, breaks=c(-1,-0.5,0,0.5,1)))
  v <- v + round(rnorm(1, 0,0.7))
  if(v<1) v<-1 else if(v>4) v <- 4
  return(c("h","j", "l", "k")[v])
}