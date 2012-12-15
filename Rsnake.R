library(grDevices)

rsnake <- function(x=8,y=8, auto=NULL, plot=TRUE, detailed=FALSE) {
  bodycheck <- function(newhead) {
    if(newhead[1] < 1 || newhead[1] > x || newhead[2] < 1 || newhead[2] > y) return(FALSE)
    return(sum(sapply(1:length(snake), function(x){identical(snake[[x]], newhead)})) == 0)
  }
  
  newfood <- function() {
    while(!bodycheck(food <<- c(round(runif(1, min=1, max=x)), round(runif(1,min=1, max=y))))) FALSE
    rx <- food[1]
    ry <- food[2]
    if(!plot) return()
    rect(rx-0.1, ry-0.1, rx+0.1, ry+0.1,col="red")
  }
  
  gameover <- function() {
    newheads <- matrix(rep(c(snake[[1]][1], snake[[1]][2]),4),ncol=2,byrow=TRUE)
    newheads[,1] <- newheads[,1] + c(1,-1,0,0)
    newheads[,2] <- newheads[,2] + c(0,0,1,-1)
    for(i in 1:nrow(newheads)) if(bodycheck(newheads[i,])) return(FALSE)
    return(TRUE)
  }
  drawsnake <- function(prevsnake=NULL) {
    if(!plot) return(NULL)
    rect(1-0.6+0.1, y+0.5, 6, y+1-0.05, col="white", lty="blank")
    text(1-0.6, y+0.7, paste("Score:",score),pos=4)
    if(!is.null(prevsnake)) {
      # draw a "body" over the previous head
      rx <- prevsnake[[1]][1]    
      ry <- prevsnake[[1]][2]
      rect(rx-0.4, ry-0.4, rx+0.4, ry+0.4,col="white")
      # clear the "tail"
      if(length(prevsnake) == length(snake)) {
        rx <- prevsnake[[length(prevsnake)]][1]
        ry <- prevsnake[[length(prevsnake)]][2]
        rect(rx-0.5, ry-0.5, rx+0.5, ry+0.5,col="white", lty="blank")
      }
      # draw the new "head"
      rx <- snake[[1]][1]
      ry <- snake[[1]][2]
      rect(rx-0.4, ry-0.4, rx+0.4, ry+0.4,col="gray")    
      return(NULL)
    }
    headx <- snake[[1]][1]
    heady <- snake[[1]][2]
    cols <- c("gray", rep("white", length(snake)-1))
    sapply(1:length(snake), function(x){
      rx <- snake[[x]][1]
      ry <- snake[[x]][2]
      rect(rx-0.4, ry-0.4, rx+0.4, ry+0.4,col=cols[x])
    })
  }
  
  origin <- c(floor(x/2), floor(y/2))
  if(origin[1]-2 < 1) return(NULL)
  snake <- list(origin, c(origin[1]-1, origin[2]), c(origin[1]-2, origin[2]))
  if(plot) {
    plot(origin[1],origin[2],type="n", main="Snake Game", xlab="", ylab="", asp=1, axes=FALSE, xlim=c(1-0.6,x+0.6), ylim=c(1-0.6,y+1))
    rect(1-0.6,1-0.6,x+0.6,y+1)
  }
  psnake <- NULL
  score <- 0
  inputs <- 0
  movements <- 0
  drawsnake(psnake)
  newfood()
  keydown <- function(key){
    #print(c(snake[[1]][1], snake[[1]][2], key))
    if(key == "q") return(invisible(1))
    inputs <<- inputs + 1
    psnake <<- snake
    newhead <- switch(key, 
                      "l" = c(psnake[[1]][1]+1, psnake[[1]][2]),
                      "Right" = c(psnake[[1]][1]+1, psnake[[1]][2]),
                      "h" = c(psnake[[1]][1]-1, psnake[[1]][2]),
                      "Left" = c(psnake[[1]][1]-1, psnake[[1]][2]),
                      "k" = c(psnake[[1]][1], psnake[[1]][2]+1),
                      "Up"= c(psnake[[1]][1], psnake[[1]][2]+1),
                      "j" = c(psnake[[1]][1], psnake[[1]][2]-1),
                      "Down"= c(psnake[[1]][1], psnake[[1]][2]-1),
                      c(psnake[[1]][1], psnake[[1]][2]))
    if(bodycheck(newhead)) {
      if(identical(newhead, food)) {
        snake <<- append(list(newhead), psnake)
        score <<- score + 1
        newfood()
      } else {
        last <- length(psnake)+1
        snake <<- append(list(newhead), psnake)[-last]
      }
      drawsnake(psnake) 
      if(gameover()) {
        if(plot) text(x/2+1, y/2+1, labels="GAME OVER")
        return(invisible(2))
      }
      movements <<- movements + 1
      return(NULL)
    }
    
  }
  if(is.null(auto)) {
    setGraphicsEventHandlers(onKeybd=keydown)
    getGraphicsEvent()
  } else {
    game <- function(){
      r <- keydown(auto(snake,food,x,y))
      if(is.null(r)) return(TRUE)
      else if(r != 2) return(TRUE)
      else return(FALSE)
    }
    while(game()) TRUE
  }
  if(!detailed) return(score)
  scores <- c(score, inputs, movements)
  names(scores) <- c("Score", "Inputs", "Movements")
  return(scores)
}

