Dice<-1:6
Dice
#' Dice problem
#'
#' @param dietype is the amount of sides of the dice
#' @param probvalues the probability of getting a particular number of divots.
#'
#' @return This function returns the sum of two dice.
#' @export
#'
#' @examples
#' set.seed(123)
#' roll()
#' roll(1:20)
#'
roll<-function(dietype=1:6, probvalues=rep(1/length(dietype),length(dietype))){
  if(sum(probvalues)!=1)
    stop("'probvalues'must add to one")
  Dice<- sample(dietype,size=2,replace=TRUE, prob=probvalues)
  #Roll<-sample(x=Dice, 2, replace=TRUE)
  sum(Dice)
}
roll()

library(ggplot2)

qplot
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1) + geom_histogram(colour="purple", fill="lightgreen")



#qplot( carat, data = diamonds, geom = "density", colour = color)


#' Squaring numbers
#'
#' @param x is some numeric object.
#'
#' @return Squares the values in the numeric object.
#' @export
#'
#' @examples
#'sq(c(1,2,5))
#'sq(3:6)
sq<- function(x){
  #if(typeof(x) !=double)
   # stop("object must be numeric")
  x*x
}

#' Power function
#'
#' @param x must be a numeric object
#' @param power must be a numeric object, raise the values in \code{power}.
#'
#' @return Raises the value x to a power
#' @export
#'
#' @examples
#' rp(c(4:12),3)
#' rp(c(2,3,5),4)
#' rp(x=c(1,2,3), power=3)
rp<- function(x, power=1){
  x^power

}
#shift control alt r

