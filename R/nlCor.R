 #' Non Linear Correaltion function - Correlation coeffecient by considering non linear realtionships among two variables
 #' @param X         a numeric vector, matrix or data frame.
 #' @param Y         a vector, matrix or data frame with compatible dimensions to x
 #' @param inverse   an optional parameter to take the relation between inverse eg. checking 1/X^2,1/X^3 insted of X^2, X^3
 #' @param n         Maximum Order number you would want to check
 #' @param method    a character string indicating which correlation coefficient (or covariance) is to be computed.
 #' @param method    log(logarithmic), poly(Polynomial), expo(Exponential), Linear. Default is log.

 #' @examples X <- mtcars[,1:2]
 #' @examples Y <- mtcars[,1:2]
 #' @examples nlCor(X,Y,method="poly",n=3)
 #' @return Returns Non Linear Correlation coeffecient
 #' @export

nlCor <-function(X,Y,inverse=F,n=2,method="log",na.rm=F){
  #checking the error conditions on arguments

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  if (!(is.logical(inverse)))
    stop("'Inverse' must be Logical")

  if (!(n%%1==0))
    stop("'n' must be an Integer")

  na.method <- pmatch(method, c("poly", "log", "expo","Linear"))
  for(i in 1:length(na.method)){
    if(is.na(na.method[i]))
      stop("invalid 'method' argument")
  }
  if (!(is.numeric(X) || is.logical(X)))
    stop("'X' must be all numeric")

  if (!(is.numeric(Y) || is.logical(Y)))
    stop("'Y' must be all numeric")


  if("poly" %in% method){
    if(inverse==FALSE){
      for(i in 2:n){
        X2 <- X^i
        print(paste0("Poly Converse of Order ",i," :"))
        print(cor(X2,Y))
      }
    }
    else{
      for(i in 1:n){
        X2 <- X^(-i)
        print(paste0("Poly Inverse of Order ",i," :"))
        print(cor(X2,Y))
      }
    }
  }
  if("expo" %in% method){
    if(inverse==FALSE){
      for(i in 2:n){
        X2 <- i^X
        print(paste0("Expo Converse of Value ",i," :"))
        print(cor(X2,Y))
      }
    }
    else{
      for(i in 2:n){
        X2 <- i^(-X)
        print(paste0("Expo Inverse of Value ",i," :"))
        print(cor(X2,Y))
      }
    }
  }
  if("log" %in% method){
    if(inverse==FALSE){
      X2 <- log(X)
      print(paste0("Logarithmic Relation :"))
      print(cor(X2,Y))
    }
    else{
      for(i in 2:n){
        X2 <- exp(X)
        print(paste0("Logarithmic Inverse Relation :"))
        print(cor(X2,Y))
      }
    }
  }
  if("Linear" %in% method){
    print(paste0("Linear  Relation :"))
    print(cor(X,Y))
  }
}
