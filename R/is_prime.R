#' Check if a number is prime
#' @description To check if a number n is prime, you only need to check for factors up to the square root of n. 
#' This is because if n has a factor greater than its square root, it must also have a smaller factor 
#' (since a factor is a number that divides n without leaving a remainder). 
#' This method significantly reduces the number of checks needed to determine if a number is prime.
#' 
#' @param n (numeric) Number to verify if it prime
#'
#' @return (bool) TRUE if the number is prime, FALSE if the number is not prime
#' @export
#'
#' @examples
#' is_prime(0)
#' is_prime(1)
#' is_prime(7)
#' is_prime(10)
#' is_prime(100011869)
is_prime <- function(n){
  # We know 1 or negatives are not a prime number
  if (n <= 1) return(FALSE)
  
  # If not integer
  if (n %% 1 !=0) return(FALSE)
  
  i <- 2
  # This will loop from 2 to int(sqrt(x))
  while (i*i <= n) {
    # Check if i divides x without leaving a remainder
    if (n %% i == 0){
      # This means that n has a factor in between 2 and sqrt(n)
      # So it is not a prime number
      return(FALSE)
    }
    i <-  i + 1
  }
  # If we did not find any factor in the above loop,
  # then n is a prime number
  return(TRUE)
}