quad <- function(a, b, c) {
  # Calculate the discriminant
  disc <- b^2 - 4*a*c
  
  # Check the discriminant for different conditions
  if (disc > 0) {
    root1 <- (-b + sqrt(disc)) / (2*a)
    root2 <- (-b - sqrt(disc)) / (2*a)
    print(paste("The solutions are", root1, "and", root2))
  } else if (disc == 0) {
    root <- -b / (2*a)
    print(paste("The solution is", root))
  } else {
    print("The equation has no real roots")
  }
}



