# Creating matrices for various tests
x <- matrix(1:9, nrow=3, byrow=TRUE)
y <- matrix(1:9, nrow=9, ncol=1)
z <- matrix(1)
# Function to rotate 90 degrees
rotate <- function(x) t(apply(x, 2, rev))
# Viewing matrix before rotation 
x
rotate(x)
y
rotate(y)
z
rotate(z)