word1 = readline(prompt = "Enter word 1: ")
word2 = readline(prompt = "Enter word 2: ")
word3 = readline(prompt = "Enter word 3: ")
x = c(word1, word2, word3)
lcPrefix <- function (x, ignore.case = FALSE) 
{
  x <- as.character(x)
  nc <- nchar(x, type = "char")
  for (i in 1:min(nc)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      return(substr(x[1], 1, i - 1))
    }
  }
  substr(x[1], 1, i)
}
lcPrefix(x)