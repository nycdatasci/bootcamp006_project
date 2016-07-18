sample_set <- function(df) {
  set.seed(42)
  s <- sample.split(df$FULLDATE, 1/83)
  return(df[s,])
}