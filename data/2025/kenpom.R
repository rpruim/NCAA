winprob <- function(e1, t1, e2, t2) {

  pts <- (e1 - e2) * (t1 + t2) / 200
  pnorm(pts, mean = 0, sd = 11)

}

W <-
  outer(
  1:68, 1:68,
  function(a, b) {
    winprob(
      ken[a, "AdjEM"][[1]], ken[a,"AdjT"][[1]],
      ken[b, "AdjEM"][[1]], ken[b,"AdjT"][[1]])
  }
)

heatmap(W)
dim(W)
