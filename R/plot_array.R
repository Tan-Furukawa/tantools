#'plot array
#'@export plot_array
plot_array <- function(x = list(a = seq(iris$Sepal.Length),
                                b = seq(iris$Sepal.Width),
                                c = seq(iris$Petal.Length)),
                       y = list(a = iris$Sepal.Length,
                                b = iris$Sepal.Width,
                                c = iris$Petal.Length),
                       xlim = range(x),
                       ylim = range(y),
                       xlab = "x",
                       ylab = "y",
                       subtitle = names(y)) {

  old.par <- par(no.readonly=TRUE)
  par(mfrow = c(1,length(y)),
      oma = c(5,5,2,2),
      mar = c(0,0,2,0))



  plot(x[[1]],
       y[[1]],
       xlim = xlim,
       ylim = ylim,
       main = subtitle[1])

  for(i in 2:length(y)) {
    plot(x[[i]],
         y[[i]],
         xlim = xlim,
         ylim = ylim,  axes = F,
         main = subtitle[i])
    box()
    axis(side = 2,  labels = F)
    axis(side = 1,  labels = T)

  }

  title(xlab = xlab,
        ylab = ylab,
        outer = TRUE,
        line = 3)

  par(old.par)

}


plot_array()

