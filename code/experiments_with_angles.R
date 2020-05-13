deg2rad <- function(x) {
  x * pi/180
}

rad2deg <- function(x) {
  x * 180/pi
}

deg2vec <- function(x) {
  c(sin(deg2rad(x)), cos(deg2rad(x)))
}

vec2deg <- function(x) {
  rad2deg(atan2(x[1], x[2]))
}

mean_vec <- function(x) {
  y <- lapply(x, deg2vec)
  Reduce(`+`, y)/length(y)
}

mean_deg <- function(x) {
  res <- vec2deg(mean_vec(x))
  if (res < 0) {
    360 + res
  } else {
    res
  }
}

mean_deg(c(350, 10))
mean_deg(c(90, 180, 270, 360))
mean_deg(c(10, 20, 30))

library(ggplot2)

testplot <- function(a, b) {
  ggplot() +
    geom_point(data = a, mapping = aes(x = angle, y = r)) +
    geom_point(data = b, mapping = aes(x = angle, y = r), color = "red", size = 5) +
    coord_polar() +
    xlim(0,360)
}

a <- tibble::tibble(angle = c(270, 10, 20, 200), r = 1)
b <- tibble::tibble(angle = mean_deg(a$angle), r = 1)

testplot(a, b)

a <- tibble::tibble(angle = c(10, 15, 355, 270, 180, 90, 90, 95, 115), r = 1)
b <- tibble::tibble(angle = mean_deg(a$angle), r = 1)

testplot(a, b)

