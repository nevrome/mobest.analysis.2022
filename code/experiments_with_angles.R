deg2rad <- function(x) {
  x * pi/180
}

rad2deg <- function(x) {
  x * 180/pi
}

deg2vec <- function(x) {
  c(sin(deg2rad(x)), cos(deg2rad(x)))
}

a <- deg2vec(360)
b <- deg2vec(270)
d <- deg2vec(280)

mean_vec_2 <- function(x) {
  Reduce(`+`, x)/length(x)
}

x <- list(a, b, d)
vec <- mean_vec_2(x)

vec2deg <- function(x) {
  rad2deg(atan2(x[1], x[2]))
}

res <- vec2deg(vec)

if (res < 0) {
  schu <- 360+res
} else {
  schu <- res
}

# mobest:::angle_between_along_360(hu)
