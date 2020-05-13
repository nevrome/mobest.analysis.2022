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

mean_deg(c(10, 15, 355, 270, 180, 90, 90, 95, 115))

mean_deg(c(360, 180))

# mobest:::angle_between_along_360(hu)
