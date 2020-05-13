library(ggplot2)

testplot <- function(a, b) {
  ggplot() +
    geom_point(data = a, mapping = aes(x = angle, y = r)) +
    geom_point(data = b, mapping = aes(x = angle, y = r), color = "red", size = 5) +
    coord_polar() +
    xlim(0,360)
}

a <- tibble::tibble(angle = c(270, 10, 20, 200), r = 1)
b <- tibble::tibble(angle = mobest::mean_deg(a$angle), r = 1)

testplot(a, b)

a <- tibble::tibble(angle = c(10, 15, 355, 270, 180, 90, 90, 95, 115), r = 1)
b <- tibble::tibble(angle = mobest::mean_deg(a$angle), r = 1)

testplot(a, b)

