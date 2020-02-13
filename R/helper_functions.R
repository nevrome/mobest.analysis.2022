#### helper functions ####

range_01 <- function(x, min, max) { (x - min) / (max - min) }
dist_scale_01 <- function(x, min, max) { x / abs(min - max) }
range_real <- function(x, min, max) { min + x * abs(min - max) }
dist_scale_real <- function(x, min, max) { x * abs(min - max) }

range_01_x <- function(x) { range_01(x, bb[1], bb[3]) }
range_01_y <- function(y) { range_01(y, bb[2], bb[4]) }
range_01_z <- function(z) { range_01(z, min(anno$calage_center), max(anno$calage_center)) }
dist_scale_01_x_km <- function(x) { dist_scale_01(x * 1000, bb[1], bb[3]) }
dist_scale_01_y_km <- function(y) { dist_scale_01(y * 1000, bb[2], bb[4]) }
dist_scale_01_z_y <- function(z) { dist_scale_01(z, min(anno$calage_center), max(anno$calage_center)) }
range_real_x <- function(x) { range_real(x, bb[1], bb[3]) }
range_real_y <- function(y) { range_real(y, bb[2], bb[4]) }
range_real_z <- function(z) { range_real(z, min(anno$calage_center), max(anno$calage_center)) }
