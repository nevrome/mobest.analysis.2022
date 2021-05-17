default_kernel <- mobest::create_kernset_multi(
  d = list(c(600000, 600000, 900)), 
  g = 0.06, 
  on_residuals = T, 
  auto = F,
  it = "ds600_dt900_g006"
)

save(default_kernel, file = "data/origin_search/default_kernel.RData")
