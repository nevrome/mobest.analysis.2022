default_kernel <- mobest::create_kernset_multi(
  d = list(c(700000, 700000, 800)), 
  g = 0.06, 
  on_residuals = T, 
  auto = F,
  it = "ds700_dt800_g006"
)

save(default_kernel, file = "data/origin_search/default_kernel.RData")
