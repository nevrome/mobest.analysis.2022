# sbatch slurm_parameter_exploration.sh

library(magrittr)

args <- unlist(strsplit(commandArgs(trailingOnly = TRUE), " "))
run <- args[1]
dt_for_this_run <- as.numeric(args[2])
g_for_this_run <- as.numeric(args[3])

#### data ####

load("data/poseidon_data/janno_final.RData")

interpol_comparison <- mobest::crossvalidate(
  independent = tibble::tibble(
    x = janno_final$x, 
    y = janno_final$y, 
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = list(
    C1 = janno_final$C1,
    C2 = janno_final$C2
  ),
  kernel = mobest::create_kernel_grid(
    ds = seq(50, 2000, 50)*1000,#seq(100, 10000, 100)*1000, 
    dt = dt_for_this_run, 
    g = g_for_this_run
  )
)

save(interpol_comparison, file = paste0(
  "data/parameter_exploration/crossvalidation/interpol_comparison_", 
  run, 
  ".RData"
))

# scp schmid@cdag2-new.cdag.shh.mpg.de:/projects1/coest_mobility/coest.interpol.2020/data/parameter_exploration/crossvalidation/interpol_comparison_* .
