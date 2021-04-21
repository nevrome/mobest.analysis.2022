source("code/technical_helpers.R")

local_script("./code/01_poseidon_data_preparation/00_fetch_poseidon.sh")
source("./code/01_poseidon_data_preparation/00_prepare_plot_reference_data.R")
source("./code/01_poseidon_data_preparation/00_prepare_spatial_data.R")
cluster_up("data/spatial")

source("./code/01_poseidon_data_preparation/01_janno_filter_for_relevant_individuals.R")
local_script("./code/01_poseidon_data_preparation/02_poseidon_extract.sh")

cluster_up("data/poseidon_data/poseidon_extracted")
cluster_qsub_script("code/01_poseidon_data_preparation/03_mds_plink.sh")
# wait until cluster run ready
cluster_down("data/poseidon_data/mds/poseidon_extracted.pruned.mds")

source("./code/01_poseidon_data_preparation/04_prepare_final_dataset.R")

cluster_up("data/poseidon_data/janno_final.RData")
