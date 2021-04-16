source("code/technical_helpers.R")



system("./code/01_poseidon_data_preparation/00_fetch_poseidon.sh")
source("./code/01_poseidon_data_preparation/00_prepare_spatial_data.R")
source("./code/01_poseidon_data_preparation/01_janno_filter_for_relevant_individuals.R")
system("./code/01_poseidon_data_preparation/02_poseidon_extract.sh")

cluster_up("data/poseidon_data/poseidon_extracted/poseidon_extracted.janno")
system("./code/01_poseidon_data_preparation/03_mds_plink.sh")
cluster_down("data/poseidon_data/mds/poseidon_extracted.pruned.mds")

source("./code/01_poseidon_data_preparation/04_prepare_final_dataset.R")
source("./code/01_poseidon_data_preparation/05_prepare_plot_reference_data.R")