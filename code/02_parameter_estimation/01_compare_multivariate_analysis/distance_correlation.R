load("data/genotype_data/janno_final.RData")

large_distance_table <- mobest::calculate_pairwise_distances(
  independent = mobest::create_spatpos(
    id = janno_final$Poseidon_ID,
    x = janno_final$x,
    y = janno_final$y,
    z = janno_final$Date_BC_AD_Median_Derived
  ),
  dependent = mobest::create_obs(
    C1_mds_u = janno_final$C1_mds_u,
    C2_mds_u = janno_final$C2_mds_u,
    C3_mds_u = janno_final$C3_mds_u,
    C4_mds_u = janno_final$C4_mds_u,
    C1_mds_f = janno_final$C1_mds_f,
    C2_mds_f = janno_final$C2_mds_f,
    C3_mds_f = janno_final$C3_mds_f,
    C4_mds_f = janno_final$C4_mds_f,
    C1_pca_u = janno_final$C1_pca_u,
    C2_pca_u = janno_final$C2_pca_u,
    C3_pca_u = janno_final$C3_pca_u,
    C4_pca_u = janno_final$C4_pca_u,
    C1_pca_f = janno_final$C1_pca_f,
    C2_pca_f = janno_final$C2_pca_f,
    C3_pca_f = janno_final$C3_pca_f,
    C4_pca_f = janno_final$C4_pca_f,
    C1_emu_u = janno_final$C1_emu_u,
    C2_emu_u = janno_final$C2_emu_u,
    C3_emu_u = janno_final$C3_emu_u,
    C4_emu_u = janno_final$C4_emu_u,
    C1_emu_f = janno_final$C1_emu_f,
    C2_emu_f = janno_final$C2_emu_f,
    C3_emu_f = janno_final$C3_emu_f,
    C4_emu_f = janno_final$C4_emu_f
  )
)

save(large_distance_table, "data/parameter_exploration/multivariate_comparison/large_distance_table.RData")
