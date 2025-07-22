## code to prepare `DATASET` dataset goes here

migration <- F
steps <- 10
switching <- T
switching_type <- 1
no_diff <- F
dataset <- "INPUT_TEST"
filenames_vector <- c("projection_structure_data.csv",
                      "projection_fertility_data.csv",
                      "projection_mortality_data.csv",
                      "projection_asfr.csv",
                      "projection_fertile_women_population.csv",
                      "projection_fertility_differentials.csv",
                      "projection_switching_matrices.csv",
                      "projection_switching_data.csv",
                      "projection_migration_data.csv")


usethis::use_data(DATASET, overwrite = TRUE)
