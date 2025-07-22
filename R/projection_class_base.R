#' @include class_plots.R

#' @import tidyverse
#' @import progress
#' @import openxlsx
#' @import plyr
#' @import logr

sex_vector <- c(rep("males", 20), rep("females", 20))
sex_vector_mortality <- c(rep("males", 21), rep("females", 21))
age_vector <- rep(c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+"), 2)
age_vector_mortality <- rep(c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+"), 2)
age_vector_fertility <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
age_vector_asfr <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "TFR")
age_vector_women_population <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

projection_object <- setRefClass(
  "Projection",
  fields = list(
    country_object_list = "list",
    steps = "numeric",
    migration = "logical",
    switching = "logical",
    switching_type = "numeric",
    no_diff = "logical"
  )
)


#' @exportClass projection_object
projection_object$methods(
  list(

  project = function(migration = TRUE, steps = 1, switching = TRUE, switching_type = 1, no_diff = FALSE) {
    # Progress bar for calculations
    if(!requireNamespace("progress", quietly = TRUE)){
      stop(stop(
        "Package \"progress\" must be installed to use this function.",
        call. = FALSE
      ))
    }
    if (migration) {
      pb <- progress::progress_bar$new(
        format = paste("Projection of ", as.character(steps), " steps [:bar] :percent Estimated time left: :eta", sep = ""),
        total = steps * length(country_object_list) * 5, clear = FALSE, width = 120
      )
    } else {
      pb <- progress::progress_bar$new(
        format = paste("Projection of ", as.character(steps), " kroków [:bar] :percent Estimated time left: :eta", sep = ""),
        total = steps * length(country_object_list), clear = FALSE, width = 120
      )
    }

    # Projections
    if (migration) {
      for (i in 1:steps) {
        for (country in country_object_list) {
          country$dem_proj_migration(1, switching, switching_type, no_diff)
          pb$tick()
        }
        for (destination in country_object_list) {
          for (origin in country_object_list) {
            if(is.matrix(origin$emigration_list[[destination$code]]))
              destination$structure_data[[length(destination$structure_data)]] <- destination$structure_data[[length(destination$structure_data)]] + origin$emigration_list[[destination$code]]
          }
          pb$tick()
        }
        for (country in country_object_list) {
          country$dem_proj_migration(2, switching, switching_type, no_diff)
          pb$tick()
        }
        for (destination in country_object_list) {
          for (origin in country_object_list) {
            if(is.matrix(origin$emigration_list[[destination$code]]))
              destination$structure_data[[length(destination$structure_data)]] <- destination$structure_data[[length(destination$structure_data)]] + origin$emigration_list[[destination$code]]
          }
          pb$tick()
        }
        for (country in country_object_list) {
          country$dem_proj_migration(3, switching, switching_type, no_diff)
          pb$tick()
        }
      }
    } else {
      for (i in 1:steps) {
        for (country in country_object_list) {
          country$dem_proj_no_mig(switching, switching_type, no_diff)
          pb$tick()
        }
      }
    }
  }


  )
)
