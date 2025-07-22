#' @include Data_adjustment.R projection_class_plots.R
NULL
#Options

dist_list <- list(c(0.2,0.1,0.1,0.1,0.1,0.1,0.1,0.2), c(0.2,0.1,0.1,0.1,0.1,0.1,0.1,0.2))

directory <- "INPUT_TEST"

country_info_name <- "country_codes_fix.xlsx"
structure_data_name <- "structure_data_fix.xlsx"
asfr_data_name <- "asfr_fix.xlsx"
fertility_differentials_name <- "fert_diff_fix.xlsx"
survivability_data_name <- "survivability_fix.xlsx"
sex_ratio_name <- "ratio_fix.xlsx"
switching_data_name <- "switching_fix.xlsx"
destination_data_name <- "Country destination_TEST.xlsx"
migration_rates_data_name <- "Migration rates_TEST.xlsx"

files <- c(country_info_name,
           structure_data_name,
           asfr_data_name,
           fertility_differentials_name,
           survivability_data_name,
           sex_ratio_name,
           switching_data_name,
           destination_data_name,
           migration_rates_data_name)

#Functions for loading and adjusting data

c_codes_creator <- function(country_codes) {
  c_country_codes <- c()
  for (i in 1:length(country_codes)) {
    if (country_codes[i] < 10) {
      c_country_codes[i] <- paste("00", country_codes[i], sep = "")
    } else if (country_codes[i] > 9 & country_codes[i] < 100) {
      c_country_codes[i] <- paste("0", country_codes[i], sep = "")
    } else {
      c_country_codes[i] <- country_codes[i]
    }
  }
  for (i in 1:length(c_country_codes)) {
    c_country_codes[i] <- paste("c", c_country_codes[i], sep = "")
  }
  return(c_country_codes)
}

country_info_adjustment <- function(directory_name, file_name) {
  country_info <- read_excel(paste(directory_name, "/", file_name, sep = ""))
  country_info <- country_info[, 1:3]
  country_info <- country_info[order(country_info[[2]]), ]
  country_info <- cbind(country_info, c_codes_creator(country_info[[2]]))
  colnames(country_info) <- c(colnames(country_info)[1:3], "C_country_code")
  return(country_info)
}

structure_data_adjustment <- function(directory_name, file_name) {
  structure_data <- read_excel(paste(directory_name, "/",
                                     file_name, sep = ""), sheet = 2)
  structure_data <- structure_data[order(structure_data[[1]],
                                         decreasing = TRUE), ]
  structure_data <- structure_data[order(structure_data[[2]]), ]
  population <- c()
  for (i in 1:nrow(structure_data)) {
    population <- c(population,
                    as.numeric(structure_data[i, 3:ncol(structure_data)]))
  }
  structure_differentials <- read_excel(paste(directory_name, "/",
                                              file_name, sep = ""), sheet = 3)
  structure_differentials <- structure_differentials[order(structure_differentials[[2]]), ]
  structure_data <- cbind(structure_differentials[, 2:4], population,
                          structure_differentials[, 5:ncol(structure_differentials)])
  structure_data[, 5:ncol(structure_data)] <- structure_data[[4]] * structure_data[, 5:ncol(structure_data)]
  structure_data <- structure_data[, -4]
  post_switch <- as.data.frame(matrix(data = 0, nrow = nrow(structure_data),
                                      ncol = (ncol(structure_data) - 3)))
  colnames(post_switch) <- paste(colnames(structure_data)[4:ncol(structure_data)],
                                 "_post_switch", sep = "")
  structure_data <- cbind(structure_data, post_switch)
  return(structure_data)
}

asfr_adjustment <- function(directory_name, file_name) {
  asfr_data <- read_excel(paste(directory_name, "/", file_name, sep = ""))
  return(asfr_data)
}

fertility_differentials_adjustment <- function(directory_name, file_name) {
  fertility_differentials_data <- read_excel(paste(directory_name, "/",
                                                   file_name, sep = ""))
  return(fertility_differentials_data)
}

sex_ratio_data_adjustment <- function(directory_name, file_name) {
  sex_ratio_data <- read_excel(paste(directory_name, "/", file_name, sep = ""))
  return(sex_ratio_data)
}

survivability_data_adjustment <- function(directory_name, file_name) {
  survivability_data <- read_excel(paste(directory_name, "/",
                                         file_name, sep = ""))
  survivability_data <- survivability_adjustment(survivability_data)
  return(survivability_data)
}

switching_data_adjustment <- function(directory_name, file_name) {
  switching_data <- read_excel(paste(directory_name, "/", file_name, sep = ""))
  return(switching_data)
}

destination_data_adjustment <- function(directory_name, file_name) {
  destination_data <- read_excel(paste(directory_name, "/",
                                       file_name, sep = ""))
  return(destination_data)
}

migration_rates_data_adjustment <- function(directory_name, file_name) {
  migration_rates_data <- read_excel(paste(directory_name, "/",
                                           file_name, sep = ""))
  return(migration_rates_data)
}

data_loader <- function(directory_name, files_names) {
  country_info <- country_info_adjustment(directory_name, files_names[[1]])
  structure_data <- structure_data_adjustment(directory_name, files_names[[2]])
  asfr_data <- asfr_adjustment(directory_name, files_names[[3]])
  fertility_differentials_data <- fertility_differentials_adjustment(directory_name, files_names[[4]])
  survivability_data <- survivability_data_adjustment(directory_name, files_names[[5]])
  sex_ratio_data <- sex_ratio_data_adjustment(directory_name, files_names[[6]])
  switching_data <- switching_data_adjustment(directory_name, files_names[[7]])
  destination_data <- destination_data_adjustment(directory_name, files_names[[8]])
  migration_rates_data <- migration_rates_data_adjustment(directory_name, files_names[[9]])
  data_list <- list(country_info,
                    structure_data,
                    asfr_data,
                    fertility_differentials_data,
                    survivability_data,
                    sex_ratio_data,
                    switching_data,
                    destination_data,
                    migration_rates_data)
  return(data_list)
}

#Function for checking data for errors

error_check <- function(object) {

  n_religions <- (ncol(object$structure_data[[1]]) / 2)
  n_age_groups <- nrow(object$structure_data[[1]])

  errors <- 0
  error <- F

  if (nrow(object$mortality_rates) != (n_age_groups + 2)) {
    log_print(paste("Error in ", as.character(object$code),": wrong dimensions of matrix mortality_rates"), blank_after = T, hide_notes = T)
    errors <- errors + 1
  }

  if (!(ncol(object$fertility_differentials[[1]]) != n_religions || ncol(object$fertility_differentials[[1]]) != (n_religions * 2))) {
    log_print(paste("Error in ", as.character(object$code),": wrong dimensions of matrix fertility_differentials"), blank_after = T, hide_notes = T)
    errors <- errors + 1
  }

  for (i in 1:length(object$sex_ratio)) {
    if (object$sex_ratio[[i]] < 0 || object$sex_ratio[[i]] > 1) {
      log_print(paste("Error in ", as.character(object$code),": value of variable sex_ratio is not between 0 and 1"), blank_after = T, hide_notes = T)
      errors <- errors + 1
    }
  }

  if (!(ncol(object$outmigration_rates[[1]]) != n_religions || ncol(object$outmigration_rates[[1]]) != (n_religions * 2) || nrow(object$outmigration_rates[[1]]) != n_age_groups)) {
    log_print(paste("Error in ", as.character(object$code),": wrong dimensions of matrix outmigration_rates"), blank_after = T, hide_notes = T)
    errors <- errors + 1
  }

  if (!(nrow(object$destination_matrix) != (n_religions * 2) || nrow(object$destination_matrix) != (n_religions * 4))) {
    log_print(paste("Error in ", as.character(object$code),": wrong dimensions of matrix destination_matrix"), blank_after = T, hide_notes = T)
    errors <- errors + 1
  }

  if (object$start_fertility_group < 0) {
    log_print(paste("Error in ", as.character(object$code),": negative value of variable start_fertility_group"), blank_after = T, hide_notes = T)
    errors <- errors + 1
  }

  if (errors > 0) {
    log_print(paste("Encountered: ", as.character(errors)," errors in", as.character(object$code)), blank_after = T, hide_notes = T)
  }

}

#Functions for creating objects

object_loader <- function(code,
                          start_year = 2010,
                          start_period = "2010-2015",
                          start_fertility_group = 4,
                          post_switch = TRUE,
                          distribution_list = dist_list,
                          country_info,
                          structure_data,
                          asfr_data,
                          fertility_differentials_data,
                          survivability_data,
                          sex_ratio_data,
                          switching_data,
                          destination_data,
                          migration_rates_data) {

  country_info <- as.data.frame(country_info[country_info[[2]] == code, ])

  structure_data <- as.data.frame(structure_data[structure_data[[1]] == code, ])
  age_groups <- unique(structure_data[[2]])
  rownames(structure_data) <- paste(structure_data[[3]],
                                    structure_data[[2]], sep = " ")
  structure_data <- as.matrix(structure_data[, -(1:3)])

  asfr_data <- as.data.frame(asfr_data[asfr_data[[1]] == code, ])
  rownames(asfr_data) <- asfr_data[[2]]
  asfr_data <- asfr_data[, -(1:2)]

  fertility_differentials_data <- as.data.frame(fertility_differentials_data[fertility_differentials_data[[1]] == code, ])
  fertility_differentials_data <- fertility_differentials_data[, -(1:2)]
  fertility_differentials_data_labels <- fertility_differentials_data[[2]]
  fertility_differentials_data <- as.matrix(fertility_differentials_data)
  rownames(fertility_differentials_data) <- fertility_differentials_data_labels
  #names(fertility_differentials_data) <- start_period

  sex_ratio_data <- sex_ratio_data[sex_ratio_data[[1]] == code, ]
  sex_ratio_colnames <- colnames(sex_ratio_data)[-1]
  sex_ratio_data <- as.numeric(unlist(sex_ratio_data[, -1]))
  names(sex_ratio_data) <- sex_ratio_colnames

  survivability_data <- as.data.frame(survivability_data[survivability_data[[1]] == code, ])
  rownames(survivability_data) <- paste(survivability_data[[2]],
                                        rep(c("0", "1-4", age_groups[-1]), 2))
  survivability_data <- survivability_data[, -(1:2)]

  switching_data <- switching_data[switching_data[[1]] == code, ]
  switching_data <- switching_data[2:ncol(switching_data)]
  switching_data <- final_decomposition(switching_data, distribution_list,
                                        structure_data, TRUE)

  destination_data <- as.data.frame(destination_data[destination_data[[1]] == code, ])
  rownames(destination_data) <- paste(destination_data[[3]],
                                      destination_data[[2]], sep = " ")
  destination_data <- as.matrix(destination_data[, -(1:3)])

  migration_rates_data <- as.data.frame(migration_rates_data[migration_rates_data[[4]] == code, ])
  migration_rates_list <- list()
  for (year in unique(migration_rates_data[[1]])) {
    migration_rates_data_single <- migration_rates_data[migration_rates_data[[1]] == year, ]
    rownames(migration_rates_data_single) <- paste(migration_rates_data_single[[2]],
                                                   migration_rates_data_single[[3]], sep = " ")
    migration_rates_list[[length(migration_rates_list) + 1]] <- as.matrix(migration_rates_data_single[, -(1:4)])
    names(migration_rates_list)[length(migration_rates_list)] <- year
  }

  object <- Country_object$new(
    year = start_year,
    period = start_period,
    name = country_info[1, 1],
    code = country_info[1, 2],
    region = country_info[1, 3],
    structure_data = list("2010" = structure_data),
    mortality_rates = as.matrix(survivability_data),
    age_specific_fertility_rates = as.matrix(asfr_data),
    fertility_differentials = list("2010-2015" = fertility_differentials_data),
    sex_ratio = sex_ratio_data,
    switching_matrices_list = switching_data,
    destination_matrix = as.matrix(destination_data),
    outmigration_rates = migration_rates_list,
    start_fertility_group = start_fertility_group,
    post_switch = post_switch,
    fertility_factorization = 1
  )

  error <- error_check(object)

  return(object)


}



object_list_creator <- function(start_year = 2010,
                                start_period = "2010-2015",
                                start_fertility_group = 4,
                                post_switch = TRUE,
                                distribution_list = dist_list,
                                country_info,
                                structure_data,
                                asfr_data,
                                fertility_differentials_data,
                                survivability_data,
                                sex_ratio_data,
                                switching_data,
                                destination_data,
                                migration_rates_data) {
  country_object_list <- list()
  for (country_code in unique(country_info[[2]])) {
    object <- object_loader(country_code,
                            start_year,
                            start_period,
                            start_fertility_group,
                            post_switch,
                            distribution_list,
                            country_info,
                            structure_data,
                            asfr_data,
                            fertility_differentials_data,
                            survivability_data,
                            sex_ratio_data,
                            switching_data,
                            destination_data,
                            migration_rates_data)

    country_object_list <- append(country_object_list, object)

  }

  names(country_object_list) <- country_info[[4]]
  return(country_object_list)
}


#' @export full_list
full_list <- function(start_year = 2010,
                      start_period = "2010-2015",
                      start_fertility_group = 4,
                      post_switch = TRUE,
                      distribution_list = dist_list,
                      directory_name = directory,
                      files_names= files) {
  data_list <- data_loader(directory, files)
  country_object_list <- object_list_creator(start_year,
                                             start_period,
                                             start_fertility_group,
                                             post_switch,
                                             distribution_list,
                                             data_list[[1]],
                                             data_list[[2]],
                                             data_list[[3]],
                                             data_list[[4]],
                                             data_list[[5]],
                                             data_list[[6]],
                                             data_list[[7]],
                                             data_list[[8]],
                                             data_list[[9]])
  return(country_object_list)
}
