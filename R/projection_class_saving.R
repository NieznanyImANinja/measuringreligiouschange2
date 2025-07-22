#' @include projection_class_base.R

#' @exportClass projection_object
projection_object$methods(
  list(

    # Faster lists - credit to JanKanis on StackOverflow: https://stackoverflow.com/a/32870310
    ##################

    expanding_list = function(capacity = 10) {
      buffer <- vector("list", capacity)
      length <- 0

      methods <- list()

      methods$double.size <- function() {
        buffer <<- c(buffer, vector("list", capacity))
        capacity <<- capacity * 2
      }

      methods$add <- function(val) {
        if (length == capacity) {
          methods$double.size()
        }

        length <<- length + 1
        buffer[[length]] <<- val
      }

      methods$as.list <- function() {
        b <- buffer[0:length]
        return(b)
      }

      methods
    },

    #####################

    save_csv_startup = function(folder = "projections"){
      if (file.exists("Projections") == FALSE) {
        dir.create("Projections")
      }
      return(expanding_list())
    },

    save_csv_structure = function(filename = "projection_structure_data.csv", folder = "projections") {
      log_print("Saving structure data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$structure_data)
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving structure data to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (5 + ncol(country_object_list[[1]]$structure_data[[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Year", "Sex", "Age_group", colnames(country_object_list[[1]]$structure_data[[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 4, 5), ~ as.character(.))) %>%
        mutate(across(c(2, 3, 6:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (year in 1:length(country$structure_data)) {
          country_vector <- rep(country$name)
          country_code_vector <- rep(country$code, nrow(country$structure_data[[year]]))
          year_vector <- rep(as.numeric(names(country$structure_data)[[year]]), nrow(country$structure_data[[year]]))
          structure_data <- as.data.frame(country$structure_data[[year]])
          rownames(structure_data) <- NULL
          df <- data.frame(Country = country_vector, Country_code = country_code_vector, Year = year_vector, Sex = sex_vector, Age_group = age_vector)
          df <- cbind(df, structure_data)
          csv_list$add(df)
          pb$tick()
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_fertility = function(filename = "projection_fertility_data.csv", folder = "projections") {
      log_print("Saving fertility data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$previous_fertility) * length(country_object_list[[1]]$previous_fertility[[1]])
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving fertility data to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (5 + ncol(country_object_list[[1]]$previous_fertility[[1]][[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Year", "Age_group", "Structure", colnames(country_object_list[[1]]$previous_fertility[[1]][[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 3, 4, 5), ~ as.character(.))) %>%
        mutate(across(c(2, 6:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (period in 1:length(country$previous_fertility)) {
          for (structure in 1:length(country$previous_fertility[[period]])) {
            country_vector <- rep(country$name, nrow(country$previous_fertility[[period]][[1]]))
            country_code_vector <- rep(country$code, nrow(country$previous_fertility[[period]][[1]]))
            year_vector <- rep(names(country$previous_fertility)[period], nrow(country$previous_fertility[[period]][[1]]))
            structure_vector <- rep(names(country$previous_fertility[[period]])[structure], nrow(country$previous_fertility[[period]][[1]]))
            fertility_data <- as.data.frame(country$previous_fertility[[period]][[structure]])
            rownames(fertility_data) <- NULL
            df <- data.frame(Country = country_vector, Country_code = country_code_vector, Year = year_vector, Age_group = age_vector_fertility, Structure = structure_vector)
            df <- cbind(df, fertility_data)
            csv_list$add(df)
            pb$tick()
          }
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_mortality = function(filename = "projection_mortality_data.csv", folder = "projections") {
      log_print("Saving mortality data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$previous_mortality) * length(country_object_list[[1]]$previous_mortality[[1]])
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving mortality data to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (6 + ncol(country_object_list[[1]]$previous_mortality[[1]][[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Year", "Sex", "Age_group", "Structure", colnames(country_object_list[[1]]$previous_mortality[[1]][[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 3, 4, 5, 6), ~ as.character(.))) %>%
        mutate(across(c(2, 7:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (period in 1:length(country$previous_mortality)) {
          for (structure in 1:length(country$previous_mortality[[period]])) {
            country_vector <- rep(country$name, nrow(country$previous_mortality[[period]][[1]]))
            country_code_vector <- rep(country$code, nrow(country$previous_mortality[[period]][[1]]))
            year_vector <- rep(names(country$previous_mortality)[period], nrow(country$previous_mortality[[period]][[1]]))
            structure_vector <- rep(names(country$previous_mortality[[period]])[structure], nrow(country$previous_mortality[[period]][[1]]))
            mortality_data <- as.data.frame(country$previous_mortality[[period]][[structure]])
            rownames(mortality_data) <- NULL
            df <- data.frame(Country = country_vector, Country_code = country_code_vector, Year = year_vector, Sex = sex_vector_mortality, Age_group = age_vector_mortality, Structure = structure_vector)
            df <- cbind(df, mortality_data)
            csv_list$add(df)
            pb$tick()
          }
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_asfr = function(filename = "projection_asfr.csv", folder = "projections") {
      log_print("Saving ASFR data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$previous_asfr) * length(country_object_list[[1]]$previous_asfr[[1]])
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving ASFR data to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (5 + ncol(country_object_list[[1]]$previous_asfr[[1]][[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Year", "Age_group", "Structure", colnames(country_object_list[[1]]$previous_asfr[[1]][[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 3, 4, 5), ~ as.character(.))) %>%
        mutate(across(c(2, 6:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (period in 1:length(country$previous_asfr)) {
          for (structure in 1:length(country$previous_asfr[[period]])) {
            country_vector <- rep(country$name, nrow(country$previous_asfr[[period]][[1]]))
            country_code_vector <- rep(country$code, nrow(country$previous_asfr[[period]][[1]]))
            year_vector <- rep(names(country$previous_asfr)[period], nrow(country$previous_asfr[[period]][[1]]))
            structure_vector <- rep(names(country$previous_asfr[[period]])[structure], nrow(country$previous_asfr[[period]][[1]]))
            asfr <- as.data.frame(country$previous_asfr[[period]][[structure]])
            rownames(asfr) <- NULL
            df <- data.frame(Country = country_vector, Country_code = country_code_vector, Year = year_vector, Age_group = age_vector_asfr, Structure = structure_vector)
            df <- cbind(df, asfr)
            csv_list$add(df)
            pb$tick()
          }
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_women_population = function(filename = "projection_fertile_women_population.csv", folder = "projections") {
      log_print("Saving fertile women population data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$previous_women_population) * length(country_object_list[[1]]$previous_women_population[[1]])
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving women population data to .csv [:bar] :percent Estimated time left: :eta Total matrices:",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (5 + ncol(country_object_list[[1]]$previous_women_population[[1]][[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Year", "Age_group", "Structure", colnames(country_object_list[[1]]$previous_women_population[[1]][[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 3, 4, 5), ~ as.character(.))) %>%
        mutate(across(c(2, 6:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (period in 1:length(country$previous_women_population)) {
          for (structure in 1:length(country$previous_women_population[[period]])) {
            country_vector <- rep(country$name, nrow(country$previous_women_population[[period]][[1]]))
            country_code_vector <- rep(country$code, nrow(country$previous_women_population[[period]][[1]]))
            year_vector <- rep(names(country$previous_women_population)[period], nrow(country$previous_women_population[[period]][[1]]))
            structure_vector <- rep(names(country$previous_women_population[[period]])[structure], nrow(country$previous_women_population[[period]][[1]]))
            women_population <- as.data.frame(country$previous_women_population[[period]][[structure]])
            rownames(women_population) <- NULL
            df <- data.frame(Country = country_vector, Country_code = country_code_vector, Year = year_vector, Age_group = age_vector_women_population, Structure = structure_vector)
            df <- cbind(df, women_population)
            csv_list$add(df)
            pb$tick()
          }
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_fertility_differentials = function(filename = "projection_fertility_differentials.csv", folder = "projections") {
      log_print("Saving fertility differentials to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$fertility_differentials)
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving fertility differentials to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (4 + ncol(country_object_list[[1]]$fertility_differentials[[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Year", "Age_group", colnames(country_object_list[[1]]$fertility_differentials[[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 3, 4), ~ as.character(.))) %>%
        mutate(across(c(2, 5:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (period in 1:length(country$fertility_differentials)) {
          country_vector <- rep(country$name, nrow(country$fertility_differentials[[period]]))
          country_code_vector <- rep(country$code, nrow(country$fertility_differentials[[period]]))
          year_vector <- rep(names(country$fertility_differentials)[period], nrow(country$fertility_differentials[[period]]))
          fertility_differentials <- as.data.frame(country$fertility_differentials[[period]])
          rownames(fertility_differentials) <- NULL
          df <- data.frame(Country = country_vector, Country_code = country_code_vector, Year = year_vector, Age_group = age_vector_women_population)
          df <- cbind(df, fertility_differentials)
          csv_list$add(df)
          pb$tick()
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_switching_matrices = function(filename = "projection_switching_matrices.csv", folder = "projections") {
      log_print("Saving switching matrices to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$switching_matrices_list)
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving switching matrices to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      # creating dataframe for saving
      dfcsv <- data.frame(matrix(data = NA, nrow = 0, ncol = (5 + ncol(country_object_list[[1]]$switching_matrices_list[[1]]))))
      colnames(dfcsv) <- c("Country", "Country_code", "Sex", "Age_group", "Origin religion", colnames(country_object_list[[1]]$switching_matrices_list[[1]]))
      dfcsv <- dfcsv %>%
        mutate(across(c(1, 3, 4, 5), ~ as.character(.))) %>%
        mutate(across(c(2, 6:ncol(dfcsv)), ~ as.numeric(.)))
      for (country in country_object_list) {
        for (origin_religion in 1:length(country$switching_matrices_list)) {
          country_vector <- rep(country$name, nrow(country$switching_matrices_list[[origin_religion]]))
          country_code_vector <- rep(country$code, nrow(country$switching_matrices_list[[origin_religion]]))
          religion_vector <- rep(names(country$switching_matrices_list)[origin_religion], nrow(country$switching_matrices_list[[origin_religion]]))
          switching <- as.data.frame(country$switching_matrices_list[[origin_religion]])
          rownames(switching) <- NULL
          df <- data.frame(Country = country_vector, Country_code = country_code_vector, Sex = sex_vector, Age_group = age_vector)
          df <- cbind(df, switching)
          csv_list$add(df)
          pb$tick()
        }
      }
      # saving dataframe to .csv
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_switching_data = function(filename = "projection_switching_data.csv", folder = "projections") {
      log_print("Saving switching data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$previous_switching) * length(country_object_list[[1]]$previous_switching[[1]]) * length(country_object_list[[1]]$previous_switching[[1]][[1]])
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving switching data to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      for (country in country_object_list) {
        for (period in 1:length(country$previous_switching)) {
          for (structure in 1:length(country$previous_switching[[period]])) {
            for (origin_rel in 1:length(country$previous_switching[[period]][[structure]])) {
              country_vector <- rep(country$name, nrow(country$previous_switching[[period]][[structure]][[origin_rel]]))
              country_code_vector <- rep(country$code, nrow(country$previous_switching[[period]][[structure]][[origin_rel]]))
              period_vector <- rep(names(country$previous_switching)[period], nrow(country$previous_switching[[period]][[structure]][[origin_rel]]))
              origin_rel_vector <- rep(names(country$previous_switching[[period]][[structure]])[origin_rel], nrow(country$previous_switching[[period]][[structure]][[origin_rel]]))
              structure_vector <- rep(names(country$previous_switching[[period]])[structure], nrow(country$previous_switching[[period]][[structure]][[origin_rel]]))
              switching <- as.data.frame(country$previous_switching[[period]][[structure]][[origin_rel]])
              rownames(switching) <- NULL
              df <- data.frame(Country = country_vector, Country_code = country_code_vector, Period = period_vector, Sex = sex_vector, Age_group = age_vector, Origin_religion = origin_rel_vector, Structure = structure_vector)
              df <- cbind(df, switching)
              csv_list$add(df)
              pb$tick()
            }
          }
        }
      }
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_migration_data = function(filename = "projection_migration_data.csv", folder = "projections") {
      log_print("Saving migration data to csv", console = FALSE)
      matrices <- length(country_object_list) * length(country_object_list[[1]]$previous_migration) * length(country_object_list[[1]]$previous_migration[[1]]) * length(country_object_list[[1]]$previous_migration[[1]][[1]])
      csv_list <- save_csv_startup(folder)
      pb <- progress_bar$new(
        format = paste(
          "Saving migration data to .csv [:bar] :percent Estimated time left: :eta Total matrices: ",
          as.character(matrices),
          sep = ""),
        total = matrices, clear = FALSE, width = 120
      )
      for (country in country_object_list) {
        for (period in 1:length(country$previous_migration)) {
          for (structure in 1:length(country$previous_migration[[period]])) {
            for (destination in 1:length(country$previous_migration[[period]][[structure]])) {
              if (!is.matrix(country$previous_migration[[period]][[structure]][[destination]])) {
                pb$tick()
                next
              }
              country_vector <- rep(country$name, nrow(country$previous_migration[[period]][[structure]][[destination]]))
              country_code_vector <- rep(country$code, nrow(country$previous_migration[[period]][[structure]][[destination]]))
              period_vector <- rep(names(country$previous_migration)[period], nrow(country$previous_migration[[period]][[structure]][[destination]]))
              dest_vector <- rep(as.character(destination), nrow(country$previous_migration[[period]][[structure]][[destination]]))
              structure_vector <- rep(names(country$previous_migration[[period]])[structure], nrow(country$previous_migration[[period]][[structure]][[destination]]))
              migration <- as.data.frame(country$previous_migration[[period]][[structure]][[destination]])
              rownames(migration) <- NULL
              df <- data.frame(Origin = country_vector, Country_code = country_code_vector, Period = period_vector, Sex = sex_vector, Age_group = age_vector, Destination = dest_vector, Structure = structure_vector)
              df <- cbind(df, migration)
              csv_list$add(df)
            }
          }
        }
      }
      dfcsv <- rbind.fill(csv_list$as.list())
      write.csv(dfcsv, paste(folder, filename, sep="/"), row.names = FALSE)
      log_print("Data saved", console = FALSE)
    },

    save_csv_all = function(filenames_vector, folder, switching = FALSE, migration = FALSE) {
      save_csv_structure(filenames_vector[1], folder)
      save_csv_fertility(filenames_vector[2], folder)
      save_csv_mortality(filenames_vector[3], folder)
      save_csv_asfr(filenames_vector[4], folder)
      save_csv_women_population(filenames_vector[5], folder)
      save_csv_fertility_differentials(filenames_vector[6], folder)
      if(switching){
        save_csv_switching_matrices(filenames_vector[7], folder)
        save_csv_switching_data(filenames_vector[8], folder)
      }
      if(migration){
        save_csv_migration_data(filenames_vector[9], folder)
      }
    }


  )
)
