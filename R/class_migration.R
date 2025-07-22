#' @include class_utilities.R
Country_object$methods(

  separate_migrants = function() {
    if (length(outmigration_rates) >= length(structure_data) - 1) {
      migrant_sum <<- structure_data[[length(structure_data)]] * cbind(outmigration_rates[[length(structure_data) - 1]], outmigration_rates[[length(structure_data) - 1]])
    } else {
      migrant_sum <<- structure_data[[length(structure_data)]] * cbind(outmigration_rates[[length(outmigration_rates)]], outmigration_rates[[length(outmigration_rates)]])
    }
    structure_data[[length(structure_data)]] <<- structure_data[[length(structure_data)]] - migrant_sum
  },

  destination_processed_migrants = function() {
    dmatrix <- destination_matrix
    for (row in 1:nrow(dmatrix))
      if (sum(dmatrix[row, ]) != 0)
        dmatrix[row, ] <- dmatrix[row, ] / sum(dmatrix[row, ])
    e_list <- list()
    for (i in 1:999)
      e_list[[i]] <- 0
    for (col in colnames(dmatrix)) {
      single_dest <- dmatrix[, col]
      if (sum(single_dest!=0)==0)
        next
      single_dest_male <- c(single_dest[1:8], single_dest[1:8])
      single_dest_female <- c(single_dest[9:16], single_dest[9:16])
      mig_male <- t(t(migrant_sum[1:20,] / 2) * single_dest_male)
      mig_female <- t(t(migrant_sum[21:40,] / 2) * single_dest_female)
      e_list[[as.numeric(col)]] <- rbind(mig_male, mig_female)
    }
    emigration_list <<- e_list
    if (length(previous_migration) == 0) {
      previous_migration <<- list(list(destination_processed = e_list))
    } else {
      previous_migration[[length(previous_migration)+1]] <<- list(destination_processed = e_list)
    }
    names(previous_migration)[length(previous_migration)] <<- period
  },

  find_migrants = function() {

  },

  origin_processed_migrants = function(switching = T, switching_type = 1, no_diff = F) {
    dmatrix <- destination_matrix
    for (row in 1:nrow(dmatrix))
      if (sum(dmatrix[row, ]) != 0)
        dmatrix[row, ] <- dmatrix[row, ] / sum(dmatrix[row, ])
    if (switching){
      if(switching_type == 1){

        temp[["sw_first_migrants"]] <<- migrant_sum / 4
        temp[["sw_second_migrants"]] <<- migrant_sum / 4
        switching_step(target = "sw_first_migrants")
        mortality_step(target = "sw_first_migrants")
        fertility_step(target = "sw_first_migrants", no_diff)

        mortality_step(target = "sw_second_migrants")
        fertility_step(target = "sw_second_migrants", no_diff)
        switching_step(target = "sw_second_migrants")

        mig_sum <- temp[["sw_first_migrants"]] + temp[["sw_second_migrants"]]
      } else {
        temp[["migrants"]] <<- migrant_sum / 2
        switching_step(target = "migrants")
        mortality_step(target = "migrants")
        fertility_step(target = "migrants", no_diff = no_diff)
      }
    } else {
      mortality_step(target = "migrants")
      fertility_step(target = "migrants", no_diff = no_diff)
    }

    e_list <- list()
    for (i in 1:999)
      e_list[[i]] <- 0
    for (col in colnames(dmatrix)) {
      single_dest <- dmatrix[, col]
      if (sum(single_dest!=0) == 0)
        next
      single_dest_male <- c(single_dest[1:8], single_dest[1:8])
      single_dest_female <- c(single_dest[9:16], single_dest[9:16])
      mig_male <- t(t(mig_sum[1:20, ]) * single_dest_male)
      mig_female <- t(t(mig_sum[21:40, ]) * single_dest_female)
      e_list[[as.numeric(col)]] <- rbind(mig_male, mig_female)
    }
    emigration_list <<- e_list

    previous_migration[[length(previous_migration)]] <<- append(previous_migration[[length(previous_migration)]], list(origin_processed = e_list))

  }
)
