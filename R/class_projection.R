#' @include class_migration.R

#' @exportClass Country_object
Country_object$methods(

  dem_proj_no_mig = function(switching = T, switching_type = 1, no_diff = F) {
    add_next_structure()
    if (switching){
      if(switching_type == 1){
        temp[["switching second"]] <<- structure_data[[length(structure_data)]] / 2
        structure_data[[length(structure_data)]] <<- structure_data[[length(structure_data)]] / 2
        switching_step()
        mortality_step()
        fertility_step(no_diff = no_diff)

        mortality_step(target = "switching second")
        fertility_step(target = "switching second", no_diff = no_diff)
        switching_step(target = "switching second")
        structure_data[[length(structure_data)]] <<- structure_data[[length(structure_data)]] + temp[["switching second"]]
      } else {
        switching_step()
        mortality_step()
        fertility_step(no_diff = no_diff)
      }
      calculate_total_previous(2)
    } else {
      mortality_step()
      fertility_step(no_diff = no_diff)
      calculate_total_previous(1)
    }

    year <<- year+5
    names(structure_data)[[length(structure_data)]] <<- as.character(year)
  },

  dem_proj_migration = function(step, switching = T, switching_type = 1, no_diff = F){
    if (step == 1) {
      add_next_structure()
      separate_migrants()
      destination_processed_migrants()
    }
    if (step == 2) {
      find_migrants()

      if (switching){
        if(switching_type == 1){
          temp[["switching second"]] <<- structure_data[[length(structure_data)]] / 2
          structure_data[[length(structure_data)]] <<- structure_data[[length(structure_data)]] / 2
          switching_step()
          mortality_step()
          fertility_step(no_diff = no_diff)

          mortality_step(target = "switching second")
          fertility_step(target = "switching second", no_diff = no_diff)
          switching_step(target = "switching second")
          structure_data[[length(structure_data)]] <<- structure_data[[length(structure_data)]] + temp[["switching second"]]
        } else {
          switching_step()
          mortality_step()
          fertility_step(no_diff = no_diff)
        }
      } else {
        mortality_step()
        fertility_step(no_diff = no_diff)
      }

      origin_processed_migrants(switching, switching_type, no_diff)
    }
    if (step == 3) {
      find_migrants()
      year <<- year + 5
      names(structure_data)[[length(structure_data)]] <<- as.character(year)
      calculate_total_previous(3)
    }
  }

)
