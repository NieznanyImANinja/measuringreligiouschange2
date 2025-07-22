#' @include Loader.R


cap_adjust <- function (vector, bottom_cap, upper_cap, in_thousands) {
  if (is.numeric(bottom_cap)){
    if (in_thousands){
      if (sum(vector) * 1000 < bottom_cap)
        vector <- vector * bottom_cap / sum(vector) * 1000
    } else {
      if (sum(vector) < bottom_cap)
        vector <- vector * bottom_cap / sum(vector)
    }
  }

  if (is.numeric(upper_cap)){
    if (in_thousands){
      if (sum(vector) * 1000 > upper_cap)
        vector <- vector * upper_cap / sum(vector) * 1000
    } else {
      if (sum(vector) > upper_cap)
        vector <- vector * upper_cap / sum(vector)
    }
  }
  return(vector)
}

percentage_fertility_change <- function(asfr, rate = 1, bottom_cap = NA, upper_cap = NA) {
    in_thousands <- F

    if (sum(asfr[1, ]) > 50)
      in_thousands <- T


    for (period in 1:nrow(asfr)) {
      asfr[period, ] <- asfr[period, ] * rate
      asfr[period, ] <- cap_adjust(asfr[period, ], bottom_cap, upper_cap, in_thousands)
    }
    return(asfr)
}

flat_fertility_change <- function(asfr, change = 0, bottom_cap = NA, upper_cap = NA) {

    in_thousands <- F

    if (sum(asfr[1, ]) > 50)
      in_thousands <- T


    for (period in 1:nrow(asfr)) {


      if(in_thousands){
        asfr[period, ] <- asfr[period, ] * (sum(asfr[period, ]) + change * 1000) / sum(asfr[period, ])
      } else {
        asfr[period, ] <- asfr[period, ] * (sum(asfr[period, ]) + change) / sum(asfr[period, ])
      }

      asfr[period, ] <- cap_adjust(asfr[period, ], bottom_cap, upper_cap, in_thousands)

      return(asfr)

    }

}

percentage_migration_change <- function(mig_rates, rate = 1) {
    for (period in mig_rates){
      period <- period * rate
    }
  return(mig_rates)
}



convergence = function(fdlist, steps = 0, object_year) {

  if (steps == 0){

  } else {
    year <- object_year + 5
    fd <- fdlist[[1]]

    for (i in 1:steps){
      fd <- ((fd - 1) * (steps - i) / (steps - i + 1)) + 1
      fdlist <- append(fdlist, list("filler" = fd))
      names(fdlist)[length(fdlist)] <- paste(as.character(year), as.character(year + 5), sep = "-")
      year <- year + 5
    }
  }
  return(fdlist)
}



#local_parameters <- read_excel(paste(dataset, "/country_codes_fix.xlsx", sep = ""))

#####
#log_print("Local starting parameters:", blank_after = T, hide_notes = T)
#log_print(local_parameters, hide_notes = T)
#####

#for (object in projection_base$country_object_list){
#  ccrow <- local_parameters[local_parameters$Country_code == object$code, ]
#  fertility_rate <- pull(ccrow, 4)
#  fertility_flat <- pull(ccrow, 5)
#  bottom_cap <- pull(ccrow, 6)
#  upper_cap <- pull(ccrow, 7)
#  migration_rate <- pull(ccrow, 8)
#  convergence_steps <- pull(ccrow, 9)
#  fert_fact <- pull(ccrow, 10)

#  object$age_specific_fertility_rates <- percentage_fertility_change(object$age_specific_fertility_rates, fertility_rate, bottom_cap, upper_cap)
#  object$age_specific_fertility_rates <- flat_fertility_change(object$age_specific_fertility_rates, fertility_flat, bottom_cap, upper_cap)
#  object$outmigration_rates <- percentage_migration_change(object$outmigration_rates, migration_rate)
#  object$fertility_differentials <- convergence(object$fertility_differentials, convergence_steps)
#  object$fertility_factorization <- fert_fact
#}

#' @export variants_loader
variants_loader <- function(object_list, directory_name, file_name) {
  country_info <- read_excel(paste(directory_name, "/",
                                   file_name, sep = ""))
  for (object in object_list) {
    country_code <- object$code
    country_single <- country_info[country_info[[2]] == country_code, ]
    fertility_rate <- pull(country_single, 4)
    fertility_flat <- pull(country_single, 5)
    bottom_cap <- pull(country_single, 6)
    upper_cap <- pull(country_single, 7)
    migration_rate <- pull(country_single, 8)
    convergence_steps <- pull(country_single, 9)
    fert_fact <- pull(country_single, 10)

    object$age_specific_fertility_rates <- percentage_fertility_change(object$age_specific_fertility_rates,
                                                                       fertility_rate,
                                                                       bottom_cap,
                                                                       upper_cap)
    object$age_specific_fertility_rates <- flat_fertility_change(object$age_specific_fertility_rates,
                                                                 fertility_flat,
                                                                 bottom_cap,
                                                                 upper_cap)
    object$outmigration_rates <- percentage_migration_change(object$outmigration_rates,
                                                             migration_rate)
    object$fertility_differentials <- convergence(object$fertility_differentials,
                                                  convergence_steps, object$year)
    object$fertility_factorization <- fert_fact
  }
}



#rm(list = c("ccrow", "local_parameters"))
