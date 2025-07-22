
#labels for mortality
rnames_mortality <- c()
for (sex in c("males", "females")) {
  for (age in c(0, 1, seq(from = 5, to = 95, by = 5))) {
    rnames_mortality <- c(rnames_mortality,
                          paste(sex, as.character(age), sep = " "))
  }
}

Country_object <- setRefClass("Country",
  fields = list(
    #Information fields
    year = "numeric",
    period = "character",
    name = "character",
    code = "numeric",
    region = "character",
    structure_data = "list",
    #Input data
    mortality_rates = "matrix",
    age_specific_fertility_rates = "matrix",
    fertility_differentials = "list",
    sex_ratio = "numeric",
    switching_matrices_list = "list",
    outmigration_rates = "list",
    destination_matrix = "matrix",
    start_fertility_group = "numeric",
    post_switch = "logical",
    fertility_factorization = "numeric",
    #Output data
    previous_mortality = "list",
    previous_fertility = "list",
    previous_asfr = "list",
    previous_women_population = "list",
    old_fertility_factors = "matrix",
    previous_switching = "list",
    previous_migration = "list",
    migration_list = "list",
    emigration_list = "list",
    migrant_sum = "matrix",
    #Technical
    temp = "list"
    )
)
Country_object$methods(
  list(
    #Functions used in dem_proj

    add_next_structure = function(){
      structure_data <<- append(structure_data,
                                list(structure_data[[length(structure_data)]]))
      names(structure_data)[[length(structure_data)]] <<- as.character(year + 5)
      update_period()
    },

    mortality_step = function(target = "sd"){
      ################
      #choosing target structure + calculating surviving/dead population
      mrates <- mortality_rates[c(2:21, 23:42), length(structure_data) - 1]
      if (target == "sd") {
        population_after_mortality <-  structure_data[[length(structure_data)]] * mrates
        mortality_data <- structure_data[[length(structure_data)]] - population_after_mortality
      } else if (target %in% names(emigration_list)) {
        population_after_mortality <- emigration_list[[target]] * mrates
        mortality_data <- emigration_list[[target]]
      } else {
        population_after_mortality <- temp[[target]] * mrates
        mortality_data <- temp[[target]] - population_after_mortality
      }

      ################
      #preparing for saving
      aftermort_males <- population_after_mortality[1:20, ]
      aftermort_males[19, ] <- aftermort_males[19, ] + aftermort_males[20, ]
      aftermort_males <- aftermort_males[1:19, ]
      rownames(aftermort_males) <- rownames(structure_data[[1]])[2:20]
      aftermort_males <- rbind(matrix(data = rep(0, ncol(aftermort_males)),
                                      nrow = 1,
                                      ncol = ncol(aftermort_males),
                                      dimnames = list(c(rownames(structure_data[[1]])[1]),
                                                      colnames(structure_data[[1]]))),
                               aftermort_males)

      aftermort_females <- population_after_mortality[21:40, ]
      aftermort_females[19, ] <- aftermort_females[19, ] + aftermort_females[20, ]
      aftermort_females <- aftermort_females[1:19, ]
      rownames(aftermort_females) <- rownames(structure_data[[1]])[22:40]
      aftermort_females <- rbind(matrix(data = rep(0, ncol(aftermort_males)),
                                        nrow = 1,
                                        ncol = ncol(aftermort_females),
                                        dimnames = list(c(rownames(structure_data[[1]])[21]),
                                                        colnames(structure_data[[1]]))),
                                 aftermort_females)
      population_after_mortality <- rbind(aftermort_males, aftermort_females)

      #previous_mortality <<- append(previous_mortality, data.matrix(mortality_data))
      ################
      #saving

      if (length(previous_mortality) == 0) {
        previous_mortality <<- list(list(mortality_data))
        names(previous_mortality)[[length(previous_mortality)]] <<- period
      } else if (length(previous_mortality) == length(structure_data) - 2) {
        previous_mortality <<- append(previous_mortality,
                                      list(list("filler" = mortality_data)))
        names(previous_mortality)[[length(previous_mortality)]] <<- period
      } else {
        previous_mortality[[length(previous_mortality)]] <<- append(previous_mortality[[length(previous_mortality)]],
                                                                    list("filler" = mortality_data))
      }


      if (target == "sd") {
        temp[["prev"]] <<- structure_data[[length(structure_data)]]
        names(previous_mortality[[length(previous_mortality)]])[length(previous_mortality[[length(previous_mortality)]])] <<- "switching first"
        structure_data[[length(structure_data)]] <<- population_after_mortality

      } else if (target %in% names(emigration_list)) {
        temp[["prev"]] <<- emigration_list[[target]]
        emigration_list[[target]] <<- population_after_mortality
        names(previous_mortality[[length(previous_mortality)]])[length(previous_mortality[[length(previous_mortality)]])] <<- target
      } else {
        temp[["prev"]] <<- temp[[target]]
        names(previous_mortality[[length(previous_mortality)]])[length(previous_mortality[[length(previous_mortality)]])] <<- target
        temp[[target]] <<- population_after_mortality
      }
      ################
    },

    fertility_step = function(target = "sd", no_diff = F) {
      ################
      #choosing correct structure
      if (target == "sd"){
        sd <- (structure_data[[length(structure_data)]] + temp[["prev"]]) / 2
      } else if (target %in% names(emigration_list)){
        sd <- (emigration_list[[target]] + temp[["prev"]]) / 2
      } else {
        sd <- (temp[[target]] + temp[["prev"]]) / 2
      }


      ################
      #choosing asfr
      period_specific_asfr <- age_specific_fertility_rates[length(structure_data) - 1, ]

      if (!is.matrix(fertility_differentials[[length(fertility_differentials)]])) {
        #print_color("Wrong differential input - not a matrix!", "red")
        log_print("Wrong differential input - not a matrix!")
        return(NULL)
      }

      if (period_specific_asfr[3] > 5)
        period_specific_asfr <- period_specific_asfr / 1000

      if(length(fertility_differentials)>=length(structure_data)){
        fd <- fertility_differentials[[length(structure_data) - 1]]
      } else {
        fd <- fertility_differentials[[length(fertility_differentials)]]
      }


      ################
      #checks
      if (nrow(fd) == 0) {
          fd <- matrix(data = 1, nrow = ncol(age_specific_fertility_rates),
                       ncol = ncol(sd))
          colnames(fd) <- colnames(sd)
        alldifferentials <- T
      } else if ((nrow(fd) > 1 & ncol(fd) == ncol(sd) & !post_switch) |
                 (nrow(fd) > 1 & 2 * ncol(fd) == ncol(sd) & post_switch)){
        alldifferentials <- T
      } else {
        alldifferentials <- F
      }
      if (alldifferentials) {
        if (post_switch) {
          if (ncol(sd) != 2 * ncol(fd)) {
            #print_color("Wrong differencial data.frame - religions", "red")
            log_print("Wrong differencial data.frame - religions")
            return(NULL)
          } else {
            fd <- cbind(fd, fd)
            colnames(fd) <- colnames(sd)
          }
        } else {
          if (ncol(sd) != ncol(fd)) {
            #print_color("Wrong differencial data.frame - religions", "red")
            log_print("Wrong differencial data.frame - religions")
            return(NULL)
          }
        }
      }
        if (length(period_specific_asfr) != nrow(fd)) {
          #print_color("Wrong differencial data.frame - age groups", "red")
          log_print("Wrong differencial data.frame - age groups")
          return(NULL)
      }
      ################
      #structure preparation
      sd <- sd[(nrow(sd) / 2 + 1):nrow(sd), ]
      sd <- sd[start_fertility_group:(length(period_specific_asfr) + start_fertility_group - 1), ]

      if (no_diff) {
        relasfr <- matrix(data = rep(period_specific_asfr, ncol(sd)),
                          nrow = length(period_specific_asfr), ncol = ncol(sd))
      } else {
        relasfr <- fd * period_specific_asfr

      }

      ################
      #expected values
      total_in_groups <- rowSums(sd)
      expected_age_groups_totals <- c()
      expected_age_groups_totals <- total_in_groups * period_specific_asfr
      ################
      #non factorized values
      raw_age_groups_totals <- c()
      raw_age_groups <- sd * relasfr
      raw_age_groups_totals <- rowSums(raw_age_groups)

      ################
      #final calculations

      if(fertility_factorization == 1){
        factor <- sum(expected_age_groups_totals) / sum(raw_age_groups_totals)

        children_df <- raw_age_groups * factor
      } else {
        factors_vector <- expected_age_groups_totals / raw_age_groups_totals

        factors_vector[is.nan(factors_vector)] <- 0

        children_df <- raw_age_groups * factors_vector
      }

      total_vector <- colSums(children_df)

      fraction_of_born_boys <- sex_ratio[length(structure_data) - 1]

      male_vector <- total_vector * fraction_of_born_boys
      female_vector <- total_vector * (1 - fraction_of_born_boys)
      nr <- nrow(structure_data[[length(structure_data)]])
      nc <- ncol(structure_data[[length(structure_data)]])
      rown <- rownames(structure_data[[length(structure_data)]])
      result <- matrix(data = c(male_vector, female_vector),
                       ncol = nc, nrow = 2, byrow = T) * mortality_rates[c(1, 22), length(structure_data) - 1]
      ################
      #"unswitching" children
      if (post_switch) {
        result[, 1:(nc / 2)] <- result[, 1:(nc / 2)] + result[, (ncol(result) / 2 + 1):ncol(result)]
        result[, (ncol(result) / 2 + 1):ncol(result)] <- 0
      }

      ################
      #preparing for saving
      result <- data.frame(result)
      colnames(result) <- colnames(structure_data[[length(structure_data)]])
      if (target == "sd") {
        result <- data.matrix(rbind(result[1, ], structure_data[[length(structure_data)]][2:(nr / 2), ], result[2, ], structure_data[[length(structure_data)]][(nr / 2 + 2):nr, ]))
      } else {
        result <- data.matrix(rbind(result[1, ], temp[[target]][2:(nr / 2), ],
                                    result[2, ], temp[[target]][(nr / 2 + 2):nr, ]))
      }

      rownames(children_df) <- rownames(sd)
      colnames(children_df) <- colnames(sd)
      rownames(result) <- rown

      #previous_fertility <<- append(previous_fertility, data.matrix(children_df))
      if(fertility_factorization == 1){
        relasfr <- relasfr * factor
      } else {
        relasfr <- relasfr * factors_vector
      }

      tfr <- matrix(colSums(relasfr), nrow = 1)
      colnames(tfr) <- colnames(relasfr)
      rownames(tfr) <- c("TFR")
      relasfr <- rbind(relasfr, tfr)

      ################
      #calculating children mortality
      newborn_mortality <- matrix(data = c(male_vector, female_vector),
                             ncol = ncol(sd),
                             nrow = 2, byrow = T) * (1 -  mortality_rates[c(1, 22), length(structure_data) - 1])
      if (post_switch) {
        newborn_mortality[, 1:(ncol(result) / 2)] <- newborn_mortality[, 1:(ncol(result) / 2)] + newborn_mortality[, (ncol(result) / 2 + 1):ncol(result)]
        newborn_mortality[, (ncol(result) / 2 + 1):ncol(result)] <- 0
      }

      ################
      #saving calculations

      if (length(previous_fertility) == 0) {
        previous_fertility <<- list(list(children_df))
        names(previous_fertility)[[length(previous_fertility)]] <<- period
      } else if (length(previous_fertility) == length(structure_data) - 2) {
        previous_fertility <<- append(previous_fertility,
                                      list(list("filler" = children_df)))
        names(previous_fertility)[[length(previous_fertility)]] <<- period
      } else {
        previous_fertility[[length(previous_fertility)]] <<- append(previous_fertility[[length(previous_fertility)]], list("filler" = children_df))
      }




      if (length(previous_asfr) == 0) {
        previous_asfr <<- list(list(relasfr))
        names(previous_asfr)[[length(previous_asfr)]] <<- period
      } else if (length(previous_asfr) == length(structure_data) - 2) {
        previous_asfr <<- append(previous_asfr, list(list("filler" = relasfr)))
        names(previous_asfr)[[length(previous_asfr)]] <<- period
      } else {
        previous_asfr[[length(previous_asfr)]] <<- append(previous_asfr[[length(previous_asfr)]],
                                                          list("filler" = relasfr))
      }


      if (length(previous_women_population) == 0) {
        previous_women_population <<- list(list(sd))
        names(previous_women_population)[[length(previous_women_population)]] <<- period
      } else if (length(previous_women_population) == length(structure_data) - 2) {
        previous_women_population <<- append(previous_women_population,
                                             list(list("filler" = sd)))
        names(previous_women_population)[[length(previous_women_population)]] <<- period
      } else {
        previous_women_population[[length(previous_women_population)]] <<- append(previous_women_population[[length(previous_women_population)]], list("filler" = sd))
      }



      if (target == "sd") {
        names(previous_fertility[[length(previous_fertility)]])[length(previous_fertility[[length(previous_fertility)]])] <<- "switching first"
        names(previous_asfr[[length(previous_asfr)]])[length(previous_asfr[[length(previous_asfr)]])] <<- "switching first"
        names(previous_women_population[[length(previous_women_population)]])[length(previous_women_population[[length(previous_women_population)]])] <<- "switching first"

        pm <- previous_mortality[[length(previous_mortality)]][["switching first"]]
        pm <- rbind(newborn_mortality[1, ], pm[1:20, ], newborn_mortality[2, ], pm[21:40, ])
        rownames(pm) <- rnames_mortality
        previous_mortality[[length(previous_mortality)]][["switching first"]] <<- pm

        structure_data[[length(structure_data)]] <<- data.matrix(result)

      } else if (target %in% names(emigration_list)){
        names(previous_fertility[[length(previous_fertility)]])[length(previous_fertility[[length(previous_fertility)]])] <<- target
        names(previous_asfr[[length(previous_asfr)]])[length(previous_asfr[[length(previous_asfr)]])] <<- target
        names(previous_women_population[[length(previous_women_population)]])[length(previous_women_population[[length(previous_women_population)]])] <<- target

        pm <- previous_mortality[[length(previous_mortality)]][[target]]
        pm <- rbind(newborn_mortality[1, ], pm[1:20, ],
                    newborn_mortality[2, ], pm[21:40, ])
        rownames(pm) <- rnames_mortality
        previous_mortality[[length(previous_mortality)]][[target]] <<- pm
      } else {
        names(previous_fertility[[length(previous_fertility)]])[length(previous_fertility[[length(previous_fertility)]])] <<- target
        names(previous_asfr[[length(previous_asfr)]])[length(previous_asfr[[length(previous_asfr)]])] <<- target
        names(previous_women_population[[length(previous_women_population)]])[length(previous_women_population[[length(previous_women_population)]])] <<- target

        pm <- previous_mortality[[length(previous_mortality)]][[target]]
        pm <- rbind(newborn_mortality[1, ], pm[1:20, ],
                    newborn_mortality[2, ], pm[21:40, ])
        rownames(pm) <- rnames_mortality
        previous_mortality[[length(previous_mortality)]][[target]] <<- pm
        temp[[target]] <<- data.matrix(result)
      }
      ################
    },

    switching_step = function(target = "sd") {

      ################
      #check for switching data
      if (length(switching_matrices_list) == 0) {
        #print_color("No switching data!", "red")
        log_print("No switching data!")
        return(NULL)
      }

      ################
      #choose correct structure
      if(target == "sd") {
        sd <- structure_data[[length(structure_data)]]
      } else if (target %in% names(emigration_list)) {
        sd <- emigration_list[[target]]
      } else {
        sd <- temp[[target]]
      }

      ################
      #calculations for religions separately
      prediction_religions_list <- list()
      for (i in 1:length(switching_matrices_list)) {
        prediction_religions_list[[i]] <- switching_matrices_list[[i]] * matrix(data = rep(sd[, i], length(switching_matrices_list)), nrow = length(sd[, i]), ncol = length(switching_matrices_list), byrow = FALSE)
      }

      ################
      #combining into one matrix
      prediction_output <- prediction_religions_list[[1]]
      for (i in 2:length(prediction_religions_list)) {
        prediction_output <- prediction_output + prediction_religions_list[[i]]
      }
      names(prediction_religions_list) <- paste(colnames(sd))

      ################
      #saving calculations
      if(length(previous_switching) == 0) {
        previous_switching <<- list(list(prediction_religions_list))
        names(previous_switching)[[length(previous_switching)]] <<- period
      } else if (length(previous_switching) == length(structure_data) - 2) {
        previous_switching <<- append(previous_switching,
                                      list(list("filler" = prediction_religions_list)))
        names(previous_switching)[[length(previous_switching)]] <<- period
      } else {
        previous_switching[[length(previous_switching)]] <<- append(previous_switching[[length(previous_switching)]], list("filler" = prediction_religions_list))
      }

      if (target == "sd") {
        names(previous_switching[[length(previous_switching)]])[length(previous_switching[[length(previous_switching)]])] <<- "switching first"
        structure_data[[length(structure_data)]] <<- prediction_output

      } else if (target %in% names(emigration_list)) {
        emigration_list[[target]] <<- prediction_output
        names(previous_switching[[length(previous_switching)]])[length(previous_switching[[length(previous_switching)]])] <<- target
      } else {
        names(previous_switching[[length(previous_switching)]])[length(previous_switching[[length(previous_switching)]])] <<- target
        temp[[target]] <<- prediction_output
      }
      ################
    },

    #Test for debugging

    testself = function() {
      print(name)
    }
    )
)
