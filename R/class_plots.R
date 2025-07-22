#' @include class_projection.R
#' @exportClass Country_object
Country_object$methods(

  plot = function(variant, period = 0, post_switch, switching_religion = 0,
                  plot_colors, plot_switching_colors) {

    if (ncol(structure_data[[1]]) != length(plot_switching_colors) &&
        post_switch == T) {
      log_print("Wrong length of plot_switching_colors",
                blank_after = T, hide_notes = T)
      return()
    } else if ((ncol(structure_data[[1]]) / 2) != length(plot_colors) &&
               post_switch == F) {
      log_print("Wrong length of plot_colors",
                blank_after = T, hide_notes = T)
      return()
    }

    if (variant == 1) {
      df <- structure_data
      population_vector <- c()
      for (i in 1:length(df)) {
        population_vector <- c(population_vector, sum(df[[i]]))
      }
      years <- names(df)
      plot_data_frame <- data.frame(year = years,
                                    population = population_vector)
      plot <- ggplot(plot_data_frame, aes(x = year,
                                          y = population,
                                          group = 1)) +
        geom_line(size = 1) +
        labs(x = "Year",
             y = "Population",
             title = paste("Population of", name, "from",
                           as.character(names(structure_data)[[1]]), "to",
                           as.character(names(structure_data)[[length(structure_data)]]))) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (variant == 2) {
      if (post_switch) {
        df <- structure_data
        population_vector <- c()
        for (i in 1:length(df)) {
          population_vector <- c(population_vector, colSums(df[[i]]))
        }
        years <- rep(names(df), each = ncol(df[[1]]))
        religions <- rep(colnames(df[[1]]), length(df))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      population = population_vector)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = population,
                                            group = religion,
                                            color = religion)) +
          geom_line(size = 1) +
          scale_color_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "Population",
               color = "Religion",
               title = paste("Population of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]),
                             "with religions")) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- structure_data
        population_vector <- c()
        for (i in 1:length(df)) {
          df_sum <- df[[i]][, 1:(ncol(df[[i]]) / 2)] + df[[i]][, (ncol(df[[i]]) / 2 + 1):ncol(df[[i]])]
          population_vector <- c(population_vector, colSums(df_sum))
        }
        years <- rep(names(df), each = ncol(df[[1]][, 1:(ncol(df[[i]]) / 2)]))
        religions <- rep(colnames(df[[1]][, 1:(ncol(df[[i]]) / 2)]), length(df))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      population = population_vector)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = population,
                                            group = religion,
                                            color = religion)) +
          geom_line(size = 1) +
          scale_color_manual(values = plot_colors) +
          labs(x = "Year",
               y = "Population",
               color = "Religion",
               title = paste("Population of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]),
                             "with religions")) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 3) {
      if (post_switch) {
        df <- structure_data
        population_vector <- c()
        for (i in 1:length(df)) {
          population_vector <- c(population_vector, colSums(df[[i]]))
        }
        years <- rep(names(df), each = ncol(df[[1]]))
        religions <- rep(colnames(df[[1]]), length(df))
        plot_data_frame <- data.frame(year = years, religion = religions,
                                      population = population_vector)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = population,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religion",
               title = paste("Population structure of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- structure_data
        population_vector <- c()
        for (i in 1:length(df)) {
          df_sum <- df[[i]][, 1:(ncol(df[[i]]) / 2)] + df[[i]][, (ncol(df[[i]]) / 2 + 1):ncol(df[[i]])]
          population_vector <- c(population_vector, colSums(df_sum))
        }
        years <- rep(names(df), each = ncol(df[[1]][, 1:(ncol(df[[i]]) / 2)]))
        religions <- rep(colnames(df[[1]][, 1:(ncol(df[[i]]) / 2)]), length(df))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      population = population_vector)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = population,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religion",
               title = paste("Population structure of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 4) {

      df_temp <- structure_data
      rows <- c()

      for (year in 1:length(structure_data)) {
        rows <- c(rows, rowSums(structure_data[[year]]))
      }

      max_population <- max(rows) * 1.1


      if (post_switch) {
        df <- structure_data[[period]]
        total_population <- c()
        sex <- rep(c(rep("males", 20), rep("females", 20)), ncol(df))
        age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                        "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                        "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                        "90-94", "95+")
        age <- c(rep(1:20, (2 * ncol(df))))
        religions <- rep(colnames(df), each = nrow(df))
        for (i in 1:ncol(df)) {
          total_population <- c(total_population, df[, i])
        }
        population <- data.frame(age_group = factor(age),
                                 sex = sex,
                                 religion = religions,
                                 population = total_population)
        population[, 3] <- factor(population[, 3],
                                  levels = unique(population[, 3]))
        plot <- ggplot(population, aes(x = age_group,
                                       y = ifelse(test = sex == "males",
                                                  yes = -population,
                                                  no = population),
                                       fill = religion)) +
          geom_bar(stat = "identity") +
          geom_hline(yintercept = 0) +
          scale_y_continuous(labels = abs, limits = c(-1 * max_population,
                                                      max_population)) +
          scale_x_discrete(labels = age_groups) +
          coord_flip() +
          scale_fill_manual(values = plot_switching_colors) +
          labs(x = "Age",
               y = "Population",
               fill = "Religion",
               title = paste("Population pyramid of", name, "for",
                             as.character(names(structure_data)[period]),
                             "with religions")) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- structure_data[[period]][, 1:(ncol(structure_data[[period]]) / 2)] + structure_data[[period]][, (ncol(structure_data[[period]]) / 2 + 1):ncol(structure_data[[period]])]
        colnames(df) <- colnames(structure_data[[period]])[1:(ncol(structure_data[[period]]) / 2)]
        row_population <- rowSums(df)
        total_population <- c()
        sex <- rep(c(rep("males", 20), rep("females", 20)), ncol(df))
        age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                        "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                        "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                        "90-94", "95+")
        age <- c(rep(1:20, (2 * ncol(df))))
        religions <- rep(colnames(df), each = nrow(df))
        for (i in 1:ncol(df)) {
          total_population <- c(total_population, df[, i])
        }
        population <- data.frame(age_group = factor(age),
                                 sex = sex,
                                 religion = religions,
                                 population = total_population)
        population[, 3] <- factor(population[, 3],
                                  levels = unique(population[, 3]))
        plot <- ggplot(population, aes(x = age_group,
                                       y = ifelse(test = sex == "males",
                                                  yes = -population,
                                                  no = population),
                                       fill = religion)) +
          geom_bar(stat = "identity") +
          geom_hline(yintercept = 0) +
          scale_y_continuous(labels = abs, limits = c(-1 * max_population,
                                                      max_population)) +
          scale_x_discrete(labels = age_groups) +
          coord_flip() +
          scale_fill_manual(values = plot_colors) +
          labs(x = "Age",
               y = "Population",
               fill = "Religion",
               title = paste("Population pyramid of", name, "for",
                             as.character(names(structure_data)[period]),
                             "with religions")) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      return(plot)
    } else if (variant == 5) {
      if (post_switch) {
        df <- previous_asfr
        tfr <- c()
        for (i in 1:length(df)) {
          tfr <- c(tfr, df[[i]][[1]][nrow(df[[i]][[1]]), ])
        }
        religions <- rep(colnames(df[[i]][[1]]), length(df))
        years <- rep(names(df), each = ncol(df[[i]][[1]]))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      tfr = tfr)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = tfr,
                                            group = religion,
                                            color = religion)) +
          geom_line(size = 1) +
          scale_color_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "TFR",
               color = "Religion",
               title = paste("TFR by religions of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- previous_asfr
        tfr <- c()
        for (i in 1:length(df)) {
          tfr <- c(tfr, df[[i]][[1]][nrow(df[[i]][[1]]), 1:(ncol(df[[i]][[1]]) / 2)])
        }
        religions <- rep(colnames(df[[i]][[1]])[1:(ncol(df[[i]][[1]]) / 2)],
                         length(df))
        years <- rep(names(df),
                     each = ncol(df[[i]][[1]][, 1:(ncol(df[[i]][[1]]) / 2)]))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      tfr = tfr)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = tfr,
                                            group = religion,
                                            color = religion)) +
          geom_line(size = 1) +
          scale_color_manual(values = plot_colors) +
          labs(x = "Year",
               y = "TFR",
               color = "Religion",
               title = paste("TFR by religions of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 6) {
      if (post_switch) {
        df <- previous_asfr[[period]][[1]]
        asfr <- c()
        for (i in 1:(nrow(df) - 1)) {
          asfr <- c(asfr, df[i, ])
        }
        religions <- rep(colnames(df), (nrow(df) - 1))
        age <- rep(rownames(df)[1:(nrow(df) - 1)], each = ncol(df))
        plot_data_frame <- data.frame(age = age,
                                      religion = religions,
                                      asfr = asfr)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = age,
                                            y = asfr,
                                            group = religion,
                                            color = religion)) +
          geom_line(size = 1) +
          scale_color_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "ASFR",
               color = "Religion",
               title = paste("ASFR by religions and age groups of", name, "in",
                             as.character(names(structure_data)[period]))) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- previous_asfr[[period]][[1]][, 1:(ncol(previous_asfr[[period]][[1]]) / 2)]
        asfr <- c()
        for (i in 1:(nrow(df) - 1)) {
          asfr <- c(asfr, df[i, ])
        }
        religions <- rep(colnames(df), (nrow(df) - 1))
        age <- rep(rownames(df)[1:(nrow(df) - 1)], each = ncol(df))
        plot_data_frame <- data.frame(age = age,
                                      religion = religions,
                                      asfr = asfr)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = age,
                                            y = asfr,
                                            group = religion,
                                            color = religion)) +
          geom_line(size = 1) +
          scale_color_manual(values = plot_colors) +
          labs(x = "Year",
               y = "ASFR",
               color = "Religion",
               title = paste("ASFR by religions and age groups of", name, "in",
                             as.character(names(structure_data)[period]))) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 7) {
      if (post_switch) {
        df <- previous_fertility
        births <- c()
        for (i in 1:length(df)) {
          births <- c(births, colSums(df[[i]][[1]]))
        }
        religions <- rep((colnames(df[[i]][[1]])), length(df))
        years <- rep(names(df), each = (ncol(df[[i]][[1]])))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      births = births)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = births,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religion",
               title = paste("Structure of births of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- previous_fertility
        births <- c()
        for (i in 1:length(df)) {
          df_sum <- df[[i]][[1]][, 1:(ncol(df[[i]][[1]]) / 2)] + df[[i]][[1]][, (ncol(df[[i]][[1]]) / 2 + 1):ncol(df[[i]][[1]])]
          births <- c(births, colSums(df_sum))
        }
        religions <- rep((colnames(df[[i]][[1]][, 1:(ncol(df[[i]][[1]]) / 2)])),
                         length(df))
        years <- rep(names(df),
                     each = (ncol(df[[i]][[1]][, 1:(ncol(df[[i]][[1]]) / 2)])))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      births = births)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = births,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religion",
               title = paste("Structure of births of", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 8) {
      df <- previous_mortality
      mortality <- c()
      for (i in 1:length(df)) {
        mortality <- c(mortality, sum(df[[i]][[1]]))
      }
      years <- names(df)
      plot_data_frame <- data.frame(year = years, mortality = mortality)
      plot <- ggplot(plot_data_frame, aes(x = year,
                                          y = mortality,
                                          group = 1)) +
        geom_line(size = 1) +
        labs(x = "Year",
             y = "Mortality",
             title = paste("Mortality of", name, "from",
                           as.character(names(structure_data)[[1]]), "to",
                           as.character(names(structure_data)[[length(structure_data)]]))) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (variant == 9) {
      df <- country_object_list
      imigrants <- c()
      imigrants_holder <- rep(0, length(previous_migration))
      for (country in 1:length(df)) {
        for (year in 1:length(df[[country]]$previous_migration)) {
          imigrants <- c(imigrants,
                         sum(df[[country]]$previous_migration[[year]][[1]][[code]]))
        }
        imigrants_holder <- imigrants_holder + imigrants
        imigrants <- c()
      }
      emigrations <- c()
      for (i in 1:length(previous_migration)) {
        emigrations <- c(emigrations,
                         sum(Reduce("+", previous_migration[[i]][[1]])))
      }
      migrations <- c(imigrants_holder, emigrations)
      types <- rep(c("In", "Out"), each = (length(migrations) / 2))
      years <- rep(names(previous_migration), 2)
      plot_data_frame <- data.frame(year = years,
                                    type = types,
                                    migration = migrations)
      plot <- ggplot(plot_data_frame, aes(x = years,
                                          y = migrations,
                                          group = type,
                                          color = type)) +
        geom_line(size = 1) +
        scale_color_manual(values = c("green4", "red4")) +
        labs(x = "Year",
             y = "Migrations",
             color = "Type",
             title = paste("Migration (In/Out) of", name, "from",
                           as.character(names(structure_data)[[1]]), "to",
                           as.character(names(structure_data)[[length(structure_data)]]))) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (variant == 10) {
      if (post_switch) {
        df <- previous_migration
        migrations <- c()
        for (i in 1:length(df)) {
          mig_df <- Reduce("+", df[[i]][[1]])
          if (sum(mig_df) > 0) {
            migrations <- c(migrations, colSums(mig_df))
          } else {
            migrations <- c(migrations, rep(0, ncol(structure_data[[1]])))
          }
        }
        years <- rep(names(df), each = ncol(structure_data[[1]]))
        religions <- rep(colnames(structure_data[[1]]), length(df))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      emigration = migrations)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = years,
                                            y = emigration,
                                            group = religion,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religion",
               title = paste("Structure of migrants (in) in", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- previous_migration
        migrations <- c()
        for (i in 1:length(df)) {
          mig_df <- Reduce("+", df[[i]][[1]])
          if (sum(mig_df) > 0) {
            mig_df_sum <- mig_df[, 1:(ncol(mig_df) / 2)] + mig_df[, (ncol(mig_df) / 2 + 1):ncol(mig_df)]
            migrations <- c(migrations, colSums(mig_df_sum))
          } else {
            migrations <- c(migrations, rep(0, (ncol(structure_data[[1]]) / 2)))
          }
        }
        years <- rep(names(df),
                     each = ncol(structure_data[[1]][, 1:(ncol(structure_data[[1]]) / 2)]))
        religions <- rep(colnames(structure_data[[1]])[1:(ncol(structure_data[[1]]) / 2)],
                         length(df))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      emigration = migrations)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = years,
                                            y = emigration,
                                            group = religion,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religion",
               title = paste("Structure of migrants (in) in", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 11) {
      if (post_switch) {
        df <- country_object_list
        imigrants <- c()
        imigrants_holder <- rep(0,
                                ncol(structure_data[[1]]) * length(previous_migration))
        for (country in 1:length(df)) {
          for (year in 1:length(df[[country]]$previous_migration)) {
            if (sum(df[[country]]$previous_migration[[year]][[1]][[code]]) > 0) {
              imigrants <- c(imigrants,
                             colSums(df[[country]]$previous_migration[[year]][[1]][[code]]))
            } else {
              imigrants <- c(imigrants, rep(0, ncol(structure_data[[1]])))
            }
          }
          imigrants_holder <- imigrants_holder + imigrants
          imigrants <- c()
        }
        religions <- rep(colnames(structure_data[[1]]),
                         length(previous_migration))
        years <- rep(names(previous_migration),
                     each = ncol(structure_data[[1]]))
        plot_data_frame <- data.frame(year = years,
                                      religion = religions,
                                      migrations = imigrants_holder)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = migrations,
                                            group = religion,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_switching_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religions",
               title = paste("Structure of migrants (out) in", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- country_object_list
        imigrants <- c()
        imigrants_holder <- rep(0, ((ncol(structure_data[[1]]) / 2) * length(previous_migration)))
        for (country in 1:length(df)) {
          for (year in 1:length(df[[country]]$previous_migration)) {
            if (sum(df[[country]]$previous_migration[[year]][[1]][[code]]) > 0) {
              imigrants_sum <- df[[country]]$previous_migration[[year]][[1]][[code]][, 1:(ncol(df[[country]]$previous_migration[[year]][[1]][[code]]) / 2)] + df[[country]]$previous_migration[[year]][[1]][[code]][, (ncol(df[[country]]$previous_migration[[year]][[1]][[code]]) / 2 + 1):ncol(df[[country]]$previous_migration[[year]][[1]][[code]])]
              imigrants <- c(imigrants, colSums(imigrants_sum))
            } else {
              imigrants <- rep(0, (ncol(structure_data[[1]]) / 2))
            }
          }
          imigrants_holder <- imigrants_holder + imigrants
          imigrants <- c()
          imigrants_sum <- c()
        }
        religions <- rep(colnames(structure_data[[1]])[1:(ncol(structure_data[[1]]) / 2)],
                         length(previous_migration))
        years <- rep(names(previous_migration),
                     each = (ncol(structure_data[[1]]) / 2))
        plot_data_frame <- data.frame(year = years, religion = religions,
                                      migrations = imigrants_holder)
        plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                       levels = unique(plot_data_frame[, 2]))
        plot <- ggplot(plot_data_frame, aes(x = year,
                                            y = migrations,
                                            group = religion,
                                            fill = religion)) +
          geom_bar(position = "fill", stat = "identity") +
          scale_fill_manual(values = plot_colors) +
          labs(x = "Year",
               y = "Structure",
               fill = "Religions",
               title = paste("Structure of migrants (out) in", name, "from",
                             as.character(names(structure_data)[[1]]), "to",
                             as.character(names(structure_data)[[length(structure_data)]]))) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    } else if (variant == 12) {
      df <- switching_matrices_list
      sw <- c()
      for (i in 1:(length(df) / 2)) {
        df_temp <- df[[i]]
        df_males <- df_temp[1:(nrow(df_temp) / 2), ]
        origin_religion <- Reduce("*", df_males[, i])
        switching_religion_males <- df_males[, (ncol(df_males) / 2 + 1):ncol(df_males)]
        switching_religion_males <- colSums(switching_religion_males)
        switching_religion_males <- switching_religion_males / sum(switching_religion_males)
        switching_religion_males <- switching_religion_males * (1 - origin_religion)
        switching_religion_males[[i]] <- origin_religion
        sw <- c(sw, switching_religion_males)
        df_females <- df_temp[(nrow(df_temp) / 2 + 1):nrow(df_temp), ]
        origin_religion <- Reduce("*", df_females[, i])
        switching_religion_females <- df_females[, (ncol(df_females) / 2 + 1):ncol(df_females)]
        switching_religion_females <- colSums(switching_religion_females)
        switching_religion_females <- switching_religion_females / sum(switching_religion_females)
        switching_religion_females <- switching_religion_females * (1 - origin_religion)
        switching_religion_females[[i]] <- origin_religion
        sw <- c(sw, switching_religion_females)
      }
      origin_religion <- rep(colnames(df[[1]])[1:(ncol(df[[1]]) / 2)],
                             each = ncol(df[[1]]))
      switching_religion <- rep(colnames(df[[1]])[1:(ncol(df[[1]]) / 2)],
                                ncol(df[[1]]))
      sex <- rep(rep(c("Males", "Females"),
                     each = ncol(df[[1]]) / 2), (length(df) / 2))
      plot_data_frame <- data.frame(origin_religion = origin_religion,
                                    switching_religion = switching_religion,
                                    sex = sex,
                                    switching = sw)
      plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                     levels = unique(plot_data_frame[, 2]))
      plot_data_frame[, 1] <- factor(plot_data_frame[, 1],
                                     levels = unique(plot_data_frame[, 1]))
      plot_data_frame[is.na(plot_data_frame)] <- 0
      plot <- ggplot(plot_data_frame, aes(x = origin_religion,
                                          y = switching,
                                          fill = switching_religion)) +
        geom_bar(position = "fill", stat = "identity") +
        facet_wrap(~sex, nrow = 2) +
        labs(x = "Origin religion",
             y = "Retention",
             fill = "New religion",
             title = paste("Retention of religions in", name)) +
        scale_fill_manual(values = plot_colors) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (variant == 13) {
      df <- previous_switching
      post_switches_out <- c()
      for (period in 1:length(df)) {
        if (sum(df[[period]][[1]][[switching_religion]][, (length(df[[1]][[1]]) / 2 + 1):length(df[[1]][[1]])]) > 0) {
          post_switches_out <- c(post_switches_out, colSums(df[[period]][[1]][[switching_religion]][, (length(df[[1]][[1]]) / 2 + 1):length(df[[1]][[1]])]))
        } else {
          post_switches_out <- c(post_switches_out,
                                 rep(0, (ncol(df[[1]][[1]][[1]]) / 2)))
        }
      }
      post_switches_in <- c()
      post_switches_religion <- c()
      for (period in 1:length(df)) {
        for (religion in 1:length(df[[period]][[1]][1:(ncol(structure_data[[1]]) / 2)])) {
          switches_sum <- sum(df[[period]][[1]][[religion]][, (switching_religion + (ncol(structure_data[[1]]) / 2))])
          post_switches_religion <- c(post_switches_religion, switches_sum)
        }
        post_switches_in <- c(post_switches_in, post_switches_religion)
        post_switches_religion <- c()
      }
      post_switches <- c(post_switches_in, post_switches_out)
      years <- rep(rep(names(df), each = (ncol(structure_data[[1]]) / 2)), 2)
      religions <- rep(names(df[[1]][[1]])[1:(ncol(structure_data[[1]]) / 2)],
                       (length(df) * 2))
      type <- c(rep("in", (length(religions) / 2)),
                rep("out", (length(religions) / 2)))
      plot_data_frame <- data.frame(year = years,
                                    religion = religions,
                                    post_switch = post_switches)
      plot_data_frame[, 2] <- factor(plot_data_frame[, 2],
                                     levels = unique(plot_data_frame[, 2]))
      plot <- ggplot(plot_data_frame, aes(x = year,
                                          y = ifelse(test = type == "out",
                                                     yes = -post_switch,
                                                     no = post_switch),
                                          group = religion,
                                          fill = religion)) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values = plot_colors) +
        labs(x = "Year",
             y = "Retention",
             fill = "Religion",
             title = paste("Retention of",
                           colnames(structure_data[[1]])[switching_religion],
                           "in", name)) +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      print_color("Wrong plot variant", "red")
      return(NULL)
    }
  }

)
