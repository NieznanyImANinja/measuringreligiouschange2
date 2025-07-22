#' @include projection_class_saving.R

projection_object$methods(
  list(

    # variant:
    # 1 - Population by years
    # 2 - Population by years with religions
    # 3 - Population structure by years
    # 4 - Population pyramid with religions
    # 5 - TFR for religion by years
    # period: numeric, declaring for which period data is plotted
    # religion_sw: numeric, used only for plotting switching, declaring for which religion switching is plotted
    # gender: characters ("males" or "females"), used only for switching, declaring for which sex switching is plotted

    generate_plots = function(country, dem, switching, migration, post_switch, plot_colors, plot_switching_colors) {
      save_plot <- function(country, var, period = 0, religion = 0) {
        if (var == 9) {
          df <- country_object_list
          imigrants <- c()
          imigrants_holder <- rep(0, length(country$previous_migration))
          for (cntry in 1:length(df)) {
            for (year in 1:length(df[[cntry]]$previous_migration)) {
              imigrants <- c(imigrants,
                             sum(df[[cntry]]$previous_migration[[year]][[1]][[country$code]]))
            }
            imigrants_holder <- imigrants_holder + imigrants
            imigrants <- c()
          }
          emigrations <- c()
          for (i in 1:length(country$previous_migration)) {
            emigrations <- c(emigrations,
                             sum(Reduce("+", country$previous_migration[[i]][[1]])))
          }
          migrations <- c(imigrants_holder, emigrations)
          types <- rep(c("In", "Out"), each = (length(migrations) / 2))
          years <- rep(names(country$previous_migration), 2)
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
                 title = paste("Migration (In/Out) of", country$name, "from",
                               as.character(names(country$structure_data)[[1]]), "to",
                               as.character(names(country$structure_data)[[length(country$structure_data)]]))) +
            theme(plot.title = element_text(hjust = 0.5))
        } else if (var == 11){
          if (post_switch) {
            df <- country_object_list
            imigrants <- c()
            imigrants_holder <- rep(0,
                                    ncol(country$structure_data[[1]]) * length(country$previous_migration))
            for (cntry in 1:length(df)) {
              for (year in 1:length(df[[cntry]]$previous_migration)) {
                if (sum(df[[cntry]]$previous_migration[[year]][[1]][[country$code]]) > 0) {
                  imigrants <- c(imigrants,
                                 colSums(df[[cntry]]$previous_migration[[year]][[1]][[country$code]]))
                } else {
                  imigrants <- c(imigrants, rep(0, ncol(country$structure_data[[1]])))
                }
              }
              imigrants_holder <- imigrants_holder + imigrants
              imigrants <- c()
            }
            religions <- rep(colnames(country$structure_data[[1]]),
                             length(country$previous_migration))
            years <- rep(names(country$previous_migration),
                         each = ncol(country$structure_data[[1]]))
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
                   title = paste("Structure of migrants (out) in", country$name, "from",
                                 as.character(names(country$structure_data)[[1]]), "to",
                                 as.character(names(country$structure_data)[[length(country$structure_data)]]))) +
              theme(plot.title = element_text(hjust = 0.5))
          } else {
            df <- country_object_list
            imigrants <- c()
            imigrants_holder <- rep(0, ((ncol(country$structure_data[[1]]) / 2) * length(country$previous_migration)))
            for (cntry in 1:length(df)) {
              for (year in 1:length(df[[cntry]]$previous_migration)) {
                if (sum(df[[cntry]]$previous_migration[[year]][[1]][[country$code]]) > 0) {
                  imigrants_sum <- df[[cntry]]$previous_migration[[year]][[1]][[country$code]][, 1:(ncol(df[[cntry]]$previous_migration[[year]][[1]][[country$code]]) / 2)] + df[[cntry]]$previous_migration[[year]][[1]][[country$code]][, (ncol(df[[cntry]]$previous_migration[[year]][[1]][[country$code]]) / 2 + 1):ncol(df[[cntry]]$previous_migration[[year]][[1]][[country$code]])]
                  imigrants <- c(imigrants, colSums(imigrants_sum))
                } else {
                  imigrants <- rep(0, (ncol(country$structure_data[[1]]) / 2))
                }
              }
              imigrants_holder <- imigrants_holder + imigrants
              imigrants <- c()
              imigrants_sum <- c()
            }
            religions <- rep(colnames(country$structure_data[[1]])[1:(ncol(country$structure_data[[1]]) / 2)],
                             length(country$previous_migration))
            years <- rep(names(country$previous_migration),
                         each = (ncol(country$structure_data[[1]]) / 2))
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
                   title = paste("Structure of migrants (out) in", country$name, "from",
                                 as.character(names(country$structure_data)[[1]]), "to",
                                 as.character(names(country$structure_data)[[length(country$structure_data)]]))) +
              theme(plot.title = element_text(hjust = 0.5))
          }
        } else {
          plot <- country$plot(var, period, post_switch, switching_religion = religion, plot_colors, plot_switching_colors)
        }



        print(plot)
      }

      if (dem) {
        save_plot(country, 1)
        save_plot(country, 2)
        save_plot(country, 3)
        save_plot(country, 4, 1)
        save_plot(country, 4, round(median(seq_along(country$structure_data))))
        save_plot(country, 4, length(country$structure_data))
        save_plot(country, 5)
        save_plot(country, 6, 1)
        save_plot(country, 6, round(median(seq_along(country$previous_asfr))))
        save_plot(country, 6, length(country$previous_asfr))
        save_plot(country, 7)
        save_plot(country, 8)
      }

      if (migration) {
        save_plot(country, 9)
        save_plot(country, 10)
        save_plot(country, 11)
      }

      if (switching) {
        save_plot(country, 12)
         for (rel in 1:8) {
           save_plot(country, 13, religion = rel)
         }
      }
    },

    save_all_plots_ver2 = function(dem = TRUE, switching = TRUE, migration = TRUE, post_switch = TRUE,
                                   plot_colors, plot_switching_colors) {
      output_dir <- "PlotsVer2_pdf"
      if (!dir.exists(output_dir)) dir.create(output_dir)

      pb <- progress_bar$new(
        format = "Saving plots [:bar] :percent ETA: :eta",
        total = length(country_object_list), clear = FALSE, width = 100
      )

      for (i in seq_along(country_object_list)) {
        country_name <- names(country_object_list)[i]
        country <- country_object_list[[i]]

        pdf(file = file.path(output_dir, paste0(country_name, ".pdf")), paper = "a4r", width = 10)

        generate_plots(country, dem, switching, migration, post_switch, plot_colors, plot_switching_colors)

        dev.off()
        pb$tick()
      }
    }



  )
)
