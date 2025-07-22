#' @include class_base.R

Country_object$methods(

  update_period = function() {
    period <<- paste(as.character(year), as.character(year + 5), sep="-")
  },

  calculate_total_previous = function(variant) {

    #previous_fertility
    m_output = matrix(data = 0, nrow = nrow(previous_fertility[[length(previous_fertility)]][[1]]), ncol = ncol(previous_fertility[[length(previous_fertility)]][[1]]))
    colnames(m_output) <- colnames(previous_fertility[[length(previous_fertility)]][[1]])
    rownames(m_output) <- rownames(previous_fertility[[length(previous_fertility)]][[1]])
    for (matrix in previous_fertility[[length(previous_fertility)]]) {
      m_output <- m_output + matrix
    }
    previous_fertility[[length(previous_fertility)]] <<- append(list("total" = m_output), previous_fertility[[length(previous_fertility)]])

    #previous_mortality
    m_output = matrix(data = 0, nrow = nrow(previous_mortality[[length(previous_mortality)]][[1]]), ncol = ncol(previous_mortality[[length(previous_mortality)]][[1]]))
    colnames(m_output) <- colnames(previous_mortality[[length(previous_mortality)]][[1]])
    rownames(m_output) <- rownames(previous_mortality[[length(previous_mortality)]][[1]])
    for (matrix in previous_mortality[[length(previous_mortality)]]) {
      m_output <- m_output + matrix
    }
    previous_mortality[[length(previous_mortality)]] <<- append(list("total" = m_output), previous_mortality[[length(previous_mortality)]])

    #previous_women_population
    m_output = matrix(data = 0, nrow = nrow(previous_women_population[[length(previous_women_population)]][[1]]), ncol = ncol(previous_women_population[[length(previous_women_population)]][[1]]))
    colnames(m_output) <- colnames(previous_women_population[[length(previous_women_population)]][[1]])
    rownames(m_output) <- rownames(previous_women_population[[length(previous_women_population)]][[1]])
    for (matrix in previous_women_population[[length(previous_women_population)]]) {
      m_output <- m_output + matrix
    }
    previous_women_population[[length(previous_women_population)]] <<- append(list("total" = m_output), previous_women_population[[length(previous_women_population)]])

    #previous_asfr
    noswfert <- previous_fertility[[length(previous_fertility)]][["total"]]
    noswpop <- previous_women_population[[length(previous_women_population)]][["total"]]
    rels <- ncol(noswfert)/2
    for (i in 1:rels){
      noswfert[,i] <- noswfert[,i] + noswfert[,i+rels]
      noswpop[,i] <- noswpop[,i] + noswpop[,i+rels]
    }
    noswpop <- noswpop[,1:rels]
    noswfert <- noswfert[,1:rels]

    relasfr <- noswfert / noswpop
    relasfr <- cbind(relasfr, relasfr)
    colnames(relasfr) <- colnames(previous_women_population[[length(previous_women_population)]][["total"]])
    tfr <- matrix(colSums(relasfr), nrow = 1)
    colnames(tfr) <- colnames(relasfr)
    rownames(tfr) <- c("TFR")
    relasfr <- rbind(relasfr, tfr)
    previous_asfr[[length(previous_asfr)]] <<- append(list("average" = relasfr), previous_asfr[[length(previous_asfr)]])

    if (variant > 1) {
      #previous_switching
      sw_list <- list()
      for (religion in 1:length(previous_switching[[length(previous_switching)]][[length(previous_switching[[length(previous_switching)]])]])) {
        m_output = matrix(data = 0, nrow = nrow(previous_switching[[length(previous_switching)]][[length(previous_switching[[length(previous_switching)]])]][[1]]), ncol = ncol(previous_switching[[length(previous_switching)]][[length(previous_switching[[length(previous_switching)]])]][[1]]))
        colnames(m_output) <- colnames(previous_switching[[length(previous_switching)]][[length(previous_switching[[length(previous_switching)]])]][[1]])
        rownames(m_output) <- rownames(previous_switching[[length(previous_switching)]][[length(previous_switching[[length(previous_switching)]])]][[1]])
        for(list in previous_switching[[length(previous_switching)]]){
          m_output <- m_output+list[[religion]]
        }
        sw_list <- append(sw_list, list("filler" = m_output))
        names(sw_list)[length(sw_list)] <- names(previous_switching[[length(previous_switching)]][[length(previous_switching[[length(previous_switching)]])]])[religion]
      }
      previous_switching[[length(previous_switching)]] <<- append(list("total" = sw_list), previous_switching[[length(previous_switching)]])
    }

    if (variant > 2) {
      #previous_migration
      mig_list <- list()
      for (country in 1:length(previous_migration[[length(previous_migration)]][[length(previous_migration[[length(previous_migration)]])]])) {
        if(!is.matrix(previous_migration[[length(previous_migration)]][[length(previous_migration[[length(previous_migration)]])]][[country]])){
          mig_list <- append(mig_list, 0)
          next
        }

        m_output = matrix(data = 0, nrow = nrow(previous_migration[[length(previous_migration)]][[length(previous_migration[[length(previous_migration)]])]][[country]]), ncol = ncol(previous_migration[[length(previous_migration)]][[length(previous_migration[[length(previous_migration)]])]][[country]]))
        colnames(m_output) <- colnames(previous_migration[[length(previous_migration)]][[length(previous_migration[[length(previous_migration)]])]][[country]])
        rownames(m_output) <- rownames(previous_migration[[length(previous_migration)]][[length(previous_migration[[length(previous_migration)]])]][[country]])
        for(list in previous_migration[[length(previous_migration)]]){
          m_output <- m_output + list[[country]]
        }
        mig_list <- append(mig_list, list(m_output))

      }
      previous_migration[[length(previous_migration)]] <<- append(list("total" = mig_list), previous_migration[[length(previous_migration)]])
    }
  }

)
