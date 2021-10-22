# DEFINE AND USE SAMPLE SELECT WRAPPER FUNCTION
sample_select <- function(pop, smp, dName, times=100, set_seed = 1234){
  # pop.............. the population or census data
  # smp.............. the sample data
  # n................ the number of samples drawn
  # set_seed......... set seed for reproduceability

  smpSizes <- table(smp[dName])
  smpSizes <- data.frame(smpidD = as.numeric(names(smpSizes)), n_smp = as.numeric(smpSizes),
                         stringsAsFactors = FALSE)

  smpSizes <- left_join(data.frame(idD = unique(pop[dName])),
                        smpSizes, by = c("idD" = "smpidD"))

  smpSizes$n_smp[is.na(smpSizes$n_smp)] <- 0

  splitPop <- split(pop, pop$idD)

  stratSamp <- function(dfList, ns) {
    do.call(rbind, mapply(dfList, ns, FUN = function(df, n) {
      popInd <- seq_len(nrow(df))
      sel <- base::sample(popInd, n, replace = FALSE)
      df[sel, ]
    }, SIMPLIFY = F))
  }

  set.seed(set_seed)
  samples <- replicate(times, stratSamp(splitPop, smpSizes$n_smp), simplify = FALSE)

  rm(splitPop)
  return(samples)
}
