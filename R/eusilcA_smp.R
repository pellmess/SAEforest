#' Simulated EU-SILC data - survey sample data
#'
#' The data set comprises synthetic EU-SILC data and is taken from the package \pkg{emdi}.
#' Originally, the data set is a simple random sample from \code{\link{eusilcA_pop}} based on
#' \code{\link[simFrame]{eusilcP}} from package \pkg{simFrame}.
#'
#' @format A data frame with 1945 observations and 18 variables:
#' \describe{
#' \item{eqIncome}{numeric; a simplified version of the equivalized household income.}
#' \item{eqsize}{numeric; the equivalized household size according to the
#' modified OECD scale.}
#' \item{gender}{factor; the person's gender (levels: male and female).}
#' \item{cash}{numeric; employee cash or near cash income (net).}
#' \item{self_empl}{numeric; cash benefits or losses from self-employment (net).}
#' \item{unempl_ben}{numeric; unemployment benefits (net).}
#' \item{age_ben}{numeric; old-age benefits (net).}
#' \item{surv_ben}{numeric; survivor's benefits (net).}
#' \item{sick_ben}{numeric; sickness benefits (net).}
#' \item{dis_ben}{numeric; disability benefits (net).}
#' \item{rent}{numeric; income from rental of a property or land (net).}
#' \item{fam_allow}{numeric; family/children related allowances (net).}
#' \item{house_allow}{numeric; housing allowances (net).}
#' \item{cap_inv}{numeric; interest, dividends, profit from capital investments
#'  in unincorporated business (net).}
#' \item{tax_adj}{numeric; repayments/receipts for tax adjustment (net).}
#' \item{state}{factor; state (nine levels).}
#' \item{district}{factor; districts (94 levels).}
#' \item{weight}{numeric; constant weight.}
#' }
#' @docType data
"eusilcA_smp"
