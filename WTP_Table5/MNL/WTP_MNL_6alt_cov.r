################################################################################
#
# R script accompanying the paper:
#
# Procedural Invariance in Environmental Valuation:
# Are Preferences Stable Across Choice-Set Sizes?
#
# Anonymous replication materials
#
# Script:       WTP_MNL_6alt_cov.r
# Purpose:      Calculate MNL WTP summaries for the 6Alt treatment.
# Manuscript:   Table 5, MNL results
#
# Inputs:
#   PrefMatWideFormatFullBiogeme.txt (repository root).
#   estimations/MNL_TableA1/MNL_6alt.R (re-estimated by this script).
#
# Sample and data preparation:
#   Retain this treatment, tasks 1--8, and education code v375 != 6.
#   Apply the attribute and demographic recoding in the script.
#
# Outputs:
#   WTP_Table5/MNL/output_cov/ (WTP summaries and diagnostics).
#   Matching fitted model in estimations/MNL_TableA1/output*/.
#
# How to run:
#   From the repository root: Rscript WTP_Table5/MNL/WTP_MNL_6alt_cov.r
#
# Requirements:
#   R; reference environment: R 4.6.1.
#   Packages: apollo, bgw, MASS.
#   See README.md for reference package versions.
#
# Reproducibility:
#   Random seed: 2345
#   Approximate runtime: About 15--60 seconds, including MNL re-estimation.
#   Timing basis: Planning range based on earlier MNL estimation times.
#   100,000 joint normal draws using the robust sampling covariance.
#   Point estimate: negative ratio of fitted coefficients.
#   Intervals: 2.5th and 97.5th percentiles of simulated ratios.
#
################################################################################

main <- function() {
  k <- 6L
  argv <- commandArgs(trailingOnly = FALSE)
  script <- sub('^--file=', '', grep('^--file=', argv, value = TRUE)[1])
  if (is.na(script)) stop('Run using Rscript, not source().')
  here <- dirname(normalizePath(script, mustWork = TRUE))
  repo <- dirname(dirname(here))
  est_dir <- file.path(repo, 'estimations', 'MNL_TableA1')
  est_file <- file.path(est_dir, sprintf('MNL_%dalt.R', k))
  out <- file.path(here, 'output_cov')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (pkg in c('apollo', 'bgw', 'MASS')) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop('Install required package: ', pkg)
  }

  # Child process isolates the original script's rm(list=ls()) and Apollo globals.
  # Use one core for portable execution of the child estimation process.
  code <- readLines(est_file, warn = FALSE)
  core_line <- grep('^\\s*nCores\\s*=\\s*4\\s*,', code)
  if (length(core_line) != 1L) stop('Inspect changed estimation script: expected nCores = 4.')
  code[core_line] <- sub('=\\s*4', '= 1', code[core_line])
  runner <- tempfile(pattern = 'mnl_cov_estimation_', fileext = '.R')
  on.exit(unlink(runner), add = TRUE)
  writeLines(code, runner)
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(est_dir)
  log <- file.path(out, sprintf('MNL_%dalt_estimation.log', k))
  status <- system2(file.path(R.home('bin'), 'Rscript'), shQuote(runner),
                    stdout = log, stderr = log)
  setwd(oldwd)
  if (status != 0L) stop('MNL estimation failed. See ', log)
  model_dirs <- c('output.MNL.2alt','output.MNL.3alt','output.MNL4alt',
                  'output.MNL5alt','output.MNL6alt')
  model_names <- c('MNL_treatment_2alt','MNL_treatment_3alt','MNL_4alt',
                   'MNL_treatment_5alt','MNL_treatment_6alt')
  model_file <- file.path(est_dir, model_dirs[k-1L],
                         paste0(model_names[k-1L], '_model.rds'))
  model <- readRDS(model_file)
  beta <- model$estimate
  V <- model$robvarcov
  if (!is.numeric(beta) || is.null(names(beta)) || !is.matrix(V) ||
      is.null(rownames(V)) || is.null(colnames(V))) stop('Missing named estimates/robust covariance.')
  if (!setequal(names(beta), rownames(V)) || !setequal(names(beta), colnames(V)))
    stop('Covariance names do not match estimates; inspect any fixed parameters.')
  V <- V[names(beta), names(beta), drop = FALSE]
  stopifnot(all(is.finite(beta)), all(is.finite(V)),
            max(abs(V - t(V))) < 1e-8, all(diag(V) > 0))
  V <- (V + t(V))/2
  if (min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) <= 0)
    stop('Robust covariance is not positive definite; inspect estimation diagnostics.')
  if (beta['b_cost'] >= 0) stop('Non-negative fitted cost coefficient; WTP interpretation needs review.')
  if (!is.null(model$robse)) {
    rse <- model$robse
    if (!is.null(names(rse))) rse <- rse[names(beta)]
    stopifnot(isTRUE(all.equal(unname(sqrt(diag(V))), unname(rse), tolerance = 1e-6)))
  }

  set.seed(2345)
  B <- 100000L
  draws <- MASS::mvrnorm(n = B, mu = beta, Sigma = V, empirical = FALSE)
  attrs <- c('b_clar','b_fish','b_bio','b_coast','b_lit')
  labels <- c('Water clarity','Fish stocks','State of biodiversity',
              'Coastal protection','Amount of litter')
  ratio <- -sweep(draws[, attrs, drop = FALSE], 1, draws[, 'b_cost'], '/')
  stopifnot(all(is.finite(ratio)))
  ci <- t(apply(ratio, 2, quantile, probs = c(0.025,0.975), names = FALSE))
  results <- data.frame(treatment = paste0(k,'Alt'), attribute = labels,
      estimate = unname(-beta[attrs]/beta['b_cost']), lower_95 = ci[,1], upper_95 = ci[,2],
      unit = 'EUR/person/year per one-category improvement', row.names = NULL)
  results$unit[c(2,4)] <- 'EUR/person/year per full binary improvement'

  # Diagnostics: no draws are discarded or truncated for the denominator sign.
  # Compare sampled and target covariance on a standardised scale.
  scaled_error <- max(abs(cov(draws)-V)/outer(sqrt(diag(V)),sqrt(diag(V))))
  if (scaled_error > 0.03) stop('Joint draws failed covariance reproduction check.')
  bad_cost <- sum(draws[,'b_cost'] >= 0)
  if (bad_cost > 0) warning(bad_cost, ' draws have non-negative cost; all retained. Inspect ratio tails.')
  # Delta-method SE is an independent diagnostic, not the reported interval.
  delta_se <- sapply(attrs, function(a) {
    g <- setNames(rep(0,length(beta)),names(beta))
    g[a] <- -1/beta['b_cost']; g['b_cost'] <- beta[a]/beta['b_cost']^2
    sqrt(drop(t(g) %*% V %*% g))
  })
  diagnostics <- data.frame(attribute = labels,
      simulated_ratio_median = apply(ratio,2,median),
      simulated_ratio_mean = colMeans(ratio), delta_method_se = unname(delta_se),
      row.names = NULL)
  prefix <- file.path(out, sprintf('WTP_MNL_%dalt_cov',k))
  write.csv(results,paste0(prefix,'_summary.csv'),row.names=FALSE)
  write.csv(V,paste0(prefix,'_robust_vcov.csv'))
  write.csv(data.frame(parameter=names(beta),estimate=unname(beta),
                      robust_se=sqrt(diag(V))),paste0(prefix,'_coefficients.csv'),row.names=FALSE)
  write.csv(diagnostics,paste0(prefix,'_diagnostics.csv'),row.names=FALSE)
  saveRDS(list(estimate=beta,robust_vcov=V,summary=results,seed=2345L,n_draws=B),
          paste0(prefix,'_inputs_results.rds'))
  writeLines(c(paste('Joint draws:',B), 'Seed: 2345',
      paste('Non-negative cost draws:',bad_cost),
      paste('Maximum standardised covariance error:',format(scaled_error,digits=8)),
      'MNL estimation used one core; all other source code unchanged.',
      'Input MD5 checksums:',capture.output(tools::md5sum(c(est_file,
                file.path(repo,'PrefMatWideFormatFullBiogeme.txt')))),
      capture.output(sessionInfo())),paste0(prefix,'_provenance.txt'))
  print(results[,1:5],row.names=FALSE,digits=7)
  cat('\nResults:',prefix,'\n')
}
main()
