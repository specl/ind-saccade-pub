require(cmdstanr)

writeChain <- function(fit, filename) {
  draws <- fit$draws(format = "df")
  draws <- posterior::as_draws_df(draws)
  param_cols <- setdiff(
    names(draws)[vapply(draws, is.numeric, logical(1))],
    c(".chain", ".iteration", ".draw"))
  fields <- list()
  for (nm in param_cols) {
    fields[[nm]] <- arrow::field(nm, arrow::float32())}
  fields[[".chain"]]     <- arrow::field(".chain", arrow::int32())
  fields[[".iteration"]] <- arrow::field(".iteration", arrow::int32())
  fields[[".draw"]]      <- arrow::field(".draw", arrow::int32())
  schema_spec <- arrow::schema(fields)
  tab <- arrow::Table$create(draws)$cast(schema_spec)
  arrow::write_parquet(tab, filename, compression = "zstd")
}

jagsFormat <- function(draws) {
  drws <- posterior::as_draws_rvars(draws)
  simsList <- lapply(drws, function(x) {
    raw <- posterior::draws_of(x)
    return(raw)})
  return(simsList)}

readChainDraws <- function(filename) {
  draws <- arrow::read_parquet(filename)
  return(posterior::as_draws_df(draws))
}

readChainPars <- function(filename) {
  draws <- arrow::read_parquet(filename)
  outDraws <- posterior::as_draws_df(draws)
  return(jagsFormat(outDraws))
}

runMod1 <- function(dat ,M=10000){
  dataList <- list(
    N = length(dat$sub),
    I = max(dat$sub),
    Y = dat$Y,
    logDur = log(dat$durMS),
    sub = dat$sub,
    cond = 2-dat$cond,
    s0=1)
  mod <- cmdstan_model("mod1.stan")
  fit <- mod$sample(
    data = dataList,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = M/10,
    iter_sampling = M,
    seed=123)
  return(fit)}

runMod2 <- function(Y, free, M = 2000) {
  mod <- cmdstan_model("mod2.stan")
  Y_z <- scale(Y)
  d   <- dim(Y_z)
  fit <- mod$sample(
    data = list(
      I = d[1], J = d[2], F = ncol(free),
      Y = Y_z, free = free),
    chains          = 4,
    parallel_chains = 4,
    iter_warmup     = 1000,
    iter_sampling   = M,
    refresh         = 100
  )
  return(fit)
}
  