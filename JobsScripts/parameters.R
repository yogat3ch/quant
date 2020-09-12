# General ----
# Tue Aug 25 11:54:03 2020
params <- qf:::params
# see qf/R/general.R for parameters to update
if (stringr::str_detect(deparse(sys.calls()[[sys.nframe()-1]]), "sourceEnv")) {
  db <- qf::get_data("db", "hours")
}





# The Portfolio google sheet
#googlesheets::gs_auth(token = "~//R//sholsen_googlesheets_token.rds")
#params$gs <- googlesheets::gs_url("https://docs.google.com/spreadsheets/d/1Iazn6lYRMhe-jdJ3P_VhLjG9M9vNWqV-riBmpvBBseg/edit#gid=0")
# TimeSeries fns ----
# Tue Aug 25 15:29:01 2020


# TuneHP ----
# Tue Aug 25 07:22:28 2020
# NOTE Must add model types and their parsnip functions
.methodlist <- c(xgboost = parsnip::boost_tree)


# Paths to the most recent data saves
params$paths <- list(Positions_tsl = "~/R/Quant/Positions_tsl2019-09-02.Rdata", 
                     Positions_ts_rv_iv = "~/R/Quant/Positions_ts_rv_iv2015-07-27_2019-07-16.Rdata",
                     Positions_new = "~/R/Quant/Positions_new.Rdata",
                     best_tsl = "~/R/Quant/best_tsl.Rdata")

# Read the columns from the Orders sheet in their appropriate classes
params$Orders_cols <- c(
  Platform = 'c',
  id = 'c',
  client_order_id = 'c',
  created_at = 'T' ,
  updated_at = 'T',
  submitted_at = 'T',
  filled_at = 'T',
  expired_at = 'T',
  canceled_at = 'T' ,
  failed_at = 'T',
  asset_id = 'c',
  symbol = 'c',
  asset_class = 'c',
  qty = 'd',
  filled_qty = 'd' ,
  filled_avg_price = 'd',
  order_type = 'c',
  type = 'c',
  side = 'c',
  time_in_force = 'c',
  limit_price = 'd',
  stop_price = 'd',
  status = 'c',
  extended_hours = 'l',
  CB = 'd',
  GL = 'd',
  TSL = 'c',
  live = 'l',
  SID = 'c'
)





