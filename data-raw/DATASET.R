## code to prepare `DATASET` dataset goes here

accel = readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)

# con = dbConnect(
#   duckdb(
#     file.path("inst", "ctrialsgovdb", "ctrialsgov.duckdb"),
#     read_only = TRUE
#   )
# )
#
# eligibilities = tbl(con, "eligibilities") |> collect()
# saveRDS(eligibilities, "data-raw/eligibilities.rds")
# usethis::use_data(eligibilities, overwrite = TRUE)
