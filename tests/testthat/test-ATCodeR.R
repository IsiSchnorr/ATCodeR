# In tests/testthat/test-calculate_ATC_codes.R
library(testthat)
library(readxl)

test_that("ATCtransform processes entire TableSubstanceCheck.xlsx file correctly", {
  # Load the TableSubstanceCheck.xlsx file
  file_path <- test_path("TableSubstanceCheck.xlsx")

  # Load the TableSubstanceCheck.xlsx file
  input.df <- read_excel(file_path)# Adjust the path as needed

  # Confirm the column name "Substance" exists in the loaded data
  expect_true("Substance" %in% colnames(input.df))

  # Call ATCtransform with the entire input data and specify "Substance" as column_name
  result <- ATCtransform(input.df = input.df, column_name = "Substance")

  # Example check: Test that some expected ATC codes are in the output
  expect_true("L04AG06-ALEMTUZUMAB" %in% result$AntineoplasticMedication)
  expect_true("L04AD02-TACROLIMUS" %in% result$AntineoplasticMedication)
})
