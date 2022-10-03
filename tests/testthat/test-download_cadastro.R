test_that("download_cadastro funciona", {

  cadastro <- download_cadastro(2022)
  expect_equal(ncol(cadastro),35)
})
