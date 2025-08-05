context("get flights")

test_that("standard get_flights (PDX, June 2018)", {
  skip_on_cran()
  skip_if_offline()
  skip_on_os("windows")
  
  flights_ <- get_flights("PDX", 2018, 6)
})

test_that("standard get_flights (NYC, February 2013)", {
  skip_on_cran()
  skip_if_offline()
  skip_on_os("windows")
  
  flights_2 <- get_flights(c("JFK", "LGA", "EWR"), 2013, 2)
  
  flights_orig <- nycflights13::flights %>% dplyr::filter(month == 2)
  
  expect_equal(nrow(flights_2), nrow(flights_orig))
  expect_equal(ncol(flights_2), ncol(flights_orig))
  expect_equal(colnames(flights_2), colnames(flights_orig))
  expect_equal(purrr::map(flights_2, class) %>% unlist(),
               purrr::map(flights_orig, class) %>% unlist())
})

test_that("arrival get_flights (NYC, February 2013)", {
  skip_on_cran()
  skip_if_offline()
  skip_on_os("windows")

  flights_arr <- get_flights(c("JFK", "LGA", "EWR"), 2013, 2, type = "arrival")
  expect_gt(nrow(flights_arr), 0)
  expect_true(all(flights_arr$dest %in% c("JFK", "LGA", "EWR")))

  flights_dep <- get_flights(c("JFK", "LGA", "EWR"), 2013, 2)
  expect_equal(ncol(flights_arr), ncol(flights_dep))
  expect_equal(colnames(flights_arr), colnames(flights_dep))
})
