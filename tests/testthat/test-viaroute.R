context("viaroute")
test_that("viaroute finds routes", {
  # skip("Skip for development of tests")

  lat1 <- 47.168
  lng1 <- 8.117
  lat2 <- 46.978
  lng2 <- 8.335
  prec <- 0.02

  if (localhost) {
    run_server(osrm_path = osrm_path, map_name = map_name)
  }
  r <- viaroute(lat1, lng1, lat2, lng2, localhost = TRUE, instructions = FALSE)
  expect_equal(class(r), "numeric")
  expect_equal(length(r), 1)
  r <- viaroute(lat1, lng1, lat2, lng2, localhost = TRUE, instructions = TRUE)
  expect_equal(class(r), "list")
  expect_equal(length(r), 3)

  # The same location should work as well:
  r <- viaroute(lat1, lng1, lat1, lng1, localhost = TRUE, instructions = FALSE)
  expect_equal(class(r), "numeric")
  expect_equal(length(r), 1)
  expect_equal(r, 0)
  r <- viaroute(lat1, lng1, lat1, lng1, localhost = TRUE, instructions = TRUE)
  expect_equal(class(r), "list")
  expect_equal(length(r), 3)
  expect_equal(r$routes[[1]]$distance, 0)
  expect_equal(r$routes[[1]]$duration, 0)

  if (localhost) {
    quit_server()
  }
})

