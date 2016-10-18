context("viaroute")

test_that("viaroute finds routes - Either (V5 + web) or (V4 + server)", {
  # skip("Skip for development of tests")

  lat1 <- 47.168
  lng1 <- 8.117
  lat2 <- 46.978
  lng2 <- 8.335
  prec <- 0.02

  run_server(osrm_path, plattform)

  expect_equal(nearest(lat1, lng1, 5, F)$waypoints[[1]]$distance,
               25, tolerance = 1, scale = 1)
  expect_equal(nearest(lat1, lng1, 4, T)$mapped_coordinate[1],
               47.168, tolerance = 0.005, scale = 1)

  # Test the ABSOLUTE difference using web
  expect_equal(viaroute(lat1, lng1, lat2, lng2, F, 5, F),
               1560, tolerance = 100, scale = 1)
  # Test the RELATIVE difference is within prec using localhost
  # expect_equal(viaroute(lat1, lng1, lat2, lng2, F, 4, T),
  #              expected = 1812, tolerance = prec, scale = 1812)

  quit_server()
})

