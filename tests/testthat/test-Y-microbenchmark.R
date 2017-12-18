context("microbenchmark")

test_that("microbenchmark test for osrm package", {
  # skip("Test only if runtime has to be checked")

  lat1 <- 47.168
  lng1 <- 8.117
  lat2 <- 46.978
  lng2 <- 8.335

  run_server(osrm_path, map_name)
  t1 <- summary(microbenchmark::microbenchmark(
    osrmr::viaroute(lat1, lng1, lat2, lng2, F, api_version, localhost),
    times = 50L, unit = "ms"))$uq
  expect_equal(t1 < 400, T)
  # print(paste0("osrm_viaroute needs: ", round(t1, 1), " ms."))

  # From C:/07 Rprogress/00 Erste Testprogramme/40b Test osrm viaroute speed.R:
  # with message:
  # osrm_viaroute_fast needs: 115.9 ms.
  # osrm_viaroute needs: 116.4 ms.

  quit_server()
})

