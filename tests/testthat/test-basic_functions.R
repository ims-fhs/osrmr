context("viaroute")

test_that("viaroute finds routes", {
  # skip("Skip for development of tests")

  lat1 <- 47.168
  lng1 <- 8.117
  lat2 <- 46.978
  lng2 <- 8.335
  prec <- 0.02

  if (localhost) {
    run_server(osrm_path)
  }
  r <- viaroute(lat1, lng1, lat2, lng2, localhost = TRUE, instructions = FALSE)
  expect_equal(class(r), "numeric")
  expect_equal(length(r), 1)
  r <- viaroute(lat1, lng1, lat2, lng2, localhost = TRUE, instructions = TRUE)
  expect_equal(class(r), "list")
  expect_equal(length(r), 3)
  if (localhost) {
    quit_server()
  }
})


context("decoder")
test_that("decoder for consistent results", {
  # skip("Skip for development of tests")

  encoded_str <- "q|x}Gkuuq@u@Vw@ReAT_AJ}@Bs@CgAQgA[gAi@mAcAsDgDkByA}@k@_Ae@oAk@Ac@gAa@yAc@o@Qe@M}@Uo@IkAOsAGk@Bm@Fo@Js@RiAh@o@^s@h@_A|@_BhBuAhByA~BkHlMoCpDoAp@kA\\eCN}C?{D^_NdEoBr@uGtByIfD{@p@_A|@cAxAo@rAGHa@p@k@lAaBxDi@|@o@v@kAtAc@p@a@v@OPOFM?MMGMGM@EDO@UE]IQKIKCEAI@MHIJEPCT@PBPLTHDB@P@BAZ`@VXTNXHN?\\MTUp@_An@{@Zg@Tk@^cAlAsC^yABGfAwCz@yA|@mAz@u@v@s@rIeDtGuB|Bw@nMyDvDc@`DDfCOpAg@jAs@rCuDfHqMdBgCnAcBr@}@fAiAz@u@z@g@j@[v@Wv@Q^Eh@Id@Ab@?`@Bh@Dx@L`@H`B^l@RrC|@dA^~Ap@`Aj@t@f@hBtAhD~Ct@n@t@h@f@Xl@Tf@Pp@JhAFjA?bAKz@Q|@UjBo@j@Q`A[v@OdAM|@A|@BvBNbFh@tEf@~CXpCVxCPzD\\r@B`@?f@Ch@Gf@Ip@Sf@Sh@Yj@c@x@{@`AmAnBeCh@eAjAkC~EmI~@oCf@uB\\iBb@mDTuCPeJ_@qYOqCWgCa@gCq@cDk@iCi@sBa@iAi@aBY_AS_AQuAIuAC{A@uA@}ABuAH{ABc@Hw@RsARoAXuAXkA`@{ARq@Zy@Zs@\\s@h@_AlA_BxB{ChBgCzJaNrAiBlBkCtB}ClAgBfAaBpCuExAsCjA_CdAeC~@aC~@gCb@qAf@aBd@eBd@cB\\yA\\{@|@}DRkAVmAXoAXsB|@_HJg@Tk@V[VIHPLHL?HCJIDKBO@SCQISJSNe@H_@Dc@JwB@y@C[CYt@a@RMlBw@|Aq@XOBFBDFBD@DADCHd@TpA"
  # encoded <- strwrap(encoded_str, width=10000, simplify=T)

  geometry <- decode_geom(encoded_str, precision = 5)
  expect_equal(all(dim(geometry) == c(281, 2)), T)
  expect_equal(geometry$lat[1], 46.99097)
  expect_equal(geometry$lng[1], 8.30822)

  encoded_str_api_5 <- encoded_string_api_5
  geometry_api_5 <- decode_geom(encoded_str_api_5, precision = 6)
  expect_equal(all(dim(geometry_api_5) == c(35, 2)), T)
  expect_equal(geometry_api_5$lat[1], 47.100198)
  expect_equal(geometry_api_5$lng[1], 8.099703)
})

