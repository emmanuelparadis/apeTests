test_that("Neighbour-joining", {
  matfile <- system.file("extdata/input/Table/M_SaitouNei.txt", package = "phylobench")
  M <- as.matrix(read.table(matfile))
  tr.nj <- nj(M)
  outfile <- system.file("extdata/output/tree_NJ_SaitouNei.tre", package = "phylobench")
  tr.ref <- read.tree(outfile)
  expect_true((all.equal(tr.nj, tr.ref)))
})
