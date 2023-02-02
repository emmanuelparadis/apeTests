test_that("topo.dist correct", {
  fl <- system.file("extdata/input/Newick/three_unrooted_trees_4tips.tre",
                    package = "phylobench")
  TR <- read.tree(fl)
  expect_equal(as.integer(dist.topo(TR)), rep(2L, 3))
})
