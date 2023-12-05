test_that("makeNodeLabel works for phylo and multiPhylo", {
    set.seed(1)
    trees <- rmtree(2, 3)

    ## phylo
    test <- makeNodeLabel(trees[[1]], prefix = "Node")
    expect_is(test, "phylo")
    expect_equal(test$node.label, c("Node1", "Node2"))

    ## multiPhylo (with options parsed correctly)
    test <- makeNodeLabel(trees, prefix = "nOde")
    expect_is(test, "multiPhylo")
   expect_equal(test[[2]]$node.label, c("nOde1", "nOde2"))
})
