test_that("adding node labels to ace output works", {
    ## Example with no node labels
    data(bird.orders)
    x <- rnorm(23)
    ### Compare the three methods for continuous characters:
    out <- ace(x, bird.orders)
    expect_equal(names(out$ace), as.character(seq(from = 1+Ntip(bird.orders), to = Nnode(bird.orders)+Ntip(bird.orders))))
    expect_equal(rownames(out$CI95), as.character(seq(from = 1+Ntip(bird.orders), to = Nnode(bird.orders)+Ntip(bird.orders))))

    ## Adding node labels
    phy_nodes <- makeNodeLabel(bird.orders)
    out <- ace(x, phy_nodes)
    expect_equal(names(out$ace), phy_nodes$node.label)
    expect_equal(rownames(out$CI95), phy_nodes$node.label)
})

test_that("adding node labels to ace output works for discrete characters", {
    ## Example with no node labels
    data(bird.orders)
    x <- as.factor(c(rep(0, 5), rep(1, 18)))
    ### Compare the three methods for continuous characters:
    out <- ace(x, bird.orders, type = "d")
    expect_equal(rownames(out$lik.anc), as.character(seq(from = 1+Ntip(bird.orders), to = Nnode(bird.orders)+Ntip(bird.orders))))

    ## Adding node labels
    phy_nodes <- makeNodeLabel(bird.orders)
    out <- ace(x, phy_nodes, type = "d")
    expect_equal(rownames(out$lik.anc), phy_nodes$node.label)
})
