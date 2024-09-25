skip_if_not_installed("xml2")
test_that("converting to phyloXML works", {
  # simple phylo
  chiroptera_rda <- system.file("data/chiroptera.rda", package = "ape")
  load(chiroptera_rda)
  # provides `chiroptera`:
  # * rooted
  # * without edge.length
  # * without node.label
  # * without root.edge
  chiroptera_phyloxml <- ape:::phylo_to_xml(chiroptera)
  expect_equal(
    length(xml2::xml_find_all(chiroptera_phyloxml, "//clade")),
    length(chiroptera$tip.label) + chiroptera$Nnode
  )
  expect_equal(
    length(xml2::xml_find_all(chiroptera_phyloxml, "//clade/parent::clade")),
    chiroptera$Nnode
  )
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(chiroptera_phyloxml, "//phylogeny"),
      "rooted"
    ),
    "true"
  )
  expect_setequal(
    xml2::xml_text(
      xml2::xml_find_all(chiroptera_phyloxml, "//clade[not(./clade)]/name")
    ),
    chiroptera$tip.label
  )

  bird_orders_rda <- system.file("data/bird.orders.rda", package = "ape")
  load(bird_orders_rda) # provides `bird.orders`
  birds <- unroot(bird.orders)
  birds$node.label <- paste("birdy", seq_len(birds$Nnode), sep = "_")
  birds$root.edge <- 3.14
  # `birds`:
  # * unrooted
  # * with edge.length
  # * with node.label
  # * with root.edge
  ape:::phylo_to_xml(birds)
  birds_phyloxml <- ape:::phylo_to_xml(birds)
  expect_equal(
    length(xml2::xml_find_all(birds_phyloxml, "//clade")),
    length(birds$tip.label) + birds$Nnode
  )
  expect_equal(
    length(xml2::xml_find_all(birds_phyloxml, "//clade/parent::clade")),
    birds$Nnode
  )
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(birds_phyloxml, "//phylogeny"),
      "rooted"
    ),
    "true"
  )
  expect_setequal(
    xml2::xml_text(
      xml2::xml_find_all(birds_phyloxml, "//clade[not(./clade)]/name")
    ),
    birds$tip.label
  )

  # multiPhylo object:
  # * with tree.names = F
  # * with tree.names = T
  # * with tree.names = <names>
  multi <- c(chiroptera, birds)

  expect_no_error(ape:::phylo_to_xml(multi, tree.names = TRUE))

  multi_phyloxml_unnamed <- ape:::phylo_to_xml(multi, tree.names = FALSE)
  expect_equal(
    length(xml2::xml_find_all(multi_phyloxml_unnamed, "//phylogeny")),
    2
  )

  multi_phyloxml_named <- ape:::phylo_to_xml(
    multi,
    tree.names = c("chiroptera", "bird_orders")
  )
  expect_equal(
    length(xml2::xml_find_all(multi_phyloxml_named, "//phylogeny")),
    2
  )
  expect_setequal(
    xml2::xml_text(
      xml2::xml_find_all(multi_phyloxml_named, "//phylogeny/name")
    ),
    c("chiroptera", "bird_orders")
  )

  # testing tree structure
  tree1 <- read.tree(text = "(A,(B,C),D);")
  tree1_xml <- ape:::phylo_to_xml(tree1)
  root <- xml2::xml_find_all(tree1_xml, "//phylogeny/clade")
  expect_length(root, 1)
  expect_equal(
    xml2::xml_text(xml2::xml_find_all(root, "./clade/name")),
    c("A", "6", "D")
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_all(root, "./clade/clade/name")),
    c("B", "C")
  )

  tree2 <- read.tree(text = "(A,(B,(C,D)));")
  tree2_xml <- ape:::phylo_to_xml(tree2)
  root <- xml2::xml_find_all(tree2_xml, "//phylogeny/clade")
  expect_length(root, 1)
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(root, "./clade/name")),
    "A"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(root, "./clade/clade/name")),
    "B"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_all(root, "./clade/clade/clade/name")),
    c("C", "D")
  )

  # testing output and structure
  expect_output(
    write.phyloXML(tree2),
    paste(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<phyloxml xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.phyloxml.org" xsi:schemaLocation="http://www.phyloxml.org http://www.phyloxml.org/1.20/phyloxml.xsd">',
      '  <phylogeny rooted="true">',
      '    <clade>',
      '      <name>5</name>',
      '      <clade>',
      '        <name>A</name>',
      '      </clade>',
      '      <clade>',
      '        <name>6</name>',
      '        <clade>',
      '          <name>B</name>',
      '        </clade>',
      '        <clade>',
      '          <name>7</name>',
      '          <clade>',
      '            <name>C</name>',
      '          </clade>',
      '          <clade>',
      '            <name>D</name>',
      '          </clade>',
      '        </clade>',
      '      </clade>',
      '    </clade>',
      '  </phylogeny>',
      '</phyloxml>',
      sep = "\n"
    ),
    fixed = TRUE
  )
})
