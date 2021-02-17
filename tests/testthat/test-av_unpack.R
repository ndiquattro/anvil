test_that("Correct SQL is generated", {

  tdf <- dbplyr::memdb_frame(mtcars[c("mpg", "hp")])

  ex1 <- tdf %>% av_unpack(hp, keys = "inner", drop = FALSE) %>% dbplyr::sql_render()
  ex2 <- tdf %>% av_unpack(hp, keys = "inner", suffix = ".suf", drop = FALSE) %>% dbplyr::sql_render()

  expect_match(ex1, dbplyr::sql("SELECT `mpg`, `hp`, `hp`.`inner` AS `inner`\n"), fixed = FALSE)
  expect_match(ex2, dbplyr::sql("SELECT `mpg`, `hp`, `hp`.`inner` AS `inner.suf`\n"), fixed = FALSE)
})

test_that("Correct SQL is generated when dropping", {

  tdf <- dbplyr::memdb_frame(mtcars[c("mpg", "hp")])

  ex1 <- tdf %>% av_unpack(hp, keys = "inner") %>% dbplyr::sql_render()

  expect_match(ex1, dbplyr::sql("SELECT `mpg`, `hp`.`inner` AS `inner`\n"), fixed = FALSE)
})
