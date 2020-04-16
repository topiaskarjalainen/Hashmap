test_that("creates_hashmap", {
  expect_error(hashmap(initial = c(2,3,4)))
  expect_s3_class(new.hashmap(), "hashmap")
})

test_that("keys and values", {
  expect_equal(keys(hashmap(list(a=1,b=2))),
               c("a", "b")
               )
  expect_equal(
    values(hashmap(list(a=1,b=2))),
    c(1,2)
  )
})
