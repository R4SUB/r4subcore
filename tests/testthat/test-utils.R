test_that("hash_id produces deterministic output", {
  h1 <- hash_id("a", "b")
  h2 <- hash_id("a", "b")
  expect_equal(h1, h2)
})

test_that("hash_id with prefix works", {
  h <- hash_id("test", prefix = "RUN")
  expect_match(h, "^RUN-")
})

test_that("hash_id differs for different inputs", {
  h1 <- hash_id("a")
  h2 <- hash_id("b")
  expect_false(h1 == h2)
})

test_that("hash_id handles single argument", {
  h <- hash_id("single")
  expect_true(is.character(h))
  expect_true(nzchar(h))
})

test_that("json_safely handles deeply nested input", {
  nested <- list(a = list(b = list(c = list(d = 1))))
  out <- json_safely(nested)
  expect_true(nzchar(out))
  parsed <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_equal(parsed$a$b$c$d, 1)
})

test_that("json_safely handles normal input", {
  out <- json_safely(list(a = 1, b = "hello"))
  expect_true(nzchar(out))
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$a, 1)
  expect_equal(parsed$b, "hello")
})

test_that("json_safely handles NULL", {
  expect_equal(json_safely(NULL), "{}")
})

test_that("json_safely handles empty list", {
  expect_equal(json_safely(list()), "{}")
})

test_that("canon_severity maps common aliases", {
  expect_equal(canon_severity("HIGH"), "high")
  expect_equal(canon_severity("Error"), "high")
  expect_equal(canon_severity("warning"), "medium")
  expect_equal(canon_severity("Note"), "info")
  expect_equal(canon_severity("severe"), "critical")
})

test_that("canon_severity rejects unknown values", {
  expect_error(canon_severity("banana"), "Unknown severity")
})

test_that("canon_result maps common aliases", {
  expect_equal(canon_result("PASS"), "pass")
  expect_equal(canon_result("Failed"), "fail")
  expect_equal(canon_result("Warning"), "warn")
  expect_equal(canon_result("N/A"), "na")
  expect_equal(canon_result("OK"), "pass")
})

test_that("canon_result rejects unknown values", {
  expect_error(canon_result("banana"), "Unknown result")
})
