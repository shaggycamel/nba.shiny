test_that("pbkdf2-sha256 matches RFC test vectors", {
  v1 <- nba.shiny.core:::pbkdf2_sha256(charToRaw("password"), charToRaw("salt"), 1L, 32L)
  expect_equal(
    nba.shiny.core:::raw_to_hex(v1),
    "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"
  )

  v2 <- nba.shiny.core:::pbkdf2_sha256(charToRaw("password"), charToRaw("salt"), 2L, 32L)
  expect_equal(
    nba.shiny.core:::raw_to_hex(v2),
    "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43"
  )
})

test_that("hash_password defaults to bcrypt and round-trips", {
  h <- hash_password("s3cret", cost = 4L)
  expect_true(startsWith(h, "$2"))
  expect_true(verify_password("s3cret", h))
  expect_false(verify_password("wrong", h))
})

test_that("hash_password can still produce the legacy pbkdf2 format", {
  h <- hash_password("s3cret", scheme = "pbkdf2", iterations = 1000L)
  expect_true(startsWith(h, "pbkdf2-sha256$1000$"))
  expect_true(verify_password("s3cret", h))
  expect_false(verify_password("wrong", h))
})

test_that("verify_password rejects empty and unknown formats", {
  expect_false(verify_password("x", NULL))
  expect_false(verify_password("x", ""))
  expect_false(verify_password("x", "not-a-known-format"))
  expect_false(verify_password("x", "pbkdf2-sha256$notanint$aa$bb"))
})

test_that("a fresh salt makes each hash unique but both verify", {
  a <- hash_password("same", cost = 4L)
  b <- hash_password("same", cost = 4L)
  expect_false(identical(a, b))
  expect_true(verify_password("same", a))
  expect_true(verify_password("same", b))
})
