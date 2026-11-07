
test_that("Le gradient conjugue resout correctement un systeme SPD", {

  A <- matrix(c(
    4, 1,
    1, 3
  ), nrow = 2, byrow = TRUE)

  b <- c(1, 2)

  resultat <- gradient_conjugue(
    A = A,
    b = b,
    tol = 1e-10,
    max_iter = 100
  )

  solution_attendue <- solve(A, b)

  expect_true(resultat$converged)
  expect_equal(resultat$solution, solution_attendue, tolerance = 1e-8)
  expect_lt(resultat$residu_final, 1e-8)
  expect_gte(resultat$iterations, 0)
  expect_true(length(resultat$historique) >= 1)
})
