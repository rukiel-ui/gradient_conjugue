
test_that("La grille des temperatures est correctement reconstruite", {

  solution <- c(25, 30, 35, 40)

  T <- reconstruire_temperature(
    solution = solution,
    n = 2,
    haut = 100,
    bas = 20,
    gauche = 20,
    droite = 20
  )

  expect_equal(dim(T), c(4, 4))

  expect_equal(T[1, 2:3], c(100, 100))
  expect_equal(T[4, 2:3], c(20, 20))

  expect_true(all(is.finite(T)))
  expect_equal(range(T), c(20, 100))

  expect_error(
    reconstruire_temperature(
      solution = c(1, 2, 3),
      n = 2
    )
  )
})
