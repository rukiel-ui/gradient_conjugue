
test_that("Le systeme thermique est correctement construit", {

  systeme <- construire_systeme_chaleur(n = 5)

  # Verification des dimensions
  expect_equal(dim(systeme$A), c(25, 25))
  expect_equal(length(systeme$b), 25)
  expect_equal(systeme$n, 5)

  # Verification de la matrice creuse
  expect_true(inherits(systeme$A, "dgCMatrix"))

  # Verification de la symetrie et validite
  expect_true(verifier_matrice(systeme$A))

  # Verification des valeurs numeriques
  expect_true(all(is.finite(systeme$b)))
})
