
verifier_matrice <- function(A, tol = 1e-10) {

  # Vérifier que A est une matrice
  if (!is.matrix(A) && !inherits(A, "Matrix")) {
    stop("A doit être une matrice.")
  }

  # Vérifier que la matrice est carrée
  if (nrow(A) != ncol(A)) {
    stop("La matrice doit être carrée.")
  }

  # Vérifier que la matrice contient des valeurs numériques
  type_ok <- is.numeric(A) ||
    inherits(A, "dMatrix") ||
    inherits(A, "iMatrix")

  if (!type_ok) {
    stop("La matrice doit contenir des valeurs numériques.")
  }

  # Vérifier l'absence de valeurs manquantes ou infinies
  if (anyNA(A) || any(!is.finite(A))) {
    stop("La matrice contient des valeurs non valides.")
  }


  # Vérifier la symétrie de la matrice
if (!isTRUE(Matrix::isSymmetric(A, tol = tol))) {
  stop("La matrice doit être symétrique.")
}

  return(TRUE)
}


