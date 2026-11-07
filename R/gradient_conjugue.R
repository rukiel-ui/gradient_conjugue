
gradient_conjugue <- function(A, b, tol = 1e-8,
                              max_iter = 1000, x0 = NULL) {

  # Vérification de la matrice
  verifier_matrice(A)

  # Vérification du vecteur b
  if (!is.numeric(b) || !is.null(dim(b))) {
    stop("b doit être un vecteur numérique.")
  }

  if (length(b) != nrow(A)) {
    stop("Les dimensions de A et b sont incompatibles.")
  }

  if (anyNA(b) || any(!is.finite(b))) {
    stop("b contient des valeurs non valides.")
  }

  # Vérification des paramètres
  if (length(tol) != 1 || !is.finite(tol) || tol <= 0) {
    stop("tol doit être un nombre strictement positif.")
  }

  if (length(max_iter) != 1 || !is.finite(max_iter) ||
      max_iter < 1 || max_iter != as.integer(max_iter)) {
    stop("max_iter doit être un entier positif.")
  }

  # Initialisation
  n <- length(b)

  if (is.null(x0)) {
    x <- rep(0, n)
  } else {
    if (!is.numeric(x0) || length(x0) != n ||
        anyNA(x0) || any(!is.finite(x0))) {
      stop("x0 doit être un vecteur numérique valide de taille n.")
    }
    x <- as.numeric(x0)
  }

  r <- as.numeric(b - A %*% x)
  p <- r

  norme_b <- sqrt(sum(b^2))
  norme_r <- sqrt(sum(r^2))

  # Gestion du cas b = 0
  denom_b <- if (norme_b == 0) 1 else norme_b

  historique <- norme_r / denom_b
  converged <- historique[1] <= tol
  iterations <- 0
  raison <- if (converged) "Convergence initiale" else "Maximum d'iterations atteint"

  # Boucle du gradient conjugué
  if (!converged) {

    for (k in seq_len(max_iter)) {

      Ap <- as.numeric(A %*% p)
      denom <- sum(p * Ap)

      if (!is.finite(denom) || denom <= 0) {
        stop("A n'est pas définie positive ou le calcul est instable.")
      }

      alpha <- sum(r * r) / denom

      x <- x + alpha * p
      r_new <- r - alpha * Ap

      norme_r <- sqrt(sum(r_new^2))
      erreur_relative <- norme_r / denom_b

      historique <- c(historique, erreur_relative)
      iterations <- k

      if (erreur_relative <= tol) {
        converged <- TRUE
        raison <- "Tolerance atteinte"
        r <- r_new
        break
      }

      beta <- sum(r_new^2) / sum(r^2)

      p <- r_new + beta * p
      r <- r_new
    }
  }

  # Résultats
  return(list(
    solution = as.numeric(x),
    iterations = iterations,
    residu_final = historique[length(historique)],
    historique = historique,
    converged = converged,
    raison = raison
  ))
}
