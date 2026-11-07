
reconstruire_temperature <- function(
    solution,
    n = 20,
    haut = 100,
    bas = 20,
    gauche = 20,
    droite = 20) {

  # Vérifier les dimensions
  if (length(solution) != n * n) {
    stop("La taille de la solution est incorrecte.")
  }

  # Créer la grille complète
  T <- matrix(NA_real_, nrow = n + 2, ncol = n + 2)

  # Définir les températures aux frontières
  T[1, ] <- haut
  T[n + 2, ] <- bas
  T[, 1] <- gauche
  T[, n + 2] <- droite

  # Reconstituer les températures intérieures
  interieur <- matrix(solution, nrow = n, ncol = n)

  T[2:(n + 1), 2:(n + 1)] <- t(interieur[, n:1])

  return(T)
}
