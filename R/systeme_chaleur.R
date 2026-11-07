
construire_systeme_chaleur <- function(
    n = 20,
    haut = 100,
    bas = 20,
    gauche = 20,
    droite = 20) {

  # Vérification des paramètres
  if (length(n) != 1 || !is.finite(n) ||
      n < 1 || n != as.integer(n)) {
    stop("n doit être un entier positif.")
  }

  temperatures <- c(haut, bas, gauche, droite)

  if (!is.numeric(temperatures) ||
      any(!is.finite(temperatures))) {
    stop("Les températures doivent être numériques et finies.")
  }

  # Nombre total d'inconnues
  N <- n * n

  # Pas spatial pour une plaque de 1 m x 1 m
  h <- 1 / (n + 1)

  # Initialisation de la matrice et du vecteur
  lignes <- integer()
  colonnes <- integer()
  valeurs <- numeric()

  b <- numeric(N)

  # Parcours des nœuds intérieurs
  for (j in 1:n) {
    for (i in 1:n) {

      # Numéro de l'inconnue
      k <- (j - 1) * n + i

      # Coefficient diagonal
      lignes <- c(lignes, k)
      colonnes <- c(colonnes, k)
      valeurs <- c(valeurs, 4)

      # Voisin gauche
      if (i > 1) {
        lignes <- c(lignes, k)
        colonnes <- c(colonnes, k - 1)
        valeurs <- c(valeurs, -1)
      } else {
        b[k] <- b[k] + gauche
      }

      # Voisin droit
      if (i < n) {
        lignes <- c(lignes, k)
        colonnes <- c(colonnes, k + 1)
        valeurs <- c(valeurs, -1)
      } else {
        b[k] <- b[k] + droite
      }

      # Voisin inférieur
      if (j > 1) {
        lignes <- c(lignes, k)
        colonnes <- c(colonnes, k - n)
        valeurs <- c(valeurs, -1)
      } else {
        b[k] <- b[k] + bas
      }

      # Voisin supérieur
      if (j < n) {
        lignes <- c(lignes, k)
        colonnes <- c(colonnes, k + n)
        valeurs <- c(valeurs, -1)
      } else {
        b[k] <- b[k] + haut
      }
    }
  }

  # Construction de la matrice creuse
  A <- Matrix::sparseMatrix(
    i = lignes,
    j = colonnes,
    x = valeurs,
    dims = c(N, N)
  )

  # Retourner le système thermique
  return(list(
    A = A,
    b = b,
    n = n,
    h = h
  ))
}
