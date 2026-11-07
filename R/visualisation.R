
# ==========================================
# 1. CARTE THERMIQUE DE LA PLAQUE
# ==========================================

visualiser_temperature <- function(T) {

  if (!is.matrix(T) || !is.numeric(T)) {
    stop("T doit être une matrice numérique.")
  }

  if (anyNA(T) || any(!is.finite(T))) {
    stop("La matrice contient des valeurs non valides.")
  }

  # Préparer les données pour ggplot2
  donnees <- expand.grid(
    x = seq_len(ncol(T)),
    y = seq_len(nrow(T))
  )

  donnees$temperature <- as.vector(T)

  # Construire la carte thermique
  graphique <- ggplot2::ggplot(
    donnees,
    ggplot2::aes(
      x = x,
      y = y,
      fill = temperature
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(
      name = "Température (°C)"
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Distribution de la chaleur dans une plaque métallique",
      subtitle = "Simulation par l'algorithme du gradient conjugué",
      x = "Position horizontale",
      y = "Position verticale"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5
      )
    )

  return(graphique)
}


# ==========================================
# 2. COURBE DE CONVERGENCE
# ==========================================

visualiser_convergence <- function(resultat) {

  if (is.null(resultat$historique) ||
      length(resultat$historique) == 0) {
    stop("Historique de convergence absent.")
  }

  donnees <- data.frame(
    iteration = seq_along(resultat$historique) - 1,
    residu = resultat$historique
  )

  graphique <- ggplot2::ggplot(
    donnees,
    ggplot2::aes(
      x = iteration,
      y = residu
    )
  ) +
    ggplot2::geom_line(linewidth = 1, color = "blue") +
    ggplot2::geom_point(size = 2, color = "red") +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = "Convergence de l'algorithme du gradient conjugué",
      x = "Nombre d'itérations",
      y = "Résidu relatif (échelle logarithmique)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      )
    )

  return(graphique)
}
