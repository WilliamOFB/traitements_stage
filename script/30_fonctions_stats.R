# --------------------------- début var_quanti() -------------------------
# --------------------------------------- #
#  GRAPHIQUES
# --------------------------------------- #

#' Produire un graphe de densités personnalisé pour les variables quantitatives
#'
#' @param data Le dataframe contenant les données.
#' @param x Variable pour l'abscisse.
#' @param x_lab,y_lab Caractère. Etiquette des axes.
#' @param caption Caractère. Légende des données.
#' @param log Booléen. L'axe des ordonnées doit-il être en échelle log ? Par défaut FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' var_quanti(data = massdo,
#' x = valeur,
#' x_lab = "Valeur",
#' y_lab = "Densité")
#' }
#' 
var_quanti <- function(data,
                       x,
                       x_lab = NULL,
                       y_lab = NULL,
                       caption = NULL,
                       log = FALSE)
  
{

  g <- ggplot(data = data,
              aes(x = {{ x }})) +
    geom_density(fill = "blue",
                 alpha = 0.1) +
    facet_wrap(~ variable,
               scales = "free") +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 13.5),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)))
  
  # gestion des étiquettes des axes
  if(!is.null(x_lab)) {g <- g + labs(x = x_lab) }
  if(!is.null(y_lab)) {g <- g + labs(y = y_lab) }
  if(!is.null(caption)) {g <- g + labs(caption = caption) }
  if(log == TRUE) {g <- g + scale_x_log10() }
  
  
  # affichage
  g
  
}

# --------------------------- fin var_quanti() -------------------------

# --------------------------- début var_quali() -------------------------
# --------------------------------------- #
#  GRAPHIQUES
# --------------------------------------- #

#' Produire un graphe en barres personnalisé pour les variables qualitatives
#'
#' @param data Le dataframe contenant les données.
#' @param x Variable pour l'abscisse.
#' @param x_lab,y_lab Caractère. Etiquette des axes.
#' @param caption Caractère. Légende des données.
#' @param ordre_x Caractère. Dans quel ordre doit être ordonné l'axe des x ? Par défaut "alphabetic".
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' var_quanti(data = massdo,
#' x = top_geol,
#' x_lab = "Type de géologie",
#' y_lab = "Occurence",
#' ordre_x = "frequence")
#' }
#' 
var_quali <- function(data,
                      x,
                      x_lab = NULL,
                      y_lab = NULL,
                      caption = NULL,
                      ordre_x = "alphabetic")

{
  if(ordre_x == "alphabetic") {
  
  data <- data %>% 
    mutate({{x}} := fct_rev(as.factor({{x}}))) %>% 
    filter(!is.na({{x}}))
  }
  
  if(ordre_x == "frequence") {data <- data %>% 
    mutate({{x}} := fct_infreq(as.factor({{x}}))) %>% 
    filter(!is.na({{x}}))
  }
  
  
  g <- ggplot(data = data,
              aes(x = {{ x }})) +
    geom_bar() +
    coord_flip() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 13.5),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)))
  
  # gestion des étiquettes des axes
  if(!is.null(x_lab)) {g <- g + labs(x = x_lab) }
  if(!is.null(y_lab)) {g <- g + labs(y = y_lab) }
  if(!is.null(caption)) {g <- g + labs(caption = caption) }
  
  # affichage
  g
  
}

# --------------------------- fin var_quali() -------------------------