
#' Title
#'
#' @param df A dataframe
#' @param x A character
#' @param y A character
#' @param fill A character
#' @param xlab A character
#' @param ylab A character
#'
#' @return A ggplot object
#'
#' @examples
#' df_test = data.frame(x1 = c(2012, 2012, 2013, 2013, 2014, 2014),
#'                      y1 = c(30, 40, 42, 67, 19, 21),
#'                      fill1 = c("new", "old", "new", "new", "old", "new"))
#' graph_bar_participation(df = df_test, x = "x1", y = "y1", fill = "fill1",
#'                         lab_title = "New/Old", ylab = "Nombre")
#'                         
graph_bar_participation <- function(df, x = "year", y = NA, color_bar = NA,
                                    fill = "anciennete_transect",
                                    lab_title = "Ancienneté transect",
                                    xlab = "Années", ylab = "Nombre de transects",
                                    stat = "identity", position = "stack",
                                    values_fill = c('#009ef8', '#76ea02'),
                                    breaks_fill = c("New", "Old"),
                                    modif_x_axis = FALSE,
                                    fix_ratio = FALSE, max_ab = 10, c_fix = 0.15){
  if (is.na(y)) {
    gg <- ggplot(df, aes(x = !!sym(x), fill = !!sym(fill)))
  }else{
    gg <- ggplot(df, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill)))
  }
  
  gg <- gg +
    geom_bar(stat = stat, position = position, color = color_bar)+
    scale_fill_manual(values=values_fill, breaks=breaks_fill)+
    ylab(ylab) +
    xlab(xlab) +
    labs(fill = lab_title) +
    theme_cowplot()
  
  if (modif_x_axis) {
    gg <- gg +
      theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) 
  }
  
  if (fix_ratio) {
    gg <- gg +
      ylim(0, max_ab)+
      coord_fixed(ratio = c_fix)
  }
  
  return(gg)
}


#' Title
#'
#' @param df 
#' @param x 
#' @param y 
#' @param group 
#' @param color 
#' @param size_p 
#' @param vec_limits_x 
#' @param limits_y 
#' @param labels_y 
#' @param xlab 
#' @param ylab 
#' @param grid 
#' @param grid_symbol 
#'
#' @return
#' @export
#'
#' @examples
suivi_temporel_transects <- function(df, x = "session_year", y = "placette_id", group = "groupe",
                                     color = "#aa3839", size_p = 1.5, vec_limits_x,
                                     limits_y=c(), labels_y=c(), xlab = "Années",
                                     ylab = "Nom du transect", grid = FALSE){
  # Data frame pour avoir les points aux extrémités des lignes
  df_points <- df %>%
    group_by(!!sym(group)) %>%
    filter(!!sym(x) == min(!!sym(x)) | !!sym(x) == max(!!sym(x))) %>%
    ungroup()
  
  gg <- ggplot(df, aes(x = !!sym(x), y = !!sym(y))) +
    geom_line(aes(group = !!sym(group)), color = color) +
    geom_point(data = df_points, mapping = aes(x = !!sym(x), y = !!sym(y)),
               size = size_p, color = color) +
    scale_x_continuous(limits = c(min(as.integer(vec_limits_x), na.rm = T)-1,
                                  max(as.integer(vec_limits_x), na.rm = T)+1)) +
    theme_minimal() + xlab(xlab) + ylab(ylab)
  
  if (grid & ("site" %in% colnames(df))) {
    gg <- gg +
      facet_grid(site ~ ., scales = "free_y") +
      scale_x_continuous(limits = c(min(as.integer(vec_limits_x), na.rm = T)-1,
                                    max(as.integer(vec_limits_x), na.rm = T)+1),
                         sec.axis = dup_axis()) +
      theme(strip.text.y = element_text(angle = 0),
            axis.text.x.top = element_text())
  }else if(length(limits_y)>0 & length(labels_y)>0){
    gg <- gg + scale_y_discrete(limits = limits_y, labels = labels_y)
  }
  
  return(gg)
}
