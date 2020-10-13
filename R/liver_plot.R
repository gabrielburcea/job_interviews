#' liver_plot
#'
#' @param data 
#' @param start_date 
#' @param end_date 
#' @param plot_chart 
#'
#' @return
#' @export 
#' @importFrom magrittr %>%
#'
#' @examples
liver_plot <- function(data, start_date = as.Date("2020-04-09", tz = "Europe/London"), end_date = as.Date("2020-08-20"), plot_chart = TRUE){
  
  
  comorb_cov_sympt_num <- cvindia::comorbidities_symptoms(data, start_date,  end_date,  plot_chart = FALSE)
  
  liver_num <- comorb_cov_sympt_num %>%
    dplyr::filter(Morbidity == "Liver disease(long term)") %>%
    dplyr::arrange(desc(Count))
  
  
  title_stub <- "Long-Standing Liver disease across symptoms\n"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(title_stub, start_date_title, " to ", end_date_title)
  
  
  plot_liver_sympt <-
    ggplot2::ggplot(liver_num, ggplot2::aes(x = Symptom, y = Count, fill = Count)) +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
    ggplot2::scale_x_discrete(limits = unique(liver_num$Symptom)) +
    #ggplot2::theme(legend.position = "bottom") +
    #ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3)) +
    ggplot2::theme_minimal() +
    ggplot2::labs( title = chart_title,
                   subtitle = "Counts of patients with comorbidities accross symptoms",
                   y = "Counts",
                   x = "Symptoms",
                   caption = "Source: Dataset - Your.md Dataset") +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 0,
        r = 21,
        b = 0,
        l = 0
      )),
      plot.title = ggplot2::element_text(size = 10, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 55, hjust = 1)
    )
  
  if(plot_chart == TRUE) {
    
    
    plot_liver_sympt
  }else{
    
    liver_num
    
  }
  
  
}
