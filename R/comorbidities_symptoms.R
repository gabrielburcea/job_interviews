#' comorbidities_symptoms - symptom count over comorbidities 
#'
#' @param data 
#' @param start_date 
#' @param end_date 
#' @param plot_chart 
#'
#' @return
#' @export
#'
#' @examples
comorbidities_symptoms <- function(data, start_date = as.Date("2020-04-09", tz = "Europe/London"),
                                   end_date = as.Date("2020-05-06",tz = "Europe/London"), plot_chart = TRUE){
  
  
  comorb_divided <- data %>%
      dplyr::select(id, covid_tested, shortness_breath, muscle_ache, cough, loss_smell_taste, chills, diarrhoea, fatigue, headache, nasal_congestion, 
                    nausea_vomiting,sore_throat, sputum, temperature, loss_appetite, sneezing, chest_pain, itchy_eyes, joint_pain, 
                    asthma, diabetes_type_one, diabetes_type_two, obesity, hypertension, heart_disease, lung_condition, liver_disease, kidney_disease) %>%
      dplyr::filter(covid_tested != "negative")
    
    data_piv <- comorb_divided %>% 
      tidyr::pivot_longer(cols = 21:29,  names_to = "Morbidity", values_to = "Yes_No") %>%
      dplyr::filter(Yes_No != "No") %>%
      tidyr::pivot_longer(cols=3:20, names_to="Symptom", values_to="Event") %>%
      dplyr::filter(Event != "No") %>%
      dplyr::group_by(Symptom, Morbidity) %>%
      dplyr::summarise(Count= dplyr::n()) %>%
      dplyr::mutate(Percentage= Count/sum(Count) *100)
    
    symptom_levels <- c('Shorthness of breath' = "shortness_breath", 
                        'Muscle ache' = "muscle_ache", 
                        "Cough" = "cough", 
                        "Loss of smell and taste" = "loss_smell_taste", 
                        "Chills" = "chills", 
                        "Diarrhoea" = "diarrhoea", 
                        "Fatigue" = "fatigue", 
                        "Headache" = "headache", 
                        "Nasal congestion" = "nasal_congestion", 
                        "Nausea and vomiting" = "nausea_vomiting", 
                        "Sore throat" = "sore_throat", 
                        "Sputum" = "sputum", 
                        "Temperature" = "temperature", 
                        "Loss of appetite" = "loss_appetite", 
                        "Sneezing" = "sneezing", 
                        "Chest pain" = "chest_pain", 
                        "Itchy eyes" = "itchy_eyes", 
                        "Joint pain" = "joint_pain")
    
    morbidity_levels <- c("Asthma" = "asthma", 
                          "Diabetes type 1" = "diabetes_type_one", 
                          "Diabetes type 2" = "diabetes_type_two", 
                          "Obesity" = "obesity", 
                          "Hypertension" = "hypertension", 
                          "Heart disease" = "heart_disease", 
                          "Lung condition(long term)" = "lung_condition", 
                          "Kidney disease(long term)" = "kidney_disease", 
                          "Liver disease(long term)" = "liver_disease")
    
    
    data_piv_levels <- data_piv %>%
      dplyr::mutate(Symptom = forcats::fct_recode(Symptom, !!!symptom_levels), 
                    Morbidity = forcats::fct_recode(Morbidity, !!!morbidity_levels)) %>%
      dplyr::select(Symptom, Morbidity, Count, Percentage)

  
  
  title_stub <- "Comorbidities across Covid-19 Symptoms, %"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(title_stub, start_date_title, " to ", end_date_title)
  
  
  
  plot_comorb_cov_sympt <-
    ggplot2::ggplot(data_piv_levels, ggplot2::aes(x = Morbidity, Percentage, fill = Symptom)) +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_x_discrete(limits = unique(data_piv_levels$Morbidity)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3)) +
    ggplot2::theme_minimal() +
    ggplot2::labs( title = chart_title,
                   subtitle = "Symptom profile accross comorbidities in respondents tested positive/showing symptoms",
                   y = "Percentage",
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
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text.x = ggplot2::element_text(angle = 55, hjust = 1)
    )
  

  
  if(plot_chart == TRUE){
    
    plot_comorb_cov_sympt
    
  }else{
    
    data_piv_levels
  }
  
}


























