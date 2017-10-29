
#' Pepare tts text
#'
#' This function takes 
#' @param text The text to be coverted.
#' @param character_ratio The sensitivity with which lines with numbers and punctuation are excluded.
#' @keywords tts
#' @export
#' @examples
#' cat_function()
#' 


prepare_tts_text <-  function(text, character_ratio = .60){
  text %>%   
    stringr::str_split("(\r\n)|(\n)") %>% 
    unlist %>% 
    dplyr::data_frame(text = .) %>% 
    dplyr::mutate(row_number = row_number()) %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "[:space:]+"," ")) %>% 
 #  dplyr::mutate(text = stringr::str_replace_all(text, "\\.+"," ")) %>% 
    dplyr::mutate(az_ratio = stringr::str_count(text,"[A-Za-z]") / nchar(text) ) %>% 
    dplyr::filter(az_ratio > character_ratio ) %>% .$text %>% paste(collapse = " ")}

