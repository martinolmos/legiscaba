#' Funcion que transforma un documento XML con una estructura padre-hijos a un tibble
#'
#' @description Parsea una relación de "un solo piso", de un solo padre a múltiples hijos
#' @param xml_doc
#'
#' @return Un objeto de clase "tibble"

children_to_df <- function(xml_doc) {

    rows <- xml_doc %>%
        xml2::xml_children() %>%
        purrr::map( ~ xml2::xml_find_all(., "*"))

    rows_df <- dplyr::tibble(row = seq_along(rows),
                             nodeset = rows)

    rows_df %>%
        dplyr::mutate(
            col_name_raw = nodeset %>%
                purrr::map( ~ xml2::xml_name(.)),
            cell_text = nodeset %>%
                purrr::map( ~ xml2::xml_text(.)),
            item = nodeset %>%
                purrr::map( ~ seq_along(.))) %>%
        dplyr::select(row, item, col_name_raw, cell_text) %>%
        tidyr::unnest() %>%
        dplyr::select(-item) %>%
        tidyr::spread(key = col_name_raw, value = cell_text) %>%
        dplyr::select(-row)

}

#' Parsea las columnas de fecha-hora a POSIXct
#'
#' @description Esta función elimina los puntos en "a.m." y "p.m." que impiden interpretar el vector como POSIXct y lo transforma a esa clase
#'
#' @param character
#'
#' @return Vector de clase POSIXct

fix_timestamp <- function(ts) {
    ts %>%
        gsub("a.m.", "am", .) %>%
        gsub("p.m.", "pm", .) %>%
        lubridate::dmy_hms(tz = "America/Buenos_Aires")
}
