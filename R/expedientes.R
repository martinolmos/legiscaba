#' Función que hace un requerimiento de expedientes
#'
#' @param IdProyectoTipo integer, ID del tipo de proyecto. Ver getTiposProyecto
#' @param IdAutoresInternos integer, ID de el/la autor/a # debería haber un lugar donde buscar el id por nombre
#' @param IdUbicacion integer, ID de la ubicación
#' @param IdEstado integer, ID del Estado
#' @param Sumario character, Texto a buscar en el sumario
#' @param SumarioExacto integer, Macheo exacto: 0 para Falso y 1 para Verdadero
#' @param FechaDesde character, Acotar búsqueda desde fecha. Formato dd/mm/aaaa
#' @param FechaHasta character, Acotar búqueda hasta fecha. Formato dd/mm/aaaa
#' @param AnioParlamentario integer, Acotar la búsqueda a un año. Formato aaaa
#' @param Limite integer, Limitar la respuesta a un número de registros
#' @export

getExpediente <- function(IdProyectoTipo = "", IdAutoresInternos = "",
                          IdUbicacion = "", IdEstado = "", Sumario = "",
                          SumarioExacto = "0", FechaDesde = "", FechaHasta = "",
                          AnioParlamentario = "", Limite = "") {

    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteAvanzada"

    query <- list(IdProyectoTipo = IdProyectoTipo, IdAutoresInternos = IdAutoresInternos,
        IdUbicacion = IdUbicacion, IdEstado = IdEstado, Sumario = Sumario, SumarioExacto = SumarioExacto,
        FechaDesde = FechaDesde, FechaHasta = FechaHasta, AnioParlamentario = AnioParlamentario,
        Limite = Limite)

    myurl <- httr::modify_url(url = base_url, path = path, query = query)

    ua <- httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(
            sprintf(
                "Falló el requerimiento a la API [%s]\n%s",
                httr::status_code(resp)),
            call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)

    expedienteToDF(parsed)
}


#' Función que parsea un expediente de XML a tibble
#'
#' @param Expediente expediente en formato XML
#' @importFrom dplyr %>%

expedienteToDF <- function(Expediente = expediente) {
    xml2::xml_ns_strip(Expediente)

    rows <- Expediente %>%
        xml2::xml_find_all("//expedienteAvanzado") %>%
        purrr::map(~ xml2::xml_find_all(., "*"))

    rows_df <- dplyr::tibble(row = seq_along(rows),
                             nodeset = rows)

    rows_df %>%
        dplyr::mutate(col_name_raw = nodeset %>%
                          purrr::map(~ xml2::xml_name(.)),
                      cell_text = nodeset %>%
                          purrr::map(~ xml2::xml_text(.)),
                      i = nodeset %>%
                          purrr::map(~ seq_along(.))) %>%
        dplyr::select(row, i, col_name_raw, cell_text) %>%
        tidyr::unnest() %>%
        dplyr::select(-i) %>%
        tidyr::spread(key = col_name_raw, value = cell_text) %>%
        dplyr::select(-row)

}

#' Función para requerir los giros de un expediente
#'
#' @param IdExpediente Integer Número de ID del expediente
#' @export

getExpedienteGiros <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteGiros"

    query <- list(IdExpediente = IdExpediente)

    myurl <- httr::modify_url(url = base_url, path = path, query = query)

    ua <- httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(
            sprintf(
                "Falló el requerimiento a la API [%s]\n%s",
                httr::status_code(resp)),
            call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    giroToDF(parsed)

}

#' Función que parsea un giro de XML a tibble
#'
#' @param Giro giro en formato XML
giroToDF <- function(Giro) {
    xml2::xml_ns_strip(Giro)

    rows <- Giro %>%
        xml2::xml_find_all("//expedienteGiros") %>%
        purrr::map(~ xml2::xml_find_all(., "*"))

    rows_df <- dplyr::tibble(row = seq_along(rows),
                             nodeset = rows)

    rows_df %>%
        dplyr::mutate(col_name_raw = nodeset %>%
                          purrr::map(~ xml2::xml_name(.)),
                      cell_text = nodeset %>%
                          purrr::map(~ xml2::xml_text(.)),
                      i = nodeset %>%
                          purrr::map(~ seq_along(.))) %>%
        dplyr::select(row, i, col_name_raw, cell_text) %>%
        tidyr::unnest() %>%
        dplyr::select(-i) %>%
        tidyr::spread(key = col_name_raw, value = cell_text) %>%
        dplyr::select(-row)

}
