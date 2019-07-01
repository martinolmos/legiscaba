#' Trae los Diputados con mandato actual
#'
#' @param id_bloque integer (opcional) ID de un bloque parlamentario. En caso de estar vacio trae todos los bloques.
#'
#' @return Objeto de clase "tibble"
#'
#' @examples
#' prueba_dipuActivos <- getDiputadosActivos(82)
#'
#' @author Martin Olmos, \email{molmos@@itba.edu.ar}
#'
#' @export

getDiputadosActivos <- function(id_bloque = "") {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetDiputadosActivosNuevo"

    query <- list(id_bloque = id_bloque)

    myurl <-
        httr::modify_url(url = base_url,
                         path = path,
                         query = query)

    ua <-
        httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(sprintf(
            "Falló el requerimiento a la API [%s]",
            httr::status_code(resp)
        ),
        call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    dipuActivosToDF(parsed)
}

#' Parsea documento XML con datos de Diputados activos a tibble
#'
#' @param dipu_activos documento-XML con datos de Diputados activos

dipuActivosToDF <- function(dipu_activos) {
    xml2::xml_ns_strip(dipu_activos)

    children_to_df(dipu_activos) %>%
        dplyr::mutate(fecha_inicio_mandato = lubridate::dmy(fecha_inicio_mandato),
                      fecha_fin_mandato = lubridate::dmy(fecha_fin_mandato)) %>%
        dplyr::select(id_legislador,
                      apellido,
                      nombre,
                      id_autor,
                      id_sexo,
                      sexo,
                      fecha_inicio_mandato,
                      fecha_fin_mandato,
                      id_bloque,
                      bloque,
                      bloque_logo,
                      bloque_color,
                      id_bloque_cargo_tipo,
                      cargo_bloque,
                      id_legislador_reemplazo,
                      reemplaza_a)
}

#' Trae el listado historico de legisladores y autores (Pueden no ser legisladores)
#'
#' @return Objeto de clase "tibble"
#'
#' @examples
#' prueba_dipuHistorico <- getDiputadosHistorico()
#'
#' @author Martin Olmos, \email{molmos@@itba.edu.ar}
#'
#' @export

getDiputadosHistorico <- function() {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetDiputadosHistorico"

    query <- list()

    myurl <-
        httr::modify_url(url = base_url,
                         path = path,
                         query = query)

    ua <-
        httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(sprintf(
            "Falló el requerimiento a la API [%s]",
            httr::status_code(resp)
        ),
        call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    dipuHistoricoToDF(parsed)
}

#' Parsea documento XML con datos de Diputados activos a tibble
#'
#' @param dipu_historico documento-XML con datos de Diputados activos

dipuHistoricoToDF <- function(dipu_historico) {
    xml2::xml_ns_strip(dipu_historico)

    dipu_historico %>%
        xml2::xml_find_all("//Listado") %>%
        children_to_df() %>%
        dplyr::select(id_legislador,
                      apellido,
                      nombre,
                      id_autor,
                      cantidad_exptes_autor,
                      cantidad_exptes_coautor,
                      cantidad_mandatos)
}
