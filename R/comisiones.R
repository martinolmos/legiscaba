#' Listado completo de comisiones donde esta activo un Diputado
#'
#' @param id_legislador integer (Requerido) ID de un legislador.
#'
#' @return Objeto de clase "tibble"
#'
#' @examples
#' prueba_comisionesPorDiputado <- getComisionesPorDiputado(30651)
#'
#' @author Martin Olmos, \email{molmos@@itba.edu.ar}
#'
#' @export

getComisionesPorDiputado <- function(id_legislador) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetComisionesPorDiputado"

    query <- list(id_legislador = id_legislador)

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
    comiPorDipToDF(parsed)
}

#' Parsea documento XML con datos de las comisiones que integra un Diputado a tibble
#'
#' @param comi_por_dip documento-XML con datos de las comisiones que integra un Diputado

comiPorDipToDF <- function(comi_por_dip) {
    xml2::xml_ns_strip(comi_por_dip)

    children_to_df(comi_por_dip) %>%
        dplyr::mutate(fch_desde = fix_timestamp(fch_desde),
                      fch_hasta = fix_timestamp(fch_hasta)) %>%
        dplyr::select(id_legislador,
                      apellido_legislador,
                      nombre_legislador,
                      id_autor,
                      nombre_cargo,
                      id_comision,
                      comision,
                      fch_desde,
                      fch_hasta
                      )

}
