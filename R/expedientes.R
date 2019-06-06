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
                "Falló el requerimiento a la API [%s]",
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

#' Funcion para requerir los giros de un expediente
#'
#' @param IdExpediente Integer Numero de ID del expediente
#' @export

getExpedienteGiros <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteGiros"

    query <- list(IdExpediente = as.integer(IdExpediente))

    myurl <- httr::modify_url(url = base_url, path = path, query = query)

    ua <- httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(
            sprintf(
                "Falló el requerimiento a la API [%s]",
                httr::status_code(resp)),
            call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    girosToDF(parsed)

}

#' Función que parsea un giro de XML a tibble
#'
#' @param Giros giros de un expediente en formato XML

girosToDF <- function(Giros) {
    xml2::xml_ns_strip(Giros)

    Giros %>%
        xml2::xml_find_all("//expedienteGiros") %>%
        purrr::map_df(~ dplyr::tibble(
            id_expediente = xml2::xml_child(.,"id_expediente") %>%
                xml2::xml_text(),
            orden = xml2::xml_child(.,"orden") %>%
                xml2::xml_text(),
            expediente_giro_tipo_des = xml2::xml_child(., "expediente_giro_tipo_des") %>%
                xml2::xml_text(),
            id_comision = xml2::xml_child(.,"id_comision") %>%
                xml2::xml_text(),
            comision_des = xml2::xml_child(.,"comision_des") %>%
                xml2::xml_text(),
            comision_url = xml2::xml_child(.,"comision_url") %>%
                xml2::xml_text()))

}

#' Funcion para requerir los movimientos de un expediente
#'
#' @param IdExpediente integer Numero de ID de un expediente
#' @export

getExpMovimientos <- function(IdExpediente){
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteMovimientos"

    query <- list(IdExpediente = as.integer(IdExpediente))

    myurl <- httr::modify_url(url = base_url, path = path, query = query)

    ua <- httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(
            sprintf(
                "Falló el requerimiento a la API [%s]",
                httr::status_code(resp)),
            call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    moviToDF(parsed)

}

#' Función que parsea los movimientos de un expediente de XML a tibble
#'
#' @param Movimientos movimientos de un expediente en formato XML


moviToDF <- function(Movimientos) {

    xml2::xml_ns_strip(Movimientos)

    Movimientos %>%
        xml2::xml_find_all("//expedienteMovimientos") %>%
        purrr::map_df(~dplyr::tibble(
            id_expediente = xml2::xml_child(.,"id_expediente") %>%
                xml2::xml_text(),
            fch_movimiento = xml2::xml_child(.,"fch_movimiento") %>%
                xml2::xml_text(),
            ubicacion_des = xml2::xml_child(., "ubicacion_des") %>%
                xml2::xml_text(),
            descripcion = xml2::xml_child(.,"descripcion") %>%
                xml2::xml_text()))

}

#' Funcion que requiere el expediente cabeza de un expediente
#'
#' @param IdExpediente integer ID de un expediente
#' @export

getExpedienteCabeza <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteCabeza"

    query <- list(IdExpediente = as.integer(IdExpediente))

    myurl <- httr::modify_url(url = base_url, path = path, query = query)

    ua <- httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(
            sprintf(
                "Falló el requerimiento a la API [%s]",
                httr::status_code(resp)),
            call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    expCabezaToDF(parsed)

}

#' Función que parsea el expediente cabeza de un expediente de XML a tibble
#'
#' @param ExpCabeza expediente cabeza de un expediente en formato XML


expCabezaToDF <- function(ExpCabeza) {

    xml2::xml_ns_strip(ExpCabeza)

    ExpCabeza %>%
        xml2::xml_find_all("//expedienteCabeza") %>%
        purrr::map_df(~dplyr::tibble(
            id_expediente_cabeza = xml2::xml_child(.,"id_expediente") %>%
                xml2::xml_text(),
            id_expediente_agregado = xml2::xml_child(.,"agregado_id_expediente") %>%
                xml2::xml_text(),
            nro_expediente_cabeza = xml2::xml_child(.,"cabeza_nro_de_expediente") %>%
                xml2::xml_text()))

}

#' Funcion que requiere las votaciones de un expediente
#'
#' @param IdExpediente integer ID de un expediente
#' @export

getVotacionesExpediente <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetVotacionesExpediente"

    query <- list(IdExpediente = as.integer(IdExpediente))

    myurl <- httr::modify_url(url = base_url, path = path, query = query)

    ua <- httr::user_agent("https://github.com/martinolmos/legiscaba")

    resp <- httr::GET(url = myurl, ua)

    if (httr::status_code(resp) != 200) {
        stop(
            sprintf(
                "Falló el requerimiento a la API [%s]",
                httr::status_code(resp)),
            call. = FALSE)
    }

    if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)
    votacionesToDF(parsed)

}

#' Función que parsea las votaciones de un expediente de XML a tibble
#'
#' @param Votaciones expediente cabeza de un expediente en formato XML


votacionesToDF <- function(Votaciones) {
    votaciones <- list()

    xml2::xml_ns_strip(Votaciones)

    votaciones$general <- Votaciones %>%
        xml2::xml_find_first("//VotacionExpediente") %>%
        purrr::map_df(~dplyr::tibble(
            afirmativos = xml2::xml_child(.,"afirmativos") %>%
                xml2::xml_text(),
            negativos = xml2::xml_child(.,"negativos") %>%
                xml2::xml_text(),
            abstenciones = xml2::xml_child(.,"abstenciones") %>%
                xml2::xml_text()),
            sin_votar = xml2::xml_child(.,"sin_votar"),
            id_votacion = xml2::xml_child(.,"id_votacion"),
            asunto = xml2::xml_child(.,"asunto"),
            id_sesion = xml2::xml_child(.,"id_sesion"),
            fch_sesion = xml2::xml_child(.,"fch_sesion"),
            tipo_sesion = xml2::xml_child(.,"tipo_sesion"),
            desc_sesion = xml2::xml_child(.,"desc_sesion"),
            presidente_sesion = xml2::xml_child(.,"presidente_sesion"),
            secretarios_sesion = xml2::xml_child(.,"secretarios_sesion"))

    votaciones$por_bloque <- Votaciones %>%
        xml2::xml_find_first("//VotacionExpediente") %>%
        xml2::xml_find_all("//VotosBloque") %>%
        purrr::map_df(~dplyr::tibble(
            afirmativos = xml2::xml_child(.,"afirmativos") %>%
                xml2::xml_text(),
            negativos = xml2::xml_child(.,"negativos") %>%
                xml2::xml_text(),
            abstenciones = xml2::xml_child(.,"abstenciones") %>%
                xml2::xml_text(),
            sin_votar = xml2::xml_child(.,"sin_votar") %>%
                xml2::xml_text(),
            id_bloque = xml2::xml_child(.,"id_bloque") %>%
                xml2::xml_text(),
            bloque = xml2::xml_child(.,"bloque") %>%
                xml2::xml_text()
        ))

    votaciones$por_legislador <- Votaciones %>%
        xml2::xml_find_first("//VotacionExpediente") %>%
        xml2::xml_find_all("//VotoLegislador") %>%
        purrr::map_df(~dplyr::tibble(
            id_legilador = xml2::xml_child(.,"id_legilador") %>%
                xml2::xml_text(),
            apellido = xml2::xml_child(.,"apellido") %>%
                xml2::xml_text(),
            nombre = xml2::xml_child(.,"nombre") %>%
                xml2::xml_child(),
            id_bloque = xml2::xml_child(.,"id_bloque") %>%
                xml2::xml_text(),
            bloque = xml2::xml_child(.,"bloque") %>%
                xml2::xml_text(),
            presencia = xml2::xml_child(.,"presencia") %>%
                xml2::xml_text(),
            voto = xml2::xml_child(.,"voto") %>%
                xml2::xml_text()
        ))
    return(votaciones)
}

