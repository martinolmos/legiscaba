#' Función que hace un requerimiento de expedientes
#'
#' @param IdProyectoTipo integer, ID del tipo de proyecto. Ver getTiposProyecto
#' @param IdAutoresInternos integer, ID de el/la autor/a
#' @param IdUbicacion integer, ID de la ubicacion
#' @param IdEstado integer, ID del Estado
#' @param Sumario character, Texto a buscar en el sumario
#' @param SumarioExacto integer, (Requerido) Macheo exacto: 0 para Falso y 1 para Verdadero
#' @param FechaDesde character, Acota busqueda desde una fecha. Formato dd/mm/aaaa
#' @param FechaHasta character, Acota buqueda hasta una fecha. Formato dd/mm/aaaa
#' @param AnioParlamentario integer, Acota búsqueda a un año. Formato aaaa
#' @param Limite integer, Limita la respuesta a un numero de registros
#'
#' @return Un objeto de clase "tibble"
#'
#' @examples
#' prueba_expediente <- getExpediente(IdProyectoTipo = 1, SumarioExacto = 0, Sumario = "planeamiento", FechaDesde = "01/03/2018", FechaHasta = "01/12/2018")
#'
#' @export

getExpediente <-
    function(IdProyectoTipo = "",
             IdAutoresInternos = "",
             IdUbicacion = "",
             IdEstado = "",
             Sumario = "",
             SumarioExacto = "0",
             FechaDesde = "",
             FechaHasta = "",
             AnioParlamentario = "",
             Limite = "") {
        base_url <- "https://parlamentaria.legislatura.gov.ar"

        path <- "webservices/Json.asmx/GetExpedienteAvanzada"

        query <-
            list(
                IdProyectoTipo = IdProyectoTipo,
                IdAutoresInternos = IdAutoresInternos,
                IdUbicacion = IdUbicacion,
                IdEstado = IdEstado,
                Sumario = Sumario,
                SumarioExacto = SumarioExacto,
                FechaDesde = FechaDesde,
                FechaHasta = FechaHasta,
                AnioParlamentario = AnioParlamentario,
                Limite = Limite
            )

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

        expedienteToDF(parsed)
    }


#' Función que parsea un expediente de XML a tibble
#'
#' @param Expediente expediente en formato XML
#'
#' @importFrom dplyr %>%

expedienteToDF <- function(Expediente = expediente) {
    xml2::xml_ns_strip(Expediente)

    children_to_df(Expediente) %>%
        dplyr::mutate(fch_inicio = fix_timestamp(fch_inicio),
                      fch_movimiento = fix_timestamp(fch_movimiento)) %>%
        dplyr::select(id_expediente,
                      nro_de_expediente,
                      nro_de_orden_JefeGob,
                      fch_inicio,
                      autor_id,
                      autor_des,
                      coautores_id,
                      coautores_des,
                      id_proyecto_tipo,
                      tipo_proyecto_des,
                      descripcion,
                      sumario,
                      TieneSancion,
                      ubicacion_des,
                      fch_movimiento,
                      urlDoc,
                      id_business_party)
}

#' Funcion para requerir los giros de un expediente
#'
#' @param IdExpediente Integer Numero de ID del expediente
#'
#' @return Un objeto de clase "tibble"
#'
#' @examples
#' prueba_giros <- getExpedienteGiros(75640)
#'
#' @export

getExpedienteGiros <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteGiros"

    query <- list(IdExpediente = as.integer(IdExpediente))

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
    girosToDF(parsed)

}

#' Función que parsea un giro de XML a tibble
#'
#' @param Giros giros de un expediente en formato XML

girosToDF <- function(Giros) {
    xml2::xml_ns_strip(Giros)
    children_to_df(Giros) %>%
        dplyr::select(id_expediente,
                      orden,
                      id_comision,
                      comision_des,
                      comision_url,
                      expediente_giro_tipo_des)
}

#' Funcion para requerir los movimientos de un expediente
#'
#' @param IdExpediente integer Numero de ID de un expediente
#'
#' @return Un objeto de clase "tibble"
#'
#' @examples
#' prueba_movs <- getExpMovimientos(75640)
#'
#' @export

getExpMovimientos <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteMovimientos"

    query <- list(IdExpediente = as.integer(IdExpediente))

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
    moviToDF(parsed)

}

#' Función que parsea los movimientos de un expediente de XML a tibble
#'
#' @param Movimientos movimientos de un expediente en formato XML


moviToDF <- function(Movimientos) {
    xml2::xml_ns_strip(Movimientos)
    children_to_df(Movimientos) %>%
        dplyr::mutate(fch_movimiento = fix_timestamp(fch_movimiento)) %>%
        dplyr::select(id_expediente,
                      fch_movimiento,
                      ubicacion_des,
                      descripcion)
}

#' Funcion que requiere el expediente cabeza de un expediente
#'
#' @param IdExpediente integer ID de un expediente
#'
#' @return Un objeto de clase "tibble"
#'
#' @examples
#' prueba_cabeza <- getExpedienteCabeza(98985)
#'
#' @export

getExpedienteCabeza <- function(IdExpediente) {
    base_url <- "https://parlamentaria.legislatura.gov.ar"

    path <- "webservices/Json.asmx/GetExpedienteCabeza"

    query <- list(IdExpediente = as.integer(IdExpediente))

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
    expCabezaToDF(parsed)

}

#' Función que parsea el expediente cabeza de un expediente de XML a tibble
#'
#' @param ExpCabeza expediente cabeza de un expediente en formato XML


expCabezaToDF <- function(ExpCabeza) {

    xml2::xml_ns_strip(ExpCabeza)
    children_to_df(ExpCabeza) %>%
        dplyr::rename(cabeza_id_expediente = id_expediente)
}
