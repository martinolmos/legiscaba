#' Función que hace un requerimiento de expedientes
#'
#'

ua <- httr::user_agent("http://github.com/martinolmos/legiscaba")

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

    resp <- httr::GET(url = myurl, ua)

        if (httr::http_type(resp) != "text/xml") {
        stop("la API no retornó un xml", call. = FALSE)
    }

    parsed <- httr::content(resp)

    if (httr::http_error(resp)) {
        stop(sprintf("Falló el requerimiento a la API [%s]\n%s\n<%s>", httr::status_code(resp),
                     parsed$message), call. = FALSE)
    }

    structure(list(content = parsed, path = path, response = resp), class = "expediente")
}

print.expediente <- function(x, ...) {
    cat("<Parlamentaria ", x$path, "\n", sep = "")
    str(x$content)
    invisible(x)

}
