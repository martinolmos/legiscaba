# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(httr)

ua <- user_agent("http://github.com/martinolmos/legiscaba")

getExpediente <- function(IdProyectoTipo = '',
                          IdAutoresInternos = '',
                          IdUbicacion = '',
                          IdEstado = '',
                          Sumario = '',
                          SumarioExacto = '0',
                          FechaDesde = '',
                          FechaHasta = '',
                          AnioParlamentario = '',
                          Limite = '') {
        base_url <- 'https://parlamentaria.legislatura.gov.ar'

        path <- 'webservices/Json.asmx/GetExpedienteAvanzada'

        query <- list(
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

        myurl <- modify_url(url = base_url,
                            path = path,
                            query = query)

        resp <- GET(url = myurl, ua)

        if(http_error(resp) != 200) {
                stop(
                        sprintf(
                                "Falló el requerimiento a la API [%s]\n%s\n<%s>",
                                status_code(resp),
                                parsed$message
                        ),
                        call. = FALSE
                )
        }

        if (http_type(resp) != "text/xml") {
                stop("la API no retornó un xml", call. = FALSE)
        }

        parsed <- xml2::read_xml(content(resp))

        structure(list(
                content = parsed,
                path = path,
                response = resp
        ),
        class = "expediente")
}

print.expediente <- function(x, ...) {
        cat("<Parlamentaria ", x$path, "\n", sep = "")
        str(x$content)
        invisible(x)
}

f <- function(x = c("apple", "banana", "orange")) {
        match.arg(x)
}

f("x")
