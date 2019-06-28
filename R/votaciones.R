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
        votacionesToDF(parsed, IdExpediente)

}

#' Función que parsea las votaciones de un expediente de XML a tibble
#'
#' @param Votaciones expediente cabeza de un expediente en formato XML
#' @importFrom dplyr %>%


votacionesToDF <- function(Votaciones, IdExpediente) {
        votaciones <- list()

        xml2::xml_ns_strip(Votaciones)

        Votaciones <- Votaciones %>% xml2::xml_find_all("//VotacionExpediente")

        for (i in seq_along(Votaciones)) {
                votaciones[[i]] <- list()
                votaciones[[i]][[1]] <- Votaciones[[i]] %>% {
                        dplyr::tibble(
                                id_expediente = IdExpediente,
                                afirmativos = xml2::xml_child(.,"afirmativos") %>%
                                        xml2::xml_text(),
                                negativos = xml2::xml_child(.,"negativos") %>%
                                        xml2::xml_text(),
                                abstenciones = xml2::xml_child(.,"abstenciones") %>%
                                        xml2::xml_text(),
                                sin_votar = xml2::xml_child(.,"sin_votar") %>%
                                        xml2::xml_text(),
                                id_votacion = xml2::xml_child(.,"id_votacion") %>%
                                        xml2::xml_text(),
                                asunto = xml2::xml_child(.,"asunto") %>%
                                        xml2::xml_text(),
                                id_sesion = xml2::xml_child(.,"id_sesion") %>%
                                        xml2::xml_text(),
                                fch_sesion = xml2::xml_child(.,"fch_sesion") %>%
                                        xml2::xml_text(),
                                tipo_sesion = xml2::xml_child(.,"tipo_sesion") %>%
                                        xml2::xml_text(),
                                desc_sesion = xml2::xml_child(.,"desc_sesion") %>%
                                        xml2::xml_text(),
                                presidente_sesion = xml2::xml_child(.,"presidente_sesion") %>%
                                        xml2::xml_text(),
                                secretarios_sesion = xml2::xml_child(.,"secretarios_sesion") %>%
                                        xml2::xml_text()
                        )
                }

                votosxbloque <- Votaciones %>%
                        xml2::xml_find_all("//VotosxBloque")
                votosxbloque <- votosxbloque[[i]]

                votaciones[[i]][[2]] <- votosxbloque %>%
                        xml2::xml_children() %>% {
                                dplyr::tibble(
                                        id_expediente = IdExpediente,
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
                                                xml2::xml_text(),
                                        id_votacion = votaciones[[i]][[1]]$id_votacion,
                                        id_sesion = votaciones[[i]][[1]]$id_sesion,
                                        fch_sesion = votaciones[[i]][[1]]$fch_sesion,
                                        asunto = votaciones[[i]][[1]]$asunto
                                )
                        }

                votosxlegislador <- Votaciones %>%
                        xml2::xml_find_all("//VotosxLegislador")
                votosxlegislador <- votosxlegislador[[i]]

                votaciones[[i]][[3]] <- votosxlegislador %>%
                        xml2::xml_children() %>% {
                                dplyr::tibble(
                                        id_expediente = IdExpediente,
                                        id_legilador = xml2::xml_child(.,"id_legilador") %>%
                                                xml2::xml_text(),
                                        apellido = xml2::xml_child(.,"apellido") %>%
                                                xml2::xml_text(),
                                        nombre = xml2::xml_child(.,"nombre") %>%
                                                xml2::xml_text(),
                                        id_bloque = xml2::xml_child(.,"id_bloque") %>%
                                                xml2::xml_text(),
                                        bloque = xml2::xml_child(.,"bloque") %>%
                                                xml2::xml_text(),
                                        presencia = xml2::xml_child(.,"presencia") %>%
                                                xml2::xml_text(),
                                        voto = xml2::xml_child(.,"voto") %>%
                                                xml2::xml_text(),
                                        id_votacion = votaciones[[i]][[1]]$id_votacion,
                                        id_sesion = votaciones[[i]][[1]]$id_sesion,
                                        fch_sesion = votaciones[[i]][[1]]$fch_sesion,
                                        asunto = votaciones[[i]][[1]]$asunto
                                )
                        }
                names(votaciones[[i]]) <- c("Resultado General",
                                            "Votos x Bloque",
                                            "Votos x Legislador")

        }


        return(votaciones)
}

