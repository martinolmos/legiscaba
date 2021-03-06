% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expedientes.R
\name{getExpediente}
\alias{getExpediente}
\title{Función que hace un requerimiento de expedientes}
\usage{
getExpediente(IdProyectoTipo = "", IdAutoresInternos = "",
  IdUbicacion = "", IdEstado = "", Sumario = "",
  SumarioExacto = "0", FechaDesde = "", FechaHasta = "",
  AnioParlamentario = "", Limite = "")
}
\arguments{
\item{IdProyectoTipo}{integer, ID del tipo de proyecto. Ver getTiposProyecto}

\item{IdAutoresInternos}{integer, ID de el/la autor/a}

\item{IdUbicacion}{integer, ID de la ubicacion}

\item{IdEstado}{integer, ID del Estado}

\item{Sumario}{character, Texto a buscar en el sumario}

\item{SumarioExacto}{integer, (Requerido) Macheo exacto: 0 para Falso y 1 para Verdadero}

\item{FechaDesde}{character, Acota busqueda desde una fecha. Formato dd/mm/aaaa}

\item{FechaHasta}{character, Acota buqueda hasta una fecha. Formato dd/mm/aaaa}

\item{AnioParlamentario}{integer, Acota búsqueda a un año. Formato aaaa}

\item{Limite}{integer, Limita la respuesta a un numero de registros}
}
\value{
Un objeto de clase "tibble"
}
\description{
Función que hace un requerimiento de expedientes
}
\examples{
prueba_expediente <- getExpediente(IdProyectoTipo = 1, SumarioExacto = 0, Sumario = "planeamiento", FechaDesde = "01/03/2018", FechaHasta = "01/12/2018")

}
\author{
Martin Olmos, \email{molmos@itba.edu.ar}
}
