library(testthat)
library(legiscaba)

test_check("legiscaba")
# Test para getExpediente
prueba_expediente <- getExpediente(IdProyectoTipo = 1, SumarioExacto = 0, Sumario = "planeamiento", FechaDesde = "01/03/2018", FechaHasta = "01/12/2018")

# Test para getExpedienteGiros
prueba_giros <- getExpedienteGiros(75640)

# Test para getExpMovimientos
prueba_movs <- getExpMovimientos(75640)

# Test para getExpedienteCabeza
prueba_cabeza <- getExpedienteCabeza(98985)

# Test para getVotacionesExpediente
prueba_votaciones <- getVotacionesExpediente(112158)
