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

# Test para getDiputadosActivos
prueba_dipuActivos <- getDiputadosActivos(82)

# Test para getDiputadosHistorico
prueba_dipuHistorico <- getDiputadosHistorico(30650)

# Test para getComisionesPorDiputado
prueba_comisionesPorDiputado <- getComisionesPorDiputado(30651)
