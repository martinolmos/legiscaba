## legiscaba: Cliente R (no oficial) del webservice de la Legislatura de la Ciudad de Buenos Aires

## Descripción General

*legiscaba* es un envoltorio simple del webservice de la Secretaría Parlamentaria de la Legislatura de la Ciudad de Buenos Aires. Permite realizar requerimientos de datos en forma sencilla y se ocupa de parsear los resultados de xml a data.frame.

Está pensado para hacerle la vida un poco más sencilla y ahorrarles un poco de trabajo a colegas politólogos o politólogas que estén interesados en analizar estos datos.

## Instalación

Para instalar el paquete desde R se necesita tener instalado `devtools`:

```{r}
if(!require(devtools)) install.packages("devtools")

devtools::install_github("martinolmos/legiscaba")
```

## Uso básico

### getExpediente()

```{r}
prueba_expediente <- getExpediente(IdProyectoTipo = 1, SumarioExacto = 0, Sumario = "planeamiento", FechaDesde = "01/03/2018", FechaHasta = "01/12/2018")

prueba_expediente
```

### getExpedienteGiros() 

```{r}
prueba_giros <- getExpedienteGiros(75640)

prueba_giros
```

### getExpMovimientos()

```{r}
prueba_movs <- getExpMovimientos(75640)

prueba_movs
```

### getExpedienteCabeza()

```{r}
prueba_cabeza <- getExpedienteCabeza(98985)

prueba_cabeza
```

### getVotacionesExpediente()

```{r}
prueba_votaciones <- getVotacionesExpediente(112158)

prueba_votaciones
```
