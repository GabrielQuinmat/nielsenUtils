#' nielsenUtils: Paqueteria de funciones utilitarias comunes
#' en GDDC Brasil Nielsen Mexico
#'
#' Esa paqueteria contiene un conjunto de funciones, variables y procesos
#' que son comunes en distintos procesos y tareas de GDDC Brasil Nielsen,
#' por lo que se engloban en esta paqueteria para poder mantenerse de
#' forma facil y compartirse con el equipo.
#'
#' @author Gabriel Quintanar \email{yahirgabriel.quintanarmartinez@nielsen.com}
#' @docType package
#' @name nielsenUtils
"_PACKAGE"


#' Carga o Instala Paqueterias
#'
#' Se encarga de hacer carga de todas las paqueterias contenidas
#' en _pkgs_. Se encarga de instalar cualquier paqueteria no instalada.
#'
#' @param pkgs Vector de caracteres con el nombre de paqueterias a cargar.
#' @return No hay retorno, solo hay una descarga de elementos
#' @export
#'
#' @examples
#' \dontrun{ load_pkgs(c("data.table", "purrr", "stringr", "futile.logger",
#' "foreach", "tidyr", "haven", "openxlsx",
#' "future", "furrr", "glue", "bit64", "feather"))
#' }
load_pkgs <- function(pkgs){
  for(pkg in pkgs){
    tryCatch({
      if(require(pkg, character.only = TRUE))
        library(pkg, character.only = TRUE)
      else{
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      }
    },
    error = function(e) message(e))
  }
}

# Variables de Clase -----------------------------------------------------------

#' #' @description Se utiliza como diccionario para eliminar caracteres especiales
#' special_chrs <- c('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'a'='A', 'Â'='A',
#'                  'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'e'='E',
#'                  'Ê'='E', 'Ë'='E', 'Ì'='I', 'i'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N',
#'                  'Ò'='O', 'o'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
#'                  'u'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a',
#'                  'a'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
#'                  'è'='e', 'e'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'i'='i', 'î'='i',
#'                  'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'o'='o', 'ô'='o', 'õ'='o',
#'                  'ö'='o', 'ø'='o', 'ù'='u', 'u'='u', 'û'='u', 'ý'='y', 'ý'='y',
#'                  'þ'='b', 'ÿ'='y' )

#' Vector de caracteres especiales
#'
#' Vector con nombre, que conserva todos los caracteres especiales existentes
#' en codificacion Latin-1. En el nombre contiene todos sus valores estandarizados.
#'
#' @format Vector con caracteres especiales:
#' \describe{
#'   \item{special_chrs}{Valores de alfabeto especial}
#' }
#' @source Creado
"special_chrs"


# Enumerados de Tipo -----------------------------------------------------------



# tipo_conveniencia <- 63
# tipo_autoservicio <- 0:5
# tipo_autoservicio_completo <- c(tipo_autoservicio, tipo_conveniencia)
# tipo_trad_favela <- 40
# tipo_tradicionales <- c(6:11)
# tipo_bar_favela <- 41
# tipo_bares <- c(12:15)
# tipo_horecas <- c(31:34)
# tipo_bares_completo <- c(tipo_bares, tipo_horecas)
# tipo_farmacias <- c(16, 17)
# tipo_perfumeria <- c(18, 19)
# tipo_farma.cosmetico <- c(tipo_farmacias, tipo_perfumeria)
# tipo_petshop <- c(51:53)
# tipo_cash_carry <- c(111)
# tipo_papeleria <- c(24:28, 88)
#
# tipo_horecas_marginal <- 35
# tipo_libreria_marginal <- 29
# tipo_papeleria_marginal <- 89
# tipo_petagro_marginal <- 54
# tipo_loja_departamento <- 60
# tipo_marginales <- c(tipo_horecas_marginal, tipo_libreria_marginal,
#                      tipo_papeleria_marginal, tipo_petagro_marginal, 109)
# tipo_reporteables <- c(tipo_autoservicio_completo, tipo_tradicionales,
#                        tipo_bares_completo, tipo_farma.cosmetico, tipo_cash_carry)
#
#
#
# # Productos --------------------------------------------------------------------
# grupo_a_products <- c("GA_CAFE", "GA_CALDO", "GA_CARNE", "GA_ENLAT", "GAIOGURT",
#                       "ALIMEN_INF", "CEREAL_MAT", "GALEITEP", "GAMARGAR", "GA_MASSA",
#                       "GA_COM_ALI", "GAAZEITE", "GA_PUDINS", "GA_SOPAS", "CHA_PRON",
#                       "SUCOS", "ACUCAR", "ARROZ", "BOLACHAS", "FARINHAS", "FEIJAO",
#                       "SAL_DE_COZ", "LEITE_ASSE", "GA_EXT_TOM", "GA_BACHO",
#                       "CEREAL_BAR", "EDULCORANT")
#
# grupo_b_products <- c("GB_BALAS", "GBCHOCOL", "CIGARRO", "GBGOMASK", "GB_SALGAD",
#                       "PRES_BEB_ALC", "CERVEJA", "REFRIG", "SORVETE", "AGUA_MINER",
#                       "CIGA_PRESE", "ALCOOL")
#
# usopes_products <- c("UPABSHIG","UPAPDESC","UP_BRONZ","UP_CDENT","UPDESODO","UP_FRALDAS","UP_P_HIG",
#                      "UP_SABON","UPSHAMPO","UPTINTUR","REMEDIOS")
#
# ciga_products <- c("CIGA_OUTRO", "CIGA_PMORR", "CIGA_SCRUZ", "CIGA_PRESE")
#
# congsorv_products <- c("CONG_KIBOM", "CONG_NESTL", "CONG_OUTRO", "CONG_PROPR")
#
# bebalc_products <- c("CHOPP", "APERITIV", "CHAMPAGN", "CONHAQUE", "GIM", "LICOR",
#                      "PINGA", "RUM", "VINHO", "VODKA", "WHISKY")
#
# all_products <- c(grupo_a_products, grupo_b_products, usopes_products, ciga_products,
#                   congsorv_products, bebalc_products) %>% unique
#
# quantity_vars <- c("CKOUT", "NLOJAS", "PVCLIVET", "PCLOCAL", "AVENDAS",
#                    "NMESA", "FUNC", "INTEGRAL", "NLOJAS", "GRUPOA", "GRUPOB",
#                    "USOPES")
# Expresiones Regulares --------------------------------------------------------------------

rvars_regex <- "\\p{L}*\\w+\\p{L}*(?=\\s*(==|>=*|<=*|%in%|%notin%|!=)\\s*)"

# Funciones --------------------------------------------------------------------

#' Evalua Columnas Duplicadas en un data.table
#'
#' Evalua columnas duplicadas en un data.table, producto de un merge, para
#' determinar si son iguales de acuerdo a su contenido y su posicion..
#' Busca columnas duplicadas de acuerdo a los nombres que terminen con '.x' o '.y'.
#'
#' @param dt Data.table a evaluar sus columnas.
#' @param suffixes vector de solo dos elementos, donde se especifica como se llaman
#' las columnas duplicadas.
#' @return Data.table con columnas evaluadas con terminacion '.equal'
#' @export
eval_duplicate_columns <- function(dt, suffixes = c(".x", ".y")){
  dups <- grep(glue("\\{suffixes[1]}$|\\{suffixes[2]}$"), names(dt), value = T)
  x.vars <- grep(glue("\\{suffixes[1]}$"), names(dt), value = T) %>% sort()
  y.vars <- grep(glue("\\{suffixes[2]}$"), names(dt), value = T) %>% sort()
  assure_type(dt, x.vars, y.vars)
  originals <- str_remove_all(dups, glue("\\{suffixes[1]}$|\\{suffixes[2]}$")) %>% unique()
  pb <- txtProgressBar(max = length(originals), style = 3)
  for(i in 1:length(originals)){
    var1 <- paste0(originals[i], suffixes[1]); var2 <- paste0(originals[i], suffixes[2])
    new.var <- paste0(originals[i], ".equal")
    dt[, (new.var) := get(var1) == get(var2)]
    setTxtProgressBar(pb, value = i)
  }
  return(dt)
}

#' Evalua Columnas .equal
#'
#' Realiza una verificacion logica entre columnas con terminacion .equal
#'
#' @param DT data.table resultado de la funcion _eval_duplicate_columns_,
#' con columnas .equal.
#' @return Lista de subsets de DT, donde cada indice es una de las columnas evaluadas,
#' retornando solo resultados no iguales.
#'
#' @export
eval_equal_columns <- function(DT){
  eq.vars <- grep("\\.equal$", names(DT), value = TRUE)
  originals <- str_remove_all(eq.vars, ".equal") %>% unique()
  result <- map2(eq.vars, originals,
                 ~DT[(!get(.x)), paste0(.y, c(".x", ".y")), with = FALSE]) %>%
    set_names(originals)
  result
}

#' Castea variables entre data.frames
#'
#' Se encarga de castear variables al mismo tipo. Si keep.y esta
#' activado, var se castea al tipo  de var2, de lo contrario var2 se castea al
#' tipo de var.
#'
#' @note Solo castea a valores primitivos.
#'
#' @param DT Data.table a realizar el casteo de variables
#' @param var Vector de caracteres correspondiente a variables a castear.
#' @param var2 Vector de caracteres correspondiente a variables a castear.
#' @param keep.y Si se quiere conservar el tipo de var2 (TRUE) o de var (FALSE)
#'
#' @return DT con variables casteadas.
#'
#' @export
assure_type <- function(DT, var, var2, keep.y = TRUE){
  types_comp <- map2_lgl(var, var2,
                         ~DT[, class(get(.x)) != class(get(.y))]) %>% set_names(var)
  x.vars <- DT[, lapply(.SD, class), .SDcols = var[types_comp]]
  y.vars <- DT[, lapply(.SD, class), .SDcols = var2[types_comp]]

  x.class <- paste0("as.", x.vars)
  y.class <- paste0("as.", y.vars)
  if(keep.y)
    walk2(var[types_comp], y.class, ~DT[,
                                        (.x) := exec(.y, get(.x))])
  else
    walk2(var2[types_comp], x.class, ~DT[,
                                         (.x) := exec(.y, get(.x))])
}


#' Aplica un merge a una lista
#'
#' Version simplificada de **iterative_merge** pero que usa solo una lista
#' de data.tables para iterar sobre si misma. Esta funcion, a diferencia de su original
#' pretende eliminar generaciones de parametros grandes. Conteniendo a su vez la misma
#' funcion de limpieza de columnas duplicadas y una sola llave compartida.
#'
#' @param list.dt lista con data.tables a unificar
#' @param key caracter con el nombre de la llave que comparten todos los data.tables
#' @param ... parametros extras a ingresar en \code{Reduce}
merge.list <- function(list.dt, key, ...){
  Reduce(f = function(x, y) merge(x, y, by = key, ...),
         x = list.dt)
}

#' Merge iterativo
#'
#' Realiza merges consecutivos teniendo como primera referencia refDT
#' por cada data.table contenido en list.DT, tomando como llaves
#' cada indice de list.keys. A refDT se le aplica un merge del primer elemento de
#' listDT, usando como llave el primer elemento de list.keys y asi consecutivamente.
#'
#' @param refDT data.table de referencia.
#' @param list.DT Lista de data.table's para aplicar en refDT.
#' @param list.keys Lista de vectores de caracteres correspondiente a las llaves.
#' @param ... Calquier parametro extra a aplicar en 'merge'.
#' @param keep.y Se utiliza para mantener prioridad a la nueva informacion entrante.
#' Esto debido a que se usa como parametro dentro del flujo de la funcion, en las
#' funciones 'prepare_dts', 'merge_substitute', 'rid_duplicate_columns'.
#' @param solve_conflict decide si los conflictos en cada merge se resuelven
#' o bien manteniendo la ultima columna ingresada o substituyendo en donde el valor
#' sea nuevo.
#' @return data.table resultado de todos los merge.
#' @export
iterative.merge <- function(refDT, list.DT, list.keys, ..., keep.y = T,
                            solve_conflict = c("erase", "substitute")){
  pb <- txtProgressBar(max = length(list.DT), style = 3)
  for(i in 1:length(list.DT)){
    if(!(list.keys[[i]] %in% names(refDT) & list.keys[[i]] %in% names(list.DT[[i]]))){
      log_msg(glue("Llave {list.keys[[i]]} no existe en ambos DT's, saltando"))
      next
    }
    dts <- prepare_dts(refDT, list.DT[[i]], list.keys[[i]], keep.y)
    refDT <- dts$ref;
    dt <- dts$dt
    switch(solve_conflict,
           "erase" = refDT <- merge(refDT, dt, by = list.keys[[i]], ...),
           'substitute' = refDT <- merge_substitute(refDT, dt, list.keys[[i]]),
           stop("Tipo de merge escogido no valido")
    )
    refDT <- rid_duplicate_columns(refDT, keep.y)
    setTxtProgressBar(pb, value = i)
  }
  refDT
}

#' Merge subtitutivo
#'
#' Realiza una subtitucion de valores dado un merge. Se utiliza para
#' evitar duplicidad de columnas con nombres iguales y si solo se quiere modificar
#' variables en comun dado otro data.table.
#'
#' @param DT1 data.table de referencia
#' @param DT2 data.table catalogo, o de quien se tomaran los nuevos valores
#' @param unique.key Vector de caracteres usados como llaves.
#' @param keep.y Si se mantendra prioridad a DT2 para substituir los valores de DT1
#'
#' @return data.table con valores de llave en comun subtituidos
#'
#' @export
merge_substitute <- function(DT1, DT2, unique.key, keep.y = T){
  dts <- prepare_dts(DT1, DT2, unique.key, keep.y)
  DT1 <- dts$ref; DT2 <- dts$dt
  outerDT <- DT1[!DT2, on = unique.key]
  exclude.vars <- intersect(names(DT1)[names(DT1) %notin% unique.key],
                            names(DT2))
  innerDT <- merge(DT1[, -exclude.vars, with = F], DT2, by = unique.key)
  finalDT <- rbind(outerDT, innerDT, fill = TRUE)
  finalDT
}

#' Prepara data.tables para un merge
#'
#'
#' Prepara dataframes para la funcion __recursive_merge__. Este
#' procesamiento consiste solo en revisar llaves para el merge para formatear
#' las columnas al mismo tipo; tambien se encarga de quitar columnas del DT
#' de referencia y conservar otras columnas del DT a unir.
#'
#' @param ref DT de referencia.
#' @param dt DT a unir.
#' @param keys Vector de caracteres correspondiente a las llaves a utilizar en merge.
#' @return Objeto lista con datatables ref y dt y las llaves.
#'
#' @export
prepare_dts <- function(xDT, yDT, keys, keep.y = T){
  assure_type_DTs(xDT, yDT, keys, keep.y)
  return(list(ref = xDT, dt = yDT, keys = keys))
}

#' Asegura tipos entre data.tables
#'
#' Se encarga de realizar el casteo entre dos data.tables de las
#' variables contenidas en _keys_.
#'
#' @param xDT data.table en la izquierda
#' @param yDT data.table en la derecha
#' @param keys Vector de caracteres correspondiente a llaves a castear.
#' @param keep.y Si se da prioridad a yDT (TRUE) o a xDT(FALSE) en el casteo.
#'
#' @return data.tables xDT y yDT con llaves casteadas.
#'
#' @export
assure_type_DTs <- function(xDT, yDT, keys, keep.y = T){
  x.class <- paste0("as.", xDT[, lapply(.SD, class), .SDcols = keys])
  y.class <- paste0("as.", yDT[, lapply(.SD, class), .SDcols = keys])
  if(keep.y)
    walk2(keys, y.class, ~xDT[,
                              (.x) := exec(.y, get(.x))])
  else
    walk2(keys, x.class, ~yDT[,
                              (.x) := exec(.y, get(.x))])
}


#' Elimina columnas duplicadas
#'
#' Unifica columnas duplicadas debido al merge, cuya extension se vuelve
#' '.x' y '.y'. Mantiene prioridad a la columna con terminacion '.y' ya que es quien
#' contiene informacion nueva.
#' @note Se puede generar un nuevo parametro 'priority' para determinar quien tiene la
#' relevancia.
#'
#' @param dt data.table al que se le acaba de aplicar un merge y resulto con
#' columnas duplicadas
#' @param keep.y valor logico para decidir si se conserva el valor nuevo \code{TRUE}
#' o el valor viejo \code{FALSE}
#'
#' @return data.table sin columnas duplicadas por merge
#' @export
rid_duplicate_columns <- function(dt, keep.y){
  dups <- names(dt)[names(dt) %like% ".x$|.y$"]
  originals <- str_remove_all(dups, ".x$|.y$") %>% unique()
  if(length(originals) > 0){
    log_msg("Columns duplicated by merge:")
    log_msg(paste(originals, collapse = ","))
  }
  for(i in originals){
    var1 <- paste0(i, ".x"); var2 <- paste0(i, ".y")
    if(keep.y)
      dt[, (i) := if(keep.y) get(var2) else get(var1)]
    dt <- dt[, -(c(var1, var2)), with = F]
  }
  dt
}

#' Unifica columnas
#'
#' Unifica los valores de columnas duplicadas resultado de un merge.
#' @param dt data.table ca aplicar funcion
#' @param keep.y Si se mantiene prioridad en columna 2 (TRUE) o en columna 1 (FALSE)
#' @param suffixes Vector de 2 caracteres que representan los sufijos de las columnas
#' duplicadas. Por default es '.x' y '.y' ya que es el resultado por defecto de merge.
#'
#' @return data.table sin columnas duplicadas
#'
#' @export
merge_columns <- function(dt, keep.y = TRUE, suffixes = c(".x", ".y")){
  dups <- grep(glue("\\{suffixes[1]}$|\\{suffixes[2]}$"),
               names(dt), value = TRUE)
  originals <- str_remove_all(dups, glue("\\{suffixes[1]}$|\\{suffixes[2]}$")) %>%
    unique()
  if(length(originals) > 0){
    log_msg("Columns duplicated by merge:")
    log_msg(originals)
  }
  pb <- txtProgressBar(max = length(originals), style = 3)

  for(i in originals){
    var1 <- paste0(i, suffixes[1]); var2 <- paste0(i, suffixes[2])
    var.eq <- paste0(i, ".equal")
    if(keep.y)
      dt[, (i) := pmap_chr(list(get(var1), get(var2)),
                           function(var1, var2){
                             if(keep.y){
                               if(is.na(var2)) return(var1)
                               else return(var2)
                             } else {
                               if(is.na(var1)) return(var2)
                               else return(var1)
                             }
                           })]
    dt <- dt[, -(c(var1, var2)), with = F]
  }
  dt
}

#' Lee cualquier archivo de datos como data.table
#'
#' Se encarga de leer un archivo compatible (texto, excel o SAS),
#' al leer la terminacion del archivo. A su vez, convierte a mayusculas todos
#' los headers y elimina caracteres especiales.
#'
#' @param path Ruta del archivo a leer.
#' @param col_class Se puede delegar la transformacion de columnas a la paqueteria
#' correspondiente con 'auto', o se pueden convertir todas a String con 'chr' y
#' despues castear.
#' @param ... Cualquier parametro extra que se pueda usar en 'read_sas', 'read.xlsx',
#' 'fread', 'readRDS' y 'read_feather'.
#' @param .verbose valor logico que define si se mandaran mensajes logs acerca
#' de la lectura y transformacion de datos.
#'
#' @return data.table resultante.
#'
#' @import haven
#' @import openxlsx
#' @import magrittr
#' @import data.table
#' @import feather
#' @import stringr
#'
#' @export
read.dt <- function(path, col_class = 'auto', .verbose = F,  ...){
  if(.verbose) log_msg(paste("Leyendo data:", path))
  if(stringr::str_detect(path, ".sas7bdat$"))
    dt <- haven::read_sas(path) %>% data.table()
  else if(stringr::str_detect(path, "\\.xls$|\\.xlsx$"))
    dt <- openxlsx::read.xlsx(xlsxFile = path, ...) %>% data.table()
  else if(stringr::str_detect(path, "\\.csv$|\\.txt$|\\.tsv$"))
    dt <- data.table::fread(path,...)
  else if(stringr::str_detect(path, regex("\\.rds$", ignore_case = T)))
    dt <- readRDS(path) %>% data.table
  else if(stringr::str_detect(path, regex("\\.feather$", ignore_case = T)))
    dt <- feather::read_feather(path) %>% data.table
  else stop("Tipo de archivo no soportado")

  if(.verbose) log_msg("Estandarizando headers")

  data.table::setnames(dt, toupper(names(dt)))
  dt <- dt[, (unique(names(dt))), with = F]

  if(.verbose) log_msg("Limpiando caracteres especiales")

  if(col_class == 'chr')
    dt[, (names(dt)) := lapply(.SD, as.character)] %>%
    .[, (names(dt)) := lapply(.SD, stringr::str_replace_all, special_chrs)]
  else if(col_class == 'auto'){
    chr.names <- names(dt)[dt[, sapply(.SD, is.character, USE.NAMES = F)]]
    dt[, (chr.names) := lapply(.SD, stringr::str_replace_all, special_chrs),
       .SDcols = chr.names]
  }
  if(.verbose) log_msg("Data leida.")
  return(dt)
}

#' Imprime una verificacion de duplicacion
#'
#' Imprime informacion de tuplas repetidas por generacion de merge en el
#' sistema de loggeo.
#'
#' @param dt data.table a verificar
#' @param key llave a usar para verificar
#'
#' @return No hay retorno, solo mensajes a consola
print.dup.verification <- function(dt, key){
  if(nrow(dt) != uniqueN(dt)){
    message("WARNING: Numero de tuplas y combinaciones unicas de llave no coinciden")
    try(print(dt[, .N, by = key][N > 1]), silent = T)
  }
}

#' Limpia data.table de duplicados
#'
#' Dado un merge con elementos de llave duplicados, se encarga de
#' mantener solo la ultima coincidencia de los casos permutados por repeticion.
#' @note Se puede agregar un nuevo parametro 'keep', para determinar que elemento
#' conservar.
#' @param dt data.table a limpiar
#' @param keys llaves que conforman cada grupo unico
#'
#' @return DT sin duplicados.
#' @export
clean.by.keys <- function(dt, keys){
  lojas.dup <- dt[, .N, by = keys] %>% .[N > 1, ..keys]
  to.save <- dt[lojas.dup, on = keys] %>% .[, .SD[.N], by = keys]
  dt <- dt[!lojas.dup, on = keys]
  dt <- rbind(dt, to.save)
  return(dt)
}

#' Inicializa un sistema de loggeo
#'
#' Inicializa un doble sistema de loggeo. Uno para un archivo, con nombre
#' logger.log, y otro para consola. El sistema de loggeo por archivo se inicializa
#' en la carpeta del working directory.
#' @param log.file Define el nombre del archivo .log, por defecto es logger.log
#'
#' @return No hay retorno, solo inicaliza el sistema
#' @export
set_logger <- function(log.file = "logger.log"){
  flog.logger("file", INFO, appender = appender.file(log.file))
  flog.logger()
}

#' Envia mensaje al sistema de loggeo
#'
#' Una vez que el sistema de loggeo se ha inicializado, por medio de esta
#' funcion se puede imprimir mensajes de loggeo. Los mensajes apareceran tanto en el
#' archivo como en la consola.
#'
#' @param msg Mensjaje a imprimir en logs.
#' @param fun Funcion del nivel de loggeo. Por default se usa __flog.info__, pero se
#' puede usar tambien __flog.warn__, __flog.debug__, __flog.error__, __flog.fatal__, etc.
#' @return No hay retorno, solo se concatena el mensaje en el archivo de loggeo y
#' en la consola
#' @export
log_msg <- function(msg, fun = flog.info){
  tryCatch({
    fun(msg, name = "file")
    fun(msg)
  }, error = function(e) message(e))
}

#' Pivotear un producto Nielsen
#'
#' Funcion pivoteadora de un solo producto. Ejecuta un dcast internamente
#' de una columna logica correspondiente a un producto. Pivotea los valores para poder
#' determinar las cantidades manejantes, porcentajes y totales.
#'
#' @param var.mm Variable correspondiente al producto a pivotear. Puede ser multinivel
#' con alguna variable categorica.
#' @param y.vars Variables correspondiente al nivel de agregacion categorica.
#' @param value Variable que se usara para agregarse.
#' @param dt Data.table del cual se recoge la informacion.
#' @param func Funcion de agregacion.
#' @param ... Parametros extras que pueden pasarse a la funcion de agregacion.
#' @param keeps Variables a mantener del resultado. Las variables resultantes son:
#' \itemsize{
#' \item .manejante: Cantidad de manejantes.
#' \item .no_manejante: Cantidad de no manejantes.
#' \item .porcentaje: Porcentaje de manejantes.
#' \item .total: Cantidad total.
#' }
#' Por default se extraen las 4 variables.
#' @return data.table con informacion pivoteada del producto.
#' @export
pivot_product <- function(var.mm, y.vars, value, dt, func, ...,
                          keeps = "manejante|no_manejante|porcentaje|total"){
  casted <- dcast(dt,
                  produce_formula(y.vars, var.mm),
                  drop = TRUE, value.var = value,
                  fun.aggregate = func, ... = ...)
  val.p <- paste0(var.mm, c(".manejante", ".no_manejante", ".porcentaje", ".total"))
  casted[, (val.p) := set.percent(`TRUE`, `FALSE`)]
  return(casted[, c(y.vars, val.p[val.p %like% keeps]), with = F])
}


#' Calcula porcentajes logicos
#'
#' Computa el porcentaje de acuerdo a dos variables logicas.
#' @param true Vector numerico correspondiente a verdaderos.
#' @param false Vector numerico correspondiente a falsos.
#' @return Lista con resultados de cantidades verdaderas, falsas, porcentaje de
#' verdaderos y total.
#'
#' @export
set.percent <- function(true, false){
  total <- true + false
  true.p <- true/total*100
  return(list(m = true,
              nm = false,
              m.p = round(true.p, digits = 2),
              total = total))
}

#' Genera un objeto Formula
#'
#' Genera un objeto formula desde un string que tenga el sintaxis de una formula.
#'
#' @param lhs Left hand side: Vector de caracteres correspondientes al lado izquierdo
#' de la formula.
#' @param rhs Right hand side:, Vector de caracteres correspondientes al lado derecho
#' de la formula.
#'
#' @return Formula sin evaluar.
#'
#' @export
produce_formula <- function(lhs, rhs){
  as.formula(paste0(str_c(lhs, collapse = " + "),
                    " ~ ",
                    str_c(rhs, collapse = " + ")))
}



#' Pivotea productos
#'
#' Funcion de alto nivel para poder pivotear varios productos a la vez.
#' Utiliza iterativamente la funcion __pivot_product__ para generar pivotes de productos.
#' Posteriormente los une por medio de un merge para la tabla final.
#'
#' @param products Vector de caracteres correspondientes a los productos a pivotear.
#' @param keys Niveles de agregacion para pivotear los productos. Posteriormente se
#' convierten en las llaves a utilizar en la union final de los pivotes.
#' @param value.var Columna de valores a agregar.
#' @param func Funcion de agregacion a utilizar en el pivoteo.
#' @param ... Parametros extras a utilizar en la funcion de agregacion __func__.
#' @param keeps Vector de caracteres correspondiente a variables a mantener de cada
#' pivoteo de los productos. Para mas detalles, refierase a funcion __pivot_product__,
#' @param dt Data.table de referencia.
#'
#' @return Tabla completa con pivoteo de productos.
#'
#' @export
product_pivotizer <- function(products, keys, value.var, func, ...,
                              keeps = "manejante|no_manejante|porcentaje|total",
                              dt)  {
  map(products,
      safe_pivot_product, keys, value.var, dt, func, ... = ...,
      keeps = keeps) %>%
    map("result") %>% compact() %>% merge.list(keys = keys)
}

#' Formatea vectores de caracteres
#'
#' Se encarga de substituir caracteres especiales englobados en la
#' variable global special_chrs solo a variables caracter en _dt_.
#' Tambien puede cambiar el texto a mayusculas.
#'
#' @param dt data.table a aplicar funcion
#' @param format Si se convierte en mayuscula todas las columnas caracter (TRUE) o no
#' (FALSE).
#'
#' @return data.table formateado
#'
#' @export
parse_char_columns <- function(dt, format = TRUE){
  char.vars <- names(dt)[dt[, sapply(.SD, is.character)]]
  dt[, (char.vars) := map(.SD, function(x){
    res <- str_replace_all(x, special_chrs)
    if(format) res <- toupper(res)
    res
  }),
  .SDcols = char.vars]
  return(dt)
}

#' Columnas/Filas Totales
#'
#' Genera totales para columnas o para tuplas de acuerdo a agregaciones.
#'
#' @param dt data.table a aplicar la funcion
#' @param axis Si los totales son por columna ('columns') o si son por tupla ('rows')
#' @param by Columnas que correpsonden a la agregacion.
#' @param keys Columnas que corresponden a llaves de identidad.
#'
#' @return data.table con columnas o tuplas con totales.
#'
#' @export
add_totals <- function(dt, axis = "columns", by, keys){
  if(axis == "columns"){
    newDT <- rbind(dt,
                   dt[, lapply(.SD, sum, na.rm = T),
                      .SDcols = keys,
                      by = by], fill = TRUE)
    setorderv(newDT, cols = by)
    return(newDT)
  } else if(axis == "rows"){
    totals <- dt[, .(Total = rowSums(.SD, na.rm = T)), .SDcols = keys, by = by]
    return(merge(dt, totals, by = by))
  }
}

#' ¿Permitido?
#'
#' Funcion predicado que sirve para evaluar si una combinacion de
#' factor y variacion esta en el rango permitido.
#'
#' @param factor vector numerico representando el factor
#' @param var vector numerico representando la variacion porcentual (en 100)
#'
#' @return valor logico de la evaluacion
#'
#' @export
is_allowed <- function(factor, var){
  if(is.na(factor) | is.na(var)) NA
  else if(factor > 1400 & var <= 2) TRUE
  else if((749 < factor & factor <= 1400) & var <= 3) TRUE
  else if((599 < factor & factor <= 749) & var <= 5) TRUE
  else if((399 < factor & factor <= 599) & var <= 7) TRUE
  else if((199 < factor & factor <= 399) & var <= 9) TRUE
  else if((99 < factor & factor <= 199) & var <= 13) TRUE
  else if((49 < factor & factor <= 99) & var <= 20) TRUE
  else if( 49 >= factor & var <= 20) TRUE
  else if(var == 0) TRUE
  else FALSE
}

#' Define Columnas Geograficas a partir de Codigo de Sector
#'
#' Se encarga de obtener variables geograficas de acuerdo a la
#' variable COD_SETOR.
#'
#' @param codsetor Caracter correspondiente a COD_SETOR, debe contener 15 digitos.
#'
#' @return Lista con las variables ESTADO, MUNIC, COD_DIG, DISTRITO, SUBDIST, SETOR.
#' @export
codsetor2geo <- function(codsetor){
  ESTADO <- str_sub(codsetor, 1, 2) %>% as.numeric
  MUNIC <- str_sub(codsetor, 3, 6) %>% as.numeric
  COD_DIG <- str_sub(codsetor, 7, 7) %>% as.numeric
  DISTRITO <- str_sub(codsetor, 8, 9) %>% as.numeric
  SUBDIST <- str_sub(codsetor, 10, 11) %>% as.numeric
  SETOR <- str_sub(codsetor, 12, 15) %>% as.numeric
  list(ESTADO = ESTADO,
       MUNIC = MUNIC,
       COD_DIG = COD_DIG,
       DISTRITO = DISTRITO,
       SUBDIST = SUBDIST,
       SETOR = SETOR
  )
}

#' Obtener BlockId desde COD_SETOR
#'
#' Obtiene el BLOCKID desde COD_SETOR.
#' @param codsetor Caracter correspondiente a COD_SETOR
#' @return Caracter correspondiente a BLOCKID
#'
#' @export
codsetor2blockid <- function(codsetor){
  paste0(str_sub(codsetor, 1, 6), str_sub(codsetor, 8, 15))
}

#' Obtiene BlockId por medio de variables geograficas.
#'
#' Genera BLOCKID de acuerdo a las variables geograficas estado,
#' municipio, distrito, subdistrito y sector.
#' @param estado id del estado
#' @param munic id del municipio
#' @param distrito id del distrito
#' @param subdist id del subdistrito
#' @param setor id del sector
#' @return BLOCKID
#'
#' @export
geo2blockid <- function(estado, munic, distrito, subdist, setor) {
  paste0(
    str_pad(estado, 2, pad = "0"),
    str_pad(munic, 4, pad = "0"),
    str_pad(distrito, 2, pad = "0"),
    str_pad(subdist, 2, pad = "0"),
    str_pad(setor, 4, pad = "0")
  )
}

#' Obtiene COD_SETOR desde Geografia
#'
#' Genera BLOCKID de acuerdo a las variables geograficas estado,
#' municipio, distrito, subdistrito y sector.
#' @param estado id del estado
#' @param munic id del municipio
#' @param distrito id del distrito
#' @param subdist id del subdistrito
#' @param setor id del sector
#' @return BLOCKID
#'
#' @export
geo2codsetor <- function(ESTADO, MUNIC, COD_DIG, DISTRITO, SUBDIST, SETOR) {
  cod_setor <- paste0(
    str_pad(ESTADO, 2, pad = "0"),
    str_pad(MUNIC, 4, pad = "0"),
    str_pad(COD_DIG, 1, pad = "0"),
    str_pad(DISTRITO, 2, pad = "0"),
    str_pad(SUBDIST, 2, pad = "0"),
    str_pad(SETOR, 4, pad = "0")
  )
  cod_setor
}

#' Obtiene Latitud y Longitud de variables GPS
#'
#' Obtiene la Latitud y Longitud de la variable GPS y GPS2.
#' @param x vector caracter representando GPS1
#' @param y vector caracter representando GPS2
#'
#' @return lista con valores Latitud y Longitud
#' @export
gps2lat_lon <- function(x, y){
  gps1 <- str_split(x, ";", simplify = T)[1:2] %>% as.numeric
  gps2 <- str_split(y, ";", simplify = T)[1:2] %>% as.numeric
  if(!is.na(gps1[1]) & gps1[1] != 0)
    latitude <- gps1[1]
  else if(!is.na(gps2[1]) & gps2[1] != 0)
    latitude <- gps2[1]
  else latitude <- NA
  if(!is.na(gps1[2]) & gps1[2] != 0)
    longitude <- gps1[2]
  else if(!is.na(gps2[2]) & gps2[2] != 0)
    longitude <- gps2[2]
  else longitude <- NA

  list(lat = latitude, lon = longitude)
}


#' Negacion de funcion %in%
#'
#' Funcion utilitaria para utilizar una verison negada del conocido %in%
#'
#' @import purrr
#' @return Funcion wrapper
#' @export
`%notin%` <- purrr::negate(`%in%`)

#' Casteo a numerico con advertemcia
#'
#' Parsea texto a numero y detiene si hay texto no parseable
#'
#' Si el vector contiene elementos no parseables, envia una advertencia
#' con el valor de dichos elementos.
#' @param x vector a castear a numerico
#'
#' @return vector numerico casteado
warn.num <- function(x){
  tryCatch(as.numeric(x),
           warning = function(w){
             P <- as.numeric(x)
             INDEXES <- which(is.na(P))
             errors <- x[INDEXES] %>% unique() %>% paste0(collapse = ", ")
             glue("Texto no parseable: {errors}")
           })
}
