
 #########################################################################################################
 ###### FUNCTION DEFINITIONS #############################################################################
 ###### Author: Nicolas Fajardo ##########################################################################
 ###### Last update: 4th of July, 2019 ###################################################################
 #########################################################################################################

 #### Statistics #########################################################################################

  calculate_volatility <- function(x, annualized = FALSE) {
  
    sample <- x %>% na.omit() %>% length() %>% -1
    summation <- sum(x, na.rm = TRUE)
    volatility <- ifelse(annualized == FALSE, sqrt(summation/sample), sqrt(summation/sample)*sqrt(365)) 
    return(volatility)
  
  }

  calculate_IBREA <- function(x, n) {
  
    x = ((1+((x/100)*(n/360)))^(365/n)-1)*100
    return(round(x,3))
  
  }

 #### Sampling ###########################################################################################

  reduce_sample <- function(data, filter, value, variable = "RENDIMIENTO", 
                            method = "median", consolidate = TRUE, group = c(
                            "FECHA", "FECHA_VENCIMIENTO", "SECTOR", "REFERENCIA",
                            "AÑOS_AL_VENCIMIENTO", "DIAS_AL_VENCIMIENTO")) {
    if (value == "NONE") {
      
      data %<>% select(!!!syms(group), (!!sym(variable)))
      message("The following columns were kept:")
      print(paste(group, sep = ", "))
      return(data)
      
    }
    
    non_reduced_data <- dplyr::filter(data, (!!sym(filter)) != value) %>% 
      select(!!!syms(group), (!!sym(variable)))
    
    if (method == "median") {
      
      reduced_data <- dplyr::filter(data, (!!sym(filter)) == value) %>% group_by(!!!syms(group)) %>%
      summarize((!!sym(variable)) := median((!!sym(variable)), na.rm = TRUE))
      
      } else if (method == "mean") {
        
        reduced_data <- dplyr::filter(data, (!!sym(filter)) == value) %>% group_by(!!!syms(group)) %>%
          summarize((!!sym(variable)) := mean((!!sym(variable)), na.rm = TRUE))
        
        } else {
          
          stop("Wrong method specified")
          
          }
    
    if (consolidate == TRUE) {
      
      data <- bind_rows(non_reduced_data, reduced_data) %>% ungroup()
      return(data)
      
      } else if (consolidate == FALSE) {
        
        return(reduced_data)
        
        } else {
          
          stop("Non valid consolidate option")
          
        }
    }
  
  filter_by_aggregate <- function(data, aggregate, instruments, directory = "default",
                                  filename = "default") {
    
    if (directory == "default" & filename == "default") {
      
      issuers <- read_rds("C:/Users/lpena/Documents/Documentos Analista/Datos/Emisores_PiP.rds")
      
    } else if (directory != "default" & filename != "default") {
      
      issuers <- read_rds(paste0(directory,filename))
      
    } else { 
      
      stop("Both directory and filename must be specified and make sure they exist.")
      
      }
    
    if (!aggregate %in% names(issuers$Agregados)) {
      
      stop("Specified aggregate does not exist")
      
      } else {
        
        data %<>% mutate(SECTOR = if_else(EMISOR %in% issuers$Agregados$GOBIERNO_CENTRAL, "gov", "corp")) %>%
          mutate(REFERENCIA = if_else(SECTOR == "gov", MONEDA, TASA_REFERENCIA))
        
        if (missing(instruments)) {
          
          filtered_data <- data %>% dplyr::filter(
            EMISOR %in% issuers$Agregados[[aggregate]],
            SECTOR == "gov" & str_detect(data$NEMO, pattern = "^(TUVT|TFIT)") |
              SECTOR == "corp" & str_detect(data$NEMO, pattern = "^(CDT)")
            )
          
          } else {
            
            filtered_data <- data %>% dplyr::filter(EMISOR %in% issuers$Agregados[[aggregate]],
            str_detect(data$NEMO, pattern = paste0("^(", paste(instruments, collapse = "|"), ")")))
          
          }
        
        return(filtered_data)
        
      }
    }
  
  node_aggregation <- function(data, filter, values = c("FS", "IPC", "DTF", "IB1"),
                             variable = "RENDIMIENTO", method = "mean", group) {
    
    if (missing(group)) {
      
      consolidate <- reduce_sample(data, filter, value = "NULL", variable = variable, method = method, consolidate = FALSE)[0, ]
      
      for (i in values) {
        
        consolidate %<>% bind_rows(reduce_sample(data, filter, value = i, variable = variable, consolidate = FALSE))
        
        }
      
      return(consolidate)
      
      } else {
        
        consolidate <- reduce_sample(data, filter, value = "NULL", variable = variable,
                                     method = method, group = group, consolidate = FALSE)[0, ]
        
        for (i in values) {
          
          consolidate %<>% bind_rows(reduce_sample(data, filter, value = i, variable = variable, 
                                                   method = method, group = group, consolidate = FALSE))
          }
        
        return(consolidate)
        
      }
    }

 #### Data gathering, summarizing and saving #############################################################

  read_n_filter <- function(x, ...) {
    
    y <- read_csv(x) %>% filter_by_aggregate(...)
    return(y)
    
  }
  
  
  consolidate_corp <- function(directory = "default", dates = "all", 
                               filtering = TRUE, aggregate = "BANCOS_AAA") {
    
    if (directory == "default") {
      
      setwd("C:/Users/lpena/Documents/Documentos Analista/Datos/Instrumentos/Corporativo/")
      
    } else if (missing(directory)) { 
      
      stop("Directory must be specified and make sure it exists.")
      
    }
    
    corporativa <- tibble(filename = list.files(full.names = TRUE),
                          date = ymd(str_sub(list.files(full.names = TRUE), -12, -5))) %>% drop_na(date)
    
    if (all(as.character(dates) == "all")) {
      
      if (filtering == TRUE) {
        
        corporativa %<>% pull(filename) %>% lapply(read_n_filter, aggregate = aggregate) %>% bind_rows()
        
      } else {
        
        corporativa %<>% pull(filename) %>% lapply(read_csv) %>% bind_rows()
        
      }
      
    } else if (is.Date(dates)) {
      
      corporativa %<>% dplyr::filter(date %in% !!dates) %>% pull(filename) %>% lapply(read_n_filter, aggregate = aggregate) %>% bind_rows()
      
    }
    
    corporativa$FECHA %<>% ymd()
    corporativa$FECHA_EMISION %<>% ymd()
    corporativa$FECHA_VENCIMIENTO %<>% ymd()
    corporativa %<>% mutate(AÑOS_AL_VENCIMIENTO = DIAS_AL_VENCIMIENTO / 365) %>% distinct()
    attr(corporativa, "last_update") <- Sys.time()
    return(corporativa)
    
  }
  
  consolidate_gov <- function(directory = "default", dates = "all", 
                              filtering = TRUE, aggregate = "GOBIERNO") {
    
    if (directory == "default") {
      
      setwd("C:/Users/lpena/Documents/Documentos Analista/Datos/Instrumentos/Gubernamental/")
      
    } else if (missing(directory)) { 
      
      stop("Directory must be specified and make sure it exists.")
      
    }
    
    gubernamental <- tibble(filename = list.files(full.names = TRUE),
                          date = ymd(str_sub(list.files(full.names = TRUE), -12, -5))) %>% drop_na(date)
    
    if (dates == "all") {
      
      if (filtering == TRUE) {
        
        gubernamental %<>% pull(filename) %>% lapply(read_n_filter, aggregate = aggregate) %>% bind_rows()
        
      } else {
        
        gubernamental %<>% pull(filename) %>% lapply(read_csv) %>% bind_rows()
        
      }
      
    } else {
      
      gubernamental %<>% dplyr::filter(date %in% !!dates) %>% pull(filename) %>% lapply(read_n_filter, aggregate = aggregate) %>% bind_rows()
      
    }
    
    gubernamental$FECHA %<>% ymd()
    gubernamental$FECHA_EMISION %<>% ymd()
    gubernamental$FECHA_VENCIMIENTO %<>% ymd()
    gubernamental %<>% mutate(AÑOS_AL_VENCIMIENTO = DIAS_AL_VENCIMIENTO / 365) %>% distinct()
    attr(gubernamental, "last_update") <- Sys.time()
    return(gubernamental)
    
  }
  
  consolidate <- function (directory.corp = "default", directory.gov = "default", 
                           dates = "all", filtering = TRUE, reduce = FALSE, 
                           aggregate = "BANCOS_AAA", directory = "default", 
                           database = "complete", filter = "REFERENCIA") {
    
    if (directory == "default") {
      
      directory = "C:/Users/lpena/Documents/Documentos Analista/Datos/"
      
    } else if (missing(directory)) { 
      
      stop("Directory must be specified and make sure it exists.")
      
    } 
    
    if (database == "complete") {
      
      if (reduce == FALSE) {
      
      message("Consolidating corporate information")
      corporativa <- consolidate_corp(directory = directory.corp, dates = dates,
                                      filtering = filtering, aggregate = aggregate)
      
      message("Consolidating government information")
      gubernamental <- consolidate_gov(directory = directory.gov, dates = dates,
                                        filtering = filtering)
      
      message("Merging information")
      result <- list(corporativa = corporativa, gubernamental = gubernamental)
      
      write_rds(result, paste0(directory, "PiP_", aggregate, ".rds"))
      return(paste("Data saved on", directory, "Name: PiP_", aggregate, ".rds"))
      setwd(directory)
      
      } else if (reduce == TRUE) {
        
        message("Consolidating corporate information")
        corporativa <- consolidate_corp(directory = directory.corp, dates = dates,
                                        filtering = filtering, aggregate = aggregate) %>%
          node_aggregation(filter = "REFERENCIA")
        
        message("Consolidating government information")
        gubernamental <- consolidate_gov(directory = directory.gov, dates = dates,
                                         filtering = filtering)
        
        message("Merging information")
        result <- list(corporativa = corporativa, gubernamental = gubernamental)
        
        write_rds(result, paste0(directory, "PiPReduced_", aggregate, ".rds"))
        return(paste("Data saved on", directory, "Name: PiPReduced_", aggregate, ".rds"))
        setwd(directory)
        
      }
      
    } else if (database == "gov") {
      
      message("Consolidating government information")
      gubernamental <- consolidate_gov(directory = directory.gov, dates = dates,
                                         filtering = filtering)
        
      write_rds(gubernamental, paste0(directory, "PiPGubernamental.rds"))
      return(paste("Data saved on", directory, "Name: PiPGubernamental.rds"))
      setwd(directory)
        
    } else if (database == "corp") {
        
      if (reduce == FALSE) {
        
        message("Consolidating corporate information")
        corporativa <- consolidate_corp(directory = directory.corp, dates = dates,
                                        filtering = filtering, aggregate = aggregate)

        write_rds(corporativa, paste0(directory, "PiPCorp_", aggregate, ".rds"))
        return(paste("Data saved on", directory, "Name: PiPCorp_", aggregate, ".rds"))
        setwd(directory)
        
      } else if (reduce == TRUE) {
        
        message("Consolidating corporate information")
        corporativa <- consolidate_corp(directory = directory.corp, dates = dates,
                                        filtering = filtering, aggregate = aggregate) %>%
          node_aggregation(filter = "REFERENCIA")
        
        write_rds(corporativa, paste0(directory, "PiPCorpReduced_", aggregate, ".rds"))
        return(paste("Data saved on", directory, "Name: PiPCorpReduced_", aggregate, ".rds"))
        setwd(directory)
        
      }}}
  
  update <- function(directory.corp = "default", directory.gov = "default", 
                     dates = "all", filtering = TRUE, reduce = FALSE, 
                     aggregates = "BANCOS_AAA", directory = "default", 
                     database = "PiP_BANCOS_AAA", filter = "REFERENCIA") {
    
    if (database == "PiP_BANCOS_AAA") {
      message("Reading prior information")
      setwd("C:/Users/lpena/Documents/Documentos Analista/Datos")
      historic <- read_rds("PiP_BANCOS_AAA.rds")
      
      message("Updating corporate information")
      setwd("C:/Users/lpena/Documents/Documentos Analista/Datos/Instrumentos/Corporativo")
      # web_harvesting(database = "corp")
      files <- file.info(list.files(full.names = T))
      recent_corp <- read_csv(rownames(files)[which.max(files$mtime)])
      recent_corp$FECHA %<>% ymd()
      recent_corp$FECHA_EMISION %<>% ymd()
      recent_corp$FECHA_VENCIMIENTO %<>% ymd()
      recent_corp %<>% mutate(AÑOS_AL_VENCIMIENTO = DIAS_AL_VENCIMIENTO / 365)
      historic_corp <- historic$corporativa
      corporativa <- bind_rows(historic_corp, recent_corp) %>% distinct()
      attr(corporativa, "last_update") <- Sys.time()
      setwd("..")
      
      message("Updating government information")
      setwd("C:/Users/lpena/Documents/Documentos Analista/Datos/Instrumentos/Gubernamental")
      # web_harvesting(database = "gov")
      files <- file.info(list.files(full.names = T))
      recent_gov <- read_csv(rownames(files)[which.max(files$mtime)])
      recent_gov$FECHA %<>% ymd()
      recent_gov$FECHA_EMISION %<>% ymd()
      recent_gov$FECHA_VENCIMIENTO %<>% ymd()
      recent_gov %<>% mutate(AÑOS_AL_VENCIMIENTO = DIAS_AL_VENCIMIENTO / 365)
      historic_gov <- historic$gubernamental
      gubernamental <- bind_rows(historic_gov, recent_gov) %>% distinct()
      attr(gubernamental, "last_update") <- Sys.time()
      setwd("C:/Users/lpena/Documents/Documentos Analista/Datos")
      
      message("Merging updated information")
      result <- list(corporativa = corporativa, gubernamental = gubernamental)
      write_rds(result, "PiP.rds")
      return(paste("Data saved on", getwd(), "Name: PiP.rds"))
    } else if (database == "corp") {
      stop("Functionality not implemented yet")
    }
    else if (database == "gov") {
      stop("Functionality not implemented yet")
    } else {
      stop("Not valid database")
    }
  }
  
  
  

# consolidate_banrep <- function() {
#   setwd("C:/Users/NFAJARDO/Documents/Datos/Banrep/")
# 
#   ## MERCADO MONETARIO
# 
#   tip <- read_csv("tip.csv", col_types = c(.default = "c")) %>%
#     rename(FECHA = `Fecha (dd/mm/aaaa)`, TIP = `Tasa de intervención de política monetaria`) %>%
#     mutate(
#       FECHA = ymd(FECHA), VARIABLE = "TIP",
#       VALOR = as.numeric(str_replace(str_replace_all(TIP, "[^0-9\\,\\.]", ""), ",", ".")) / 100
#     ) %>%
#     select(-TIP) %>%
#     distinct()
#   tib <- read_csv("tib.csv", col_types = c(.default = "c")) %>%
#     rename(FECHA = `Fecha`) %>%
#     mutate(
#       FECHA = ymd(FECHA), VARIABLE = "TIB",
#       VALOR = as.numeric(str_replace(str_replace_all(TIB, "[^0-9\\,\\.]", ""), ",", ".")) / 100
#     ) %>%
#     select(-TIB) %>%
#     distinct()
#   ib1 <- read_csv("ib1.csv", col_types = c(.default = "c")) %>%
#     rename(FECHA = `fecha`, VALOR = valor_tasa, PLAZO = plazo, AGENTE = agente, `DESCRIPCION TASA` = tipo_tasa) %>%
#     mutate(
#       FECHA = dmy(FECHA), PLAZO = ifelse(PLAZO == "plazo overnight", 1, NA), AGENTE = as_factor(AGENTE),
#       VALOR = as.numeric(str_replace(str_replace_all(VALOR, "[^0-9\\,\\.]", ""), ",", ".")) / 100
#     )
#   ib1_ea <- dplyr::filter(ib1, `DESCRIPCION TASA` == "Tasa efectiva") %>%
#     mutate(VARIABLE = "IB1") %>%
#     select(FECHA, VALOR, VARIABLE) %>%
#     distinct()
#   ib30 <- read_csv("ib30.csv", col_types = c(.default = "c")) %>%
#     rename(FECHA = `fecha`, VALOR = valor_tasa, PLAZO = plazo, AGENTE = agente, `DESCRIPCION TASA` = tipo_tasa) %>%
#     mutate(
#       FECHA = dmy(FECHA), PLAZO = ifelse(PLAZO == "plazo un mes", 30, NA), AGENTE = as_factor(AGENTE),
#       VALOR = as.numeric(str_replace(str_replace_all(VALOR, "[^0-9\\,\\.]", ""), ",", ".")) / 100
#     )
#   ib30_ea <- dplyr::filter(ib30, `DESCRIPCION TASA` == "Tasa efectiva") %>%
#     mutate(VARIABLE = "IB30") %>%
#     select(FECHA, VALOR, VARIABLE) %>%
#     distinct()
#   ib90 <- read_csv("ib90.csv", col_types = c(.default = "c")) %>%
#     rename(FECHA = `fecha`, VALOR = valor_tasa, PLAZO = plazo, AGENTE = agente, `DESCRIPCION TASA` = tipo_tasa) %>%
#     mutate(
#       FECHA = dmy(FECHA), PLAZO = ifelse(PLAZO == "plazo tres meses", 30, NA), AGENTE = as_factor(AGENTE),
#       VALOR = as.numeric(str_replace(str_replace_all(VALOR, "[^0-9\\,\\.]", ""), ",", ".")) / 100
#     )
#   ib90_ea <- dplyr::filter(ib90, `DESCRIPCION TASA` == "Tasa efectiva") %>%
#     mutate(VARIABLE = "IB90") %>%
#     select(FECHA, VALOR, VARIABLE) %>%
#     distinct()
#   ib180 <- read_csv("ib180.csv", col_types = c(.default = "c")) %>%
#     rename(FECHA = `fecha`, VALOR = valor_tasa, PLAZO = plazo, AGENTE = agente, `DESCRIPCION TASA` = tipo_tasa) %>%
#     mutate(
#       FECHA = dmy(FECHA), PLAZO = ifelse(PLAZO == "plazo seis meses", 30, NA), AGENTE = as_factor(AGENTE),
#       VALOR = as.numeric(str_replace(str_replace_all(VALOR, "[^0-9\\,\\.]", ""), ",", ".")) / 100
#     )
#   ib180_ea <- dplyr::filter(ib180, `DESCRIPCION TASA` == "Tasa efectiva") %>%
#     mutate(VARIABLE = "IB180") %>%
#     select(FECHA, VALOR, VARIABLE) %>%
#     distinct()
#   dtf <- read_delim("dtf.csv", ";", col_types = c(.default = "c")) %>%
#     mutate(FECHA = dmy(fecha_inicio), VALOR = as.numeric(str_replace(str_replace_all(valor_tasa, "[^0-9\\,\\.]", ""), ",", ".")) / 100, VARIABLE = tasa_interes) %>%
#     select(-c("fecha_final", "fecha_inicio", "tasa_interes", "valor_tasa")) %>%
#     dcast(FECHA ~ VARIABLE, value.var = "VALOR") %>%
#     right_join(tibble(FECHA = seq.Date(as.Date("1984-01-12"), today(), by = "day"))) %>%
#     na.locf() %>%
#     melt(id = "FECHA", value.name = "VALOR", variable.name = "VARIABLE") %>%
#     mutate(VARIABLE = as.character(VARIABLE)) %>%
#     arrange(FECHA)
#   uvr <- read_delim("uvr.csv", ";", col_types = c(.default = "c")) %>%
#     rename(FECHA = `fecha`, VALOR = `valorUVR`) %>%
#     select(VALOR, FECHA) %>%
#     mutate(FECHA = dmy(FECHA), VALOR = as.numeric(str_replace_all(VALOR, "[\\,]", ".")), VARIABLE = "UVR")
# 
#   m.monetario <- bind_rows(tip, tib, ib1_ea, ib30_ea, ib90_ea, ib180_ea, dtf, uvr) %>% arrange(FECHA)
# 
#   ## OPERACIONES DE MERCADO ABIERTO
# 
#   contraccion <- read_csv("contraccion.csv",
#     col_types = c(.default = "c"), skip = 1,
#     col_names = c(
#       "FECHA", rep("0", 5), "CODIGO", "0", "ID", "0", "0", "GRUPO", "0",
#       "FECHA_VENCIMIENTO", "MONTO_APROBADO", "0", "MONTO_DEMANDADO", "CUPO",
#       "TASA", "PLAZO", "VUELTA"
#     )
#   )[-c(2:6, 8, 10:11, 13, 16)]
# 
#   contraccion %<>% mutate(
#     FECHA = ymd(FECHA), `FECHA_VENCIMIENTO` = ymd(`FECHA_VENCIMIENTO`),
#     `MONTO_APROBADO` = as.numeric(`MONTO_APROBADO`), `MONTO_DEMANDADO` = as.numeric(`MONTO_DEMANDADO`),
#     CUPO = as.numeric(CUPO), PLAZO = as.numeric(PLAZO), VUELTA = as.numeric(VUELTA),
#     TASA = as.numeric(str_replace(str_replace_all(TASA, "[^0-9\\,\\.]", ""), ",", ".")) / 100,
#     MECANISMO = if_else(CODIGO %in% c("180", "30"), "Subasta", "Ventanilla"),
#     TIPO = if_else(CODIGO %in% c("180", "190"), "Repo", "Deposito"), OPERACION = "Contraccion"
#   )
# 
#   expansion <- read_csv("expansion.csv",
#     col_types = c(.default = "c"), skip = 1,
#     col_names = c(
#       "FECHA", rep("0", 5), "CODIGO", "0", "ID", "0", "0", "GRUPO", "0",
#       "FECHA_VENCIMIENTO", "MONTO_APROBADO", "0", "MONTO_DEMANDADO", "CUPO",
#       "TASA", "PLAZO", "VUELTA"
#     )
#   )[-c(2:6, 8, 10:11, 13, 16)]
# 
#   expansion %<>% mutate(
#     FECHA = ymd(FECHA), `FECHA_VENCIMIENTO` = ymd(`FECHA_VENCIMIENTO`),
#     `MONTO_APROBADO` = as.numeric(`MONTO_APROBADO`), `MONTO_DEMANDADO` = as.numeric(`MONTO_DEMANDADO`),
#     CUPO = as.numeric(CUPO), PLAZO = as.numeric(PLAZO), VUELTA = as.numeric(VUELTA),
#     TASA = as.numeric(str_replace(str_replace_all(TASA, "[^0-9\\,\\.]", ""), ",", ".")) / 100,
#     MECANISMO = if_else(CODIGO %in% "10", "Subasta", "Ventanilla"),
#     TIPO = if_else(CODIGO %in% c("10", "20"), "Repo", "Deposito"), OPERACION = "Expansion"
#   )
# 
#   omas <- bind_rows(expansion, contraccion) %>%
#     mutate(ID = as.factor(ID), GRUPO = as_factor(GRUPO)) %>%
#     distinct()
# 
#   ## CONSOLIDADO
# 
#   attr(m.monetario, "last_update") <- Sys.time()
#   attr(omas, "last_update") <- Sys.time()
# 
#   return <- list("mercado_monetario" = m.monetario, "omas" = omas)
#   attr(return, "last_update") <- Sys.time()
# 
#   setwd("C:/Users/NFAJARDO/Documents/Datos")
#   return(return)
# }

  consolidate_returns <- function() {
    
    setwd("C:/Users/lpena/Documents/Documentos Analista/Datos/FICS/Rentabilidad")
  
    rentabilidad <- list.files(full.names = TRUE) %>%
    lapply(read_excel) %>%
    bind_rows()
    
    names(rentabilidad) <- c(
    "FECHA", "TIPO_ENTIDAD", "CODIGO_ENTIDAD", "NOMBRE_ENTIDAD", "CODIGO_NEGOCIO",
    "NOMBRE_NEGOCIO", "SUBTIPO_NEGOCIO", "PRINCIPAL", "TIPO_PARTICIPACION", "ID_PARTICIPACION",
    "NUMERO_UNIDAD", "VALOR_UNIDAD", "VALOR_FONDO", "NUMERO_INVERSIONISTAS",
    "RENTABILIDAD_DIA", "RENTABILIDAD_MES", "RENTABILIDAD_SEMANA", "RENTABILIDAD_AÑO"
  )
  rentabilidad$NUMERO_UNIDAD %<>% str_replace_all("[\\,]", "") %>% as.numeric()
  rentabilidad$VALOR_UNIDAD %<>% str_replace_all("[\\,\\$]", "") %>% as.numeric()
  rentabilidad$VALOR_FONDO %<>% str_replace_all("[\\(]", "-") %>%
    str_replace_all("[\\,\\$\\)]", "") %>%
    as.numeric()
  rentabilidad$RENTABILIDAD_DIA %<>% as.numeric()
  rentabilidad$RENTABILIDAD_MES %<>% as.numeric()
  rentabilidad$RENTABILIDAD_SEMANA %<>% as.numeric()
  rentabilidad$RENTABILIDAD_AÑO %<>% as.numeric()
  rentabilidad$FECHA %<>% dmy()
  rentabilidad %<>% mutate(PARTICIPACION = paste0(TIPO_PARTICIPACION, ID_PARTICIPACION))
  attr(rentabilidad, "last_update") <- Sys.time()
  setwd("..")
  return(rentabilidad)
}

### DATA ANALYSIS

find_peaks <- function(x, m = 3) {
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i) {
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if (all(x[c(z:i, (i + 2):w)] <= x[i + 1])) {
      return(i + 1)
    } else {
      return(numeric(0))
    }
  })
  pks <- unlist(pks)
  pks
}

minmax_filter <- function(data, size = 3, m = 15) {
  mins.pos <- find_peaks(-data$MINIMO, m)
  mins.values <- tail(data$MAXIMO[mins.pos], size)
  maxs.pos <- find_peaks(data$MAXIMO, m)
  maxs.values <- tail(data$MAXIMO[maxs.pos], size)

  date <- min(
    data[data$MAXIMO %in% maxs.values, ] %>% select(FECHA) %>% pull(),
    data[data$MINIMO %in% mins.values, ] %>% select(FECHA) %>% pull()
  )

  data %<>% dplyr::filter(FECHA >= date)

  attr(data, "first_obs") <- if_else(data$MAXIMO[data$FECHA == date] > data$MINIMO[data$FECHA == date], "max", "min")

  return(data)
}

channel_select <- function(data, m = 20, manual = FALSE, trend = NULL, ...) {
  if (manual == FALSE) {
    if (attr(data, "first_obs") == "max") {
      first.max <- data$MAXIMO[1] %>% head(1)
      first.max.date <- data$FECHA[data$MAXIMO == first.max] %>% head(1)
      last.max <- data$MAXIMO[tail(find_peaks(data$MAXIMO, m), 1)] %>% tail(1)
      last.max.date <- data$FECHA[data$MAXIMO == last.max] %>% tail(1)

      first.min <- data$MINIMO[head(find_peaks(-data$MINIMO, m), 1)] %>% head(1)
      first.min.date <- data$FECHA[data$MINIMO == first.min] %>% head(1)
      last.min <- data$MINIMO[tail(find_peaks(-data$MINIMO, m), 1)] %>% tail(1)
      last.min.date <- data$FECHA[data$MINIMO == last.min] %>% tail(1)

      upper.slope <- (last.max - first.max) / as.integer(last.max.date - first.max.date)
      upper.intrcept <- first.max
      lower.slope <- (last.min - first.min) / as.integer(last.min.date - first.min.date)
      lower.intrcept <- first.min - lower.slope * as.integer(first.min.date - min(data$FECHA))

      first.upper.point <- upper.intrcept + upper.slope * as.integer(first.max.date - min(data$FECHA))
      last.upper.point <- upper.intrcept + upper.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      first.lower.point <- lower.intrcept
      last.lower.point <- lower.intrcept + lower.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      channel <- tibble(
        FECHA = c(min(data$FECHA), max(data$FECHA)),
        MAX = c(first.upper.point, last.upper.point),
        MIN = c(first.lower.point, last.lower.point)
      )
    } else if (attr(data, "first_obs") == "min") {
      first.max <- data$MAXIMO[head(find_peaks(-data$MAXIMO, m), 1)] %>% head(1)
      first.max.date <- data$FECHA[data$MAXIMO == first.max] %>% head(1)
      last.max <- data$MAXIMO[tail(find_peaks(data$MAXIMO, m), 1)] %>% tail(1)
      last.max.date <- data$FECHA[data$MAXIMO == last.max] %>% tail(1)

      first.min <- data$MINIMO[1] %>% head(1)
      first.min.date <- data$FECHA[data$MINIMO == first.min] %>% head(1)
      last.min <- data$MINIMO[tail(find_peaks(-data$MINIMO, m), 1)] %>% tail(1)
      last.min.date <- data$FECHA[data$MINIMO == last.min] %>% tail(1)

      upper.slope <- (last.max - first.max) / as.integer(last.max.date - first.max.date)
      upper.intrcept <- first.max - upper.slope * as.integer(first.max.date - min(data$FECHA))
      lower.slope <- (last.min - first.min) / as.integer(last.min.date - first.min.date)
      lower.intrcept <- first.min

      first.upper.point <- upper.intrcept
      last.upper.point <- upper.intrcept + upper.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      first.lower.point <- lower.intrcept + lower.slope * as.integer(first.min.date - min(data$FECHA))
      last.lower.point <- lower.intrcept + lower.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      channel <- tibble(
        FECHA = c(min(data$FECHA), max(data$FECHA)),
        MAX = c(first.upper.point, last.upper.point),
        MIN = c(first.lower.point, last.lower.point)
      )
    }
  } else if (manual == TRUE) {
    
    if (trend == "up") {
      
      first.min.date <- as.Date(first.min.date)
      first.min <- data$MINIMO[data$FECHA == first.min.date] %>% head(1)
      last.min.date <- as.Date(last.min.date)
      last.min <- data$MINIMO[data$FECHA == last.min.date] %>% tail(1)
      
      first.max.date <- as.Date(first.max.date)
      first.max <- data$MAXIMO[data$FECHA == first.max.date] %>% head(1)
      last.max.date <- as.Date(last.max.date)
      last.max <- data$MAXIMO[data$FECHA == last.max.date] %>% tail(1)

      lower.slope <- (last.min - first.min) / as.integer(last.min.date - first.min.date)
      lower.intrcept <- first.min - lower.slope * as.integer(first.min.date - min(data$FECHA))
      upper.slope <- lower.slope
      upper.intrcept <- -(upper.slope * (as.integer(first.max.date - min(data$FECHA))) - first.max)
      
      first.upper.point <- upper.intrcept + upper.slope #* as.integer(first.max.date - min(data$FECHA))
      last.upper.point <- upper.intrcept + upper.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      first.lower.point <- lower.intrcept
      last.lower.point <- lower.intrcept + lower.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      channel <- tibble(
        FECHA = c(min(data$FECHA), max(data$FECHA)),
        MAX = c(first.upper.point, last.upper.point),
        MIN = c(first.lower.point, last.lower.point)
      )
      
      attr(channel, "upper.slope") <- upper.slope
      attr(channel, "lower.slope") <- lower.slope
      
    } else if (trend == "down") {
      first.min.date <- as.Date(first.min.date)
      first.min <- data$MINIMO[data$FECHA == first.min.date] %>% head(1)
      last.min.date <- as.Date(last.min.date)
      last.min <- data$MINIMO[data$FECHA == last.min.date] %>% tail(1)

      first.max.date <- as.Date(first.max.date)
      first.max <- data$MAXIMO[data$FECHA == first.max.date] %>% head(1)
      last.max.date <- as.Date(last.max.date)
      last.max <- data$MAXIMO[data$FECHA == last.max.date] %>% tail(1)
      
      upper.slope <- (last.max - first.max) / as.integer(last.max.date - first.max.date)
      upper.intrcept <- first.max - upper.slope * as.integer(first.max.date - min(data$FECHA))
      lower.slope <- upper.slope
      lower.intrcept <- -(lower.slope * (as.integer(first.min.date - min(data$FECHA))) - first.min)

      first.upper.point <- upper.intrcept
      last.upper.point <- upper.intrcept + upper.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      first.lower.point <- lower.intrcept + lower.slope * as.integer(first.min.date - min(data$FECHA))
      last.lower.point <- lower.intrcept + lower.slope * as.integer(max(data$FECHA) - min(data$FECHA))

      channel <- tibble(
        FECHA = c(min(data$FECHA), max(data$FECHA)),
        MAX = c(first.upper.point, last.upper.point),
        MIN = c(first.lower.point, last.lower.point)
      )
      
      attr(channel, "upper.slope") <- upper.slope
      attr(channel, "lower.slope") <- lower.slope
      
    } else {
      stop("Wrong object loaded: Object must come out of minmax_filter()")
    }
    return(channel)
  }
}

channel_zoom <- function(data, channel) {
  
  lower.slope = attributes(channel)$lower.slope
  upper.slope = attributes(channel)$upper.slope
  
  last.max <- channel$MAX[channel$FECHA == max(channel$FECHA)]
  last.min <- channel$MIN[channel$FECHA == max(channel$FECHA)]
  run <- as.integer(max(data$FECHA)-min(data$FECHA))
  
  upper.intrcept <- last.max - upper.slope * run
  lower.intrcept <- last.min - lower.slope * run
  
  zoomed_channel <- tibble(
    FECHA = c(min(data$FECHA), max(data$FECHA)),
    MAX = c(upper.intrcept, last.max),
    MIN = c(lower.intrcept, last.min)
  )
  return(zoomed_channel)
}

## Nelson-Siegel

  NS_parameters <- function(data, days) {
  
    aggregate <- tibble(
      beta0 = as.numeric(NULL),
      beta1 = as.numeric(NULL),
      beta2 = as.numeric(NULL),
      tau1 = as.numeric(NULL),
      FECHA = as_date(as.integer()),
      REFERENCIA = as.character(NULL)
    )

    for (i in days) {
    
      message(paste0("Calculating lag for: ", as_date(i)))
      grouped_data <- data %>% group_by(FECHA) %>% dplyr::filter(FECHA == as_date(i))

      if (dim(grouped_data)[1] == 0) {
        
        print("Filtering procedure retrieved no data")
        
      } else {
        
        parameters <- enframe(NelsonSiegel(grouped_data$RENDIMIENTO, grouped_data$AÑOS_AL_VENCIMIENTO, doplot = FALSE)[[1]]) %>%
          spread(name, value) %>% mutate(FECHA = as_date(i), REFERENCIA = paste(unique(grouped_data$REFERENCIA), collapse = " "))
        aggregate <- bind_rows(aggregate, parameters)
      }
    }

    return(aggregate)
  }

  NS_rates <- function(theta, parameters) {
    beta0 <- as.matrix(pull(parameters, 1), col = 1)
    beta1 <- as.matrix(pull(parameters, 2), col = 1)
    beta2 <- as.matrix(pull(parameters, 3), col = 1)
    tau <- as.matrix(pull(parameters, 4), col = 1)
    FECHA <- pull(parameters, 5)
    REFERENCIA <- pull(parameters, 6)

    rates <- tibble(
      MADURACION = numeric(),
      RENDIMIENTO = numeric(),
      FECHA = as_date(as.integer())
    )
    
    for (i in 1:dim(parameters)[1]) {
      
      interest.rates <- c()

      for (j in 1:length(theta)) {
        zero.coupon <- beta0[i] + beta1[i] * (1 - exp(-theta[j] / tau[i])) / (theta[j] / tau[i]) + beta2[i] * ((1 - exp(-theta[j] / tau[i])) / (theta[j] / tau[i]) - exp(-theta[j] / tau[i]))
        interest.rates <- c(interest.rates, zero.coupon)
      }

      row <- tibble(MADURACION = theta, RENDIMIENTO = interest.rates, FECHA = unique(FECHA), REFERENCIA = unique(REFERENCIA))
      rates <- bind_rows(rates, row)
    }

    return(rates)
  }

  calculate_NS <- function(data, theta, days = c(today() - rezago), instruments) {
    
    if (missing(instruments)) {
      DTF <- dplyr::filter(data, REFERENCIA == "DTF")
      IB1 <- dplyr::filter(data, REFERENCIA == "IB1")
      IPC <- dplyr::filter(data, REFERENCIA == "IPC")
      FS <- dplyr::filter(data, REFERENCIA == "FS")
      COP <- dplyr::filter(data, REFERENCIA == "COP")
      UVR <- dplyr::filter(data, REFERENCIA == "UVR")

      for (i in c("FS", "IPC", "IB1", "DTF", "COP", "UVR")) {
      
        if (dim(eval(parse(text = i)))[1] == 0) {
        
          message(paste("No data for", i, "found in database"))
        
        } else {
        
          for (j in days) {
          
            message(paste("Calculating Nelson-Siegel parameters for:", i))
            assign(paste("parameters", i, j, sep = "."), NS_parameters(eval(parse(text = i)), j))
            message(paste("Calculating Nelson-Siegel curve for:", i))
            assign(paste("rates", i, j, sep = "."), NS_rates(theta, eval(parse(text = paste("parameters", i, j, sep = ".")))))
          
          }
        }
      }

      rates <- mget(ls(pattern = "rates")) %>% bind_rows()
      parameters <- mget(ls(pattern = "parameters")) %>% bind_rows()
    
    } else {
    
      for (k in instruments) {
      
        assign(k, dplyr::filter(data, REFERENCIA == (!!sym(k))))
      
      }

      for (i in instruments) {
      
        if (dim(eval(parse(text = i)))[1] == 0) {
        
          message(paste("No data for", i, "found in database"))
        
        } else {
        
          message(paste("Calculating Nelson-Siegel parameters for:", i))
          assign(paste("parameters", i, sep = "."), NS_parameters(eval(parse(text = i)), days))
          message(paste("Calculating Nelson-Siegel curve for:", i))
          assign(paste("rates", i, sep = "."), NS_rates(theta, eval(parse(text = paste("parameters", i, sep = ".")))))
        
        }
      }

      rates <- mget(ls(pattern = "rates")) %>% bind_rows()
      parameters <- mget(ls(pattern = "parameters")) %>% bind_rows()
    
    }

    output <- list(rates=rates, parameters=parameters)
    return(output)

  }

## Nelson-Siegel Database

  save_NS <- function(data, theta, days, instruments, directory = "C:/Users/lpena/Documents/Documentos Analista/Datos") {

    tryCatch(
      
      {assign("rates", read_rds(paste0(directory,"/NelsonSiegel.rds")))},
      error = function(e) {print("Saved rates cannot be loaded")}
      
      )
    
    tryCatch(
      
      {assign("parameters", read_rds(paste0(directory,"/NelsonSiegelParameters.rds")))},
      error = function(e) {print("Saved parameters cannot be loaded")}
      
    )

    if (missing(days) & missing(instruments)) {
      
      output <- calculate_NS(data, theta)
      
    } else if (missing(instruments)) {
      
      output <- calculate_NS(data, theta, days)#estoy utilizando este para dayly_update
      
    } else if (missing(days)) {
      
      output <- calculate_NS(data, theta, instruments)
      
    } else {
      
      output <- calculate_NS(data, theta, days, instruments)
      
    }

    if (exists("rates")) {
      
      message("Merging rates with updated information")
      rates2save <- bind_rows(rates, output$rates) %>% distinct()
      
    } else {
      
      message("No merging possible, saving new rates database")
      rates2save <- output$rates
      
    }
    
    if (exists("parameters")) {
      
      message("Merging parameters with updated information")
      parameters2save <- bind_rows(parameters, output$parameters) %>% distinct()
      
    } else {
      
      message("No merging possible, saving new database")
      parameters2save <- output$parameters
      
    }

    message("Saving rates information as NelsonSiegel.rds")
    write_rds(rates2save, paste0(directory,"/NelsonSiegel.rds"))
    message("Saving parameters information as NelsonSiegelParameters.rds")
    write_rds(parameters2save, paste0(directory,"/NelsonSiegelParameters.rds"))
    
    return(paste("WARNING: Data was saved on", directory))
    
  }

check_NS <- function(days, theta, instruments) {
  setwd("C:/Users/lpena/Documents/Documentos Analista/Datos")
  tryCatch({
    assign("data", read_rds("NelsonSiegel.rds"))
  },
  error = function(e) {
    print("Database file cannot be loaded")
  }
  )

  missing_values <- tibble(
    FECHA = as_date(as.integer()),
    REFERENCIA = as.character(),
    MADURACION = as.character()
  )

  for (d in days) {
    length_days <- dim(dplyr::filter(data, FECHA == as_date(d)))[1]

    if (length_days == 0) {
      missing_values %<>% add_row(FECHA = as_date(d), REFERENCIA = NA, MADURACION = NA)
      next
    }

    for (i in instruments) {
      length_instruments <- dim(dplyr::filter(data, FECHA == as_date(d) & REFERENCIA == i))[1]

      if (length_instruments == 0) {
        missing_values %<>% add_row(FECHA = as_date(d), REFERENCIA = i, MADURACION = NA)
        next
      }

      for (m in theta) {
        length_maturity <- dim(dplyr::filter(data, FECHA == as_date(d) & REFERENCIA == i & MADURACION == m))[1]

        if (length_maturity == 0) {
          missing_values %<>% add_row(FECHA = as_date(d), REFERENCIA = i, MADURACION = m)
        }
      }
    }
  }

  missing_days <- missing_values %>%
    group_by(FECHA) %>%
    count()
  missing_instruments <- missing_values %>%
    group_by(REFERENCIA) %>%
    count()
  missing_maturity <- missing_values %>%
    group_by(MADURACION) %>%
    count()

  missing <- list(
    CONSOLIDADO = missing_values,
    FECHA = missing_days,
    REFERENCIA = missing_instruments,
    MADURACION = missing_maturity
  )

  print(paste("Consistency test encountered", dim(missing_days)[1] * dim(missing_instruments)[1] * dim(missing_maturity)[1], "missing value(s)"))
  return(missing)
}

extract_name <- function(data, type = "entidad") {
  if (type == "entidad") {
    data %<>% str_replace_all(".S\\...\\.", "")
    data %<>% str_replace_all(".S\\...", "")
    #data %<>% str_replace_all("COLOMBIA", "")
  } else if (type == "negocio") {
    data %<>% str_replace_all("FONDO DE INVERSI\\XN COLECTIVA", "")
    data %<>% str_replace_all("CARTERA COLECTIVA ABIERTA", "")
    data %<>% str_replace_all("SIN PACTO DE PERMANENCIA", "")
    data %<>% str_replace_all("DEL MERCADO MONETARIO", "")
    data %<>% str_replace_all("FONDO DE INVERSI\\XN.", "")
    data %<>% str_replace_all("CON PARTICIPACIONES DIFERENCIALES", "")
    data %<>% str_replace_all("ABIERTO.", "")
    data %<>% str_replace_all("[:punct:]|COLOMBIA", "")
    data %<>% str_replace_all("M\\XNIMA|BBVA|BTG PACTUAL|Old Mutual Fondo de Inversión Colectiva |ULTRASERFINCO|RENTA 4 GLOBAL| FIDUCIARIA CENTRAL", "")
    data %<>% str_replace_all("RENTA ESTABLE DE LA FAMILIA DE FONDOS DE FIDUAGRARIA SA", "RENTA ESTABLE")
    data %<>% str_replace_all("FIC RENTA AGRARIA 1525", "RENTA AGRARIA")
  }
}

### Imaging functions

  extended_range_breaks_0 <- function(n = 5, ...)  {
    function(x) {
      extended_range_breaks_(min(x), max(x), n, ...)[-1]
    }
  }

### Read portfolios
  
  # read_portfolios <- function() {
  #   
  #   years <- 2016:year(today())
  #   
  #   for (i in years) {
  #   
  #   names <- list.files(path = paste0("Q:/", i, "/BITACORAS/07. Valoraciones/"))
  #   
  #     for (j in names) {
  #   
  #       assign(paste(j,i, sep = "."), list.files(path = paste0("Q:/", i, "/BITACORAS/07. Valoraciones/", j, "/VALORACION"), recursive = TRUE, full.names = TRUE))
  #         
  #         read.xls(xls = CONFIRENTA.2016[1])
  #         
  #         read_excel(path = CONFIRENTA.2016[1], sheet = "CR")
  #         
  #         read_excel(path = "CR 010116.xls", sheet = "CR")
  #         
  #         
  #         lapply(read_excel)
  #   
  #       
  #         }
  #   }
  #   
  #   
  #   
  # }
  # 
  
### Yield calculation

  calculate_HPR <- function(data, price.forecast, macro.forecast, start.date = today(), end.date, annualized = TRUE) {
    
    purchase <- data %>% dplyr::filter(FECHA == start.date) %>% select(CODIGO_ISIN, RENDIMIENTO, PRECIO_SUCIO)
    selling <- purchase %>% rename(RENDIMIENTO_VENTA = RENDIMIENTO, PRECIO_VENTA = PRECIO_SUCIO)
    
    data <- Gubernamental %>% mutate(FRECUENCIA_PAGO_CUPON = as.numeric(FRECUENCIA_PAGO_CUPON))
    
    start.date <- as.Date("2019-07-22")
    
    forecast <- selling %>% mutate(FECHA_VENTA = as.Date("2020-07-31"))
    
    data %<>% dplyr::filter(FECHA == start.date) %>% left_join(forecast) %>%
      mutate(CUPONES_REDIMIDOS = case_when((FECHA_VENTA - FECHA < 365/FRECUENCIA_PAGO_CUPON) ~ 0,
                                           (FECHA_VENTA - FECHA > 365/FRECUENCIA_PAGO_CUPON) & (format(FECHA_VENTA, format="%m-%d") <= format(FECHA_VENCIMIENTO, format="%m-%d")) ~ (floor(as.numeric(FECHA_VENTA - FECHA)/365)*FRECUENCIA_PAGO_CUPON)-1,
                                           (FECHA_VENTA - FECHA > 365/FRECUENCIA_PAGO_CUPON) & (format(FECHA_VENTA, format="%m-%d") >  format(FECHA_VENCIMIENTO, format="%m-%d")) ~ (floor(as.numeric(FECHA_VENTA - FECHA)/365)*FRECUENCIA_PAGO_CUPON)),
             VALOR_TASA = 0,
             VALOR_CUPON = case_when(CUPONES_REDIMIDOS <= 0 ~ 0,
                                     TASA_REFERENCIA == "FS" ~ CUPON,
                                     TASA_REFERENCIA == "IPC" ~ ((1+SPREAD)*(1+VALOR_TASA))-1,
                                     TASA_REFERENCIA == "IB1" ~ ((1+((VALOR_TASA+SPREAD)/12))^12-1),
                                     TASA_REFERENCIA == "DTF" ~ (((SPREAD+VALOR_TASA)/4)/((1-((SPREAD+VALOR_TASA)/4)))+1)^(4/1)-1),
             FLUJO = (VALOR_CUPON + PRECIO_VENTA),
             TIR_DE_TENENCIA = ((FLUJO - PRECIO_SUCIO)/PRECIO_SUCIO)*100)
    
    data %<>% dplyr::filter(FECHA == start.date) %>% left_join(forecast) %>%
      mutate(CUPONES_REDIMIDOS = case_when((FECHA_VENTA - FECHA < 365/FRECUENCIA_PAGO_CUPON) ~ 0,
                                           (FECHA_VENTA - FECHA > 365/FRECUENCIA_PAGO_CUPON) & (format(FECHA_VENTA, format="%m-%d") <= format(FECHA_VENCIMIENTO, format="%m-%d")) ~ (floor(as.numeric(FECHA_VENTA - FECHA)/365)*FRECUENCIA_PAGO_CUPON)-1,
                                           (FECHA_VENTA - FECHA > 365/FRECUENCIA_PAGO_CUPON) & (format(FECHA_VENTA, format="%m-%d") >  format(FECHA_VENCIMIENTO, format="%m-%d")) ~ (floor(as.numeric(FECHA_VENTA - FECHA)/365)*FRECUENCIA_PAGO_CUPON)),
             VALOR_CUPON = CUPONES_REDIMIDOS*CUPON*(1+RENDIMIENTO_VENTA/100)^CUPONES_REDIMIDOS,
             FLUJO = (VALOR_CUPON + PRECIO_VENTA)/(1+RENDIMIENTO/100),
             TIR_DE_TENENCIA = ((FLUJO - PRECIO_SUCIO)/PRECIO_SUCIO)*100)
    
    data %<>% dplyr::filter(FECHA == start.date) %>% left_join(forecast) %>%
      mutate(CUPONES_REDIMIDOS = case_when((FECHA_VENTA - FECHA < 365/FRECUENCIA_PAGO_CUPON) ~ 0,
                                           (FECHA_VENTA - FECHA > 365/FRECUENCIA_PAGO_CUPON) & (format(FECHA_VENTA, format="%m-%d") <= format(FECHA_VENCIMIENTO, format="%m-%d")) ~ (floor(as.numeric(FECHA_VENTA - FECHA)/365)*FRECUENCIA_PAGO_CUPON)-1,
                                           (FECHA_VENTA - FECHA > 365/FRECUENCIA_PAGO_CUPON) & (format(FECHA_VENTA, format="%m-%d") >  format(FECHA_VENCIMIENTO, format="%m-%d")) ~ (floor(as.numeric(FECHA_VENTA - FECHA)/365)*FRECUENCIA_PAGO_CUPON)),
             VALOR_CUPON = CUPONES_REDIMIDOS*CUPON*(1+RENDIMIENTO_VENTA/100)^CUPONES_REDIMIDOS,
             FLUJO = (VALOR_CUPON + PRECIO_VENTA)/(1+RENDIMIENTO/100),
             TIR_DE_TENENCIA = ((FLUJO - PRECIO_SUCIO)/PRECIO_SUCIO)*100)
    
    
    
    
    FLUJO = ((((1+(CUPON/100))^CUPONES_REDIMIDOS)-1)/(CUPON/100))
    FLUJO = CUPON*CUPONES_REDIMIDOS/FRECUENCIA_PAGO_CUPON
    purchase.price <- data %>% dplyr::filter(FECHA == start.date) %>% select(CODIGO_ISIN, PRECIO_SUCIO) %>%
      rename(INICIO = PRECIO_SUCIO)
    selling.price <- purchase.price %>% rename(PRECIO_VENTA = INICIO)
    
    data %<>% mutate(DIA = day(FECHA_VENCIMIENTO), MES = month(FECHA_VENCIMIENTO))
      
      
      mutate(FLUJO = ((((1+(CUPON/100))^CUPONES_POR_COBRAR)-1)/(CUPON/100))) %>% 
      left_join(purchase.price) %>% left_join(selling.price) %>% drop_na(INICIO, FIN, FLUJO) %>%
      mutate(TIR_DE_TENENCIA = ((FLUJO + FIN - INICIO)/INICIO))
    
    
    
  }
