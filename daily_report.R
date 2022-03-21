##### INFORME DIARIO

#### Lectura / ModificaciÃ³n de datos

setwd("/home/nico/Documents/Personal/Informes Fiduagraria")

#### Carga

source("C:/Users/lpena/Documents/Documentos Analista/Datos/load_packages.R", encoding = "utf8") # Paquetes
source("functions.R", encoding = "utf8") # Funciones
source("style.R", encoding = "utf8") # Estilo
source("C:/Users/lpena/Documents/Documentos Analista/Datos/tidyquant_correction.R", encoding = "utf8") # CorrecciÃ³n (Tidyquant)

#### InicializaciÃ³n

locale(date_names = "es")
rezago <- 1  # 0 si se hace el informe el día anterior a su publicación, 1 si se hace el informe en el mismo día de su publicación, 3 si es lunes, 4 si es martes y el lunes fue festivo.
options(scipen = 999)
directorio <- "/home/nico/Documents/Personal/Informes Fiduagraria"
business.dates <- read_excel("Diario.xlsx", sheet = 1, skip = 5, col_types = "numeric")[,1] %>%
  pull() %>% as.Date(origin = "1899-12-30")

#### ActualizaciÃ³n

source("C:/Users/lpena/Documents/Documentos Analista/Datos/daily_update.R", encoding = "utf8") # ActualizaciÃ³n diaria

#### Mercado de dinero

banrep.nom <- read_excel("Diario.xlsx", sheet = 5, skip = 6, na = "#N/A N/A",
                     col_types = "numeric",
                     col_names = c("FECHA", "IBR 1", "IBR 1_CMBIO", "IBR 30", 
                                   "IBR 30_CMBIO", "IBR 90", "IBR 90_CMBIO", 
                                   "IBR 180", "IBR 180_CMBIO", "DTF", "DTF_CMBIO", 
                                   "UVR", "UVR_CMBIO", "DEMANDA", "DEMANDA_CMBIO", 
                                   "CUPO", "CUPO_CMBIO", "TRP", "TRP_CMBIO", 
                                   "TIP", "TIP_CMBIO")) %>% 
  mutate(FECHA = as.Date(FECHA, origin = "1899-12-30")) %>% 
  drop_na(FECHA) %>% dplyr::filter(FECHA >= today() - rezago - days(180))
  
banrep <- banrep.nom %>% mutate(`IBR 1` = calculate_IBREA(`IBR 1`,1), `IBR 30` = calculate_IBREA(`IBR 30`, 30),
         `IBR 90` = calculate_IBREA(`IBR 90`, 90), `IBR 180` = calculate_IBREA(`IBR 180`, 180),
         CUPO = round(CUPO/1000,2), CUPO_CMBIO = round(CUPO_CMBIO/1000,2), 
         DEMANDA = round(DEMANDA/1000,2), DEMANDA_CMBIO = round(DEMANDA_CMBIO/1000,2),
         UTILIZACION = ifelse(DEMANDA > CUPO, 100, DEMANDA/CUPO*100),
         SOBREPASO = ifelse(DEMANDA > CUPO, TRUE, FALSE))

tasas <- banrep %>% select(FECHA, starts_with("IBR"), starts_with("TIP")) %>% na.locf()
tasas.melt <- tasas %>% select(-ends_with("_CMBIO")) %>% melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR")
tasas.dif <- tasas %>% select(FECHA, ends_with("_CMBIO")) %>% melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR") %>%
  dplyr::filter(FECHA == today()-rezago, INDICADOR != "TIP_CMBIO") %>% mutate(INDICADOR = str_replace_all(INDICADOR, "_CMBIO", "")) %>% pull(VALOR)
tasas.ult <- tasas.melt %>% dplyr::filter(FECHA == today()-rezago, INDICADOR != "TIP") %>% pull(VALOR)

omas  <- banrep %>% select(FECHA, starts_with("CUPO"), starts_with("DEMANDA"), starts_with("TRP"), UTILIZACION, SOBREPASO) %>% na.locf()
omas.ult <- omas %>% select(-ends_with("_CMBIO")) %>% melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR") %>%
  dplyr::filter(FECHA == today()-rezago, INDICADOR %in% c("CUPO", "DEMANDA")) %>% pull(VALOR)

## Curva OIS

  tasas.img <- ggplot(tasas.melt, aes(x = FECHA, y = VALOR, color = INDICADOR)) + geom_step(size = 0.5, alpha = 1) +
  scale_x_bd(business.dates = business.dates, labels = date_format("%b"), max.major.breaks = 20, expand = c(0, 0)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.001, decimal.mark = ",", big.mark = ".", scale = 1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = tasas.ult, labels=paste(tasas.ult, "%"))) +
  scale_color_manual(values = colors_mm) + labs(x = "", y = "Tasa de interÃ©s (%)", color = "") + theme_diario() + no.y.label + no.x.label +
  theme(axis.text.y = element_text(size = rel(0.6)), plot.margin = unit(c(0.3, 0.1, 0.15, 0.175), "cm"),
    panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5),
    axis.line = element_blank(), legend.text.align = 0.5, legend.direction = "horizontal", legend.position = c(0.425,0.1),
    legend.spacing = unit(0, 'cm'), legend.key.size = unit(0.25, "cm"), axis.text.y.right = element_text(size=4, vjust = 0.5),
  ) + guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(plot = tasas.img, paste0(directorio,"MercadoMonetario.png"), width = 8.45, height = 5.31, units = "cm", dpi = 1000, type = "cairo-png")

## Operaciones de Mercado Abierto [OMAs]

omas.img <- ggplot(omas) + geom_col(aes(y = CUPO, x = FECHA), width = 1, na.rm = TRUE, fill = "#BAB0AC") +
  geom_col(aes(y = DEMANDA, x = FECHA, fill = SOBREPASO), width = 1, na.rm = TRUE, position = "stack") +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 0.1, unit = "BN", big.mark = ".", decimal.mark = ","), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = omas.ult, labels=paste(omas.ult, "BN"))) +
  scale_x_bd(business.dates = business.dates, labels = date_format("%d %b"), max.major.breaks = 20, expand = c(0, 0)) +
  theme_diario() + no.x.label + no.y.label + scale_fill_manual(values = c("#76B7B2", "#E15759")) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)), plot.margin = unit(c(0.3, 0.1, 0.1, 0), "cm"),
    panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5), legend.position = "none",
    axis.line = element_blank(), axis.text.y = element_text(size = rel(0.6))
  )

ggsave(plot = omas.img, paste0(directorio,"DemandaDinero.png"), width = 7.45, height = 5.31, units = "cm", dpi = 1000, type = "cairo-png")

#### Mercado cambiario

dolar <- read_excel("Diario.xlsx", sheet = 1, skip = 6, col_types = "numeric", 
                    col_names = c("FECHA", "APERTURA", "MAXIMO",
                                  "MINIMO", "ULTIMO", "VOLUMEN",
                                  "VOLATILIDAD", "DEPRECIACION")) %>%
  mutate(FECHA = as.Date(FECHA, origin = "1899-12-30"), VOLUMEN = VOLUMEN/1000000)

dolar.HLC <- as.matrix(dolar[, 3:5])

dolar %<>% mutate(
  MEDIA = EVWMA(dolar$ULTIMO, dolar$VOLUMEN, n = 200),
  SUPERIOR = BBands(dolar.HLC, n = 90)[, 3],
  INFERIOR = BBands(dolar.HLC, n = 90)[, 1])

dolar %<>% dplyr::filter(FECHA >= (today() - rezago - days(720)))

zoomed_dolar <- dolar %>% minmax_filter()

fibonacci <- max(dolar[, 2:5]) - min(dolar[, 2:5])
fibonacci_zoomed <- max(zoomed_dolar[, 2:5]) - min(zoomed_dolar[, 2:5])

fibonacci_interval <- c(
  min(dolar[, 2:5]) + 0.236 * fibonacci,
  min(dolar[, 2:5]) + 0.382 * fibonacci,
  min(dolar[, 2:5]) + 0.500 * fibonacci,
  min(dolar[, 2:5]) + 0.618 * fibonacci,
  min(dolar[, 2:5]) + 0.764 * fibonacci
)

fibonacci_interval_zoomed <- c(
  min(zoomed_dolar[, 2:5]) + 0.236 * fibonacci_zoomed,
  min(zoomed_dolar[, 2:5]) + 0.382 * fibonacci_zoomed,
  min(zoomed_dolar[, 2:5]) + 0.500 * fibonacci_zoomed,
  min(zoomed_dolar[, 2:5]) + 0.618 * fibonacci_zoomed,
  min(zoomed_dolar[, 2:5]) + 0.764 * fibonacci_zoomed
)

  first.min.date = dolar$FECHA[dolar$MINIMO == min(dolar$MINIMO)]
  last.min.date = "2020-01-08"
  first.max.date = "2020-03-12"
  last.max.date = "2020-03-12"

channel <- channel_select(dolar, manual = TRUE, trend = "up")

zoomed_channel <- channel_zoom(zoomed_dolar, channel)

dolar.primaria.precio <- ggplot() + annotate("rect", xmin = min(zoomed_dolar$FECHA), xmax = max(zoomed_dolar$FECHA), ymin = min(dolar[, 2:5]), ymax = max(dolar[, 2:5]), fill = "#e2e2de") +
  geom_hline(yintercept = fibonacci_interval[1], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 23.6
  geom_hline(yintercept = fibonacci_interval[2], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 38.2
  geom_hline(yintercept = fibonacci_interval[3], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 50
  geom_hline(yintercept = fibonacci_interval[4], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 61.8
  geom_hline(yintercept = fibonacci_interval[5], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 76.4
  geom_ribbon(data = channel, mapping = aes(x = FECHA, ymin = MIN, ymax = MAX), alpha = 0.1) +
  geom_candlestick(data = dolar, mapping = aes(x = FECHA, open = APERTURA, high = MAXIMO, low = MINIMO, close = ULTIMO), size = 0.1) + scale_y_continuous(breaks = extended_range_breaks(), labels = number_format(accuracy = 1, big.mark = ".", decimal.mark = ","), expand = c(0, 0)) +
  geom_line(data = dolar, mapping = aes(x = FECHA, y = MEDIA), color = "#7F7F7F", size = 0.1) +
  geom_line(data = dolar, mapping = aes(x = FECHA, y = SUPERIOR), color = "#7F7F7F", linetype = "dashed", size = 0.1) +
  geom_line(data = dolar, mapping = aes(x = FECHA, y = INFERIOR), color = "#7F7F7F", linetype = "dashed", size = 0.1) +
  scale_x_bd(business.dates = dolar$FECHA, labels = date_format("%b %y"), max.major.breaks = 30, expand = c(0, 0)) +
  theme_diario() + theme(
    panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5),
    axis.line = element_blank(),
    axis.text.y = element_text(size = rel(0.6)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5))
  ) +
  no.y.label + no.x.label + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  coord_cartesian(ylim = c(min(dolar$MINIMO), max(dolar$MAXIMO)))

dolar.primaria.volumen <- ggplot(dolar, aes(x = FECHA, y = VOLUMEN)) +
  annotate("rect", xmin = min(zoomed_dolar$FECHA), xmax = max(zoomed_dolar$FECHA), ymin = min(dolar[, 6]), ymax = max(dolar[, 6]), fill = "#e2e2de") +
  geom_col() + scale_y_continuous(breaks = extended_range_breaks(), labels = number_format(accuracy = 0.1, decimal.mark = ",", big.mark = "."), expand = c(0, 0)) +
  scale_x_bd(business.dates = dolar$FECHA, labels = date_format("%b %y"), max.major.breaks = 30, expand = c(0, 0)) +
  theme_diario() + theme(
    panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5),
    axis.line = element_blank(),
    axis.text.y = element_text(size = rel(0.4)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5))
  ) +
  no.y.label + no.x.label + theme(plot.margin = unit(c(0, 0, 0.1, 0), "cm"))

dolar.primaria <- plot_grid(dolar.primaria.precio, dolar.primaria.volumen, ncol = 1, align = "v", rel_heights = c(3, 1))

dates <- as.Date(c("2019-05-27", "2019-06-08"))

dolar.intermedia.precio <- ggplot() + geom_ribbon(data = zoomed_channel, mapping = aes(x = FECHA, ymin = MIN, ymax = MAX), alpha = 0.1) +
  geom_candlestick(data = zoomed_dolar, mapping = aes(x = FECHA, open = APERTURA, high = MAXIMO, low = MINIMO, close = ULTIMO)) +
  geom_line(data = zoomed_dolar, mapping = aes(x = FECHA, y = MEDIA), color = "#7F7F7F", size = 0.1) +
  geom_line(data = zoomed_dolar, mapping = aes(x = FECHA, y = SUPERIOR), color = "#7F7F7F", linetype = "dashed", size = 0.1) +
  geom_line(data = zoomed_dolar, mapping = aes(x = FECHA, y = INFERIOR), color = "#7F7F7F", linetype = "dashed", size = 0.1) +
  geom_hline(yintercept = fibonacci_interval_zoomed[1], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 23.6
  geom_hline(yintercept = fibonacci_interval_zoomed[2], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 38.2
  geom_hline(yintercept = fibonacci_interval_zoomed[3], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 50
  geom_hline(yintercept = fibonacci_interval_zoomed[4], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 61.8
  geom_hline(yintercept = fibonacci_interval_zoomed[5], colour = "black", size = 0.2, linetype = "dotted") + # Fibonacci 76.4
  scale_y_continuous(position = "right", breaks = extended_range_breaks_(min(zoomed_dolar$MINIMO), max(zoomed_dolar$MAXIMO)), labels = number_format(accuracy = 1, big.mark = ".", decimal.mark = ","), expand = c(0, 0), sec.axis = dup_axis(breaks = fibonacci_interval_zoomed, labels = c("23,6", "38,2", "50", "61,8", "76,4"))) +
  scale_x_bd(business.dates = zoomed_dolar$FECHA, labels = date_format("%d %b"), max.major.breaks = 30, expand = c(0, 0)) +
  theme_diario() + theme(
    panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5),
    axis.text.y = element_text(size = rel(0.6)),
    axis.line = element_blank(), panel.background = element_rect(fill = "#e2e2de", colour = "#e2e2de"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)),
    axis.text.y.left = element_text(hjust = 1, vjust = 0.5, size = rel(1), face = "bold")
  ) + no.y.label + no.x.label + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + coord_cartesian(ylim = c(min(zoomed_dolar$MINIMO), max(zoomed_dolar$MAXIMO)))

dolar.intermedia.volumen <- ggplot(zoomed_dolar, aes(x = FECHA, y = VOLUMEN)) + geom_col() +
  scale_y_continuous(
    position = "right", breaks = extended_range_breaks(), labels = number_format(accuracy = 0.1, decimal.mark = ",", big.mark = "."), expand = c(0, 0),
    sec.axis = dup_axis(breaks = 0.8, labels = "Millones de USD")
  ) +
  scale_x_bd(business.dates = zoomed_dolar$FECHA, labels = date_format("%d %b"), max.major.breaks = 30, expand = c(0, 0)) +
  theme_diario() + theme(
    panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5),
    axis.line = element_blank(), panel.background = element_rect(fill = "#e2e2de", colour = "#e2e2de"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)),
    axis.text.y = element_text(size = rel(0.4)),
    axis.text.y.left = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = rel(1), face = "bold"),
    axis.ticks.y.left = element_blank(),
    plot.margin = unit(c(0, 0, 0.1, 0), "cm")
  ) + no.y.label + no.x.label 

dolar.intermedia <- plot_grid(dolar.intermedia.precio, dolar.intermedia.volumen, ncol = 1, align = "v", rel_heights = c(3, 1))

dolar.img <- grid.arrange(dolar.primaria, dolar.intermedia, nrow = 1)
ggsave(plot = dolar.img, paste0(directorio, "Dolar.png"), width = 17.90, height = 5.5, units = "cm", dpi = 1000, type = "cairo-png")

## Datos econÃ³micos

datos <- read_excel("Diario.xlsx", sheet = 3, skip = 7, col_type = "numeric", col_names = c("FECHA", "WTI", "DIFWTI", 
                                                                      "BRENT", "DIFBRT", "IPC (A/A)", 
                                                                      "IPC (M/M)", "IPC (A. corr.)", "PIB", "ISE", 
                                                                      "Desempleo")) %>% 
  mutate(FECHA = as.Date(FECHA, origin = "1899-12-30")) %>% tail(90) 

petroleo <- datos %>% select(FECHA, BRENT, WTI) %>% melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR")
petroleo.dif <- datos %>% select(FECHA, starts_with("DIF")) %>% melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR") %>% dplyr::filter(FECHA == today()-rezago) %>% pull(VALOR)
petroleo.ult <- petroleo %>% dplyr::filter(FECHA == today()-rezago) %>% pull(VALOR)

petroleo.img <- ggplot(petroleo, aes(x = FECHA, y = VALOR, color = INDICADOR)) + geom_line(size = 0.25, alpha = 1) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 0.1, decimal.mark = ",", big.mark = ".", scale = 1, suffix = " USD"), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = petroleo.ult, labels=paste0(petroleo.ult, " USD\nDif. ", petroleo.dif))) +
  scale_color_manual(values = colors_oil) + labs(x = "", y = "", color = "") + theme_diario(base_size = 8) + no.y.label + no.x.label +
  theme(axis.text = element_text(size = rel(0.6)), plot.margin = unit(c(0.1, 0.25, 0, 0), "cm"),
        panel.border = element_rect(colour = "#7F7F7F", fill = NA, size = 0.5),
        axis.line = element_blank(), legend.text.align = 0, legend.position = c(1.15,0.015), 
        legend.spacing = unit(0, 'cm'), legend.key.size = unit(0.2, "cm"), legend.margin=margin(-10,-10,-10,-10),
        legend.box.margin=margin(-5,-5,-5,-5), legend.text = element_text(margin = margin(-2,-2,-2,-2))
  ) + guides(color = guide_legend(override.aes = list(size = 2)))

ggsave(plot=petroleo.img, paste0(directorio, "Petroleo.png"), width = 4, height = 1.75, units = "cm", dpi = 1000, type = "cairo-png")

tasas.vigentes <- banrep.nom %>% select(FECHA, DTF, starts_with("IBR"), -ends_with("_CMBIO"), UVR) %>% dplyr::filter( 
  if(rezago == 3) {
    FECHA == today()-3
  } else if (rezago == 4) {
      FECHA == today()-4
  } else if (rezago == 0) {
    FECHA == today()    
  } else
    FECHA == today()-1) %>% 
  melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR") %>%    
  mutate(VALOR = round(VALOR, 2), LARGO = 1, ANCHO = 1, UNIDAD.SF = c(rep(" %",5), ""), UNIDAD.PF = c(rep("",5), "$"))

ggsave(plot=ggplot(tasas.vigentes, aes(x = LARGO, y = ANCHO)) + geom_tile(fill = "#EEEEEE") +
         geom_text(aes(label=paste0(UNIDAD.PF,VALOR,UNIDAD.SF)), family = "Arial Narrow", color = "#7f7f7f", fontface = "bold", size = 2.75) + facet_grid(. ~ INDICADOR) + theme_diario(base_size = 6.75) +
         theme(panel.grid.major = element_blank(), aspect.ratio = NULL, axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
               legend.title = element_blank(), legend.position = "none", axis.line = element_blank(),
               plot.margin = unit(c(0, 0, 0, 0), "cm"), panel.spacing.x = unit(-1, "pt"), strip.background = element_rect(fill="#7f7f7f", color = "#7f7f7f"), strip.text.x = element_text(colour="#F8F8F8", vjust = 0)),
       paste0(directorio, "TasasVigentes.png"), width = 6, height = 1, units = "cm", dpi = 1000, type = "cairo-png")

indicadores <- datos %>% select(-matches("WTI"),-matches("BR")) %>% dplyr::filter(FECHA == today()-rezago) %>%
  melt(id = "FECHA", value.name = "VALOR", variable.name = "INDICADOR") %>% mutate(LARGO = 1, ANCHO = 1)

ggsave(plot=ggplot(indicadores, aes(x = LARGO, y = ANCHO)) + geom_tile(fill = "#EEEEEE") +
  geom_text(aes(label=paste(VALOR, "%")), family = "Arial Narrow", color = "#7f7f7f", fontface = "bold", size = 2.75) + facet_grid(. ~ INDICADOR) + theme_diario(base_size = 6.75) +
  theme(panel.grid.major = element_blank(), aspect.ratio = NULL, axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    legend.title = element_blank(), legend.position = "none", axis.line = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"), panel.spacing.x = unit(-1, "pt"), strip.background = element_rect(fill="#7f7f7f", color = "#7f7f7f"), strip.text.x = element_text(colour="#F8F8F8", vjust = 0)),
  paste0(directorio, "Indicadores.png"), width = 6, height = 1, units = "cm", dpi = 1000, type = "cairo-png")

## Ãndices bursÃ¡tiles

indices <- read.xlsx("Diario.xlsx", sheet = 2, startRow = 8, colNames = FALSE)

names(indices) <- c("FECHA", "COLCAP", "DOW", "S&P", "STOXX", "DAX", "SHANGAI", "TOPIX")

indices$FECHA %<>% as.Date(origin = "1899-12-30")

base <- slice(indices, which(indices$FECHA == as.Date("2018-01-02")))

indices %<>% group_by(FECHA) %>% mutate(
  COLCAP = COLCAP * 100 / (base[2] %>% pull()),
  DOW = DOW * 100 / (base[3] %>% pull()),
  `S&P` = `S&P` * 100 / (base[4] %>% pull()),
  STOXX = STOXX * 100 / (base[5] %>% pull()),
  DAX = DAX * 100 / (base[6] %>% pull()),
  SHANGAI = SHANGAI * 100 / (base[7] %>% pull()),
  TOPIX = TOPIX * 100 / (base[8] %>% pull())
)

indices %<>% gather(key = "INDICE", value = "VALOR", -FECHA) %>%
  mutate(GRUPO = if_else(INDICE == "COLCAP", "Colombia",
    if_else(INDICE %in% c("DOW", "S&P"), "EE.UU",
      if_else(INDICE %in% c("STOXX", "DAX"), "Europa",
        if_else(INDICE %in% c("SHANGAI", "TOPIX"), "Asia", "Ninguno")
      )
    )
  ))

indices$INDICE <- factor(indices$INDICE, levels = c("COLCAP", "DOW", "S&P", "STOXX", "DAX", "SHANGAI", "TOPIX"))
indices$GRUPO <- factor(indices$GRUPO, levels = c("Colombia", "EE.UU", "Europa", "Asia"))

indices %<>% dplyr::filter(FECHA >= (today() - rezago - weeks(24)))

## Renta fija

# Deuda: BANCOS AAA/GUBERNAMENTAL

corporativa <- deuda$corporativa %>% filter_by_aggregate(aggregate = "BANCOS_AAA")

gubernamental <- deuda$gubernamental %>% filter_by_aggregate(aggregate = "GOBIERNO")

# Nelson-Siegel

nelsonsiegel <- read_rds("NelsonSiegel.rds")
#nelsonsiegel <- read_rds("C:/Users/lpena/Documents/Documentos Analista/Datos/NelsonSiegel.rds")
##############################################################################################################################################################################################
deuda.ns.sf <- nelsonsiegel %>%
  dplyr::filter(FECHA %in% c(today()-rezago, today() - rezago - 7)) %>%
  mutate(LINEA = factor(FECHA, as.character(c(today() - rezago, today() - rezago - 7)), labels = c("Vigente", "Semanal"))) %>%
  mutate(REFERENCIA = factor(REFERENCIA, levels = c("DTF", "IB1", "FS", "IPC", "COP", "UVR")))

#deuda.hoy <- nelsonsiegel %>%
#  dplyr::filter(FECHA %in% c(today()-rezago))
#deuda.7dias <- nelsonsiegel %>%
#  dplyr::filter(FECHA %in% c(today()-rezago-7))
#deuda.ns.sf <- merge(deuda.hoy, deuda.7dias, sort = TRUE) %>%
#  mutate(LINEA = factor(FECHA, as.character(c(today() - rezago, today() - rezago - 7)), labels = c("Vigente", "Semanal"))) %>%
#  mutate(REFERENCIA = factor(REFERENCIA, levels = c("DTF", "IB1", "FS", "IPC", "COP", "UVR")))

#class(ultimasemana)
#ultimasemana<-c(today()-rezago, today()-rezago-7)
#prueba1<-nelsonsiegel
#prueba1<-  dplyr::filter(prueba1, FECHA %in% c(today()-rezago-7, today()-rezago))
#prueba1<-mutate(LINEA = factor(FECHA, as.character(c(today() - rezago, today() - rezago - 7)), labels = c("Vigente", "Semanal")))

# CÃ¡lculo de Medias

corporativa.media <- corporativa %>%
  dplyr::filter(FECHA %in% c(today() - rezago, today() - rezago - 1, today() - rezago - 7)) %>%
  node_aggregation(filter = "REFERENCIA")

gubernamental.media <- gubernamental %>%
  dplyr::filter(FECHA %in% c(today() - rezago, today() - rezago - 1, today() - rezago - 7)) %>%
  reduce_sample(filter = "REFERENCIA", value = "NONE")

ref_names <- c(FS = "TASA FIJA", IPC = "IPC", IB1 = "IBR", DTF = "DTF", COP = "TES COP", UVR = "TES UVR")

exps <- gubernamental.media$FECHA_VENCIMIENTO

exps.cop <- gubernamental.media$FECHA_VENCIMIENTO[gubernamental.media$REFERENCIA == "COP"]
exps.uvr <- gubernamental.media$FECHA_VENCIMIENTO[gubernamental.media$REFERENCIA == "UVR"]

nodes <- seq.Date(today() - rezago, today() - rezago + years(15), by = 7)

deuda.media.sf <- bind_rows(corporativa.media, gubernamental.media) %>%
  ungroup() %>%
  mutate(LINEA = factor(FECHA, as.character(c(today() - rezago, today() - rezago - 1, today() - rezago - 7)), labels = c("Vigente", "Ayer", "Semanal"))) %>%
  mutate(REFERENCIA = factor(REFERENCIA, levels = c("DTF", "IB1", "FS", "IPC", "COP", "UVR"))) %>%
  dplyr::filter( # (FECHA_VENCIMIENTO %in% exps | FECHA_VENCIMIENTO %in% nodes) &
    FECHA_VENCIMIENTO >= today() - rezago,
    FECHA %in% c(today() - rezago, today() - rezago - 7)
  )

# Filtrado

deuda.media <- deuda.media.sf %>%
  dplyr::filter((REFERENCIA %in% c("DTF") & AÑOS_AL_VENCIMIENTO <= 1) |
    (REFERENCIA %in% c("IB1") & AÑOS_AL_VENCIMIENTO <= 2) |
    (REFERENCIA %in% c("FS", "IPC") & AÑOS_AL_VENCIMIENTO <= 5) |
    (REFERENCIA %in% c("COP", "UVR") & AÑOS_AL_VENCIMIENTO <= 15)) %>%
  mutate(PUNTO = case_when(
    REFERENCIA %in% c("DTF", "IB1", "FS", "IPC") ~ 10 / 365,
    REFERENCIA %in% c("COP", "UVR") ~ 50 / 365
  ))

deuda.ns <- deuda.ns.sf %>% dplyr::filter((REFERENCIA %in% c("DTF") & MADURACION <= 1) |
  (REFERENCIA %in% c("IB1") & MADURACION <= 2) |
  (REFERENCIA %in% c("FS", "IPC") & MADURACION <= 5) |
  (REFERENCIA %in% c("COP", "UVR") & MADURACION <= 15))

# Diferencias

spread.ns <- deuda.ns %>%
  select(-FECHA) %>%
  dcast(REFERENCIA + MADURACION ~ LINEA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = ((Vigente - Semanal) * 100))

spread.media <- deuda.media %>%
  select(FECHA_VENCIMIENTO, RENDIMIENTO, LINEA, REFERENCIA) %>%
  dcast(REFERENCIA + FECHA_VENCIMIENTO ~ LINEA, value.var = "RENDIMIENTO") %>%
  mutate(
    DIFERENCIA = ((Vigente - Semanal) * 100), MADURACION = time_length(lubridate::interval(today() - rezago, FECHA_VENCIMIENTO), unit = "years"),
    GROSOR = case_when(REFERENCIA %in% c("DTF") ~ 7 / 365, REFERENCIA %in% c("IB1") ~ 14 / 365, REFERENCIA %in% c("FS", "IPC") ~ 30 / 365, REFERENCIA %in% c("COP", "UVR") ~ 120 / 365)
  )

# Comparaciones

corp.media <- deuda.media.sf %>% dplyr::filter(
  REFERENCIA %in% c("DTF", "IB1", "FS", "IPC"), DIAS_AL_VENCIMIENTO <= 1825,
  FECHA == today() - rezago
)

corp.ns <- deuda.ns.sf %>%
  mutate(DIAS_AL_VENCIMIENTO = MADURACION * 365) %>%
  dplyr::filter(
    REFERENCIA %in% c("DTF", "IB1", "FS", "IPC"), DIAS_AL_VENCIMIENTO <= 1825,
    FECHA == today() - rezago
  )

fs.cop.media <- deuda.media.sf %>% dplyr::filter(
  REFERENCIA %in% c("FS", "COP"), DIAS_AL_VENCIMIENTO <= 1825,
  FECHA == today() - rezago, FECHA_VENCIMIENTO %in% exps.cop
)
fs.cop.media.spread <- fs.cop.media %>%
  select(FECHA_VENCIMIENTO, RENDIMIENTO, REFERENCIA) %>%
  dcast(FECHA_VENCIMIENTO ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (FS - COP) * 100, DIAS_AL_VENCIMIENTO = as.integer(time_length(lubridate::interval(today() - rezago, FECHA_VENCIMIENTO), unit = "years") * 365))

fs.cop.ns <- deuda.ns.sf %>%
  mutate(DIAS_AL_VENCIMIENTO = MADURACION * 365) %>%
  dplyr::filter(REFERENCIA %in% c("FS", "COP"), DIAS_AL_VENCIMIENTO <= 1825, FECHA == today() - rezago)
fs.cop.ns.spread <- fs.cop.ns %>%
  select(MADURACION, RENDIMIENTO, REFERENCIA) %>%
  dcast(MADURACION ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (FS - COP) * 100, DIAS_AL_VENCIMIENTO = MADURACION * 365)

ipc.uvr.media <- deuda.media.sf %>% dplyr::filter(
  REFERENCIA %in% c("IPC", "UVR"), DIAS_AL_VENCIMIENTO <= 1825,
  FECHA == today() - rezago, FECHA_VENCIMIENTO %in% exps.uvr
)
ipc.uvr.media.spread <- ipc.uvr.media %>%
  select(FECHA_VENCIMIENTO, RENDIMIENTO, REFERENCIA) %>%
  dcast(FECHA_VENCIMIENTO ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (IPC - UVR) * 100, DIAS_AL_VENCIMIENTO = as.integer(time_length(lubridate::interval(today() - rezago, FECHA_VENCIMIENTO), unit = "years") * 365))

ipc.uvr.ns <- deuda.ns.sf %>%
  mutate(DIAS_AL_VENCIMIENTO = MADURACION * 365) %>%
  dplyr::filter(
    REFERENCIA %in% c("IPC", "UVR"), DIAS_AL_VENCIMIENTO <= 1825,
    FECHA == today() - rezago
  )
ipc.uvr.ns.spread <- ipc.uvr.ns %>%
  select(MADURACION, RENDIMIENTO, REFERENCIA) %>%
  dcast(MADURACION ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (IPC - UVR) * 100, DIAS_AL_VENCIMIENTO = MADURACION * 365)

fs.ipc.media <- deuda.media.sf %>% dplyr::filter(
  REFERENCIA %in% c("FS", "IPC"), DIAS_AL_VENCIMIENTO <= 1825,
  FECHA == today() - rezago
)
fs.ipc.media.spread <- fs.ipc.media %>%
  select(FECHA_VENCIMIENTO, RENDIMIENTO, REFERENCIA) %>%
  dcast(FECHA_VENCIMIENTO ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (FS - IPC) * 100, DIAS_AL_VENCIMIENTO = as.integer(time_length(lubridate::interval(today() - rezago, FECHA_VENCIMIENTO), unit = "years") * 365))

fs.ipc.ns <- deuda.ns.sf %>%
  mutate(DIAS_AL_VENCIMIENTO = MADURACION * 365) %>%
  dplyr::filter(REFERENCIA %in% c("FS", "IPC"), DIAS_AL_VENCIMIENTO <= 1825, FECHA == today() - rezago)
fs.ipc.ns.spread <- fs.ipc.ns %>%
  select(MADURACION, RENDIMIENTO, REFERENCIA) %>%
  dcast(MADURACION ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (FS - IPC) * 100, DIAS_AL_VENCIMIENTO = MADURACION * 365)

cop.uvr.media <- deuda.media.sf %>% dplyr::filter(
  REFERENCIA %in% c("UVR", "COP"), DIAS_AL_VENCIMIENTO <= 5475,
  FECHA == today() - rezago, FECHA_VENCIMIENTO %in% exps
)
cop.uvr.media.spread <- cop.uvr.media %>%
  select(FECHA_VENCIMIENTO, RENDIMIENTO, REFERENCIA) %>%
  dcast(FECHA_VENCIMIENTO ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (COP - UVR) * 100, DIAS_AL_VENCIMIENTO = as.integer(time_length(lubridate::interval(today() - rezago, FECHA_VENCIMIENTO), unit = "years") * 365))

cop.uvr.ns <- deuda.ns.sf %>%
  mutate(DIAS_AL_VENCIMIENTO = MADURACION * 365) %>%
  dplyr::filter(REFERENCIA %in% c("UVR", "COP"), DIAS_AL_VENCIMIENTO <= 5475, FECHA == today() - rezago)
cop.uvr.ns.spread <- cop.uvr.ns %>%
  select(MADURACION, RENDIMIENTO, REFERENCIA) %>%
  dcast(MADURACION ~ REFERENCIA, value.var = "RENDIMIENTO") %>%
  mutate(DIFERENCIA = (COP - UVR) * 100, DIAS_AL_VENCIMIENTO = MADURACION * 365)

# Comportamiento histÃ³rico

corporativa.resumen <- corporativa %>%
  dplyr::filter(FECHA >= today() - rezago - 30) %>%
  node_aggregation(filter = "REFERENCIA") %>%
  mutate(NODO = case_when(
    DIAS_AL_VENCIMIENTO %[]% c(0, 30) ~ "0--30",
    DIAS_AL_VENCIMIENTO %[]% c(31, 61) ~ "31--61",
    DIAS_AL_VENCIMIENTO %[]% c(62, 92) ~ "62--92",
    DIAS_AL_VENCIMIENTO %[]% c(93, 181) ~ "93--181",
    DIAS_AL_VENCIMIENTO %[]% c(182, 273) ~ "181--273",
    DIAS_AL_VENCIMIENTO %[]% c(274, 365) ~ "274--365",
    DIAS_AL_VENCIMIENTO %[]% c(366, 548) ~ "366--548",
    DIAS_AL_VENCIMIENTO %[]% c(549, 730) ~ "549--730",
    DIAS_AL_VENCIMIENTO %[]% c(731, 1095) ~ "731--1095",
    DIAS_AL_VENCIMIENTO %[]% c(1096, 1460) ~ "1096--1460",
    DIAS_AL_VENCIMIENTO %[]% c(1461, 1825) ~ "1461--1825",
    DIAS_AL_VENCIMIENTO %[]% c(1826, 2190) ~ "1826--2190",
    DIAS_AL_VENCIMIENTO %[]% c(2191, 2555) ~ "2191--2555",
    DIAS_AL_VENCIMIENTO > 2555 ~ "$>$ 2555"
  ))

gubernamental.resumen <- gubernamental %>%
  dplyr::filter(FECHA >= today() - rezago - 30) %>%
  reduce_sample(filter = "REFERENCIA", value = "NONE") 

# %>%
#   mutate(FECHA_VENCIMIENTO = year(FECHA_VENCIMIENTO), RENDIMIENTO = round(RENDIMIENTO, 2)) %>%
#   group_by(FECHA_VENCIMIENTO, REFERENCIA) %>% mutate(DIFERENCIA = round(RENDIMIENTO - Lag(RENDIMIENTO),2),
#                                                      COLOR = ifelse(DIFERENCIA > 0, "#D01C8B", "#4DAC26"))
# 
# gubernamental.mins <- group_by(gubernamental.resumen, FECHA_VENCIMIENTO, REFERENCIA) %>% slice(which.min(RENDIMIENTO))
# gubernamental.maxs <- group_by(gubernamental.resumen, FECHA_VENCIMIENTO, REFERENCIA) %>% slice(which.max(RENDIMIENTO))
# gubernamental.first <- group_by(gubernamental.resumen, FECHA_VENCIMIENTO, REFERENCIA) %>% slice(which.min(FECHA))
# gubernamental.ends <- group_by(gubernamental.resumen, FECHA_VENCIMIENTO, REFERENCIA) %>% dplyr::filter(FECHA == today()-rezago)
# gubernamental.quarts <- group_by(gubernamental.resumen, FECHA_VENCIMIENTO, REFERENCIA) %>%
#   summarize(Q25 = quantile(RENDIMIENTO, 0.25),
#             Q75 = quantile(RENDIMIENTO, 0.75)) %>%
#   right_join(gubernamental.resumen)
# 
# ggsave(plot=ggplot(gubernamental.resumen %>% dplyr::filter(REFERENCIA == "COP"), aes(x=FECHA, y=RENDIMIENTO)) + 
#   facet_grid(FECHA_VENCIMIENTO ~ ., scales = "free_y") + 
#   geom_ribbon(data = gubernamental.quarts %>% dplyr::filter(REFERENCIA == "COP"), aes(ymin = Q25, ymax = Q75), fill = 'grey90') +
#   geom_line(size=0.1) +
#   geom_point(data = gubernamental.mins %>% dplyr::filter(REFERENCIA == "COP") , col = '#F28E2B', size = 0.001) +
#   geom_text(data = gubernamental.mins %>% dplyr::filter(REFERENCIA == "COP") , aes(label = RENDIMIENTO), vjust = -1, size = 1.5) +
#   geom_point(data = gubernamental.maxs %>% dplyr::filter(REFERENCIA == "COP"), col = '#76B7B2', size = 0.001) +
#   geom_text(data = gubernamental.maxs %>% dplyr::filter(REFERENCIA == "COP"), aes(label = RENDIMIENTO), vjust = 2, size = 1.5) +
#   geom_text(data = gubernamental.ends %>% dplyr::filter(REFERENCIA == "COP") , aes(label = RENDIMIENTO), hjust = 0, size = 3) +
#   geom_text(data = gubernamental.ends %>% dplyr::filter(REFERENCIA == "COP") , aes(label = paste0("(",DIFERENCIA,")"), color = COLOR), hjust = 1, nudge_x = 12, size = 3) +
#   geom_text(data = gubernamental.ends %>% dplyr::filter(REFERENCIA == "COP") , aes(label = FECHA_VENCIMIENTO), hjust = 1, nudge_x = 20, size = 3) +
#   expand_limits(x = max(gubernamental.resumen$FECHA) + (0.25 * (max(gubernamental.resumen$FECHA) - min(gubernamental.resumen$FECHA)))) +
#   scale_x_bd(business.dates = business.dates, expand = c(0.01,0.01)) +
#   scale_y_continuous(expand = c(0.25, 0.25)) +
#   theme_diario(base_size = 3) +
#   theme(strip.text = element_blank(),
#     panel.grid.major = element_blank(), aspect.ratio = NULL, axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
#         legend.title = element_blank(), legend.position = "none", axis.line = element_blank(),
#         plot.margin = unit(c(0, 0, 0, 0), "cm"), panel.spacing.x = unit(0, "pt"), strip.background = element_rect(fill="#7f7f7f", color = "#7f7f7f")),
#   paste0(directorio, "TESCOP.png"), width = 9.3, height = 6, units = "cm", dpi = 1000, type = "cairo-png")
# 
# ggsave(plot=ggplot(gubernamental.resumen %>% dplyr::filter(REFERENCIA == "UVR"), aes(x=FECHA, y=RENDIMIENTO)) + 
#          facet_grid(FECHA_VENCIMIENTO ~ ., scales = "free_y") + 
#          geom_ribbon(data = gubernamental.quarts %>% dplyr::filter(REFERENCIA == "UVR"), aes(ymin = Q25, ymax = Q75), fill = 'grey90') +
#          geom_line(size=0.1) +
#          geom_point(data = gubernamental.mins %>% dplyr::filter(REFERENCIA == "UVR") , col = '#F28E2B', size = 0.001) +
#          geom_text(data = gubernamental.mins %>% dplyr::filter(REFERENCIA == "UVR") , aes(label = RENDIMIENTO), vjust = -1, size = 1.5) +
#          geom_point(data = gubernamental.maxs %>% dplyr::filter(REFERENCIA == "UVR"), col = '#76B7B2', size = 0.001) +
#          geom_text(data = gubernamental.maxs %>% dplyr::filter(REFERENCIA == "UVR"), aes(label = RENDIMIENTO), vjust = 2, size = 1.5) +
#          geom_text(data = gubernamental.ends %>% dplyr::filter(REFERENCIA == "UVR") , aes(label = RENDIMIENTO), hjust = 0, size = 3) +
#          geom_text(data = gubernamental.ends %>% dplyr::filter(REFERENCIA == "UVR") , aes(label = paste0("(",DIFERENCIA,")"), color = COLOR), hjust = 1, nudge_x = 12, size = 3) +
#          geom_text(data = gubernamental.ends %>% dplyr::filter(REFERENCIA == "UVR") , aes(label = FECHA_VENCIMIENTO), hjust = 1, nudge_x = 20, size = 3) +
#          expand_limits(x = max(gubernamental.resumen$FECHA) + (0.25 * (max(gubernamental.resumen$FECHA) - min(gubernamental.resumen$FECHA)))) +
#          scale_x_bd(business.dates = business.dates, expand = c(0.01,0.01)) +
#          scale_y_continuous(expand = c(0.25, 0.25)) +
#          theme_diario(base_size = 3) +
#          theme(strip.text = element_blank(),
#                panel.grid.major = element_blank(), aspect.ratio = NULL, axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
#                legend.title = element_blank(), legend.position = "none", axis.line = element_blank(),
#                plot.margin = unit(c(0, 0, 0, 0), "cm"), panel.spacing.x = unit(0, "pt"), strip.background = element_rect(fill="#7f7f7f", color = "#7f7f7f")),
#        paste0(directorio, "TESUVR.png"), width = 9.3, height = 6, units = "cm", dpi = 1000, type = "cairo-png")


for (r in c("COP","UVR")){

resumen <- tibble(Vcto. = as.numeric(0), `Últimos 30 días` = as.character(0), `Rend.` = as.numeric(0), Dif. = as.numeric(0))[0,]

for (i in sort(unique(gubernamental.resumen$FECHA_VENCIMIENTO[gubernamental.resumen$REFERENCIA == r]))) {

data <- gubernamental.resumen %>% dplyr::filter(REFERENCIA == r, FECHA_VENCIMIENTO == as.Date(i))

xdots <- c(data$FECHA[data$RENDIMIENTO == max(data$RENDIMIENTO)], data$FECHA[data$RENDIMIENTO == min(data$RENDIMIENTO)])
ydots <- c(max(data$RENDIMIENTO), min(data$RENDIMIENTO))
colors_latex <- c("Max", "Min")
endcolor = if_else(tail(data$RENDIMIENTO,2)[1]-tail(data$RENDIMIENTO,2)[2]>0,"Up","Down")

spark <- sparkline(x = data$FECHA, y = round(data$RENDIMIENTO,2), xdots=xdots, ydots = ydots, dotcolor = colors_latex, enddotcolor = endcolor,  width = 15, rectangle = quantile(data$RENDIMIENTO, c(0.25, 0.75)))

collector <- tibble(Vcto. = year(as.Date(i)), `Últimos 30 días` = spark, `Rend.` = round(tail(data$RENDIMIENTO,1),2), Dif. = round((tail(data$RENDIMIENTO,2)[1]-tail(data$RENDIMIENTO,2)[2])*100,2))

resumen %<>% bind_rows(collector) }

resumen %<>% rowid_to_column()
assign(paste0("resumen.", r), resumen)

}

for (r in c("FS", "IPC", "IB1", "DTF")) {
  resumen <- tibble(`Nodo (días)` = as.character(0), `Últimos 30 días` = as.character(0), `Rend.` = as.numeric(0), Dif. = as.numeric(0))[0, ]
  nodos <- corporativa.resumen %>% dplyr::filter(REFERENCIA == r) %>% ungroup() %>% select(NODO) %>% distinct() %>% pull()
  
  for (i in nodos) {
    data <- corporativa.resumen %>%
      dplyr::filter(REFERENCIA == r, NODO == i) %>%
      group_by(NODO, FECHA) %>%
      summarise(RENDIMIENTO = mean(RENDIMIENTO))

    xdots <- c(data$FECHA[data$RENDIMIENTO == max(data$RENDIMIENTO)], data$FECHA[data$RENDIMIENTO == min(data$RENDIMIENTO)])
    ydots <- c(max(data$RENDIMIENTO), min(data$RENDIMIENTO))
    colors <- c("Max", "Min")
    endcolor <- if_else(tail(data$RENDIMIENTO, 2)[1] - tail(data$RENDIMIENTO, 2)[2] > 0, "Up", "Down")

    spark <- sparkline(x = data$FECHA, y = round(data$RENDIMIENTO, 2), xdots = xdots, ydots = ydots, dotcolor = colors, enddotcolor = endcolor, width = 15, rectangle = quantile(data$RENDIMIENTO, c(0.25, 0.75)))

    collector <- tibble(`Nodo (días)` = i, `Últimos 30 días` = spark, `Rend.` = round(tail(data$RENDIMIENTO, 1), 2), Dif. = round((tail(data$RENDIMIENTO, 2)[1] - tail(data$RENDIMIENTO, 2)[2]) * 100, 2))

    resumen %<>% bind_rows(collector)
  }

  assign(paste0("resumen.", r), resumen)
}

tabla.publica <- right_join(resumen.COP, resumen.UVR, by = "rowid") %>% select(-rowid) %>%
  mutate(`Dif..x` = cell_spec(`Dif..x`,"latex", color = ifelse(Dif..x > 0, "Up", "Down")),
         `Dif..y` = cell_spec(`Dif..y`,"latex", color = ifelse(Dif..y > 0, "Up", "Down"))) %>%
  kable("latex", booktabs = TRUE, escape = FALSE, linesep = "") %>% add_header_above(c("TES COP" = 4, "TES UVR" = 4)) %>% 
  str_replace_all("(\\.x)|(\\.y)", "")

tabla.fs.ipc <- right_join(resumen.FS, resumen.IPC, by = "Nodo (días)") %>%
  mutate(`Dif..x` = cell_spec(`Dif..x`,"latex", color = ifelse(Dif..x > 0, "Up", "Down")),
         `Dif..y` = cell_spec(`Dif..y`,"latex", color = ifelse(Dif..y > 0, "Up", "Down"))) %>%
  kable("latex", booktabs = TRUE, escape = FALSE, linesep = "") %>% add_header_above(c(" "=1, "\textin{TASA FIJA}" = 3, "\textin{IPC}" = 3)) %>% 
  str_replace_all("(\\.x)|(\\.y)", "")

tabla.ib1.dtf <- right_join(resumen.IB1, resumen.DTF, by = "Nodo (días)") %>%
  mutate(`Dif..x` = cell_spec(`Dif..x`,"latex", color = ifelse(Dif..x > 0, "Up", "Down")),
         `Dif..y` = cell_spec(`Dif..y`,"latex", color = ifelse(Dif..y > 0, "Up", "Down"))) %>%
  kable("latex", booktabs = TRUE, escape = FALSE, linesep = "") %>% add_header_above(c(" "=1, "\textin{IBR}" = 3, "\textin{DTF}" = 3)) %>% 
  str_replace_all("(\\.x)|(\\.y)", "")

setwd(directorio)

#### Ãndices bursÃ¡tiles

indices.img <- ggplot(indices) + geom_line(aes(x = FECHA, y = VALOR, group = INDICE, color = INDICE)) + facet_grid(cols = vars(GRUPO)) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), expand = c(0, 0), labels = number_format(accuracy = 0.1)) +
  scale_x_bd(business.dates = indices$FECHA, labels = date_format("%b %y"), max.major.breaks = 10, expand = c(0, 0)) + scale_color_manual(values = colors_idn) +
  no.x.label + no.y.label + theme(
    aspect.ratio = NULL, axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)),
    legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.justification = "top", axis.text.y = element_text(size = rel(0.6))
  )

ggsave(plot = indices.img, "Indices.png", width = 17.90, height = 4.31, units = "cm", dpi = 1000, type = "cairo-png")

#### Renta fija

## Movimiento curvas

rfija.img <- ggplot() + geom_line(data = deuda.ns, aes(x = MADURACION, y = RENDIMIENTO, group = FECHA, color = REFERENCIA, linetype = LINEA), size = 0.25) +
  geom_point(data = deuda.media[deuda.media$FECHA == today() - rezago, ], aes(x = AÑOS_AL_VENCIMIENTO, y = RENDIMIENTO, color = REFERENCIA, size = REFERENCIA), alpha = 0.4) +
  facet_wrap(. ~ REFERENCIA, scales = "free", nrow = 1, labeller = labeller(REFERENCIA = ref_names)) + theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ",", trim = TRUE), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(), labels = number_format(accuracy = 0.1, scale = 1, decimal.mark = ",", trim = FALSE), expand = c(0, 0)) + no.x.label + no.y.label +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Vigente", "Hace una semana")) + scale_color_manual(values = colors_fx, guide = "none") +
  scale_size_manual(breaks = c("DTF", "IB1", "FS", "IPC", "COP", "UVR"), values = c(0.01, 0.01, 0.01, 0.01, 0.8, 0.8)) +
  theme(
    aspect.ratio = NULL, axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.position = "bottom", axis.text.y = element_text(size = rel(0.6), margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm")), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0, "lines")
  )

# Diferencias

spread <- ggplot() + geom_col(data = spread.media, aes(x = MADURACION, y = DIFERENCIA, width = GROSOR), fill = "#E15759") +
  facet_wrap(. ~ REFERENCIA, scales = "free", nrow = 1) +
  geom_col(data = spread.ns, aes(x = MADURACION, y = DIFERENCIA), fill = "#BAB0AC", alpha = 0.6, width = 1 / 365) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 1, scale = 1, unit = "pbs", decimal.mark = ","), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 0.1, scale = 1, unit = "años", decimal.mark = ","), expand = c(0, 0)) + no.x.label + no.y.label + no.x.label +
  theme(
    aspect.ratio = NULL, legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), strip.text = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)), axis.text.y = element_text(size = rel(0.4)),
    plot.margin = unit(c(0, 0, 0.1, 0), "cm"), panel.spacing.x = unit(0.15, "lines"), panel.spacing.y = unit(0, "lines")
  ) + guides(color = guide_legend(override.aes = list(size = 3)))

# Rejilla

img <- plot_grid(rfija.img + theme(legend.position = "none"), spread, align = "v", ncol = 1, rel_heights = c(1.618, 1))
ggsave(plot = img, "fx_income.png", width = 17.90, height = 5.545, units = "cm", dpi = 1000, type = "cairo-png")

## Corporativa

corp.img <- ggplot() + geom_line(data = corp.ns, aes(x = DIAS_AL_VENCIMIENTO, y = RENDIMIENTO, group = REFERENCIA, color = REFERENCIA), size = 0.25) +
  geom_point(data = corp.media, aes(x = DIAS_AL_VENCIMIENTO, y = RENDIMIENTO, group = REFERENCIA, color = REFERENCIA), alpha = 0.2, size = 0.01) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ",", trim = TRUE), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = unit_format(accuracy = 1, scale = 1, suffix = " días", decimal.mark = ",", big.mark = "."), expand = c(0, 0)) + no.x.label + no.y.label + scale_color_manual(values = colors_fx, guide = "none") +
  theme(
    aspect.ratio = NULL, axis.text.y = element_text(size = rel(0.6), margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm")), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0, "lines"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5))
  )

ggsave(plot = corp.img, "corp.png", width = 7.89, height = 4, units = "cm", dpi = 1000, type = "cairo-png")

## Parejas

i <- "fs.cop"

com.img <- ggplot() + geom_line(data = eval(parse(text = paste(i, "ns", sep = "."))), aes(x = MADURACION, y = RENDIMIENTO, color = REFERENCIA, group = REFERENCIA), size = 0.25) +
  geom_point(data = eval(parse(text = paste(i, "media", sep = "."))), aes(x = AÑOS_AL_VENCIMIENTO, y = RENDIMIENTO, color = REFERENCIA), alpha = 0.4, size = 0.8) + theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ",", trim = TRUE), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = number_format(accuracy = 0.1, scale = 1, decimal.mark = ",", trim = FALSE), expand = c(0, 0)) + no.x.label + no.y.label +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Vigente", "Hace una semana")) + scale_color_manual(values = colors_fx, guide = "none") +
  theme(
    aspect.ratio = NULL, axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.position = "bottom", axis.text.y = element_text(size = rel(0.6), margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm")), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0, "lines")
  )

com.spread <- ggplot() + geom_col(data = eval(parse(text = paste(i, "media.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#E15759", width = 50) +
  geom_col(data = eval(parse(text = paste(i, "ns.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#BAB0AC", alpha = 0.6) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 1, scale = 1, unit = "pbs", decimal.mark = ","), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = unit_format(accuracy = 1, scale = 1, unit = "días", decimal.mark = ",", big.mark = "."), expand = c(0, 0)) + no.x.label + no.y.label + no.x.label +
  theme(
    aspect.ratio = NULL, legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), strip.text = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)), axis.text.y = element_text(size = rel(0.4)),
    plot.margin = unit(c(0, 0, 0.1, 0), "cm"), panel.spacing.x = unit(0.15, "lines"), panel.spacing.y = unit(0, "lines")
  ) + guides(color = guide_legend(override.aes = list(size = 3)))

com_img <- plot_grid(com.img, com.spread, align = "v", ncol = 1, rel_heights = c(1.618, 1))
ggsave(plot = com_img, "fs_cop.png", width = 5.695, height = 4, units = "cm", dpi = 1000, type = "cairo-png")

i <- "ipc.uvr"

com.img <- ggplot() + geom_line(data = eval(parse(text = paste(i, "ns", sep = "."))), aes(x = MADURACION, y = RENDIMIENTO, color = REFERENCIA, group = REFERENCIA), size = 0.25) +
  geom_point(data = eval(parse(text = paste(i, "media", sep = "."))), aes(x = AÑOS_AL_VENCIMIENTO, y = RENDIMIENTO, color = REFERENCIA), alpha = 0.4, size = 0.8) + theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ",", trim = TRUE), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = number_format(accuracy = 0.1, scale = 1, decimal.mark = ",", trim = FALSE), expand = c(0, 0)) + no.x.label + no.y.label +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Vigente", "Hace una semana")) + scale_color_manual(values = colors_fx, guide = "none") +
  theme(
    aspect.ratio = NULL, axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.position = "bottom", axis.text.y = element_text(size = rel(0.6), margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm")), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0, "lines")
  )

com.spread <- ggplot() + geom_col(data = eval(parse(text = paste(i, "media.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#E15759", width = 50) +
  geom_col(data = eval(parse(text = paste(i, "ns.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#BAB0AC", alpha = 0.6) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 1, scale = 1, unit = "pbs", decimal.mark = ","), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = unit_format(accuracy = 1, scale = 1, unit = "días", decimal.mark = ",", big.mark = "."), expand = c(0, 0)) + no.x.label + no.y.label + no.x.label +
  theme(
    aspect.ratio = NULL, legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), strip.text = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)), axis.text.y = element_text(size = rel(0.4)),
    plot.margin = unit(c(0, 0, 0.1, 0), "cm"), panel.spacing.x = unit(0.15, "lines"), panel.spacing.y = unit(0, "lines")
  ) + guides(color = guide_legend(override.aes = list(size = 3)))

com_img <- plot_grid(com.img, com.spread, align = "v", ncol = 1, rel_heights = c(1.618, 1))
ggsave(plot = com_img, "ipc_uvr.png", width = 5.695, height = 4, units = "cm", dpi = 1000, type = "cairo-png")

i <- "fs.ipc"

com.img <- ggplot() + geom_line(data = eval(parse(text = paste(i, "ns", sep = "."))), aes(x = MADURACION, y = RENDIMIENTO, color = REFERENCIA, group = REFERENCIA), size = 0.25) +
  geom_point(data = eval(parse(text = paste(i, "media", sep = "."))), aes(x = AÑOS_AL_VENCIMIENTO, y = RENDIMIENTO, color = REFERENCIA), alpha = 0.4, size = 0.01) + theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ",", trim = TRUE), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(), labels = number_format(accuracy = 1, scale = 1, decimal.mark = ",", trim = FALSE), expand = c(0, 0)) + no.x.label + no.y.label +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Vigente", "Hace una semana")) + scale_color_manual(values = colors_fx, guide = "none") +
  theme(
    aspect.ratio = NULL, axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.position = "bottom", axis.text.y = element_text(size = rel(0.6), margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm")), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0, "lines")
  )

com.spread <- ggplot() + geom_col(data = eval(parse(text = paste(i, "media.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#E15759", width = 20) +
  geom_col(data = eval(parse(text = paste(i, "ns.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#BAB0AC", alpha = 0.6) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 1, scale = 1, unit = "pbs", decimal.mark = ","), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 1, scale = 1, unit = "días", decimal.mark = ",", big.mark = "."), expand = c(0, 0)) + no.x.label + no.y.label + no.x.label +
  theme(
    aspect.ratio = NULL, legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), strip.text = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)), axis.text.y = element_text(size = rel(0.4)),
    plot.margin = unit(c(0, 0, 0.1, 0), "cm"), panel.spacing.x = unit(0.15, "lines"), panel.spacing.y = unit(0, "lines")
  ) + guides(color = guide_legend(override.aes = list(size = 3)))

com_img <- plot_grid(com.img, com.spread, align = "v", ncol = 1, rel_heights = c(1.618, 1))
ggsave(plot = com_img, "fs_ipc.png", width = 5.695, height = 4, units = "cm", dpi = 1000, type = "cairo-png")

i <- "cop.uvr"

com.img <- ggplot() + geom_line(data = eval(parse(text = paste(i, "ns", sep = "."))), aes(x = MADURACION, y = RENDIMIENTO, color = REFERENCIA, group = REFERENCIA), size = 0.25) +
  geom_point(data = eval(parse(text = paste(i, "media", sep = "."))), aes(x = AÑOS_AL_VENCIMIENTO, y = RENDIMIENTO, color = REFERENCIA), alpha = 0.4, size = 0.8) + theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = percent_format(accuracy = 0.01, scale = 1, suffix = "%", decimal.mark = ",", trim = TRUE), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = number_format(accuracy = 0.1, scale = 1, decimal.mark = ",", trim = FALSE), expand = c(0, 0)) + no.x.label + no.y.label +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Vigente", "Hace una semana")) + scale_color_manual(values = colors_fx, guide = "none") +
  theme(
    aspect.ratio = NULL, axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), legend.position = "bottom", axis.text.y = element_text(size = rel(0.6), margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm")), plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0, "lines")
  )

com.spread <- ggplot() + geom_col(data = eval(parse(text = paste(i, "media.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#E15759") +
  geom_col(data = eval(parse(text = paste(i, "ns.spread", sep = "."))), aes(x = DIAS_AL_VENCIMIENTO, y = DIFERENCIA), fill = "#BAB0AC", alpha = 0.6) +
  theme_diario() + theme(panel.border = element_rect(color = "#7F7F7F", fill = NA, size = 0.5)) +
  scale_y_continuous(breaks = extended_range_breaks(), labels = unit_format(accuracy = 1, scale = 1, unit = "pbs", decimal.mark = ","), expand = c(0, 0)) +
  scale_x_continuous(breaks = extended_range_breaks(0), labels = unit_format(accuracy = 1, scale = 1, unit = "días", decimal.mark = ",", big.mark = "."), expand = c(0, 0)) + no.x.label + no.y.label + no.x.label +
  theme(
    aspect.ratio = NULL, legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), strip.text = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.5)), axis.text.y = element_text(size = rel(0.4)),
    plot.margin = unit(c(0, 0, 0.1, 0), "cm"), panel.spacing.x = unit(0.15, "lines"), panel.spacing.y = unit(0, "lines")
  ) + guides(color = guide_legend(override.aes = list(size = 3)))

com_img <- plot_grid(com.img, com.spread, align = "v", ncol = 1, rel_heights = c(1.618, 1))
ggsave(plot = com_img, "cop_uvr.png", width = 5.695, height = 4, units = "cm", dpi = 1000, type = "cairo-png")
