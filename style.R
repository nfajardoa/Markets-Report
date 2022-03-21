theme_diario <- function(base_size = 10,
                         base_family = "",
                         base_line_size = base_size / 170,
                         base_rect_size = base_size / 170,
                         ticks = TRUE) {
  theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(color = rgb(25, 43, 65, maxColorValue = 255), face = "bold", hjust = 0),
      axis.title = element_text(color = rgb(20, 20, 20, maxColorValue = 255), size = rel(0.9), hjust = 1, margin = margin(t = 0, r = 20, b = 20, l = 0)),
      axis.text = element_text(color = rgb(105, 105, 105, maxColorValue = 255), size = rel(0.75)),
      axis.title.y = element_text(margin = margin(0, 8, 0, 8), angle = 90),
      axis.title.x = element_text(margin = margin(4, 0, 4, 0), angle = 0),
      panel.grid.major = element_line(rgb(200, 200, 200, maxColorValue = 255), linetype = "dotted", size = rel(3)),
      text = element_text(family = "Arial Narrow", face = "plain", color = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), debug = FALSE),
      legend.box.spacing = unit(2.5, "mm"),
      legend.text = element_text(size = rel(0.5), color = "#7F7F7F", margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
      panel.border = element_blank(),
      legend.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"),
      panel.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"),
      plot.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"),
      strip.text.x = element_text(color = "#7F7F7F", face = "bold", margin = margin(t = 0, r = 0, b = 2.5, l = 0, unit = "pt")),
      strip.text.y = element_text(color = "#7F7F7F", face = "bold"),
      strip.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      complete = TRUE
    )
}

theme_semanal <- function(base_size = 10,
                          base_family = "Arial Narrow",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170,
                          ticks = TRUE) {
  theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(color = rgb(25, 43, 65, maxColorValue = 255), face = "bold", hjust = 0),
      axis.title = element_text(color = rgb(20, 20, 20, maxColorValue = 255), size = rel(0.9), hjust = 1, margin = margin(t = 0, r = 20, b = 20, l = 0)),
      axis.text = element_text(color = rgb(105, 105, 105, maxColorValue = 255), size = rel(0.75)),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line.y = element_blank(),
      panel.grid.major = element_line(rgb(200, 200, 200, maxColorValue = 255), linetype = "dotted", size = rel(3)),
      text = element_text(family = "Arial Narrow", face = "plain", color = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), debug = FALSE),
      legend.box.spacing = unit(2.5, "mm"),
      legend.text = element_text(size = rel(0.5), color = "#7F7F7F", margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
      panel.border = element_blank(),
      strip.text.x = element_text(color = "#7F7F7F", face = "bold", margin = margin(t = 0, r = 0, b = 2.5, l = 0, unit = "pt")),
      strip.text.y = element_text(color = "#7F7F7F", face = "bold"),
      strip.background = element_rect(fill = "transparent",colour = NA),
      panel.background = element_rect(fill = "transparent",colour = NA),
      legend.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      plot.margin=unit(c(0.25,0.5,0.25,0.5),"cm"),
      legend.position = "none",
      complete = TRUE
    )
}

theme_mensual <- function(base_size = 10,
                        base_family = "",
                        base_line_size = base_size / 170,
                        base_rect_size = base_size / 170,
                        ticks = TRUE) {
  theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(color = rgb(25, 43, 65, maxColorValue = 255), face = "bold", hjust = 0),
      axis.title = element_text(color = rgb(20, 20, 20, maxColorValue = 255), size = rel(0.9), hjust = 1, margin = margin(t = 0, r = 20, b = 20, l = 0)),
      axis.text = element_text(color = rgb(105, 105, 105, maxColorValue = 255), size = rel(0.75)),
      axis.title.y = element_text(margin = margin(0, 8, 0, 8), angle = 90),
      axis.title.x = element_text(margin = margin(4, 0, 4, 0), angle = 0),
      panel.grid.major = element_line(rgb(200, 200, 200, maxColorValue = 255), linetype = "dotted", size = rel(3)),
      text = element_text(family = "Arial Narrow", face = "plain", color = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), debug = FALSE),
      legend.box.spacing = unit(2.5, "mm"),
      legend.text = element_text(size = rel(0.5), color = "#7F7F7F", margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
      panel.border = element_blank(),
      strip.text.x = element_text(color = "#7F7F7F", face = "bold", margin = margin(t = 0, r = 0, b = 2.5, l = 0, unit = "pt")),
      strip.text.y = element_text(color = "#7F7F7F", face = "bold"),
      strip.background = element_rect(fill = "transparent",colour = NA),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      panel.background = element_rect(fill = "transparent",colour = NA),
      legend.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      complete = TRUE
    )
}

theme_plot <- function() {
  theme_complete() + theme(legend.position = "none")
}
theme_spread <- function() {
  theme_plot() + theme(aspect.ratio = NULL)
}
theme_analysis <- function() {
  theme_plot() + theme(
    aspect.ratio = NULL,
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.75))
  )
}

no.x.label <- theme(axis.title.x = element_blank())
no.y.label <- theme(axis.title.y = element_blank())

colors <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14E", "#EDC949", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
colors_oil <- c(BRENT = "#F28E2B", WTI = "#7f7f7f")
colors_fx <- c(DTF = "#59A14E", IB1 = "#F28E2B", IBR = "#F28E2B", IPC = "#B07AA1", FS = "#76B7B2", `Tasa fija` = "#76B7B2", COP = "#4E79A7", UVR = "#EDC949")
colors_mm <- c(TIP = "#4E79A7", `IBR 30` = "#EDC949", `IBR 1` = "#59A14E", `IBR 90` = "#B07AA1", `IBR 180` = "#FF9DA7")
colors_idn <- c(SHANGAI = "#4E79A7", DOW = "#F28E2B", `S&P` = "#76B7B2", STOXX = "#59A14E", DAX = "#EDC949", COLCAP = "#B07AA1", TOPIX = "#FF9DA7")
colors_resumen <- c(`t-1` = "#EDC949", `t-2` = "#395879", `t-7` = "#4e79a7", `t-14` = "#7498be", `t-30` = "#a8bed6")
colors_fiduagraria <- c("#6496CD", "#93B751", "#EFCD3D", "#F06449", "#B4D2BA", "#8D6A9F", "#757761", "#F28E2B", "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14E", "#EDC949")

