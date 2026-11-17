#!/usr/bin/env Rscript

# 等间隔冲击响应（Figure 02a--02c）。
# 分别以Brent现货收益、Brent期货波动率和STOXX波动率为响应变量；
# 每张总图包含该响应变量面对其余三个变量冲击的三个横向面板。
# 每个面板的三条线是1、3、6日ahead的时变后验均值IRF。
#
# 注意：模型实际变量是Brent_Spot_Return和Brent_Futures_Vol，代码不会
# 将收益率或波动率误标为价格水平。

source(file.path(dirname(normalizePath(sub(
  "^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L]
), mustWork = TRUE)), "Figure_common.R"))

project_dir <- figure_project_dir()
model <- figure_load_model(project_dir)

response_variables <- c("Brent_Spot_Return", "Brent_Futures_Vol", "STOXX_Vol")
shock_order <- c("Delta_HDMI", "Brent_Spot_Return", "Brent_Futures_Vol", "STOXX_Vol")
required_variables <- unique(c(response_variables, shock_order))
if (length(setdiff(required_variables, model$variables))) {
  stop("模型缺少变量：", paste(setdiff(required_variables, model$variables), collapse = ","))
}

pairs <- do.call(rbind, lapply(response_variables, function(response) {
  shocks <- shock_order[shock_order != response]
  data.frame(
    Response = response,
    Shock = shocks,
    Direction = paste(response, shocks, sep = "__response_to__"),
    Panel = sprintf("(%s)", letters[seq_along(shocks)]),
    stringsAsFactors = FALSE
  )
}))
row.names(pairs) <- NULL

horizons <- figure_integer_vector("IRF_HORIZONS", c(1L, 3L, 6L))
available <- sort(unique(model$irf$horizon))
if (length(setdiff(horizons, available))) {
  stop("模型缺少horizon：", paste(setdiff(horizons, available), collapse = ","))
}

values <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(i) {
  x <- model$irf[
    model$irf$response == pairs$Response[i] &
      model$irf$shock == pairs$Shock[i] &
      model$irf$horizon %in% horizons,
    , drop = FALSE
  ]
  if (!nrow(x)) stop("缺少IRF方向：", pairs$Shock[i], " -> ", pairs$Response[i])
  x$Direction <- pairs$Direction[i]
  x$Panel <- pairs$Panel[i]
  x
}))
values <- values[order(values$response, values$shock, values$horizon, values$Date), ]

output_dir <- file.path(project_dir, "02_figures", "Figure02")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  values[c("Panel", "Direction", "Date", "time", "horizon", "response", "shock", "value")],
  file.path(output_dir, "Figure02_equal_interval_IRF_values.csv"),
  row.names = FALSE
)
write.csv(pairs, file.path(output_dir, "Figure02_pair_map.csv"), row.names = FALSE)

colours <- setNames(
  c("#009E73", "#0072B2", "#D55E00", "#CC79A7")[seq_along(horizons)],
  horizons
)
line_types <- setNames(c(3L, 2L, 1L, 4L)[seq_along(horizons)], horizons)
font_family <- "Arial"
font_pointsize <- 42

file_stems <- c(
  Brent_Spot_Return = "Figure02a_Brent_spot_return_responses",
  Brent_Futures_Vol = "Figure02b_Brent_futures_volatility_responses",
  STOXX_Vol = "Figure02c_STOXX_volatility_responses"
)

make_draw_function <- function(response) {
  response_pairs <- pairs[pairs$Response == response, , drop = FALSE]
  response_values <- values[values$response == response, , drop = FALSE]

  function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(
      mfrow = c(1L, 3L), mar = c(5.5, 7.5, 3.5, 1.1),
      oma = c(4.5, 0.8, 0.8, 0.5), mgp = c(4.5, 1.25, 0),
      tcl = -0.25, las = 1, family = font_family, ps = font_pointsize,
      cex.axis = 1, cex.lab = 1, cex.main = 1
    )

    for (i in seq_len(nrow(response_pairs))) {
      direction <- response_pairs$Direction[i]
      panel <- response_values[response_values$Direction == direction, , drop = FALSE]
      title <- paste0(response_pairs$Panel[i], " ",
                      figure_label(response_pairs$Shock[i]), " shock")
      y_limits <- figure_range(panel$value)
      # 上方预留45%的纵向空间，图例不会压住IRF曲线。
      y_limits[2L] <- y_limits[2L] + 0.45 * diff(y_limits)

      graphics::plot(
        range(panel$Date), y_limits, type = "n",
        xlab = "Date", ylab = "Impulse response", main = title, bty = "l"
      )
      graphics::abline(h = 0, col = "grey60", lwd = 1.2)
      for (h in horizons) {
        line <- panel[panel$horizon == h, , drop = FALSE]
        graphics::lines(
          line$Date, line$value,
          col = colours[as.character(h)],
          lty = line_types[as.character(h)], lwd = 2.6
        )
      }
      graphics::legend(
        "topright", legend = paste0(horizons, "-day ahead"),
        col = colours[as.character(horizons)],
        lty = line_types[as.character(horizons)], lwd = 2.6,
        bty = "o", bg = "white", box.col = "white", cex = 1
      )
    }

    graphics::mtext(
      paste0("Time-varying response of ", figure_label(response),
             " to alternative shocks (posterior means)."),
      side = 1, outer = TRUE, line = 1.7, font = 2, cex = 1
    )
  }
}

output_files <- do.call(rbind, lapply(response_variables, function(response) {
  files <- figure_devices(
    make_draw_function(response), project_dir, "Figure02",
    unname(file_stems[response]), 36.0, 13.5,
    font_family = font_family, pointsize = font_pointsize
  )
  data.frame(
    Response = response,
    PDF = unname(files[["PDF"]]),
    PNG = unname(files[["PNG"]]),
    stringsAsFactors = FALSE
  )
}))
write.csv(output_files, file.path(output_dir, "Figure02_output_files.csv"), row.names = FALSE)

figure_write_irf_config(
  model, file.path(output_dir, "Figure02_configuration.txt"),
  "Three response-specific figures; each contains the other three structural shocks",
  c(
    paste0("horizons=", paste(horizons, collapse = ",")),
    paste0("font_family=", font_family),
    paste0("font_pointsize=", font_pointsize),
    paste0("responses=", paste(response_variables, collapse = ",")),
    paste0("directions=", nrow(pairs)),
    paste0("pdf_files=", paste(output_files$PDF, collapse = ";")),
    paste0("png_files=", paste(output_files$PNG, collapse = ";"))
  )
)

message("Figure 02a--02c generated:\n", paste(output_files$PDF, collapse = "\n"))
