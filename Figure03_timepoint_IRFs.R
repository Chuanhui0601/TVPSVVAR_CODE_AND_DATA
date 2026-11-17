#!/usr/bin/env Rscript

# 三个霍尔木兹海峡事件点冲击响应：展示HDMI冲击对三个市场变量的
# 0--12日响应路径。默认事件对应的模型交易日为：
# 2025-06-23（关闭海峡威胁）、2026-03-02（商船遇袭与通行中断）、
# 2026-08-14（两艘油轮遭袭）。仍可用IRF_DATES显式覆盖。

source(file.path(dirname(normalizePath(sub(
  "^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L]
), mustWork = TRUE)), "Figure_common.R"))

project_dir <- figure_project_dir()
model <- figure_load_model(project_dir)
shock_variable <- "Delta_HDMI"
response_variables <- c("Brent_Spot_Return", "Brent_Futures_Vol", "STOXX_Vol")
required_variables <- c(shock_variable, response_variables)
if (length(setdiff(required_variables, model$variables))) {
  stop("模型缺少变量：", paste(setdiff(required_variables, model$variables), collapse = ","))
}
pairs <- data.frame(
  Response = response_variables,
  Shock = rep(shock_variable, length(response_variables)),
  Panel = sprintf("(%s)", letters[seq_along(response_variables)]),
  stringsAsFactors = FALSE
)
valid_dates <- sort(unique(model$irf$Date))

default_event_dates <- "2025-06-23,2026-03-02,2026-08-14"
date_text <- trimws(Sys.getenv("IRF_DATES", unset = default_event_dates))
if (nzchar(date_text)) {
  requested <- as.Date(trimws(strsplit(date_text, ",", fixed = TRUE)[[1L]]))
  if (length(requested) != 3L || anyNA(requested)) {
    stop("IRF_DATES必须包含三个逗号分隔的YYYY-MM-DD日期。")
  }
  selected_dates <- as.Date(vapply(requested, function(x) {
    as.character(valid_dates[which.min(abs(valid_dates - x))])
  }, character(1L)))
} else stop("IRF_DATES不能为空。")
if (anyDuplicated(selected_dates)) stop("三个时间点映射后发生重复，请重新指定IRF_DATES。")
date_labels <- format(selected_dates, "%Y-%m-%d")

values <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(i) {
  x <- model$irf[
    model$irf$response == pairs$Response[i] &
      model$irf$shock == pairs$Shock[i] &
      model$irf$Date %in% selected_dates,
    , drop = FALSE
  ]
  x$Panel <- pairs$Panel[i]
  x
}))
values <- values[order(values$Panel, values$Date, values$horizon), ]
output_dir <- file.path(project_dir, "02_figures", "Figure03")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(values[c("Panel", "Date", "time", "horizon", "response", "shock", "value")],
          file.path(output_dir, "Figure03_timepoint_IRF_values.csv"), row.names = FALSE)
write.csv(data.frame(
  Label = c("Closure threat", "Shipping disruption", "Tanker attacks"),
  Date = selected_dates
),
          file.path(output_dir, "Figure03_selected_dates.csv"), row.names = FALSE)

event_file <- file.path(project_dir, "experiments", "01_baseline_timepoints", "Hormuz_event_dates.csv")
if (file.exists(event_file)) {
  events <- read.csv(event_file, check.names = FALSE)
  write.csv(events, file.path(output_dir, "Figure03_selected_events.csv"), row.names = FALSE)
}
write.csv(pairs, file.path(output_dir, "Figure03_pair_map.csv"), row.names = FALSE)

colours <- c("#009E73", "#0072B2", "#D55E00")
line_types <- c(3L, 2L, 1L)
font_family <- "Arial"
font_pointsize <- 42

draw_figure <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(
    mfrow = c(1L, 3L), mar = c(5.5, 7.5, 3.5, 1.1),
    oma = c(4.5, 0.8, 0.8, 0.5), mgp = c(4.5, 1.25, 0),
    tcl = -0.25, las = 1, family = font_family, ps = font_pointsize,
    cex.axis = 1, cex.lab = 1, cex.main = 1
  )
  for (i in seq_len(nrow(pairs))) {
    panel <- values[values$Panel == pairs$Panel[i], ]
    title <- paste0(pairs$Panel[i], " ", figure_label(pairs$Response[i]))
    y_limits <- figure_range(panel$value)
    # 在上方预留图例区，避免三个日期的图例覆盖响应路径。
    y_limits[2L] <- y_limits[2L] + 0.45 * diff(y_limits)
    graphics::plot(range(panel$horizon), y_limits, type = "n",
                   xlab = "Days ahead", ylab = "Response", main = title,
                   bty = "l", xaxt = "n")
    horizon_ticks <- pretty(range(panel$horizon), n = 5L)
    horizon_ticks <- horizon_ticks[horizon_ticks >= min(panel$horizon) &
                                     horizon_ticks <= max(panel$horizon)]
    graphics::axis(1, at = horizon_ticks)
    graphics::abline(h = 0, col = "grey60", lwd = 0.8)
    for (j in seq_along(selected_dates)) {
      line <- panel[panel$Date == selected_dates[j], ]
      graphics::lines(line$horizon, line$value,
                      col = colours[j], lty = line_types[j], lwd = 2.6)
    }
    graphics::legend("topright", legend = date_labels, col = colours,
                     lty = line_types, lwd = 2.6,
                     bty = "o", bg = "white", box.col = "white", cex = 1)
  }
  graphics::mtext(
    "Responses to a Delta HDMI shock at three Strait of Hormuz event dates (posterior means).",
    side = 1, outer = TRUE, line = 1.45, font = 2, cex = 1
  )
}

files <- figure_devices(draw_figure, project_dir, "Figure03",
                        "Figure03_HDMI_shock_three_timepoint_IRFs", 36.0, 13.5,
                        font_family = font_family, pointsize = font_pointsize)
figure_write_irf_config(
  model, file.path(output_dir, "Figure03_configuration.txt"),
  "Delta HDMI shock: Brent spot return, Brent futures volatility, and STOXX volatility at three Strait of Hormuz event dates",
  c(paste0("selected_dates=", paste(selected_dates, collapse = ",")),
    paste0("font_family=", font_family),
    paste0("font_pointsize=", font_pointsize),
    paste0("shock=", shock_variable),
    paste0("responses=", paste(response_variables, collapse = ",")),
    paste0("pdf=", files[["PDF"]]), paste0("png=", files[["PNG"]]))
)
message("Three-time-point IRFs generated: ", files[["PDF"]])
