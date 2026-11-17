#!/usr/bin/env Rscript

# 对应参考论文 Fig. 1：第一行ACF、第二行MCMC样本路径、第三行后验分布。
# 四链版本不把独立链错误地首尾拼接计算ACF，而是逐链计算并叠加显示。
# 全图使用Arial；字号由font_scale控制，保持3行×6列。
# 使用英寸控制面板留白，不让大字号挤占坐标轴；图例放在独立底栏。
# ACF、样本路径和后验密度各占一整行，第一行仍保留四条链各自的ACF。

source(file.path(dirname(figure_script_path <- normalizePath(sub(
  "^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L]
), mustWork = TRUE)), "Figure_common.R"))

project_dir <- figure_project_dir()
model <- figure_load_model(project_dir)
details_file <- figure_env_path(project_dir, "TABLE3_DETAILS_FILE", "Table3_model_estimation_details.csv")
if (!file.exists(details_file)) stop("缺少Table 3参数映射：", details_file)
details <- read.csv(details_file, check.names = FALSE)
paper_names <- c("sb1", "sb2", "sa1", "sa2", "sh1", "sh2")
if (!identical(as.character(details$Parameter), paper_names)) {
  stop("Table 3参数必须依次为sb1、sb2、sa1、sa2、sh1、sh2。")
}

draws <- model$fit$sigma_draws
chain_id <- model$fit$sigma_chain_id
if (!is.matrix(draws) || is.null(colnames(draws)) || anyNA(draws)) stop("sigma_draws无效。")
if (is.null(chain_id) || length(chain_id) != nrow(draws)) stop("四链标识不存在或长度错误。")
chains <- sort(unique(as.integer(chain_id)))
if (length(chains) < 2L) stop("Figure01需要多链模型对象。")

draw_index <- match(details$Source_parameter, colnames(draws))
if (anyNA(draw_index)) stop("Table 3参数无法映射到sigma_draws。")
selected <- draws[, draw_index, drop = FALSE]
colnames(selected) <- paper_names
within_draw <- ave(seq_along(chain_id), chain_id, FUN = seq_along)

acf_lag_max <- figure_positive_integer("FIG1_ACF_LAG_MAX", 500L)
trace_points <- figure_positive_integer("FIG1_TRACE_POINTS_PER_CHAIN", 2500L)
density_points <- figure_positive_integer("FIG1_DENSITY_POINTS", 512L)
histogram_breaks <- figure_positive_integer("FIG1_HISTOGRAM_BREAKS", 30L)
output_dir <- file.path(project_dir, "02_figures", "Figure01")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

draw_table <- data.frame(
  Chain = as.integer(chain_id), Draw = as.integer(within_draw), selected,
  check.names = FALSE
)
write.csv(draw_table, file.path(output_dir, "Figure01_selected_parameter_draws.csv"), row.names = FALSE)
write.csv(data.frame(Display_parameter = paper_names,
                     Source_parameter = details$Source_parameter),
          file.path(output_dir, "Figure01_parameter_map.csv"), row.names = FALSE)

acf_rows <- list(); density_rows <- list(); position <- 1L
for (parameter in paper_names) {
  for (chain in chains) {
    x <- selected[chain_id == chain, parameter]
    lag_max <- min(acf_lag_max, length(x) - 1L)
    ac <- stats::acf(x, lag.max = lag_max, plot = FALSE, demean = TRUE)
    acf_rows[[position]] <- data.frame(
      Parameter = parameter, Chain = chain,
      Lag = as.integer(ac$lag), ACF = as.numeric(ac$acf)
    )
    de <- stats::density(x, n = density_points)
    density_rows[[position]] <- data.frame(
      Parameter = parameter, Chain = chain,
      Value = de$x, Density = de$y
    )
    position <- position + 1L
  }
}
acf_data <- do.call(rbind, acf_rows)
density_data <- do.call(rbind, density_rows)
acf_median_data <- stats::aggregate(ACF ~ Parameter + Lag, acf_data, stats::median)
acf_median_data <- acf_median_data[order(
  match(acf_median_data$Parameter, paper_names), acf_median_data$Lag
), ]
write.csv(acf_data, file.path(output_dir, "Figure01_autocorrelation_by_chain.csv"), row.names = FALSE)
write.csv(acf_median_data,
          file.path(output_dir, "Figure01_autocorrelation_four_chain_median.csv"),
          row.names = FALSE)
write.csv(density_data, file.path(output_dir, "Figure01_density_by_chain.csv"), row.names = FALSE)
write.csv(details, file.path(output_dir, "Figure01_diagnostics_used.csv"), row.names = FALSE)

# 不使用plotmath表达式，避免参数标题切换到数学/符号字体。
# 这些纯文本标题会与坐标轴一样严格使用Arial。
panel_titles <- paper_names
chain_colours <- grDevices::adjustcolor(
  c("#0072B2", "#D55E00", "#009E73", "#CC79A7")[seq_along(chains)],
  alpha.f = 0.58
)
font_family <- "Arial"
font_scale <- 2.5
font_pointsize <- 84 * font_scale
# 旧版mfrow=c(3,6)会自动把文字缩到0.66倍。这里显式保留该系数，
# 实际字号为55.44 * font_scale pt，避免叠加隐式缩放。
text_cex <- 0.66
figure_width <- 90
figure_height <- 84
legend_height <- 6
# 图例只改变颜色示意条，不改变面板中四条链的线宽或后验结果。
legend_segment_inches <- 4.5    # 每条颜色示意条的水平长度，单位为英寸
legend_line_width <- 24        # 四条链的示意条粗细，R的lwd相对单位（原来为2）
legend_pooled_line_width <- 28  # 合并后验密度的黑色示意条粗细（原来为3）
legend_text_cex <- 0.72        # 图例文字相对面板文字的字号比例，保持原值
legend_text_gap_inches <- 0.7 # 颜色示意条末端与文字起点之间的距离
legend_item_gap_inches <- 2.0 # 相邻图例项之间的距离
legend_outer_padding_inches <- 1.5 # 图例左右最少留白
legend_alpha <- 1             # 图例颜色不透明度：1为完全不透明，0为不可见
# 顺序：下、左、上、右。单位为英寸，不随设备默认字号发生变化。
# 比原版缩小每个面板的绘图区，给标题、刻度、倍率说明留出独立空间。
panel_margins <- c(6.1, 4.8, 5.5, 1.0)

# 只缩写坐标显示，不改变抽样值、密度或导出的CSV。
# 例如0.002显示为2，并在该坐标旁明确标注×10⁻³。
axis_spec <- function(limits, n = 3L, integer = FALSE) {
  ticks <- pretty(limits, n = n)
  ticks <- ticks[ticks >= limits[1L] & ticks <= limits[2L]]
  if (!length(ticks)) ticks <- mean(limits)
  magnitude <- max(abs(ticks))
  exponent <- if (magnitude > 0 && (magnitude < 0.1 || magnitude >= 1000)) {
    floor(log10(magnitude))
  } else 0L
  # 迭代次数统一用千次；避免12500、25000等长数字互相遮盖。
  if (integer && magnitude >= 10000) exponent <- 3L
  labels <- format(round(ticks / 10^exponent, 3), trim = TRUE,
                   scientific = FALSE, drop0trailing = TRUE)
  # 指数使用普通Arial字符手动抬高；避免Unicode上标负号回退到其他字体。
  list(at = ticks, labels = labels,
       unit = if (exponent == 0) "" else as.character(exponent))
}

draw_figure <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(
    oma = rep(0, 4), family = font_family, ps = font_pointsize,
    cex = text_cex, cex.axis = 1, cex.lab = 1, cex.main = 1,
    las = 1, xaxs = "i", yaxs = "i"
  )
  panel_number <- 0L
  start_panel <- function(row, column, xlim, ylim) {
    cell_height <- (figure_height - legend_height - 0.5) / 3
    cell_width <- figure_width / 6
    top <- figure_height - 0.5 - (row - 1L) * cell_height
    graphics::par(
      fig = c((column - 1L) * cell_width / figure_width,
              column * cell_width / figure_width,
              (top - cell_height) / figure_height, top / figure_height),
      mai = panel_margins, new = panel_number > 0L
    )
    panel_number <<- panel_number + 1L
    graphics::plot.new()
    graphics::plot.window(xlim = xlim, ylim = ylim)
  }
  finish_panel <- function(title, x_ticks, y_ticks) {
    usr <- graphics::par("usr")
    pin <- graphics::par("pin")
    x_inch <- diff(usr[1:2]) / pin[1L]
    y_inch <- diff(usr[3:4]) / pin[2L]
    # 刻度线与文字分开画：字距用英寸指定，不依赖默认12 pt设备行高。
    graphics::axis(1, at = x_ticks$at, labels = FALSE, tck = -0.012, lwd = 2)
    graphics::axis(2, at = y_ticks$at, labels = FALSE, tck = -0.012, lwd = 2)
    graphics::box(bty = "l", lwd = 2)
    # 用设备上的实际字宽检查横轴标签间距，必要时减少中间刻度。
    keep <- seq_along(x_ticks$at)
    repeat {
      widths <- graphics::strwidth(x_ticks$labels[keep], units = "inches")
      centers <- (x_ticks$at[keep] - usr[1L]) / x_inch
      left <- pmin(pmax(centers - widths / 2, 0), pin[1L] - widths)
      right <- left + widths
      last <- length(keep)
      if (last < 2L || all(left[-1L] - right[-last] >= 0.3)) break
      keep <- keep[-ceiling(last / 2)]
    }
    for (k in seq_along(keep)) {
      j <- keep[k]
      graphics::text(usr[1L] + left[k] * x_inch, usr[3L] - 1.5 * y_inch,
        labels = x_ticks$labels[j], xpd = NA,
        adj = c(0, 0.5))
    }
    graphics::text(usr[1L] - 0.6 * x_inch, y_ticks$at,
      labels = y_ticks$labels, adj = c(1, 0.5), xpd = NA)
    graphics::text(mean(usr[1:2]), usr[4L] + 4.1 * y_inch,
      labels = title, font = 2, xpd = NA)
    draw_multiplier <- function(x, y, exponent, centered = FALSE) {
      if (!nzchar(exponent)) return(invisible(NULL))
      base_width <- graphics::strwidth("×10", units = "inches")
      total_width <- base_width + graphics::strwidth(exponent, units = "inches", cex = 0.7)
      left <- x - if (centered) total_width / 2 * x_inch else 0
      graphics::text(left, y, "×10", adj = c(0, 0.5), xpd = NA)
      graphics::text(left + base_width * x_inch, y + 0.6 * y_inch,
        exponent, adj = c(0, 0.5), cex = 0.7, xpd = NA)
    }
    draw_multiplier(usr[1L], usr[4L] + 1.1 * y_inch, y_ticks$unit)
    draw_multiplier(mean(usr[1:2]), usr[3L] - 4.5 * y_inch,
                    x_ticks$unit, centered = TRUE)
  }

  # 第一行：保留四条链各自的ACF；每条链只使用自身连续抽样计算。
  for (i in seq_along(paper_names)) {
    block <- acf_data[acf_data$Parameter == paper_names[i], ]
    main_title <- panel_titles[[i]]
    start_panel(1L, i, c(0, acf_lag_max), c(-1, 1))
    graphics::abline(h = 0, col = "grey60", lwd = 0.9)
    for (j in seq_along(chains)) {
      line <- block[block$Chain == chains[j], ]
      graphics::lines(line$Lag, line$ACF, col = chain_colours[j], lwd = 1.25)
    }
    finish_panel(main_title,
      list(at = c(0, round(acf_lag_max / 2), acf_lag_max),
           labels = as.character(c(0, round(acf_lag_max / 2), acf_lag_max)), unit = ""),
      list(at = c(-1, 0, 1), labels = c("-1", "0", "1"), unit = ""))
  }

  # 第二行：各链横轴都从1重新开始，叠加可直接观察链间位置差异。
  for (i in seq_along(paper_names)) {
    x_range <- range(selected[, i], finite = TRUE)
    n_per_chain <- max(tabulate(match(chain_id, chains)))
    main_title <- panel_titles[[i]]
    y_range <- extendrange(x_range, f = 0.04)
    start_panel(2L, i, c(0, n_per_chain), y_range)
    for (j in seq_along(chains)) {
      rows <- which(chain_id == chains[j])
      stride <- max(1L, ceiling(length(rows) / trace_points))
      shown <- rows[seq.int(1L, length(rows), by = stride)]
      graphics::lines(within_draw[shown], selected[shown, i],
                      col = chain_colours[j], lwd = 0.7)
    }
    finish_panel(main_title,
                 axis_spec(c(0, n_per_chain), integer = TRUE), axis_spec(y_range))
  }

  # 第三行：合并样本直方图、合并核密度黑线和逐链彩色密度线。
  for (i in seq_along(paper_names)) {
    x <- selected[, i]
    is_sh2 <- identical(paper_names[i], "sh2")
    main_title <- panel_titles[[i]]
    histogram <- graphics::hist(x, breaks = histogram_breaks, plot = FALSE)
    pooled_density <- stats::density(x, n = density_points)
    block_density <- density_data[density_data$Parameter == paper_names[i], ]
    y_range <- if (is_sh2) c(0, 400) else c(
      0, 1.04 * max(histogram$density, pooled_density$y, block_density$Density))
    start_panel(3L, i, range(histogram$breaks), y_range)
    graphics::rect(head(histogram$breaks, -1L), 0, tail(histogram$breaks, -1L),
                   histogram$density, col = "#9ECAE1", border = "white")
    for (j in seq_along(chains)) {
      graphics::lines(stats::density(x[chain_id == chains[j]], n = density_points),
                      col = chain_colours[j], lwd = 1.25)
    }
    graphics::lines(stats::density(x, n = density_points), col = "black", lwd = 1.8)
    finish_panel(main_title, axis_spec(range(histogram$breaks)),
      if (is_sh2) list(at = c(0, 200, 400), labels = c("0", "200", "400"), unit = "")
      else axis_spec(y_range))
  }

  # 公共图例独占底栏，完全避开18个绘图面板。
  graphics::par(fig = c(0, 1, 0, legend_height / figure_height),
                mai = rep(0, 4), new = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, figure_width), ylim = c(0, legend_height))
  legend_labels <- c(paste("Chain", chains), "Pooled density")
  # 直接保留原RGB，只替换alpha，避免反复adjustcolor后仍保留旧透明度。
  rgb_channels <- grDevices::col2rgb(c(chain_colours, "black"))
  legend_colours <- grDevices::rgb(rgb_channels[1L, ], rgb_channels[2L, ],
                                   rgb_channels[3L, ], alpha = 255 * legend_alpha,
                                   maxColorValue = 255)
  legend_widths <- c(rep(legend_line_width, length(chains)), legend_pooled_line_width)
  text_widths <- graphics::strwidth(legend_labels, units = "inches", cex = legend_text_cex)
  item_widths <- legend_segment_inches + legend_text_gap_inches + text_widths
  total_width <- sum(item_widths) + (length(item_widths) - 1L) * legend_item_gap_inches
  if (total_width > figure_width - 2 * legend_outer_padding_inches) {
    stop("图例过宽：请减少legend_segment_inches、legend_item_gap_inches或legend_text_cex，",
         "或者增加figure_width。为避免重叠，本次停止绘图。")
  }
  # 按设备实际测量字宽逐项放置，不依赖默认12 pt设备的字符宽度。
  left <- (figure_width - total_width) / 2
  center_y <- legend_height / 2
  text_height <- max(graphics::strheight(legend_labels, units = "inches", cex = legend_text_cex))
  if (text_height >= legend_height - 1) stop("图例过高：请增加legend_height或减小legend_text_cex。")
  for (i in seq_along(legend_labels)) {
    graphics::segments(left, center_y, left + legend_segment_inches, center_y,
                       col = legend_colours[i], lwd = legend_widths[i], lend = "butt")
    graphics::text(left + legend_segment_inches + legend_text_gap_inches, center_y,
                   legend_labels[i], adj = c(0, 0.5), cex = legend_text_cex)
    left <- left + item_widths[i] + legend_item_gap_inches
  }
}

if (!nzchar(Sys.getenv("FIGURE_PNG_DPI", unset = ""))) {
  Sys.setenv(FIGURE_PNG_DPI = "120")
}
files <- figure_devices(draw_figure, project_dir, "Figure01",
                        "Figure01_MCMC_diagnostics", figure_width, figure_height,
                        font_family = font_family, pointsize = font_pointsize)
writeLines(c(
  "Figure 1: four-chain MCMC diagnostics",
  paste0("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("fit_file=", model$fit_file),
  paste0("fit_md5=", unname(tools::md5sum(model$fit_file))),
  paste0("chains=", paste(chains, collapse = ",")),
  paste0("draws_per_chain=", paste(as.integer(table(chain_id)), collapse = ",")),
  paste0("acf_lag_max=", acf_lag_max),
  paste0("parameters=", paste(paper_names, collapse = ",")),
  "row_1=four separately computed within-chain ACFs",
  "row_2=overlaid within-chain sample paths",
  "row_3=pooled histogram and pooled/within-chain kernel densities",
  "layout=3 rows x 6 columns; manually sized panels with separate bottom legend",
  paste0("font_scale_relative_to_previous_figure=", font_scale),
  paste0("effective_text_pointsize=", font_pointsize * text_cex),
  paste0("figure_size_inches=", figure_width, ",", figure_height),
  paste0("panel_margins_inches_bottom_left_top_right=", paste(panel_margins, collapse = ",")),
  paste0("legend_segment_inches=", legend_segment_inches),
  paste0("legend_line_width=", legend_line_width),
  paste0("legend_pooled_line_width=", legend_pooled_line_width),
  paste0("legend_text_cex=", legend_text_cex),
  paste0("legend_text_gap_inches=", legend_text_gap_inches),
  paste0("legend_item_gap_inches=", legend_item_gap_inches),
  paste0("legend_alpha=", legend_alpha),
  "axis_multipliers=display only; posterior draws and exported CSV values are unchanged",
  "embedded_caption=omitted to prevent overlap; add the caption in the manuscript",
  "sh2_posterior_density_y_axis=0,200,400",
  paste0("font_family=", font_family),
  paste0("font_pointsize=", font_pointsize),
  paste0("pdf=", files[["PDF"]]), paste0("png=", files[["PNG"]])
), file.path(output_dir, "Figure01_configuration.txt"), useBytes = TRUE)
message("Figure 1 generated: ", files[["PDF"]])
