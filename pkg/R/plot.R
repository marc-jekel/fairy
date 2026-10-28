# Plot of the polytopes of one or several models (2 or 3 probabilities at a time)

# Real edges of a convex hull (not the diagonals of its triangulated faces):
# edges between two facets that are not coplanar.
hull_edge_lines <- function(pts, faces) {
  nf <- nrow(faces)
  fn <- t(vapply(seq_len(nf), function(i) {
    a <- pts[faces[i, 1], ]; b <- pts[faces[i, 2], ]; cc <- pts[faces[i, 3], ]
    nv <- c((b[2] - a[2]) * (cc[3] - a[3]) - (b[3] - a[3]) * (cc[2] - a[2]),
            (b[3] - a[3]) * (cc[1] - a[1]) - (b[1] - a[1]) * (cc[3] - a[3]),
            (b[1] - a[1]) * (cc[2] - a[2]) - (b[2] - a[2]) * (cc[1] - a[1]))
    nv / (sqrt(sum(nv^2)) + 1e-15)
  }, numeric(3)))
  ek <- list()
  for (i in seq_len(nf)) for (e in list(c(1, 2), c(2, 3), c(1, 3))) {
    key <- paste(sort(faces[i, e]), collapse = "-")
    ek[[key]] <- c(ek[[key]], i)
  }
  xs <- ys <- zs <- numeric(0)
  for (key in names(ek)) {
    fi <- ek[[key]]
    if (!(length(fi) == 1 || abs(sum(fn[fi[1], ] * fn[fi[2], ])) < 0.9999)) next
    v <- as.integer(strsplit(key, "-")[[1]])
    xs <- c(xs, pts[v, 1], NA); ys <- c(ys, pts[v, 2], NA); zs <- c(zs, pts[v, 3], NA)
  }
  list(x = xs, y = ys, z = zs)
}

darker_col <- function(col, f = 0.6) {
  rgbv <- tryCatch(grDevices::col2rgb(col), error = function(e) matrix(c(80, 80, 80), ncol = 1))
  grDevices::rgb(t(rgbv * f), maxColorValue = 255)
}

#' Plot the polytopes of one or several models
#'
#' Draws the vertices and the convex hull of each model in the unit cube with
#' \pkg{plotly}: a 3D figure for three probabilities, a 2D figure for two.
#' For models with more probabilities, `dims` picks the three (or two)
#' probabilities that are shown (the polytope is projected onto them), as in
#' the Fairy app. Models with an equality constraint appear as faces, lines or
#' points.
#'
#' @param models An H-representation from [fairy_h()], or a (named) list of
#'   them. All models must have the same number of probabilities.
#' @param dims Integers: the one, two or three probabilities (columns) to plot.
#'   Default: all probabilities if there are up to three, otherwise the first
#'   three. One probability gives a line, two a flat picture, three a 3D scene.
#' @param colors Colours, one per model. Default: a colour palette.
#' @param opacity Opacity of the faces (default `0.35`).
#' @return A `plotly` object.
#' @examples
#' \dontrun{
#' m <- list(A = fairy_h("p1 > p2 > p3", n_p = 3), B = fairy_h("p1 > p3", n_p = 3))
#' fairy_plot(m)
#' }
#' @export
fairy_plot <- function(models, dims = NULL, colors = NULL, opacity = 0.35) {
  if (!requireNamespace("plotly", quietly = TRUE)) stop("Package 'plotly' is needed for fairy_plot().")
  if (!is.list(models) || is.matrix(models)) models <- list(models)
  if (is.null(names(models))) names(models) <- paste0("model", seq_along(models))
  vs <- lapply(models, fairy_vertices)
  n <- ncol(vs[[1]])
  if (is.null(dims)) dims <- seq_len(min(n, 3L))
  if (!length(dims) %in% 1:3 || any(dims > n)) stop("'dims' must name one, two or three of the model's probabilities.")
  if (is.null(colors)) colors <- rep(c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b", "#e377c2", "#17becf"), length.out = length(vs))
  lab <- gsub("[_{}]", "", colnames(vs[[1]])[dims])
  p <- plotly::plot_ly()
  if (length(dims) == 1) {
    for (k in seq_along(vs)) {
      nm <- names(vs)[k]; col <- colors[k]
      vv <- sort(unique(round(vs[[k]][, dims], 10)))
      if (length(vv) == 0) next
      if (length(vv) >= 2) p <- plotly::add_trace(p, x = range(vv), y = c(k, k), type = "scatter", mode = "lines",
        line = list(color = darker_col(col), width = 8), hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
      p <- plotly::add_trace(p, x = vv, y = rep(k, length(vv)), type = "scatter", mode = "markers",
        marker = list(color = darker_col(col, 0.65), size = 11, line = list(color = darker_col(col, 0.4), width = 1)),
        text = paste0(nm, " \u00b7 vertex ", seq_along(vv), "<br>", lab, " = ", format(round(vv, 3), trim = TRUE)),
        hoverinfo = "text", name = nm, legendgroup = nm)
    }
    return(plotly::layout(p, xaxis = list(title = lab, range = c(-0.02, 1.02), zeroline = FALSE),
      yaxis = list(visible = FALSE, range = c(0.4, length(vs) + 0.6))))
  }
  for (k in seq_along(vs)) {
    nm <- names(vs)[k]; col <- colors[k]
    pts <- unique(round(vs[[k]][, dims, drop = FALSE], 10))
    if (nrow(pts) == 0) next
    hov <- vapply(seq_len(nrow(pts)), function(r) paste0(nm, " · vertex ", r, "<br>",
      paste0(lab, " = ", format(round(pts[r, ], 3), trim = TRUE), collapse = "<br>")), character(1))
    if (length(dims) == 2) {
      p <- plotly::add_trace(p, x = pts[, 1], y = pts[, 2], type = "scatter", mode = "markers",
        marker = list(color = darker_col(col, 0.65), size = 8), text = hov, hoverinfo = "text",
        name = nm, legendgroup = nm)
      if (nrow(pts) >= 3 && qr(sweep(pts, 2, pts[1, ]), tol = 1e-9)$rank == 2) {
        h <- grDevices::chull(pts); h <- c(h, h[1])
        p <- plotly::add_trace(p, x = pts[h, 1], y = pts[h, 2], type = "scatter", mode = "lines",
          fill = "toself", fillcolor = grDevices::adjustcolor(col, opacity), line = list(color = darker_col(col), width = 2),
          hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
      } else if (nrow(pts) >= 2) {
        o <- order(pts[, 1], pts[, 2])
        p <- plotly::add_trace(p, x = pts[o, 1], y = pts[o, 2], type = "scatter", mode = "lines",
          line = list(color = darker_col(col), width = 3), hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
      }
      next
    }
    p <- plotly::add_trace(p, x = pts[, 1], y = pts[, 2], z = pts[, 3], type = "scatter3d", mode = "markers",
      marker = list(color = darker_col(col, 0.65), size = 5, line = list(color = darker_col(col, 0.4), width = 1)),
      text = hov, hoverinfo = "text", name = nm, legendgroup = nm)
    rk <- if (nrow(pts) > 1) qr(sweep(pts, 2, pts[1, ]), tol = 1e-9)$rank else 0L
    if (rk == 3) {
      faces <- tryCatch(geometry::convhulln(pts), error = function(e) NULL)
      if (!is.null(faces)) {
        p <- plotly::add_trace(p, type = "mesh3d", x = pts[, 1], y = pts[, 2], z = pts[, 3],
          i = faces[, 1] - 1, j = faces[, 2] - 1, k = faces[, 3] - 1, opacity = opacity, facecolor = rep(col, nrow(faces)),
          flatshading = TRUE, lighting = list(ambient = 0.85, diffuse = 0.35, specular = 0, roughness = 1, fresnel = 0),
          hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
        e <- tryCatch(hull_edge_lines(pts, faces), error = function(e) NULL)
        if (!is.null(e) && length(e$x) > 0)
          p <- plotly::add_trace(p, type = "scatter3d", mode = "lines", x = e$x, y = e$y, z = e$z,
            line = list(color = darker_col(col), width = 4), hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
      }
    } else if (rk == 2) {
      # planar model (one equality): draw the flat polygon in its own plane
      sc <- stats::prcomp(pts)$x[, 1:2, drop = FALSE]
      h <- grDevices::chull(sc)
      m <- length(h)
      p <- plotly::add_trace(p, type = "mesh3d", x = pts[h, 1], y = pts[h, 2], z = pts[h, 3],
        i = rep(0L, m - 2L), j = seq_len(m - 2L), k = seq_len(m - 2L) + 1L, opacity = opacity, facecolor = rep(col, m - 2L),
        hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
      hh <- c(h, h[1])
      p <- plotly::add_trace(p, type = "scatter3d", mode = "lines", x = pts[hh, 1], y = pts[hh, 2], z = pts[hh, 3],
        line = list(color = darker_col(col), width = 4), hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
    } else if (rk == 1) {
      o <- order(pts[, 1], pts[, 2], pts[, 3])
      p <- plotly::add_trace(p, type = "scatter3d", mode = "lines", x = pts[o, 1], y = pts[o, 2], z = pts[o, 3],
        line = list(color = darker_col(col), width = 4), hoverinfo = "skip", name = nm, legendgroup = nm, showlegend = FALSE)
    }
  }
  if (length(dims) == 2) {
    plotly::layout(p, xaxis = list(title = lab[1], range = c(-0.02, 1.02), constrain = "domain", zeroline = FALSE),
      yaxis = list(title = lab[2], range = c(-0.02, 1.02), scaleanchor = "x", scaleratio = 1, constrain = "domain", zeroline = FALSE))
  } else {
    plotly::layout(p, scene = list(xaxis = list(title = lab[1], range = c(0, 1)), yaxis = list(title = lab[2], range = c(0, 1)),
      zaxis = list(title = lab[3], range = c(0, 1)), aspectmode = "cube"))
  }
}
