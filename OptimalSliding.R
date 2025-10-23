# =============================================
# OptimalSliding.R — self-contained R port of OptimalSliding.pm
# Public API:
#   OS <- OptimalSliding(max_iterations=50, distance_penalty=0.15,
#                        reverse_flow_penalty=0.3, verbose=1)
#   res <- OS$optimize_sliding(weekly_data, age_groups)  # list(slides, weekly_data)
#   v   <- OS$validate_sliding(res$weekly_data, age_groups, res$slides)
# =============================================

`%||%` <- function(x, y) if (is.null(x)) y else x

OptimalSliding <- function(max_iterations = 20,
                           convergence_threshold = 0.01,   # kept for parity
                           distance_penalty = 0.2,
                           reverse_flow_penalty = 0.4,
                           verbose = 1) {
  cfg <- list(
    max_iterations = max_iterations,
    convergence_threshold = convergence_threshold,
    distance_penalty = distance_penalty,
    reverse_flow_penalty = reverse_flow_penalty,
    verbose = verbose
  )

  # ---------- internals (closed over cfg) ----------
  .build_state <- function(weekly_data, age_groups) {
    for (ag in age_groups) {
      if (is.null(weekly_data[[ag]]$mzcr_adjusted)) {
        weekly_data[[ag]]$mzcr_adjusted <- as.numeric(weekly_data[[ag]]$mzcr_original %||% 0)
      }
      diff <- (weekly_data[[ag]]$eurostat %||% 0) - (weekly_data[[ag]]$mzcr_adjusted %||% 0)
      weekly_data[[ag]]$deficit <- if (diff > 0)  diff else 0
      weekly_data[[ag]]$surplus <- if (diff < 0) -diff else 0
    }
    total_deficit <- sum(vapply(age_groups, function(ag) weekly_data[[ag]]$deficit %||% 0, numeric(1)))
    total_surplus <- sum(vapply(age_groups, function(ag) weekly_data[[ag]]$surplus %||% 0, numeric(1)))
    list(weekly_data = weekly_data,
         age_groups = age_groups,
         total_deficit = as.numeric(total_deficit),
         total_surplus = as.numeric(total_surplus))
  }

  .needs_sliding <- function(state) {
    if ((state$total_deficit %||% 0) < 10) return(FALSE)
    if ((state$total_surplus %||% 0) < 10) return(FALSE)
    total_deaths <- sum(vapply(state$age_groups, function(ag) state$weekly_data[[ag]]$eurostat %||% 0, numeric(1)))
    if (total_deaths <= 0) total_deaths <- 1
    (state$total_deficit / total_deaths) > 0.02
  }

  .edge_cost <- function(i, j) {
    # i,j are positions in age_groups (1 = oldest bin for your ordering)
    dist <- abs(i - j)
    cst <- 1 + dist * cfg$distance_penalty
    if (i < j) cst <- cst * (1 + cfg$reverse_flow_penalty)  # older -> younger penalty
    cst
  }

  .build_flow_network <- function(state, age_groups) {
    edges <- list()
    for (i in seq_along(age_groups)) {
      from <- age_groups[[i]]
      s <- state$weekly_data[[from]]$surplus %||% 0
      if (s <= 0) next
      for (j in seq_along(age_groups)) {
        if (i == j) next
        to <- age_groups[[j]]
        d <- state$weekly_data[[to]]$deficit %||% 0
        if (d <= 0) next
        cap <- min(s, d)
        if (cap > 0) {
          edges[[length(edges) + 1L]] <- list(
            from = from, to = to, i = i, j = j,
            capacity = cap, cost = .edge_cost(i, j)
          )
        }
      }
    }
    list(edges = edges)
  }

  .find_shortest_path <- function(network, state, age_groups) {
    big <- 999999
    dist <- setNames(rep(big, length(age_groups)), age_groups)
    prev <- new.env(parent = emptyenv())
    visited <- setNames(rep(FALSE, length(age_groups)), age_groups)

    for (ag in age_groups) if ((state$weekly_data[[ag]]$surplus %||% 0) > 0) dist[[ag]] <- 0

    repeat {
      u <- NULL; min_d <- big
      for (node in age_groups) if (!visited[[node]] && dist[[node]] < min_d) { u <- node; min_d <- dist[[node]] }
      if (is.null(u) || min_d >= big) break
      visited[[u]] <- TRUE

      for (e in network$edges) {
        if (!identical(e$from, u)) next
        v <- e$to
        if (!((state$weekly_data[[u]]$surplus %||% 0) > 0)) next
        if (!((state$weekly_data[[v]]$deficit %||% 0) > 0)) next
        alt <- dist[[u]] + e$cost
        if (alt < dist[[v]]) { dist[[v]] <- alt; prev[[v]] <- e }
      }
    }

    best <- NULL; best_d <- big
    for (ag in age_groups) if ((state$weekly_data[[ag]]$deficit %||% 0) > 0 && dist[[ag]] < best_d) { best <- ag; best_d <- dist[[ag]] }
    if (is.null(best) || best_d >= big) return(NULL)

    path <- list(); cur <- best
    while (!is.null(prev[[cur]])) { e <- prev[[cur]]; path <- c(list(e), path); cur <- e$from }
    path
  }

  .get_path_capacity <- function(path, state) {
    if (length(path) == 0) return(0)
    cap <- 999999
    for (e in path) {
      cap <- min(cap, (state$weekly_data[[e$from]]$surplus %||% 0))
      cap <- min(cap, (state$weekly_data[[e$to]]$deficit %||% 0))
    }
    cap
  }

  .solve_min_cost_flow <- function(network, state, age_groups) {
    flows <- new.env(parent = emptyenv())
    iter <- 0L; total_flow <- 0
    target <- min(state$total_surplus %||% 0, state$total_deficit %||% 0)
    if (cfg$verbose >= 1) cat(sprintf("  Target flow: %g deaths\n", target))

    repeat {
      if ((iter <- iter + 1L) > cfg$max_iterations) break
      if (total_flow >= target) break
      path <- .find_shortest_path(network, state, age_groups)
      if (is.null(path)) break
      amt <- .get_path_capacity(path, state)
      if (amt <= 0) break

      for (e in path) {
        if (is.null(flows[[e$from]])) flows[[e$from]] <- new.env(parent = emptyenv())
        flows[[e$from]][[e$to]] <- (flows[[e$from]][[e$to]] %||% 0) + amt
        state$weekly_data[[e$from]]$surplus <- (state$weekly_data[[e$from]]$surplus %||% 0) - amt
        state$weekly_data[[e$to]]$deficit   <- (state$weekly_data[[e$to]]$deficit   %||% 0) - amt
      }
      total_flow <- total_flow + amt
      if (cfg$verbose >= 1 && iter %% 5L == 0L) cat(sprintf("  Iteration %d: Added %g flow, total: %g\n", iter, amt, total_flow))
    }

    if (cfg$verbose >= 1) cat(sprintf("  Final flow: %g deaths in %d iterations\n", total_flow, iter))

    # env -> nested list
    out <- list()
    for (from in ls(flows, all.names = TRUE)) {
      out[[from]] <- list()
      for (to in ls(flows[[from]], all.names = TRUE)) out[[from]][[to]] <- flows[[from]][[to]]
    }
    out
  }

  .apply_flows <- function(flows, weekly_data, age_groups) {
    slides <- list(); total_slid <- 0
    for (from in names(flows)) {
      for (to in names(flows[[from]])) {
        amt <- as.numeric(flows[[from]][[to]] %||% 0)
        if (amt <= 0) next
        weekly_data[[from]]$mzcr_adjusted <- (weekly_data[[from]]$mzcr_adjusted %||% 0) - amt
        weekly_data[[to]]$mzcr_adjusted   <- (weekly_data[[to]]$mzcr_adjusted   %||% 0) + amt
        slides[[from]] <- c(slides[[from]], list(list(to = to, amount = amt)))
        total_slid <- total_slid + amt
        if (cfg$verbose >= 1) cat(sprintf("  Optimal slide: %s -> %s: %g deaths\n", from, to, amt))
      }
    }
    if (cfg$verbose >= 1) cat(sprintf("  Total slid: %g deaths\n", total_slid))
    list(slides = slides, weekly_data = weekly_data)
  }

  .validate_sliding <- function(weekly_data, age_groups, slides = NULL) {
    out <- list(total_improvement = 0, age_group_improvements = list(), warnings = character())
    for (ag in age_groups) {
      before <- abs((weekly_data[[ag]]$eurostat %||% 0) - (weekly_data[[ag]]$mzcr_original %||% 0))
      after  <- abs((weekly_data[[ag]]$eurostat %||% 0) - (weekly_data[[ag]]$mzcr_adjusted %||% 0))
      imp <- before - after
      out$age_group_improvements[[ag]] <- list(
        before_error = before, after_error = after, improvement = imp,
        improvement_pct = if (before > 0) (imp / before * 100) else 0
      )
      out$total_improvement <- out$total_improvement + imp
      if (after > before * 1.1) {
        pct <- if (before > 0) ((after - before) / before * 100) else Inf
        out$warnings <- c(out$warnings, sprintf("Age group %s got worse after sliding (error increased by %.1f%%)", ag, pct))
      }
    }
    tb <- sum(vapply(age_groups, function(ag) weekly_data[[ag]]$mzcr_original %||% 0, numeric(1)))
    ta <- sum(vapply(age_groups, function(ag) weekly_data[[ag]]$mzcr_adjusted %||% 0, numeric(1)))
    if (abs(tb - ta) > 1) out$warnings <- c(out$warnings, sprintf("Total deaths not conserved: before=%g, after=%g", tb, ta))
    out
  }

  # ---------- public ----------
  optimize_sliding <- function(weekly_data, age_groups) {
    state <- .build_state(weekly_data, age_groups)
    if (!.needs_sliding(state)) return(list(slides = list(), weekly_data = weekly_data))
    if (cfg$verbose >= 1) cat("\n=== Optimal Sliding Optimization ===\n")
    net <- .build_flow_network(state, age_groups)
    flows <- .solve_min_cost_flow(net, state, age_groups)
    .apply_flows(flows, weekly_data, age_groups)
  }

  validate_sliding <- function(weekly_data, age_groups, slides = NULL) {
    .validate_sliding(weekly_data, age_groups, slides)
  }

  list(optimize_sliding = optimize_sliding,
       validate_sliding = validate_sliding,
       config = cfg)
}
