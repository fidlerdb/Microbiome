#### ---- PLOT MICROBIOME ---- ####

library(ggplot2)

plot_microbiome <- function(game, ncol = 5, microbe_radius = 0.35) {
  
  tiles <- game$tiles
  R <- 1  # hex circumradius; with this layout, neighbours share edges exactly
  
  # ---------------------------------------------------------------------------
  # Tile centres: pointy-top hexes, odd rows shifted half a hex (Catan-style).
  # Row 0 is at the top so tile 1 is top-left.
  # ---------------------------------------------------------------------------
  tile_coords <- data.frame(ID = tiles$ID, SOM = tiles$SOM, stress = tiles$stress)
  idx <- seq_len(nrow(tile_coords)) - 1
  tile_coords$col <- idx %% ncol
  tile_coords$row <- idx %/% ncol
  tile_coords$x <- sqrt(3) * R * (tile_coords$col + 0.5 * (tile_coords$row %% 2))
  tile_coords$y <- -1.5 * R * tile_coords$row
  
  # ---------------------------------------------------------------------------
  # Colour ramps: fill intensity scales with value across all tiles
  # ---------------------------------------------------------------------------
  ramp_col <- function(v, lo, hi, rng) {
    t <- if (diff(rng) == 0) rep(1, length(v)) else (v - rng[1]) / diff(rng)
    rgb(colorRamp(c(lo, hi))(t), maxColorValue = 255)
  }
  
  som_rng    <- range(tile_coords$SOM)
  stress_rng <- range(tile_coords$stress)
  
  tile_coords$som_fill    <- ramp_col(tile_coords$SOM,    "#F3E5D8", "#C9966B", som_rng)
  tile_coords$stress_fill <- ramp_col(tile_coords$stress, "#FBE3E3", "#F08A8A", stress_rng)
  
  # ---------------------------------------------------------------------------
  # Polygons: full outline plus left (SOM) and right (stress) halves.
  # Pointy-top vertices sit at 30, 90, 150, 210, 270, 330 degrees; the vertical
  # split runs through the top (90) and bottom (270) vertices.
  # ---------------------------------------------------------------------------
  make_poly <- function(i, angles, half) {
    data.frame(
      ID    = tile_coords$ID[i],
      half  = half,
      fill  = if (half == "SOM") tile_coords$som_fill[i] else
        if (half == "stress") tile_coords$stress_fill[i] else NA_character_,
      x     = tile_coords$x[i] + R * cos(angles),
      y     = tile_coords$y[i] + R * sin(angles)
    )
  }
  
  full_angles  <- pi / 6 + (0:5) * pi / 3
  left_angles  <- pi / 2 + (0:3) * pi / 3   # 90, 150, 210, 270
  right_angles <- -pi / 2 + (0:3) * pi / 3  # 270, 330, 30, 90
  
  n <- nrow(tile_coords)
  half_polys <- do.call(rbind, lapply(seq_len(n), function(i) {
    rbind(make_poly(i, left_angles, "SOM"),
          make_poly(i, right_angles, "stress"))
  }))
  half_polys$group <- paste(half_polys$ID, half_polys$half)
  
  outlines <- do.call(rbind, lapply(seq_len(n), function(i) {
    make_poly(i, full_angles, "outline")
  }))
  
  # ---------------------------------------------------------------------------
  # Labels
  # ---------------------------------------------------------------------------
  labels_df <- tile_coords
  labels_df$id_x     <- labels_df$x
  labels_df$id_y     <- labels_df$y + 0.62 * R
  labels_df$som_x    <- labels_df$x - 0.38 * R
  labels_df$stress_x <- labels_df$x + 0.38 * R
  labels_df$stat_y   <- labels_df$y - 0.58 * R
  
  # ---------------------------------------------------------------------------
  # Microbial populations, arranged in a ring inside each tile
  # ---------------------------------------------------------------------------
  populations <- game$populations[game$populations$n > 0, , drop = FALSE]
  populations <- merge(populations, tile_coords[, c("ID", "x", "y")],
                       by.x = "tile", by.y = "ID", all.x = TRUE)
  
  if (nrow(populations) > 0) {
    populations$position <- ave(populations$tile, populations$tile, FUN = seq_along)
    populations$n_pop    <- ave(populations$tile, populations$tile, FUN = length)
    
    populations$angle <- 2 * pi * (populations$position - 1) / populations$n_pop
    radius <- ifelse(populations$n_pop == 1, 0, microbe_radius * R)
    
    populations$microbe_x <- populations$x + radius * cos(populations$angle)
    populations$microbe_y <- populations$y + radius * sin(populations$angle)
    populations$player    <- factor(populations$player)
  }
  
  # ---------------------------------------------------------------------------
  # Plot
  # ---------------------------------------------------------------------------
  p <- ggplot() +
    geom_polygon(data = half_polys,
                 aes(x, y, group = group, fill = fill),
                 colour = NA) +
    scale_fill_identity() +
    geom_polygon(data = outlines,
                 aes(x, y, group = ID),
                 fill = NA, colour = "grey20", linewidth = 0.9) +
    # tile ID token
    geom_point(data = labels_df, aes(id_x, id_y),
               shape = 21, size = 8, fill = "white", colour = "grey20") +
    geom_text(data = labels_df, aes(id_x, id_y, label = ID),
              size = 3.5, fontface = "bold", colour = "grey10") +
    # SOM (brown) and stress (red) values
    geom_text(data = labels_df, aes(som_x, stat_y, label = SOM),
              colour = "brown4", size = 5, fontface = "bold") +
    geom_text(data = labels_df, aes(stress_x, stat_y, label = stress),
              colour = "red3", size = 5, fontface = "bold")
  
  if (nrow(populations) > 0) {
    p <- p +
      geom_point(data = populations,
                 aes(microbe_x, microbe_y, colour = player,
                     shape = guild, size = n)) +
      scale_colour_brewer(palette = "Set1", name = "Player") +
      scale_shape_manual(
        values = c(Opportunist = 16, Competitor = 17, Oligotroph = 15),
        name = "Guild"
      ) +
      scale_size_continuous(range = c(2, 7), name = "Abundance")
  }
  
  p +
    coord_equal() +
    theme_void() +
    labs(caption = "Left half: SOM (brown)   |   Right half: stress (red)")
}


#### ---- PLOT RESOURCES ---- ####

plot_resources <- function(game, player = NULL) {
  
  x <- game$resource_pools
  
  # Optional player filter
  if (!is.null(player)) {
    x <- x[x$player == player, , drop = FALSE]
  }
  
  ggplot(
    x,
    aes(
      x = factor(guild),
      y = SOM,
      fill = guild,
      colour = player # guild
      , shape = guild
    )
  ) +
    
    # geom_col(
    #   position = position_dodge(0.9)
    # ) +
    geom_segment(
      aes(x = factor(guild), xend = factor(guild), y = 0, yend = SOM)
      ) +
    
    #geom_vline(
    #  position = position_dodge(0.9)
    #) +
    geom_point(
      position = position_dodge(0.9)
    ) +
    
    facet_wrap(
      ~ player,# guild,
      #scales = "free_y"
    ) +
    
    labs(
      x = "Guild",
      y = "SOM",
      fill = "Guild"
    ) +
    
    theme_classic() +
    theme(legend.position = 'none') +
    expand_limits(y = 0)
}