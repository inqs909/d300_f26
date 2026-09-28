# Install these packages once if needed:
# install.packages(c("ggplot2", "gganimate", "gifski"))

library(ggplot2)
library(gganimate)
library(gifski)

# --------------------------------------------------
# Example tables
# --------------------------------------------------

table_a <- data.frame(
  id = c(1, 2, 3),
  name = c("Ana", "Ben", "Cara")
)

table_b <- data.frame(
  id = c(2, 3, 4),
  score = c(88, 95, 72)
)

# Perform the inner join
joined_table <- merge(table_a, table_b, by = "id")

# --------------------------------------------------
# Create labels for the animation
# --------------------------------------------------

table_a_labels <- data.frame(
  x = 1,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_a$id,
    "   Name: ",
    table_a$name
  ),
  matched = table_a$id %in% table_b$id
)

table_b_labels <- data.frame(
  x = 5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_b$id,
    "   Score: ",
    table_b$score
  ),
  matched = table_b$id %in% table_a$id
)

result_labels <- data.frame(
  x = 9,
  y = c(2.5, 1.5),
  label = paste0(
    "ID: ",
    joined_table$id,
    "   ",
    joined_table$name,
    "   Score: ",
    joined_table$score
  )
)

# --------------------------------------------------
# Build animation frames
# --------------------------------------------------

make_frame <- function(frame_number) {
  a <- table_a_labels
  b <- table_b_labels
  result <- result_labels

  a$frame <- frame_number
  b$frame <- frame_number
  result$frame <- frame_number

  # Frame 1: display both original tables
  if (frame_number == 1) {
    a$fill_group <- "Original row"
    b$fill_group <- "Original row"
    result <- result[0, ]
  }

  # Frame 2: identify matching IDs
  if (frame_number == 2) {
    a$fill_group <- ifelse(a$matched, "Matching row", "Excluded row")
    b$fill_group <- ifelse(b$matched, "Matching row", "Excluded row")
    result <- result[0, ]
  }

  # Frame 3: retain matching rows and show result
  if (frame_number == 3) {
    a$fill_group <- ifelse(a$matched, "Matching row", "Excluded row")
    b$fill_group <- ifelse(b$matched, "Matching row", "Excluded row")
    result$fill_group <- "Joined row"
  }

  list(a = a, b = b, result = result)
}

frames <- lapply(1:3, make_frame)

a_animation <- do.call(
  rbind,
  lapply(frames, function(x) x$a)
)

b_animation <- do.call(
  rbind,
  lapply(frames, function(x) x$b)
)

result_animation <- do.call(
  rbind,
  lapply(frames, function(x) x$result)
)

# Lines connecting matching IDs
join_lines <- data.frame(
  x = c(2.1, 2.1),
  xend = c(3.9, 3.9),
  y = c(2, 1),
  yend = c(3, 2)
)

join_lines <- rbind(
  transform(join_lines, frame = 2),
  transform(join_lines, frame = 3)
)

# Frame-specific titles and explanations
frame_text <- data.frame(
  frame = 1:3,
  title = c(
    "Step 1",
    "Step 2",
    "Step 3"
  ),
  explanation = c(
    "The join condition is Table A.id = Table B.id",
    "IDs 2 and 3 match; IDs 1 and 4 are excluded",
    "The INNER JOIN returns IDs 2 and 3"
  )
)

# --------------------------------------------------
# Create the plot
# --------------------------------------------------

inner_join_animation <- ggplot() +
  geom_label(
    data = a_animation,
    aes(x = x, y = y, label = label, fill = fill_group),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_label(
    data = b_animation,
    aes(x = x, y = y, label = label, fill = fill_group),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_segment(
    data = join_lines,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#E69F00",
    linewidth = 1.5,
    arrow = arrow(length = unit(0.2, "inches"))
  ) +
  geom_label(
    data = result_animation,
    aes(x = x, y = y, label = label, fill = fill_group),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_text(
    data = frame_text,
    aes(x = 5, y = 4.2, label = title),
    fontface = "bold",
    size = 10
  ) +
  # geom_text(
  #   data = frame_text,
  #   aes(x = 5, y = 4.2, label = explanation),
  #   size = 4.5
  # ) +
  annotate(
    "text",
    x = 1,
    y = 3.5,
    label = "Table A",
    fontface = "bold",
    size = 8
  ) +
  annotate(
    "text",
    x = 5,
    y = 3.5,
    label = "Table B",
    fontface = "bold",
    size = 8
  ) +
  annotate(
    "text",
    x = 9,
    y = 3.5,
    label = "INNER JOIN Result",
    fontface = "bold",
    size = 8
  ) +
  scale_fill_manual(
    values = c(
      "Original row" = "#0072B2",
      "Matching row" = "#009E73",
      "Excluded row" = "#999999",
      "Joined row" = "#CC79A7"
    ),
    breaks = c("Matching row", "Excluded row", "Joined row")
  ) +
  coord_cartesian(
    xlim = c(-0.5, 10.5),
    ylim = c(0.3, 5)
  ) +
  labs(fill = NULL) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 18),
    legend.key.size = grid::unit(3, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  transition_manual(frame) +
  enter_fade() +
  exit_fade()

# --------------------------------------------------
# Render and save the GIF
# --------------------------------------------------

animated_gif <- animate(
  inner_join_animation,
  nframes = 90,
  fps = 15,
  width = 1000,
  height = 550,
  renderer = gifski_renderer(loop = TRUE)
)

anim_save(
  filename = "lectures/img/inner_join.gif",
  animation = animated_gif
)

# Install these packages once if needed:
# install.packages(c("ggplot2", "gganimate", "gifski", "dplyr"))

library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)

# --------------------------------------------------
# Example tables
# --------------------------------------------------

table_a <- data.frame(
  id = c(1, 2, 3),
  name = c("Ana", "Ben", "Cara")
)

table_b <- data.frame(
  id = c(2, 3, 4),
  score = c(88, 95, 72)
)

# A LEFT JOIN keeps every row from Table A
joined_table <- left_join(
  table_a,
  table_b,
  by = "id"
)

# --------------------------------------------------
# Create labels
# --------------------------------------------------

table_a_labels <- data.frame(
  x = 1.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_a$id,
    "   Name: ",
    table_a$name
  ),
  matched = table_a$id %in% table_b$id
)

table_b_labels <- data.frame(
  x = 5.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_b$id,
    "   Score: ",
    table_b$score
  ),
  matched = table_b$id %in% table_a$id
)

result_labels <- data.frame(
  x = 9.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    joined_table$id,
    "   ",
    joined_table$name,
    "   Score: ",
    ifelse(is.na(joined_table$score), "NA", joined_table$score)
  )
)

# --------------------------------------------------
# Build animation frames
# --------------------------------------------------

make_frame <- function(frame_number) {
  a <- table_a_labels
  b <- table_b_labels
  result <- result_labels

  a$frame <- frame_number
  b$frame <- frame_number
  result$frame <- frame_number

  if (frame_number == 1) {
    a$fill_group <- "Original row"
    b$fill_group <- "Original row"
    result$fill_group <- "Joined row"

    # Hide the result during the first frame
    result <- result[0, ]
  }

  if (frame_number == 2) {
    # Every row in Table A will be retained
    a$fill_group <- ifelse(
      a$matched,
      "Matching row",
      "Left-only row"
    )

    # Only matching rows from Table B contribute to the result
    b$fill_group <- ifelse(
      b$matched,
      "Matching row",
      "Excluded row"
    )

    result$fill_group <- "Joined row"
    result <- result[0, ]
  }

  if (frame_number == 3) {
    a$fill_group <- ifelse(
      a$matched,
      "Matching row",
      "Left-only row"
    )

    b$fill_group <- ifelse(
      b$matched,
      "Matching row",
      "Excluded row"
    )

    result$fill_group <- ifelse(
      is.na(joined_table$score),
      "Unmatched result",
      "Joined row"
    )
  }

  list(
    a = a,
    b = b,
    result = result
  )
}

frames <- lapply(1:3, make_frame)

a_animation <- bind_rows(
  lapply(frames, function(x) x$a)
)

b_animation <- bind_rows(
  lapply(frames, function(x) x$b)
)

result_animation <- bind_rows(
  lapply(frames, function(x) x$result)
)

# --------------------------------------------------
# Matching lines between the tables
# --------------------------------------------------

join_lines <- data.frame(
  x = c(2.6, 2.6),
  xend = c(4.4, 4.4),
  y = c(2, 1),
  yend = c(3, 2)
)

join_lines <- bind_rows(
  transform(join_lines, frame = 2),
  transform(join_lines, frame = 3)
)

# --------------------------------------------------
# Frame titles
# --------------------------------------------------

frame_titles <- data.frame(
  frame = 1:3,
  x = 5.5,
  y = 4.8,
  title = c(
    "Step 1",
    "Step 2",
    "Step 3"
  )
)

# --------------------------------------------------
# Table titles
# --------------------------------------------------

table_titles <- data.frame(
  x = c(1.5, 5.5, 9.5),
  y = c(3.8, 3.8, 3.8),
  label = c(
    "Table A",
    "Table B",
    "LEFT JOIN Result"
  )
)

# --------------------------------------------------
# Create the animation
# --------------------------------------------------

left_join_animation <- ggplot() +
  geom_label(
    data = a_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_label(
    data = b_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_segment(
    data = join_lines,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    color = "#E69F00",
    linewidth = 1.5,
    arrow = arrow(
      length = grid::unit(0.2, "inches")
    )
  ) +
  geom_label(
    data = result_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +

  # Centered animation title
  geom_text(
    data = frame_titles,
    aes(
      x = x,
      y = y,
      label = title
    ),
    size = 10,
    fontface = "bold",
    hjust = 0.5
  ) +

  # Centered table titles
  geom_text(
    data = table_titles,
    aes(
      x = x,
      y = y,
      label = label
    ),
    size = 8,
    fontface = "bold",
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Original row" = "#0072B2",
      "Matching row" = "#009E73",
      "Left-only row" = "#56B4E9",
      "Excluded row" = "#999999",
      "Joined row" = "#CC79A7",
      "Unmatched result" = "#D55E00"
    ),
    breaks = c(
      "Matching row",
      "Left-only row",
      "Excluded row",
      "Joined row",
      "Unmatched result"
    )
  ) +
  coord_cartesian(
    xlim = c(-0.5, 11.5),
    ylim = c(0.3, 5.3),
    clip = "off"
  ) +
  labs(fill = NULL) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.key.size = grid::unit(3, "cm"),
    plot.margin = margin(30, 20, 20, 20)
  ) +
  transition_manual(frame) +
  enter_fade() +
  exit_fade()

# --------------------------------------------------
# Render and save the GIF
# --------------------------------------------------

animated_gif <- animate(
  left_join_animation,
  nframes = 90,
  fps = 15,
  width = 1200,
  height = 600,
  renderer = gifski_renderer(loop = TRUE)
)

anim_save(
  filename = "lectures/img/left_join.gif",
  animation = animated_gif
)


# Install these packages once if needed:
# install.packages(c("ggplot2", "gganimate", "gifski", "dplyr"))

library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)

# --------------------------------------------------
# Example tables
# --------------------------------------------------

table_a <- data.frame(
  id = c(1, 2, 3),
  name = c("Ana", "Ben", "Cara")
)

table_b <- data.frame(
  id = c(2, 3, 4),
  score = c(88, 95, 72)
)

# A RIGHT JOIN keeps every row from Table B
joined_table <- right_join(
  table_a,
  table_b,
  by = "id"
) |>
  arrange(id)

# --------------------------------------------------
# Create labels
# --------------------------------------------------

table_a_labels <- data.frame(
  x = 1.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_a$id,
    "   Name: ",
    table_a$name
  ),
  matched = table_a$id %in% table_b$id
)

table_b_labels <- data.frame(
  x = 5.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_b$id,
    "   Score: ",
    table_b$score
  ),
  matched = table_b$id %in% table_a$id
)

result_labels <- data.frame(
  x = 9.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    joined_table$id,
    "   Name: ",
    ifelse(
      is.na(joined_table$name),
      "NA",
      joined_table$name
    ),
    "   Score: ",
    joined_table$score
  )
)

# --------------------------------------------------
# Build the animation frames
# --------------------------------------------------

make_frame <- function(frame_number) {
  a <- table_a_labels
  b <- table_b_labels
  result <- result_labels

  a$frame <- frame_number
  b$frame <- frame_number
  result$frame <- frame_number

  if (frame_number == 1) {
    a$fill_group <- "Original row"
    b$fill_group <- "Original row"
    result$fill_group <- "Joined row"

    # Hide the result in the first frame
    result <- result[0, ]
  }

  if (frame_number == 2) {
    # Only matching rows from Table A contribute values
    a$fill_group <- ifelse(
      a$matched,
      "Matching row",
      "Excluded row"
    )

    # Every row from Table B is retained
    b$fill_group <- ifelse(
      b$matched,
      "Matching row",
      "Right-only row"
    )

    result$fill_group <- "Joined row"
    result <- result[0, ]
  }

  if (frame_number == 3) {
    a$fill_group <- ifelse(
      a$matched,
      "Matching row",
      "Excluded row"
    )

    b$fill_group <- ifelse(
      b$matched,
      "Matching row",
      "Right-only row"
    )

    result$fill_group <- ifelse(
      is.na(joined_table$name),
      "Unmatched result",
      "Joined row"
    )
  }

  list(
    a = a,
    b = b,
    result = result
  )
}

frames <- lapply(1:3, make_frame)

a_animation <- bind_rows(
  lapply(frames, function(x) x$a)
)

b_animation <- bind_rows(
  lapply(frames, function(x) x$b)
)

result_animation <- bind_rows(
  lapply(frames, function(x) x$result)
)

# --------------------------------------------------
# Lines connecting matching IDs
# --------------------------------------------------

join_lines <- data.frame(
  x = c(2.6, 2.6),
  xend = c(4.4, 4.4),
  y = c(2, 1),
  yend = c(3, 2)
)

join_lines <- bind_rows(
  transform(join_lines, frame = 2),
  transform(join_lines, frame = 3)
)

# --------------------------------------------------
# Frame titles
# --------------------------------------------------

frame_titles <- data.frame(
  frame = 1:3,
  x = 5.5,
  y = 4.8,
  title = c(
    "Step 1",
    "Step 2",
    "Step 3"
  )
)

# --------------------------------------------------
# Table titles
# --------------------------------------------------

table_titles <- data.frame(
  x = c(1.5, 5.5, 9.5),
  y = c(3.8, 3.8, 3.8),
  label = c(
    "Table A",
    "Table B",
    "RIGHT JOIN Result"
  )
)

# --------------------------------------------------
# Create the animation
# --------------------------------------------------

right_join_animation <- ggplot() +
  geom_label(
    data = a_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_label(
    data = b_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_segment(
    data = join_lines,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    color = "#E69F00",
    linewidth = 1.5,
    arrow = arrow(
      length = grid::unit(0.2, "inches")
    )
  ) +
  geom_label(
    data = result_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +

  # Centered main title
  geom_text(
    data = frame_titles,
    aes(
      x = x,
      y = y,
      label = title
    ),
    size = 10,
    fontface = "bold",
    hjust = 0.5
  ) +

  # Centered table titles
  geom_text(
    data = table_titles,
    aes(
      x = x,
      y = y,
      label = label
    ),
    size = 8,
    fontface = "bold",
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Original row" = "#0072B2",
      "Matching row" = "#009E73",
      "Right-only row" = "#56B4E9",
      "Excluded row" = "#999999",
      "Joined row" = "#CC79A7",
      "Unmatched result" = "#D55E00"
    ),
    breaks = c(
      "Matching row",
      "Right-only row",
      "Excluded row",
      "Joined row",
      "Unmatched result"
    )
  ) +
  coord_cartesian(
    xlim = c(-0.5, 11.5),
    ylim = c(0.3, 5.3),
    clip = "off"
  ) +
  labs(fill = NULL) +

  # Use two legend rows because the keys and text are large
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.key.size = grid::unit(2.5, "cm"),
    legend.spacing.x = grid::unit(0.4, "cm"),
    plot.margin = margin(30, 20, 20, 20)
  ) +
  transition_manual(frame) +
  enter_fade() +
  exit_fade()

# --------------------------------------------------
# Render and save the GIF
# --------------------------------------------------

animated_gif <- animate(
  right_join_animation,
  nframes = 90,
  fps = 15,
  width = 1600,
  height = 900,
  renderer = gifski_renderer(loop = TRUE)
)

anim_save(
  filename = "lectures/img/right_join.gif",
  animation = animated_gif
)

# Install these packages once if needed:
# install.packages(c("ggplot2", "gganimate", "gifski", "dplyr"))

library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)

# --------------------------------------------------
# Example tables
# --------------------------------------------------

table_a <- data.frame(
  id = c(1, 2, 3),
  name = c("Ana", "Ben", "Cara")
)

table_b <- data.frame(
  id = c(2, 3, 4),
  score = c(88, 95, 72)
)

# A FULL JOIN keeps every row from both tables
joined_table <- full_join(
  table_a,
  table_b,
  by = "id"
) |>
  arrange(id)

# --------------------------------------------------
# Create labels
# --------------------------------------------------

table_a_labels <- data.frame(
  x = 1.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_a$id,
    "   Name: ",
    table_a$name
  ),
  matched = table_a$id %in% table_b$id
)

table_b_labels <- data.frame(
  x = 5.5,
  y = c(3, 2, 1),
  label = paste0(
    "ID: ",
    table_b$id,
    "   Score: ",
    table_b$score
  ),
  matched = table_b$id %in% table_a$id
)

result_labels <- data.frame(
  x = 9.5,
  y = c(3.3, 2.4, 1.5, 0.6),
  label = paste0(
    "ID: ",
    joined_table$id,
    "   Name: ",
    ifelse(
      is.na(joined_table$name),
      "NA",
      joined_table$name
    ),
    "   Score: ",
    ifelse(
      is.na(joined_table$score),
      "NA",
      joined_table$score
    )
  )
)

# --------------------------------------------------
# Build animation frames
# --------------------------------------------------

make_frame <- function(frame_number) {
  a <- table_a_labels
  b <- table_b_labels
  result <- result_labels

  a$frame <- frame_number
  b$frame <- frame_number
  result$frame <- frame_number

  if (frame_number == 1) {
    a$fill_group <- "Original row"
    b$fill_group <- "Original row"
    result$fill_group <- "Joined row"

    # Hide the result during the first frame
    result <- result[0, ]
  }

  if (frame_number == 2) {
    a$fill_group <- ifelse(
      a$matched,
      "Matching row",
      "Left-only row"
    )

    b$fill_group <- ifelse(
      b$matched,
      "Matching row",
      "Right-only row"
    )

    result$fill_group <- "Joined row"
    result <- result[0, ]
  }

  if (frame_number == 3) {
    a$fill_group <- ifelse(
      a$matched,
      "Matching row",
      "Left-only row"
    )

    b$fill_group <- ifelse(
      b$matched,
      "Matching row",
      "Right-only row"
    )

    result$fill_group <- ifelse(
      is.na(joined_table$name) |
        is.na(joined_table$score),
      "Unmatched result",
      "Joined row"
    )
  }

  list(
    a = a,
    b = b,
    result = result
  )
}

frames <- lapply(1:3, make_frame)

a_animation <- bind_rows(
  lapply(frames, function(x) x$a)
)

b_animation <- bind_rows(
  lapply(frames, function(x) x$b)
)

result_animation <- bind_rows(
  lapply(frames, function(x) x$result)
)

# --------------------------------------------------
# Lines connecting matching IDs
# --------------------------------------------------

join_lines <- data.frame(
  x = c(2.6, 2.6),
  xend = c(4.4, 4.4),
  y = c(2, 1),
  yend = c(3, 2)
)

join_lines <- bind_rows(
  transform(join_lines, frame = 2),
  transform(join_lines, frame = 3)
)

# --------------------------------------------------
# Frame titles
# --------------------------------------------------

frame_titles <- data.frame(
  frame = 1:3,
  x = 5.5,
  y = 5,
  title = c(
    "Step 1",
    "Step 2",
    "Step 3"
  )
)

# --------------------------------------------------
# Table titles
# --------------------------------------------------

table_titles <- data.frame(
  x = c(1.5, 5.5, 9.5),
  y = c(3.9, 3.9, 3.9),
  label = c(
    "Table A",
    "Table B",
    "FULL JOIN Result"
  )
)

# --------------------------------------------------
# Create the animation
# --------------------------------------------------

full_join_animation <- ggplot() +
  geom_label(
    data = a_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_label(
    data = b_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +
  geom_segment(
    data = join_lines,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    color = "#E69F00",
    linewidth = 1.5,
    arrow = arrow(
      length = grid::unit(0.2, "inches")
    )
  ) +
  geom_label(
    data = result_animation,
    aes(
      x = x,
      y = y,
      label = label,
      fill = fill_group
    ),
    size = 4,
    label.size = 0.5,
    color = "white"
  ) +

  # Centered main title
  geom_text(
    data = frame_titles,
    aes(
      x = x,
      y = y,
      label = title
    ),
    size = 10,
    fontface = "bold",
    hjust = 0.5
  ) +

  # Centered table titles
  geom_text(
    data = table_titles,
    aes(
      x = x,
      y = y,
      label = label
    ),
    size = 8,
    fontface = "bold",
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Original row" = "#0072B2",
      "Matching row" = "#009E73",
      "Left-only row" = "#56B4E9",
      "Right-only row" = "#E69F00",
      "Joined row" = "#CC79A7",
      "Unmatched result" = "#D55E00"
    ),
    breaks = c(
      "Matching row",
      "Left-only row",
      "Right-only row",
      "Joined row",
      "Unmatched result"
    )
  ) +
  coord_cartesian(
    xlim = c(-0.5, 11.5),
    ylim = c(0, 5.5),
    clip = "off"
  ) +
  labs(fill = NULL) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.key.size = grid::unit(2.5, "cm"),
    legend.spacing.x = grid::unit(0.4, "cm"),
    plot.margin = margin(30, 20, 20, 20)
  ) +
  transition_manual(frame) +
  enter_fade() +
  exit_fade()

# --------------------------------------------------
# Render and save the GIF
# --------------------------------------------------

animated_gif <- animate(
  full_join_animation,
  nframes = 90,
  fps = 15,
  width = 1600,
  height = 950,
  renderer = gifski_renderer(loop = TRUE)
)

anim_save(
  filename = "lectures/img/full_join.gif",
  animation = animated_gif
)
