# Load packages
library(tidyverse)
library(gt)
library(gtExtras)
library(easystats)


# Prepare data -------------------------------------------------------

# Load data
db = read.csv('.\\Data\\PlanningInfants_Session_Data.csv')
db = db[!is.na(db$Grasp_onset), ]

S6 = unique(db[db$ID_id_num == 2 & db$ID_session_num == 6, ]$ID_tdate)
S7 = unique(db[db$ID_id_num == 2 & db$ID_session_num == 7, ]$ID_tdate)
S8 = unique(db[db$ID_id_num == 2 & db$ID_session_num == 8, ]$ID_tdate)
S9 = unique(db[db$ID_id_num == 2 & db$ID_session_num == 9, ]$ID_tdate)

db[db$ID_id_num == 2 & db$ID_tdate == S6, ]$ID_session_num = 8
db[db$ID_id_num == 2 & db$ID_tdate == S7, ]$ID_session_num = 7
db[db$ID_id_num == 2 & db$ID_tdate == S8, ]$ID_session_num = 9
db[db$ID_id_num == 2 & db$ID_tdate == S9, ]$ID_session_num = 6


df = db |>
  # Arrange the data by 'ID_id_num' and 'id_session_num'
  arrange(ID_id_num, ID_session_num) |>

  # Create new variables or modify existing ones
  mutate(
    # Convert these columns to factors
    ID_id_num = as.factor(ID_id_num),
    ID_session_num = as.factor(ID_session_num),
    Trial_tool_direction = as.factor(Trial_tool_direction),
    Trial_target = as.factor(Trial_target),
    Trial_tool = factor(
      Trial_tool,
      levels = c('h', 'b', 'm', 's'),
      labels = c('Hammer', 'Brush', 'Magnet', 'Spoon')
    ),

    # Calculate age in days at the time of the test
    born = dmy(ID_bdate), # Convert birth date to date format
    test = dmy(ID_tdate), # Convert test date to date format
    Age = as.numeric(difftime(test, born, units = "days")), # Calculate age in days

    # Create a new variable 'AdaptiveGrasp' that is 1 if 'Grasp_hand' equals 'Trial_tool_direction', and 0 otherwise
    AdaptiveGrasp = case_when(
      Grasp_hand == Trial_tool_direction & Grasp_overunder == 'o' ~ 1,
      Grasp_hand != Trial_tool_direction & Grasp_overunder == 'u' ~ 1,
      Grasp_hand == Trial_tool_direction & Grasp_overunder == 'm' ~ 1,
      Grasp_hand == Trial_tool_direction & Grasp_overunder == 'n' ~ 1,

      .default = 0
    )
  ) |>

  # Group the data by 'ID_id_num'
  group_by(ID_id_num) |>
  # Create a new variable 'session' that represents the session number for each subject
  mutate(session = match(ID_session_num, unique(ID_session_num))) |>
  ungroup()

df$Age = df$Age/7
df$AgeSt = standardize(df$Age)


# Table for sessions ------------------------------------------------------

# Constants
WEEKS_PER_MONTH <- 4.348
X_RANGE <- c(35, 85)
PLOT_HEIGHT <- px(60)
PLOT_ASPECT_RATIO <- 8
POINT_SIZE <- 14

# Color palette for subjects (darker pastels)
COLOR_PALETTE <- c("#c85555", "#5a8bc7", "#59b359", "#9066b2", "#e07d44", 
                   "#d4a62b", "#b2725b", "#e081bf", "#8c8c8c", "#4988c4")

# --- DATA PREPARATION ---
df <- df %>%
  mutate(AgeWeeks = as.numeric(difftime(test, born, units = "weeks")))

# Summary statistics by ID
A <- df %>%
  group_by(ID_id_num) %>%
  summarize(
    `First session`        = round(min(AgeWeeks), 1),
    `Last session`         = round(max(AgeWeeks), 1),
    `First session_months` = round(min(AgeWeeks) / WEEKS_PER_MONTH, 1),
    `Last session_months`  = round(max(AgeWeeks) / WEEKS_PER_MONTH, 1),
    `# Session`            = n_distinct(AgeWeeks),
    .groups = "drop"
  )

# Trial tool counts (pivoted)
B <- df %>%
  count(ID_id_num, Trial_tool, name = "SessionN") %>%
  pivot_wider(names_from = Trial_tool, values_from = SessionN, values_fill = 0)

# Get unique IDs and create color mapping
all_ids <- unique(A$ID_id_num)
n_subjects <- length(all_ids)
extended_palette <- rep(COLOR_PALETTE, ceiling(n_subjects / length(COLOR_PALETTE)))
color_mapping <- setNames(extended_palette[seq_len(n_subjects)], all_ids)

# --- HELPER FUNCTIONS ---

# Common ggplot theme for consistency
create_base_plot_theme <- function(show_x_axis = FALSE) {
  base_theme <- theme_minimal(base_size = 14) +
    theme(
      plot.margin = margin(5, 8, 5, 8),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(linewidth = 2),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 2),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  if (!show_x_axis) {
    base_theme <- base_theme +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank()
      )
  } else {
    base_theme <- base_theme +
      theme(
        axis.text.x = element_text(size = 100, margin = margin(t = 15)),
        axis.ticks.x = element_line(linewidth = 4),
        axis.line.x = element_line(linewidth = 4),
        axis.title.x = element_text(size = 100, margin = margin(t = 25))
      )
  }
  
  base_theme
}

# Create plot for individual subject
create_plot_html <- function(id_value) {
  session_data <- df %>%
    filter(ID_id_num == id_value) %>%
    distinct(AgeWeeks) %>%
    arrange(AgeWeeks)
  
  if (nrow(session_data) == 0) {
    return('<div style="height:80px; width:300px; background:#f0f0f0; display:flex; align-items:center; justify-content:center; font-size:12px; border:1px solid #ccc;">No data</div>')
  }
  
  subject_color <- color_mapping[as.character(id_value)]
  
  p <- ggplot(session_data, aes(x = AgeWeeks, y = 0)) +
    geom_point(size = POINT_SIZE, color = subject_color, alpha = 1, shape = 19) +
    xlim(X_RANGE[1], X_RANGE[2]) +
    ylim(-0.1, 0.1) +
    create_base_plot_theme(show_x_axis = FALSE)
  
  ggplot_image(p, height = PLOT_HEIGHT, aspect_ratio = PLOT_ASPECT_RATIO)
}

# Create x-axis only plot
create_axis_plot <- function() {
  axis_data <- data.frame(x = X_RANGE, y = c(0, 0))
  
  p <- ggplot(axis_data, aes(x = x, y = y)) +
    xlim(X_RANGE[1], X_RANGE[2]) +
    ylim(-0.01, 0.1) +
    create_base_plot_theme(show_x_axis = TRUE) +
    labs(x = "Age (weeks)")
  
  ggplot_image(p, height = PLOT_HEIGHT, aspect_ratio = PLOT_ASPECT_RATIO)
}

# --- GENERATE PLOT DATA ---
plot_data <- tibble(ID_id_num = all_ids) %>%
  mutate(plot_html = map_chr(ID_id_num, create_plot_html))

# --- COMBINE DATA FOR TABLE ---
# Create spacer columns
spacer_cols <- c(" ", "  ", "   ", "    ")
spacer_data <- setNames(rep("", length(spacer_cols)), spacer_cols)

table_data <- A %>%
  left_join(B, by = "ID_id_num") %>%
  left_join(plot_data, by = "ID_id_num") %>%
  mutate(!!!spacer_data) %>%
  select(
    ID_id_num,
    ` `,
    `# Session`,
    `  `,
    `First session`, `Last session`,
    `    `,
    `First session_months`, `Last session_months`,
    `   `,
    all_of(names(select(B, -ID_id_num))),
    plot_html
  )

# --- CREATE AXIS ROW ---
tool_cols <- names(select(B, -ID_id_num))
axis_row_data <- list(
  ID_id_num = "",
  `# Session` = NA_real_,
  `First session` = NA_real_,
  `Last session` = NA_real_,
  `First session_months` = NA_real_,
  `Last session_months` = NA_real_,
  plot_html = create_axis_plot()
)

# Add spacer and tool columns
axis_row_data <- c(axis_row_data, spacer_data)
axis_row_data[tool_cols] <- NA_real_

axis_row <- as_tibble(axis_row_data)

# Combine main data with axis row
final_table_data <- bind_rows(table_data, axis_row)

# --- CREATE GT TABLE ---
gt_tbl <- final_table_data %>%
  gt() %>%
  fmt(
    columns = plot_html,
    fns = function(x) map_chr(x, html)
  ) %>%
  cols_label(
    ID_id_num = "Id",
    plot_html = "Sessions distribution",
    `First session_months` = "First session",
    `Last session_months` = "Last session"
  ) %>%
  tab_spanner("Age (weeks)", c(`First session`, `Last session`)) %>%
  tab_spanner("Age (months)", c(`First session_months`, `Last session_months`)) %>%
  tab_spanner("# of trials", starts_with(c("Spoon", "Brush", "Hammer", "Magnet"))) %>%
  cols_width(
    ` ` ~ px(12), `  ` ~ px(12), `   ` ~ px(12), `    ` ~ px(12),
    plot_html ~ px(320)
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = !plot_html)
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = plot_html)
  ) %>%
  tab_style(
    style = cell_text(color = "transparent"),
    locations = cells_body(
      columns = !plot_html,
      rows = nrow(final_table_data)
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "transparent"),
      cell_fill(color = "white")
    ),
    locations = cells_body(
      columns = everything(),
      rows = nrow(final_table_data)
    )
  ) %>%
  tab_options(
    table.font.size = px(20),
    data_row.padding = px(4),
    table_body.border.bottom.style = "hidden",
    table.border.bottom.style = "hidden"
  ) %>%
  opt_css(
    css = "
      #gt_tbl tbody tr:last-child td {
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        height: 40px !important;
      }
    "
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      style = "hidden"
    ),
    locations = cells_body(rows = nrow(final_table_data))
  )

gt_tbl %>% gtsave(".\\Results\\Tables\\SessionCounter.html")
gt_tbl %>%
  gtsave(
    ".\\Results\\Tables\\SessionCounter.png",
    vwidth = 2000,   # increase width
    vheight = 1200   # increase height
  )



# Skipped sessions -------------------------------------------------------

SessionSummary = df |> 
  group_by(ID_id_num) |> 
  summarize(Age = unique(Age)) |> 
  mutate(Age = floor(Age)) |> 
  ungroup() |> 
  group_by(ID_id_num) |> 
  summarize(
    'Completed sessions' = n(),
    'Missed sessions' = sum(diff(Age) - 1)
  ) |> 
  rename(Id = ID_id_num) |> 
  gt() |> 
  cols_align(align = "center")  # Center all columns

SessionSummary %>% gtsave(".\\Results\\Tables\\SkippedSessions.html")
SessionSummary %>% gtsave(".\\Results\\Tables\\SkippedSessions.png")


# Table tools ------------------------------------------------------------

df %>%
  group_by(ID_id_num, Trial_tool) %>%
  summarise(
    Left = sum(Trial_tool_direction == 'l'),
    Right = sum(Trial_tool_direction == 'r')
  ) %>%
  ungroup() %>%
  rename(
    Id = ID_id_num,
    Tool = Trial_tool
  ) %>%
  pivot_longer(
    cols = c(Left, Right),
    names_to = 'Direction',
    values_to = 'Count'
  ) %>%
  pivot_wider(names_from = Tool, values_from = Count) %>%
  gt(rowname_col = "ID_id_num") %>%
  tab_spanner(
    label = "Tools",
    columns = c(Spoon, Brush, Hammer, Magnet)
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) 

gt_tbl %>% gtsave(".\\Results\\Tables\\SessionCounter.html")
gt_tbl %>% gtsave(".\\Results\\Tables\\SessionCounter.png")
