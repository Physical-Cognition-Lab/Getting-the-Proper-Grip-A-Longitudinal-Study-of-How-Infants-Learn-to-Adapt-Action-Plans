
library(brms)
library(tidyverse)
library(easystats)
library(patchwork)
library(ggtext)
library(emmeans)
library(svglite)


# Setting and paths -------------------------------------------------------

# Uncomment and set your working directory if needed
# setwd("path/to/your/directory")

# Load data
db  =  read.csv('.\\Data\\PlanningInfants_Session_Data.csv')

# Define the full palette (4 tools) for the main legend (MDir1)
my_colors <- c(
  Spoon  = "#440154",
  Hammer = "#31688e",
  Brush  = "#35b779",
  Magnet = "#f1a340"
)

# Define the subset palette (2 tools) for the second plot (MDir2)
custom_colors2 <- c(
  "Brush" = "#35b779",
  "Spoon" = "#440154"
)


# Fix data ---------------------------------------------------------------

db = db[!is.na(db$Grasp_onset), ]

S6 = unique(db[db$ID_id_num ==2 & db$ID_session_num == 6,]$ID_tdate)
S7 = unique(db[db$ID_id_num ==2 & db$ID_session_num == 7,]$ID_tdate)
S8 = unique(db[db$ID_id_num ==2 & db$ID_session_num == 8,]$ID_tdate)
S9 = unique(db[db$ID_id_num ==2 & db$ID_session_num == 9,]$ID_tdate)

db[db$ID_id_num ==2 & db$ID_tdate == S6, ]$ID_session_num = 8
db[db$ID_id_num ==2 & db$ID_tdate == S7, ]$ID_session_num = 7
db[db$ID_id_num ==2 & db$ID_tdate == S8, ]$ID_session_num = 9
db[db$ID_id_num ==2 & db$ID_tdate == S9, ]$ID_session_num = 6



df = db %>%
  # Arrange the data by 'ID_id_num' and 'id_session_num'
  arrange(ID_id_num, ID_session_num) %>%
  
  # Create new variables or modify existing ones
  mutate(
    
    # Convert these columns to factors
    ID_id_num = as.factor(ID_id_num),
    ID_session_num = as.factor(ID_session_num),
    Trial_tool_direction = as.factor(Trial_tool_direction),
    Trial_tool = factor(Trial_tool,
                        levels = c( 'h', 'b', 'm','s'),
                        labels = c( 'Hammer', 'Brush', 'Magnet','Spoon')),
    
    # Calculate age in days at the time of the test
    born = dmy(ID_bdate),  # Convert birth date to date format
    test = dmy(ID_tdate),  # Convert test date to date format
    Age = as.numeric(difftime(test, born, units = "days")),  # Calculate age in days
    
    # Create a new variable 'AdaptiveGrasp' that is 1 if 'Grasp_hand' equals 'Trial_tool_direction', and 0 otherwise
    AdaptiveGrasp = case_when(
      Grasp_hand == Trial_tool_direction & Grasp_overunder == 'o' ~ 1,
      Grasp_hand != Trial_tool_direction & Grasp_overunder == 'u' ~ 1,
      Grasp_hand == Trial_tool_direction & Grasp_overunder == 'm' ~ 1,
      Grasp_hand == Trial_tool_direction & Grasp_overunder == 'n' ~ 1,
      
      .default = 0)
  )


# Filter only the one that have direction and the spoon adn brush
df = df %>% filter(
  Trial_target == 'i' |  Trial_target == 'e',
  Trial_tool == 'Brush' | Trial_tool == 'Spoon') %>%
  mutate(Trial_target = factor(Trial_target,
                               levels = c( 'e','i'),
                               labels = c( 'Other','Self')))

# Standardizagre
df$Age = df$Age/7
df$AgeSt = standardize(df$Age)

db = df %>%
  mutate(Trial_tool_direction =  factor(Trial_tool_direction, levels = c("l", "r")))



# Bayesian model ----------------------------------------------------------

Priors = prior(normal(0, 4), class = b)

mod = brm(AdaptiveGrasp ~ Trial_tool_direction*Trial_tool*Trial_target*AgeSt + (1 + AgeSt | ID_id_num),
          data = db, family = bernoulli(), prior = Priors,
          chains = 4, iter = 8000, warmup = 6000, cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15),
          file = '.\\Results\\Models\\SelfOtherWeeks')

tool_p = parameters(mod, ci =.89)
tool_c = estimate_contrasts(mod, contrast = c('Trial_target','Trial_tool'), ci =.89)
tool_s = estimate_slopes(mod, trend = 'AgeSt', by=c('Trial_target','Trial_tool'),ci = .89)

 
##################### Effect of non-habitual and habitual ##################### 
tool_p %>% 
  mutate(Zero_in_CI = ifelse(CI_low < 0 & CI_high > 0, NA, TRUE))

estimate_contrasts(mod, contrast = 'Trial_tool', by = 'Trial_tool_direction' ,ci = .89)
estimate_contrasts(mod, contrast = c('Trial_tool', 'Trial_tool_direction') ,ci = .89)


Est_mod2 = estimate_means(mod, by= c('Trial_tool', 'Trial_tool_direction'), ci =.89)
Est_mod2$Trial_tool_direction = factor(Est_mod2$Trial_tool_direction, 
                                       levels = c("l", "r"), 
                                       labels = c("Handle-left", "Handle-right"))


#### 1. PREPARE PLOT 2 (Right Side - No Legend)
# Create the HTML string for colored text 
colored_labels <- paste0("<span style='color:", my_colors, "'>", names(my_colors), "</span>")
names(colored_labels) <- names(my_colors)


MDir2 <- Est_mod2 %>% 
  ggplot(aes(x = Trial_tool_direction, y = Probability, color = Trial_tool)) +
  geom_point(size = 5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                position = position_dodge(width = 0.5), width = .5, lwd = 1.6) +
  theme_bw(base_size = 30) +
  labs(x = '', y = 'Estimated probability') +
  scale_color_manual(values = custom_colors2) +
  
  # Hide elements for side-by-side view
  guides(color = "none") + 
  theme(
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.title.y = element_blank()  
  ) + 
  ylim(0.3, 0.85)

ggsave('.\\Results\\Plots\\SelfOther_DirectionRaw.svg',
       height = 10, width = 12, dpi = 300,device = 'svglite')

#### 2. PREPARE PLOT 1 (Left Side - Carrier of the Custom Legend)

MDir1 <- readRDS('.\\Results\\Plots\\ToolDirection.rds')

# Update MDir1 to use the custom text-only legend
MDir1 <- MDir1 + 
  ylim(0.3, 0.85) +
  
  # Apply Custom Legend Settings
  scale_color_manual(
    values = my_colors,       
    labels = colored_labels   # Apply the HTML colored labels
  ) +
  labs(color = NULL) +        
  
  # TRICK: Hide the graphical symbols (dots/lines) in the legend
  guides(
    color = guide_legend(
      override.aes = list(size = 0, linetype = 0, shape = NA) 
    )
  ) +
  
  theme(
    legend.position = "bottom",
    # Render the HTML labels as markdown
    legend.text = element_markdown(size = 30, face = "bold"), 
    # Remove the grey background boxes behind legend keys
    legend.key = element_rect(fill = NA, color = NA) 
  )

#### 3. COMBINE AND DISPLAY

final_plot <- MDir1 + MDir2 +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = list(c('A', 'B'))) &
  theme(
    plot.tag = element_text(size = 30), 
    legend.position = "bottom"
  )

final_plot
ggsave('.\\Results\\Plots\\SelfOther_ComboRaw.svg',
       height = 8, width = 16, dpi = 300, device = 'svglite')

 
# Plot main effect --------------------------------------------------------
gc()
 
# Calculate marginal means
Pred_tool <- emmeans(mod, ~ Trial_tool * Trial_target * AgeSt ,
             at = list(AgeSt =seq(min(db$AgeSt), max(db$AgeSt), .5)))

Pred_tool = as.data.frame(summary(Pred_tool, type = "response", level = 0.89))
Pred_tool$Age = unstandardise(Pred_tool$AgeSt, reference = df$Age)
Pred_tool$AgeM = round(unstandardise(Pred_tool$AgeSt, reference = df$Age)/ 4.33, 3)


# for each tool i add a small offset on the y axis to avoid overlapping
db <- db |>
  mutate(
    AdaptiveGraspTool = case_when(
      Trial_target == 'Self' & AdaptiveGrasp == 1 ~ 1.05,   # 1 + 0.5
      Trial_target == 'Self' & AdaptiveGrasp == 0 ~ -0.05,  # 0 - 0.5
      
      Trial_target == 'Other' & AdaptiveGrasp == 1 ~ 1.1,   # 1 + 0.5 + 0.5
      Trial_target == 'Other' & AdaptiveGrasp == 0 ~ -0.1,  # 0 - 0.5 - 0.5
      
      TRUE ~ AdaptiveGrasp
    ),
    AgeM = round(Age / 4.33, 3)
  )  



## Set the color for each plot
my_Spoon = c(Self = "#440154", Other = "#5a4e65")
Main_Spoon <- Pred_tool %>%
  filter(Trial_tool == 'Spoon') %>%
  mutate(Trial_target = fct_relevel(Trial_target, "Self", "Other")) |> 
  ggplot(aes(
    x = AgeM,
    y = response,
    color = Trial_target,
    fill = Trial_target,
    linetype = Trial_target
  )) +

  # Raw data
  geom_hline(yintercept = c(0, 1), lwd = 1.3, color = 'darkgray', alpha = 0.5) +
  geom_point(
    data = filter(db, Trial_tool == 'Spoon'),
    aes(y = AdaptiveGraspTool),
    size = 3,
    position = position_jitter(height = 0.018, width = 0.1),
    alpha = 0.6
  ) +

  geom_line(lwd = 2.2) +
  geom_ribbon(
    aes(ymin = lower.HPD, ymax = upper.HPD),
    color = 'transparent',
    alpha = 0.4
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_color_manual(values = my_Spoon) +
  scale_fill_manual(values = my_Spoon) +
  scale_linetype_manual(values = c("Self" = "solid", "Other" = "dashed")) + # Correct linetype mapping

  theme_classic(base_size = 50) +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5),
    legend.key.width = unit(3, "cm")
  ) + # Increase legend key width for better visibility
  labs(
    x = 'Age (months)',
    y = 'Estimated probability',
    fill = "",
    color = "",
    linetype = "",
    title = 'Spoon'
  ) +
  scale_y_continuous(
    breaks = seq(0.2, 1, 0.2),
    labels = c('0.2', '0.4', '0.6', '0.8', 'Adaptive\nGrasp')
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

Main_Spoon


my_Brush = c(Self = "#35b779", Other = "#7e9980")
Main_Brush = Pred_tool %>%
  filter(Trial_tool == 'Brush') %>%
  mutate(Trial_target = fct_relevel(Trial_target, "Self", "Other")) |> 

  ggplot(aes(
    x = AgeM,
    y = response,
    color = Trial_target,
    linetype = Trial_target
  )) +

  # Raw data
  geom_hline(yintercept = c(0, 1), lwd = 1.3, color = 'darkgray', alpha = 0.5) +
  geom_point(
    data = filter(db, Trial_tool == 'Brush'),
    aes(y = AdaptiveGraspTool),
    size = 3,
    position = position_jitter(height = 0.018, width = 0.1),
    alpha = 0.6
  ) +

  geom_line(lwd = 2.2) +
  geom_ribbon(
    aes(ymin = lower.HPD, ymax = upper.HPD, fill = Trial_target),
    color = 'transparent',
    alpha = 0.4
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_color_manual(values = my_Brush) +
  scale_fill_manual(values = my_Brush) +
  scale_linetype_manual(values = c("Self" = "solid", "Other" = "dashed")) +

  theme_classic(base_size = 50) +
  labs(
    x = 'Age (months)',
    y = 'Estimated probability',
    fill = "",
    color = "",
    linetype = "",
    title = 'Brush'
  ) +
  scale_y_continuous(
    breaks = seq(0.2, 1, 0.2),
    labels = c('0.2', '0.4', '0.6', '0.8', 'Adaptive\nGrasp')
  ) +
  theme(
    legend.position = 'bottom',
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.key.width = unit(3, "cm")
  ) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 1)),
    color = "none", # Remove color from legend
    linetype = "none" # Remove linetype from legend
  )

Main_Brush

Main_Spoon + Main_Brush
ggsave('.\\Results\\Plots\\SelfOtherDoubleRaw.svg',
       height = 25, width = 35, dpi = 300, device = 'svglite')



# Plot Individual level ---------------------------------------------------
gc()

# Calculate marginal means
Pre_Subject_tool <- emmeans(mod, ~ Trial_tool * Trial_target * AgeSt | ID_id_num,
                     at = list(AgeSt = seq(min(db$AgeSt), max(db$AgeSt), .1)),
                     re_formula = NULL)

Pre_Subject_tool = as.data.frame(summary(Pre_Subject_tool, type = "response", level = 0.89))
Pre_Subject_tool$Age = unstandardise(Pre_Subject_tool$AgeSt, reference = df$Age)
Pre_Subject_tool$AgeM = round(unstandardise(Pre_Subject_tool$AgeSt, reference = df$Age)/ 4.33, 3)


## Plot
SS = colorRampPalette(c("#f2a9ff","#440154"))(9)
Self_Spoon = Pre_Subject_tool %>%
  filter(Trial_target == 'Self', Trial_tool == 'Spoon') %>%

  ggplot(aes(x = AgeM, y = response, color = ID_id_num, fill = ID_id_num)) +

  # Raw data
  geom_hline(yintercept = c(0, 1), lwd = 1.3, color = 'darkgray', alpha = 0.5) +
  geom_point(
    data = filter(db, Trial_target == 'Self', Trial_tool == 'Spoon'),
    aes(
      y = ifelse(AdaptiveGrasp == 1, AdaptiveGrasp + 0.1, AdaptiveGrasp - 0.1)
    ),
    size = 3,
    position = position_jitter(height = 0.022, width = 0.01),
    alpha = 0.7)+

  geom_line(lwd = 1.9) +
  geom_ribbon(
    aes(ymin = lower.HPD, ymax = upper.HPD),
    color = 'transparent',
    alpha = 0.2
  ) +

  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(x = 'Age(months)', y = 'Estimated probability') +
  theme_classic(base_size = 40) +
  theme(legend.position = 'none') +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = c(
      'Non adaptive\nGrasp',
      '0.2',
      '0.4',
      '0.6',
      '0.8',
      'Adaptive\nGrasp'
    )
  ) +
  scale_x_continuous(
    breaks = seq(10, 18, 2),
  ) +
  scale_color_manual(values = SS) +
  scale_fill_manual(values = SS)


SO = colorRampPalette(c("#ADA8B6", "#5a4e65"))(9)
Other_Spoon = Pre_Subject_tool %>%
  filter(Trial_target == 'Other', Trial_tool == 'Spoon') %>%

  ggplot(aes(x = AgeM, y = response, color = ID_id_num, fill = ID_id_num)) +

  # Raw data
  geom_hline(yintercept = c(0, 1), lwd = 1.3, color = 'darkgray', alpha = 0.5) +
  geom_point(
    data = filter(db, Trial_target == 'Other', Trial_tool == 'Spoon'),
    aes(
      y = ifelse(AdaptiveGrasp == 1, AdaptiveGrasp + 0.1, AdaptiveGrasp - 0.1)
    ),
    size = 3,
    position = position_jitter(height = 0.022, width = 0.01),
    alpha = 0.7) +

  geom_line(lwd = 1.9) +
  geom_ribbon(
    aes(ymin = lower.HPD, ymax = upper.HPD),
    color = 'transparent',
    alpha = 0.2
  ) +

  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(x = 'Age(months)', y = '') +
  theme_classic(base_size = 40) +
  theme(legend.position = 'none', axis.text.y = element_blank()) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = c(
      'Non adaptive\nGrasp',
      '0.2',
      '0.4',
      '0.6',
      '0.8',
      'Adaptive\nGrasp'
    )
  ) +
  scale_x_continuous(
    breaks = seq(10, 18, 2),
  ) +
  scale_color_manual(values = SO) +
  scale_fill_manual(values = SO)



BS = colorRampPalette(c("#35b779","#455335"))(9)
Self_Brush = Pre_Subject_tool %>%
  filter(Trial_target == 'Self', Trial_tool == 'Brush') %>%

  ggplot(aes(x = AgeM, y = response, color = ID_id_num, fill = ID_id_num)) +

  # Raw data
  geom_hline(yintercept = c(0, 1), lwd = 1.3, color = 'darkgray', alpha = 0.5) +
  geom_point(
    data = filter(db, Trial_target == 'Self', Trial_tool == 'Brush'),
    aes(
      y = ifelse(AdaptiveGrasp == 1, AdaptiveGrasp + 0.1, AdaptiveGrasp - 0.1)
    ),
    size = 3,
    position = position_jitter(height = 0.022, width = 0.01),
    alpha = 0.7) +

  geom_line(lwd = 1.9) +
  geom_ribbon(
    aes(ymin = lower.HPD, ymax = upper.HPD),
    color = 'transparent',
    alpha = 0.2
  ) +

  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(x = 'Age(months)', y = '') +
  theme_classic(base_size = 40) +
  theme(legend.position = 'none', axis.text.y = element_blank()) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = c(
      'Non adaptive\nGrasp',
      '0.2',
      '0.4',
      '0.6',
      '0.8',
      'Adaptive\nGrasp'
    )
  ) +
  scale_x_continuous(
    breaks = seq(10, 18, 2),
  ) +
  scale_color_manual(values = BS) +
  scale_fill_manual(values = BS)


BO = colorRampPalette(c("#ADA8B6", "#7e9980"))(9)
Other_Brush = Pre_Subject_tool %>%
  filter(Trial_target == 'Other', Trial_tool == 'Brush') %>%

  ggplot(aes(x = AgeM, y = response, color = ID_id_num, fill = ID_id_num)) +

  # Raw data
  geom_hline(yintercept = c(0, 1), lwd = 1.3, color = 'darkgray', alpha = 0.5) +
  geom_point(
    data = filter(db, Trial_target == 'Other', Trial_tool == 'Brush'),
    aes(
      y = ifelse(AdaptiveGrasp == 1, AdaptiveGrasp + 0.1, AdaptiveGrasp - 0.1)
    ),
    size = 3,
    position = position_jitter(height = 0.022, width = 0.01),
    alpha = 0.7) +

  geom_line(lwd = 1.9) +
  geom_ribbon(
    aes(ymin = lower.HPD, ymax = upper.HPD),
    color = 'transparent',
    alpha = 0.2
  ) +

  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(x = 'Age(months)', y = '') +
  theme_classic(base_size = 40) +
  theme(legend.position = 'none', axis.text.y = element_blank()) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = c(
      'Non adaptive\nGrasp',
      '0.2',
      '0.4',
      '0.6',
      '0.8',
      'Adaptive\nGrasp'
    )
  ) +
  scale_x_continuous(
    breaks = seq(10, 18, 2),
  ) +
  scale_color_manual(values = BO) +
  scale_fill_manual(values = BO)



Top = Main_Spoon + Main_Brush
Bottom =(Self_Spoon + Other_Spoon + Self_Brush + Other_Brush) +
  plot_layout(ncol = 4)

Top / Bottom +
  plot_layout(heights = c(3, 1.5))&
  theme(plot.tag = element_text(size = 50))&
  plot_annotation(tag_levels = list(c('A', 'B', 'C', 'D','E','F')))


ggsave('.\\Results\\Plots\\SelfOtherComboRaw.svg',
       height = 30, width = 30, dpi = 300, device = 'svglite')



# After review 1 -----------------------------------------------------------


# Extract individual effects from the new model
individual_effects_new <- ranef(mod)

# Get slope deviations
slope_deviations_new <- individual_effects_new$ID_id_num[, "Estimate", "AgeSt"]
participant_ids_new <- as.numeric(rownames(individual_effects_new$ID_id_num))

slope_data_new <- data.frame(
  ID_id_num = factor(participant_ids_new),
  slope_deviation = slope_deviations_new
)

# Count sessions per participant (same as before)
trials_per_participant_new <- db %>%
  group_by(ID_id_num) %>%
  summarise(
    n_sessions = n_distinct(AgeSt),
    .groups = 'drop'
  )

# Combine with slope data
analysis_data_new <- slope_data_new %>%
  left_join(trials_per_participant_new, by = "ID_id_num")

# Test correlation
cor_sessions_new <- cor_test('n_sessions' ,'slope_deviation', data = analysis_data_new)
print(cor_sessions_new)


## Session x direction plots ----------------------------------------------------------

SesXDir = readRDS('.\\Results\\Plots\\SessionSpreadPlotToolDirection.rds') +
  scale_x_continuous(breaks = seq(1, 9, 1)*4, labels = rep('',9), name = NULL) +
  theme(axis.ticks.x = element_blank())+
  guides(
    colour = guide_legend(order = 1, title = "Tool"),  # keep only colour
    shape  = "none"                                     # drop shape
  )


SesXDirTargetDf <- db |> 
  # 0) prep factors & IDs
  mutate(
    TOOLDIR = paste(Trial_tool, Trial_target),
    TOOLDIR = factor(
      TOOLDIR,
      levels = c(
        "Spoon Self",
        "Spoon Other",
        "Brush Self",
        "Brush Other"
      )
    ),
    Trial_tool_direction = recode_factor(
      Trial_tool_direction,
      l = "Left",
      r = "Right"
    ),
    ID_id_num = as.numeric(ID_id_num) * 4
  ) |> 
  # 1) count sessions per Age within each ID/ToolDir/Direction
  group_by(ID_id_num, Age, TOOLDIR, Trial_tool_direction) |>
  summarise(
    Session = n(),
    .groups = "drop"
  ) |> 
  # 2) now average those Session counts across Ages
  group_by(ID_id_num, TOOLDIR, Trial_tool_direction) |>
  summarise(
    meanSession = mean(Session),
    .groups = "drop"
  )


SesXDirTargetPlot  = 
  ggplot(SesXDirTargetDf,  aes(x = as.numeric(ID_id_num)*1, y = meanSession, color=TOOLDIR, shape=factor(Trial_tool_direction))) +
  geom_point(size = 4,       position = position_dodge2(
        width    = 3.3,   # ← controls distance between colour‑groups
        padding  = 1.2,   # ← controls distance between shapes *within* each colour
        preserve = "single"
      )) +
  labs(x = 'Subject Id', y = '# session', shape= 'Direction', color= 'Tool')+
  
  scale_y_continuous(breaks = seq(2, 18, 2))+
  scale_x_continuous(breaks = seq(1, 9, 1)*4, labels = seq(1, 9, 1))+
  theme_minimal(base_size = 35)+
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor   = element_blank()
  )+
  
  scale_color_manual(values =  c(
    "Brush Self"  = "#35b779",
    "Spoon Self"  = "#440154",
    "Brush Other"  = "#7e9980",
    "Spoon Other"  = "#5a4e65"
)) +
guides(
    shape  = guide_legend(order = 2, title = "Direction"),  # only shape
    colour = guide_legend(order = 3, title = "Tool & Target")  
  )


# 3) Stack them, collect, right side
(SesXDir / SesXDirTargetPlot) +
  plot_layout(guides = "collect") &
  theme(
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.justification = "center"
  )

ggsave('.\\Results\\Plots\\SessionSpreadPlotToolDirectionTotal.svg',  height = 16, width = 20, dpi = 300, device = 'svglite')



#### Run the model to check distribution

Priors = prior(normal(0, 4), class = b)

mod = brm(
  Trial_tool_direction ~
      Trial_tool *
      Trial_target *
      AgeSt +
      (1 + AgeSt | ID_id_num),
  data = db,
  family = bernoulli(),
  prior = Priors,
  chains = 4,
  iter = 8000,
  warmup = 6000,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  file = '.\\Results\\Models\\DirectionHand_Target'
)


parameters(mod2, ci = .89)