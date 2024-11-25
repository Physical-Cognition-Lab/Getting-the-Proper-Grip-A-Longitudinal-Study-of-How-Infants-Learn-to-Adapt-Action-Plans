
library(tidyverse)
library(easystats)
library(parallel)
library(gt)
library(patchwork)

library(brms)

library(lme4)
library(lmerTest)
library(emmeans)


# Setting and paths -------------------------------------------------------

# Uncomment and set your working directory if needed
setwd("path/to/your/directory")

# Load data
db  =  read.csv('.\\Data\\PlanningInfants_Session_Data.csv')



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
    Trial_target = as.factor(Trial_target),
    Trial_tool = factor(Trial_tool,
                        levels = c('h', 'b', 'm','s'),
                        labels = c('Hammer', 'Brush', 'Magnet','Spoon')),
    
    
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
  ) %>%
  
  # Group the data by 'ID_id_num'
  group_by(ID_id_num) %>%
  # Create a new variable 'session' that represents the session number for each subject
  mutate(session = match(ID_session_num, unique(ID_session_num))) %>%
  ungroup()

df$AgeSt = standardize(df$Age)

db = df %>%  filter( Trial_target != 'e') %>% 
  mutate(Trial_tool_direction =  factor(Trial_tool_direction, levels = c("l", "r")))



# Table information ------------------------------------------------------

# Remove file if exist
if (file.exists(".\\Results\\Tables\\ToolDirection.docx")) {
  file.remove(".\\Results\\Tables\\ToolDirection.docx")}

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
  pivot_longer(cols = c(Left, Right), names_to = 'Direction', values_to = 'Count') %>% 
  pivot_wider( names_from = Tool, values_from = Count) %>% 
  gt(rowname_col = "ID_id_num") %>% 
  tab_spanner(
    label = "Tools",
    columns = c(Spoon, Brush, Hammer, Magnet)
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())) %>%
  gtsave(".\\Results\\Tables\\ToolDirection.docx")


# Remove file if exist
if (file.exists(".\\Results\\Tables\\SessionCounter.docx")) {
  file.remove(".\\Results\\Tables\\SessionCounter.docx")}

A = df %>%
  group_by(ID_id_num) %>%
  mutate(AgeWeeks = as.numeric(difftime(test, born, units = "weeks"))) %>% 
  summarize(
    `First session` = round(min(AgeWeeks),1),
    `Last session` = round(max(AgeWeeks),1),
    `# Session` = n_distinct(AgeWeeks)) %>% 
  ungroup()

B = df %>%
  group_by(ID_id_num, Trial_tool) %>% 
  summarise(SessionN = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Trial_tool, values_from = SessionN )


left_join(A, B, by = 'ID_id_num') %>%
  mutate(` ` = '', `  ` = '', `   ` = '', `    ` = '') %>%  # Create an empty column
  select(ID_id_num, ` `, `# Session`, `  `, `First session`, `Last session`, `   `, `    `, Spoon, Brush, Hammer, Magnet) %>%  # Manually reorder columns with the spacer
  gt() %>%
  cols_label(ID_id_num = "Id") %>%  # Renames the rowname column
  tab_spanner(
    label = "Age (weeks)",
    columns = c(`First session`, `Last session`)
  ) %>%
  tab_spanner(
    label = "# of trials",
    columns = c(Spoon, Brush, Hammer, Magnet)
  ) %>%
  cols_width( ` ` ~ px(20), `  ` ~ px(20), `   ` ~ px(20), `    ` ~ px(20)) %>% # Adjust the width of the empty column (spacer)
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())) %>%
  gtsave(".\\Results\\Tables\\SessionCounter.docx")




# Model -------------------------------------------------------


db %>% 
  group_by(Trial_tool, Trial_tool_direction) %>% 
  summarise(n())

Priors = prior(normal(0, 4), class = b)
mod = brm(AdaptiveGrasp ~ Trial_tool*AgeSt*Trial_tool_direction + 
             (1 + AgeSt | ID_id_num),
           data = db, family = bernoulli(), prior = Priors,
           chains = 4, iter = 8000, warmup = 6000, cores = 4,
           control = list(adapt_delta = 0.99, max_treedepth = 15),
           file = '.\\Results\\Models\\ToolUse')

parameters::parameters(mod, ci=.89)

c = estimate_contrasts(mod, contrast = 'Trial_tool', ci = 0.89)
s = estimate_slopes(mod, trend = 'AgeSt', by = 'Trial_tool', ci = 0.89)


##################### Effect of non-habitual and habitual ##################### 

estimate_contrasts(mod, contrast = 'Trial_tool' , by = 'Trial_tool_direction' ,ci = .89)

Est_mod = estimate_means(mod, by = c('Trial_tool', 'Trial_tool_direction'))
Est_mod$Trial_tool_direction = factor(Est_mod$Trial_tool_direction, 
                                      levels = c("l", "r"), 
                                      labels = c("Handle-left", "Handle-right"))

custom_colors <- c(
  "Hammer" = "#31688e",
  "Brush"  = "#35b779",
  "Magnet" = "#fde725",
  "Spoon"  = "#440154"
)


MDir1 = Est_mod %>% 
  ggplot(aes(x = Trial_tool_direction, y = Probability, color = Trial_tool ))+
  geom_point(size = 5, position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), position = position_dodge(width = 0.5), width =.5, lwd = 1.6)+
  theme_bw(base_size = 20)+
  labs(x = '', color = 'Tool', y = 'Estimated probability')+
  scale_color_manual(values = custom_colors)
MDir1
saveRDS(MDir1, '.\\Results\\Plots\\ToolDirection.rds' )
ggsave('.\\Results\\Plots\\ToolDirection.svg',
       height = 10, width = 12, dpi = 300)







# Plot Main effect --------------------------------------------------------

# Calculate marginal means
Pred <- emmeans(mod, ~ Trial_tool * AgeSt ,
                     at = list(AgeSt = seq(-2, 2, .5)))

Pred = as.data.frame(summary(Pred, type = "response", level = 0.89))
Pred$Age = unstandardise(Pred$AgeSt, reference = df$Age)


my_colors <- c(Spoon = "#440154", Hammer = "#31688e", Brush = "#35b779", Magnet = "#fde725")
Main = ggplot(Pred, aes(x = Age, y =  response, color =  Trial_tool, fill = Trial_tool))+
  geom_line(lwd = 2)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.4, color = 'transparent')+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors)+
  
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'bottom')+
  labs(x = 'Age(days)', y = 'Estimated probability',fill="", color= "")+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  guides(colour = "none", point = 'none')+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(277,530))

Main



# Plot individual level ---------------------------------------------------

# Calculate marginal means
Pred_Subject <- emmeans(mod, ~ Trial_tool * AgeSt | ID_id_num,
                            at = list(AgeSt = seq(-2, 2, .5)),
                            re_formula = NULL)

Pred_Subject = as.data.frame(summary(Pred_Subject, type = "response", level = 0.89))
Pred_Subject$Age = unstandardise(Pred_Subject$AgeSt, reference = df$Age)


## Spoon
S = colorRampPalette(c("#f2a9ff","#440154"))(9)
SPOON = Pred_Subject %>%
  filter(Trial_tool == 'Spoon')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.20, color = 'transparent')+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = '', y = 'Estimated probability')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none',
        axis.text.x = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = S )+
  scale_fill_manual(values  = S)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(277,530))

## Hammer
H = colorRampPalette(c("#31688e", "#3a4856"))(9)
HAMMER = Pred_Subject %>%
  filter(Trial_tool == 'Hammer')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.20, color = 'transparent')+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = '', y = '')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = H )+
  scale_fill_manual(values  = H)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(277,530))

## Brush
B = colorRampPalette(c("#35b779","#455335"))(9)
BRUSH = Pred_Subject %>%
  filter(Trial_tool == 'Brush')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.20, color = 'transparent')+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = 'Age(days)', y = 'Estimated probability')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none')+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = B )+
  scale_fill_manual(values  = B)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(277,530))

## Magnet
M = colorRampPalette(c("#fde725","#ab9d68"))(9)
MAGNET = Pred_Subject %>%
  filter(Trial_tool == 'Magnet')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.20, color = 'transparent')+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = 'Age(days)', y = '')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = M )+
  scale_fill_manual(values  = M)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(277,530))


(Main / (SPOON + HAMMER) / (BRUSH + MAGNET))+
  plot_layout(heights = c(2.5, 1, 1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 50))

ggsave('.\\Results\\Plots\\Tools.svg',
       height = 35, width = 25, dpi = 300)




# Plot individual slopes --------------------------------------------------

# Using emmtrends to calculate slopes of AgeSt for each subject and tool
slopes_Subject_tool <- emtrends(mod, ~ Trial_tool | ID_id_num , 
                                var = "AgeSt", 
                                at = list(AgeSt = seq(-2, 2, 0.25)),
                                re_formula = NULL)

slopes_df <- summary(slopes_Subject_tool, level = 0.89)
slopes_df = slopes_df %>% 
  mutate(Sign = ifelse(lower.HPD <= 0 & upper.HPD >=0, F, T),
         Trial_tool = factor(Trial_tool, levels = c("Spoon", "Hammer", "Brush", "Magnet")))

plot_sl = ggplot(slopes_df, aes(x = ID_id_num, y = AgeSt.trend, color = ID_id_num)) +
  geom_point(position = position_dodge(width= 1), size =3)+
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD),position = position_dodge(width= 1), lwd = 1.4)+
  geom_hline(yintercept = 0, lwd =1, linetype = 'dashed', alpha =0.5)+
  facet_wrap(~Trial_tool, nrow = 1)+
  labs(y = expression("Estimated" ~ beta), x = 'Subject Id')+
  theme_bw(base_size = 35)+
  theme(legend.position = 'none',
        strip.text = element_text(color = "white", face = "bold",size = 35),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 55))+
  scale_color_see()


# Build the plot using ggplot_gtable
g <- ggplot_gtable(ggplot_build(plot_sl))

# Find which grobs correspond to the facet strips (facet titles)
stripr <- which(grepl('strip-', g$layout$name))  # Works for both strip-r and strip-t (facet labels)

# Define custom colors for each facet
fills <- c("#440154","#31688e", "#35b779", "#fde725")  # Customize colors as needed

# Loop through the facet strips and apply the background colors
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k + 1
}

# Draw the plot and save it
svg(".\\Results\\Plots\\IndividualSlopes.svg", height = 10, width = 16)  # Open PNG device
grid::grid.draw(g)  # Draw the plot
dev.off()  # Close the device and save the file



#### Giant Plot
(Main / (SPOON + HAMMER) / (BRUSH + MAGNET) / g)+
  plot_layout(heights = c(2.5, 1, 1,1.2))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 50))

ggsave('.\\Results\\Plots\\ToolsTotal.svg',
       height = 35, width = 25, dpi = 300)



# Prediction of spoon over age ------------------------------------------------------------
gc()
library(tidybayes)

Age_to_calculate_over = standardize( seq(270, 600, 15), reference = db$Age)

# Create new data
MeansDraws <- expand.grid(AgeSt = Age_to_calculate_over,
                          Trial_tool = "Spoon",
                          Trial_tool_direction= c('r','l'),
                          ID_id_num = NA )



# Add expected posterior draws to the new data
MeansDraws <- MeansDraws %>%
  add_epred_draws(object = mod, re_formula = NA, ndraws = 1000)

Pred <-  MeansDraws %>%
  group_by(AgeSt) %>%
  summarize(
    Predicted = mean(.epred),
    Se = sd(.epred) / sqrt(n()),
    CI_low = tidybayes::qi(.epred, .width = .89)[1,1],
    CI_high = tidybayes::qi(.epred, .width = .89)[1,2]
  ) %>%
  mutate(
    Age = unstandardize(AgeSt, reference = db$Age),
    years = floor(Age / 365.25),
    months = floor((Age %% 365.25) / 30.44),
    days = floor((Age %% 365.25) %% 30.44),
    TotMonths = Age / 30.44
  )


# Unstandardize age
MeansDraws$Age = unstandardize(MeansDraws$AgeSt, reference = db$Age )

# Plot distributions for age
Prediction_plot = MeansDraws %>%
  ggplot(aes(x = Age, y = .epred)) +
  stat_halfeye( aes(fill = after_stat(level)),
                width= 15, .width = .89, adjust = 0.6)+
  
  # Add the highlighted rectangle
  annotate("rect", xmin = Pred[which(Pred$CI_low >= 0.75)[1],]$Age-5,
           xmax = max(Pred$Age)+22,
           
           ymin = 0.752, ymax = .975,
           fill = "black", alpha = 0.1) +
  geom_hline(yintercept = .5, linetype = 'dashed', lwd=1.2)+
  geom_hline(yintercept = .75, linetype = 'dashed', lwd=1.2, color ='darkred')+
  
  theme_bw(base_size = 35)+
  theme(legend.position = 'none')+
  scale_fill_oi() +  # Set a specific color for the fill
  labs(y = 'Estimated probability',
       x = "Age(days)")+
  coord_cartesian(ylim = c(0.38, 1))+
  scale_y_continuous(breaks = seq(0.4, 1, 0.1),
                     labels = c('0.4','0.5','0.6','0.7','0.8','0.9','Adaptive\nGrasp'))


Prediction_plot
ggsave('.\\Results\\Plots\\SpoonPrediction.svg',
       width = 20, height= 12, dpi=300)




## Simulation (add stepwise change) ---------------------------------------------------------
gc()
if (file.exists('.\\Results\\Models\\Simualtion.RDS')){
  
  Df = readRDS('.\\Results\\Models\\Simualtion.RDS')
  
} else{
  
  # Function to run the simulation
  run_simulation <- function(i, xx) {
    dbb <- xx
    dbb$Simulation <- NA
    
    dbb <- dbb %>%
      group_by(ID_id_num, Trial_tool) %>%
      mutate(
        tresh = sample(unique(AgeSt), 1),
        Simulation = if_else(AgeSt < tresh, 0, 1)
      ) %>%
      ungroup()
    
    modLm <- glmer(Simulation ~ Trial_tool*AgeSt*Trial_tool_direction + (1 + AgeSt | ID_id_num),
                   data = dbb, family = binomial(), control = glmerControl(optimizer = "bobyqa"))
    
    Db = as.data.frame(estimate_slopes(modLm, trend = 'AgeSt', by ='Trial_tool'))
    Db$Iter = i
    return(Db)
  }
  
  # Number of cores to use
  num_cores <- detectCores() - 1  # Use one less than the total number of cores
  
  # Set up the cluster
  cl <- makeCluster(num_cores)
  
  # Export necessary libraries and objects to each worker
  clusterEvalQ(cl, {
    library(tidyverse)
    library(lme4)
    library(lmerTest)
    library(easystats)
  })
  
  # Export the dataframe to each worker
  clusterExport(cl, varlist = c("df"))
  
  # Run the simulations in parallel, passing df to each function call
  results <- parLapply(cl, 1:1000, run_simulation, db)
  
  # Stop the cluster
  stopCluster(cl)
  Df = bind_rows(results)
  saveRDS(Df, '.\\Results\\Models\\Simualtion.RDS')
  gc()
}


## Extract distribution ----------------------------------------------------

Hammer=  Df %>% filter(Trial_tool == 'Hammer')
HammerCi = ci(Hammer$Coefficient, ci= .89)
Hammer = as.data.frame(density(Hammer$Coefficient))

Magnet =  Df %>% filter(Trial_tool == 'Magnet')
MagnetCi = ci(Magnet$Coefficient, ci= .89)
Magnet = as.data.frame(density(Magnet$Coefficient))

Spoon =  Df %>% filter(Trial_tool == 'Spoon')
SpoonCi = ci(Spoon$Coefficient, ci= .89)
Spoon = as.data.frame(density(Spoon$Coefficient))

Brush =  Df %>% filter(Trial_tool == 'Brush')
BrushCi = ci(Brush$Coefficient, ci= .89)
Brush = as.data.frame(density(Brush$Coefficient))



## Posterior draws ---------------------------------------------------------

posterior_effects <- mod %>%
  spread_draws( b_AgeSt,
                b_Trial_toolBrush,
                b_Trial_toolMagnet,
                b_Trial_toolSpoon) %>%
  mutate(
    Hammer = b_AgeSt, 
    Brush = Hammer + b_Trial_toolBrush,
    Magnet = Hammer + b_Trial_toolMagnet,
    Spoon = Hammer + b_Trial_toolSpoon
  )


HammerP = as.data.frame(density(posterior_effects$Hammer))
HammerPCi = ci(posterior_effects$Hammer, ci= .89)

MagnetP= as.data.frame(density(posterior_effects$Magnet))
MagnetPCi = ci(posterior_effects$Magnet, ci= .89)

SpoonP = as.data.frame(density(posterior_effects$Spoon))
SpoonPCi = ci(posterior_effects$Spoon, ci= .89)

BrushP = as.data.frame(density(posterior_effects$Brush))
BrushPCi = ci(posterior_effects$Brush, ci= .89)



## Plot --------------------------------------------------------------------

H  = ggplot(Hammer, aes(x = x,y = y)) +
  geom_area( fill = "#D95F02", alpha= 0.3) +  # Background area (y2 equivalent)
  geom_area(data = Hammer %>% filter(x >= HammerCi$CI_low & x <= HammerCi$CI_high),
            fill = "#D95F02", alpha = 1) +
  
  geom_area(data = HammerP, fill = "#31688e", alpha= 0.3)+
  geom_area(data = HammerP %>% filter(x >= HammerPCi$CI_low & x <= HammerPCi$CI_high),
            fill = "#31688e", alpha = 1)+
  
  geom_hline(yintercept = 0, lwd=1)+
  coord_cartesian(xlim = c(-1,12), ylim = c(-0.1, 3.8)) +
  theme_bw(base_size = 35)+
  labs(y = '', x = '')



M = ggplot(Magnet, aes(x = x, y = y)) +
  geom_area(fill = "#D95F02", alpha= 0.3) +  # Background area
  geom_area(data = Magnet %>% filter(x >= MagnetCi$CI_low & x <= MagnetCi$CI_high),
            fill = "#D95F02", alpha = 1) +
  
  geom_area(data = MagnetP, fill = "#fde725", alpha= 0.3) +  # Overlay area
  geom_area(data = MagnetP %>% filter(x >= MagnetPCi$CI_low & x <= MagnetPCi$CI_high),
            fill = "#fde725", alpha = 1) +
  
  geom_hline(yintercept = 0, lwd=1) +
  coord_cartesian(xlim = c(-1,12), ylim = c(-0.1, 3.8)) +
  theme_bw(base_size = 35) +
  labs(y = '', x = 'Estimated beta coefficient')


S = ggplot(Spoon, aes(x = x, y = y)) +
  geom_area(fill = "#D95F02", alpha= 0.3) +  # Background area
  geom_area(data = Spoon %>% filter(x >= SpoonCi$CI_low & x <= SpoonCi$CI_high),
            fill = "#D95F02", alpha = 1) +
  
  geom_area(data = SpoonP, fill = "#440154", alpha= 0.3) +  # Overlay area
  geom_area(data = SpoonP %>% filter(x >= SpoonPCi$CI_low & x <= SpoonPCi$CI_high),
            fill = "#440154", alpha = 1)+
  
  geom_hline(yintercept = 0, lwd=1) +
  coord_cartesian(xlim = c(-1,12), ylim = c(-0.1, 3.8)) +
  theme_bw(base_size = 35) +
  labs(y = 'Density', x = '')


B = ggplot(Brush, aes(x = x, y = y)) +
  geom_area(fill = "#D95F02", alpha= 0.3) +  # Background area
  geom_area(data = Brush %>% filter(x >= BrushCi$CI_low & x <= BrushCi$CI_high),
            fill = "#D95F02", alpha = 1) +
  
  geom_area(data = BrushP, fill = "#35b779", alpha= 0.3) +  # Overlay area
  geom_area(data = BrushP %>% filter(x >= BrushPCi$CI_low & x <= BrushPCi$CI_high),
            fill = "#35b779", alpha = 1) +
  
  geom_hline(yintercept = 0, lwd=1) +
  coord_cartesian(xlim = c(-1,12), ylim = c(-0.1, 3.8)) +
  theme_bw(base_size = 35) +
  labs(y = 'Density', x = 'Estimated beta coefficient')

combined_plot <- (S | H) / 
  (B | M) +
  plot_layout(guides = "collect")

combined_plot
ggsave('.\\Results\\Plots\\SimulationCoef.svg',
       width = 20, height= 16, dpi=300)


### Combination plot -------------------------------------------------------------

Prediction_plot / combined_plot +
  plot_annotation(tag_levels = list(c('A', 'B', '', '')))

ggsave('.\\Results\\Plots\\CombinationSupplementary.svg',
       width = 18, height= 17, dpi=300)
