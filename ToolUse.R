

library(tidyverse)
library(easystats)
library(parallel)
library(gt)
library(patchwork)

library(brms)
library(tidybayes)

library(lme4)
library(lmerTest)


# Setting and paths -------------------------------------------------------

# Set working directory
setwd('C:\\Users\\tomma\\OneDrive - Birkbeck, University of London\\OriginsAdaptiveBehaviour_2023')

# Load data
db  =  read.csv('.\\Data\\PlanningInfants_Session_Data.csv')
db = db[!is.na(db$Grasp_onset), ]



# Data preparation --------------------------------------------------------

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



# Table information ------------------------------------------------------

# Remove file if exist
if (file.exists(".\\Manuscript\\ToolDirection.docx")) {
  file.remove(".\\Manuscript\\ToolDirection.docx")}

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
    columns = c(Hammer, Brush, Magnet, Spoon)
  ) %>% 
  gtsave(".\\Manuscript\\ToolDirection.docx")


# Remove file if exist
if (file.exists(".\\Manuscript\\SessionCounter.docx")) {
  file.remove(".\\Manuscript\\SessionCounter.docx")}

df %>%
  group_by(ID_id_num) %>%
  mutate(AgeWeeks = round(Age/7, 1)) %>%
  summarize(
    `First session` = min(AgeWeeks),
    `Last session` = max(AgeWeeks),
    Attended = n_distinct(AgeWeeks),
    Spoon = n_distinct(AgeWeeks[Trial_tool == 'Spoon']),
    Brush = n_distinct(AgeWeeks[Trial_tool == 'Brush']),
    Hammer = n_distinct(AgeWeeks[Trial_tool == 'Hammer']),
    Magnet = n_distinct(AgeWeeks[Trial_tool == 'Magnet'])
  ) %>% 
  gt() %>%
  cols_label(ID_id_num = "Id") %>%  # Renames the rowname column
  tab_spanner(
    label = "Age (weeks)",
    columns = c(`First session`, `Last session`)
  ) %>%
  tab_spanner(
    label = "# of sessions",
    columns = c(Attended, Spoon, Brush, Hammer, Magnet)
  ) %>% 
  gtsave(".\\Manuscript\\SessionCounter.docx")





# Bayesian model ----------------------------------------------------------

Priors = prior(normal(0, 10), class = b)

mod = brm(AdaptiveGrasp ~ Trial_tool*AgeSt + (1 + AgeSt | ID_id_num),
          data = df, family = bernoulli(), prior = Priors,
          chains = 4, iter = 8000, warmup = 6000, cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15),
          file = '.\\Analysis\\R\\Test_Tommaso\\ModelResults\\AgeSt')
summary(mod, prob=.89)
p = parameters::parameters(mod, ci =.89)

c = estimate_contrasts(mod, contrast = 'Trial_tool', ci = 0.89)
s = estimate_slopes(mod, trend = 'AgeSt', by = 'Trial_tool', ci = 0.89)



# Plot Main effect --------------------------------------------------------

Viz = visualisation_matrix(mod , by = c('Trial_tool', 'AgeSt'), length = 100)
Pred = bind_cols(Viz, as.data.frame(get_predicted(Viz, mod, ci = 0.89)))
Pred = Pred %>%
  select(1:8) %>% 
  mutate(Age = unstandardize(AgeSt, reference = df$Age ))

my_colors <- c(Spoon = "#440154", Hammer = "#31688e", Brush = "#35b779", Magnet = "#fde725")
Main = ggplot(Pred, aes(x = Age, y =  Predicted, color =  Trial_tool, fill = Trial_tool))+
  geom_line(lwd = 2)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), alpha = 0.4, color = 'transparent')+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors)+
  
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'bottom')+
  labs(x = 'Age(days)', y = 'Estimated probability',fill="", color= "")+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  guides(colour = "none", point = 'none')+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))

Main



# Plot individual level ---------------------------------------------------

VizS = get_datagrid(mod , by = c('Trial_tool', 'AgeSt', 'ID_id_num'), length = 100)
Pred_Subject = bind_cols(VizS, as.data.frame(get_predicted(VizS, mod, ci = 0.89)))

Pred_Subject = Pred_Subject %>%
  select(1:8) %>% 
  mutate(Age = unstandardize(AgeSt, reference = df$Age ))


## Spoon
S = colorRampPalette(c("#f2a9ff","#440154"))(9)
SPOON = Pred_Subject %>%
  filter(Trial_tool == 'Spoon')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = '', y = 'Estimated probability')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none',
        axis.text.x = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = S )+
  scale_fill_manual(values  = S)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))

## Hammer
H = colorRampPalette(c("#31688e", "#3a4856"))(9)
HAMMER = Pred_Subject %>%
  filter(Trial_tool == 'Hammer')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = '', y = '')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = H )+
  scale_fill_manual(values  = H)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))

## Brush
B = colorRampPalette(c("#35b779","#455335"))(9)
BRUSH = Pred_Subject %>%
  filter(Trial_tool == 'Brush')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = 'Age(days)', y = 'Estimated probability')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none')+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = B )+
  scale_fill_manual(values  = B)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))

## Magnet
M = colorRampPalette(c("#fde725","#ab9d68"))(9)
MAGNET = Pred_Subject %>%
  filter(Trial_tool == 'Magnet')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  geom_hline(yintercept = 0.5, linetype = 'dashed', lwd = 1.3)+
  
  labs(x = 'Age(days)', y = '')+
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = M )+
  scale_fill_manual(values  = M)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))


(Main / (SPOON + HAMMER) / (BRUSH + MAGNET))+
  plot_layout(heights = c(2.5, 1, 1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 50))

ggsave('.\\Manuscript\\Figures\\Tools.svg',
       height = 35, width = 25, dpi = 300)



# Prediction of spoon over age ------------------------------------------------------------

Age_to_calculate_over = standardize( seq(270, 600, 15), reference = df$Age)

# Create new data
MeansDraws <- expand.grid(AgeSt = Age_to_calculate_over,
                          Trial_tool = "Spoon",
                          ID_id_num = NA ) # assuming you need to specify an ID

# Add expected posterior draws to the new data
MeansDraws <- MeansDraws %>%
  add_epred_draws(object = mod, re_formula = NA, ndraws = 1000) # Adjust `ndraws` to control the number of posterior samples

Pred <-  MeansDraws %>%
  group_by(AgeSt) %>%
  summarize(
    Predicted = mean(.epred),
    Se = sd(.epred) / sqrt(n()),
    CI_low = tidybayes::qi(.epred, .width = .89)[1,1],
    CI_high = tidybayes::qi(.epred, .width = .89)[1,2]
  ) %>%
  mutate(
    Age = unstandardize(AgeSt, reference = df$Age),
    years = floor(Age / 365.25),
    months = floor((Age %% 365.25) / 30.44),
    days = floor((Age %% 365.25) %% 30.44),
    TotMonths = Age / 30.44
  )


# Unstandardize age
MeansDraws$Age = unstandardize(MeansDraws$AgeSt, reference = df$Age )

# Plot distributions for age
Prediction_plot = MeansDraws %>%
  ggplot(aes(x = Age, y = .epred)) +
  stat_halfeye( aes(fill = after_stat(level)),
                width= 15, .width = .89, adjust = 0.6)+
  
  # Add the highlighted rectangle
  annotate("rect", xmin = Pred[which(Pred$CI_low >= 0.75)[1],]$Age-5,
           xmax = max(Pred$Age)+22,
           
           ymin = 0.755, ymax = .95,
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
ggsave('.\\Manuscript\\Figures\\SpoonPrediction.svg',
       width = 20, height= 12, dpi=300)



## Simulation (add stepwise change) ---------------------------------------------------------

if (file.exists('.\\Analysis\\R\\Test_Tommaso\\ModelResults\\Simualtion.RDS')){
  
  Df = readRDS('.\\Analysis\\R\\Test_Tommaso\\ModelResults\\Simualtion.RDS')
  
} else{
  
  # Function to run the simulation
  run_simulation <- function(i, df) {
    db <- df
    db$Simulation <- NA
    
    db <- db %>%
      group_by(ID_id_num, Trial_tool) %>%
      mutate(
        tresh = sample(unique(AgeSt), 1),
        Simulation = if_else(AgeSt < tresh, 0, 1)
      ) %>%
      ungroup()
    
    modLm <- glmer(Simulation ~ Trial_tool * AgeSt + (1 + AgeSt | ID_id_num),
                   data = db, family = binomial())
    
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
  results <- parLapply(cl, 1:1000, run_simulation, df)
  
  # Stop the cluster
  stopCluster(cl)
  Df = bind_rows(results)
  saveRDS(Df, '.\\Analysis\\R\\Test_Tommaso\\ModelResults\\Simualtion.RDS')
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
ggsave('.\\Manuscript\\Figures\\SimulationCoef.svg',
       width = 20, height= 16, dpi=300)



### Combination plot -------------------------------------------------------------

Prediction_plot / combined_plot +
  plot_annotation(tag_levels = list(c('A', 'B', '', '')))

ggsave('.\\Manuscript\\Figures\\CombinationSupplementary.svg',
       width = 18, height= 17, dpi=300)
