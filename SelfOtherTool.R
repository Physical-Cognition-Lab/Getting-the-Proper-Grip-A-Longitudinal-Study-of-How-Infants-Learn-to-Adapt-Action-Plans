
library(brms)
library(tidyverse)
library(easystats)
library(patchwork)

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
df$AgeSt = standardize(df$Age)



# Linear model -------------------------------------------------------------------

Priors = prior(normal(0, 10), class = b)

tool_mod = brm(AdaptiveGrasp ~ Trial_tool*Trial_target*AgeSt + (1 + AgeSt | ID_id_num),
               data = df, family = bernoulli(), prior = Priors,
               chains = 4, iter = 8000, warmup = 6000, cores = 4,
               control = list(adapt_delta = 0.99, max_treedepth = 15),
               file = '.\\Analysis\\R\\Test_Tommaso\\ModelResults\\LinearModelFinalSelfOtherSt_inter')

tool_p = parameters(tool_mod, ci =.89)
tool_c = estimate_contrasts(tool_mod, contrast = c('Trial_target','Trial_tool'), ci =.89)
tool_s = estimate_slopes(tool_mod, trend = 'AgeSt', by=c('Trial_target','Trial_tool'),ci = .89)


# Plot main effect --------------------------------------------------------
gc()

Pred_tool = get_datagrid(tool_mod, length = 100, by = c('Trial_tool','Trial_target', 'AgeSt'),include_response=T)
Pred_tool = bind_cols(Pred_tool, as.data.frame(get_predicted(tool_mod, Pred_tool, ci = T)))
Pred_tool = Pred_tool %>%
  select(1:8) %>% 
  mutate(Age = unstandardize(AgeSt, reference = df$Age ))

Pred_tool$Trial_target = factor(Pred_tool$Trial_target, levels = c('Self', 'Other'))


## Set the color for each plot
my_Spoon = c(Self = "#440154", Other = "#5a4e65")
Main_Spoon = Pred_tool %>% 
  filter(Trial_tool=='Spoon') %>% 
  ggplot( aes(x = Age, y = Predicted, color = Trial_target, fill = Trial_target))+
  geom_line(lwd = 2, )+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  scale_color_manual(values = my_Spoon) +
  scale_fill_manual(values = my_Spoon)+
  
  theme_minimal(base_size = 35)+ 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))+
  labs(x = 'Age(days)', y = 'Estimated probability',fill="", color= "", title = 'Spoon')+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  guides(colour = "none", point = 'none')+
  coord_cartesian(ylim = c(0.4, 1.05), xlim = c(277,562))

my_Brush = c(Self = "#35b779", Other = "#7e9980")
Main_Brush = Pred_tool %>% 
  filter(Trial_tool=='Brush') %>% 
  ggplot( aes(x = Age, y = Predicted, color = Trial_target, fill = Trial_target))+
  geom_line(lwd = 2, )+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  scale_color_manual(values = my_Brush) +
  scale_fill_manual(values = my_Brush)+
  
  theme_minimal(base_size = 35)+ 
  labs(x = 'Age(days)', y = 'Estimated probability',fill="", color= "",title = 'Brush')+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  guides(colour = "none", point = 'none')+
  coord_cartesian(ylim = c(0.4, 1.05), xlim = c(277,562))+
  theme(legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  

Main_Spoon + Main_Brush
ggsave('.\\Manuscript\\Figures\\SelfOtherDouble.svg',
       height = 25, width = 35, dpi = 300)



# Plot Individual level ---------------------------------------------------
gc()

Pre_Subject_tool = get_datagrid(tool_mod, length = 100, by = c('Trial_tool','Trial_target' , 'AgeSt', 'ID_id_num'), include_random = T,include_response=T)
Pre_Subject_tool = bind_cols(Pre_Subject_tool, as.data.frame(get_predicted(tool_mod, Pre_Subject_tool, ci = T)))
Pre_Subject_tool = Pre_Subject_tool %>%
  select(1:8) %>% 
  mutate(Age = unstandardize(AgeSt, reference = df$Age ))


## Plot
SS = colorRampPalette(c("#f2a9ff","#440154"))(9)
Self_Spoon = Pre_Subject_tool %>%
  filter(Trial_target == 'Self', Trial_tool == 'Spoon')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(days)', y = 'Estimated probability')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none')+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = SS )+
  scale_fill_manual(values  = SS)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))


SO = colorRampPalette(c("#ADA8B6", "#5a4e65"))(9)
Other_Spoon = Pre_Subject_tool %>%
  filter(Trial_target == 'Other', Trial_tool == 'Spoon')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(days)', y = '')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = SO )+
  scale_fill_manual(values  = SO)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))


BS = colorRampPalette(c("#35b779","#455335"))(9)
Self_Brush = Pre_Subject_tool %>%
  filter(Trial_target == 'Self', Trial_tool == 'Brush')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(days)', y = '')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = BS )+
  scale_fill_manual(values  = BS)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))


BO = colorRampPalette(c("#ADA8B6", "#7e9980"))(9)
Other_Brush = Pre_Subject_tool %>%
  filter(Trial_target == 'Other', Trial_tool == 'Brush')%>%
  ggplot( aes(x = Age, y = Predicted, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.5)+
  geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE), color = 'transparent', alpha = 0.4)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(days)', y = '')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = BO )+
  scale_fill_manual(values  = BO)+
  coord_cartesian(ylim = c(0.45, 1.05), xlim = c(277,562))



Top = Main_Spoon + Main_Brush
Bottom =(Self_Spoon + Other_Spoon + Self_Brush + Other_Brush) +
  plot_layout(ncol = 4)

Top / Bottom +
  plot_layout(heights = c(3, 1.5))&
  theme(plot.tag = element_text(size = 50))&
  plot_annotation(tag_levels = list(c('A', 'B', 'C', 'D','E','F')))


ggsave('.\\Manuscript\\Figures\\SelfOtherCombo.svg',
       height = 30, width = 30, dpi = 300)

