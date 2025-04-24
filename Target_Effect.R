
library(brms)
library(tidyverse)
library(easystats)
library(patchwork)
library(emmeans)



# Setting and paths -------------------------------------------------------

# Uncomment and set your working directory if needed
# setwd("path/to/your/directory")

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


Est_mod2 = estimate_means(mod, by= c('Trial_tool', 'Trial_tool_direction'), ci =.89)
Est_mod2$Trial_tool_direction = factor(Est_mod2$Trial_tool_direction, 
                                       levels = c("l", "r"), 
                                       labels = c("Handle-left", "Handle-right"))
custom_colors2 <- c(
  "Brush"  = "#35b779",
  "Spoon"  = "#440154"
)

MDir2 = Est_mod2 %>% 
  ggplot(aes(x = Trial_tool_direction, y  = Probability  , color = Trial_tool ))+
  geom_point(size = 5, position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), position = position_dodge(width = 0.5), width =.5, lwd = 1.6)+
  theme_bw(base_size = 20)+
  labs(x = '', color = 'Tool', y = 'Estimated probability')+
  scale_color_manual(values = custom_colors2)
MDir2
ggsave('.\\Results\\Plots\\SelfOther_Direction.svg',
       height = 10, width = 12, dpi = 300)


MDir1 =  readRDS('.\\Results\\Plots\\ToolDirection.rds' )

MDir1 = MDir1 + labs(color = '') +  theme(legend.position = 'bottom') + ylim(0.3, 0.85)
MDir2 = MDir2 + labs(color = '') +  theme(legend.position = 'bottom',
                                          axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),         
                                          axis.title.y = element_blank()) + ylim(0.3, 0.85)


MDir1 + MDir2 +
  plot_annotation(tag_levels = list(c('A', 'B'))) &
  theme(plot.tag = element_text(size = 30))

ggsave('.\\Results\\Plots\\SelfOther_Combo.svg',
       height = 8, width = 16, dpi = 300)

 
# Plot main effect --------------------------------------------------------
gc()
 
# Calculate marginal means
Pred_tool <- emmeans(mod, ~ Trial_tool * Trial_target * AgeSt ,
             at = list(AgeSt = seq(-2, 2, 0.5)))

Pred_tool = as.data.frame(summary(Pred_tool, type = "response", level = 0.89))
Pred_tool$Age = unstandardise(Pred_tool$AgeSt, reference = df$Age)
 

ggplot(Pred_tool, aes(x = AgeSt, y = response, color = Trial_target, fill = Trial_target))+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, color = 'transparent')+
  geom_line()+
  facet_wrap(~Trial_tool)+
  theme_bw(base_size = 20)



## Set the color for each plot
my_Spoon = c(Self = "#440154", Other = "#5a4e65")
Main_Spoon <- Pred_tool %>% 
  filter(Trial_tool == 'Spoon') %>% 
  ggplot(aes(x = Age, y = response, color = Trial_target, fill = Trial_target, linetype = Trial_target)) +
  geom_line(lwd = 2.2) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), color = 'transparent', alpha = 0.4) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_color_manual(values = my_Spoon) +
  scale_fill_manual(values = my_Spoon) +
  scale_linetype_manual(values = c("Self" = "solid", "Other" = "dashed")) +  # Correct linetype mapping
  
  theme_minimal(base_size = 35) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(3, "cm")) +  # Increase legend key width for better visibility
  labs(x = 'Age (weeks)', y = 'Estimated probability', fill = "", color = "", linetype = "", title = 'Spoon') +
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2', '0.4', '0.6', '0.8', 'Adaptive\nGrasp')) +
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(38, 76)) +
  theme(legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(3, "cm"))

Main_Spoon


my_Brush = c(Self = "#35b779", Other = "#7e9980")
Main_Brush = Pred_tool %>% 
  filter(Trial_tool == 'Brush') %>% 
  ggplot(aes(x = Age, y = response, color = Trial_target, linetype = Trial_target)) +
  geom_line(lwd = 2.2) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD, fill = Trial_target), color = 'transparent', alpha = 0.4) +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_color_manual(values = my_Brush) +
  scale_fill_manual(values = my_Brush) +
  scale_linetype_manual(values = c("Self" = "solid", "Other" = "dashed")) +
  
  theme_minimal(base_size = 35) + 
  labs(x = 'Age (weeks)', y = 'Estimated probability', fill = "", color = "", linetype = "", title = 'Brush') +
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2', '0.4', '0.6', '0.8', 'Adaptive\nGrasp')) +
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(38, 76)) +
  theme(legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(3, "cm"))  # Increase the legend key width
Main_Brush

Main_Spoon + Main_Brush
ggsave('.\\Results\\Plots\\SelfOtherDouble.svg',
       height = 25, width = 35, dpi = 300)



# Plot Individual level ---------------------------------------------------
gc()

# Calculate marginal means
Pre_Subject_tool <- emmeans(mod, ~ Trial_tool * Trial_target * AgeSt | ID_id_num,
                     at = list(AgeSt = seq(-2, 2, 0.5)),
                     re_formula = NULL)

Pre_Subject_tool = as.data.frame(summary(Pre_Subject_tool, type = "response", level = 0.89))
Pre_Subject_tool$Age = unstandardise(Pre_Subject_tool$AgeSt, reference = df$Age)


## Plot
SS = colorRampPalette(c("#f2a9ff","#440154"))(9)
Self_Spoon = Pre_Subject_tool %>%
  filter(Trial_target == 'Self', Trial_tool == 'Spoon')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), color = 'transparent', alpha = 0.2)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(weeks)', y = 'Estimated probability')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none')+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = SS )+
  scale_fill_manual(values  = SS)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(38, 76))


SO = colorRampPalette(c("#ADA8B6", "#5a4e65"))(9)
Other_Spoon = Pre_Subject_tool %>%
  filter(Trial_target == 'Other', Trial_tool == 'Spoon')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), color = 'transparent', alpha = 0.2)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(weeks)', y = '')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = SO )+
  scale_fill_manual(values  = SO)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(38, 76))



BS = colorRampPalette(c("#35b779","#455335"))(9)
Self_Brush = Pre_Subject_tool %>%
  filter(Trial_target == 'Self', Trial_tool == 'Brush')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), color = 'transparent', alpha = 0.2)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(weeks)', y = '')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = BS )+
  scale_fill_manual(values  = BS)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(38, 76))


BO = colorRampPalette(c("#ADA8B6", "#7e9980"))(9)
Other_Brush = Pre_Subject_tool %>%
  filter(Trial_target == 'Other', Trial_tool == 'Brush')%>%
  ggplot( aes(x = Age, y = response, color = ID_id_num, fill = ID_id_num))+
  geom_line(lwd = 1.9)+
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), color = 'transparent', alpha = 0.2)+
  
  geom_hline(yintercept = 0.5, linetype = 'dashed')+
  labs(x = 'Age(weeks)', y = '')+
  theme_minimal(base_size = 40)+ 
  theme(legend.position = 'none',
        axis.text.y = element_blank())+
  scale_y_continuous(breaks = seq(0.2, 1, 0.2), labels = c('0.2','0.4','0.6','0.8','Adaptive\nGrasp'))+
  scale_color_manual(values  = BO )+
  scale_fill_manual(values  = BO)+
  coord_cartesian(ylim = c(0.2, 1.05), xlim = c(38, 76))



Top = Main_Spoon + Main_Brush
Bottom =(Self_Spoon + Other_Spoon + Self_Brush + Other_Brush) +
  plot_layout(ncol = 4)

Top / Bottom +
  plot_layout(heights = c(3, 1.5))&
  theme(plot.tag = element_text(size = 50))&
  plot_annotation(tag_levels = list(c('A', 'B', 'C', 'D','E','F')))


ggsave('.\\Results\\Plots\\SelfOtherCombo.svg',
       height = 30, width = 30, dpi = 300)
