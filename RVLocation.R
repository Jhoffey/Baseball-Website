# Load required libraries
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(caret)
library(xgboost)
library(purrr)
library(pROC)
library(xgboost)
library(caret)
library(dplyr)
df <- read_csv("Copy of Full_College_TM.csv")
new_df <- read_csv("2025Season_CSV.csv")
dft <- df %>% select(Date,PitchNo,Pitcher, Balls, Strikes, PitcherTeam, Batter,TaggedPitchType, PitchCall, PlayResult,PlateLocHeight, PlateLocSide, BatterSide, PitcherThrows)
new_dft <- new_df %>% select(Date,PitchNo,Balls, Strikes, Pitcher, PitcherTeam, Batter,TaggedPitchType, PitchCall, PlayResult,  PlateLocHeight, PlateLocSide, BatterSide, PitcherThrows)
dft2 <- bind_rows(dft, new_dft) 
dft2 %>% distinct(Pitcher) 

# Data transformations
dft2 <- dft2 %>% select (Date,PitchNo,Balls, Strikes, Pitcher, PitcherTeam, Batter,TaggedPitchType, PitchCall, PlayResult ,PlateLocHeight, PlateLocSide, BatterSide, PitcherThrows)
run_value_dict <- tibble(
  Event = c("StrikeSwinging", "StrikeCalled", "Foul", "FoulBall", "BallCalled", 
            "HitByPitch", "Walk", "HomeRun", "Single", "Double", "Triple", 
            "Out", "StrikeoutSwinging", "StrikeoutLooking", "FieldersChoice", 
            "Error", "Sacrifice"),
  RunValue = c(0.15, 0.12, 0.10, 0.10, -0.15, 
               -0.30, -0.30, -1.40, -0.45, -0.75, -1.00, 
               0.25, 0.25, 0.25, 0.25, 0.00, 0.00)
)

RV <- dft2 %>%
  left_join(run_value_dict, by = c("PitchCall" = "Event")) %>%
  rename(RunValue_PitchCall = RunValue) %>%
  left_join(run_value_dict, by = c("PlayResult" = "Event")) %>%
  rename(RunValue_PlayResult = RunValue) %>%
  mutate(RunValue = coalesce(RunValue_PitchCall, RunValue_PlayResult, 0)) %>%
  select(-RunValue_PitchCall, -RunValue_PlayResult)# Predictor and response variables
FBLoc <- RV %>% filter(TaggedPitchType %in% c('Fastball', 'Sinker', 'TwoSeamFastBall', 'FourSeamFastBall', 'OneSeamFastBall'))%>% na.omit()
# Filter datasets by pitch type
CHLoc <- RV %>% filter(TaggedPitchType %in% c('Changeup','ChangeUp'))%>% na.omit()
SLLoc <- RV %>% filter(TaggedPitchType == 'Slider')%>% na.omit()
CRVLoc <- RV %>% filter(TaggedPitchType == 'Curveball')%>% na.omit()
CTLoc <- RV %>% filter(TaggedPitchType == 'Cutter') %>% na.omit()
nrow(FBLoc)
# Filter datasets for left and right-handed batters
FBLoc_Left <- FBLoc %>% filter(BatterSide == "Left")
FBLoc_Right <- FBLoc %>% filter(BatterSide == "Right")
# Changeup
nrow(FBLoc_Right)
CHLoc_Left <- CHLoc %>% filter(BatterSide == "Left")
CHLoc_Right <- CHLoc %>% filter(BatterSide == "Right")

# Slider
SLLoc_Left <- SLLoc %>% filter(BatterSide == "Left")
SLLoc_Right <- SLLoc %>% filter(BatterSide == "Right")

# Curveball
CRVLoc_Left <- CRVLoc %>% filter(BatterSide == "Left")
CRVLoc_Right <- CRVLoc %>% filter(BatterSide == "Right")

# Cutter
CTLoc_Left <- CTLoc %>% filter(BatterSide == "Left")
CTLoc_Right <- CTLoc %>% filter(BatterSide == "Right")

prepare_data <- function(data) {
  X <- data %>% select(Pitcher, TaggedPitchType, PlateLocHeight, PlateLocSide)
  Y <- data$RunValue
  
  data_rf <- cbind(X, Y) %>%
    mutate(Y = as.factor(Y))
  
  set.seed(12345678)
  N <- nrow(data_rf)
  TrainingSize <- round(N * 0.7)
  TrainingCases <- sample(N, TrainingSize)
  Training <- data_rf[TrainingCases, ]
  Test <- data_rf[-TrainingCases, ]
  
  dtrain <- xgb.DMatrix(
    data = as.matrix(Training %>% select(-Pitcher, -Y, -TaggedPitchType)), 
    label = as.numeric(Training$Y) - 1
  )
  dtest <- xgb.DMatrix(
    data = as.matrix(Test %>% select(-Pitcher, -Y, -TaggedPitchType)), 
    label = as.numeric(Test$Y) - 1
  )
  
  list(dtrain = dtrain, dtest = dtest, Training = Training, Test = Test)
}
train_xgb_model<- function(dtrain, dtest, pitch_type) {
  params <- list(
    # **Core settings**
    booster = "gbtree",
    objective = "reg:squarederror",  
    eval_metric = "rmse",            
    tree_method = "hist",            
    
    # **Learning Rate & Boosting**
    eta = 0.1,                      # Slightly faster learning (fewer features)
    nrounds = 1500,                  # Fewer boosting rounds (smaller dataset)
    early_stopping_rounds = 15,      # Stops if no improvement in 15 rounds
    
    # **Tree Complexity**
    max_depth = 4,                   # Smaller trees (since we only have 2 features)
    min_child_weight = 12,           # Prevents overfitting (higher than for large dataset)
    
    # **Regularization (Prevents Overfitting)**
    lambda = 10,                     # Stronger L2 regularization
    alpha = 3,                       # Stronger L1 regularization
    gamma = 2,                       # More strict on allowing splits
    
    # **Feature Sampling (More Aggressive Due to Fewer Features)**
    subsample = 0.6,                 # Lower subsample since data is smaller
    colsample_bytree = 0.7,          # Limits feature selection (but we only have 2 features)
    
    # **Advanced Settings**
    max_leaves = 20,                 # Smaller trees since dataset is simpler
    grow_policy = "depthwise"         # Traditional depth-based tree growth
  )
  
  xgb.train(
    params = params,
    data = dtrain,
    nrounds = 1500,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 15,
    verbose = TRUE
  )
}


# Prepare datasets and train models
# Prepare Fastball dataset for model training
FB_data_Left <- prepare_data(FBLoc_Left)
FB_data_Right <- prepare_data(FBLoc_Right)
CH_data_Left <- prepare_data(CHLoc_Left)
CH_data_Right <- prepare_data(CHLoc_Right)
SL_data_Left <- prepare_data(SLLoc_Left)
SL_data_Right <- prepare_data(SLLoc_Right)
CRV_data_Left <- prepare_data(CRVLoc_Left)
CRV_data_Right <- prepare_data(CRVLoc_Right)
CT_data_Left <- prepare_data(CTLoc_Left)
CT_data_Right <- prepare_data(CTLoc_Right)

# Train models
# Train models for Fastballs
FB_xgb_model_Left <- train_xgb_model(FB_data_Left$dtrain, FB_data_Left$dtest, "Fastball_Left")
FB_xgb_model_Right <- train_xgb_model(FB_data_Right$dtrain, FB_data_Right$dtest, "Fastball_Right")
CH_xgb_model_Left <- train_xgb_model(CH_data_Left$dtrain, CH_data_Left$dtest, "Changeup_Left")
CH_xgb_model_Right <- train_xgb_model(CH_data_Right$dtrain, CH_data_Right$dtest, "Changeup_Right")
SL_xgb_model_Left <- train_xgb_model(SL_data_Left$dtrain, SL_data_Left$dtest, "Slider_Left")
SL_xgb_model_Right <- train_xgb_model(SL_data_Right$dtrain, SL_data_Right$dtest, "Slider_Right")
CRV_xgb_model_Left <- train_xgb_model(CRV_data_Left$dtrain, CRV_data_Left$dtest, "Curveball_Left")
CRV_xgb_model_Right <- train_xgb_model(CRV_data_Right$dtrain, CRV_data_Right$dtest, "Curveball_Right")
CT_xgb_model_Left <- train_xgb_model(CT_data_Left$dtrain, CT_data_Left$dtest, "Cutter_Left")
CT_xgb_model_Right <- train_xgb_model(CT_data_Right$dtrain, CT_data_Right$dtest, "Cutter_Right")
make_predictions <- function(model, data, mean_value, sd_value) {
  X_matrix <- as.matrix(data %>% select(PlateLocHeight, PlateLocSide))
  data$predicted_whiff <- predict(model, newdata = X_matrix)
  
  data <- data %>%
    mutate(Location = ((predicted_whiff - mean_value) / sd_value) * 10 + 100)
  
  return(data)
}

normalize_predictions <- function(data, model) {
  mean_pred <- mean(data$predicted_whiff, na.rm = TRUE)
  sd_pred <- sd(data$predicted_whiff, na.rm = TRUE)
  make_predictions(model, data, mean_pred, sd_pred)
}
FBLoc_Left <- normalize_predictions(FBLoc_Left, FB_xgb_model_Left)
FBLoc_Right <- normalize_predictions(FBLoc_Right, FB_xgb_model_Right)
CHLoc_Left <- normalize_predictions(CHLoc_Left, CH_xgb_model_Left)
CHLoc_Right <- normalize_predictions(CHLoc_Right, CH_xgb_model_Right)
SLLoc_Left <- normalize_predictions(SLLoc_Left, SL_xgb_model_Left)
SLLoc_Right <- normalize_predictions(SLLoc_Right, SL_xgb_model_Right)
CRVLoc_Left <- normalize_predictions(CRVLoc_Left, CRV_xgb_model_Left)
CRVLoc_Right <- normalize_predictions(CRVLoc_Right, CRV_xgb_model_Right)
CTLoc_Left <- normalize_predictions(CTLoc_Left, CT_xgb_model_Left)
CTLoc_Right <- normalize_predictions(CTLoc_Right, CT_xgb_model_Right)
write_csv(FBLoc_Left, file="C:/Users/jason/OneDrive/Documents/FBLoc_Left.csv")
write_csv(FBLoc_Right, file="C:/Users/jason/OneDrive/Documents/FBLoc_Right.csv")
write_csv(CHLoc_Left, file="C:/Users/jason/OneDrive/Documents/CHLoc_Left.csv")
write_csv(CHLoc_Right, file="C:/Users/jason/OneDrive/Documents/CHLoc_Right.csv")
write_csv(SLLoc_Left, file="C:/Users/jason/OneDrive/Documents/SLLoc_Left.csv")
write_csv(SLLoc_Right, file="C:/Users/jason/OneDrive/Documents/SLLoc_Right.csv")
write_csv(CRVLoc_Left, file="C:/Users/jason/OneDrive/Documents/CRVLoc_Left.csv")
write_csv(CRVLoc_Right, file="C:/Users/jason/OneDrive/Documents/CRVLoc_Right.csv")
write_csv(CTLoc_Left, file="C:/Users/jason/OneDrive/Documents/CTLoc_Left.csv")
write_csv(CTLoc_Right, file="C:/Users/jason/OneDrive/Documents/CTLoc_Right.csv")

Loc_Combined <- bind_rows(
  FBLoc_Left, FBLoc_Right,
  CHLoc_Left, CHLoc_Right,
  SLLoc_Left, SLLoc_Right,
  CRVLoc_Left, CRVLoc_Right,
  CTLoc_Left, CTLoc_Right
)
Staff <- c("Smith, Jace","Quinn, Walter","Mattison, Connor","Lyon, Isaac","Key, Chance","Skaugrud, Barrett","Clark, Ross","Cunnings, Cam","Ahern, Garrett","Gregory, Billy","Bailey, Gray","Quinn, Walter","Devon Laguinto","Devon Laguinto","Billy Gregory","Gray Bailey","Connor Mattison","Grant Richardson","Walt Quinn","Ben Smith","Walter Quinn","Isaac Lyon", "Barrett Skaugrud", "Benjamin Smith", "Josh Wakefield","Chance Key", "Justin Hanson", "Gray Bailey", "Billy Gregory", "Ross Clark", "Jace Smith", "Cam Cunnings", "Elijah Higginbottom", "Garrett Ahern", "Zach Hauser", "Jace Behnke", "Cayden Collins","Dillon Orr", "Alec Ammerman", "Shawn Barros", "Gunnar Penzkover")
post_location_plus <- Loc_Combined %>% filter(Pitcher %in% Staff )
write_csv(Loc_Combined, file="C:/Users/jason/OneDrive/Documents/All_PitchTypes_Locations.csv")
write_csv(post_location_plus, file="C:/Users/jason/OneDrive/Documents/DiamondCloud2/postlocplus.csv")
write_csv(post_location_plus, file="C:/Users/jason/OneDrive/Documents/postlocplus.csv")
combined_stuff_location <- bind_rows(post_stuff_plus,post_location_plus)
write_csv(combined_stuff_location, file= "C:/Users/jason/OneDrive/Documents/combined_stuff_location.csv")

