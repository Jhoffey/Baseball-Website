library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(caret)
library(xgboost)
library(purrr)
library(pROC)
# Load Data
df <- read_csv("Copy of Full_College_TM.csv")
new_df <- read_csv("2025Season_CSV.csv")
write_csv(new_df, "C:/Users/jason/OneDrive/Documents/DiamondCloud2/2025Season_CSV.csv")

# Select relevant columns
dft <- df %>% select(Date, PitchNo, Pitcher, Balls, Strikes, PitcherTeam, Batter, 
                     TaggedPitchType, PitchCall, PlayResult, RelSpeed, SpinRate, 
                     RelHeight, RelSide, Extension, InducedVertBreak, HorzBreak, 
                     VertApprAngle, HorzApprAngle)

new_dft <- new_df %>% select(Date, PitchNo, Balls, Strikes, Pitcher, PitcherTeam, Batter, 
                             TaggedPitchType, PitchCall, PlayResult, RelSpeed, SpinRate, 
                             RelHeight, RelSide, Extension, InducedVertBreak, HorzBreak, 
                             VertApprAngle, HorzApprAngle)%>%
  mutate(
    PitchCall = ifelse(PitchCall == "","Undefined", PitchCall),
    PlayResult = ifelse(PlayResult == "", "Undefined", PlayResult)
  )

dft2 <- bind_rows(dft, new_dft) %>%
  mutate(HorzBreak = abs(HorzBreak),
         RelSide = abs(RelSide),
         DiffBreak = abs(InducedVertBreak - HorzBreak))

# Run Value Dictionary
run_value_dict <- tibble(
  Event = c("StrikeSwinging", "StrikeCalled", "Foul", "FoulBall", "BallCalled", 
            "HitByPitch", "Walk", "HomeRun", "Homerun", "Single", "Double", "Triple", 
            "Out", "StrikeoutSwinging", "StrikeoutLooking", "Strikeout", "FieldersChoice", 
            "Error", "Sacrifice"),
  RunValue = c(0.046, 0.046, 0.046, 0.046, -0.052, -0.390, -0.222, -1.377, -1.377, 
               -0.469, -0.776, -1.051, 0.247, 0.247, 0.247, 0.247, 0.247, 0.00, 0.00)
)
RV <- dft2 %>%
  left_join(run_value_dict, by = c("PitchCall" = "Event")) %>%
  rename(RunValue_PitchCall = RunValue) %>%
  left_join(run_value_dict, by = c("PlayResult" = "Event")) %>%
  rename(RunValue_PlayResult = RunValue) %>%
  mutate(RunValue = coalesce(RunValue_PitchCall, RunValue_PlayResult, 0)) %>%
  select(-RunValue_PitchCall, -RunValue_PlayResult)

# Separate by Pitch Type
# Standardize TaggedPitchType Labels
RV <- RV %>%
  mutate(TaggedPitchType = case_when(
    TaggedPitchType %in% c('Fastball', 'Sinker', 'TwoSeamFastBall', 'FourSeamFastBall', 'OneSeamFastBall') ~ "Fastball",
    TaggedPitchType %in% c('Changeup', 'ChangeUp') ~ "Changeup",
    TRUE ~ TaggedPitchType  # Keep other pitch types unchanged
  ))

# Separate by Pitch Type with Updated Labels
FBStuff <- RV %>% filter(TaggedPitchType == "Fastball") %>% na.omit()
CHStuff <- RV %>% filter(TaggedPitchType == "Changeup") %>% na.omit()
SLStuff <- RV %>% filter(TaggedPitchType == "Slider") %>% na.omit()
CRVStuff <- RV %>% filter(TaggedPitchType == "Curveball") %>% na.omit()
CTStuff <- RV %>% filter(TaggedPitchType == "Cutter") %>% na.omit()


# Prepare Data Function (Without Feature Weighting)
prepare_data <- function(data, pitch_type) {
  feature_list <- c("RelSpeed", "SpinRate", "DiffBreak", "RelHeight", "RelSide", "Extension")
  
  # Select features without applying weights
  selected_features <- data %>%
    select(all_of(feature_list))
  
  target <- data$RunValue
  
  dtrain <- xgb.DMatrix(data = as.matrix(selected_features), label = target)
  return(dtrain)
}
prepare_data2 <- function(data, pitch_type) {
  feature_list <- c("RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak","RelHeight", "RelSide", "Extension")
  
  # Select features without applying weights
  selected_features <- data %>%
    select(all_of(feature_list))
  
  target <- data$RunValue
  
  dtrain <- xgb.DMatrix(data = as.matrix(selected_features), label = target)
  return(dtrain)
}


# Train XGBoost Model with Cross-Validation
train_xgb_model_cv <- function(dtrain, pitch_type) {
params_list <- list(
  "Fastball" = list(
    eta = 0.010, max_depth = 3, subsample = 0.85, colsample_bytree = 0.8, 
    min_child_weight = 12, gamma = 0.4, lambda = 1.5, alpha = 1.2
  ),
  "Changeup" = list(
    eta = 0.012, max_depth = 3, subsample = 0.75, colsample_bytree = 0.70, 
    min_child_weight = 15, gamma = 0.6, lambda = 2.0, alpha = 1.5
  ),
  "Slider" = list(
    eta = 0.016,         # Slightly lower learning rate to stabilize projections
    max_depth = 4,       # Reduce depth to balance Stuff+ outputs
    subsample = 0.80,    # Reduce sample size for more diverse training
    colsample_bytree = 0.75,  # Lower feature sampling to prevent bias
    min_child_weight = 12,    # Require larger node samples to avoid overfitting to low-end values
    gamma = 0.65,        # Increase pruning to balance projection distribution
    lambda = 2.0,        # Increase L2 regularization for stability
    alpha = 1.2          # Increase L1 regularization to reduce over-reliance on bottom-heavy trends
  ),
  "Curveball" = list(
    eta = 0.015,            # Increase learning rate to adapt more dynamically
    max_depth = 5,          # Allow deeper splits for more diverse predictions
    subsample = 0.85,       # Increase randomness in training
    colsample_bytree = 0.80,# Use more features per tree to introduce variation
    min_child_weight = 12,  # Lower to allow more splits and variance
    gamma = 0.4,            # Reduce pruning to retain more splits
    lambda = 1.5,           # Reduce L2 regularization to allow more fluctuation
    alpha = 1.2             # Lower L1 regularization for more movement
  )
  ,
  "Cutter" = list(
    eta = 0.015,            # Slightly higher learning rate to capture pitch variations better
    max_depth = 5,          # Allow deeper tree splits for fine-tuned movement evaluation
    subsample = 0.85,       # Increase randomness to ensure model doesn't overfit to bad Cutters
    colsample_bytree = 0.92,# Stronger feature selection to emphasize SpinRate & HorzBreak
    min_child_weight = 10,  # Lower threshold to allow smaller movement-based splits
    gamma = 0.2,            # Reduce pruning to retain meaningful variations
    lambda = 1.2,           # Lower L2 regularization to let better Cutters stand out
    alpha = 1.0             # Reduce L1 regularization to prevent extreme Stuff+ penalties
  )
  
  
  
)
  
  params <- params_list[[pitch_type]]
  
  # Cross-validation setup
  cv_results <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,  # Large number to allow early stopping
    nfold = 5,  # 5-fold cross-validation
    early_stopping_rounds = 20,
    metrics = "rmse",
    verbose = TRUE
  )
  
  # Best number of rounds from CV
  best_nrounds <- cv_results$best_iteration
  
  # Train the final model
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = TRUE
  )
  
  return(model)
}

# Prepare Data for Each Pitch Type
FB_data <- prepare_data(FBStuff, "Fastball")
CH_data <- prepare_data2(CHStuff, "Changeup")
SL_data <- prepare_data2(SLStuff, "Slider")
CRV_data <- prepare_data2(CRVStuff, "Curveball")
CT_data <- prepare_data2(CTStuff, "Cutter")

# Train Models Using Cross-Validation
FB_xgb_model <- train_xgb_model_cv(FB_data, "Fastball")
CH_xgb_model <- train_xgb_model_cv(CH_data, "Changeup")
SL_xgb_model <- train_xgb_model_cv(SL_data, "Slider")
CRV_xgb_model <- train_xgb_model_cv(CRV_data, "Curveball")
CT_xgb_model <- train_xgb_model_cv(CT_data, "Cutter")

# Normalize Stuff+ Scores & Remove Outliers
normalize_predictions <- function(data, model, pitch_type) {
  data$predicted_whiff <- predict(model, as.matrix(select(data, RelSpeed, SpinRate, DiffBreak, RelHeight, RelSide, Extension)))
  
  # Compute mean & SD of predicted whiffs
  mean_stuff <- mean(data$predicted_whiff, na.rm = TRUE)
  sd_stuff <- sd(data$predicted_whiff, na.rm = TRUE)
  
  # Apply robust scaling for Stuff+
  data <- data %>%
    mutate(
      Stuff = (((predicted_whiff - mean_stuff) / (sd_stuff + 1e-6)) * 10) + 100
    )%>% filter(Stuff >65)
  

  return(data)
}
normalize_predictions2 <- function(data, model, pitch_type) {
  data$predicted_whiff <- predict(model, as.matrix(select(data, RelSpeed, SpinRate,InducedVertBreak,HorzBreak, RelHeight, RelSide, Extension)))
  
  # Compute mean & SD of predicted whiffs
  mean_stuff <- mean(data$predicted_whiff, na.rm = TRUE)
  sd_stuff <- sd(data$predicted_whiff, na.rm = TRUE)
  
  # Apply robust scaling for Stuff+
  data <- data %>%
    mutate(
      Stuff = (((predicted_whiff - mean_stuff) / (sd_stuff + 1e-6)) * 10) + 100
    ) %>% filter(Stuff >65)
  
  
  return(data)
}

# Apply Normalization
FBStuff <- normalize_predictions(FBStuff, FB_xgb_model, "Fastball")
CHStuff <- normalize_predictions2(CHStuff, CH_xgb_model, "Changeup")
SLStuff <- normalize_predictions2(SLStuff, SL_xgb_model, "Slider")
CRVStuff <- normalize_predictions2(CRVStuff, CRV_xgb_model, "Curveball")
CTStuff <- normalize_predictions2(CTStuff, CT_xgb_model, "Cutter")

# Save Processed Data
write_csv(FBStuff, "C:/Users/jason/OneDrive/Documents/FBStuff.csv")
write_csv(CHStuff, "C:/Users/jason/OneDrive/Documents/CHStuff.csv")
write_csv(SLStuff, "C:/Users/jason/OneDrive/Documents/SLStuff.csv")
write_csv(CRVStuff, "C:/Users/jason/OneDrive/Documents/CRVStuff.csv")
write_csv(CTStuff, "C:/Users/jason/OneDrive/Documents/CTStuff.csv")

Stuff_Combined <- bind_rows(FBStuff, CHStuff, SLStuff, CRVStuff, CTStuff)
write_csv(Stuff_Combined, "C:/Users/jason/OneDrive/Documents/Combined_Stuff.csv")


Staff <- c("Devon Laguinto", "Billy Gregory", "Gray Bailey", "Connor Mattison", 
           "Grant Richardson", "Walt Quinn", "Ben Smith", "Walter Quinn", "Isaac Lyon", 
           "Barrett Skaugrud", "Benjamin Smith", "Josh Wakefield", "Chance Key", 
           "Justin Hanson", "Gray Bailey", "Billy Gregory", "Ross Clark", "Jace Smith", 
           "Cam Cunnings", "Elijah Higginbottom", "Garrett Ahern", "Zach Hauser", 
           "Jace Behnke", "Cayden Collins", "Dillon Orr", "Alec Ammerman", 
           "Shawn Barros", "Gunnar Penzkover")

post_stuff_plus <- Stuff_Combined %>% filter(Pitcher %in% Staff)
write_csv(post_stuff_plus, file = "C:/Users/jason/OneDrive/Documents/poststuffplus.csv")
