# Import relevant libraries
library(Robyn)
library(reticulate)
library(readxl)
library(ggplot2)
library(reshape2)

# datasets
myData <- read.csv("data.csv")
# Summary of the data
summary(myData)
# Define Robyn object
robyn_object <- "MyRobyn.RDS"

# Dataset with 22 variables
# Dependent variable is sales
# Define as lists for use in Robyn
sales <- list('sales')
# Define control variables for list
control <- list('CPI','gas_dpg','store_count','markdown_1','markdown_2')
mediaSpend <- list('spend_direct_mail','spend_insert','spend_newspaper','spend_audio','spend_radio','spend_tv','spend_video','spend_social_media','spend_online','spend_SEM')

# check for missing values
missingData <- myData[rowSums(is.na(myData)) > 0,]
summary(missingData)
# No missing data in this dataset

# Exploratory data analysis to understand variables
# Define base variables
baseVars <- myData[, -c(1:6)]
# Build a correlation distribution and heatmap
cormat <- round(cor(baseVars),2)
head(cormat)
meltedCormat <- melt(cormat)
head(meltedCormat)
ggplot(data = meltedCormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
# Visualisation shows no clear signs of multicollinearity between any two variables selected

# If we needed to fill in missing values then typically I take a moving average of the two values before and after
# na_ma(x, k = 4, weighting = "simple", maxgap = Inf)

# Initiate robyn model
# Input initial model specifications
InputCollect <- robyn_inputs(
  dt_input = myData # raw data input
  , dt_holidays = dt_prophet_holidays # we use the holidays data provided through the Prophet package
  , date_var = "wk_strt_dt" # set the date variable
  , dep_var = "sales" # set the dependent variable
  , dep_var_type = "revenue"
  , prophet_vars = c("trend", "season", "holiday")
  , prophet_signs = c("default", "default", "positive") # defined holiday as only being positive
  , prophet_country = "US" # list found on GitHub site
  , context_vars = c('CPI','gas_dpg','store_count','markdown_1','markdown_2')
  , context_signs = c("default","negative","positive","positive","positive")
  , paid_media_vars = c('spend_direct_mail','spend_insert','spend_newspaper','spend_audio','spend_radio','spend_tv','spend_video','spend_social_media','spend_online','spend_SEM')
  , paid_media_signs = c("default","default","default","default","default","default","default","default","default","default")
  , paid_media_spends = c('spend_direct_mail','spend_insert','spend_newspaper','spend_audio','spend_radio','spend_tv','spend_video','spend_social_media','spend_online','spend_SEM')
  , adstock = c("geometric")
  , cores = 6
  , window_start = "2014-08-03"
  , window_end = "2018-07-29"
  , iterations = 2000
  , nevergrad_algo = "TwoPointsDE"
  , trials = 5
)

# Input hyperparameters separately
# As we're using the simple adstock hyperparameter model 
# We use the simple rule of thumb ranges 
hyperparameters <- list(
  spend_direct_mail_alphas = c(0.5,3)
  , spend_direct_mail_gammas = c(0.3,1)
  , spend_direct_mail_thetas = c(0.5,0.8)
  , spend_insert_alphas = c(0.5, 3)
  , spend_insert_gammas = c(0.6,1)
  , spend_insert_thetas = c(0.3, 0.6)
  , spend_newspaper_alphas = c(0.5,3)
  , spend_newspaper_gammas = c(0.5,1)
  , spend_newspaper_thetas = c(0.1,0.4)
  , spend_audio_alphas = c(0.5, 3)
  , spend_audio_gammas = c(0.3, 1)
  , spend_audio_thetas = c(0.5, 0.7)
  , spend_radio_alphas = c(0.5, 3)
  , spend_radio_gammas = c(0.3, 1)
  , spend_radio_thetas = c(0.1, 0.4)
  , spend_tv_alphas = c(0.5, 3)
  , spend_tv_gammas = c(0.5, 1)
  , spend_tv_thetas = c(0.5, 0.8)
  , spend_video_alphas = c(0.5, 3)
  , spend_video_gammas = c(0.6, 1)
  , spend_video_thetas = c(0.6, 0.8)
  , spend_social_media_alphas = c(0.5,3)
  , spend_social_media_gammas = c(0.7, 1)
  , spend_social_media_thetas = c(0.7, 1.0)
  , spend_online_alphas = c(0.5,3)
  , spend_online_gammas = c(0.6,1)
  , spend_online_thetas = c(0.6, 0.9)
  , spend_SEM_alphas = c(0.5,3)
  , spend_SEM_gammas = c(0.8,1)
  , spend_SEM_thetas = c(0.8, 1)
)

# Adjust inputs
InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)

# Can also so a calibration inputs
OutputCollect <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  , plot_folder = robyn_object # plots will be saved in the same folder as robyn_object
  , pareto_fronts = 3
  , plot_pareto = TRUE
)

# Helper plots
plot_adstock(TRUE)
plot_saturation(TRUE)

# New we analyse the pareto model solutions plot
# This is achieved by selecting the model that best relates to the business reality
OutputCollect$allSolutions
select_model <- "5_277_3"

# Save the Robyn model that most fits your solutions
robyn_save(robyn_object = robyn_object
           , select_mode = select_model
           , InputCollect = InputCollect
           , OutputCollect = OutputCollect)

# Capital budget allocation based on the selected model above
# Creates a dataframe 
OutputCollect$xDecompAgg[solID == select_model & !is.na(mean_spend)
                         , .(rn,mean_spend, mean_response, roi_mean
                             , total_spend, total_response=xDecompAgg, roi_total, solID)]

# Below displays how spend could be best allocated with spend minimum and maximum
# for each channel
# This is then simulated using robyn_allocator
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_historical_response"
  , channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7)
  , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)
)

AllocatorCollect$dt_optimOut

AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_response_expected_spend"
  , channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
  , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
  , expected_spend = 1000000 # Total spend to be simulated
  , expected_spend_days = 7 # Duration of expected_spend in days
)

AllocatorCollect$dt_optimOut

robyn_allocator(
  robyn_object = "MyRobyn.RDS",
  select_build = NULL,
  InputCollect = NULL,
  OutputCollect = NULL,
  select_model = NULL,
  optim_algo = "SLSQP_AUGLAG",
  scenario = "max_historical_response",
  expected_spend = NULL,
  expected_spend_days = NULL,
  channel_constr_low = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
  channel_constr_up = c(2,2,2,2,2,2,2,2,2,2),
  maxeval = 1e+05,
  constr_mode = "eq"
)

