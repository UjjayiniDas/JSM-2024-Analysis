# **********************
# Run State Space Models
# *****************************************************************************

# Function to run KFAS
Run_SSM_Models <- function(variable)
{
  
  # Data matrices
  Y_mat <- as.matrix(df_RRs[-1])
  X_mat <- as.matrix(100*df_Trust_Series[variable])
  
  
  # Assign N and T dimensions
  N    <- ncol(Y_mat) # Number of time series
  Tmax <- nrow(X_mat) # Length of the X vector (time points)
  
  
  # Initialize Z matrix
  Z <- array(0, dim = c(N, 2 + 2*N, Tmax))  # Z has dimensions (N, state vector length, T)
  
  # Fill in the Z matrix
  for (t in 1:Tmax) {
    for (i in 1:N) {
      Z[i, 1, t] <- 1            # Common intercept term 'a'
      Z[i, 2, t] <- X_mat[t]     # Common slope term 'b'
      Z[i, 2*i+1, t] <- 1        # Random intercept for series i ('a_i')
      Z[i, 2*i+2, t] <- X_mat[t] # Random slope for series i ('b_i')
    }
  }
  
  # Define state dimension (2 + 2N: intercept, slope, and random effects for each series)
  state_dim <- 2 + 2 * N
  
  # Assuming time-invariant intercepts and slopes:
  # Transition matrix (T): identity matrix
  T_matrix <- diag(state_dim)
  
  # Define R matrix: (identity matrix if state disturbances are independent)
  R_matrix <- diag(state_dim)
  
  # Initial variances for random intercepts and slopes (could start with small positive values)
  initial_variance_a <- 0.1  # Initial variance for random intercepts
  initial_variance_b <- 0.1  # Initial variance for random slopes
  
  # Define a blocked matrix for Q
  Q <- array(0, dim = c(state_dim, state_dim, Tmax))
  
  # Loop over time points to fill in the Q matrix for each time point
  for (t in 1:Tmax) {
    
    # Increasing variances over time (for simplicity, assume a linear increase)
    variance_a_t <- initial_variance_a * t
    variance_b_t <- initial_variance_b * t
    
    # Create the state covariance matrix for time t
    Q_t <- diag(0, state_dim)  # Start with a diagonal matrix
    
    # Common intercept and slope variances stay zero
    Q_t[3:(2+N), 3:(2+N)] <- diag(variance_a_t, N)  # Variances for random intercepts
    Q_t[(2+N+1):(2+2*N), (2+N+1):(2+2*N)] <- diag(variance_b_t, N)  # Variances for random slopes
    
    # Assign Q_t to the 3D array
    Q[, , t] <- Q_t
    
  }
  
  
  # Define H matrix: (diagonal matrix for observation noise)
  H_matrix <- diag(N)  # Adjust to the variances of your observed series
  
  # Initial state vector (zeros, or any reasonable starting values)
  initial_state <- rep(0, state_dim)
  
  # Initial covariance matrix for the state (diagonal or other appropriate guess)
  P1_matrix <- diag(state_dim)
  
  # Add random intercept and slope names for each time series
  state_names <- c("common_intercept", "common_slope",
                   paste(sort(rep(regressors, 2)), c("intercept", "slope"), sep = "_"))
  
  # Construct the custom state space model using SSMcustom
  model <- SSModel(
    Y_mat ~ -1 + SSMcustom(
      Z = Z,              # Observation matrix
      T = T_matrix,       # Transition matrix for the state
      R = R_matrix,       # State noise effect matrix
      Q = Q,       # State covariance matrix
      a1 = initial_state, # Initial state
      P1 = P1_matrix, # Initial state covariance matrix
      state_names = state_names      
    ),
    H = H_matrix)
  
  
  # Initial guesses for observation noise and random effect process noise
  initial_params <- rep(0.1, n_series*2)  # 5 for observation noise variances, 5 for random effect variances
  
  # Fit the model using maximum likelihood estimation
  fit <- fitSSM(model, 
                inits = rep(0.1, length(Q)), 
                method = "BFGS")
  
  # Check if fitting was successful and extract the fitted model
  if (!is.null(fit$model)) {
    fitted_model        <- fit$model
    kalman_results      <- KFS(fitted_model, filtering = "state")
    kalman_results$fit  <- fit
    kalman_results$name <- variable
  }
  
  return(kalman_results)
  
}

# Function to export the coeff's and SE's from KFAS
export_KF_results <- function(kalman_results)
{
  
  coef_vector <- kalman_results$att[nrow(kalman_results$att), ]
  se_vector <- kalman_results$V[, , dim(kalman_results$V)[3]] %>% 
    diag(.) %>% 
    sqrt(.)
  
  tibble(trust_var = kalman_results$name, 
         survey_name = names(coef_vector), coef_vector, se_vector)
  
}

# Run the state space models
List_of_SSM <- map(
  .x = names(df_Trust_Series)[-1],
  .f = Run_SSM_Models
)





# Function to add significance stars
add_sig_stars <- function(coef, std_err) {
  # Calculate the z-scores
  z_scores <- coef / std_err
  
  # Calculate the p-values (two-tailed test)
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  # Create a vector to store the significance stars
  stars <- rep("", length(p_values))
  
  # Assign stars based on p-values
  stars[p_values < 0.01] <- "***"   # 1% significance level
  stars[p_values >= 0.01 & p_values < 0.05] <- "**"   # 5% significance level
  stars[p_values >= 0.05 & p_values < 0.10] <- "*"    # 10% significance level
  
  # Combine coefficients with stars
  coef_with_stars <- paste0(round(coef, 3), stars)
  
  return(coef_with_stars)
}


# Collect output in a list
output_list <- map_dfr(List_of_SSM, export_KF_results) %>% 
  # Add significance stars
  mutate(coef_vector = add_sig_stars(coef_vector, se_vector),
         se_vector = round(se_vector, 3)) %>% 
  # Variblaes in columns
  pivot_wider(names_from = trust_var, 
              values_from = c(coef_vector, se_vector)) %>%
  separate(col  = survey_name, 
           into = c("survey_name", "coef"),
           sep  = "_")


## Social Institutions ----
output_list %>% 
  arrange(coef, survey_name) %>% 
  select(survey_name, contains(c("personal", "clerg"))) %>% 
  kable(., "latex", booktabs = TRUE) %>% 
  clipr::write_clip()


## Government Institutions ----
output_list %>% 
  arrange(coef, survey_name) %>% 
  select(survey_name, contains(c("confed", "conlegis", "conjudge", "conarmy"))) %>% 
  kable(., "latex", booktabs = TRUE) %>% 
  clipr::write_clip()


## Economic Institutions ----
output_list %>% 
  arrange(coef, survey_name) %>% 
  select(survey_name, contains(c("confinan", "conbus", "conlabor"))) %>% 
  kable(., "latex", booktabs = TRUE) %>% 
  clipr::write_clip()


## Education and Health Institutions ----
output_list %>% 
  arrange(coef, survey_name) %>% 
  select(survey_name, contains(c("conpress", "contv"))) %>% 
  kable(., "latex", booktabs = TRUE) %>% 
  clipr::write_clip()

## Education and Health Institutions ----
output_list %>% 
  arrange(coef, survey_name) %>% 
  select(survey_name, contains(c("coneduc", "consci", "conmedic"))) %>% 
  kable(., "latex", booktabs = TRUE) %>% 
  clipr::write_clip()


### EOF ###