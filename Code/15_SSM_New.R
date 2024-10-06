# This is a WIP so far! Will update soon  

# Re-scale X (if not already done)
X <- X*100

# Assume T is the number of time points and N = 13
T <- length(X)  # Length of the X vector (time points)
N <- 13         # Number of time series

# Initialize Z matrix
Z <- array(0, dim = c(N, 2 + 2*N, T))  # Z has dimensions (N, state vector length, T)

# Fill in the Z matrix
for (t in 1:T) {
for (i in 1:N) {
  Z[i, 1, t] <- 1          # Common intercept term 'a'
  Z[i, 2, t] <- X[t]       # Common slope term 'b'
  Z[i, 2*i+1, t] <- 1      # Random intercept for series i ('a_i')
  Z[i, 2*i+2, t] <- X[t]   # Random slope for series i ('b_i')
}
}

# Define state dimension (2 + 2N: intercept, slope, and random effects for each series)
state_dim <- 2 + 2 * N

# Assuming time-invariant intercepts and slopes:
# Transition matrix (T): identity matrix
T_matrix <- diag(state_dim)

# Define R matrix: (identity matrix if state disturbances are independent)
R_matrix <- diag(state_dim)

# Define Q matrix: (can start as a zero matrix if no evolution in the random effects)
Q_matrix <- diag(state_dim) * 0  # Modify if random effects evolve over time

# Define H matrix: (diagonal matrix for observation noise)
H_matrix <- diag(N)  # Adjust to the variances of your observed series

# Initial state vector (zeros, or any reasonable starting values)
initial_state <- rep(0, state_dim)

# Initial covariance matrix for the state (diagonal or other appropriate guess)
P1_matrix <- diag(state_dim)


# Assuming time-invariant intercepts and slopes:
# Transition matrix (T): identity matrix
T_matrix <- diag(state_dim)

# Define R matrix: (identity matrix if state disturbances are independent)
R_matrix <- diag(state_dim)

# Initial variances for random intercepts and slopes (could start with small positive values)
initial_variance_a <- 0.1  # Initial variance for random intercepts
initial_variance_b <- 0.1  # Initial variance for random slopes

# Define a blocked matrix for Q
Q <- array(0, dim = c(state_dim, state_dim, T))

# Loop over time points to fill in the Q matrix for each time point
for (t in 1:T) {
  
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

# State names
state_names <- c("common_intercept", "common_slope")

# Add random intercept and slope names for each time series
for (i in 1:N) {
  state_names <- c(state_names, paste0("random_intercept_", i), paste0("random_slope_", i))
}

# Construct the custom state space model using SSMcustom
model <- SSModel(
    Y ~ -1 + SSMcustom(
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
  fitted_model <- fit$model
  kalman_results <- KFS(fitted_model, filtering = "state")
  kalman_results$fit <- fit
  #kalman_results$name <- names(.TSdata[, index])
}
