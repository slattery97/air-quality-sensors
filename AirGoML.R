#Get mobile sensor behavior matrix
#devtools::install_github("nproellochs/ReinforcementLearning")
#install.packages(c("devtools", "ReinforcementLearning"))
# install.packages("osmdata")
#library(ReinforcementLearning)
#library("osmdata")

#Input number of fixed sensors in each subregion
S <- data.frame(SubRegion=(1:subArea_nos),SensorNumber=0)
for(i in 1:subArea_nos) {
  print(S[i,1])
  S[i,2] <- as.numeric(readline(prompt="Enter Number of Fixed Sensors in Subregion: "))
}

# Define state and action sets
k <- sqrt(subArea_nos)
SubregSensors <- matrix(S[,2],nrow=k,ncol=k)
states <- matrix(paste0('s', 1:k^2), nrow = k, ncol = k)
actions <- c("up", "down", "left", "right", "ne","nw","se","sw")



# Load built-in environment function for n x n gridworld 
env <- function(state, action)
{
  state <- as.numeric(substring(state, 2))
  next_state <- state
  if(action == "up")
  {
    if(state %% k != 1)
      next_state <- state - 1
  }
  if (action == "nw")
  {
    if (state %% k != 1 && state - k > 0 )
      next_state <- state - 1 - k
  }
  if (action == "ne")
  {
    if (state %% k != 1 && state + k <= k^2 )
      next_state <- state + k - 1
  }
  if (action == "down")
  {
    if(state %% k != 0)
      next_state <- state + 1
  }
  if(action == "right")
  {
    if(state + k <= k^2)
      next_state <- state + k
  }
  if(action == "left")
  {
    if(state - k > 0)
      next_state <- state - k
  }
  if (action == "se")
  {
    if (state + k <= k^2 && state %% k != 0)
      next_state <- state + k + 1
  }
  if (action =="sw")
  {
    if (state %% k != 0 && state - k > 0)
      next_state <- state - k + 1
  }
  #Calculate average fixed sensors per subregion
  avgFixed <- floor(n)/(length(res)+length(ind)+length(com))
  #Assign rewards
  if(next_state != state && SubregSensors[next_state] <= avgFixed && SubregSensors[next_state] > 0){
    reward <- 100
  }else if (next_state != state && SubregSensors[next_state]  > avgFixed){
    reward <- 20
  }else  {
    reward <- -1
  }
  out <- list(NextState = paste0('s', next_state), Reward = reward)
  return(out)
}



# Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000, 
                         env = env, 
                         states = states, 
                         actions = actions)
head(data)

# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(data, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "NextState", 
                               control = control)

# Example data
data_unseen <- data.frame(State = c(states), 
                          stringsAsFactors = FALSE)

# Pick optimal action
data_unseen$OptimalAction <- predict(model, data_unseen$State)


# Sample N = 1000 sequences from the environment
# using epsilon-greedy action selection
data_new <- sampleExperience(N = 1000, 
                             env = env, 
                             states = states, 
                             actions = actions, 
                             actionSelection = "epsilon-greedy",
                             model = model, 
                             control = control)

# Update the existing policy using new training data
model_new <- ReinforcementLearning(data_new, 
                                   s = "State", 
                                   a = "Action", 
                                   r = "Reward", 
                                   s_new = "NextState", 
                                   control = control,
                                   model = model)

data_unseen$OptimalAction <- predict(model_new, data_unseen$State)
actionsM <- data_unseen[,2]

# Print result
print("Recommended Actions:")
print(matrix(actionsM, nrow=k,ncol=k))
print("Movement Choices:")
print(actions)
View(sensor_ml)

#Update sensor coordinates
for(i in 1:floor(m)){
  
  #Get desired movement
  sensorAction <- as.character(readline(prompt="Enter direction of desired movement: "))
  
  if(sensorAction == "up")
  {
    #Get Sensor ID
    mobileKey <- as.numeric(readline(prompt="Enter mobile sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1]
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2] + (1/sqrt(subArea_nos))*side
    
  }
  
  if(sensorAction == "down")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] 
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2] - (1/sqrt(subArea_nos))*side
    
    
  }
  
  if(sensorAction == "right")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] + (1/sqrt(subArea_nos))*side
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2]
    
  }
  
  if(sensorAction == "left")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] - (1/sqrt(subArea_nos))*side
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2]
    
  }
  
  if(sensorAction == "ne")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] + (1/sqrt(subArea_nos))*side
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2] + (1/sqrt(subArea_nos))*side
    
  }
  
  if(sensorAction == "se")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] + (1/sqrt(subArea_nos))*side
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2] - (1/sqrt(subArea_nos))*side
    
  }
  
  if(sensorAction == "nw")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] - (1/sqrt(subArea_nos))*side
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2] + (1/sqrt(subArea_nos))*side
    
  }
  
  if(sensorAction == "sw")
  {
    #Get Sensor Key
    mobileKey <- as.numeric(readline(prompt="Enter Mobile Sensor SID: "))
    
    sensor_ml[mobileKey,1] <- sensor_ml[mobileKey,1] - (1/sqrt(subArea_nos))*side
    sensor_ml[mobileKey,2] <- sensor_ml[mobileKey,2] - (1/sqrt(subArea_nos))*side
    
  }
  
  points(sensor_ml[mobileKey,1],sensor_ml[mobileKey,2],pch=20,col='blue')
}

#Convert x,y to Lat,Long
coor <- getbb(city)
center_long <- ((coor[3]-coor[1])/2)+coor[1]
center_lat <- ((coor[4]-coor[2])/2)+coor[2]
sensor_ml[1:(index-1),2] <- (sensor_ml[1:(index-1),2]/111.3)+center_lat
cosrad_temp <- cos((sensor_ml[1:(index-1),2]*pi/180))*69.172
sensor_ml[1:(index-1),1] <- (sensor_ml[1:(index-1),1]/cosrad_temp)+center_long
names(sensor_ml)[1] <- "longitude"
names(sensor_ml)[2] <- "latitude"



