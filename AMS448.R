# Generate 1000 exponentially distributed values with mean 0.2
service_times <- rexp(1000, rate = 0.2)

# Generate 1000 Poisson distributed values with mean 8
interarrival_times <- rpois(1000, lambda = 8)

# Randomly sample 10 values from each vector
service_times_sample <- sample(service_times, 10)
interarrival_times_sample <- sample(interarrival_times, 10)

# Calculate arrival times
arrival_times <- cumsum(interarrival_times_sample)

# Calculate service end times
service_end_times <- arrival_times + service_times_sample

# Print the simulation schedule
schedule <- data.frame(Customer = 1:10,
                       Arrival = arrival_times,
                       ServiceTime = service_times_sample,
                       ServiceEnd = service_end_times)
print(schedule)

# 1. average service time
service_times <- schedule$ServiceTime
avg_service_time <- mean(service_times)

print(avg_service_time)

# 2. average waiting time
# Calculate ServiceStart times
schedule$ServiceStart <- numeric(length=nrow(schedule))
schedule$ServiceStart[1] <- schedule$Arrival[1]
for (i in 2:nrow(schedule)) {
  schedule$ServiceStart[i] <- max(schedule$Arrival[i], schedule$ServiceEnd[i-1])
}
# Calculate waiting times
waiting_times <- schedule$ServiceStart - schedule$Arrival

# Calculate the average waiting time
avg_waiting_time <- mean(waiting_times)

# Print the result
print(avg_waiting_time)

# 3. average time spent in the system
# Calculate time spent in the system
time_spent <- schedule$ServiceEnd - schedule$Arrival

# Calculate the average time spent in the system
avg_time_spent <- mean(time_spent)

# Print the result
print(avg_time_spent)

# 4. probability the server is idle
# Calculate the total time of the simulation
total_time <- max(schedule$ServiceEnd)

# Calculate the number of time intervals in which the server is idle
n_idle_intervals <- 0
for (t in 0:ceiling(total_time)) {
  busy <- FALSE
  for (i in 1:nrow(schedule)) {
    if (t >= schedule$ServiceStart[i] && t < schedule$ServiceEnd[i]) {
      busy <- TRUE
      break
    }
  }
  if (!busy) {
    n_idle_intervals <- n_idle_intervals + 1
  }
}

# Calculate the probability that the server is idle
prob_idle <- n_idle_intervals / ceiling(total_time)

# Print the result
print(prob_idle)
