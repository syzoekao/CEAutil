#############################################
# Intro to R
#############################################

#### Data type
### Vectors

x1 <- c(3, 1, 4)
print(x1)

x2 <- c(2, 7, 1)
print(x1 + x2)

future_human <- c("earther", "martian", "belter")
print(future_human)

x1_and_future_human <- c(x1, future_human)
print(x1_and_future_human)

v_init <- c(0.9, 0.09, 0.01)
names(v_init) <- c("healthy", "sick", "dead")
print(v_init)

print(sum(v_init))
print(v_init[2])
print(v_init["sick"])
print(v_init[2:3])
print(v_init[c("sick", "dead")])

### Matrix & Array

x <- matrix(c(1, 0, 0,
              0.8, 0.2, 0,
              0, 0, 1),
            byrow = T, nrow = 3)
print(x)

rownames(x) <- colnames(x) <- c("healthy", "sick", "dead")
print(x)
print(x[2, 2])
print(x["sick", "sick"])
print(t(v_init) %*% x)

tr1 <- x
tr2 <- matrix(c(0.9, 0.1, 0,
                0.7, 0.2, 0.1,
                0, 0, 1),
              byrow = T, nrow = 3,
dimnames = list(c("healthy", "sick", "dead"),
c("healthy", "sick", "dead")))

tr_array <- array(dim = c(3, 3, 2),
data = cbind(tr1, tr2),
dimnames = list(c("healthy", "sick", "dead"),
c("healthy", "sick", "dead"),
c(1, 2)))
print(tr_array)

print(tr_array[ , ,  1])
print(tr_array[ , ,  2])
print(tr_array[1, 2, 1])

v_time2 <- t(v_init) %*% tr_array[ , , 1]
print(v_time2)

v_time3 <- v_time2 %*% tr_array[ , , 2]
print(v_time3)

### List

temp_ls <- list(future_human = future_human, v_init = v_init, tr_array = tr_array)

print(temp_ls)
print(temp_ls[[1]])
print(temp_ls$v_init)
print(temp_ls$tr_array[, , 1])

### data.frame

profile <- data.frame(name = c("Amos", "Bobbie", "Naomi"),
                      human_type = c("earther", "martian", "belter"),
                      height = c(1.8, 2.1, 1.78))

print(profile)
typeof(profile)
length(profile$name)
length(profile$human_type)
length(profile$height)

profile[profile$name == "Bobbie", ]
profile[profile$name == "Bobbie", "height"]

#### Data input and output

load("data/ONtan.rda")

data <- "this is nothing"
save(data, file = "data/nothing.rda")

#### Loops
### Example 1: Fibinocci sequency
fib_vec <- rep(0, 10)
fib_vec[c(1:2)] <- c(0, 1)

for (i in c(3 : 10)) {
  fib_vec[i] <- fib_vec[i - 1] + fib_vec[i - 2]
}

print(fib_vec)

### Example 2: Population growth in Mars

popsize <- rep(3000, 100)

# Calculate
for (t in c(1 : 100)) {
  popsize[t + 1] <- popsize[t] + 0.05 * popsize[t] * (1 - popsize[t] / 100000)
}

popdf <- data.frame(year = c(2400:2500),
                    popsize = popsize)
print(popdf[popdf$year == 2500, ])

#### If statement

"yoda" == "windu"

Jedi <- c("yoda", "windu", "kenobi")
"yoda" %in% Jedi

Mandalorian <- c("satine", "sabine", "jango")

x <- "yoda"

if (x %in% Jedi) {
  print("May the force be with you!")
} else if (x %in% Mandalorian) {
  print("This is the way!")
} else {
  print("Hello, world!")
}

#### R function

speak <- function(x) {
  Jedi <- c("yoda", "windu", "kenobi")
  Mandalorian <- c("satine", "sabine", "jango")

  membership <- "Not Jedi or Mandalorian"
  if (x %in% Jedi) {
    say <- "May the force be with you!"
    membership <- "Jedi"
  } else if (x %in% Mandalorian) {
    say <- "This is the way!"
    membership <- "Mandalorian"
  } else {
    say <- "Hello, world!"
  }
  return(list(say = say, membership = membership))
}

formals(speak)
body(speak)
environment(speak)

speak("jango")

#### Plots
### Histogram
x <- rnorm(1000, 0, 1) # draw 1000 samples from a normal distribution

print(mean(x))
print(sd(x))
hist(x, freq = F, col = "gray")

### Scatter plot

load("data/worldHE.rda")
print(head(worldHE))
worldHE2010 <- worldHE[worldHE$Year == 2010, ]

plot(worldHE2010$HealthExp, worldHE2010$LEyr)

plot(worldHE2010$HealthExp, worldHE2010$LEyr, ylim = c(70, 90))

plot(worldHE2010$HealthExp, worldHE2010$LEyr, ylim = c(70, 90),
     xlab = "health expenditure", ylab = "life expectancy")

text(worldHE2010$HealthExp, worldHE2010$LEyr, labels = worldHE2010$Entity, cex = 0.8)

### Line plot

worldHE_US <- worldHE[worldHE$Entity == "United States" & worldHE$Year >= 1970 & worldHE$Year <= 2015, ]
worldHE_UK <- worldHE[worldHE$Entity == "United Kingdom" & worldHE$Year >= 1970 & worldHE$Year <= 2015, ]

plot(worldHE_US$Year, worldHE_US$HealthExp, type = "l", col = "red",
     ylab = "expenditure", xlab = "year", ylim = c(500, 10000))
lines(worldHE_UK$Year, worldHE_UK$HealthExp, col = "royalblue")
legend("topleft", legend=c("US", "UK"),
       col=c("red", "royalblue"), lty = 1, cex = 0.8)

#### Packages
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(devtools)) install.packages("devtools")
if(!require(remotes)) install.packages("remotes")
if(!require(dampack)) remotes::install_github("DARTH-git/dampack", dependencies = TRUE)
if(!require(CEAutil)) remotes::install_github("syzoekao/CEAutil", dependencies = TRUE)



