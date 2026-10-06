## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# load the package
library(causaljudgment)

## -----------------------------------------------------------------------------
# exogenous probabilities
p_math <- .1 # probability of passing math exam
p_history <- .9 # probability of passing history

# structural equation
causal_rule <- 'history & math' # Alice passes if she passes history and math

# collect the above information in a causal model
causalmodel <- list(graduate=causal_rule, math=p_math, history=p_history)

# what happened in the actual world
actual_world <- list(graduate=1, math=1, history=1)


## -----------------------------------------------------------------------------
# to what extent did passing the history exam (high-probability event) cause 
# Alice to graduate?
compute_judgment(
  'history', 'graduate', causalmodel, actual_world, 'ces', .7
  )

# to what extent did passing the math exam (low-probability event) cause 
# Alice to graduate?
compute_judgment(
  'math', 'graduate', causalmodel, actual_world, 'ces', .7
  )


## -----------------------------------------------------------------------------
# new structural equation
disjunctive_rule <- 'history | math'
# causal model
causalmodel_disjunctive <- list(graduate=disjunctive_rule, history=p_history,
                                 math=p_math)

# to what extent did passing the history exam (high-probability event) cause 
# Alice to graduate?
compute_judgment(
  'history', 'graduate', causalmodel_disjunctive, actual_world, 'ces', .7
  )

# to what extent did passing the math exam (low-probability event) cause 
# Alice to graduate?
compute_judgment(
  'math', 'graduate', causalmodel_disjunctive, actual_world, 'ces', .7
  )


## -----------------------------------------------------------------------------
# conjunctive structure
compute_judgment(
  'history', 'graduate', causalmodel, actual_world, 'ns'
  )
compute_judgment(
  'math', 'graduate', causalmodel, actual_world, 'ns'
  )

# disjunctive structure
compute_judgment(
  'history', 'graduate', causalmodel_disjunctive, actual_world, 'ns'
  )
compute_judgment(
  'math', 'graduate', causalmodel_disjunctive, actual_world, 'ces', .7
  )

## -----------------------------------------------------------------------------
causal_model <- list(e='a+b+c>1.5', a=.05, b=.5, c=.95) 


## -----------------------------------------------------------------------------
actual_world2a <- list(e=1, a=1, b=1, c=1) # study 2a
actual_world2b <- list(e=1, a=1, b=1, c=0) # study 2b

## -----------------------------------------------------------------------------
# A (low probability)
compute_judgment('a', 'e', causal_model, actual_world2a, 'ces', s=.7)
# B (medium probability)
compute_judgment('b', 'e', causal_model, actual_world2a, 'ces', s=.7)
# C (high probability)
compute_judgment('c', 'e', causal_model, actual_world2a, 'ces', s=.7)

## -----------------------------------------------------------------------------
# A (low probability)
compute_judgment('a', 'e', causal_model, actual_world2b, 'ces', s=.7)
# B (medium probability)
compute_judgment('b', 'e', causal_model, actual_world2b, 'ces', s=.7)

## -----------------------------------------------------------------------------
causal_model <- list(e='a', a=.9, c='a')
actual_world <- list(e=1, a=1, c=1)

## -----------------------------------------------------------------------------
# A: 
compute_judgment('a', 'e', causal_model, actual_world, 'ces', s=.7)
# C:
compute_judgment('c', 'e', causal_model, actual_world, 'ces', s=.7)

## -----------------------------------------------------------------------------
causal_model <- list(e='a | c', c='a | b', a=.5, b=.5)

## -----------------------------------------------------------------------------
# actual-world values
actual_world_onePath <- list(e=1, a=1, c=1, b=0)

# A:
compute_judgment('a', 'e', causal_model, actual_world_onePath,
                 'ces', s=.7)
# C:
compute_judgment('c', 'e', causal_model, actual_world_onePath,
                 'ces', s=.7)

## -----------------------------------------------------------------------------
# actual-world values
actual_world_twoPaths <- list(e=1, a=1, c=1, b=1)

# A:
compute_judgment('a', 'e', causal_model, actual_world_twoPaths,
                 'ces', s=.7)
# C:
compute_judgment('c', 'e', causal_model, actual_world_twoPaths,
                 'ces', s=.7)

## -----------------------------------------------------------------------------
causal_model <- list(e='a & c', c='a | b', a=.5, b=.5)
# A:
compute_judgment('a', 'e', causal_model, actual_world_twoPaths,
                 'ces', s=.7)
# C:
compute_judgment('c', 'e', causal_model, actual_world_twoPaths,
                 'ces', s=.7)

## -----------------------------------------------------------------------------
# define exogenous probabilities
pc <- .5
pu <- .5 
pd <- .5
# define the causal model. R represents the event of successful prevention.
causal_model <- list(e='c & !r', r='u & !d', c=pc, u=pu, d=pd)
# define the actual world
actual_world <- list(e=1, u=1, c=1, r=0, d=1)

# CES judgment for C:
compute_judgment('c', 'e', causal_model, actual_world, 'ces', s=.7)
# CES judgment for D:
compute_judgment('d', 'e', causal_model, actual_world, 'ces', s=.7)


## -----------------------------------------------------------------------------
pc <- .5
pu_high <- .9 # increase p(U)
pd_low <- .1 # decrease p(D)
causal_model_newprobs <- list(e='c & !r', r='u & !d', c=pc, u=pu_high, d=pd_low)

# CES judgment for C:
compute_judgment('c', 'e', causal_model_newprobs, actual_world, 'ces', s=.7)
# CES judgment for D:
compute_judgment('d', 'e', causal_model_newprobs, actual_world, 'ces', s=.7)

## -----------------------------------------------------------------------------

## with initial set of probabilities (all p=.5)
# NS judgment for C:
compute_judgment('c', 'e', causal_model, actual_world, 'ns')
# NS judgment for D:
compute_judgment('d', 'e', causal_model, actual_world, 'ns')

## with new set of probabilities
# NS judgment for C:
compute_judgment('c', 'e', causal_model_newprobs, actual_world, 'ns')
# NS judgment for D:
compute_judgment('d', 'e', causal_model_newprobs, actual_world, 'ns')

## -----------------------------------------------------------------------------
# define probabilities of A and B
pa <- .5 # Alice throws with .5 probability
pb <- .5 # Billy throws with .5 probability

# define reliability with which A and B trigger E
# we define new exogenous variables for that purpose
p_uae <- .6 # Alice's rock reaches bottle with .6 probability
p_ube <- .9 # Billy's rock reaches bottle with .9 probability

# the structural equation for E: E happens if A and U_a->e happen or if B and
# U_b->e happen
equation_e <- 'a&uae | b&ube'

# define the causal model
causal_model <- list(e=equation_e, a=pa, b=pb,
                     uae=p_uae, ube=p_ube)

## -----------------------------------------------------------------------------
# in the actual world, everything happens
actual_world <- list(e=1, a=1, b=1, uae=1, ube=1)
# CES judgment for A (less reliable cause):
compute_judgment('a','e',causal_model, actual_world, 'ces', .7)
# CES judgment for B (more reliable cause):
compute_judgment('b','e',causal_model, actual_world, 'ces', .7)


## -----------------------------------------------------------------------------
# exogenous probabilitiy of variables
pa <- .2 # p(alice scores)
pb <- .2 # p(billy scores)
pc <- .2 # p(claire scores)
pd <- .2 # p(denis scores)

# time step at which events happen
ta <- 1 # alice scores first
tb <- 3 # billy scores third
tc <- 2 # claire scores second


# function giving counterfactual stability as a function of time
compute_stability <- function(time){
  s_vector <- c(.7, .5, .3)
  return(s_vector[time])
}

# stability parameters 
s_list <- list(a=compute_stability(ta), b=compute_stability(tb), 
               c=compute_stability(tc), d=.5)

# condition for victory
equation_e <- 'a+b > c+d'

# define the causal model
gameModel <- list(e = equation_e, a = pa, b = pb,
                  c=pc, d=pd)

# the values of variables in the actual world
actual_world <- list(e = 1, a = 1, b = 1, c=1, d=0)

## -----------------------------------------------------------------------------
# CES judgment for A (early event):
compute_judgment(
  "a", "e", gameModel, actual_world, "ces", s_list
)
# CES judgment for B (late event)
compute_judgment(
  "b", "e", gameModel, actual_world, "ces", s_list
)

## -----------------------------------------------------------------------------
# time step at which events happen
ta <- 1 # alice scores first
tb <- 2 # billy scores second

# stability parameters 
s_list <- list(a=compute_stability(ta), b=compute_stability(tb), c=.5, d=.5)

# the values of variables in the actual world
actual_world <- list(e = 1, a = 1, b = 1, c=0, d=0)

# CES judgment for A (early event):
compute_judgment(
  "a", "e", gameModel, actual_world, "ces", s_list
)
# CES judgment for B (late event)
compute_judgment(
  "b", "e", gameModel, actual_world, "ces", s_list
)


