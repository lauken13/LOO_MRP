## experiment1  
bernoulli model for individual (leave-one-out)

## experiment2 
trialling binomial model (leave-one-GROUP/CELL-out rather than individual)

## experiment3
- trying AR prior on X4 
- X4 set to have different relationships with the outcome
  - fx1 (smiley face reln. with the outcome)
  - fx2 (sad face reln. ")
  - fx3 (increasing-shaped reln. ")
  
  ### brms code
  - fit on 23 models 

  ### stan code
  - cutting down to 6 models (choosing the largest model from each group of models)
