# Game-Theory-Simulator
Simulation Runs a 2X2 Game Theory Matrix to Find Dominant Strategies and Nash Equilibria

In Theory , everyone knows the payoff matrix

In reality, payoff matrixes are estimates

This simulation provides insight by benchmarking payoffs as ranges and relative value by a competitor

In the game, Player A chooses the decisions both players will make (eg Buy, Sell)

The intersection of the choices creates the matrix... for example:
                A    B
  Upper Left:  Buy | Buy
  Upper Right: Sell | Buy
  Lower Left:  Buy | Sell
  Lower Right:  Sell | Sell
  

Player A chooses an Upper and Lower Pay-Out value for each of the cells
The Upper Value minus the Lower Value is the range of Player A's Payout

Upper Left:  Max A= 8  and Min A= 4; range= 8-4 = 4

Player B selects a relative value for each of Player A's Cells.  

For example:

  1 would be exactly equal 

  -1 would be exactly opposite

  .5 would be half A's value
  
  Game Start.  Since random numbers are used to populate Player's A value for a stated number of runs, the game finds dominant theories for planning and assessment
