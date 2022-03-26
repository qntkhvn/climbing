## `climbing`

This GitHub repository contains the code and data for the paper **An Examination of Sport Climbing Competition Format and Scoring System** by Quang Nguyen, Hannah Butler, and Gregory J. Matthews.

### Data

Data were collected for past climbing contests that took place between 2018 and 2021, where the combined format was used to determine the rankings of climbers. The competitions include the 2020 Tokyo Olympics, 2020 Continental Championships of Europe, Africa, Oceania, Pan-America; 2019 and 2018 World Championships; 2018 Asian Games; and 2018 Youth Olympics. The data for these events were obtained from Wikipedia, Olympics Library, and the [official website of International Federation of Sport Climbing](https://www.ifsc-climbing.org/index.php/world-competition). 

### Code

`R` scripts are provided for reproducing the following sections in our paper:

* Simulation study: examining the general features of the rank-product scoring method of sport climbing such as win probability and advancement scores. The method we ended up implementing for our simulations is copulas (see [`01c_sim_copula.R`](https://github.com/qntkhvn/climbing/blob/main/code/01c_sim_copula.R)).
* Data analysis: correlation analysis of the climbing event ranks (see [`02_correlation.R`](https://github.com/qntkhvn/climbing/blob/main/code/02_correlation.R)), and "leave-one-climber-out" analysis using ideas from social choice theory (see [`03_leave_one_out.R`](https://github.com/qntkhvn/climbing/blob/main/code/03_leave_one_out.R)).

### Paper

[A pre-print of the paper is available on arXiv](https://arxiv.org/abs/2111.05310).

