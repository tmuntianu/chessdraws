# chessdraws

## Data
For data, [Caissabase](http://caissabase.co.uk/) is used as input to the `data_cleaning.ipynb` notebook

### Ideas/todo
Learn probability of drawing from data, estimate $P(\text{Draw}\,\vert\, R_1, R_2)$ where $R_1, R_2$ are ratings

$$
\text{Assuming optimal play, }\\
E(P_1, P_2)=E(R_1,R_2) = P(P_1 \text{ Win}) + \frac{1}{2} P(\text{Draw})\\
\text{and since $E(R_1,R_2)$ is known} \\
P(\text{Draw}) = 2(E(R_1,R_2)-P(P_1\text{ Win})) \\
\text{We want a function } w: (R_1, R_2)\to P(P_1\text{ Win}) \text{ so then we can compute }
P(\text{Draw}) = 2(\text{Elo}(R_1,R_2)-w(R_1, R_2)) \\
\text{We can let $w$ be drawn from a Gaussian process prior, which induces a GP
posterior on $P(\text{Draw})$}
$$

We can then estimate $P(\text{Draw})$ using the model (i.e., with player
drawing behavior), and compare the distributions
