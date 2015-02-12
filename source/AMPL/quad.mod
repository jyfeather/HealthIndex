param n_w;
param n_xi;
param C;
param E{1..n_xi, 1..n_w};

#var w{i in 1..n_w} >= 0;
var w{i in 1..n_w};
var xi{j in 1..n_xi} >= 0;

minimize cost: 
	C * sum{j in 1..n_xi} xi[j] + 
	sum{i in 1..n_w} w[i]*w[i];

subject to Monotone {i in 1..n_xi}: sum{j in 1..n_w} E[i,j]*w[j] >= 1-xi[i]; 