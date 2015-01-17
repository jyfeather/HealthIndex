E = csvread('./data/Ematrix.csv');
[n_x, n_w] = size(E);
C = 20; % determined by cross validation

%% L1 norm formulation
cvx_begin
    variable x(n_x) nonnegative;
    variable u(n_w) nonnegative;
    variable v(n_w) nonnegative;
    minimize(sum(x));
    subject to
        sum(u) + sum(v) <= C;
        E * (u-v) >= 1-x;
cvx_end

w = u - v;

csvwrite('./data/w.res', w);

%% Quadratic formulation
