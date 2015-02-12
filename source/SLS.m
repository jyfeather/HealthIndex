E = csvread('./data/E.csv');
[n_x, n_w] = size(E);
C = 5; % determined by cross validation

%% L1 norm formulation
cvx_begin
    variable x(n_x) nonnegative;
    variable w(n_w);
    minimize(C*sum(x)+w'*w);
    subject to
        -E * w >= 1-x;
cvx_end

csvwrite('./data/w.res', w);

%% decreasing vs. nondecreasing
clear all; clc;
E1 = csvread('./data/Edec.csv');
E0 = csvread('./data/Enondec.csv');
[n_x1, n_w] = size(E1);
[n_x0, n_w] = size(E0);
C = 5;

%% formulation 1
cvx_begin
    variable x1(n_x1) nonnegative;
    variable x0(n_x0) nonnegative;
    variable w(n_w);
    minimize(C*(sum(x1)+sum(x0))+w'*w);
    subject to
        -E1 * w >= 1-x1;
        E0 * w >= 1-x0;
cvx_end
csvwrite('./data/w.res', w);

%% formulation 2
cvx_begin
    variable x1_1(n_x1) nonnegative;
    variable x1_0(n_x1) nonnegative;
    variable x0_1(n_x0) nonnegative;
    variable x0_0(n_x0) nonnegative;
    variable w(n_w);
    minimize(C*(sum(x1_0-x1_1)+sum(x0_1-x0_0))+w'*w);
    subject to
        E1 * w == x1_1-x1_0;
        E0 * w == x0_1-x0_0;
cvx_end
csvwrite('./data/w.res', w);