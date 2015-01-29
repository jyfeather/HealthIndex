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

%% for bootstrapping confidence interval use
boot.num = 100;
for i = 1:boot.num
    E = csvread(strcat('./data/bootstrapping/E/E', num2str(i), '.csv'));
    [n_x, n_w] = size(E);
    C = 20; % determined by cross validation
    cvx_begin quiet
        variable x(n_x) nonnegative;
        variable u(n_w) nonnegative;
        variable v(n_w) nonnegative;
        minimize(sum(x));
        subject to
            sum(u) + sum(v) <= C;
            E * (u-v) >= 1-x;
    cvx_end

    w = u - v;

    csvwrite(strcat('./data/bootstrapping/W/W', num2str(i), '.csv'), w);    
end