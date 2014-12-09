clc; clear all;
options =optimset('MaxIter',10^6,'MaxFunEvals',10^6,'TolFun',1e-6);

%% Read data
H = csvread('./inst/dat/H.csv', 1, 0);
l = csvread('./inst/dat/l.csv', 1, 0);
E = csvread('./inst/dat/E.csv', 1, 0);
b0 = csvread('./inst/dat/b0.csv', 1, 0);

%% Solve
[wq,fvalq,exitflagq,outputq] = quadprog(2*H,l,-E,b0,[],[],[],[],[],options);

%% Dump the results
csvwrite('./inst/dat/res.csv', wq);