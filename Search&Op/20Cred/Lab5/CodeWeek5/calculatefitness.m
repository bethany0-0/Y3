function [fitness, totl_cost, Violations] = calculatefitness(x, c, a)

%% Your code here
% Follow the code in Code example 3 on page 23
totl_cost = [];
Violations  = [];
penalty = 10000*Violations;

fitness = totl_cost + penalty;

