function [cost, S, violation] = RandomInitilisation(matrix_a, column_cost, PopSize)
% S = the set of columns in a solution,
% U = the set of uncovered rows,
% wi = the number of columns that cover row i, i ∈ I in S.
% I = {1, . . . , m} the set of rows
% J = {1, . . . , n} the set of columns,
% α i = { j | a i j = 1, j ∈ J } the set of columns that cover row i
% β j = {i | a i j = 1, i ∈ I } the set of rows covered by column j.
%set S k := ∅; set U := I ;

%A(row, column)

%Pop size
N = PopSize;
% number of rows
m = size(matrix_a,1);
% number of columns
n = size(matrix_a, 2);


% I is the set of rows
I = [1:1:m];
% S is the solution, i.e., S_j=1 means the j_th column is selcted, S_j=0,
% otherwise
S = zeros(N,n);
cost = zeros(N,1);
violation = zeros(N,1);

for k = 1 : N 
   
    [cost(k,1), S(k,:), violation(k,1)] = StochasticSetCover(matrix_a, column_cost);
    
end

%min_cost = min(cost);

end
