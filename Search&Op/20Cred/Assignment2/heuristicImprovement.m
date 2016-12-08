function w = heuristicImprovement( matrix_a, column_cost )
%S = the set of columns in a solution,
%U = the set of uncovered rows,
%w i = the number of columns that cover row i, i ∈ I in S.
% α i = { j | a i j = 1, j ∈ J } the set of columns that cover row i
% β j = {i | a i j = 1, i ∈ I } the set of rows covered by column j.

% number of rows
m = size(matrix_a,1);
% number of columns
n = size(matrix_a, 2);


I = [1:1:m];
% S is the solution, i.e., S_j=1 means the j_th column is selcted, S_j=0,
% otherwise
S = zeros(1,n);

U = I;

%initialise w i := |α i ∩ S|, ∀i ∈ I ;
w = zeros(1,m);

for i = 1:m
        % alpha_i is the indices of columns that cover row i
        alpha_i = find(matrix_a(i,:)==1); 
        
       [x, w(i)] = size(intersect(alpha_i, S));
end

%T is dummy set
T = S;

%T = ∅;
while (T ~= zeros(1,n))
    
    %randomly select a column j, j ∈ T and set T ← T − j;
   rand = randi(size(T));
   j = T(rand);
   
   T(rand) = 0;
   
   beta_j = find(matrix_a(:,j)==1); 
   
   %if w i ≥ 2, for any i ∈ β j then
   for i = 1:m
    if ismember(i,beta_j) & w(i) >= 2
   
        %set S ← S − j; set w i ← w i − 1, ∀i ∈ β j ;
       S(1, j) = 0;
       w(i) = w(i) -1;
       
    end
   end
   
end

%initialise U := {i | w i = 0, ∀i ∈ I };

for i = 1:m
    
    if w(i) == 0
        U = [U i];
    end

end

%set V := U ; /∗ V is a dummy set ∗/
V = U;

%until V = ∅.
while (V ~= zeros(1,size(U)))
    %randomly select a row i ∈ V and set V ← V − i;
    rand = randi(size(V));
    i = V(rand);
    
    V(rand) = 0;
    
    %search for the column j ∈ α i that satisfies β j ⊆ U , and minimises c j /|β j |;
    
    
    %if j exists then
    
    %set S ← S + j; set w i ← w i + 1, ∀i ∈ β j ; set U ← U − β j and V ← V − β j ;

end

end

