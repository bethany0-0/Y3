function [total_cost, F] = RandomInitilisation(matrix_a, column_cost)
% S = the set of columns in a solution,
% U = the set of uncovered rows,
% wi = the number of columns that cover row i, i ∈ I in S.
% I = {1, . . . , m} the set of rows
% J = {1, . . . , n} the set of columns,
% α i = { j | a i j = 1, j ∈ J } the set of columns that cover row i
% β j = {i | a i j = 1, i ∈ I } the set of rows covered by column j.
%set S k := ∅; set U := I ;

%Pop size
% number of rows
m = size(matrix_a,1);
% number of columns
n = size(matrix_a, 2);


% I is the set of rows
I = [1:1:m];
% S is the solution, i.e., S_j=1 means the j_th column is selcted, S_j=0,
% otherwise
S = zeros(1,n);

for k = 1 : n 
    %set U := I ;
    U = I; 
    
    count = 1;
    %repeat
    %until U = ∅.
    while U ~= zeros(1, m)
        
        % randomly select a row i ∈ U ;
        rand = randi(length(U));
        i = U(rand); 
    	
        %randomly select a column j ∈ α i such that β j ∩ (I − U ) = ∅;

        % alpha_i is the indices of columns that cover row i
    	alpha_i = find(matrix_a(i,:)==1); 

        %select j so bj n (I-U) = empty
       	alpha_U_union = intersect(U, alpha_i);
        rand = randi(length(alpha_U_union));
        j = alpha_U_union(rand);
         
        %if j exists
        if (j ~= 0)
            %set S k ← S k + j; 
            S(k, count) = j;
            
            %set U ← U − i, ∀i ∈ β j
            U(i) = 0;
    
            count = count +1;
            
        else 
            %set U ← U − i;
            U(i) = 0;
            
        end
    
    end

end

end
