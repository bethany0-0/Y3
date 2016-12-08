function new_pop = HeuristicImprovement(offspring, matrix_a, column_cost )

new_pop = offspring;
[N,x] = size(new_pop);
% number of rows
m = size(matrix_a,1);
for i = 1:N
    
    S = offspring(i,:);
    
    %initialise w i := |α i ∩ S|, ∀i ∈ I ;
    for j = 1:m
        % alpha_i is the indices of columns that cover row i
        alpha_i = find(matrix_a(j,:)==1); 
        covered_S = find(S(1,:)==1); 
       [w(j), x] = size(intersect(alpha_i, covered_S));
    end
    
    
    T = S;
    
    %% Remove double covered rows
    while sum(T)>0  
        % Find out which columns have been covered
        covered_col_idx = find(S==1);  

        rand = randi(length(covered_col_idx));
        % randomly select an uncovered row j
        j = covered_col_idx(rand);  
        
        %set T ← T − j;
        T(j) = 0;
         
        beta_j = find(matrix_a(:,j)==1);
        
        for k = 1:m
        if w(k) > 1
            S(k) = 0;
            w(k) = w(j)-1;
        end
        end

    end
   


%% Add uncovered rows
%%%%%%%%%%%%%%%update to work

uncovered_rows_idx = find(w==0);
[x,y] = size(uncovered_rows_idx);
while y > 0;
 % Find out which rows have not been covered     
        
    %% Code section you need to complete
    rand = randi(length(uncovered_rows_idx));
    w(1,rand) = 0;
    % randomly select an uncovered row i
    j = uncovered_rows_idx(rand);
    % alpha_i is the indices of columns that cover row i
    alpha_i = find(matrix_a(:,j)==1);
    %  select column j \in \alpha_i which covers row i with minimum cost
    [mincost, idx] = min(column_cost(alpha_i)); % Replace the square brackets with your code. Hint: use alpha_i and costs
    
    %%    
    % However, there are multiple column with the same minimum cost
    % If we use min function in matlab, we will always selet the first column with the minimum cost
    idx_array = find(column_cost(alpha_i) == mincost);
    num_same_mincost = length(idx_array);
    % To prevent this problem, we randomly select one column if there are multiple column with the same minimum cost
    if num_same_mincost > 1
      idx = idx_array(randi(num_same_mincost));
    end
    j = alpha_i(idx);
    % Set column j as part of the solution
    S(1,j) = 1;
    % Set I to include the rows covered by column j
    I(1, matrix_a(:,j)==1) = 1;
   
end

new_pop(i,:) = S;

end

end

