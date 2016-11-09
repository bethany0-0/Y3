function [total_cost, F] = RandomInitilisation(matrix_a, column_cost)
% S = the set of columns in a solution,
% U = the set of uncovered rows,
% wi = the number of columns that cover row i, i ∈ I in S.
% I = {1, . . . , m} the set of rows
% J = {1, . . . , n} the set of columns,
% α i = { j | a i j = 1, j ∈ J } the set of columns that cover row i
% β j = {i | a i j = 1, i ∈ I } the set of rows covered by column j.
%set S k := ∅; set U := I ;

k = ;
%Pop size
N = ;
Rows = [1:1:size(matrix_a)];

%linear indecie of covered element
U = find(I==1) % = covered_idx

%columns covered
a = zeors(length(U));

%rows covered
b = 

for k = 1 : N 
    
    % Initiate Sk to empty
    S(k) = zeros(1,m);
    % Find out which rows have not been covered
    U= Rows; 
    
    %until U = ∅.
    while U ~= zeors(1, size(c))
        
        % randomly select a row i ∈ U ;
        rand = randi(length(U));
        i = U(rand) 
    	
        %randomly select a column j ∈ α i such that β j ∩ (I − U ) = ∅;

	% alpha_i is the indices of columns that cover row i
    	alpha_i = find(matrix_a(i,:)==1); 

   	%select j so bj n (I-U) = empty
%%%%%%%%%%%%%%%%%%need to update (now just randomly selects j)
	rand = randi(length(alpha_i));
        j = alpha_i(rand);


       
        %if j exists
        if
            %set S k ← S k + j; set U ← U − i, ∀i ∈ β j
    
        else
            %set U ← U − i;
        
        end
    
    end

end

end
