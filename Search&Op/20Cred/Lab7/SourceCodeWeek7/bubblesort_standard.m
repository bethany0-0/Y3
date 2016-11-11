function [x idx] = bubblesort_standard(x)
% Bubble sort
n = length(x);
idx = [1:n];
swapped = true;
while (swapped == true)
    % Iterate through x
    swapped = false; 
    for i = 2:n
        % Swap elements in wrong order
        % We order the elements from smallest to the greatest
        if (x(i) < x(i - 1))
            x = swap(x,i,i - 1);
            idx = swap(idx, i, i-1);
            swapped = true;
        end
    end
end


end

function x = swap(x,i,j)
% Swap x(i) and x(j)
% Note: In practice, x xhould be passed by reference

val = x(i);
x(i) = x(j);
x(j) = val;

end


