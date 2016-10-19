A = []                          % Create an empty matrix
A = [1 2; 3 4]                  % Create a 2-by-2 matrix
A = [1:2:100]                   % Create an array from 1 to 100, with an interval of 2
A = zeros(4,5)                  % 4-by-5 matrix of all zeros
B = ones (2,3)                  % 2-by-3 matrix of all ones
C = rand(3,3)                   % 3-by-3 matrix of uniform random numbers in [0,1]
D = randn(2,5)                  % 2-by-5 matrix of standard normally distributed numbers
[E,F] = meshgrid(1:5)           % 5-by-5 grids of numbers
[X,Y] = meshgrid(1:3,10:14)     % Create a full grid from two monotonically increasing grid vectors
M = magic(6)                    % Create a magic square in which sum(A,1) = sum(A,2) = trace(A) = trace(rot90(A))


A = [1 2; 3 4]                  % Create a 2-by-2 matrix
B = A'                          % Transpose A
C = [A; A]                      % Concatenate a matrix

A(1,2)                          % Extract the element in row 1, column 2
A(1,:)                          % Extract the element in row 1 
A(end,:)                        % Extract the row of the last index
A>2                             % Logic array of the condition 
A(A>2)                          % Extract all the elements of A that are greater than 2


A = rand(10,10)
A(1,2) = 100                    % Assign 100 to the element in row 1, column 2
A = rand(10,10)
A(:,1:3:end) = 100              % Assign 100 to the elements of columns from 1 to the last index with an interval of 3       
A(1,:) = []                     % Delete the first row   


A = [9 0 -7 5 3 8 -10 4 2]      % Sort an array
B = sort(A)

A = [3 6 5; 7 -2 4; 1 0 -9]     % Sort a matrix along its rows
B = sort(A,2)

A = [9 0 -7 5 3 8 -10 4 2]      % Sort an array and also obtain the indics 
[B, idx] = sort(A)



A = [1 2; 3 4]
B = [2 4; 6 8]
C = A*B                         % Matirx multiplication 
Cdot = A.*B                     % Matirx elememt-wise multiplication 

%% Exercise 2
m = [1, 2, 3; 2, 1, 5; 4, 6, 4; 2, 3, 2], 
n = m(1:2, [1 3])
%% Exercise 3
A = [1, 2, 3; 2, 1, 5; 4, 6, 4; 2, 3, 2]
a = A(:,2)
[b,idx] = sort(a,'descend')
B = A(idx,:)

%% Exercise 4
[X,Y] = meshgrid(-1:0.01:1);           % grids of numbers within the range of -1:0.01:1
Z = zfunction(X,Y);
mesh(X,Y,Z)
