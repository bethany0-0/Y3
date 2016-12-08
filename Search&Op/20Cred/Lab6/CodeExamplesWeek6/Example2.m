
%% Try the B727 problem
[matrix_a, column_cost] = ReadInData('sppnw41.txt');
results = [];
for i=1:100
  [total_cost, x] = StochasticSetCovering(matrix_a, column_cost);
  results = [results total_cost];
end