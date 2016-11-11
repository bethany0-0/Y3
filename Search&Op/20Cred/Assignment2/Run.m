Pt = 0.4;



[matrix_a, column_cost] = ReadInData('sppnw41.txt');
results = [];

for i=1:10

    [total_cost, x] = RandomInitilisation(matrix_a, column_cost);
  
    results = [results total_cost];
end