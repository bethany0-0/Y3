Pt = 0.4;



[matrix_a, column_cost] = ReadInData('sppnw41.txt');

[total_cost, F] = RandomInitilisation(matrix_a, column_cost);

for i=1:10
    
    StochasticRanking(total_cost, F);
  
 heuristicImprovement
   
end