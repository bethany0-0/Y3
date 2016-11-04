function results = RunTimes(number, inputcities)

avg = zeros(number,1)

for i = 1 : number

[avg x] = SimulatedAnnealing(inputcities);

results(i) = avg;

end

%%avg_best_distance = mean(results);
%%standard_deviation = std2(results);

end