function avg_best_distance = RunTimes(number, inputcities)

avg = zeros(number,1)

for i = 1 : number

[avg x] = SimulatedAnnealing(inputcities);

average(i) = avg;

end

avg_best_distance = average;
end