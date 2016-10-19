function t = temperature(inputcities)


x := x0; e := f(x) // Initial solution, objective function value (energy). xbest := x; xbest := x // Initial “best” solution k := 0 // Count evaluation number. while (k < kmax)
T := temperature(t0) // Temperature calculation. xnew := neighbour(x) // Pick some neighbour. enew := f(xnew) // Compute its objective function value. if P(e, enew, T) > R(0, 1) then // Should we move to it? x := xnew; e := enew // Yes, change state. if enew < ebest then // Is this a new best? xbest := xnew; ebest := enew // Save as ’best found’. k := k + 1 // Increase Evaluation
Output xbest
end
