%% Swap two cities
function newroute = twoopt(route, i, k)

if (i == k+1) 
    newroute = [route(1:i) k i route(k+1:end)];
else
% Step 1: take $route[1]$ to $route[i-1]$ and add them in order to

first= route(1:i);

% Step 2: take $route[i]$ to $route[k]$ and add them in reverse order to

mid = fliplr(route(i+1:k));

% Hint: type help fliplr

% Step 3: take $route[k+1]$ to end and add them in order to new $newroute$
 
last = route(k+1:end);

newroute = [first mid last];
end