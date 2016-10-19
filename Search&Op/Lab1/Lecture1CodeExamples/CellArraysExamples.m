B = {[1,2,3],'hello',1;[3;5],'yes','no'}

% How to use cellfun
A = {'A ', 'test ', 'message', 'Which'}
[nrows, ncols] = cellfun(@size, A)


%% How to use brackets
B(1,2) = {'test'}   % pass it to a cell as we are using () brackets
B{1,2} = 'test'     % same effect as line before.
B{1,2} = {'test'}   % careful, this adds a cell to the cell at (1,2), (nesting cells)
H = B{1,2}{1}       % to then extract it, we have to index twice.