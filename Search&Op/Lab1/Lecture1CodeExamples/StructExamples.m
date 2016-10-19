S = struct('name','shan','matrix',[1 1; 2 2])

S.name
S.matrix

% Add field and remove field
S.newField = 'foo'
S = rmfield(S, 'matrix')

% Access fields dynamically at runtime
fieldname = {'name', 'newField'}
for i=1:2 
    S.(fieldname{i})
end




