%Rastrigin Function
function Rastrigin=Rastrigin(x)

Bound=[-5.12 5.12];

if nargin==0
    Rastrigin = Bound;
else
    [PopSize, Dim] = size(x);
    Rastrigin = sum(x.^2 - 10*cos(2*pi.*x)) + Dim*10;
end
