function Rosenbrock=Rosenbrock(x)
Bound=[-30 30];

if nargin==0
    Rosenbrock = Bound;
else
    h=0;
    x=x';
    [row col]=size(x);
    xa=x(1:row-1,:);
    xb=x(2:row,:);
    Rosenbrock = sum(100*(xa.^2-xb).^2 + (1-xa).^2);
end
