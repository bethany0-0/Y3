'''
Created on 1 Oct 2016

@author: shan
'''


# Python imports
import numpy as np # Matrix and vector computation package
np.seterr(all='ignore') # ignore numpy warning like multiplication of inf
import matplotlib.pyplot as plt  # Plotting library
from matplotlib.colors import colorConverter, ListedColormap # some plotting functions
from random import randint
from random import shuffle
from sklearn import datasets

# Set the seed of the numpy random number generator so that the tutorial is reproducable
np.random.seed(seed=1)

# Define and generate the samples
# Generate a dataset and plot it
np.random.seed(0)
nb_of_samples = 200
X, t = datasets.make_moons(nb_of_samples, noise=0.15)
plt.scatter(X[:,0], X[:,1], s=40, c=t)
plt.title('red vs. blue classes in the input space')
plt.show()



# Define the logistic function
def logistic(z): 
    return 1 / (1 + np.exp(-z))

# Define the cross entropy cost function for binary class classification
def cost(y, t):
    return - np.sum(np.multiply(t, np.log(y)) + np.multiply((1-t), np.log(1-y)))

def ann_predict(w1, w2, X):   
    nb_of_samples = len(X)  
    y_hat = np.zeros((1, nb_of_samples)) 
    for p in range(nb_of_samples):
        x_p = np.asmatrix(X[p,:])
        ## Try the updated weights
        s_layer1_p =  x_p * w1.T 
            # Remember the activation function is tanh
        a_layer1_p = np.tanh(s_layer1_p)
        # For output layer, we first calculate s^{p(2)}
        s_layer2_p = a_layer1_p * w2.T
        # The activation function is logistic
        y_p = logistic(s_layer2_p)
        y_hat[:,p] = np.around(y_p)
    return y_hat

# Plot the resulting decision boundary
def plot_decision_boundary(w1, w2, X, t):
    nb_of_xs = 200
    # Generate a grid over the input space to plot the color of the
    #  classification at that grid point
    xs1 = np.linspace(-3, 3, num=nb_of_xs)
    xs2 = np.linspace(-3, 3, num=nb_of_xs)
    xx, yy = np.meshgrid(xs1, xs2) # create the grid
    # Initialize and fill the classification plane
    classification_plane = np.zeros((nb_of_xs, nb_of_xs))
    for i in range(nb_of_xs):
        for j in range(nb_of_xs):
            x_p = np.asmatrix([xx[i,j], yy[i,j]])
            y_p = ann_predict(w1,w2,x_p)
            classification_plane[i,j] = y_p
    # Create a color map to show the classification colors of each grid point
    cmap = ListedColormap([
            colorConverter.to_rgba('r', alpha=0.30),
            colorConverter.to_rgba('b', alpha=0.30)])
    # Plot the classification plane with decision boundary and input samples
    plt.contourf(xx, yy, classification_plane, cmap=cmap)
    plt.scatter(X[:,0], X[:,1], s=40, c=t)
    plt.grid()
    plt.xlabel('$x_1$', fontsize=15)
    plt.ylabel('$x_2$', fontsize=15)
    plt.title('red vs. blue classification boundary')
    plt.show()

# Binary classification problem, one output neruon
num_output_neurons = 1

# We have a two dimensional input, hence two input neurons
num_input_neurons = 2

# Set the random initial weight parameter
# now we have 3 layers, which consists of
# one output (l=L=2), one hidden (l=1) and one input layer (l=0)
# We set the number of hidden neurons 
# Since we incorporated the bias term into the weights, we need one
# extra neurons, therefore, at least 3 neurons to have some non-linearity  
num_hidden_neurons = 10
# The weights connecting from input neurons and hidden 
# neurons are on layer 1
# A matrix of random numbers
# Each row for each hidden neuron
# Each column for each input neuron
#    x1,    x2
# h1 w_11   w_12
# h2 w_21   w_22
# ..............
# h_{num_hidden_neurons} w_{num_hidden_neurons1} w_{num_hidden_neurons2}
# The values of weights w_ij are from standard normal distribution (randn())
w1 = np.asmatrix(np.random.randn(num_hidden_neurons, num_input_neurons) )

# One single output neuron, only one row with num_hidden_neurons columns 
# each column is the weight from a hidden neuron to the output neuron
# The values of weights are from standard normal distribution (randn())
w2 = np.asmatrix(np.random.randn(num_output_neurons, num_hidden_neurons) )
 
y_hat = ann_predict(w1,w2,X)        
error = np.sum(np.abs(y_hat - t))
print(error)
plot_decision_boundary(w1,w2,X, t)

cost_iter = []
# Set the learning rate
eta = 0.1

# Start the gradient descent updates  
nb_of_iterations = 100  # Number of gradient descent updates

## 18 lines of code can implement BP for a 3-layer MLP
for k in range(nb_of_iterations):
    cost_all = 0   
    # Randomly reshuffle training samples 
    inx = [[i] for i in range(nb_of_samples)] 
    shuffle(inx)
    for d in range(nb_of_samples):
        ### The forward step ###:
        # Pick one sample from the randomly reshuffled train set
        p = inx[d];
        x_p = np.asmatrix(X[p,:])
        # For each hidden neuron, the input signal s^{p(1)} 
        # which is a vector of num_hidden_neurons elements
        s_layer1_p =  x_p * w1.T 
        # Remember the activation function is tanh
        a_layer1_p = np.tanh(s_layer1_p)
        # For output layer, we first calculate s^{p(2)}
        s_layer2_p = a_layer1_p * w2.T
        # The activation function is logistic
        y_p = logistic(s_layer2_p)        
        ### The backward step ###:
        delta2_p = y_p - t[p]
        # Update the weights for output layer (l=2)       
        dw2 = -eta*delta2_p*a_layer1_p
        w2 = w2 + dw2      
        # Update the weights for hidden layer (l=1)
        # You can use matrix form to vertorise this double loop implementation       
        for i in range(num_hidden_neurons):
            # Calculate the error signal delta1 Equations on slides page 11
            delta1_p  = delta2_p * w2[:,i] * (1 - np.power(a_layer1_p[:,i],2))
            for j in range(num_input_neurons):
                # Calculate Delta_ij using equations on slides page 12 
                dw1 =  -eta*x_p[:,j]*delta1_p 
                w1[i,j] = w1[i,j] + dw1.T
        ## Try the updated weights to calculate cost
        ## You can delete this bit
        s_layer1_p =  x_p * w1.T 
        # Remember the activation function is tanh
        a_layer1_p = np.tanh(s_layer1_p)
        # For output layer, we first calculate s^{p(2)}
        s_layer2_p = a_layer1_p * w2.T
        # The activation function is logistic
        y_p = logistic(s_layer2_p)  
        cost_all = cost_all + cost(y_p, t[p])
    cost_iter.append(cost_all)
    print(cost_all)               
 

y_hat = ann_predict(w1,w2,X)        
error = np.sum(np.abs(y_hat - t))
print(error)
plot_decision_boundary(w1, w2, X, t)


