## In many situation in statistics, the goal is to find an optimal solution to a problem. Sometimes, the optimal solution can be found analytically (i.e., taking the derivate and finding its roots), but often we do not have analytical solutions available and must rely on numerical solutions.

The general form of an optimization problem is as follows, where \(f(\mathbf{x})\) is called the objective function and \(g(\mathbf{x})=\mathbf{0}\) are called the constraints. \[ \arg \min_{\mathbf{x}} f(\mathbf{x}), \mathrm{~subject~to~} g(\mathbf{x}) =\mathbf{0} \]

There are various methods for optimization, and some of the most famous ones are built in R’s optim function.

The simplest use of optim, with default methods and without and constraints is optim(par, fn, ...) where par is the initial value for the parameters to be optimized (\(\mathbf{x}\) in the formula above), fn is an R function that takes par as the first argument, and ... show other potential arguments for the function. par can be a vector.

Note that depending on the starting value, we may receive different results back. So if we are not certain about the uniqueness of the answer, we may start from a few randomly chosen starting points and see what we obtain.

optim returns a list that shows the optimal set of parameters, the optimal value corresponding to that, and whether or not convergenence happened (we want convergence=0) or something else happened, like we did not find an answer using the given number of iterations (the default is \(500\) iterations.). A list of control parameters can be passed to optim including maximum iterations, maxit, and an overall scaling fnscaling (use control = list(fnscaling = -1) for maximizing the function instead of minimizing it.). See the help page for more details.

Your task is to write a function that finds root \(k\) of a positive number using numerical optimzation. Hint: this function is going to use optim to find the optimal solution. The solution is going to be the variable \(r\) that minimizes a cost function obj.f. The obj.f function should take three numbers, say, x and r and k, and then raise r to the power of k, and compare it with x. To make it give you the answer, your returned value should be something like the square of the difference. E.g., obj.f = function(r,number, k) cost = (number - r^k)^2

As a bonus question, you are advised to improve your code so that it can handle a vector of number and return root \(k\) of all the elements as vector in return. You should not use any loops for this and should take advantage of the fact that optim can take a vector as an input.

Given that the default method is relatively slow, you should either increase the number of iterations or use method = "BFGS".
You can use \(1\) as a starting point.
Use stop to produce an error if the input in not positive (or all positive in case the input is a vector).
NB: R Markdown may by default stop if there is an error. This, of course, is not the behavior you want to see if you want to show how your code produces error messages. The solution is to use the option error=TRUE when you wrap your code with tripple backticks: ```{r error=TRUE} lines-of-code ```
```{r}
The following provides some scaffolding.
objf = function(r, number , k) 
{
  # how good a candidate is r for root k of number
  measure =   
    return (measure)
}

rootk = function(number, k, start = NULL)
{
  ...
  if (is.null(start)) # assign a starting point if it is not given
    start = # starting point
    # call optim
    
    # check if it is working
    
    return(r)
}
```