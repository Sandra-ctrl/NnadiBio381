Anatomy of a user defined function

```
1. functionName <- function(parX=defaultX,parY=defaultY){
# curly bracket marks the start of function body
# lines of R code and annotations
# may also call other functions
# may also create functions
# may define local variables

return(singleObject) # returns a single object (could be a list)
}
# curly bracket marks the end of the function body

functionName # will print the function body
functionName() # will run the function with the default values
functionName(parX=myMatrix,parY="Order",parZ=c(0,3,3,4))

​```
```

Stylistic Conventions for writing functions

- use prominent hash character fencing at start and finish
- give a header with function name, description, inputs and outputs
- names inside function can be fairly short and generic
- functions should be short and simple, no more than about a screenful
- if too long or complex, break it up into several functions
- provide default values for all function arguments
- ideally use random numbers as default values for rapid testing



Scoping

- global variables visible to all parts of the code; declared in the main body
- local variables visible only within a function; declared in function or passed to the function through parameters
- functions can see global variables, but should not use them
- global environment cannot see variables in the function environment
- what happens in the function stays in the function