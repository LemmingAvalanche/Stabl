A concatenative language that currently _aspires_ to be functional, interpreted, optionally typed, homoiconic and highly interactive. 

What is meant by optionally typed is that you should be able to decide how strict words (functions) are when it comes to statically enforcing stack effects: the loosest stack effect is when a word may consume any number of items on the stack (i.e. it can change the whole program), and return any number of items back to the stack. This will probably be achieved by not supplying any type definition of a word. Something slightly more strict is if the function is restricted to consuming only n items from the stack, or if it is restricted to returning only n items to the stack but it may consume any number of items of a stack (I think this is equivalent to a variadic function?) The strictest effect is a word has to consume n items off the stack and return m (which makes the stack effects as statically typed as the "function effects" (number of return values, number of return values, if applicable) of your usual conventional language). 

The above optional typing should hopefully help in making the language highly interactive, since it allows you to effectively manipulate the whole "call stack" (simply the value stack, with an (optional) return stack) no matter the position in the (suspended) execution.