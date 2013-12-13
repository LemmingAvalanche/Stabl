Compiling the REPL

The code can be compiled by running:

> ghc REPL.hs

if ghc is installed. This will make an executable, which can be run by:

> ./REPL

Which will give the prompt of the REPL. The REPL can will exit on any errors encountered, such as if you try to give a syntactically incorrect code snippet or try to call a function that is not defined. 

Using the REPL

You can type in integers and one of the built in words:

pop
dup
swap
rot
over
apply
add
minus
mul
div

The REPL will print out the contents of the stack when you give it an int or a word (or abruptly exit if you give it words that are misspelled or don't have enough elements on the stack to use). You can define your own words with the following syntax:

def <name> {<tokens here>}

It is important to not give any extra whitespace, as the parser will programs that have too much whitespace (such as whitespace right after a word definition).
