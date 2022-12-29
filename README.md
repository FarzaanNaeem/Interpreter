# Interpreter
This is an interpeter that evaluataes a stack based language. It takes a string as an input for all of the list of commands as well as a text file where the output will go to. Each command will be separated by a new line. This was all written in the language OCaml. The language has the following grammar:  
[digit]::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9  
[int]::= {-}[digit]{[digit]}  
[char]::= [uchar] | [lchar]  
[uchar]::= A | B | C | D | ... | Z  
[lchar]::= a | b | c | d | ... | z  
[string]::= "{[char]}"  
[name]::= <lchar>{<char>|_|<digit>}  
[const]::= [int] | [string] | [name]  
[com]::= Quit  
| Push [const]  
| Pop    
|Add  
|Sub (subtraction)  
| Mul (multiplcation)  
| Div (division)  
| Swap (swap the top 2 values off the stack)  
| Neg (multiply an in by -1)  
| Concat (concat two strings)  
| And  
| Or  
| Not  
| Equal  
| Lte (less than or equal to)  
| Local [name] (local variable)  
| Global [name] (global variable)  
| Begin [prog] End  
| IfThen [prog] Else [prog] End  
| InjL (union value)  
| InjR (another union value)  
| CaseLeft [prog] Right [prog] End   
| Tuple [int] (int being the length of the tuple)  
| Get [int] (getting a tuple value at index of [int] starting from 0)  
| Fun [name][name] [prog] (first [name] is func name and second [name] is argument)  
| Call  
| Return   
[prog]::= [com][prog] | [com]  

An example of a valid input for the list of commands would be: Push 1\nPush 2\nAdd\nQuit. This would result in the output to the text file created to be 3.
