# Basic Prolog interpreter implemented in Haskell
Course project in functional programming

# Build

# Main

## Tokens

A program has the following syntax:
```
<identfier> := <lowercase_letter>[<letter_or_number>]
<variable> := <uppercase_letter>[<letter_or_numer>]
<constant> := <identifier>
<term> := <constant> | <variable> | <identifier>(<term>, [<term>])
<atom> := <identifier>(<term>, [<term>])
<fact> := <atom>.
<rule> := <atom> :- <atom>[, <atom>].
```
Note: underscores are not supported.

Valid operators are rule operator `:-` , comma operator `,`, dot operator `.`, brackets `(`, `)`.
Valid statements are rules, facts and queries.

## Parsing
Parsers are applied in the following order:
Firstly, the file is divided into an array of symbols that go through `parserA`. `parserA` reads strings and operators.
`parserB` removes unnecessary spaces.
`parserC` reads terms with nesting.
`parserD` reads atoms without nesting, which means nested atoms are treated as terms. This is not an issue when converting to intermediate representation.
`parserE` reads facts, rules and queries.

The `ast` function generates abstract syntax tree from the tokenized statements.
The `compileFile` function parses a text file and returns a Program object of AST. If the parsing is unsuccessful, then the function returns Nothing.

## Unification
The intermediate representation data types start with either `P` or `PL`.
The main difference between the AST and IR is that terms can be either an atomic formula (a function with arguments, or a constant) or a variable. Constants are represented as functions with no arguments. 

A unifier (sometimes denoted in the code as `mgu` as most general unifier) is a set of equation - `PLEquation` (or `PLSubstitution`). Unification is the process of trying to find a substitution that makes two terms equal. The unification algorithm pseudocode is as follows:

```
Initialise the MGU to an empty unifier
Push T1 = T2 to the stack
While the stack is not empty
	Pop X = Y from the stack
	       case: X is a variable AND X does not occur in Y
        	Create unifier U such that X = Y
                Apply U to MGU
                Add U to MGU
                Apply U to stack
        case: Y is a variable AND Y does not occur in X
        	Create unifier U such that Y = X
                Apply U to MGU
                Add U to MGU
                Apply U to stack
        case: X and Y are identical constants or variables
        	do nothing
        case: X is of form p(a0,..,an) and Y is of form p(b0,..,bn)
		        For m = 0 to n
                	push am = bm to the stack
        default case:
        	Failure
Return the MGU
```
_The algorithm is borrowed from the book The Art of Prolog._

## Resolution

`Database` represents AST converted into IR.

The resolution algorithm is as follows:
```
Input: Goal and program P
Output: Instance of G that is logical consequence of P

tree = Tree Empty [Tree $ Node query [] $ []]

resolve tree =
    solutions []
    newTree = tree 
    foreach (child in tree.children)
        nodes = genn child
        foreach (node in nodes)
            if node.unifier is Nothing then
                remove node from nodes
            else if node.unifier is [] then
                add node.mgu to solutions
            else continue
        newTree = add nodes to newTree
    return solutions ++ resolve newTree
```

Algorithm for generating children nodes (`genn`):
```
foreach pclause{term = body} in DB:
     let u = plUnify query term in
     case u of
         Nothing -> continue
         Just _ -> return Node with u and newQuery = apply u to query
goal reduction
```

## Testing
_Note: to-do: automize unit tests._
