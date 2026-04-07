# Repl calculator

**Supported values:** `Double`

**Supported operations:** `+`, `-`, `*`, `/`, and parentheses `()`
  
**Modules:**
`Main` - entry point
`Repl` - IO function that evaluates user input
`Types` - module that describes all data types used in project
`Parser` - parses query string to expression tree (via functions described later)
`Tokenize` - converts query string into token sequence
`Syntax` - converts token sequence into expression tree
`Eval` - evaluates expression tree and get the result of calculation

Currently only 2/6 modules are declared as `total` by default, other are `covering`. Haven't figured how to fix that yet `:(`
