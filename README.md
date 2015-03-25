# mantl

A wrapper around ANTLR4.

## Usage

First you need to generate and compile the antlr4 grammars. See lein-antlr4 for a leg-up on that.
Now you've got your generated Grammar in package with startRule.

Then, when you've got mantl on your classpath:

```clojure
(use 'mantl.core)
;If you don't have a package just leave out the argument package
(def grammar-reader (lexer-parser startRule Grammar package))
;txt is some text you have in language Grammar
(def ast (grammar-reader txt))
```

## License

Copyright © 2015 Edward Ross

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
