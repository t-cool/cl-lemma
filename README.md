# cl-lemma

Englsh lemmatizer in Common Lisp

## Installation

```
$ ros install t-cool/cl-lemma
```

## Usage

### Roswell Script

```
$ lemma leaves verb
leave
$ lemma leaves noun
leaf
$ lemma best adj
good
$ lemma best adv
well
```

### REPL

```  
* (ql:quickload :cl-lemma)
* (cl-lemma:lemma "went")
"go"
* (cl-lemma:lemma "leaves" :noun)
"leaf"
* (cl-lemma:lemma "leaves" :verb)
"leave"
```

## Author

t-cool

This project is ported from [yohasebe/lemmatizer](https://github.com/yohasebe/lemmatizer).

## License

Licensed under the MIT license.
