# cl-lemma

Englsh lemmatizer in Common Lisp

## Installation

```
$ ros install t-cool/cl-lemma
```

## Usage

You can lemmalize a word by `(cl-lemma:lemma "word")`.

```  
* (ql:quickload :cl-lemma)
* (cl-lemma:lemma "went")
"go"
```

You can get the exact result by specifying its pos(part of speech) at the end.

```
* (cl-lemma:lemma "leaves" :noun)
"leaf"
* (cl-lemma:lemma "leaves" :verb)
"leave"
```

### Roswell Script

If you add `~/.roswell/bin` to PATH, you can use lemma command.

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

## Author

t-cool

This project is ported from [yohasebe/lemmatizer](https://github.com/yohasebe/lemmatizer).

## License

Licensed under the MIT license.
