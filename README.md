# cl-lemma

Englsh lemmatizer in Common Lisp

## Installation

```
$ ros install t-cool/lemmatizer
```

## Usage

```  
$ ros run
* (ql:quickload :cl-lemma)
* (cl-lemma:lemma "went")
"go"
* (cl-lemma:lemma "gone")
"go"
```

## Author

t-cool

Note: This project is ported from [yohasebe/lemmatizer](https://github.com/yohasebe/lemmatizer).

## License

Licensed under the MIT license.
