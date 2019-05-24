# cl-lemma

English lemmatizer in Common Lisp

## Installation

```
$ ros install t-cool/cl-lemma
```

## Usage

You can lemmalize a word by `(cl-lemma:lemma <word> <pos> )`.

```
* (cl-lemma:lemma "leaves" :noun)
"leaf"
* (cl-lemma:lemma "leaves" :verb)
"leave"
```

If you ommit a pos, cl-lemma will look up a lemma in the following order: Noun, Verb, Adjective, and Adverb.

```  
* (cl-lemma:lemma "leaves")
"leaf"
* (cl-lemma:lemma "better")
"good"
```

### Roswell Script

If you add `~/.roswell/bin` to PATH, you can use lemma command.

```
$ lemma leaves verb
leave
$ lemma leave
leaf
```

## Author

t-cool

This project is ported from [yohasebe/lemmatizer](https://github.com/yohasebe/lemmatizer).

## License

Licensed under the MIT license.
