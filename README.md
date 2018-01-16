# eravan

A chaotic lisp compiler in Clojure.

## Usage

Very basic. You can use the `run` function to evaluate code.

```
(run "(define r 10) (* pi (* r r))")
```

## TODO
1. Change eval such that it's easier to add new functionality without bloating up the function. Ideally in a data directed style (like in SICP).

2. Add more basic functions in the global environment.

3. In case of procedure call, create and execute in a new environment.

## License

MIT