# programming-in-haskell exercises

Some exercises from Graham Hutton's book "Programming in Haskell".

## Running tests

While working on this I discovered the amazing [sensei](https://github.com/hspec/sensei) tool which automatically runs tests whenever you save a file. It's incredibly fast, relying on `ghci` to reload and execute modules without doing a full compilation step.

```
stack exec sensei test/Spec.hs
```
