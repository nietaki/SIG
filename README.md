To avoid running the long-running tests run them using

```
sbt "test-only test.DeckSpec"
```

to include long running tests use

```
sbt -mem 10000 test
```
(if you have the 10GB memory available)
