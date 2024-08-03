# Error Record


## not a procedure

error:

```
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 0
  context...:
   thelittlelearner\malt\tools\A-hypers.rkt:43:13
   body of "the-little-learner\8TheNearerYourDestinationTheSlowerYouBecome\main.rkt"
```

fix:

`[...]` is `(tensor ...)`

## '((tensor -inf.0 -inf.0) -inf.0)

error:

```
'((tensor -inf.0 -inf.0) -inf.0)
```

fix in `velocity-u`

change from

```scheme
(let ([v (+ (* mu (ref big-p 1)) (* alpha grad)
```

to

```scheme
(let ([v (- (* mu (ref big-p 1)) (* alpha grad)
```