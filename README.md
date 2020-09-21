# computor-v1

A polynomial solver.

It can solve up to 2nd degree polynomials. It tries to reduce expressions to normalized forms but it doesn't always succeed.

Example for calculating root naturally:

```sh
$ ./computor-v1 "X * X = 36"
Equation of degree 2
Solved equation, the discriminant is 144.0 solutions are X = 6.0 and X = -6.0
```

It can show you the steps it uses (pretty colours in terminal):
```sh
$ ./computor-v1 -sc "5*X^2 + 3*X + 5 = 10"
┌─────────────────────────────────┐
│ Using simple solution algorithm │
└─────────────────────────────────┘
    |
    | (((5 * (X ^ 2)) + 3X) + 5) = 10
    |
Reduce both sides
    |
    | ((5 + 3X) + (5 * (X ^ 2))) = 10
    |
Move all terms to rhs
    |
    | (((5 + (10 * -1)) + (5 * (X ^ 2))) + 3X) = 0
    |
Reduce both sides
    |
    | ((-5 + (5 * (X ^ 2))) + 3X) = 0
    |
Equation of degree 2
Solved equation, the discriminant is 109.0 solutions are X = 0.744030650891055 and X = -1.3440306508910551
```
