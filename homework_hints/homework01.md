# General hints

You may like to use these functions from standard library

```haskell
sum :: (Foldable t, Num a) => t a -> a
reverse :: [a] -> [a]
```

# Exercise 1

`toDigits` can be implemented in term of `toDigitsRev` (and is easier to do like that).

**Bonus**: implement `toDigits` in a way that does not involve reversing the output list.

# Exercise 2

Try to implement the function that doubles from `head` to `last` first. Then implement
the `last` to `head` version in term of the former function.

```haskell
doubleEveryOtherLeftToRight :: [Integer] -> [Integer]
-- implementation

doubleEveryOther l = _E -- an expression using doubleEveryOtherLeftToRight
```

**Bonus**: implement `doubleEveryOther` in a way that does not involve reversing a list

# Bonus `validate`

Knowing that:

- `sum l == sum (reverse l)` because the addition is commutative and associative
- `toDigits n == reverse (toDigitsRev n)`

Optimize `validate` by reducing the number of transformations to get to the result
