1. Need type info in view
  - need either:
    1. a parametrized version of Expr; or
    2. a foldr where type info is passed in
2. When a block is dragged off, types should be re-inferred
  - both parent and dragged item should become as general
    as their type signatures allows
3. When hovering over a hole, types should be re-inferred
  - both parent and dragged item should specialize to be the
    most general type when composed
  - if InferenceError, hole glow red,
    and a tooltip is displayed that shows the mismatch
4. Tap the type label to choose to infer
  - most general type (auto mode); or
  - to restrict (manual mode)

=== Replacing constructors with foldr

foldr cons1 cons2 cons3 . map f = foldr (cons1 . f) (cons2 . f) (cons3 . f)

ex. for Lists:
foldr cons nil . map f
  == foldr (cons . f) nil
  == foldrMap f cons nil
so we can do away with maps as long as we have foldr

- Advantages of using foldr only, and not making a separate parametrized type
  1. If the extra args need to satisfy constraints depending on the original ones,
  using a foldr that the constraints are satisfied

- Advantages of using a foldr over a parametrized type:
  1. Data can be generated once, then shared by two different functions.
    Whereas if we use a foldr in two different places, the extra data needs
    to be generated twice

ex:
With parametrized types:
  let typedExpr = infer expr
  in (foldr f typedExpr, foldr g typedExpr)
With foldr:
  (foldrWithInferredType f expr, foldrWithInferredType g expr)
