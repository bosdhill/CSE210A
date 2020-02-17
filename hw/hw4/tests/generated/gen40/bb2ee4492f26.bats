load ../../harness

@test "bb2ee4492f26" {
  check 'while -4     -  x<N     -     2   ∧   1   -    a  < 3     -   y   do 
  y  :=  4- z ' '⇒ y := (4-z); while (((-4-x)<(N-2))∧((1-a)<(3-y))) do { y := (4-z) }, {}
⇒ skip; while (((-4-x)<(N-2))∧((1-a)<(3-y))) do { y := (4-z) }, {y → 4}
⇒ while (((-4-x)<(N-2))∧((1-a)<(3-y))) do { y := (4-z) }, {y → 4}
⇒ skip, {y → 4}'
}
