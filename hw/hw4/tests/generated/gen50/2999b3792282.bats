load ../../harness

@test "2999b3792282" {
  check 'while false  ∨y     -   3     =zR  +   2     do {skip;
y     :=   x     -     x}    ' '⇒ skip, {}'
}
