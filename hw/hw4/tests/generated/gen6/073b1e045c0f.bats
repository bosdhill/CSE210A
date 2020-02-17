load ../../harness

@test "073b1e045c0f" {
  check 'if (false    ∨  x   <    2    -   x)   then    skip else   MB:=y    *    y' '⇒ skip, {}'
}
