load ../../harness

@test "6353d856659b" {
  check 'if (3     +     -3    =3* B6 ∨  true) then skip else x     :=    -1-   q     ' '⇒ skip, {}'
}
