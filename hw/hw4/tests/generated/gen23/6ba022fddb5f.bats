load ../../harness

@test "6ba022fddb5f" {
  check 'if (-3 *   y     =   y* -2    ∧     true)  then iy    :=    z  *zY      else   skip     ' '⇒ iy := (z*zY), {}
⇒ skip, {iy → 0}'
}
