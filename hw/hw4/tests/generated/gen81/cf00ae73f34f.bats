load ../../harness

@test "cf00ae73f34f" {
  check 'if (2< z+     0   ∧     3  *  z   =-2 --1)     then skip     else   z    :=  x  *   2  ' '⇒ z := (x*2), {}
⇒ skip, {z → 0}'
}
