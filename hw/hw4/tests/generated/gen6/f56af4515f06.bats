load ../../harness

@test "f56af4515f06" {
  check 'z:=p     -    z;z    :=   x *z   ' '⇒ skip; z := (x*z), {z → 0}
⇒ z := (x*z), {z → 0}
⇒ skip, {z → 0}'
}
