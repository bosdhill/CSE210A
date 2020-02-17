load ../../harness

@test "1ba83706ef87" {
  check 'z    :=   1   + x ;


 z:=   z     *     z  ' '⇒ skip; z := (z*z), {z → 1}
⇒ z := (z*z), {z → 1}
⇒ skip, {z → 1}'
}
