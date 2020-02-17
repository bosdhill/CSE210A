load ../../harness

@test "dd8040631f7c" {
  check 'HG  :=  z -     -3   ;
 TN :=   y   *     -4 ' '⇒ skip; TN := (y*-4), {HG → 3}
⇒ TN := (y*-4), {HG → 3}
⇒ skip, {HG → 3, TN → 0}'
}
