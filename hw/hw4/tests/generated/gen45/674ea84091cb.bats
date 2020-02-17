load ../../harness

@test "674ea84091cb" {
  check 'z     :=  x+ z ;
  
 f :=   z     *  4 ' '⇒ skip; f := (z*4), {z → 0}
⇒ f := (z*4), {z → 0}
⇒ skip, {f → 0, z → 0}'
}
