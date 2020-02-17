load ../../harness

@test "3e982432247b" {
  check 'if (¬(p  *     y    <    z *   -4))  then    z     :=  3 --4      else 

 x   :=     z   -   -4 ' '⇒ z := (3--4), {}
⇒ skip, {z → 7}'
}
