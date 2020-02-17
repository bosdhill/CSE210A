load ../../harness

@test "bdd8e7f6f5ab" {
  check 'if (¬(y-y< x *y))     then 
   skip    else  y   :=    x   *  -1     ' '⇒ skip, {}'
}
