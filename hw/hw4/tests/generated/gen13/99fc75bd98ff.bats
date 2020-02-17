load ../../harness

@test "99fc75bd98ff" {
  check 'if (4  +    HT     < 2   +     -4     ∧   3    *-2    =  z*     2) then skip      else 
 skip' '⇒ skip, {}'
}
