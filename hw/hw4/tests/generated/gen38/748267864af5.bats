load ../../harness

@test "748267864af5" {
  check 'if (3     *x   <    L*3    ∧    x *    Wd=     4    -     2)      then 
   y:=    x    +    -1 else skip' '⇒ skip, {}'
}
