load ../../harness

@test "085fa79259e6" {
  check 'if (¬true)   then 
skip     else 

 
 {Lc:=  2    *1  ;
 Mb   := jD   + -1}  ' '⇒ Lc := (2*1); Mb := (jD+-1), {}
⇒ skip; Mb := (jD+-1), {Lc → 2}
⇒ Mb := (jD+-1), {Lc → 2}
⇒ skip, {Lc → 2, Mb → -1}'
}
