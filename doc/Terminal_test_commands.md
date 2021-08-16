erl -sname main -setcookie cookie

erl -sname q1 -setcookie cookie
erl -sname q2 -setcookie cookie
erl -sname q3 -setcookie cookie
erl -sname q4 -setcookie cookie


cd("../TBRPF_Protocol/src").
c(mainNode).
c(qNode).
c(elementNode).


mainNode:start_link(['q1@host_name', 'q2@host_name', 'q3@host_name', 'q4@host_name'],[1, 2, 3, 4]).
