erl -sname main -setcookie aaa

erl -sname q1 -setcookie aaa
erl -sname q2 -setcookie aaa
erl -sname q3 -setcookie aaa
erl -sname q4 -setcookie aaa

cd("/home/dgridish/IdeaProjects/TBRPF_Protocol/src").
c(mainNode).
c(qNode).
c(elementNode).
c(protocolTBRPF).

mainNode:start_link(['q1@dgridish', 'q2@dgridish', 'q3@dgridish', 'q4@dgridish'],[1, 2, 3, 4]).

net_adm:ping('q2@dgridish').
