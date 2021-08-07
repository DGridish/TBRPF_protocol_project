erl -sname master -setcookie aaa

erl -sname slave1 -setcookie aaa
erl -sname slave2 -setcookie aaa
erl -sname slave3 -setcookie aaa
erl -sname slave4 -setcookie aaa

cd("/home/dgridish/IdeaProjects/TBRPF_Protocol/src").
c(masterNode).
c(slaveNode).
c(elementNode).

masterNode:start_link(['slave1@dgridish', 'slave2@dgridish', 'slave3@dgridish', 'slave4@dgridish'],[1, 2, 3, 4]).

net_adm:ping('slave2@dgridish').

