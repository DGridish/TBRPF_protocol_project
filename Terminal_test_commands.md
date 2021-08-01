erl -sname master -setcookie aaa

erl -sname slave1 -setcookie aaa
erl -sname slave2 -setcookie aaa
erl -sname slave3 -setcookie aaa
erl -sname slave4 -setcookie aaa

cd("/home/dgridish/IdeaProjects/TBRPF_Protocol/src").
c(masterNode).
c(slaveNode).

masterNode:start_link(['slave1@dgridish', 'slave2@dgridish'],[1,2]).

net_adm:ping('slave2@dgridish').

