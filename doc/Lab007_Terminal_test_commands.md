main !!

erl -name main@132.72.104.86 -setcookie aaa

cd("/home/cse_student/Desktop/TBRPF_protocol_project-main/src").

c(mainNode).
c(qNode).
c(elementNode).
c(protocolTBRPF).

mainNode:start_link(['q1@132.72.104.56', 'q2@132.72.104.74', 'q3@132.72.104.213', 'q4@132.72.104.203'],[1,2,3,4]).



erl -name main@132.72.104.86 -setcookie aaa
erl -name q1@132.72.104.56 -setcookie aaa
erl -name q2@132.72.104.74 -setcookie aaa	
erl -name q3@132.72.104.213 -setcookie aaa
erl -name q4@132.72.104.203 -setcookie aaa




