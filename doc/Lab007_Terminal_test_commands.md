main !!

erl -name main@IP_ADDRESS0 -setcookie cookie

cd("../TBRPF_protocol_project/src").

c(mainNode).
c(qNode).
c(elementNode).

mainNode:start_link(['q1@IP_ADDRESS1', 'q2@IP_ADDRESS2', 'q3@IP_ADDRESS3', 'q4@IP_ADDRESS4'],[1,2,3,4]).


erl -name main@IP_ADDRESS0 -setcookie cookie
erl -name q1@IP_ADDRESS1 -setcookie cookie
erl -name q2@IP_ADDRESS2 -setcookie cookie
erl -name q3@IP_ADDRESS3 -setcookie cookie
erl -name q4@IP_ADDRESS4 -setcookie cookie




