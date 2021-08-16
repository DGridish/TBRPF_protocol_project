# TBRPF Protocol Project
 Concurrent and Distributed Systems implemented in Erlang OTP 
</br> </br>
## TBRPF Project
The project describes communication conduct between mobile communication elements via TBRPF protocol.
</br> 
Elements can move according to a predefined mathematical motion model.
</br>
The traffic area is a square with a 2 km long edge. This area is divided into four quarters, each square with a side of 1 km.
</br>
Elements can move between the four quarters while in motion.
</br>
## TBRPF Protocol
Topology broadcast based on reverse-path forwarding (TBRPF) is a proactive routing protocol.
</br> 
TBRPF is a full-topology link-state protocol: each node is provided with the state of each link in the network (or within a cluster if hierarchical routing is used).
</br> 
Each link-state update is broadcast reliably along a dynamic min-hop-path tree rooted at the source U of the update
</br> 
Each update is sent along a single path to each node, resulting in improved efficiency.
</br>
## The System Components
### 1. Main Node
Use gen_server to create a client-server relation - OTP Design Principles.
</br> 
Start the Q nodes and monitor them.
</br> 
Provides the information needed for Q nodes when a request is sent.
</br> 
Responsible for handling crashed nodes, If Q node crashes, responsible for transferring the elements to another Q node.
</br> 
Sends a request to send a message and prints information to the screen periodically.
</br> 
Two ETS: Q’s, All elements. 
### 2. Q Node 
Use gen_server to create a client-server relation - OTP Design Principles.
</br>
Start the element nodes.
</br>
Provides the information needed for Element nodes when a request is sent.
</br>
Ability to contact other nodes requesting to send elements not in its area. 
</br>
ETS: Own elements. 
### 3. Element Node 
Use gen_server to create a client-server relation - OTP Design Principles.
</br>
movement model: Direction and speed. 
</br>
Responsible for maintaining an up-to-date list of neighbors
</br>
Find the shortest path for messaging
</br></br>
## Activation Instructions
### Multiple Computers
</br>
1. For each Q node computer, open a terminal and enter the following:
erl -name qx@IP_ADDRESS -setcookie cookie
Where x is the computer's number and IP_ADDRESS is the computer IP address.
</br>
2. For the main computer, open a terminal and enter the following:
erl -name main@IP_ADDRESS -setcookie cookie
Where IP_ADDRESS is the computer IP address.
</br>
3. In each terminal, type in erlang shell the following to compile the files:
cd("../TBRPF_protocol_project/src").
</br>
c(mainNode).
</br>
c(qNode).
</br>
c(elementNode).
</br>
4. In the main computer enter the command:
mainNode:start_link(['q1@IP_ADDRESS1', 'q2@IP_ADDRESS2', 'q3@IP_ADDRESS3', 'q4@IP_ADDRESS4'],[1,2,3,4]).
</br>
### Single Computer 
</br>
1. For each Q node computer, open a terminal tab and enter the following:
erl -sname qx -setcookie cookie
Where x is the computer's number.
</br>
2. For the main computer, open a terminal and enter the following:
erl -sname main -setcookie cookie
</br>
3. In each terminal tab, type in erlang shell the following to compile the files:
cd("../TBRPF_protocol_project/src").
</br>
c(mainNode).
</br>
c(qNode).
</br>
c(elementNode).
</br>
4. In the main computer enter the command:
mainNode:start_link(['q1@HOST_NAME', 'q2@HOST_NAME', 'q3@HOST_NAME', 'q4@HOST_NAME'],[1, 2, 3, 4]).
</br>
### Video simulation
you can watch a video simulation by clicking <a href="">here</a>.
</br>
### Creator
*Dan Gridish*  
Computer Engineer, Ben-gurion University, Israel
