# cloudy

...an experiment in Haskell Cloud (distributed-process)

## Build and run

The script file _run.sh_ sets up slaves and master, e.g.

	stack build

	stack exec cloudy-exe "slave" "127.0.0.1" "10502" &
	stack exec cloudy-exe "slave" "127.0.0.1" "10503" &
	stack exec cloudy-exe "slave" "127.0.0.1" "10504" &
	stack exec cloudy-exe "slave" "127.0.0.1" "10505" &

	sleep 1

	stack exec cloudy-exe "master" "127.0.0.1" "10501" 2000000 1000000 0
	# (run with 2 sec SEND period, 1 sec GRACE period, SEED=0)

## Master node and Ring of slave nodes

The Master node

* spawns a ring of slave nodes

* initialises each slave with its peer node Ids

* sends a single "kickstart message" to a single node to kick off the sending process (the first message index is 1)

* when a slave receives a kickstart message or a message from a "parent node" (the node to it's left in the ring),
  then it sends a message to its peers (with index i+1 and a new random number)

* nodes are triggered by "parent nodes" to generate the next message to broadcast to peers - in this way
  nodes generate messages with increasing index i in a decentralised fashion

## Slave States

The master node moves the slave nodes through different states by sending them messages:

* START: first the Master spawns n slaves with status START

* SEND: move slaves into SEND mode by sending each slave their peer IDs. Slaves have a preference for receiving messages (a slave checks for incoming messages (for a short timeout - TICK) before sending messages. This is to ensure that status changes or messages from peers are quickly processed.

* GRACE: after the SEND period, the master moves the slaves into GRACE mode (slaves only receive messages, but no longer send any)

* STOP: after the GRACE period, the master moves slaves into STOP STOP mode (slaves stop and print their results)

## Open Issues

### Naive Ring Model

Explore more optimal ring models than my first implementation...

### Messages are not being guaranteed

To ensure that peer nodes move their state forward in lockstep, we need to guarantee message delivery (to the extent possible):

* when a node receives a message, it should send back a confirmation message
* when a node broadcasts a message, it must keep track of all recipients. All confirmations should be tallied, and only  
  when all confirmations are counted should the node include the original sent message in its "result".
*	also, once a broadcasted message is confirmed in this way, a MessageApproved must be sent to all original recipients to allow
  them to include the message in their "result"

### Nodes are not resilient to failure

Currently, when a master fails the process will be corrupted. Also, when a slave fails, it will not be restarted.
