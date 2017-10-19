# cloudy

...an experiment in Haskell Cloud (distributed-process)

## Build and run (local)

	cabal run 2000000 2000000 0

(run with 2 sec SEND period, 2 sec GRACE period, SEED=0)
	  
## Notes

### Master moves slaves through different states

The master node moves the slave nodes through different modes by sending them messages:

* START: first the Master spawns n slaves with status START

* SEND: move slaves into SEND mode by sending each slave their peer IDs. Slaves have a preference for receiving messages (a slave checks for incoming messages (for a short timeout - TICK) before sending messages. This is to ensure that status changes or messages from peers are quickly processed. 

* GRACE: after the SEND period, the master moves the slaves into GRACE mode (slaves only receive messages, but no longer send any)
* STOP: after the GRACE period, the master moves slaves into STOP STOP mode (slaves stop and print their results)

### Work Queue
* the Master spawns a Work Queue to allow all slaves to PULL work on demand ("work stealing" allows the slaves to be more responsive by sending only when they are not receiving)
* the work queue listens for Job requests from slave nodes: a slave node asks for a list of tuples to send to its peers [(index, randomNum)]
* in this way the work queue serves the slaves
	* unique, increasing indices
	* for each index, random number to go with it
