# simple-actors
The actor model implemented with closures.

# Creating an actor

    CL-USER> (defvar *result* nil)
    *RESULT*
    CL-USER> (defparameter *actor* (simple-actors:actor (&rest numbers)
    				  (setf *result*
    					(loop for n in numbers
    					   collect (* n 2)))))
    *ACTOR*
    CL-USER> *actor*
    #<SIMPLE-ACTORS:ACTOR {10069D2963}>

Actors can be sent messages that match their lambda-list. These messages go into a queue, and the actor's body is evaluated with each message in order:
    
    CL-USER> (simple-actors:send *actor* 1 2 3 4 5 6)
    NIL
    CL-USER> *result*
    (2 4 6 8 10 12)

Each actor has a thread associated with it:

    CL-USER> (bt:all-threads)
    (#<SB-THREAD:THREAD "Actor" RUNNING {1006919D53}>
     #<SB-THREAD:THREAD "finalizer" RUNNING {1001B80053}>
     #<SB-THREAD:THREAD "repl-thread" RUNNING {100250FF93}>
     #<SB-THREAD:THREAD "auto-flush-thread" RUNNING {100250FDA3}>
     #<SB-THREAD:THREAD "swank-indentation-cache-thread" RUNNING {1004458183}>
     #<SB-THREAD:THREAD "reader-thread" RUNNING {1004458083}>
     #<SB-THREAD:THREAD "control-thread" RUNNING {1004450D73}>
     #<SB-THREAD:THREAD "Swank Sentinel" RUNNING {1004164873}>
     #<SB-THREAD:THREAD "main thread" RUNNING {10005E8203}>)
 
 The actor's thread terminates when it is sent the `simple-actors:stop` message. This message is not passed to the actor's body.
 
    CL-USER> (simple-actors:send *actor* 'simple-actors:stop)
    NIL
    CL-USER> (bt:all-threads)
    (#<SB-THREAD:THREAD "finalizer" RUNNING {1001B80053}>
     #<SB-THREAD:THREAD "repl-thread" RUNNING {100250FF93}>
     #<SB-THREAD:THREAD "auto-flush-thread" RUNNING {100250FDA3}>
     #<SB-THREAD:THREAD "swank-indentation-cache-thread" RUNNING {1004458183}>
     #<SB-THREAD:THREAD "reader-thread" RUNNING {1004458083}>
     #<SB-THREAD:THREAD "control-thread" RUNNING {1004450D73}>
     #<SB-THREAD:THREAD "Swank Sentinel" RUNNING {1004164873}>
     #<SB-THREAD:THREAD "main thread" RUNNING {10005E8203}>)
    
