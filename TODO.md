* Name the "packetBlock" function more appropriately
  * forkCh
  * cloneCh
* Event handlers:
  * Sholud be able to just check for an event non-blockingly (evtAsdfGhjk ...)
    * Read through all available messages
    * Return a list of values returned by the handler
  * Combinator for blocking until a message arrives (waitFor)
  * Combinator for loop on all messages and deliver result on an appropriate message (loopFor)
  * Combinator for a promise / future (promise, deliver, tryDeliver, canDeliver)
