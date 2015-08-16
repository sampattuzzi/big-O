(* Meta counting functions *)
val funCallCount = ref 0;
fun call (_, _) = funCallCount := !funCallCount + 1;

val thread_id = ref 0;
fun getFunctionCount f t d =
let
  open Thread.Thread;
  open Thread.ConditionVar;
  open Thread.Mutex;
  val id = Int.toString (!thread_id)
  val start = ref false
  val startCond = conditionVar ()
  val startMutex = mutex ()
  val finish = ref false
  val finishCond = conditionVar ()
  val finishMutex = mutex ()
  val thread =
    Thread.Thread.fork((fn () => 
      (
        lock startMutex; 

        if not (!start) then wait (startCond, startMutex) else ();

        f d; 

        lock finishMutex;
        finish := true;
        signal finishCond;
        unlock finishMutex
      ) handle Interrupt => 
      (
        lock finishMutex; 
        finish := true;
        signal finishCond;
        unlock finishMutex
      )
      ), []);
  open Time
in
  funCallCount := 0;

  thread_id := Int.+ (!thread_id,1);

  PolyML.DebuggerInterface.setOnEntry (thread, SOME call);

  lock startMutex;
  start := true;
  signal startCond;

  lock finishMutex;
  unlock startMutex;

  if waitUntil(finishCond, finishMutex, (now ()) + t) then
    (
    SOME (!funCallCount)
    )
  else
    (
      (
      if (not (!finish)) 
      then
        (
        interrupt thread;
        wait(finishCond, finishMutex)
        )
      else
        ()
      );
      NONE
    )
end;

