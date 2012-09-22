(in-package :com.clearly-useful.generic-collection-interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (member :bordeaux-threads *features*)
	     (not (member :allegro-cl-trial *features*)))
    (pushnew :com.clearly-useful.threading-supported *features*)))
