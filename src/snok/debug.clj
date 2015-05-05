(ns snok.debug)

; A useful macro found on Stack Overflow:
; http://stackoverflow.com/a/2352280/493122
; I feel like I'm going to need it.
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
