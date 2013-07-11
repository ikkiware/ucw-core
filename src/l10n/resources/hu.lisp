(in-package :lang)

(defresources hu
  (file-last-modification-timestamp<> (file)
    (<:as-html "Frissítve: ")
    (aif (file-write-date file)
         (timestamp<> (local-time:local-time :universal it))
         (<:span :class "missing-file"
                 "Hiányzik a fájl!")))
  (your-session-has-expired "Lejárt a biztonsági idő vagy törlődott a kapcsolat a szerverrel"))

(define-js-resources hu
  (confirm-pending-changes
   #.(format nil "Az oldalon kimentetlen adatok vannak amelyek elveszhetnek.~%Folytassuk a műveletet?"))

  (warning.session-will-time-out-at "Le fog telni a biztonsági idő '${deadline}'-kor!")
  (warning.session-timed-out "Letelt a biztonsági idő, a kapcsolat megszakadt!")

  (unknown-error-while-processing-server-answer
   "Hiba történt a kérés feldolgozása közben. Próbálja meg frissíteni az oldalt, és ha a hiba ezután is fennáll, akkor lépjen kapcsolatba a karbantartókkal.")

  (confirm-pending-changes #.(format nil "Az oldalon módosítások vannak amelyek elveszhetnek!~%Folytatja a műveletet?"))
  (unknown-server-error "Ismeretlen eredetű szerver hiba")
  (ucw.session-expired-approve-redirect-message "Letelt a biztonsági idő. Szeretné frissíteni az oldalt?")
  (network-error "Hálózati hiba történt, probálkozzon újra, esetleg kicsit később.")
  
  (progress.tooltip "Kattints az eltávolításhoz")
  (progress-label.default "Töltés...")
  (progress-label.closing-tab "Tab bezárása...")
  (progress-label.loading-tab "Tab letöltése...")
  (progress-label.loading-container-child "Oldal letöltése..."))
