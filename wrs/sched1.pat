SCHED1:
"/    redistribute job size bias    /
QSTAB[32.,,-PQ1
196.,,-PQ2
"/    set max quantum to 5 tics    /
QQSTBL+2/QQYTTY
QQYTTY
"/    disk IO at end of queue 2 with small quantum    /
QSSW/EQFIX
JFYS12,,-PQ2
QWSW/EQFIX
JFYS12,,-PQ2
"/    set TTY satisfied quantum to 15 tics    /
QQYTTY[15.
   