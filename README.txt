Membri del gruppo:
Grassi Marco 829664
Frigati Francesco 829817

Questo README si riferisce al programma "lmc.pl", una implementazione
in Prolog di un Little Man Computer.
Questa simulazione avviene in due fasi:

1) Un file di testo scritto in un linguaggio assembly specifico
viene tradotto in machine code eseguibile dal simulatore lmc.
Questo parsing/traduzione viene eseguito tramite un automa
la cui struttura è definita nel file "parsing.pl".
Un' immagine di questo automa è presente in questa directory.


2) Se la traduzione ha successo, cosa che avviene solo con un file
assembly sintatticamente valido, questa viene usata come memoria del simulatore.
Ogni cella di memoria identificata dal program counter del lmc viene
decodificata nell' istruzione corretta e poi eseguita.
Se il file assembly è logicamente corretto (a seconda del programma che si vuole implementare)
il risultato dell'esecuzione sarà l'output generato secondo questo.
