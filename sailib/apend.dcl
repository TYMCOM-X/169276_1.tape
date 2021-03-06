forward external procedure !WtEnd( integer chan );
COMMENT sets the text file on chan to the end-of-file (which it does 
	by reading the last block and writing it back out).  Chan 
	should have both read and write buffers, and should be both
	LOOKUPed and ENTERed to make this meaningful.  In some special
	cases this may actually do a "USETI -1".
;
forward external integer procedure Append( integer chan; string file;
					reference integer Flag );
COMMENT	The file specified is associated with CHAN in single-user 
	update mode (creating the file if necessary).  Then the last
	block of the file is read and re-written (so concatenating
	works properly).  Be sure the channel was OPENed with both
	read and write buffers in either mode 0 or 1.  Flag is just
	like the ENTER/LOOKUP flag, 0 on success, otherwise -1,,code.
	A non-negative return value means this call was successful
		(1 means a new file was created).
	A negative return value means that an error was encountered.
	  this is normally due to an ENTER, and the flag variable is
	  set as in LOOKUP or ENTER.  In these cases, a close is done
	  on the channel before returning.
;


    