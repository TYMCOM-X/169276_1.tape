0010?	
  0020?	external record!class map1 
0030?		( integer array a; integer ui; record!pointer (any!class) up );
   0040?	
  0050?	external record!class map2 
0060?		( integer array a; integer ui; record!pointer (any!class) up );
   0070?	
  0080?	external record!pointer (map1) procedure map1cr
0090?		( integer lower!bound, upper!bound, arr!offset(0) );
    0100?	
  0110?	external record!pointer (map2) procedure map2cr
0120?		( integer low!bd1, upp!bd1, low!bd2, upp!bd2, arr!offset(0) );
    0130?	
  0140?	external integer procedure getmap
    0150?		( record!pointer (map1,map2) holder;
                              0160?		  integer option, channel, pageinfile );
 0170?	
  0180?	external integer procedure allow( integer low, high );
   0190?	
  0200?	external integer procedure opfile( string FullName, Options;
  0210?						integer channel );
    