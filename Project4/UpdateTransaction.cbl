       PROGRAM-ID. UpdateTransaction as "Program2".
       AUTHOR     PRIYANK PATEL & JASHANJOT PRUTHI & VIVEK PATEL &
                   AMARJEET SINGH & SAHILVIR SINGH DHILLON.
                   
      * Environment division for file Invent6.txt
       ENVIRONMENT DIVISION.
       SELECT INVENT-FILE-OUT
         ASSIGN TO "E:\level 3\Cobol\projects\project 4\INVENT6.TXT"    
         ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS PART-NUMBER-OUT.
       
      *Describing the file, record, and field structures to be used in
      *the program
       DATA DIVISION.
       FILE SECTION.
       
      * File division
       FD INVENT-FILE-OUT.
       01 INVENTORY-REPORT-OUT.
           05 PART-NUMBER-OUT               PIC 9(5).
           05 PART-NAME-OUT                 PIC X(20).
           05 QTY-ON-HAND-OUT               PIC 9(3).
           05 UNIT-PRICE-OUT                PIC 9(2)V99.
           05 SUPPLIER-COD-OUT              PIC X(5).
           05 REORDER-POINT-OUT             PIC 9(3).

      * Working storage section begins
       WORKING-STORAGE SECTION.
       01 TEMP.
           05 DECREASE-WS                   PIC 9(3).
           05 INCREASE-WS                   PIC 9(3).
           
       01 CONDITIONS.
         05 TRANSACTION-TYPE PIC X(1).
           88 SELLING VALUE "S".
           88 BUYING VALUE "B".
      
      * Screen Section begins    
       SCREEN SECTION.
       01 SELLING-INVENT-SCRN.
         05 VALUE "ENTER PART-NUMBER->" LINE 5 COL 10.
         05 PART-NUMBER TO PART-NUMBER-OUT LINE 5 COL 30.
         05 VALUE "Quantity of Stock Sold->" LINE 6 COL 10.
         05 DECREASE-IN TO DECREASE-WS LINE 6 COL 34. 
         
       01 BUYING-INVENT-SCRN.
         05 VALUE "ENTER PART-NUMBER->" LINE 5 COL 10.
         05 PART-NUMBER TO PART-NUMBER-OUT LINE 5 COL 30.
         05 VALUE "Quantity of Stock Bought->" LINE 6 COL 10.
         05 INCREASE-IN TO INCREASE-WS LINE 6 COL 34. 
       
      * Procedure division begins  
       PROCEDURE DIVISION.
       100-UPDATE-INVENTORY-FILE.
           PERFORM  201-INIT-OPEN-INVENTORY-FILES.
           PERFORM  202-PROMPT-ONLINE-TRANSACTION.
           PERFORM  203-UPDATE-ONLINE-TRANSACTION.
           PERFORM  204-TERMINATE-INVENTORY-CONVERTING.
           STOP RUN.


       201-INIT-OPEN-INVENTORY-FILES.
           OPEN I-O INVENT-FILE-OUT.

       202-PROMPT-ONLINE-TRANSACTION.
           DISPLAY 
           "TYPE 'S' IF SELLING INVENTORY,TYPE 'B' IF BUYING INVENTORY"
           ACCEPT TRANSACTION-TYPE.
           IF SELLING DISPLAY SELLING-INVENT-SCRN
                       ACCEPT SELLING-INVENT-SCRN.
           IF BUYING DISPLAY BUYING-INVENT-SCRN
                       ACCEPT BUYING-INVENT-SCRN.
       
       203-UPDATE-ONLINE-TRANSACTION.
           READ INVENT-FILE-OUT
           INVALID KEY DISPLAY "INVALID PART-NUMBER"
           END-READ.
           
           IF SELLING COMPUTE QTY-ON-HAND-OUT = QTY-ON-HAND-OUT -      
           DECREASE-WS.
           IF BUYING COMPUTE QTY-ON-HAND-OUT = QTY-ON-HAND-OUT +        
           INCREASE-WS.
       
           REWRITE INVENTORY-REPORT-OUT
           INVALID KEY DISPLAY "INVALID KEY"
           END-REWRITE.

       204-TERMINATE-INVENTORY-CONVERTING.                              
           CLOSE INVENT-FILE-OUT.

      * Ending of Program UpdateTransaction
       END PROGRAM UpdateTransaction.