       PROGRAM-ID. Project4 as "Project4".
       AUTHOR     PRIYANK PATEL & JASHANJOT PRUTHI & VIVEK PATEL & 
                    AMARJEET SINGH & SAHILVIR SINGH DHILLON.
       
      * Establishing the working environment for the program:            
                    
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
      * File control section for all files
       FILE-CONTROL.
         SELECT INVENT-FILE-IN
         ASSIGN TO "E:\level 3\Cobol\projects\project 4\INVENT.TXT"
         ORGANIZATION LINE SEQUENTIAL.
                   
         SELECT SUPPLIER-FILE-IN
         ASSIGN TO "E:\level 3\Cobol\projects\project 4\SUPPLIERS4.TXT"
         ORGANIZATION LINE SEQUENTIAL.
                   
         SELECT INVENT-FILE-OUT
         ASSIGN TO "E:\level 3\Cobol\projects\project 4\INVENT6.TXT"  
         ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS PART-NUMBER-OUT.
                  
         SELECT SUPPLIER-FILE-OUT
         ASSIGN TO "E:\level 3\Cobol\projects\project 4\SUPPLIERI.TXT" 
         ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS SUPPLIER-CODE-OUT.

      *Describing the file, record, and field structures to be used in
      *the program
       DATA DIVISION.
       
      * File division begins
       FILE SECTION.
       FD INVENT-FILE-IN.
       01 INVENTORY-RECORD-IN.
           05 PART-NUMBER-IN               PIC 9(5).
           05 PART-NAME-IN                 PIC X(20).
           05 QTY-ON-HAND-IN               PIC 9(3).
           05 UNIT-PRICE-IN                PIC 9(2)V99.
           05 SUPPLIER-COD-IN              PIC X(5).
           05 REORDER-POINT-IN             PIC 9(3).
           
       FD SUPPLIER-FILE-IN.
       01 SUPPLIER-RECORD-IN.
           05 SUPPLIER-CODE-IN             PIC X(5).
           05 SUPPLIER-NAME-IN             PIC X(15).
           
           
       FD INVENT-FILE-OUT.
       01 INVENTORY-REPORT-OUT.
           05 PART-NUMBER-OUT               PIC 9(5).
           05 PART-NAME-OUT                 PIC X(20).
           05 QTY-ON-HAND-OUT               PIC 9(3).
           05 UNIT-PRICE-OUT                PIC 9(2)V99.
           05 SUPPLIER-COD-OUT              PIC X(5).
           05 REORDER-POINT-OUT             PIC 9(3).
           
           
       FD SUPPLIER-FILE-OUT.
       01 SUPPLIER-OUT-REPORT-LINE.
           05 SUPPLIER-CODE-OUT             PIC X(5).
           05 SUPPLIER-NAME-OUT             PIC X(15).

      * Working storage section begins
       WORKING-STORAGE SECTION.
       01 INVENTORY-REPORT-OUT-WS.
           05 PART-NUMBER-OUT-WS            PIC 9(5).
           05 PART-NAME-OUT-WS              PIC X(20).
           05 QTY-ON-HAND-OUT-WS            PIC 9(3).
           05 UNIT-PRICE-OUT-WS             PIC 9(2)V99.
           05 SUPPLIER-COD-OUT-WS           PIC X(5).
           05 REORDER-POINT-OUT-WS          PIC 9(3).

       01 SUPPLIER-OUT-REPORT-LINE-WS.
           05 SUPPLIER-CODE-OUT-WS          PIC X(5).
           05 SUPPLIER-NAME-OUT-WS          PIC X(15).
               
       01 TEMP.
           05 INVENT-EOF-FLAG               PIC X(1).
           05 SUPPLIER-EOF-FLAG             PIC X(1).
       
      * Procedure division begins    
       PROCEDURE DIVISION.
       100-CREATE-INVENTORY-FILE.
           PERFORM  201-INIT-OPEN-INVENTORY-FILES.
           PERFORM  202-CONVERT-FILES-TO-INDEXED-BASED
           PERFORM  203-TERMINATE-INVENTORY-CONVERTING.
           STOP RUN.

       201-INIT-OPEN-INVENTORY-FILES.
           PERFORM  701-OPEN-FILES.
           PERFORM  702-READ-INVENT-DATA.
           PERFORM  703-READ-SUPPLIER-DATA.

       202-CONVERT-FILES-TO-INDEXED-BASED.    
           PERFORM  704-CONVERT-INVENTORY-FILE 
               UNTIL INVENT-EOF-FLAG ="Y".
           PERFORM  705-CONVERT-SUPPLIER-FILE 
               UNTIL SUPPLIER-EOF-FLAG ="Y".

       203-TERMINATE-INVENTORY-CONVERTING.                              
           PERFORM  706-CLOSE-OPENED-FILES.

       701-OPEN-FILES.
           OPEN INPUT INVENT-FILE-IN.
           OPEN INPUT SUPPLIER-FILE-IN.
           OPEN OUTPUT INVENT-FILE-OUT.
           OPEN OUTPUT SUPPLIER-FILE-OUT.

       702-READ-INVENT-DATA.
           READ INVENT-FILE-IN
               AT END 
               MOVE "Y" TO INVENT-EOF-FLAG.
        
       703-READ-SUPPLIER-DATA.
           READ SUPPLIER-FILE-IN
               AT END 
               MOVE "Y" TO SUPPLIER-EOF-FLAG.

       704-CONVERT-INVENTORY-FILE.
           PERFORM  801-MOVE-INVENTORY-FIELD.                   
           PERFORM  802-WRITE-INVENT-RECORD.
           PERFORM  702-READ-INVENT-DATA.
        
       705-CONVERT-SUPPLIER-FILE.
           PERFORM  803-MOVE-SUPPLIER-FIELDS
           PERFORM  804-WRITE-SUPPLIER-RECORDS.
           PERFORM  703-READ-SUPPLIER-DATA.
           
       801-MOVE-INVENTORY-FIELD.
           MOVE PART-NUMBER-IN TO PART-NUMBER-OUT-WS.                   
           MOVE PART-NAME-IN TO PART-NAME-OUT-WS.                       
           MOVE QTY-ON-HAND-IN TO QTY-ON-HAND-OUT-WS. 
           MOVE UNIT-PRICE-IN TO UNIT-PRICE-OUT-WS.
           MOVE SUPPLIER-COD-IN TO SUPPLIER-COD-OUT-WS.
           MOVE REORDER-POINT-IN TO REORDER-POINT-OUT-WS.
           
       802-WRITE-INVENT-RECORD.
           WRITE  INVENTORY-REPORT-OUT FROM INVENTORY-REPORT-OUT-WS 
               INVALID KEY DISPLAY "KEY INVALID"
           END-WRITE.

       803-MOVE-SUPPLIER-FIELDS.
           MOVE SUPPLIER-CODE-IN TO SUPPLIER-CODE-OUT-WS.               
           MOVE SUPPLIER-NAME-IN TO SUPPLIER-NAME-OUT-WS.               

       804-WRITE-SUPPLIER-RECORDS.
           WRITE SUPPLIER-OUT-REPORT-LINE 
               FROM SUPPLIER-OUT-REPORT-LINE-WS                         
           INVALID KEY DISPLAY "KEY INVALID"
           END-WRITE.
           
       706-CLOSE-OPENED-FILES.
           CLOSE INVENT-FILE-IN.
           CLOSE SUPPLIER-FILE-IN.
           CLOSE INVENT-FILE-OUT.
           CLOSE SUPPLIER-FILE-OUT.

      * Ending of program Project4
       END PROGRAM Project4.