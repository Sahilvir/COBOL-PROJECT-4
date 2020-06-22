       PROGRAM-ID. InventoryReportProgram as "InventoryReportProgram".
       AUTHOR     PRIYANK PATEL & JASHANJOT PRUTHI & VIVEK PATEL &
                   AMARJEET SINGH & SAHILVIR SINGH DHILLON.
       
      *Establishing the working environment for the program:
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      
      *File control division
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
           ASSIGN TO "E:\level 3\Cobol\projects\project 4\INVENT6.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS PART-NUMBER-IN.
                   
           SELECT SUPPLIER-FILE-IN
           ASSIGN TO "E:\level 3\Cobol\projects\project 4\SUPPLIERI.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS SUPPLIER-CODE-IN.
                   
           SELECT INVENT-REPORT-OUT
           ASSIGN TO "E:\level 3\Cobol\projects\project 4\INVREPRT.TXT"
           ORGANIZATION LINE SEQUENTIAL.
                   
           SELECT REORDER-REPORT-OUT
           ASSIGN TO "E:\level 3\Cobol\projects\project 4\REORDER.TXT"
           ORGANIZATION LINE SEQUENTIAL.

      *Describing the file, record, and field structures to be used in 
      *the program
       DATA DIVISION.
       
      * InventRecord.txt file section
       FILE SECTION.
       FD INVENT-FILE-IN.
       COPY "E:\level 3\Cobol\projects\project 4\InventRecord.TXT".
           
      * File division for supplier
       FD SUPPLIER-FILE-IN.
       01 SUPPLIER-RECORD-IN.
           05 SUPPLIER-CODE-IN             PIC X(5).
           05 SUPPLIER-NAME-IN             PIC X(15).
           
      * File division for invent report    
       FD INVENT-REPORT-OUT.
       01 INVENTORY-REPORT-OUT             PIC X(56).
           
      * File division for reorder report    
       FD REORDER-REPORT-OUT.
       01 REORDER-OUT-REPORT-LINE          PIC X(75).

      * Working storage section for file variables
       WORKING-STORAGE SECTION.
       01  INVENT-DETAIL-LINE-WS.
           05  INVENTORY-PART-NUMBER       PIC 99,999.
           05  FILLER                      PIC X(6) VALUE SPACES.
           05  INVENTORY-PART-NAME         PIC X(20).
           05  FILLER                      PIC X(5) VALUE SPACES.
           05  INVENTORY-QUANTITY          PIC 9(3).
           05  FILLER                      PIC X(5) VALUE SPACES.
           05  STOCK-VALUE                 PIC $$$$,$$9.99.

       01  REPORT-TITLE.
           05  FILLER              PIC X(16)  VALUE "INVENTORY REPORT".
           05  FILLER              PIC X(4)   VALUE SPACES.
           05  WEEK-DAY            PIC X(9).
           05  FILLER              PIC X(3)   VALUE SPACES.
           05  YEAR-IN             PIC 9(4).
           05  FILLER              PIC X(1)   VALUE "/".
           05  MONTH-IN            PIC 9(2).
           05  FILLER              PIC X(1)   VALUE "/".
           05  DATE-IN             PIC 9(2).
           
       01 WEEK-DAY-NAMES.
           05  FILLER                  PIC X(9) VALUE "MONDAY".
           05  FILLER                  PIC X(9) VALUE "TUESDAY".
           05  FILLER                  PIC X(9) VALUE "WEDNESDAY".
           05  FILLER                  PIC X(9) VALUE "THURSDAY".
           05  FILLER                  PIC X(9) VALUE "FRIDAY".
           05  FILLER                  PIC X(9) VALUE "SATURDAY".
           05  FILLER                  PIC X(9) VALUE "SUNDAY".
       
       01 WEEK-DAY-TABLE REDEFINES WEEK-DAY-NAMES.
           05 NAME-OF-WEEKDAY PIC X(9) OCCURS 7 TIMES.
           
       01 WEEK-DAY-NUMBER              PIC 9(1).
       
       01 INV-COLUMN-HDR.
           05  FILLER                  PIC X(10) VALUE "PARTNUMBER".
           05  FILLER                  PIC X(2)  VALUE  SPACES.
           05  FILLER                  PIC X(8)  VALUE "PARTNAME".
           05  FILLER                  PIC X(14) VALUE SPACES.
           05  FILLER                  PIC X(8)  VALUE "QUANTITY".
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE "STOCKVALUE".
           05  FILLER                  PIC X(1)  VALUE SPACES.
      
       01 AUDIT-TRAIL.
           05  FILLER                  PIC X(11) VALUE "TOTAL VALUE".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  TOTAL-VALUE             PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(4) VALUE "READ".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  READ-COUNTER            PIC Z,ZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(7) VALUE "WRITTEN".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WRITTEN-COUNTER         PIC Z,ZZ9.

       01 VARIABLES.
           05  READ-COUNTER-TEMP       PIC 9(4) VALUE 0.
           05  WRITTEN-COUNTER-TEMP    PIC 9(4) VALUE 0.
           05  STOCK-VALUE-TEMP        PIC 9(6)V99.
           05  TOTAL-VALUE-TEMP        PIC 9(8)V99.
           
       01 REORDER-OUT-REPORT-LINE-WS .
           05 REORDER-PART-NUMBER-OUT      PIC 9(5).
           05  FILLER                      PIC X(7)  VALUE SPACES.
           05 REORDER-PART-NAME-OUT        PIC X(20).
           05  FILLER                      PIC X(5)  VALUE SPACES.
           05 REORDER-QTY-ON-HAND-OUT      PIC 9(3).
           05  FILLER                      PIC X(10) VALUE SPACES.
           05 REORDER-POINT-OUT            PIC 9(3).
           05  FILLER                      PIC X(7) VALUE SPACES.
           05 REORDER-SUPPLIER-NAME-OUT    PIC X(15).
       
       01 REORDER-COLUMN-HDR.
           05  FILLER                  PIC X(10) VALUE "PARTNUMBER".
           05  FILLER                  PIC X(2)  VALUE  SPACES.
           05  FILLER                  PIC X(8)  VALUE "PARTNAME".
           05  FILLER                  PIC X(14) VALUE SPACES.
           05  FILLER                  PIC X(8)  VALUE "QUANTITY".
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  FILLER                  PIC X(13) VALUE "REORDER POINT".
           05  FILLER                  PIC X(2)  VALUE SPACES. 
           05  FILLER                  PIC X(12) VALUE "SUPPLIERNAME".
           05  FILLER                  PIC X(3)  VALUE SPACES.

       01  FLAGS-AND-COUNTERS.
           05  EOF-FLAG                PIC X(1) VALUE "N".
           05  SEARCH-SUB              PIC 9(2).
           05  FOUND-FLAG              PIC X(3) VALUE "NO".
           
      * Procedure division begins
       PROCEDURE DIVISION.
       100-CREATE-INVENTORY-FILE.
           PERFORM  201-INIT-OPEN-INVENTORY-FILES.
           PERFORM  202-READ-WRITE-INVENTORY-RECORD
                    UNTIL EOF-FLAG = "Y".
           PERFORM  203-TERMINATE-INVENTORY-READ-WRITE.
           STOP RUN.

       201-INIT-OPEN-INVENTORY-FILES.
           PERFORM  701-OPEN-FILES.
           PERFORM  702-PRINT-INVENTORY-TITLE.
           PERFORM  703-PRINT-INVENTORY-HEADER.                         
           PERFORM  704-READ-INVENT-DATA.
           PERFORM  705-PRINT-REORDER-HEADER.

       202-READ-WRITE-INVENTORY-RECORD.
           PERFORM  706-PROCESS-INVENT-RECORDS.                         
           PERFORM  707-WRITE-INVENT-RECORD.
           PERFORM  708-PROCESS-REORDER-RECORDS.                        
           PERFORM  704-READ-INVENT-DATA.

       203-TERMINATE-INVENTORY-READ-WRITE.
           PERFORM  709-PRINT-INVENTORY-TRAIL.
           PERFORM  710-CLOSE-OPENED-FILES.

       701-OPEN-FILES.
           OPEN INPUT INVENT-FILE-IN.
           OPEN INPUT SUPPLIER-FILE-IN.
           OPEN OUTPUT INVENT-REPORT-OUT.
           OPEN OUTPUT REORDER-REPORT-OUT.
                   
       702-PRINT-INVENTORY-TITLE.
           ACCEPT WEEK-DAY-NUMBER FROM DAY-OF-WEEK.
           MOVE NAME-OF-WEEKDAY(WEEK-DAY-NUMBER) TO WEEK-DAY. 
           MOVE FUNCTION CURRENT-DATE(1:4) TO YEAR-IN .
           MOVE FUNCTION CURRENT-DATE(5:2) TO MONTH-IN.
           MOVE FUNCTION CURRENT-DATE(7:2) TO DATE-IN.
           WRITE INVENTORY-REPORT-OUT FROM REPORT-TITLE                 
           BEFORE ADVANCING 2 LINES.
        
       703-PRINT-INVENTORY-HEADER.
           WRITE INVENTORY-REPORT-OUT FROM INV-COLUMN-HDR           
           BEFORE ADVANCING 2 LINE.
           
       704-READ-INVENT-DATA.
           READ INVENT-FILE-IN
               AT END 
               MOVE "Y" TO EOF-FLAG
                   NOT AT END 
                   ADD 1 TO READ-COUNTER-TEMP.

       705-PRINT-REORDER-HEADER.
           WRITE REORDER-OUT-REPORT-LINE FROM REORDER-COLUMN-HDR
           BEFORE ADVANCING 2 LINES.

       706-PROCESS-INVENT-RECORDS.
           PERFORM 801-CALCULATE-INVENT-VALUE.
           PERFORM 802-MOVE-INVENTORY-FIELD.
           
       707-WRITE-INVENT-RECORD.
       IF FUNCTION MOD(READ-COUNTER-TEMP 10) = 1 AND
           READ-COUNTER-TEMP NOT LESS THAN 10 THEN
           WRITE INVENTORY-REPORT-OUT FROM INV-COLUMN-HDR           
           AFTER ADVANCING PAGE
           WRITE  INVENTORY-REPORT-OUT FROM INVENT-DETAIL-LINE-WS
           AFTER ADVANCING 2 LINES
           ADD  1 TO WRITTEN-COUNTER-TEMP
       ELSE
           WRITE  INVENTORY-REPORT-OUT FROM INVENT-DETAIL-LINE-WS
           ADD  1 TO WRITTEN-COUNTER-TEMP
       END-IF.
       
       708-PROCESS-REORDER-RECORDS.
           IF QTY-ON-HAND-IN < REORDER-POINT-IN
               MOVE "NO" TO FOUND-FLAG
               PERFORM 803-MOVE-REORDER-FIELDS
               PERFORM 804-SEARCH-SUPPLIER-NAME
               PERFORM 805-WRITE-REORDER-RECORDS.                       
           
       709-PRINT-INVENTORY-TRAIL.
           MOVE READ-COUNTER-TEMP TO READ-COUNTER.
           MOVE WRITTEN-COUNTER-TEMP TO WRITTEN-COUNTER.
           WRITE INVENTORY-REPORT-OUT FROM AUDIT-TRAIL                  
           AFTER ADVANCING 1 LINE.
           
       710-CLOSE-OPENED-FILES.
           CLOSE INVENT-FILE-IN.
           CLOSE SUPPLIER-FILE-IN.
           CLOSE INVENT-REPORT-OUT.
           CLOSE REORDER-REPORT-OUT.
           
       801-CALCULATE-INVENT-VALUE.
           CALL "C:\Users\priya\workspace\Project4\CalculateInventory" 
           USING
             QTY-ON-HAND-IN, UNIT-PRICE-IN, STOCK-VALUE-TEMP, 
               TOTAL-VALUE-TEMP.
           
       802-MOVE-INVENTORY-FIELD.
           MOVE PART-NUMBER-IN TO INVENTORY-PART-NUMBER.                
           MOVE PART-NAME-IN TO INVENTORY-PART-NAME.                    
           MOVE QTY-ON-HAND-IN TO INVENTORY-QUANTITY. 
           MOVE STOCK-VALUE-TEMP TO STOCK-VALUE.
           MOVE TOTAL-VALUE-TEMP TO TOTAL-VALUE.
           
       803-MOVE-REORDER-FIELDS.
           MOVE PART-NUMBER-IN TO REORDER-PART-NUMBER-OUT.              
           MOVE PART-NAME-IN TO REORDER-PART-NAME-OUT.                  
           MOVE QTY-ON-HAND-IN TO REORDER-QTY-ON-HAND-OUT.
           MOVE REORDER-POINT-IN TO REORDER-POINT-OUT.
           
       804-SEARCH-SUPPLIER-NAME.
           MOVE SUPPLIER-COD-IN TO SUPPLIER-CODE-IN.
           READ SUPPLIER-FILE-IN
           INVALID KEY DISPLAY "KEY INVALID"
           END-READ.
           MOVE SUPPLIER-NAME-IN TO REORDER-SUPPLIER-NAME-OUT.
           
       805-WRITE-REORDER-RECORDS.
           WRITE REORDER-OUT-REPORT-LINE 
               FROM REORDER-OUT-REPORT-LINE-WS.

      * Ending of Program InventoryReportProgram
       END PROGRAM InventoryReportProgram.