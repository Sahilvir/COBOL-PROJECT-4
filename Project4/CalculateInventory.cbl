       PROGRAM ID. CalculateInventory as "CalculateInventory".
       AUTHOR.     PRIYANK PATEL & JASHANJOT PRUTHI & VIVEK PATEL & 
                   AMARJEET SINGH & SAHILVIR SINGH DHILLON.

      *Establishing the working environment for the program:
                   
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      
      * Linkage section
       LINKAGE SECTION.
       01 QTY-ON-HAND-IN       PIC 9(3).
       01 UNIT-PRICE-IN        PIC 9(2)V99.
       01 STOCK-VALUE-TEMP     PIC 9(6)V99.
       01 TOTAL-VALUE-TEMP     PIC 9(8)V99.

      * Procedure division begins
       PROCEDURE DIVISION USING QTY-ON-HAND-IN, UNIT-PRICE-IN, 
       STOCK-VALUE-TEMP, TOTAL-VALUE-TEMP.
       MULTIPLY QTY-ON-HAND-IN BY UNIT-PRICE-IN                     
             GIVING STOCK-VALUE-TEMP.
       ADD STOCK-VALUE-TEMP TO TOTAL-VALUE-TEMP.
           EXIT PROGRAM.

      * Ending of program CalculateInventory
       END PROGRAM CalculateInventory.