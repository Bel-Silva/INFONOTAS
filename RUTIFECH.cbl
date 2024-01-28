      ******************************************************************
      * Author: SILVA, D. BELEN
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTIFECH.
       DATA DIVISION.

       WORKING-STORAGE SECTION.


       01  WS-CONTROL     PIC 9(02).

       LINKAGE SECTION.

       01  LK-AREA.

           05 LK-ENTRADA.
              10 LK-MES   PIC 9(02).
              10 LK-LIT   PIC X VALUE '/'.
              10 LK-ANIO  PIC 9(04).
           05 LK-SALIDA.
              10 FORMATO-1     PIC X(07).
              10 LK-RETORNO    PIC X(20).
              10 LK-CONTROL    PIC 9(02).

      **********************************************************

       PROCEDURE DIVISION USING LK-AREA.

      ****************************************
      ***         CUERPO DEL PROGRAMA      ***
      ****************************************

       MAIN-PROCEDURE.

           PERFORM 1000-I-PROCESO.

           PERFORM 9999-I-FINAL

           GOBACK.

      **********************
      ** PROCEDIMIENTOS   **
      **********************

       1000-I-PROCESO.



           INITIALIZE WS-CONTROL

           PERFORM  2200-I-VALIDARFECHA.

           IF WS-CONTROL= 10 THEN
              PERFORM  2300-I-FORMATEARFECHA

           END-IF.

     ********************************

       2200-I-VALIDARFECHA.


           IF LK-ANIO >= 2000 AND LK-ANIO <= 2024
              IF LK-MES >= 1 AND LK-MES <= 12
                   MOVE 10 TO WS-CONTROL
              ELSE
                  MOVE 20 TO WS-CONTROL

           ELSE
              MOVE 30 TO WS-CONTROL
           END-IF.


    *****************************************

       2300-I-FORMATEARFECHA.


           MOVE LK-MES  TO FORMATO-1(1:2)
           MOVE LK-LIT  TO FORMATO-1(3:1)
           MOVE LK-ANIO TO FORMATO-1(4:4).

    *****************************************


        9999-I-FINAL.

           MOVE WS-CONTROL TO LK-CONTROL
           INITIALIZE LK-RETORNO

           EVALUATE LK-CONTROL
             WHEN 10
               MOVE 'FECHA VALIDA.' TO LK-RETORNO
             WHEN 20
               MOVE 'MES INVALIDO.' TO LK-RETORNO
             WHEN 30
               MOVE 'ANIO INVALIDO.' TO LK-RETORNO
             WHEN OTHER
               MOVE 'ERROR FECHA.'  TO LK-RETORNO

           END-EVALUATE.



        END PROGRAM RUTIFECH.
