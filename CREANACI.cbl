      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREANACI.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT NACIONALIDADES      ASSIGN TO DISK "NACIO.dat"
                                      ORGANIZATION IS LINE SEQUENTIAL
                                      FILE STATUS IS WS-FS-NACIO.

       DATA DIVISION.
       FILE SECTION.
       FD NACIONALIDADES.
           01 REG-NACIO.
              05 COD-NACIO   PIC X(03).
              05 NOM-NACIO   PIC X(25).

       WORKING-STORAGE SECTION.


       01  WS-FS-NACIO      PIC X(02).
             88 WS-OK            VALUE '00'.
             88 WS-NO-OK         VALUE '10'.

       01  WS-FLAG-FIN      PIC X.
             88 WS-SI-PROCESO     VALUE 'T'.
             88 WS-FIN-PROCESO    VALUE 'F'.

       01  WS-COD-VALIDO    PIC 99.
       01  WS-NOM-VALIDO    PIC 99.



       01  WS-CODIGO        PIC X(03).
       01  WS-NOMBRE        PIC X(25).
       01  WS-I             PIC 9.


       PROCEDURE DIVISION.

      ****************************************************
      ****************************************************
       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESOS UNTIL WS-FIN-PROCESO

           PERFORM 9999-CIERRE.

       MAIN-PROGRAM-FINAL.
           EXIT.
               GOBACK.


      ***********************************************************
      *    CUERPO INICIO APERTURA ARCHIVOS E INICIO DE VARIBLES *
      ***********************************************************

       1000-INICIO.

           SET WS-SI-PROCESO TO TRUE

           OPEN OUTPUT NACIONALIDADES.
           IF WS-FS-NACIO IS NOT EQUAL '00'
               DISPLAY 'ERROR EN APERTURA DE ARCHIVO: ' WS-FS-NACIO
               SET WS-FIN-PROCESO TO TRUE
           END-IF.

           INITIALIZE REG-NACIO
                      WS-I
                      WS-NOMBRE
                      WS-CODIGO

           MOVE 1 TO WS-I
           MOVE 99 TO WS-COD-VALIDO
           MOVE 99 TO WS-NOM-VALIDO.

      *************************************
       2000-PROCESOS.



           DISPLAY 'INGRESE CODIGO (XXX) DEL PAIS: '
           ACCEPT WS-CODIGO

           IF LENGTH OF WS-CODIGO NOT EQUAL 3 AND
                    NOT WS-CODIGO NUMERIC
            DISPLAY "Ingrese codigo de tres digitos. Intente nuevamente"
           ELSE
               MOVE 00 TO WS-COD-VALIDO
           END-IF

           EVALUATE WS-COD-VALIDO

           WHEN 00
               PERFORM 2200-NOMBRE-PAIS UNTIL WS-FIN-PROCESO

           WHEN 99
              DISPLAY 'INGRESE CODIG NUEVAMENTE'

           WHEN OTHER
             DISPLAY 'ERROR NO CONTEMPLADO.'

           END-EVALUATE.



      *************************************

       2200-NOMBRE-PAIS.

           DISPLAY 'INGRESE NOMBRE DEL PAIS: '
           ACCEPT WS-NOMBRE


           PERFORM 2400-GRAGAR

           IF WS-I = 5
               SET WS-FIN-PROCESO TO TRUE
               DISPLAY 'FIN DEL ARCHIVO.'
           END-IF.

      ******************************************

       2400-GRAGAR.

           MOVE WS-CODIGO TO COD-NACIO
           MOVE WS-NOMBRE TO NOM-NACIO

           WRITE REG-NACIO
           IF WS-FS-NACIO IS NOT EQUAL '00'
               DISPLAY 'ERROR EN GRABAR REGISTRO: ' WS-FS-NACIO
               SET WS-FIN-PROCESO TO TRUE
           ELSE
               ADD 1 TO WS-I
           END-IF.


      *************************************

      *************************************
       9999-CIERRE.

           CLOSE NACIONALIDADES
           IF WS-FS-NACIO NOT EQUAL '00'
               DISPLAY 'EROR EN CERRAR ARCHIVO: ' WS-FS-NACIO
           END-IF.



       END PROGRAM CREANACI.
