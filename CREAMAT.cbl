      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAMAT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT MATERIAS      ASSIGN TO DISK "MATERIAS.dat"
                                      ORGANIZATION IS LINE SEQUENTIAL
                                      FILE STATUS IS WS-FS-MATE.

       DATA DIVISION.
       FILE SECTION.
       FD MATERIAS.
           01 REG-MATE.
              05 MAT-NRO-MATERIA   PIC X(02).
              05 MAT-DESCRIPCION   PIC X(25).

       WORKING-STORAGE SECTION.


       01  WS-FS-MATE      PIC X(02).
             88 WS-OK            VALUE '00'.
             88 WS-NO-OK         VALUE '10'.

       01  WS-FLAG-FIN      PIC X.
             88 WS-SI-PROCESO     VALUE 'T'.
             88 WS-FIN-PROCESO    VALUE 'F'.



       01  WS-CODIGO        PIC X(02).
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

           OPEN OUTPUT MATERIAS.
           IF WS-FS-MATE IS NOT EQUAL '00'
               DISPLAY 'ERROR EN APERTURA DE ARCHIVO: ' WS-FS-MATE
               SET WS-FIN-PROCESO TO TRUE
           END-IF.

           INITIALIZE REG-MATE
                      WS-I
                      WS-NOMBRE
                      WS-CODIGO

           MOVE 1 TO WS-I.

      *************************************
       2000-PROCESOS.



           DISPLAY 'INGRESE CODIGO (XX) DE MATERIA: '
           ACCEPT WS-CODIGO
           DISPLAY 'INGRESE NOMBRE DE LA MATERIA: '
           ACCEPT WS-NOMBRE

           PERFORM 2400-GRAGAR

           IF WS-I = 9
               SET WS-FIN-PROCESO TO TRUE
               DISPLAY 'FIN DEL ARCHIVO.'
           END-IF.



      *************************************

       2400-GRAGAR.

           MOVE WS-CODIGO TO MAT-NRO-MATERIA
           MOVE WS-NOMBRE TO MAT-DESCRIPCION

           WRITE REG-MATE
           IF WS-FS-MATE IS NOT EQUAL '00'
               DISPLAY 'ERROR EN GRABAR REGISTRO: ' WS-FS-MATE
               SET WS-FIN-PROCESO TO TRUE
           ELSE
               ADD 1 TO WS-I
           END-IF.


      *************************************

      *************************************
       9999-CIERRE.

           CLOSE MATERIAS
           IF WS-FS-MATE NOT EQUAL '00'
               DISPLAY 'EROR EN CERRAR ARCHIVO: ' WS-FS-MATE
           END-IF.



       END PROGRAM CREAMAT.
