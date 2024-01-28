      ******************************************************************
      * Author: SILVA D. BELEN
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTNACIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT  NACIONALIDADES ASSIGN TO DISK "NACIONALIDAD.dat"
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WS-FS-NAC.


       DATA DIVISION.
       FILE SECTION.

       FD  NACIONALIDADES.
           01 REG-NACIO.
              05 COD-NACIO   PIC X(03).
              05 NOM-NACIO   PIC X(20).

      *****************************************************

       WORKING-STORAGE SECTION.

      ***VARIABLE DE CONTROL

       01  WS-FS-NAC    PIC X(02).
             88 WS-OK          VALUE '00'.
             88 WS-NO-OK       VALUE '10'.

       01  WS-FLAG-FIN   PIC X.
           88 WS-SI-PROCESO    VALUE 'T'.
           88 WS-FIN-PROCESO   VALUE 'F'.

      *****VARIABLES AUXILIARES ****

       01  WS-NACIO.
           05 PAISES  OCCURS 10 TIMES INDEXED BY WS-I.
               10 WS-NAC-COD    PIC X(03).
               10 WS-NAC-NOMBRE PIC X(20).

       01 WS-OPCION          PIC 9(02).



      ***********************************************

        LINKAGE SECTION.

       01  LK-COM-NACIONALIDAD.
           03  LK-NOM-NAC         PIC X(20).
           03  LK-OPCION-NAC      PIC 9.
           03  LK-NUM-NAC         PIC X(03).
           03  LK-RESULTADO-NAC   PIC 9(02).



       PROCEDURE DIVISION USING LK-COM-NACIONALIDAD.

      ***************************************
      *    CUERPOR PRINCIPAL DEL PROGRAMA   *
      ***************************************

       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESOS UNTIL WS-FIN-PROCESO

           PERFORM 9999-CIERRE.


            GOBACK.


      ***********************************************************
      *    CUERPO INICIO APERTURA ARCHIVOS E INICIO DE VARIBLES *
      ***********************************************************

       1000-INICIO.



           SET WS-SI-PROCESO TO TRUE

           OPEN INPUT NACIONALIDADES
           IF WS-FS-NAC NOT EQUAL '00'
             DISPLAY 'ERROR EN APERTURA DE ARCHIVO: ' WS-FS-NAC
             SET WS-FIN-PROCESO TO TRUE
           END-IF.


      **************************************
      *    CUERPO PRINCIPAL DE PROCESOS    *
      **************************************

       2000-PROCESOS.


         EVALUATE LK-OPCION-NAC

           WHEN 1

             MOVE 1 TO WS-I
             PERFORM 2200-CARGARTABLA
                    VARYING WS-I FROM 1 BY 1
                                  UNTIL WS-I > 5

           WHEN 2

            MOVE 1 TO WS-I
            PERFORM 2400-BUSCARDATO
            SET WS-FIN-PROCESO TO TRUE

           WHEN OTHER
             MOVE 30 TO LK-RESULTADO-NAC
             MOVE SPACES TO LK-COM-NACIONALIDAD
             SET WS-FIN-PROCESO TO TRUE
         END-EVALUATE.



      ********************************

       2200-CARGARTABLA.

         READ NACIONALIDADES

         IF WS-FS-NAC NOT EQUAL '00' AND
                   WS-FS-NAC NOT EQUAL '10'
          DISPLAY 'ERROR EN LECTURA DE ARCHIVO: ' WS-FS-NAC
          SET WS-FIN-PROCESO TO TRUE
         ELSE
             IF WS-FS-NAC NOT EQUAL '10'
               MOVE COD-NACIO   TO  WS-NAC-COD(WS-I)
               MOVE NOM-NACIO   TO  WS-NAC-NOMBRE(WS-I)
              ELSE
                  SET WS-FIN-PROCESO TO TRUE
             END-IF
         END-IF.

         IF WS-I = 4 THEN
            MOVE 40 TO LK-RESULTADO-NAC
            SET WS-FIN-PROCESO TO TRUE
         END-IF.

     **********************************

       2400-BUSCARDATO.

           MOVE 1 TO WS-I


           SEARCH PAISES
               AT END
               MOVE 20 TO LK-RESULTADO-NAC
               WHEN WS-NAC-COD (WS-I) = LK-NUM-NAC
               MOVE WS-NAC-NOMBRE(WS-I) TO LK-NOM-NAC
               MOVE 10 TO LK-RESULTADO-NAC

           END-SEARCH.



      **************************************
      *    CUERPO CIERRE ARCHIVO           *
      **************************************

       9999-CIERRE.

         CLOSE NACIONALIDADES.
         IF WS-FS-NAC NOT EQUAL '00'
             DISPLAY 'EROR EN CERRAR ARCHIVO: ' WS-FS-NAC
         END-IF.


       END PROGRAM RUTNACIO.
