      ******************************************************************
      * Author: SILVA D. BELEN
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTMATER.

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

       FD  MATERIAS.
           01 REG-MATE.
              05 COD-MATE   PIC X(02).
              05 NOM-MATE   PIC X(25).

      *****************************************************

       WORKING-STORAGE SECTION.

      ***VARIABLE DE CONTROL

       01  WS-FS-MATE    PIC X(02).
             88 WS-OK          VALUE '00'.
             88 WS-NO-OK       VALUE '10'.

       01  WS-FLAG-FIN   PIC X.
           88 WS-SI-PROCESO    VALUE 'T'.
           88 WS-FIN-PROCESO   VALUE 'F'.

      *****VARIABLES AUXILIARES ****

       01  WS-MATE.
           05 MATERIA  OCCURS 10 TIMES INDEXED BY WS-I.
               10 WS-MAT-COD    PIC X(02).
               10 WS-MAT-NOMBRE PIC X(25).

       01 WS-OPCION          PIC 9(02).



      ***********************************************

        LINKAGE SECTION.

         01  LK-COM-MATERIA.
           03  LK-NOM-MATE        PIC X(25).
           03  LK-OPCION-MAT      PIC 9.
           03  LK-NUM-MATE        PIC X(02).
           03  LK-RESULTADO       PIC 9(02).



       PROCEDURE DIVISION USING LK-COM-MATERIA.

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

           OPEN INPUT MATERIAS
           IF WS-FS-MATE NOT EQUAL '00'
             DISPLAY 'ERROR EN APERTURA DE ARCHIVO: ' WS-FS-MATE
             SET WS-FIN-PROCESO TO TRUE
           END-IF.


      **************************************
      *    CUERPO PRINCIPAL DE PROCESOS    *
      **************************************

       2000-PROCESOS.

         EVALUATE LK-OPCION-MAT

           WHEN 1

             MOVE 1 TO WS-I
             PERFORM 2200-CARGARTABLA
                    VARYING WS-I FROM 1 BY 1
                                  UNTIL WS-I > 9

           WHEN 2
            MOVE 1 TO WS-I
            PERFORM 2200-CARGARTABLA
                    VARYING WS-I FROM 1 BY 1
                                  UNTIL WS-I > 9

            PERFORM 2400-BUSCARDATO
            SET WS-FIN-PROCESO TO TRUE

           WHEN OTHER
             MOVE 30 TO LK-RESULTADO
             MOVE SPACES TO LK-COM-MATERIA
             SET WS-FIN-PROCESO TO TRUE
         END-EVALUATE.



      ********************************

       2200-CARGARTABLA.


         READ MATERIAS
         IF WS-FS-MATE NOT EQUAL '00' AND
                   WS-FS-MATE NOT EQUAL '10'
          DISPLAY 'ERROR EN LECTURA DE ARCHIVO: ' WS-FS-MATE
          SET WS-FIN-PROCESO TO TRUE
         ELSE
             IF WS-FS-MATE NOT EQUAL '10'
               MOVE COD-MATE   TO  WS-MAT-COD(WS-I)
               MOVE NOM-MATE   TO  WS-MAT-NOMBRE(WS-I)
              ELSE
                  SET WS-FIN-PROCESO TO TRUE
             END-IF
         END-IF.

         IF WS-I = 8 THEN
            MOVE 40 TO LK-RESULTADO
            SET WS-FIN-PROCESO TO TRUE
         END-IF.

     **********************************

       2400-BUSCARDATO.

           MOVE 1 TO WS-I

           INITIALIZE LK-RESULTADO


           SEARCH MATERIA
               AT END
               MOVE 20 TO LK-RESULTADO
               WHEN WS-MAT-COD (WS-I) = LK-NUM-MATE
               MOVE 10 TO LK-RESULTADO
               MOVE WS-MAT-NOMBRE(WS-I) TO LK-NOM-MATE

           END-SEARCH.


      **************************************
      *    CUERPO CIERRE ARCHIVO           *
      **************************************

       9999-CIERRE.

         CLOSE MATERIAS.
         IF WS-FS-MATE NOT EQUAL '00'
             DISPLAY 'EROR EN CERRAR ARCHIVO: ' WS-FS-MATE
         END-IF.


       END PROGRAM RUTMATER.
