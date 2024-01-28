      ******************************************************************
      * Author: SILVA, D. BELEN
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMINFOR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOTAS ASSIGN TO DISK "NOTAS.dat"
      *PARA CONECTAR CON JCL SELECT NOTAS ASSING TO DDNOTA
                              ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FS-NOTA.


           SELECT ALUMNOS ASSIGN TO DISK "ALUMNOS.dat"
      *PARA CONECTAR CON JCL SELECT ALUMNOS ASSING TO DDALUM
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS WS-FS-ALU.





       DATA DIVISION.
       FILE SECTION.
       FD NOTAS.
         01 REG-NOTAS.
             03 NOT-NRO-ALU    PIC 9(04).
             03 NOT-NRO-MAT    PIC 99.
             03 NOT-ANIO       PIC 9(04).
             03 NOT-MES        PIC 99.
             03 NOT-NOTA       PIC 99.

       FD ALUMNOS.
         01 REG-ALUMNO.
             03 ALU-NRO-ALU    PIC 9(04).
             03 ALU-NOMBRE     PIC X(23).
             03 ALU-NRO-PAIS   PIC 9(03).

    *********************************
       WORKING-STORAGE SECTION.

    ******VARIABLES DE CONTROL*******

         01 WS-FS-NOTA    PIC X(02).

         01 WS-FS-ALU    PIC X(02).


         01  WS-FLAG-FIN    PIC X.
           88 WS-SI-PROCESO      VALUE 'T'.
           88 WS-FIN-PROCESO     VALUE 'F'.

         01 WS-BUSCA-ALU  PIC X.
           88 WS-NO-ENCONTRADO   VALUE 'T'.
           88 WS-ENCONTRADO      VALUE 'F'.

         01 WS-BUSCA-ANIO  PIC X.
           88 WS-SI-ANIO         VALUE 'T'.
           88 WS-NO-ANIO         VALUE 'F'.

    *********COMUNICACION ******************

         01 WS-RUTIF  PIC X(08)  VALUE 'RUTIFECH'.

         01 WS-RUTIM  PIC X(08)  VALUE 'RUTMATER'.

         01 WS-RUTIN  PIC X(08)  VALUE 'RUTNACIO'.

         01  LK-COM-MATERIA.
           03  LK-NOM-MATE        PIC X(25).
           03  LK-OPCION-MAT      PIC 9.
           03  LK-NUM-MATE        PIC X(02).
           03  LK-RESULTADO-MAT   PIC 9(02).

         01  LK-COM-NACIONALIDAD.
           03  LK-NOM-NAC         PIC X(20).
           03  LK-OPCION-NAC      PIC 9.
           03  LK-NUM-NAC         PIC X(03).
           03  LK-RESULTADO-NAC   PIC 9(02).

         01  LK-AREA.

           05 LK-ENTRADA.
              10 LK-MES   PIC 9(02).
              10 LK-LIT   PIC X   VALUE '/'.
              10 LK-ANIO  PIC 9(04).
           05 LK-SALIDA.
              10 FORMATO-1     PIC X(07).
              10 LK-RETORNO    PIC X(20).
              10 LK-CONTROL    PIC 9(02).

     ********VARIABLES AUXILIARES **********

         01 WS-FECHA     PIC 9(06).

         01 WS-SUM-NOTA  PIC 9(04).
         01 WS-CANT      PIC 9(03).
         01 WS-PROM      PIC 9(03).
         01 WP-PROM      PIC Z(03).
         01 WS-CONT-ALU  PIC 99.

         01 WS-ALU-ANT   PIC 9(04).
         01 WS-MAT-ANT   PIC 99.


         01 LIT-FECHA    PIC X(20) VALUE '        DESDE: '.
         01 LIT-SPACES   PIC X(75) VALUE ALL '-'.
         01 LIT-SPACES2  PIC X(75) VALUE ALL SPACES.



     ********VARIABLES DE IMPRESION *************


       PROCEDURE DIVISION.
      ***************************************
      *    CUERPOR PRINCIPAL DEL PROGRAMA   *
      ***************************************


       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESOS UNTIL WS-FIN-PROCESO

           PERFORM 9999-CIERRE.



            STOP RUN.


      ***********************************************
      *    APERTURA DE ARCHIVOS- INICIO DE VARIBLES *
      ***********************************************

       1000-INICIO.

           SET WS-SI-PROCESO TO TRUE

           MOVE 0  TO  WS-CANT
           MOVE 0  TO  WS-SUM-NOTA
           MOVE 0  TO  WS-PROM
           MOVE 99 TO  LK-CONTROL
           MOVE 0  TO  WS-CONT-ALU



           OPEN INPUT NOTAS
           IF WS-FS-NOTA NOT EQUAL '00'
             DISPLAY 'ERROR EN APERTURA DE ARCHIVO: ' WS-FS-NOTA
             SET WS-FIN-PROCESO TO TRUE
           END-IF


           OPEN INPUT ALUMNOS
           IF WS-FS-ALU NOT EQUAL '00'
             DISPLAY 'ERROR EN APERTURA DE ARCHIVO: ' WS-FS-ALU
             SET WS-FIN-PROCESO TO TRUE
           END-IF

      ***  CARGAR TABLAS ***

           MOVE  1 TO LK-OPCION-MAT
           CALL WS-RUTIM USING LK-COM-MATERIA

           MOVE 1 TO LK-OPCION-NAC
           CALL WS-RUTIN USING LK-COM-NACIONALIDAD

      ******CARGO Y VALIDO FECHA

           PERFORM 1010-FECHA UNTIL LK-CONTROL =10

           DISPLAY '----------------------------'
           '------------------DESDE: ' FORMATO-1

           DISPLAY LIT-SPACES


      *****PRIMER LECTURA DE REGISTRO

           PERFORM 1020-PRIMERA-LECTURA.

     ******************************
     *******INGRESO DE FECHA ******

       1010-FECHA.

           DISPLAY 'INGRESE FECHA: (MMAAAA)'
           ACCEPT WS-FECHA

           MOVE WS-FECHA(1:2) TO LK-MES

           MOVE WS-FECHA(3:4) TO LK-ANIO


           CALL WS-RUTIF USING LK-AREA.




      *****PRIMERA LECTURA *****

       1020-PRIMERA-LECTURA.


           READ NOTAS

           EVALUATE WS-FS-NOTA
           WHEN  '00'
               MOVE NOT-NRO-ALU TO WS-ALU-ANT
               MOVE NOT-NRO-MAT TO WS-MAT-ANT

           WHEN '10'
               DISPLAY '*ARCHIVO NOTAS VACIO. ' WS-FS-NOTA
               SET WS-FIN-PROCESO TO TRUE

           WHEN OTHER
               DISPLAY'ERROR LECTURA REGISTRO: ' WS-FS-NOTA
               SET WS-FIN-PROCESO TO TRUE

           END-EVALUATE


           IF LK-ANIO = NOT-ANIO
               SET WS-SI-ANIO TO TRUE
               IF NOT-MES >= LK-MES
                   ADD 1 TO WS-CANT
                   ADD NOT-NOTA TO WS-SUM-NOTA
               END-IF
           ELSE
               SET WS-NO-ANIO TO TRUE
           END-IF

           PERFORM 2800-BUSCAALUMNO UNTIL WS-ENCONTRADO

           DISPLAY '*ALUMNO: ' ALU-NOMBRE
                      '         NACIONALIDAD: ' LK-NOM-NAC

           DISPLAY LIT-SPACES2.



      ***********************************************
      *        CUERPO PROCESOS                      *
      ***********************************************

       2000-PROCESOS.

           READ NOTAS

           EVALUATE WS-FS-NOTA

           WHEN 00
               PERFORM 2050-GENERA-INFORME

           WHEN 10
               DISPLAY LIT-SPACES
               DISPLAY '*FIN ARCHIVO* ' WS-FS-NOTA
               DISPLAY LIT-SPACES
               IF WS-NO-ANIO
                  DISPLAY LIT-SPACES
                  DISPLAY LIT-SPACES
                  DISPLAY '** INFORME PARA EL ANIO ' LK-ANIO
                            ': NO EXISTEN DATOS PARA EL ANIO INGRESADO'
                  DISPLAY 'CANTIDAD DE ALUMNOS ANALISADOS: ' WS-CONT-ALU
                  DISPLAY LIT-SPACES
               END-IF

               SET WS-FIN-PROCESO TO TRUE

           WHEN OTHER
               DISPLAY'ERROR LECTURA REGISTRO: ' WS-FS-NOTA
               SET WS-FIN-PROCESO TO TRUE


           END-EVALUATE.



     **********PROCESOS PARA GENERAR INFORME***

       2050-GENERA-INFORME.

           IF NOT-NRO-ALU = WS-ALU-ANT
               IF NOT-NRO-MAT = WS-MAT-ANT
                   PERFORM 3000-COMPARAFECHA
               ELSE
                    PERFORM 2600-CORTE-MATERIA
               END-IF
           ELSE
               PERFORM 2400-CORTE-ALUMNO

           END-IF.


      *****CORTE MAYOR - ALUMNO ********

       2400-CORTE-ALUMNO.


           PERFORM 2600-CORTE-MATERIA

           PERFORM 2800-BUSCAALUMNO

           DISPLAY LIT-SPACES
           DISPLAY '*ALUMNO: ' ALU-NOMBRE
                      '         NACIONALIDAD: ' LK-NOM-NAC


           MOVE NOT-NRO-ALU TO WS-ALU-ANT
           MOVE NOT-NRO-MAT TO WS-MAT-ANT.




      ******CORTE MENOR - MATERIA *****

       2600-CORTE-MATERIA.

           DIVIDE WS-SUM-NOTA BY WS-CANT GIVING WS-PROM

           MOVE WS-MAT-ANT TO LK-NUM-MATE
           MOVE 2 TO LK-OPCION-MAT
           CALL WS-RUTIM USING LK-COM-MATERIA

           MOVE WS-PROM TO WP-PROM

           IF WS-CANT NOT EQUAL 0
               DISPLAY 'MATERIA: ' LK-NOM-MATE
                      '             PROMEDIO: ' WP-PROM
           END-IF

           MOVE 0 TO WS-SUM-NOTA
           MOVE 0 TO WS-CANT
           MOVE 0 TO WS-PROM


           PERFORM 3000-COMPARAFECHA

           MOVE NOT-NRO-ALU TO WS-ALU-ANT
           MOVE NOT-NRO-MAT TO WS-MAT-ANT.


      *****BUSCA DATOS DEL ALUMNO ********
       2800-BUSCAALUMNO.

           SET WS-NO-ENCONTRADO TO TRUE

           READ ALUMNOS
           IF WS-FS-ALU IS NOT EQUAL '00'
               DISPLAY'ERROR LECTURA REGISTRO: ' WS-FS-ALU
               SET WS-FIN-PROCESO TO TRUE
           END-IF

           IF NOT-NRO-ALU = ALU-NRO-ALU
               ADD 1 TO WS-CONT-ALU
               MOVE ALU-NRO-PAIS TO LK-NUM-NAC
               PERFORM 2900-BUSCANACIONALIDAD
               SET WS-ENCONTRADO TO TRUE
           ELSE
               IF WS-FS-ALU = 10
                   DISPLAY 'ALUMNO NO EXISTE.'
           END-IF.


      ***
      ******LLAMA RUTINA PARA BUSCAR NOM-NACIONALIDAD *****

       2900-BUSCANACIONALIDAD.

           MOVE 2 TO LK-OPCION-NAC

           CALL WS-RUTIN USING LK-COM-NACIONALIDAD.


      ******COMPARA FECHAS Y SUMA

       3000-COMPARAFECHA.

           IF LK-ANIO = NOT-ANIO
               SET WS-SI-ANIO TO TRUE
               IF NOT-MES >= LK-MES
                   ADD 1 TO WS-CANT
                   ADD NOT-NOTA TO WS-SUM-NOTA
               END-IF
           END-IF.


      ***********************************************
      *     CIERRE DE ARCHIVOS                      *
      ***********************************************

        9999-CIERRE.

            CLOSE NOTAS
            IF WS-FS-NOTA IS NOT EQUAL '00'
                DISPLAY 'ERROR EN CIERRE DE ARCHIVO NOTA'
                          WS-FS-NOTA
                SET WS-FIN-PROCESO TO TRUE
            END-IF


           CLOSE ALUMNOS
           IF WS-FS-ALU NOT EQUAL '00'
             DISPLAY 'ERROR EN CIERRE DE ARCHIVO: ' WS-FS-ALU
             SET WS-FIN-PROCESO TO TRUE
           END-IF.




       END PROGRAM PGMINFOR.
