      ******************************************************************
      * Author:David Castillo
      * Date:05/12/2024
      * Purpose:Hacer una ruleta de la fortuna, mantenerme activo con
      * COBOL y recordar lo aprendido con Alberto.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. RULETAFORTUNA.

       ENVIRONMENT DIVISION.

         CONFIGURATION SECTION.
           SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

         FILE-CONTROL.

       DATA DIVISION.

         FILE SECTION.

         WORKING-STORAGE SECTION.

       01  VARIABLES.
      *VARIABLE PARA BUCLE CUENTA ATRAS
           05 EJECUTANDO PIC X(2) VALUES SPACES.
      *VARIABLE PARA DETENER PROGRAMA
           05 PAUSAR PIC X(1) VALUES SPACES.
      *VARIABLE PARA CALCULAR MOVIMIENTOS
           05 FUERZA PIC 9(2) VALUES ZEROS.
           05 FUERZAINICIAL PIC 9(2) VALUES ZEROS.
      *VARIABLE PARA COLOR FLECHA, 2 VERDE, 8 BLANCO
           05 COLORFLECHAUNO PIC 9(1) VALUES ZEROS.
           05 COLORFLECHADOS PIC 9(1) VALUES ZEROS.
           05 COLORFLECHATRES PIC 9(1) VALUES ZEROS.
      *VARIABLES PARA MODIFICAR LA COLOCACION DE LOS ELEMENTOS
           05 POS-X      PIC 9(2) VALUES ZEROS.
           05 POS-Y      PIC 9(2) VALUES ZEROS.
      *VARIABLES PARA ALMACENAR HORA DEL SISTEMA
           05 HORASYSTEM.
               10 HHS  PIC 9(2) VALUES ZEROS.
               10 MMS  PIC 9(2) VALUES ZEROS.
               10 SSS  PIC 9(2) VALUES ZEROS.
               10 MMSS PIC 9(2) VALUES ZEROS.
      *VARIABLES PARA ALMACENAR LA ÚLTIMA LECTURA DEL SISTEMA
           05 HORATEMPORAL.
               10 HHT  PIC 9(2) VALUES ZEROS.
               10 MMT  PIC 9(2) VALUES ZEROS.
               10 SST  PIC 9(2) VALUES ZEROS.
               10 MMST PIC 9(2) VALUES ZEROS.
      *VARIACION TEMPORAL
           05 HORADIF PIC 9(4) VALUES ZEROS.
      *TEMPORIZADOR AJUSTABLE PARA VARIAR LOS INTERVALOS DE CAMBIO
           05 TEMPORIZADOR PIC 9(4) VALUES ZEROS.




         SCREEN SECTION.

       01  PANTALLAS.
           05 PANTALLA-BLANCA BLANK SCREEN.

           05 PANTALLA-PRINCIPAL.
               10 FLECHAS.
                  15 FLECHAUNO.
                     20 LINE 16 COLUMN 35 VALUES "|"
                     FOREGROUND-COLOR COLORFLECHAUNO.
                     20 LINE 15 COLUMN 35 VALUES "|"
                     FOREGROUND-COLOR COLORFLECHAUNO.
                     20 LINE 14 COLUMN 35 VALUES "^"
                     FOREGROUND-COLOR COLORFLECHAUNO.
                  15 FLECHADOS.
                     20 LINE 18 COLUMN 37 VALUES "--->"
                     FOREGROUND-COLOR COLORFLECHADOS.
                  15 FLECHATRES.
                     20 LINE 18 COLUMN 30 VALUES "<---"
                     FOREGROUND-COLOR COLORFLECHATRES.
               10 TEXTOS.
                  15 LINE 5 COLUMN 20
                     VALUES "BIENVENIDO A LA RULETA DEL DESTINO".
                  15 LINE 7 COLUMN 25
                     VALUES "INTRODUCE TU FUERZA".
                  15 LINE 18 COLUMN 43
                     VALUES "HAS GANADO UN TRABAJO".
                  15 LINE 18 COLUMN 7
                     VALUES "HAS GANADO FORMACION".
                  15 LINE 12 COLUMN 25
                     VALUES "HAS GANADO EXPERIENCIA".


       PROCEDURE DIVISION.

           PERFORM 1000-INICIO THRU 1000-FIN-INICIO
           PERFORM 2000-PROCESO THRU 2000-FIN-PROCESO
                           UNTIL FUERZA = 0
           PERFORM 3000-FIN THRU 3000-FIN-FIN.

       1000-INICIO.

           MOVE 8 TO COLORFLECHADOS
                     COLORFLECHATRES
           MOVE 2 TO COLORFLECHAUNO

           DISPLAY PANTALLA-PRINCIPAL

           ACCEPT FUERZA WITH AUTO AT LINE 9 COLUMN 35
           MOVE FUERZA TO FUERZAINICIAL

           ACCEPT HORATEMPORAL FROM TIME.

       1000-FIN-INICIO.
           EXIT.

       2000-PROCESO.

           MOVE 10 TO TEMPORIZADOR
           PERFORM 2100-CAMBIO THRU 2100-FIN-CAMBIO
                               UNTIL FUERZA < 20

           MOVE 20 TO TEMPORIZADOR
           PERFORM 2100-CAMBIO THRU 2100-FIN-CAMBIO
                               UNTIL FUERZA < 10

           MOVE 40 TO TEMPORIZADOR
           PERFORM 2100-CAMBIO THRU 2100-FIN-CAMBIO
                               UNTIL FUERZA < 1

           ACCEPT PAUSAR WITH AUTO.

       2000-FIN-PROCESO.
           EXIT.

       2100-CAMBIO.

           ACCEPT HORASYSTEM FROM TIME
           COMPUTE HORADIF = SSS * 100 + MMSS - SST * 100 - MMST

           IF HORADIF > TEMPORIZADOR
               PERFORM 2110-CAMBIOCOLOR THRU 2110-FIN-CAMBIOCOLOR
               MOVE HORASYSTEM TO HORATEMPORAL
               ADD -1 TO FUERZA
           END-IF.

       2100-FIN-CAMBIO.
           EXIT.

       2110-CAMBIOCOLOR.

           IF COLORFLECHAUNO = 2
               MOVE 2 TO COLORFLECHADOS
               MOVE 8 TO COLORFLECHAUNO
                         COLORFLECHATRES
           ELSE
               IF COLORFLECHADOS = 2
                   MOVE 2 TO COLORFLECHATRES
                   MOVE 8 TO COLORFLECHAUNO
                             COLORFLECHADOS
               ELSE
                   MOVE 2 TO COLORFLECHAUNO
                   MOVE 8 TO COLORFLECHADOS
                             COLORFLECHATRES
               END-IF
           END-IF

           DISPLAY PANTALLA-BLANCA
           DISPLAY PANTALLA-PRINCIPAL
           DISPLAY FUERZAINICIAL AT LINE 9 COLUMN 35.

       2110-FIN-CAMBIOCOLOR.
           EXIT.

       3000-FIN.
           ACCEPT PAUSAR WITH AUTO
           STOP RUN.

       3000-FIN-FIN.
           EXIT.

       END PROGRAM RULETAFORTUNA.
