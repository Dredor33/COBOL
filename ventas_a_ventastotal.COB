      ******************************************************************
      * Author:David Castillo
      * Date:12/12/2024
      * Purpose: Dado el archivo VENTAS.DAT con copy:
      *    código de producto (5 caracteres)
      *    cantidad vendida (3 digitos)
      *    precio unitario (5 digitos con dos decimales)
      *    se solicita leer el archivo, calcular el total vendido y
      *    guardar la información en el archivo TOTALVENTAS.DAT con copy
      *    código de producto (5 caracteres)
      *    total vendido con dos decimales

      *    reto adicional calcula y muestra el producto más vendido
      *    en cantidad

      *    añadido parametros adicionales

      *    se tomara un máximo de 99 registros en el archivo VENTAS.DAT

      * Tectonics: cobc

      ******************************************************************
      *                 IDENTIFICATION DIVISION
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VENTAS-A-TOTALVENTAS.

      ******************************************************************
      *                 ENVORINMENT DIVISION
      ******************************************************************

       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
      *--------------ARCHIVO DE ENTRADA
           SELECT VENTAS ASSIGN TO "VENTAS.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-VENTAS.
      *--------------ARCHIVO DE SALIDA
           SELECT TOTAL ASSIGN TO "TOTALVENTAS.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-TOTAL.
      *--------------ARCHIVO DE SALIDA
           SELECT ERRORCODIGO ASSIGN TO "ERRORCODIGO.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-TOTAL.
      *--------------ARCHIVO DE SALIDA
           SELECT ERRORVENTA ASSIGN TO "ERRORVENTA.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-TOTAL.

      ******************************************************************
      *                 DATA DIVISION
      ******************************************************************

       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.

           FD VENTAS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REG-VENTAS-DAT PIC X(13).

           FD TOTAL
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
      *    LA CANTIDAD TOTAL MÁXIMA ES DE 8 DIGITOS
       01  REG-TOTAL-DAT PIC X(13).

           FD ERRORCODIGO
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
      *AL NO TENER CODIGO SE GUARDA EL NUMERO DE REGISTRO
      *EL EJERCICIO SE REALIZA CON MAX 99 VENTAS
       01  REG-ERRORCODIGO-DAT PIC X(10).

           FD ERRORVENTA
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
      *ALMACENAMOS LOS REGISTROS QUE SU CANTIDAD Y/O IMPORTE SEA 0
       01  REG-ERRORVENTAS-DAT PIC X(13).


      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

       01  FILE-STATUS.
           05 FS-VENTAS         PIC 9(2) VALUES ZEROS.
           05 FS-TOTAL          PIC 9(2) VALUES ZEROS.
           05 FS-ERRORCODIGO    PIC 9(2) VALUES ZEROS.
           05 FS-ERRORVENTA     PIC 9(2) VALUES ZEROS.

       01  SWITCH-END-OF-FILE.
           05 SW-EOF-VENTAS     PIC X(2) VALUES "NO".

      *    LAS COPYS PODRIAN ESTAR EN ARCHIVOS APARTE
       01  COPYS.
           02 REG-VENTAS.
               05 CODIGO-VENTAS PIC X(5) VALUES SPACES.
               05 CANT-VENTAS   PIC 9(3) VALUES ZEROS.
               05 PRECIO-VENTAS PIC 9(3)V99 VALUES ZEROS.

           02 REG-TOTAL.
               05 CODIGO-TOTAL  PIC X(5) VALUES SPACES.
               05 INGRESO-TOTAL PIC 9(6)V99 VALUES ZEROS.

           02 REG-ERRORCODIGO.
               05 NUMERO-REGISTRO    PIC 9(2) VALUES ZEROS.
               05 CANT-ERRORCODIGO   PIC 9(3) VALUES ZEROS.
               05 PRECIO-ERRORCODIGO PIC 9(3)V99 VALUES ZEROS.

           02 REG-ERRORVENTA.
               05 CODIGO-ERRORVENTAS PIC X(5) VALUES SPACES.
               05 CANT-ERRORVENTAS   PIC 9(3) VALUES ZEROS.
               05 PRECIO-ERRORVENTAS PIC 9(3)V99 VALUES ZEROS.

      * LOS CONTADORES SERAN DE 2 DIGITOS
       01  CONTADORES.
      *CONTADOR DE REGISTROS LEIDOS DE VENTAS
           05 CONT-REG-VENTAS   PIC 9(2) VALUES ZEROS.
      *CONTADOR DE REGISTROS GRABADOS EN TOTAL
           05 CONT-REG-TOTAL    PIC 9(2) VALUES ZEROS.
      *CONTADOR DE EMPATES EN CANTIDAD
           05 CONT-EMPATES      PIC 9(2) VALUES ZEROS.
      *CONTADOR DE ERRORES DE CODIGO
           05 CONT-ERRORCODIGO  PIC 9(2) VALUES ZEROS.
      *CONTADOR DE ERRORES DE VENTA
           05 CONT-ERRORVENTA   PIC 9(2) VALUES ZEROS.

       01  ACUMULADORES.
      *ACUMULADOR DE CANTIDADES VENDIDAS MAX 5 DIGITOS
           05 ACU-CANT          PIC 9(5)    VALUES ZEROS.
      *ACUMULADOR DE INGRESO MAX 10 DIGITOS
           05 ACU-INGRESO       PIC 9(6)V99 VALUES ZEROS.
      *VARIABLE USADA PARA CALCULAR EL INGRESO MEDIO POR PRODUCTO
       01  VARIABLES.
           05 INGRESO-MEDIO     PIC 9(6)V99 VALUES ZEROS.

      *USAMOS AUXILIARES DE REGISTRO PARA PODER HACER COMPARACIONES
       01  AUXILIARES.
           05 AUX-MAYOR-CANT.
               10 AUX-CODIGO-VENTAS PIC X(5) VALUES SPACES.
               10 AUX-CANT-VENTAS   PIC 9(3) VALUES ZEROS.

      *GESTION DE ERRORES
       01  ERROR-FICHERO.
           05 ERRORFI-EN-ACCION     PIC X(08) VALUES SPACES.
           05 ERRORFI-EN-FICHERO    PIC X(15) VALUES SPACES.
           05 ERRORFI-FS            PIC 9(02) VALUES ZEROS.

      ******************************************************************
      *                 PROCEDURE DIVISION
      ******************************************************************

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

           PERFORM 1000-INICIO  THRU 1000-FIN-INICIO
           PERFORM 2000-PROCESO THRU 2000-FIN-PROCESO
                                UNTIL SW-EOF-VENTAS = "SI"
           PERFORM 3000-FIN     THRU 3000-FIN-FIN.

       1000-INICIO.
      *------------

           INITIALIZE CONTADORES
                      ACUMULADORES
                      AUXILIARES.

           PERFORM 1100-ABRIR-FICHEROS THRU 1100-FIN-ABRIR-FICHEROS.

      *REALIZAMOS UNA LECTURA ANTICIPADA
           PERFORM 4000-LEER-VENTAS    THRU 4000-FIN-LEER-VENTAS

      *APROVECHAMOS LA LECTURA PARA DETECTAR SI EL ARCHIVO ESTA VACIO
           IF SW-EOF-VENTAS = "SI"

             PERFORM 9100-VENTAS-VACIO THRU 9100-VENTAS-VACIO

           END-IF

      *AL DEJAR EL CODIGO ANTERIOR EN CASO DE EMPATE, PARA EVITAR
      *DEJAR EN EL PRIMER EMPATE VACIO, MOVEMOS EL CODIGO Y EL EMPATE

           MOVE CODIGO-VENTAS TO AUX-CODIGO-VENTAS
           MOVE 0             TO CONT-EMPATES.

       1000-FIN-INICIO.
      *----------------
           EXIT.

       1100-ABRIR-FICHEROS.
      *--------------------
           OPEN INPUT VENTAS

           IF FS-VENTAS <> 0
               MOVE "ABRIR" TO ERRORFI-EN-ACCION
               MOVE "VENTAS.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-VENTAS TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF

           OPEN OUTPUT TOTAL

           IF FS-TOTAL <> 0
               MOVE "ABRIR" TO ERRORFI-EN-ACCION
               MOVE "TOTAL.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-TOTAL TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

           OPEN OUTPUT ERRORCODIGO

           IF FS-ERRORCODIGO <> 0
               MOVE "ABRIR" TO ERRORFI-EN-ACCION
               MOVE "ERRORCODIGO.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-ERRORCODIGO TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

           OPEN OUTPUT ERRORVENTA

           IF FS-ERRORVENTA <> 0
               MOVE "ABRIR" TO ERRORFI-EN-ACCION
               MOVE "ERRORVENTA.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-ERRORVENTA TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

       1100-FIN-ABRIR-FICHEROS.
      *------------------------
           EXIT.

       2000-PROCESO.
      *-------------
           EVALUATE TRUE

               WHEN CODIGO-VENTAS = SPACES
                   PERFORM 2300-GRABAR-ERRORCODIGO
                      THRU 2300-FIN-GRABAR-ERRORCODIGO

               WHEN CANT-VENTAS = ZEROS OR PRECIO-VENTAS = ZEROS
                   PERFORM 2400-GRABAR-ERRORVENTA
                      THRU 2400-FIN-GRABAR-ERRORVENTA

               WHEN OTHER

                   ADD CANT-VENTAS TO ACU-CANT
      *COMPARAMOS Y ACTUALIZAMOS EL MAS VENDIDO
                   PERFORM 2100-COMPARAR-CANT
                      THRU 2100-FIN-COMPARAR-CANT

                   PERFORM 2200-GRABAR-TOTAL THRU 2200-FIN-GRABAR-TOTAL

           END-EVALUATE

           PERFORM 4000-LEER-VENTAS  THRU 4000-FIN-LEER-VENTAS.

       2000-FIN-PROCESO.
      *-----------------
           EXIT.

       2100-COMPARAR-CANT.
      *-------------------
           EVALUATE TRUE
             WHEN CANT-VENTAS > AUX-CANT-VENTAS
               MOVE CANT-VENTAS TO AUX-CANT-VENTAS
               MOVE CODIGO-VENTAS TO AUX-CODIGO-VENTAS
               MOVE 0 TO CONT-EMPATES
      *AL SOLO ALMACENAR UN CODIGO QUE TENGA MAYOR VENTAS, QUIERO AÑADIR
      *UN PARAMETRO QUE INDIQUE CUANTOS CODIGOS MÁS TIENES ESAS MISMAS
      * VENTAS
             WHEN CANT-VENTAS = AUX-CANT-VENTAS
               ADD 1 TO CONT-EMPATES.

       2100-FIN-COMPARAR-CANT.
      *-----------------------
           EXIT.

       2200-GRABAR-TOTAL.
      *------------------
      *PREPARAMOS EL REGISTRO A GUARDAR EN TOTALVENTAS
           MOVE CODIGO-VENTAS TO CODIGO-TOTAL
           COMPUTE INGRESO-TOTAL = CANT-VENTAS * PRECIO-VENTAS
           ADD INGRESO-TOTAL TO ACU-INGRESO

           WRITE REG-TOTAL-DAT FROM REG-TOTAL

           IF FS-TOTAL <> 0
               MOVE "ESCRIBIR" TO ERRORFI-EN-ACCION
               MOVE "TOTALVENTAS.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-TOTAL TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF

           ADD 1 TO CONT-REG-TOTAL.

       2200-FIN-GRABAR-TOTAL.
      *----------------------
           EXIT.

       2300-GRABAR-ERRORCODIGO.
      *------------------
      *PREPARAMOS EL REGISTRO A GUARDAR EN ERRORCODIGO
      *APROVECHAMOS EL CONTADOR DE REGISTROS PARA SABER EL NUMERO DE REGISTRO
           MOVE CONT-REG-VENTAS TO NUMERO-REGISTRO
           MOVE CANT-VENTAS     TO CANT-ERRORCODIGO
           MOVE PRECIO-VENTAS   TO PRECIO-ERRORCODIGO

           WRITE REG-ERRORCODIGO-DAT FROM REG-ERRORCODIGO

           IF FS-TOTAL <> 0
               MOVE "ESCRIBIR" TO ERRORFI-EN-ACCION
               MOVE "ERRORCODIGO.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-ERRORCODIGO TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF

           ADD 1 TO CONT-ERRORCODIGO.

       2300-FIN-GRABAR-ERRORCODIGO.
      *----------------------
           EXIT.

       2400-GRABAR-ERRORVENTA.
      *------------------
      *PREPARAMOS EL REGISTRO A GUARDAR EN ERRORVENTA
           MOVE CODIGO-VENTAS   TO CODIGO-ERRORVENTAS
           MOVE CANT-VENTAS     TO CANT-ERRORVENTAS
           MOVE PRECIO-VENTAS   TO PRECIO-ERRORVENTAS

           WRITE REG-ERRORVENTAS-DAT FROM REG-ERRORVENTA

           IF FS-TOTAL <> 0
               MOVE "ESCRIBIR" TO ERRORFI-EN-ACCION
               MOVE "ERRORVENTAS.DAT" TO ERRORFI-EN-FICHERO
               MOVE FS-ERRORVENTA TO ERRORFI-FS
               PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF

           ADD 1 TO CONT-ERRORVENTA.

       2400-FIN-GRABAR-ERRORVENTA.
      *----------------------
           EXIT.

       3000-FIN.
      *---------
           PERFORM 3100-MOSTRAR-INFORME THRU 3100-FIN-MOSTRAR-INFORME

           PERFORM 3200-CERRAR-ARCHIVOS THRU 3200-FIN-CERRAR-ARCHIVOS

           STOP RUN.

       3000-FIN-FIN.
      *-------------
           EXIT.

       3100-MOSTRAR-INFORME.
      *---------------------
           COMPUTE INGRESO-MEDIO = ACU-INGRESO / ACU-CANT

           DISPLAY "ESTADISTICAS"
           DISPLAY "EL CODIGO DE MAYOR VENTAS:    " AUX-CODIGO-VENTAS
           DISPLAY "LA CANTIDAD DE MAYOR VENTAS:    " AUX-CANT-VENTAS
           DISPLAY "LA COINCIDENCIA DE MAYOR VENTAS: "CONT-EMPATES
           DISPLAY "SE HAN LEIDO:   " CONT-REG-VENTAS
           DISPLAY "SE HAN GRABADO: " CONT-REG-TOTAL
           DISPLAY "LA CANTIDAD TOTAL VENDIDA ES DE: " ACU-CANT
           DISPLAY "EL INGRESO TOTAL HA SIDO DE: " ACU-INGRESO
           DISPLAY "EL INGRESO MEDIO ES DE:      " INGRESO-MEDIO
           DISPLAY "ERRORES EN EL CODIGO:  " CONT-ERRORCODIGO
           DISPLAY "ERRORES EN LAS VENTAS: " CONT-ERRORVENTA.

       3100-FIN-MOSTRAR-INFORME.
      *-------------------------
           EXIT.

       3200-CERRAR-ARCHIVOS.
      *---------------------
           CLOSE VENTAS

           IF FS-VENTAS <> 0
             DISPLAY "EL PROGRAMA SE HA EJECUTADO CORRECTAMENTE"
             DISPLAY "PERO NO SE HA PODIDO CERRAR LOS ARCHIVOS"
             MOVE "CERRAR" TO ERRORFI-EN-ACCION
             MOVE "VENTAS.DAT" TO ERRORFI-EN-FICHERO
             MOVE FS-VENTAS TO ERRORFI-FS
             PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

           CLOSE TOTAL

           IF FS-TOTAL <> 0
             DISPLAY "EL PROGRAMA SE HA EJECUTADO CORRECTAMENTE"
             DISPLAY "PERO NO SE HA PODIDO CERRAR TOTALVENTAS.DAT"
             MOVE "CERRAR" TO ERRORFI-EN-ACCION
             MOVE "TOTALVENTAS.DAT" TO ERRORFI-EN-FICHERO
             MOVE FS-TOTAL TO ERRORFI-FS
             PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

           CLOSE ERRORCODIGO

           IF FS-ERRORCODIGO <> 0
             DISPLAY "EL PROGRAMA SE HA EJECUTADO CORRECTAMENTE"
             DISPLAY "PERO NO SE HA PODIDO CERRAR ERRORCODIGO.DAT"
             MOVE "CERRAR" TO ERRORFI-EN-ACCION
             MOVE "TOTALVENTAS.DAT" TO ERRORFI-EN-FICHERO
             MOVE FS-TOTAL TO ERRORFI-FS
             PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

           CLOSE ERRORVENTA

           IF FS-ERRORVENTA <> 0
             DISPLAY "EL PROGRAMA SE HA EJECUTADO CORRECTAMENTE"
             DISPLAY "PERO NO SE HA PODIDO CERRAR ERRORVENTA.DAT"
             MOVE "CERRAR" TO ERRORFI-EN-ACCION
             MOVE "TOTALVENTAS.DAT" TO ERRORFI-EN-FICHERO
             MOVE FS-TOTAL TO ERRORFI-FS
             PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF.

       3200-FIN-CERRAR-ARCHIVOS.
      *-------------------------
           EXIT.

       4000-LEER-VENTAS.
      *-----------------
           READ VENTAS INTO REG-VENTAS
                 AT END MOVE "SI" TO SW-EOF-VENTAS
           END-READ

           IF FS-VENTAS <> 0 AND 10
             MOVE "LEER" TO ERRORFI-EN-ACCION
             MOVE "VENTAS.DAT" TO ERRORFI-EN-FICHERO
             MOVE FS-VENTAS TO ERRORFI-FS
             PERFORM 9000-ERROR-FICHERO THRU 9000-FIN-ERROR-FICHERO
           END-IF

      *ACTUALIZAMOS PARAMETROS POR LECTURA DE REGISTRO Y VERIFICAMOS SI
      *EL NUEVO REGISTRO ES MAYOR QUE EL GUARDADO EN AUXILIARES
           IF SW-EOF-VENTAS = "NO"
             ADD 1 TO CONT-REG-VENTAS

           END-IF.

       4000-FIN-LEER-VENTAS.
      *---------------------
           EXIT.

       9000-ERROR-FICHERO.
      *-------------------
           DISPLAY "ERROR DE FICHERO EN " ERRORFI-EN-FICHERO "EN LA ACCI
      -"ON: " ERRORFI-EN-ACCION
           DISPLAY "FILE STATUS: " ERRORFI-FS

           STOP RUN.

       9000-FIN-ERROR-FICHERO.
      *-----------------------
           EXIT.

       9100-VENTAS-VACIO.
      *------------------
           CLOSE VENTAS
                 TOTAL
           DISPLAY "EL ARCHIVO VENTAS ESTA VACIO, PROGRAMA NO INICIADO"
           DISPLAY "FIN DE IMPRESION"

           STOP RUN.

       9100-FIN-VENTAS-VACIO.
      *----------------------
           EXIT.

       END PROGRAM VENTAS-A-TOTALVENTAS.
