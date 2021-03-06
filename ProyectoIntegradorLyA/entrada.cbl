IDENTIFICATION DIVISION.
PROGRAM-ID. USODEACCEPT.
AUTHOR. HIRONAKAMURA.
INSTALLATION. GITHUB.

**********************************************************
* COBCALC                                                *
*                                                        *
* Un programa simple que permite realizar funciones      *
* financieras utilizando funciones intrinsecas.          *
*                                                        *
**********************************************************

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. HP.
OBJECT-COMPUTER. HP.


DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-NOMBRE PIC A(34).
01 WS-NUMERO PIC 9(3).


PROCEDURE DIVISION.
  DISPLAY "**********************".
  DISPLAY "*** USO DE ACCEPT ***".
  DISPLAY "INTRODUCE TU NOMBRE:" WITH NO ADVANCING.
  ACCEPT WS-NOMBRE.
  DISPLAY "HOLA, "WS-NOMBRE.
  DISPLAY "LONGITUD ORIGINAL: "FUNCTION LENGTH(WS-NOMBRE).
  DISPLAY "INTRODUCE NUMERO:" WITH NO ADVANCING.
  ACCEPT WS-NUMERO.
  DISPLAY "NUMERO: "WS-NUMERO.
STOP RUN.


  