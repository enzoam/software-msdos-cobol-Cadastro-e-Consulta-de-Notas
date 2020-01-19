
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCURSO-COB.
      *     EMPRESA S / A
      * ANALISTA       : FABIO
      * PROGRAMADOR(A) : FABIO
      * FINALIDADE : Efetua CADASTRO de dados CURSO no arq indexado
      * DATA : 29/03/2000
      * VRS         DATA           DESCRICAO
      * 1.0      29/03/2000        IMPLATACAO

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCURSO ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-CURSO
                       FILE STATUS STATUS-CUR.

       DATA DIVISION.
       FILE SECTION.
       FD  CADCURSO
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'CURSO.DAT'
           RECORD CONTAINS 42 CHARACTERS.
       01  REG-CADCURSO.
           05 CHAVE-CURSO.
              10 COD-CURSO         PIC 9(02).
           05 NOME-CURSO           PIC X(40).

       WORKING-STORAGE SECTION.
       01  STATUS-CUR          PIC X(02) VALUE SPACES.
       01  WS-RESPOSTA         PIC X.
       01  RESP-CURSO          PIC X.
       01  RETORNO             PIC X(02).
           88 ESC              VALUE '01'.

       SCREEN SECTION.
       01  TELA-ENTRADA.
           05  BLANK SCREEN.
           05  LINE 01 COLUMN 01   VALUE "ษอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออป".
           05  LINE 02 COLUMN 01 VALUE "บ".
           05  LINE 02 COLUMN 80 VALUE "บ".
           05  LINE 03 COLUMN 01 VALUE "บ".
           05  LINE 03 COLUMN 80 VALUE "บ".
           05  LINE 04 COLUMN 01 VALUE "บ".
           05  LINE 04 COLUMN 80 VALUE "บ".
           05  LINE 05 COLUMN 01 VALUE "บ".
           05  LINE 05 COLUMN 80 VALUE "บ".
           05  LINE 06 COLUMN 01 VALUE "บ".
           05  LINE 06 COLUMN 80 VALUE "บ".
           05  LINE 07 COLUMN 01 VALUE "บ".
           05  LINE 07 COLUMN 80 VALUE "บ".
           05  LINE 08 COLUMN 01 VALUE "บ".
           05  LINE 08 COLUMN 80 VALUE "บ".
           05  LINE 09 COLUMN 01 VALUE "บ".
           05  LINE 09 COLUMN 80 VALUE "บ".
           05  LINE 10 COLUMN 01 VALUE "บ".
           05  LINE 10 COLUMN 80 VALUE "บ".
           05  LINE 11 COLUMN 01 VALUE "บ".
           05  LINE 11 COLUMN 80 VALUE "บ".
           05  LINE 12 COLUMN 01 VALUE "บ".
           05  LINE 12 COLUMN 80 VALUE "บ".
           05  LINE 13 COLUMN 01 VALUE "บ".
           05  LINE 13 COLUMN 80 VALUE "บ".
           05  LINE 14 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
           05  LINE 15 COLUMN 01 VALUE "บ".
           05  LINE 15 COLUMN 13   VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -            "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
           05  LINE 15 COLUMN 80 VALUE "บ".
           05  LINE 16 COLUMN 01 VALUE "บ".
           05  LINE 16 COLUMN 13 VALUE "ณ".
           05  LINE 16 COLUMN 66 VALUE "ณ".
           05  LINE 16 COLUMN 80 VALUE "บ".
           05  LINE 17 COLUMN 01 VALUE "บ".
           05  LINE 17 COLUMN 13 VALUE "ณ".
           05  LINE 17 COLUMN 66 VALUE "ณ".
           05  LINE 17 COLUMN 80 VALUE "บ".
           05  LINE 18 COLUMN 01 VALUE "บ".
           05  LINE 18 COLUMN 13 VALUE "ณ".
           05  LINE 18 COLUMN 66 VALUE "ณ".
           05  LINE 18 COLUMN 80 VALUE "บ".
           05  LINE 19 COLUMN 01 VALUE "บ".
           05  LINE 19 COLUMN 13 VALUE "ณ".
           05  LINE 19 COLUMN 66 VALUE "ณ".
           05  LINE 19 COLUMN 80 VALUE "บ".
           05  LINE 20 COLUMN 01 VALUE "บ".
           05  LINE 20 COLUMN 13 VALUE "ณ".
           05  LINE 20 COLUMN 66 VALUE "ณ".
           05  LINE 20 COLUMN 80 VALUE "บ".
           05  LINE 21 COLUMN 01 VALUE "บ".
           05  LINE 21 COLUMN 13   VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -            "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
           05  LINE 21 COLUMN 80 VALUE "บ".
           05  LINE 22 COLUMN 01 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
           05  LINE 02   COLUMN 02   VALUE "CADCURSO".
           05  LINE 02   COLUMN 35   VALUE "SISTEMA DE NOTAS".
           05  LINE 02   COLUMN 73   VALUE "VRS 1.0".
           05  LINE 05   COLUMN 30   VALUE "[ CADASTRAMENTO DO CURSO ]"
               FOREGROUND-COLOR 15.
           05  LINE 09   COLUMN 16   VALUE "CODIGO DO CURSO [    ]".
           05  LINE 11   COLUMN 16   VALUE "NOME [".
           05  LINE 11   COLUMN 64   VALUE "].".

       PROCEDURE DIVISION
       0100-INICIO.
           OPEN I-O CADCURSO
           IF STATUS-CUR = '00'
                GO TO 0200-TELA.
           DISPLAY (12 20)'CURSO.DAT INEXISTENTE'
           DISPLAY (14 20)'DESEJA GERAR(S OU N)  [ X ]'.

       0150-RESP.
           ACCEPT (14 44) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) '                                           '

           IF WS-RESPOSTA = 'S' OR 's'
              OPEN OUTPUT CADCURSO
              CLOSE CADCURSO
              GO TO 0100-INICIO.

           IF WS-RESPOSTA = 'N' OR 'n'
              DISPLAY(17 20) 'CURSO ABORTADO!'
              CHAIN 'menu.EXE'.

           DISPLAY(17 20) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 0150-RESP.

       0200-TELA.
           DISPLAY TELA-ENTRADA.

       0300-CURSO.
           ACCEPT (09 34) COD-CURSO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0200-TELA.

           IF COD-CURSO = 0
              GO TO  1200-FINALIZA.

           IF COD-CURSO<10
              DISPLAY(17 26) 'CODIGO DO CURSO INVALIDO     '
              GO TO 0300-CURSO.

           READ CADCURSO INVALID KEY
                     GO TO 0400-NOME-CURSO.

           DISPLAY(17 26)'CODIGO DO CURSO JA CADASTRADO'
           GO TO 0300-CURSO.

       0400-NOME-CURSO.
           ACCEPT (11 23) NOME-CURSO WITH PROMPT AUTO-SKIP
           DISPLAY (17 17)
             '                                           '
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
             DISPLAY (11 23) '                                         '
             GO TO 0300-CURSO.

           IF NOME-CURSO = SPACES
              DISPLAY (17 34) "NOME INVALIDO"
              GO TO 0400-NOME-CURSO.

       0450-RESP-CURSO.
           DISPLAY (17 20) '                '
           DISPLAY (17 20) 'CONFIRMA INCLUSAO DO CURSO(S OU N)  [ X ]'
           ACCEPT (17 58) RESP-CURSO WITH PROMPT AUTO-SKIP
           DISPLAY (17 2) '                                         '

           IF RESP-CURSO = 'N' OR 'n'
              DISPLAY (17 20)
                '                                         '
              DISPLAY (17 30)
               'CURSO NAO CADASTRADO'
              STOP ' '
              GO TO 0200-TELA.

           IF RESP-CURSO NOT= 'S' AND 's'
              GO TO 0450-RESP-CURSO.

           READ CADCURSO INVALID KEY
              WRITE REG-CADCURSO
              DISPLAY (17 20)
                '                                         '
              DISPLAY (17 33)
               'CURSO CADASTRADO'
              STOP ' '
              GO TO 0200-TELA.

           DISPLAY (17 20) 'ERRO - CURSO JA EXISTENTE'
           CLOSE CADCURSO
           STOP RUN.


       1200-FINALIZA.
           DISPLAY (17 17)
           "[ENTER] P/ CONTINUAR  [F] P/FINALIZAR    [   ]".

       1300-RESPOSTA.
           ACCEPT (17 60) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (17 17)
            '                                              '
           IF WS-RESPOSTA = SPACES
              GO TO 0200-TELA.

           IF WS-RESPOSTA NOT= "F" AND "f"
              DISPLAY(18 33) 'RESPOSTA INVALIDA'
              GO TO 1200-FINALIZA.
           CLOSE CADCURSO
           CHAIN 'menu.EXE'.

