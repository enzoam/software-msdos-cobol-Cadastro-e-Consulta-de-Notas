       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADDISCI-COB.
      *     EMPRESA S / A
      * ANALISTA       : FABIO
      * PROGRAMADOR(A) : FABIO
      * FINALIDADE : Efetua CADASTRO de DISCIPLINAS no arq indexado
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

           SELECT CADDISCI ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-DIS
                       FILE STATUS STATUS-DIS.


       DATA DIVISION.
       FILE SECTION.
       FD  CADCURSO
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'CURSO.DAT'
           RECORD CONTAINS 42 CHARACTERS.

       01  REG-CADCURSO.
           05  CHAVE-CURSO.
               10  COD-CURSO           PIC 9(02).
           05  NOME-CURSO              PIC X(40).

       FD  CADDISCI
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'DISCI.DAT'
           RECORD CONTAINS 52 CHARACTERS.

       01  REG-CADDISCI.
           05  CHAVE-DIS.
               10  COD-CURSO-DIS       PIC 9(02).
               10  COD-DISCIPLINA      PIC 9(04).
               10  ANO-DISCI           PIC 9(04).
           05  NOME-DISCIPLINA         PIC X(35).
           05  QTDE-AULAS-DIS          PIC 9(03).
           05  CARGA-DIS               PIC 9(04).

       WORKING-STORAGE SECTION.
       01  STATUS-CUR          PIC X(02) VALUE SPACES.
       01  STATUS-DIS          PIC X(02) VALUE SPACES.
       01  MIN-DIS             PIC 9(02) VALUE 45.
       01  WS-RESPOSTA         PIC X.
       01  RESP-DISCI          PIC X.
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
           05  LINE 14 COLUMN 01 VALUE "บ".
           05  LINE 14 COLUMN 80 VALUE "บ".
           05  LINE 15 COLUMN 01 VALUE "บ".
           05  LINE 15 COLUMN 80 VALUE "บ".
           05  LINE 16 COLUMN 01 VALUE "บ".
           05  LINE 16 COLUMN 80 VALUE "บ".
           05  LINE 17 COLUMN 01 VALUE "บ".
           05  LINE 17 COLUMN 80 VALUE "บ".
           05  LINE 18 COLUMN 01 VALUE "บ".
           05  LINE 18 COLUMN 80 VALUE "บ".
           05  LINE 19 COLUMN 01 VALUE "บ".
           05  LINE 19 COLUMN 80 VALUE "บ".
           05  LINE 20 COLUMN 01 VALUE "บ".
           05  LINE 20 COLUMN 80 VALUE "บ".
           05  LINE 21 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
           05  LINE 22 COLUMN 01 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01 VALUE "บ".
           05  LINE 23 COLUMN 80 VALUE "บ".
           05  LINE 24 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออ".
           05  LINE 02   COLUMN 02   VALUE "CADDISCI".
           05  LINE 02   COLUMN 35   VALUE "SISTEMA DE NOTAS".
           05  LINE 02   COLUMN 73   VALUE "VRS 1.0".
           05  LINE 05   COLUMN 30   VALUE "[ CADASTRO DE DISCIPLINAS ]"
               FOREGROUND-COLOR 15.
           05  LINE 07   COLUMN 10   VALUE "CODIGO DO CURSO [    ]".
           05  LINE 09   COLUMN 10   VALUE "NOME [".
           05  LINE 09   COLUMN 72   VALUE "]".
           05  LINE 11   COLUMN 10   VALUE "CODIGO DO DISCIPLINA [ ".
           05  LINE 11   COLUMN 38   VALUE "]".
           05  LINE 13   COLUMN 10   VALUE "DESCRICAO DISCIPLINA [ ".
           05  LINE 13   COLUMN 72   VALUE "]".
           05  LINE 15   COLUMN 10   VALUE "ANO [      ]".
           05  LINE 17   COLUMN 10   VALUE "QUANTIDADES DE AULAS [".
           05  LINE 17   COLUMN 37   VALUE "]".
           05  LINE 19   COLUMN 10   VALUE "CARGA HORARIA [".
           05  LINE 19   COLUMN 32   VALUE "]".

       PROCEDURE DIVISION.

       0050-OPEN-CADCURSO.
           OPEN INPUT CADCURSO
           IF STATUS-CUR = '00'
              GO TO 0100-INICIO.
           DISPLAY (12 20) 'CADCURSO INEXISTENTE'
           STOP RUN.

       0100-INICIO.
           OPEN I-O CADDISCI
           IF STATUS-DIS = '00'
              GO TO 0200-TELA.
           DISPLAY (12 20)'DISCI.DAT INEXISTENTE'
           DISPLAY (14 20)'DESEJA GERAR(S OU N)  [ X ]'.

       0150-RESP.
           ACCEPT (14 44) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) '                                           '

           IF WS-RESPOSTA = 'S' OR 's'
              OPEN OUTPUT CADDISCI
              CLOSE CADDISCI
              GO TO 0100-INICIO.

           IF WS-RESPOSTA = 'N' OR 'n'
              DISPLAY(17 20) 'DISCIPLINA ABORTADO!'
              CHAIN 'menu.EXE'.

           DISPLAY(17 20) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 0150-RESP.


       0200-TELA.
           DISPLAY TELA-ENTRADA.

       0300-CURSO.
           ACCEPT (07 28) COD-CURSO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0200-TELA.
           DISPLAY (22 20)
             '                                                  '
           IF COD-CURSO = 0
              GO TO  1200-FINALIZA.

           READ CADCURSO INVALID KEY
                DISPLAY (22 30) 'CURSO NAO CADASTRADO'
                STOP ' '
                GO TO 0200-TELA.
           DISPLAY (09 17) NOME-CURSO
           MOVE COD-CURSO TO COD-CURSO-DIS.

       0400-DISCIPLINA.
           ACCEPT (11 33) COD-DISCIPLINA WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0300-CURSO.
           DISPLAY (22 20)
             '                                                  '
           IF COD-DISCIPLINA = 0
              DISPLAY (22 31) 'CODIGO INVALIDO '
              GO TO 0400-DISCIPLINA.

       0500-NOME-DISCIPLINA.
           ACCEPT (13 34) NOME-DISCIPLINA WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0400-DISCIPLINA.
           DISPLAY (22 20)
             '                                                  '
           IF NOME-DISCIPLINA = SPACES
              DISPLAY (22 31) 'DESCRICAO INVALIDA'
              GO TO 0500-NOME-DISCIPLINA.

       0600-ANO-DISCI.
           ACCEPT (15 16) ANO-DISCI WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0500-NOME-DISCIPLINA.
           DISPLAY (22 20)
             '                                                   '
           IF ANO-DISCI < 1997 OR > 2004
              DISPLAY (22 31) 'ANO INVALIDO'
              GO TO 0600-ANO-DISCI.
           READ CADDISCI INVALID KEY
                GO TO 0700-QTDE-AULAS-DIS.
           DISPLAY (22 20)
             '[CODIGO-ANO] DE DISCIPLINA JA CADASTRADA P/ CURSO'
           GO TO 0600-ANO-DISCI.


       0700-QTDE-AULAS-DIS.
           ACCEPT (17 33) QTDE-AULAS-DIS WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0600-ANO-DISCI.
           DISPLAY (22 20)
             '                                                  '
           IF QTDE-AULAS-DIS < 40
              DISPLAY (22 31) 'QUANTIDADE INVALIDA'
                 GO TO 0700-QTDE-AULAS-DIS.

       0800-CARGA-DIS.
           MULTIPLY QTDE-AULAS-DIS BY MIN-DIS GIVING CARGA-DIS
           DIVIDE 60 INTO CARGA-DIS GIVING CARGA-DIS
           ROUNDED
           DISPLAY (19 26) CARGA-DIS.

       0900-RESP-DISCI.
           DISPLAY (22 20)
             '                                                  '
           DISPLAY (22 20) 'CONFIRMA INCLUSAO DO CURSO(S OU N)  [ X ]'
           ACCEPT (22 58) RESP-DISCI WITH PROMPT AUTO-SKIP
           DISPLAY (22 20)
             '                                                  '

           IF RESP-DISCI = 'N' OR 'n'
              DISPLAY (22 27)'DISCIPLINA NAO CADASTRADA'
              STOP ' '
              GO TO 0200-TELA.

           IF RESP-DISCI = 'S' OR 's'
              READ CADDISCI INVALID KEY
                   WRITE REG-CADDISCI
                   DISPLAY (22 29)'DISCIPLINA CADASTRADA'
                   STOP ' '
                   GO TO 0200-TELA.

           DISPLAY (22 30)'RESPOSTA INVALIDA'
           GO TO 0900-RESP-DISCI.

       1200-FINALIZA.
           DISPLAY (22 17)
           "[ENTER] P/ CONTINUAR  [F] P/FINALIZAR    [   ]".

       1300-RESPOSTA.
           ACCEPT (22 60) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (22 17)
            '                                              '
           IF WS-RESPOSTA = SPACES
              GO TO 0200-TELA.

           IF WS-RESPOSTA NOT= "F" AND "f"
              DISPLAY(23 33) 'RESPOSTA INVALIDA'
              GO TO 1200-FINALIZA.
           CLOSE CADCURSO
                 CADDISCI
           CHAIN 'menu.EXE'.
