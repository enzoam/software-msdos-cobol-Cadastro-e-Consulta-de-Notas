       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELDISCI-COB.
      *       SISTEMA DE NOTAS
      *    ANALISTA: FABIO
      *    PROGRAMADOR(A): FABIO
      *    FINALIDADE: EFETUA A EMISSAO DO RELATORIO:TELA E IMPRESSORA
      *                RELACAO DE DISCIPLINA DOS CURSOS A PARTIR DO
      *                CADASTRO DE DISCIPLINA 

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADDISCI ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-DIS
                       FILE STATUS STATUS-DIS.

           SELECT RELDISCI ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.

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

       FD RELDISCI
           LABEL RECORD OMITTED.
       01  REG-ORELATO              PIC X(80).


       WORKING-STORAGE SECTION.
       01  STATUS-DIS              PIC X(02) VALUE SPACE.
       01  CONTLIN                 PIC 99 VALUE 99.
       01  CONTPAG                 PIC 9(05) VALUE ZEROS.
       01  REL-RESP                PIC 9.

       01  CAB1.
           05 FILLER               PIC X(33) VALUE 'CURSO'.
           05 FILLER               PIC X(37) VALUE 'SISTEMA DE NOTAS'.
           05 FILLER               PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1             PIC ZZ.ZZ9.

       01  CAB2.
           05 FILLER               PIC X(24) VALUE SPACES.
           05 FILLER               PIC X(56) VALUE
           'RELACAO DE DISCIPLINAS CADASTRADAS'.

       01  CAB3.
           05 FILLER               PIC X(80) VALUE
           '    [ COD CURSO ] [ COD DISCIPLINA ] [ DESC. DISCIPLINA ]'.

       01  DET1.
           05 FILLER               PIC X(06) VALUE SPACES.
           05 COD-CURSO-DET1       PIC 9(02).
           05 FILLER               PIC X(12) VALUE SPACES.
           05 COD-DISCI-DET1       PIC X(04).
           05 FILLER               PIC X(15) VALUE SPACES.
           05 NOME-DISCI-DET1      PIC X(35).
           05 FILLER               PIC X(06) VALUE SPACES.

       SCREEN SECTION.
       01  TELA-INICIO.
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
           05  LINE 05 COLUMN 10   VALUE "ออออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออ".
           05  LINE 05 COLUMN 80 VALUE "บ".
           05  LINE 06 COLUMN 01 VALUE "บ".
           05  LINE 06 COLUMN 80 VALUE "บ".
           05  LINE 07 COLUMN 01 VALUE "บ".
           05  LINE 07 COLUMN 80 VALUE "บ".
           05  LINE 08 COLUMN 01 VALUE "บ".
           05  LINE 08 COLUMN 80 VALUE "บ".
           05  LINE 09 COLUMN 01 VALUE "บ".
           05  LINE 09 COLUMN 11   VALUE "ษอออออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออป".
           05  LINE 09 COLUMN 80 VALUE "บ".
           05  LINE 10 COLUMN 01 VALUE "บ".
           05  LINE 10 COLUMN 11 VALUE "บ".
           05  LINE 10 COLUMN 69 VALUE "บ".
           05  LINE 10 COLUMN 80 VALUE "บ".
           05  LINE 11 COLUMN 01 VALUE "บ".
           05  LINE 11 COLUMN 11 VALUE "บ".
           05  LINE 11 COLUMN 69 VALUE "บ".
           05  LINE 11 COLUMN 80 VALUE "บ".
           05  LINE 12 COLUMN 01 VALUE "บ".
           05  LINE 12 COLUMN 11 VALUE "บ".
           05  LINE 12 COLUMN 69 VALUE "บ".
           05  LINE 12 COLUMN 80 VALUE "บ".
           05  LINE 13 COLUMN 01 VALUE "บ".
           05  LINE 13 COLUMN 11 VALUE "บ".
           05  LINE 13 COLUMN 69 VALUE "บ".
           05  LINE 13 COLUMN 80 VALUE "บ".
           05  LINE 14 COLUMN 01 VALUE "บ".
           05  LINE 14 COLUMN 11 VALUE "บ".
           05  LINE 14 COLUMN 69 VALUE "บ".
           05  LINE 14 COLUMN 80 VALUE "บ".
           05  LINE 15 COLUMN 01 VALUE "บ".
           05  LINE 15 COLUMN 11 VALUE "บ".
           05  LINE 15 COLUMN 69 VALUE "บ".
           05  LINE 15 COLUMN 80 VALUE "บ".
           05  LINE 16 COLUMN 01 VALUE "บ".
           05  LINE 16 COLUMN 11   VALUE "ศอออออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออผ".
           05  LINE 16 COLUMN 80 VALUE "บ".
           05  LINE 17 COLUMN 01 VALUE "บ".
           05  LINE 17 COLUMN 80 VALUE "บ".
           05  LINE 18 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".

       PROCEDURE DIVISION.

       0100-INICIO.
           OPEN INPUT CADDISCI
           IF STATUS-DIS = '30'
              DISPLAY (17 25) 'DISCI.DAT INEXISTENTE - ABORTADO'
              STOP ' '
              CHAIN 'muni.EXE'.

       0150-TELA.
           DISPLAY TELA-INICIO.
           DISPLAY (04 24) 'RELATORIO DOS disci CADASTRADOS'
           DISPLAY (11 22) 'ฎ 1 ฏ TELA <******> ฎ 2 ฏ IMPRESSORA'
           DISPLAY (13 30) '### OPCAO [ X ] ###'.

       0200-RESP.
           ACCEPT (13 42) REL-RESP WITH PROMPT AUTO-SKIP

           IF REL-RESP = 1
              DISPLAY (01 01) ERASE
              GO TO 0400-LE-CADASTRO-TELA.
           IF REL-RESP = 2
              OPEN OUTPUT RELDISCI
              GO TO 0300-LE-CADASTRO-IMPRESSORA.
           DISPLAY (17 33) 'RESPOSTA INVALIDA'
              GO TO 0200-RESP.

       0300-LE-CADASTRO-IMPRESSORA.
           READ CADDISCI NEXT
           IF STATUS-DIS = '10'
              CLOSE CADDISCI
              CLOSE RELDISCI
              STOP ' '
              CHAIN 'menu.EXE'.
           IF STATUS-DIS NOT= '00'
              DISPLAY (17 33) 'PROBLEMA READ ' STATUS-DIS
              CLOSE CADDISCI
              CLOSE RELDISCI
              STOP ' '
              CHAIN 'menu.EXE'.

           IF CONTLIN > 14
              ADD 1 TO CONTPAG
              MOVE CONTPAG TO PAG-CAB1
              WRITE REG-ORELATO FROM CAB1 AFTER PAGE
              WRITE REG-ORELATO FROM CAB2 AFTER 4
              WRITE REG-ORELATO FROM CAB3 AFTER 3
              MOVE 8 TO CONTLIN.

           IF COD-CURSO-DIS NOT= 00
              MOVE COD-CURSO-DIS TO COD-CURSO-DET1
              MOVE COD-DISCIPLINA TO COD-DISCI-DET1
              MOVE NOME-DISCIPLINA TO NOME-DISCI-DET1
              WRITE REG-ORELATO FROM DET1 AFTER 2
              ADD 1 TO CONTLIN.
              GO TO 0300-LE-CADASTRO-IMPRESSORA.

           GO TO 0300-LE-CADASTRO-IMPRESSORA.

       0400-LE-CADASTRO-TELA.
           READ CADDISCI NEXT
           IF STATUS-DIS = '10'
              CLOSE CADDISCI
              STOP ' '
              CHAIN 'menu.EXE'.
           IF STATUS-DIS NOT= '00'
              DISPLAY (17 33) 'PROBLEMA READ ' STATUS-DIS
              CLOSE CADDISCI
              STOP ' '
              CHAIN 'menu.EXE'.

           IF CONTLIN > 14
              ADD 1 TO CONTPAG
              MOVE CONTPAG TO PAG-CAB1
              DISPLAY (03 01) CAB1
              DISPLAY (05 01) CAB2 
              DISPLAY (08 01) CAB3
              MOVE 6 TO CONTLIN
              MOVE 08 TO LIN.

           IF COD-CURSO-DIS NOT= 00
              MOVE COD-CURSO-DIS TO COD-CURSO-DET1
              MOVE COD-DISCIPLINA TO COD-DISCI-DET1
              MOVE NOME-DISCIPLINA TO NOME-DISCI-DET1
              ADD 2 TO LIN
              DISPLAY (LIN) DET1
              ADD 1 TO CONTLIN.
           GO TO 0400-LE-CADASTRO-TELA.



