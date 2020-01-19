       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELCURSO-COB.
      *       SISTEMA DE NOTAS
      *    ANALISTA: FABIO
      *    PROGRAMADOR(A): FABIO
      *    FINALIDADE: EFETUA A EMISSAO DO RELATORIO:TELA E IMPRESSORA
      *                RELACAO DE CURSOS A PARTIR DO CADASTRO DE CURSOS

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

           SELECT REL2CURSO ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  CADCURSO
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'CURSO.DAT'
           RECORD CONTAINS 42 CHARACTERS.

       01  REG-CADCURSO.
           05 CHAVE-CURSO.
              10  COD-CURSO        PIC 9(02).
           05 NOME-CURSO           PIC X(40).

       FD REL2CURSO
           LABEL RECORD OMITTED.
       01  REG-ORELATO              PIC X(80).


       WORKING-STORAGE SECTION.
       01  STATUS-CUR              PIC X(02) VALUE SPACE.
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
           'RELACAO DE CURSOS CADASTRADO'.

       01  CAB3.
           05 FILLER               PIC X(80) VALUE
           '          CODIGO DO CURSO          DESCRICAO DO CURSO'.

       01  DET1.
           05 FILLER               PIC X(10) VALUE SPACES.
           05 COD-CURSO-DET1       PIC 9(02).
           05 FILLER               PIC X(23) VALUE SPACES.
           05 NOME-CURSO-DET1      PIC X(40).
           05 FILLER               PIC X(05) VALUE SPACES.

       SCREEN SECTION.
       01  TELA-INICIO.
           05  BLANK SCREEN.
           05  LINE 01 COLUMN 01   VALUE "������������������������������
      -            "������������������������������������������������ͻ".
           05  LINE 02 COLUMN 01 VALUE "�".
           05  LINE 02 COLUMN 80 VALUE "�".
           05  LINE 03 COLUMN 01 VALUE "�".
           05  LINE 03 COLUMN 80 VALUE "�".
           05  LINE 04 COLUMN 01 VALUE "�".
           05  LINE 04 COLUMN 80 VALUE "�".
           05  LINE 05 COLUMN 01 VALUE "�".
           05  LINE 05 COLUMN 10   VALUE "������������������������������
      -            "�������������������������������".
           05  LINE 05 COLUMN 80 VALUE "�".
           05  LINE 06 COLUMN 01 VALUE "�".
           05  LINE 06 COLUMN 80 VALUE "�".
           05  LINE 07 COLUMN 01 VALUE "�".
           05  LINE 07 COLUMN 80 VALUE "�".
           05  LINE 08 COLUMN 01 VALUE "�".
           05  LINE 08 COLUMN 80 VALUE "�".
           05  LINE 09 COLUMN 01 VALUE "�".
           05  LINE 09 COLUMN 11   VALUE "������������������������������
      -            "���������������������������ͻ".
           05  LINE 09 COLUMN 80 VALUE "�".
           05  LINE 10 COLUMN 01 VALUE "�".
           05  LINE 10 COLUMN 11 VALUE "�".
           05  LINE 10 COLUMN 69 VALUE "�".
           05  LINE 10 COLUMN 80 VALUE "�".
           05  LINE 11 COLUMN 01 VALUE "�".
           05  LINE 11 COLUMN 11 VALUE "�".
           05  LINE 11 COLUMN 69 VALUE "�".
           05  LINE 11 COLUMN 80 VALUE "�".
           05  LINE 12 COLUMN 01 VALUE "�".
           05  LINE 12 COLUMN 11 VALUE "�".
           05  LINE 12 COLUMN 69 VALUE "�".
           05  LINE 12 COLUMN 80 VALUE "�".
           05  LINE 13 COLUMN 01 VALUE "�".
           05  LINE 13 COLUMN 11 VALUE "�".
           05  LINE 13 COLUMN 69 VALUE "�".
           05  LINE 13 COLUMN 80 VALUE "�".
           05  LINE 14 COLUMN 01 VALUE "�".
           05  LINE 14 COLUMN 11 VALUE "�".
           05  LINE 14 COLUMN 69 VALUE "�".
           05  LINE 14 COLUMN 80 VALUE "�".
           05  LINE 15 COLUMN 01 VALUE "�".
           05  LINE 15 COLUMN 11 VALUE "�".
           05  LINE 15 COLUMN 69 VALUE "�".
           05  LINE 15 COLUMN 80 VALUE "�".
           05  LINE 16 COLUMN 01 VALUE "�".
           05  LINE 16 COLUMN 11   VALUE "������������������������������
      -            "���������������������������ͼ".
           05  LINE 16 COLUMN 80 VALUE "�".
           05  LINE 17 COLUMN 01 VALUE "�".
           05  LINE 17 COLUMN 80 VALUE "�".
           05  LINE 18 COLUMN 01   VALUE "������������������������������
      -            "������������������������������������������������ͼ".

       PROCEDURE DIVISION.

       0100-INICIO.
           OPEN INPUT CADCURSO
           IF STATUS-CUR = '30'
              DISPLAY (17 25) 'CURSO.DAT INEXISTENTE - ABORTADO'
              STOP ' '
              CHAIN 'menui.EXE'.

       0150-TELA.
           DISPLAY TELA-INICIO.
           DISPLAY (04 24) 'RELATORIO DOS CURSOS CADASTRADOS'
           DISPLAY (11 22) '� 1 � TELA <******> � 2 � IMPRESSORA'
           DISPLAY (13 30) '### OPCAO [ X ] ###'.

       0200-RESP.
           ACCEPT (13 42) REL-RESP WITH PROMPT AUTO-SKIP

           IF REL-RESP = 1
              DISPLAY (01 01) ERASE
              GO TO 0400-LE-CADASTRO-TELA.
           IF REL-RESP = 2
              OPEN OUTPUT REL2CURSO
              GO TO 0300-LE-CADASTRO-IMPRESSORA.
           DISPLAY (17 33) 'RESPOSTA INVALIDA'
              GO TO 0200-RESP.

       0300-LE-CADASTRO-IMPRESSORA.
           READ CADCURSO NEXT
           IF STATUS-CUR = '10'
              CLOSE CADCURSO
              CLOSE REL2CURSO
              STOP ' '
              CHAIN 'menu.EXE'.
           IF STATUS-CUR NOT= '00'
              DISPLAY (17 33) 'PROBLEMA READ ' STATUS-CUR
              CLOSE CADCURSO
              CLOSE REL2CURSO
              STOP ' '
              CHAIN 'menu.EXE'.

           IF CONTLIN > 14
              ADD 1 TO CONTPAG
              MOVE CONTPAG TO PAG-CAB1
              WRITE REG-ORELATO FROM CAB1 AFTER PAGE
              WRITE REG-ORELATO FROM CAB2 AFTER 4
              WRITE REG-ORELATO FROM CAB3 AFTER 3
              MOVE 6 TO CONTLIN.

           IF COD-CURSO NOT= 00
              MOVE COD-CURSO TO COD-CURSO-DET1
              MOVE NOME-CURSO TO NOME-CURSO-DET1
              WRITE REG-ORELATO FROM DET1 AFTER 2
              ADD 1 TO CONTLIN.

           GO TO 0300-LE-CADASTRO-IMPRESSORA.

       0400-LE-CADASTRO-TELA.
           READ CADCURSO NEXT
           IF STATUS-CUR = '10'
              CLOSE CADCURSO
              STOP ' '
              CHAIN 'menu.EXE'.
           IF STATUS-CUR NOT= '00'
              DISPLAY (17 33) 'PROBLEMA READ ' STATUS-CUR
              CLOSE CADCURSO
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

           IF COD-CURSO NOT= 00
              MOVE COD-CURSO TO COD-CURSO-DET1
              MOVE NOME-CURSO TO NOME-CURSO-DET1
              ADD 02 TO LIN
              DISPLAY (LIN) DET1
              ADD 1 TO CONTLIN.
           GO TO 0400-LE-CADASTRO-TELA.



