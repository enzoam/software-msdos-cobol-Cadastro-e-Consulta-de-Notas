       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADNOTAS-COB.
      * ANALISTA       : ENZO 19 - JAMILE 26
      * PROGRAMADOR(A) : ENZO 19 - JAMILE 26
      * FINALIDADE : CADASTRO E CONSULTA DAS NOTAS
      * DATA :
      * VRS         DATA           DESCRICAO
      * 1.5         15/11/2000     IMPLATACAO

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ENTNOT ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-ent
                         FILE STATUS sentnot-arq.

           SELECT CADNOT ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-cad
                         FILE STATUS scadnot-arq.

       DATA DIVISION.
       FILE SECTION.

       FD  ENTNOT
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'notas.dat'
           RECORD CONTAINS 68 CHARACTERS.

       01  REG-ENTNOT.
           05  chave-ent.
               10 ano-nota              PIC 9(04).
               10 nota-cod-alu          PIC 9(06).
               10 nota-cod-cur          PIC 9(06).
               10 nota-cod-dis          PIC 9(06).
           05  nota-bim                 PIC 9.
           05  nota                     PIC 9(02)V9.
           05  falta                    PIC 9(02).
           05  nota-nome-alu            PIC X(40).

       FD  CADNOT
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'notafim.dat'
           RECORD CONTAINS 88 CHARACTERS.

       01  REG-CADNOT.
           05  chave-cad.
               10 ano-notaf              PIC 9(04).
               10 notaf-cod-cur          PIC 9(06).
               10 notaf-cod-alu          PIC 9(06).
               10 notaf-cod-dis          PIC 9(06).
           05  notafalta OCCURS 5 TIMES.
               10 notaf                  PIC 9(02)V9.
               10 faltaf                 PIC 9(02).
           05  notaf-bim                 PIC 9.
           05  notaf-nome-alu            PIC X(40).


       WORKING-STORAGE SECTION.
       01  sentnot-arq         PIC X(02) VALUE SPACES.
       01  scadnot-arq         PIC X(02) VALUE SPACES.
       01  linha-branco        PIC X(45) VALUE SPACES.
       01  ws-resp             PIC X.
       01  ws-retorno          PIC X(02).
           88 ESC              VALUE '01'.

       SCREEN SECTION.
       01  TELA-MAIN.
           05  BLANK SCREEN.
           05  LINE 10 COLUMN 29 VALUE "[ATUALIZANDO O CADASTRO]".
           05  LINE 04 COLUMN 01   VALUE "ษออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออป".
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
           05  LINE 21 COLUMN 01 VALUE "บ".
           05  LINE 21 COLUMN 80 VALUE "บ".
           05  LINE 22 COLUMN 01 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01 VALUE "บ".
           05  LINE 23 COLUMN 80 VALUE "บ".
           05  LINE 24 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".

       PROCEDURE DIVISION.

       0010-abre-entnot.
           OPEN INPUT ENTNOT
           IF sentnot-arq = '00'
              GO TO 0100-abre-cadnot.
           DISPLAY (12 28) 'NOTA.DAT NAO EXISTE'
           STOP RUN.

       0100-abre-cadnot.
           OPEN I-O CADNOT
           IF scadnot-arq = '00'
              GO TO 0200-tela-inicio.
           DISPLAY (12 20)'NOTAF.DAT NAO EXISTE'
           DISPLAY (14 20)'DESEJA GERAR?(S OU N)  [ ]'.

       0110-resp.
           ACCEPT (14 44) ws-resp WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) linha-branco
           IF ws-resp = 'S' OR 's'
              OPEN OUTPUT CADNOT
              CLOSE CADNOT
              GO TO 0100-abre-cadnot.
           IF ws-resp = 'N' OR 'n'
              DISPLAY(17 20) 'ABORTADO'
              CLOSE ENTNOT
              STOP RUN.
           DISPLAY(17 20) 'RESPOSTA NAO E VALIDA'
           GO TO 0110-RESP.

       0200-tela-inicio.
           DISPLAY TELA-MAIN.

       0300-entnot.
           READ ENTNOT NEXT
           IF sentnot-arq = '10'
              CLOSE ENTNOT
              CLOSE CADNOT
              STOP ' '
              CHAIN 'mnotas.exe'.

           IF sentnot-arq NOT = '00'
              DISPLAY (20 30) 'ERRO DE LEITURA'
              CLOSE ENTNOT
              CLOSE CADNOT
              CHAIN 'mnotas.exe'.
           MOVE chave-ent TO chave-cad
           MOVE nota-nome-alu TO notaf-nome-alu
           READ CADNOT INVALID KEY
                GO TO 0400-testa-bim.
           GO TO 0500-testa-outros-bim.

       0400-testa-bim.
            IF nota-bim = 1
               MOVE nota-bim TO notaf-bim
               MOVE nota TO notaf(nota-bim)
               MOVE falta TO faltaf(nota-bim).
            GO TO 0600-zera.

       0500-testa-outros-bim.
           IF nota-bim > 0 AND < 6
              GO TO 0610-muda-linha.
           DISPLAY (20 20) 'X'.

       0600-zera.
            ADD 1 TO nota-bim
            IF nota-bim > 5
                GO TO 0610-muda-linha.
            MOVE ZEROS TO notaf-bim          
            MOVE ZEROS TO notaf(nota-bim)          
            MOVE ZEROS TO faltaf(nota-bim)
            GO TO 0600-zera.

       0610-muda-linha.
            IF LIN > 19 OR < 12
               MOVE 12 TO LIN
               GO TO 0700-cadnot.
            ADD 1 TO LIN.

       0700-cadnot.
            READ CADNOT INVALID KEY
                 MOVE 1 TO notaf-bim
                 WRITE REG-CADNOT
                 DISPLAY (20 26) linha-branco
                 DISPLAY (20 29) 'REGISTRO(S) GRAVADO(S)'
                 GO TO 0300-entnot.
            DISPLAY (LIN , 04) nota-nome-alu
            DISPLAY (LIN , 60) nota
            DISPLAY (LIN , 75) falta
            MOVE nota-bim TO notaf-bim
            MOVE nota TO notaf(nota-bim)          
            MOVE falta TO faltaf(nota-bim)
            REWRITE REG-CADNOT
            DISPLAY (20 26) linha-branco
            DISPLAY (20 29) 'REGISTRO(S) ATUALIZADO(S)'
            GO TO 0300-entnot.
