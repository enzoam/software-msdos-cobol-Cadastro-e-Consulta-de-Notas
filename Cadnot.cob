       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADNOTAS-COB.
      *     EMPRESA S / A
      * ANALISTA       : FABIO
      * PROGRAMADOR(A) : FABIO
      * FINALIDADE : Efetua CADASTRO de NOTAS no arq indexado
      * DATA : 29/03/2000
      * VRS         DATA           DESCRICAO
      * 1.0      29/03/2000        IMPLATACAO

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ENTNOTAS ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-ENTNT
                       FILE STATUS STATUS-ENTNT.

           SELECT CADNOTAS ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-CADNT
                       FILE STATUS STATUS-CADNT.


       DATA DIVISION.
       FILE SECTION.

       FD  ENTNOTAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'NOTAS.DAT'
           RECORD CONTAINS 62 CHARACTERS.

       01  REG-ENTNOTAS.
           05  CHAVE-ENTNT.
               10 ANO-NOTA              PIC 9(04).
               10 COD-CURSO-NOTA        PIC 9(02).
               10 COD-ALUNO-NOTA        PIC 9(06).
               10 COD-DISCI-NOTA        PIC 9(04).
           05  BIM-NOTA                 PIC 9.
           05  NOTA                     PIC 99V9.
           05  FALTAS                   PIC 9(02).
           05  NOME-ALUNO-NOTA          PIC X(40).

       FD  CADNOTAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'CADNOTAS.DAT'
           RECORD CONTAINS 87 CHARACTERS.

       01  REG-CADNOTAS.
           05  CHAVE-CADNT.
               10 ANO-CADNOTA              PIC 9(04).
               10 COD-CURSO-CADNOTA        PIC 9(02).
               10 COD-ALUNO-CADNOTA        PIC 9(06).
               10 COD-DISCI-CADNOTA        PIC 9(04).
           05  NOTA-FALTA OCCURS 5 TIMES.
               10  NOTA-CADNOTA           PIC 99V9.
               10  FALTAS-CADNOTA         PIC 9(03).
           05  BIM-CADNOTA                PIC 9.
           05  NOME-ALUNO-CADNOTA         PIC X(40).

       WORKING-STORAGE SECTION.
       01  STATUS-ENTNT        PIC X(02) VALUE SPACES.
       01  STATUS-CADNT        PIC X(02) VALUE SPACES.
       01  WS-RESPOSTA         PIC X.
       01  RETORNO             PIC X(02).
           88 ESC              VALUE '01'.

       SCREEN SECTION.
       01  TELA-1.
           05  BLANK SCREEN.
           05  LINE 10 COLUMN 27 VALUE
               "ATUALIZANDO O CADASTRO..."
               FOREGROUND-COLOR 4.
           05  LINE 04 COLUMN 01   VALUE "ษอออออออออออออออออออออออออออออ
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
           05  LINE 19 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
           05  LINE 20 COLUMN 01 VALUE "บ".
           05  LINE 20 COLUMN 80 VALUE "บ".
           05  LINE 21 COLUMN 01 VALUE "บ  ษออออออออออออออออออออ".
           05  LINE 21 COLUMN 34 VALUE "*** STATUS ***".
           05  LINE 21 COLUMN 58 VALUE "อออออออออออออออออออป".
           05  LINE 21 COLUMN 80 VALUE "บ".
           05  LINE 22 COLUMN 01 VALUE "บ  บ".
           05  LINE 22 COLUMN 77 VALUE "บ".
           05  LINE 22 COLUMN 80 VALUE "บ".
           05  LINE 23 COLUMN 01   VALUE "บ  ศออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออออออออออออออออออออผ".
           05  LINE 23 COLUMN 80 VALUE "บ".
           05  LINE 24 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".

       PROCEDURE DIVISION.

       0050-OPEN-ENTNOTAS.
           OPEN INPUT ENTNOTAS
           IF STATUS-ENTNT = '30'
              DISPLAY (12 28) 'ENTNOTAS.DAT NAO EXISTE'
              STOP RUN.

       0100-INICIO.
           OPEN I-O CADNOTAS
           IF STATUS-CADNT = '00'
              GO TO 0200-TELA.
           DISPLAY (12 20) 'CADANOTAS.DAT INEXISTENTE'
           DISPLAY (14 20)'DESEJA GERAR(S OU N)  [ X ]'.

       0150-RESP.
           ACCEPT (14 44) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) '                                           '

           IF WS-RESPOSTA = 'S' OR 's'
              OPEN OUTPUT CADNOTAS
              CLOSE CADNOTAS
              GO TO 0100-INICIO.

           IF WS-RESPOSTA = 'N' OR 'n'
              DISPLAY(17 20) 'NOTAS ABORTADO!'
              CLOSE ENTNOTAS
              STOP RUN.

           DISPLAY(17 20) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 0150-RESP.


       0200-TELA.
           DISPLAY TELA-1.

       0300-LE-ENTNOTAS.
           READ ENTNOTAS NEXT
           IF STATUS-ENTNT = '10'
              CLOSE ENTNOTAS
                    CADNOTAS
              DISPLAY (23 20)
                'FINALIZADO-PRESSIONE QUALQUER TECLA P/ VOLTAR'
              STOP ' '
              CHAIN 'menu.exe'.

           IF STATUS-ENTNT NOT = '00'
              DISPLAY (22 30)
              'PROBLEMA NO READ ENTNOTAS FS ' STATUS-ENTNT
              CLOSE ENTNOTAS
                    CADNOTAS
              CHAIN 'menu.exe'.

           MOVE CHAVE-ENTNT TO CHAVE-CADNT
           MOVE NOME-ALUNO-NOTA TO NOME-ALUNO-CADNOTA
           READ CADNOTAS INVALID KEY
                GO TO 0400-VER1BIM.
           GO TO 0500-VER234BIM.

       0400-VER1BIM.
            IF BIM-NOTA = 1
               MOVE BIM-NOTA TO BIM-CADNOTA
               MOVE NOTA TO NOTA-CADNOTA(BIM-NOTA)
               MOVE FALTAS TO FALTAS-CADNOTA(BIM-NOTA).
               GO TO 0600-MOVE-ZEROS.

       0500-VER234BIM.

           IF BIM-NOTA = 1 OR 2 OR 3 OR 4 OR 5
              GO TO  1100-GRAVA-CADNOTA.


           DISPLAY (22 10) 'ERRRO PASSOU PELOS IFS'
           STOP ' '.

       0600-MOVE-ZEROS.
            ADD 1 TO BIM-NOTA
            IF BIM-NOTA > 5
                STOP ' '
                GO TO 1100-GRAVA-CADNOTA.

            MOVE ZEROS TO BIM-CADNOTA          
            MOVE ZEROS TO NOTA-CADNOTA(BIM-NOTA)          
            MOVE ZEROS TO FALTAS-CADNOTA(BIM-NOTA)
            GO TO 0600-MOVE-ZEROS.

       1100-GRAVA-CADNOTA.

            READ CADNOTAS INVALID KEY
                 MOVE 1 TO BIM-CADNOTA
                 WRITE REG-CADNOTAS
                 DISPLAY (22 26)
                       '                                   '
                 DISPLAY (22 29)'REGISTRO GRAVADO NO CADASTRO'
                 GO TO 0300-LE-ENTNOTAS.

            MOVE BIM-NOTA TO BIM-CADNOTA
            MOVE NOTA TO NOTA-CADNOTA(BIM-NOTA)          
            MOVE FALTAS TO FALTAS-CADNOTA(BIM-NOTA)

            REWRITE REG-CADNOTAS
            DISPLAY (22 26)
               '                                   '
            DISPLAY (22 29)'REGISTRO ATUALIZADO NO CADASTRO'
            GO TO 0300-LE-ENTNOTAS.

