       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELFINAL-COB.
      *       SISTEMA DE NOTAS
      *    ANALISTA: FABIO
      *    PROGRAMADOR(A): 
      *    FINALIDADE: EFETUA A EMISSAO DO RELATORIO:TELA E IMPRESSORA
      *                NOTAS E FREQUENCIA DE ALUNOS  A PARTIR DO
      *                CADASTRO DE NOTAS 

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

           SELECT CADNOTAS ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-CADNT
                       FILE STATUS STATUS-CADNT.

           SELECT SORTALU ASSIGN TO DISK.

           SELECT RELFINAL ASSIGN TO PRINTER.

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

       FD RELFINAL
           LABEL RECORD OMITTED.

       01  REG-ORELATO                    PIC X(80).

       SD SORTALU
          VALUE OF FILE-ID 'SORTALU.DAT'.

       01 REG-SORTALUNO.
           05  ANO-SD                     PIC 9(04).
           05  CURSO-SD                   PIC 9(02).
           05  COD-ALU-SD                 PIC 9(06).
           05  COD-DIS-SD                 PIC 9(04).
           05  NOT1-SD                    PIC 99V9.
           05  FAL1-SD                    PIC 9(03).
           05  NOT2-SD                    PIC 99V9.
           05  FAL2-SD                    PIC 9(03).
           05  NOT3-SD                    PIC 99V9.
           05  FAL3-SD                    PIC 9(03).
           05  NOT4-SD                    PIC 99V9.
           05  FAL4-SD                    PIC 9(03).
           05  NOT5-SD                    PIC 99V9.
           05  FAL5-SD                    PIC 9(03).
           05  BIM-SD                     PIC 9.
           05  ALUNO-SD                   PIC X(40).

       WORKING-STORAGE SECTION.
       01  STATUS-CUR              PIC X(02) VALUE SPACE.
       01  STATUS-DIS              PIC X(02) VALUE SPACE.
       01  STATUS-CADNT            PIC X(02) VALUE SPACE.
       01  CONTLIN                 PIC 99 VALUE ZEROS.
       01  CONTPAG                 PIC 9(05) VALUE ZEROS.
       01  REL-RESP                PIC 9.
       01  FREQ                    PIC 9(03).
       01  TOT-FALTAS              PIC 9(03) VALUE ZEROS.
       01  TOT-PRES                PIC 9(04) VALUE ZEROS.
       01  SITUACAO                PIC X(12).
       01  NOTA-FINAL              PIC 99V9 VALUE ZEROS.
       01  ALUNO-ANT               PIC 9(06) VALUE ZEROS.
       01  FLAG                    PIC 9 VALUE ZEROS.


       01  CAB1.
           05 FILLER               PIC X(33) VALUE 'CURSO'.
           05 FILLER               PIC X(37) VALUE 'SISTEMA DE NOTAS'.
           05 FILLER               PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1             PIC ZZ.ZZ9.

       01  CAB2.
           05 FILLER               PIC X(24) VALUE SPACES.
           05 FILLER               PIC X(56) VALUE
           'DEMONSTRATIVOS DE NOTAS E FREQUENCIAS'.

       01  CAB3.
           05 FILLER               PIC X(02) VALUE SPACES.
           05 FILLER               PIC X(36) VALUE
           '[ COD ][   DISCIPLINA   ] NOT1 NOT2'.
           05 FILLER               PIC X(42) VALUE
           'NOT3 NOT4 NOTF FINAL FREQ   SITUACAO'.

       01  DET1.
           05 FILLER               PIC X(02) VALUE SPACES.
           05 COD-CURSO-DET1       PIC 9(02).
           05 FILLER               PIC X(06) VALUE SPACES.
           05 NOME-CURSO-DET1      PIC X(40).
           05 FILLER               PIC X(30) VALUE SPACES.

       01  DET2.
           05 FILLER               PIC X(02) VALUE SPACES.
           05 COD-ALUNO-DET2       PIC 9(06).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 NOME-ALUNO-DET2      PIC X(35).
           05 FILLER               PIC X(35) VALUE SPACES.

       01  DET3.
           05 FILLER               PIC X(03) VALUE SPACES.
           05 COD-DISCI-DET3       PIC 9(04).
           05 FILLER               PIC X(03) VALUE SPACES.
           05 NOME-DISCI-DET3      PIC X(17).
           05 FILLER               PIC X(01) VALUE SPACES.
           05 NOTA1-DET3           PIC 99,9.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 NOTA2-DET3           PIC 99,9.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 NOTA3-DET3           PIC 99,9.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 NOTA4-DET3           PIC 99,9.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 NOTA5-DET3           PIC 99,9.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 NOTAF-DET3           PIC 99,9.
           05 FILLER               PIC X(02) VALUE SPACES.
           05 FREQ-DET3            PIC 9(03).
           05 FILLER               PIC X(01) VALUE "%".
           05 FILLER               PIC X(01) VALUE SPACES.
           05 SIT-DET3             PIC X(12) VALUE SPACES.



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

       0050-OPEN-CURSO.
           OPEN INPUT CADCURSO
           IF STATUS-CUR = '30'
              DISPLAY (17 25) 'CURSO.DAT INEXISTENTE - ABORTADO'
              STOP ' '
              CHAIN 'meni.EXE'.

       0100-OPEN-DISCI.
           OPEN INPUT CADDISCI
           IF STATUS-DIS = '30'
              DISPLAY (17 25) 'DISCI.DAT INEXISTENTE - ABORTADO'
              STOP ' '
              CLOSE CADCURSO
              CHAIN 'meni.EXE'.

       0200-OPEN-CADNOTAS.
           OPEN INPUT CADNOTAS
           IF STATUS-CADNT = '30'
              DISPLAY (17 25) 'CADNOTAS.DAT INEXISTENTE - ABORTADO'
              STOP ' '
              CLOSE CADCURSO
                    CADDISCI
              CHAIN 'menu.EXE'.

       0300-TELA.
           DISPLAY TELA-INICIO.
           DISPLAY (04 24) 'RELATORIO DOS ALUNOS CADASTRADOS'
           DISPLAY (11 22) 'ฎ 1 ฏ TELA <******> ฎ 2 ฏ IMPRESSORA'
           DISPLAY (13 30) '### OPCAO [ X ] ###'.

       0200-RESP.
           ACCEPT (13 42) REL-RESP WITH PROMPT AUTO-SKIP

           IF REL-RESP = 1
              DISPLAY (01 01) ERASE
              GO TO 0600-LE-CADASTRO-TELA.
           IF REL-RESP = 2
              OPEN OUTPUT RELFINAL
              GO TO 0300-LE-CADNOTAS-IMPRESSORA.
           DISPLAY (17 33) 'RESPOSTA INVALIDA'
           GO TO 0200-RESP.

       0300-LE-CADNOTAS-IMPRESSORA.
           SORT SORTALU ASCENDING KEY CURSO-SD
                                      ALUNO-SD
                                      COD-ALU-SD
                INPUT  PROCEDURE 0400-SELECIONA-ALU 
                OUTPUT PROCEDURE 0500-EMITE-RELATORIO
                CLOSE CADCURSO
                      CADDISCI
                      CADNOTAS
                      RELFINAL
                CHAIN 'menu.EXE'.

       0400-SELECIONA-ALU SECTION.
       0400-LE-CADNOTAS.
           READ CADNOTAS NEXT
           IF STATUS-CADNT = '10'
              GO TO 0499-FIM.
           IF STATUS-CADNT NOT= '00'
              DISPLAY (17 33) 'PROBLEMA READ ' STATUS-CADNT
              CLOSE CADCURSO
                    CADDISCI
                    CADNOTAS
                    RELFINAL
              STOP ' '
              CHAIN 'menu.EXE'.

           IF ANO-CADNOTA = 2000
              RELEASE REG-SORTALUNO FROM REG-CADNOTAS.

           GO TO 0400-LE-CADNOTAS.

       0499-FIM. EXIT.

       0500-EMITE-RELATORIO SECTION.
       0500-LE-SORT.
           RETURN SORTALU AT END
                   GO TO 0599-FIM.

       0520-MOSTRA-CABS.

           IF FLAG = 0
              MOVE COD-ALU-SD TO ALUNO-ANT
              MOVE 9 TO FLAG
              ADD 1 TO CONTPAG
              MOVE CONTPAG TO PAG-CAB1
              WRITE REG-ORELATO FROM CAB1 AFTER PAGE
              WRITE REG-ORELATO FROM CAB2 AFTER 4
              MOVE CURSO-SD TO COD-CURSO
              GO TO 0540-LE-CUR2.

       0530-MOVE-ALU.

           IF COD-ALU-SD NOT= ALUNO-ANT
                 MOVE COD-ALU-SD TO ALUNO-ANT
                 MOVE CURSO-SD TO COD-CURSO
                 ADD 1 TO CONTPAG
                 MOVE CONTPAG TO PAG-CAB1
                 WRITE REG-ORELATO FROM CAB1 AFTER PAGE
                 WRITE REG-ORELATO FROM CAB2 AFTER 4
                 MOVE CURSO-SD TO COD-CURSO
                 GO TO 0540-LE-CUR2.
           GO TO 0550-MOVE-NOT.

       0540-LE-CUR2.
                 READ CADCURSO INVALID KEY
                   DISPLAY (23 20) 'ERRO NO CURSO.DAT - ABORTADO'
                   DISPLAY (23 17) COD-CURSO-CADNOTA
                   CLOSE CADCURSO
                         CADDISCI
                         CADNOTAS
                         RELFINAL
                   STOP RUN.
                 MOVE CURSO-SD TO COD-CURSO-DET1
                 MOVE NOME-CURSO TO NOME-CURSO-DET1
                 WRITE REG-ORELATO FROM DET1 AFTER 3

                 MOVE COD-ALU-SD TO COD-ALUNO-DET2
                 MOVE ALUNO-SD TO NOME-ALUNO-DET2
                 WRITE REG-ORELATO FROM DET2 AFTER 1
                 WRITE REG-ORELATO FROM CAB3 AFTER 2.

       0550-MOVE-NOT.

                MOVE CURSO-SD TO COD-CURSO-DIS
                MOVE COD-DIS-SD TO COD-DISCIPLINA
                MOVE 2000 TO ANO-DISCI
                READ CADDISCI INVALID KEY
                   DISPLAY (23 20) 'ERRO NO DISCI.DAT - ABORTADO'
                   DISPLAY (23 17) COD-DISCI-CADNOTA
                   CLOSE CADCURSO
                         CADDISCI
                         CADNOTAS
                         RELFINAL
                   STOP RUN.
                MOVE COD-DIS-SD TO COD-DISCI-DET3
                MOVE NOME-DISCIPLINA TO NOME-DISCI-DET3

                ADD NOT1-SD TO NOTA-FINAL
                ADD NOT2-SD TO NOTA-FINAL
                ADD NOT3-SD TO NOTA-FINAL
                ADD NOT4-SD TO NOTA-FINAL

                ADD FAL1-SD TO TOT-FALTAS
                ADD FAL2-SD TO TOT-FALTAS
                ADD FAL3-SD TO TOT-FALTAS
                ADD FAL4-SD TO TOT-FALTAS

                MOVE NOT1-SD TO NOTA1-DET3 
                MOVE NOT2-SD TO NOTA2-DET3 
                MOVE NOT3-SD TO NOTA3-DET3 
                MOVE NOT4-SD TO NOTA4-DET3 
                MOVE NOT5-SD TO NOTA5-DET3 

                DIVIDE 4 INTO NOTA-FINAL
                MOVE NOTA-FINAL TO NOTAF-DET3

                SUBTRACT TOT-FALTAS
                  FROM QTDE-AULAS-DIS GIVING TOT-PRES
                MULTIPLY 100 BY TOT-PRES GIVING TOT-PRES
                DIVIDE QTDE-AULAS-DIS INTO TOT-PRES
                MOVE TOT-PRES TO FREQ-DET3

                MOVE 10 TO NOT1-SD
                SUBTRACT NOTA-FINAL FROM NOT1-SD
                IF NOTA-FINAL > 7 AND FREQ-DET3 > 75
                   MOVE "APROVADO" TO SIT-DET3.

                IF NOTA-FINAL > 7 AND FREQ-DET3 < 75
                   MOVE "REPROVADO-F" TO SIT-DET3.

                IF NOTA-FINAL < 7 AND FREQ-DET3 < 75
                   MOVE "REPROVADO-NF" TO SIT-DET3.

                IF NOTA-FINAL < 7 AND FREQ-DET3 > 75
                   AND NOT5-SD < NOT1-SD
                   MOVE "REPROVADO-N" TO SIT-DET3.

                IF NOTA-FINAL < 7 AND FREQ-DET3 > 75
                   AND NOT5-SD > NOT1-SD OR NOTA-FINAL < 7
                   AND FREQ-DET3 > 75 AND NOT5-SD = NOT1-SD
                   MOVE "APROVADO-F" TO SIT-DET3.

                
                WRITE REG-ORELATO FROM DET3 AFTER 1
                MOVE ZEROS TO NOTA-FINAL
                MOVE ZEROS TO TOT-FALTAS
                MOVE ZEROS TO TOT-PRES
                GO TO 0500-LE-SORT.

       0599-FIM. EXIT.

       0600-FIM.
            CLOSE CADCURSO
                  CADDISCI
                  CADNOTAS
                  RELFINAL
            CHAIN 'menu.EXE'.

       0600-LE-CADASTRO-TELA.
           SORT SORTALU ASCENDING KEY CURSO-SD
                                      ALUNO-SD
                                      COD-ALU-SD
                INPUT  PROCEDURE 0700-SELECIONA-ALU 
                OUTPUT PROCEDURE 0800-EMITE-RELATORIO
                STOP ' '
                CLOSE CADCURSO
                      CADDISCI
                      CADNOTAS
                CHAIN 'menu.EXE'.

       0700-SELECIONA-ALU SECTION.
       0700-LE-CADNOTAS.
           READ CADNOTAS NEXT
           IF STATUS-CADNT = '10'
              GO TO 0799-FIM.

           IF STATUS-CADNT NOT= '00'
              DISPLAY (17 33) 'PROBLEMA READ ' STATUS-CADNT
              CLOSE CADCURSO
                    CADDISCI
                    CADNOTAS
              STOP ' '
              CHAIN 'menu.EXE'.

           IF ANO-CADNOTA = 2000
              RELEASE REG-SORTALUNO FROM REG-CADNOTAS.

           GO TO 0700-LE-CADNOTAS.

       0799-FIM. EXIT.

       0800-EMITE-RELATORIO SECTION.
       0800-LE-SORT.
           RETURN SORTALU AT END
                   GO TO 0899-FIM.

           IF FLAG = 0
              DISPLAY(01 01) ERASE
              MOVE COD-ALU-SD TO ALUNO-ANT
              MOVE 9 TO LIN
              MOVE 9 TO FLAG
              ADD 1 TO CONTPAG
              MOVE CONTPAG TO PAG-CAB1
              DISPLAY (01 01) CAB1
              DISPLAY (03 01) CAB2
              MOVE CURSO-SD TO COD-CURSO
              GO TO 0840-LE-CUR2.

       0830-MOVE-ALU.

           IF COD-ALU-SD NOT= ALUNO-ANT
              DISPLAY (20 24) 'PRESSIONE [ ENTER ] PARA CONTINUAR!'
              STOP ' '
              DISPLAY(01 01) ERASE
              MOVE 9 TO LIN
              MOVE COD-ALU-SD TO ALUNO-ANT
              MOVE CURSO-SD TO COD-CURSO
              ADD 1 TO CONTPAG
              MOVE CONTPAG TO PAG-CAB1
              DISPLAY (01 01) CAB1
              DISPLAY (03 01) CAB2
              GO TO 0840-LE-CUR2.
           GO TO 0850-MOVE-NOT.

       0840-LE-CUR2.
           READ CADCURSO INVALID KEY
                DISPLAY (23 20) 'ERRO NO CURSO.DAT - ABORTADO'
                DISPLAY (23 17) COD-CURSO-CADNOTA
                CLOSE CADCURSO
                      CADDISCI
                      CADNOTAS
                STOP RUN.
           MOVE CURSO-SD TO COD-CURSO-DET1
           MOVE NOME-CURSO TO NOME-CURSO-DET1
           DISPLAY(06  01) DET1 

           MOVE COD-ALU-SD TO COD-ALUNO-DET2
           MOVE ALUNO-SD TO NOME-ALUNO-DET2
           DISPLAY(07 01) DET2
           DISPLAY (09 01) CAB3.

       0850-MOVE-NOT.

           MOVE CURSO-SD TO COD-CURSO-DIS
           MOVE COD-DIS-SD TO COD-DISCIPLINA
           MOVE 2000 TO ANO-DISCI
           READ CADDISCI INVALID KEY
                DISPLAY (23 20) 'ERRO NO DISCI.DAT - ABORTADO'
                DISPLAY (23 17) COD-DISCI-CADNOTA
                CLOSE CADCURSO
                      CADDISCI
                      CADNOTAS
                      RELFINAL
                STOP RUN.
           MOVE COD-DIS-SD TO COD-DISCI-DET3
           MOVE NOME-DISCIPLINA TO NOME-DISCI-DET3

           ADD NOT1-SD TO NOTA-FINAL
           ADD NOT2-SD TO NOTA-FINAL
           ADD NOT3-SD TO NOTA-FINAL
           ADD NOT4-SD TO NOTA-FINAL

           ADD FAL1-SD TO TOT-FALTAS
           ADD FAL2-SD TO TOT-FALTAS
           ADD FAL3-SD TO TOT-FALTAS
           ADD FAL4-SD TO TOT-FALTAS

           MOVE NOT1-SD TO NOTA1-DET3
           MOVE NOT2-SD TO NOTA2-DET3 
           MOVE NOT3-SD TO NOTA3-DET3 
           MOVE NOT4-SD TO NOTA4-DET3 
           MOVE NOT5-SD TO NOTA5-DET3 

           DIVIDE 4 INTO NOTA-FINAL
           MOVE NOTA-FINAL TO NOTAF-DET3

           SUBTRACT TOT-FALTAS
             FROM QTDE-AULAS-DIS GIVING TOT-PRES
           MULTIPLY 100 BY TOT-PRES GIVING TOT-PRES
           DIVIDE QTDE-AULAS-DIS INTO TOT-PRES
           MOVE TOT-PRES TO FREQ-DET3

           MOVE 10 TO NOT1-SD
           SUBTRACT NOTA-FINAL FROM NOT1-SD

           IF NOTA-FINAL > 7 AND FREQ-DET3 > 75
              MOVE "APROVADO" TO SIT-DET3.

           IF NOTA-FINAL > 7 AND FREQ-DET3 < 75
              MOVE "REPROVADO-F" TO SIT-DET3.

           IF NOTA-FINAL < 7 AND FREQ-DET3 < 75
              MOVE "REPROVADO-NF" TO SIT-DET3.

           IF NOTA-FINAL < 7 AND FREQ-DET3 > 75
              AND NOT5-SD < NOT1-SD
                  MOVE "REPROVADO-N" TO SIT-DET3.

           IF NOTA-FINAL < 7 AND FREQ-DET3 > 75
              AND NOT5-SD > NOT1-SD OR NOTA-FINAL < 7
                  AND FREQ-DET3 > 7 AND NOT5-SD = NOT1-SD
                      MOVE "APROVADO-F" TO SIT-DET3.

           ADD 1 TO LIN     
           DISPLAY (LIN , 01)DET3
           MOVE ZEROS TO NOTA-FINAL
           MOVE ZEROS TO TOT-FALTAS
           MOVE ZEROS TO TOT-PRES

           GO TO 0800-LE-SORT.

       0899-FIM. EXIT.

