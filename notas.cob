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

           SELECT CADALUNO ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-ALU
                       FILE STATUS STATUS-ALU.

           SELECT CADNOTAS ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-NOTA
                       FILE STATUS STATUS-NOTA.

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
           RECORD CONTAINS 48 CHARACTERS.

       01  REG-CADDISCI.
           05  CHAVE-DIS.
               10  COD-CURSO-DIS       PIC 9(02).
               10  COD-DISCIPLINA      PIC 9(04).
           05  NOME-DISCIPLINA         PIC X(35).
           05  QTDE-AULAS-DIS          PIC 9(03).
           05  CARGA-DIS               PIC 9(04).

       FD  CADALUNO
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'ALUNO.DAT'
           RECORD CONTAINS 119 CHARACTERS.

       01  REG-CADALUNO.
           05  CHAVE-ALU.
               10  COD-ALUNO           PIC 9(06).
               10  COD-CURSO-ALU       PIC 9(02).
           05  NOME-ALUNO              PIC X(35).
           05  SEXO                    PIC X. 
           05  ENDERECO                PIC X(40).
           05  CIDADE                  PIC X(15).
           05  CEP                     PIC 9(08).
           05  FONE                    PIC 9(12).

       FD  CADNOTAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'NOTAS.DAT'
           RECORD CONTAINS 18 CHARACTERS.

       01  REG-CADNOTAS.
           05  CHAVE-NOTA.
               10 COD-DISCI-NOTA        PIC 9(04).
               10 COD-CURSO-NOTA        PIC 9(02).
               10 COD-ALUNO-NOTA        PIC 9(06).
           05  BIM-NOTA                 PIC 9.
           05  NOTA                     PIC 99V9.
           05  FALTAS                   PIC 9(02).

       WORKING-STORAGE SECTION.
       01  STATUS-CUR          PIC X(02) VALUE SPACES.
       01  STATUS-DIS          PIC X(02) VALUE SPACES.
       01  STATUS-ALU          PIC X(02) VALUE SPACES.
       01  STATUS-NOTA         PIC X(02) VALUE SPACES.
       01  WS-BIM              PIC 9.
       01  WS-RESPOSTA         PIC X.
       01  WS-VOLTAR           PIC X.
       01  RESP-NOTA           PIC X.
       01  RETORNO             PIC X(02).
           88 ESC              VALUE '01'.

       SCREEN SECTION.
       01  TELA-1.
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
           05  LINE 10 COLUMN 01   VALUE "ศอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
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
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
           05  LINE 02   COLUMN 02   VALUE "NOTAS".
           05  LINE 02   COLUMN 35   VALUE "SISTEMA DE NOTAS".
           05  LINE 02   COLUMN 73   VALUE "VRS 1.0".
           05  LINE 05   COLUMN 04   VALUE "COD CURSO [  ] [".
           05  LINE 05   COLUMN 79   VALUE "]".
           05  LINE 07   COLUMN 04   VALUE "COD DISCIPLINA [    ] [".
           05  LINE 07   COLUMN 79   VALUE "]".
           05  LINE 09   COLUMN 04   VALUE "BIMESTRE [ ]".
           05  LINE 09   COLUMN 30   VALUE "TURMA [ ]".

       01  TELA-2.
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
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออผ".
           05  LINE 11   COLUMN 04   VALUE "COD ALUNO".
           05  LINE 11   COLUMN 17   VALUE "NOME ALUNO".
           05  LINE 11   COLUMN 61   VALUE "NOTA".
           05  LINE 11   COLUMN 69   VALUE "FALTAS".
           05  LINE 13   COLUMN 04   VALUE "[      ]".
           05  LINE 13   COLUMN 14   VALUE "[".
           05  LINE 13   COLUMN 54   VALUE "]".
           05  LINE 13   COLUMN 60   VALUE "[    ]".
           05  LINE 13   COLUMN 70   VALUE "[  ]".
           05  LINE 15   COLUMN 04   VALUE "[      ]".
           05  LINE 15   COLUMN 14   VALUE "[".
           05  LINE 15   COLUMN 54   VALUE "]".
           05  LINE 15   COLUMN 60   VALUE "[    ]".
           05  LINE 15   COLUMN 70   VALUE "[  ]".
           05  LINE 17   COLUMN 04   VALUE "[      ]".
           05  LINE 17   COLUMN 14   VALUE "[".
           05  LINE 17   COLUMN 54   VALUE "]".
           05  LINE 17   COLUMN 60   VALUE "[    ]".
           05  LINE 17   COLUMN 70   VALUE "[  ]".
           05  LINE 19   COLUMN 04   VALUE "[      ]".
           05  LINE 19   COLUMN 14   VALUE "[".
           05  LINE 19   COLUMN 54   VALUE "]".
           05  LINE 19   COLUMN 60   VALUE "[    ]".
           05  LINE 19   COLUMN 70   VALUE "[  ]".


       PROCEDURE DIVISION.

       0010-OPEN-CADCURSO.
           OPEN INPUT CADCURSO
           IF STATUS-CUR = '00'
              GO TO 0020-OPEN-CADDISCI.
           DISPLAY (12 20) 'CURSO.DAT NAO EXISTE'
           STOP RUN.

       0020-OPEN-CADDISCI.
           OPEN INPUT CADDISCI
           IF STATUS-DIS = '00'
              GO TO 0030-OPEN-CADALUNO.
           DISPLAY (12 20) 'DISCI.DAT NAO EXISTE'
           CLOSE CADCURSO
           STOP RUN.

       0030-OPEN-CADALUNO.
           OPEN INPUT CADALUNO
           IF STATUS-ALU = '00'
              GO TO 0100-INICIO.
           DISPLAY (12 20) 'ALUNO.DAT NAO EXISTE'
           CLOSE CADCURSO
                 CADDISCI
           STOP RUN.

       0100-INICIO.
           OPEN I-O CADNOTAS
           IF STATUS-NOTA = '00'
              GO TO 0200-TELA.
           DISPLAY (12 20)'NOTAS.DAT INEXISTENTE'
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
              CLOSE CADCURSO
                    CADDISCI
                    CADALUNO
              STOP RUN.

           DISPLAY(17 20) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 0150-RESP.


       0200-TELA.
           DISPLAY TELA-1.

       0300-ENTRADA-CURSO.
           ACCEPT (05 15) COD-CURSO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0200-TELA.
           DISPLAY (23 20) '                                           '
           IF COD-CURSO = 0
              GO TO  1200-FINALIZA.

           READ CADCURSO INVALID KEY
                DISPLAY (23 30) 'CURSO NAO CADASTRADO'
                STOP ' '
                GO TO 0300-ENTRADA-CURSO.

           MOVE COD-CURSO TO COD-CURSO-NOTA
           MOVE COD-CURSO TO COD-CURSO-DIS
           MOVE COD-CURSO TO COD-CURSO-ALU
           DISPLAY (05 22) NOME-CURSO
           GO TO 0400-ENTRADA-DIS.


       0400-ENTRADA-DIS.
           ACCEPT (07 20) COD-DISCIPLINA WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0300-ENTRADA-CURSO.
           DISPLAY (23 20) '                                           '
           IF COD-DISCIPLINA = 0
              DISPLAY (23 33) 'CODIGO INVALIDO'
              STOP ' '
              GO TO  0400-ENTRADA-DIS.

           READ CADDISCI INVALID KEY
                DISPLAY (23 30) 'DISCIPLINA NAO CADASTRADA'
                STOP ' '
                GO TO 0400-ENTRADA-DIS.

           DISPLAY (07 40) NOME-DISCIPLINA
           MOVE COD-DISCIPLINA TO COD-DISCI-NOTA
           GO TO 0500-BIMESTRE.

       0500-BIMESTRE.
           ACCEPT (09 14) WS-BIM WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0400-ENTRADA-DIS.
           DISPLAY (23 20) '                                           '
           IF WS-BIM = 0 OR > 4
              DISPLAY (23 32) 'BIMESTRE INVALIDO'
              STOP ' '
              GO TO  0500-BIMESTRE.
           GO TO 0600-NOTAS.

       0600-NOTAS.
           MOVE 13 TO LIN
           DISPLAY (13 01) ERASE
           DISPLAY TELA-2.

       0615-CODALU.
           ACCEPT (LIN , 05) COD-ALUNO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY

           IF ESC
              GO TO 0500-BIMESTRE.
           DISPLAY (23 20) '                                           '
           IF COD-ALUNO = 0
              GO TO 1400-VOLTA.

           READ CADALUNO INVALID KEY
                DISPLAY (23 25) 'CODIGO DE ALUNO NAO CADASTRADO'
                GO TO 0615-CODALU.
           DISPLAY (LIN , 16) NOME-ALUNO.
           MOVE COD-ALUNO TO COD-ALUNO-NOTA

           READ CADNOTAS INVALID KEY
                GO TO 0630-VER-BIM1.
           DISPLAY (23 26) ' '
           GO TO 0645-VER-BIM234.

       0630-VER-BIM1.
           IF WS-BIM NOT= 1
                DISPLAY (23 25) '1งBIM NAO CADASTRADO P/ O ALUNO'
                GO TO 0615-CODALU.
           GO TO 0700-NOTAS-ALU.

       0645-VER-BIM234.
           DISPLAY (23 10) BIM-NOTA
           STOP ' '

           IF WS-BIM = 1
              DISPLAY (23 26) '1ง BIM JA CADASTRADO'
              GO TO 0615-CODALU.

           IF WS-BIM = 2  AND BIM-NOTA = 2
              DISPLAY (23 26) '2ง BIM JA CADASTRADO '
              STOP ' '
              GO TO 0615-CODALU.
           IF WS-BIM = 2  AND BIM-NOTA = 1
              GO TO 0700-NOTAS-ALU.

           IF WS-BIM = 3
              IF BIM-NOTA = 3
                   DISPLAY (23 26) '3ง BIM JA CADASTRADO '
                   GO TO 0615-CODALU.
              IF BIM-NOTA = 1
                   DISPLAY (23 26) '2ง BIM AINDA NAO CADASTRADO '
                   GO TO 0615-CODALU.
              GO TO 0700-NOTAS-ALU.

           IF WS-BIM = 4
              IF BIM-NOTA = 4 
                   DISPLAY (23 26) '4ง BIM JA CADASTRADO '
                   GO TO 0615-CODALU.
              IF BIM-NOTA = 2
                   DISPLAY (23 26) '3ง BIM AINDA NAO CADASTRADO '
                   GO TO 0615-CODALU.
              IF BIM-NOTA = 1
                   DISPLAY (23 26) '2ง BIM AINDA NAO CADASTRADO '
                   GO TO 0615-CODALU.
              GO TO 0700-NOTAS-ALU.
           DISPLAY (23 10) 'ERRRO PASSOU PELOS IFS'
           STOP ' '.

       0700-NOTAS-ALU.
           ACCEPT (LIN , 61) NOTA WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY

           IF ESC
              GO TO 0615-CODALU.
           DISPLAY (23 20) '                                           '
           IF NOTA < 0 OR > 10
              DISPLAY (23 35) 'NOTA INVALIDA'
              STOP ' '
              GO TO 0700-NOTAS-ALU.
           GO TO 0800-NOTAS-FALTAS.

       0800-NOTAS-FALTAS.
           ACCEPT (LIN , 71) FALTAS WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY

           IF ESC
              GO TO 0700-NOTAS-ALU.
           DISPLAY (23 20) '                                           '
           IF FALTAS < 0 OR > 20
              DISPLAY (23 28) 'NUMERO DE FALTAS INVALIDO'
              STOP ' '
              GO TO 0800-NOTAS-FALTAS.
           GO TO 1100-RESP-CADNOTA.

       1100-RESP-CADNOTA.
           DISPLAY (23 14)
              '                                                     '
           DISPLAY (23 14)
              'CONFIRMA CADASTRO DO NOTA P/ O ALUNO(S OU N)  [ X ]'
           ACCEPT (23 62) RESP-NOTA WITH PROMPT AUTO-SKIP
           DISPLAY (23 14)
              '                                                      '

           IF RESP-NOTA = 'N' OR 'n'
              DISPLAY (23 27)'NOTA P/ O ALUNO NAO CADASTRADA'
              STOP ' '
              GO TO 0615-CODALU.

           IF RESP-NOTA = 'S' OR 's'
              READ CADNOTAS INVALID KEY
                   MOVE WS-BIM TO BIM-NOTA
                   WRITE REG-CADNOTAS
                   DISPLAY (23 29)'NOTA CADASTRADA P/ O ALUNO'
                   STOP ' '
                   GO TO 1150-LIN-AQUI.
              MOVE WS-BIM TO BIM-NOTA
              REWRITE REG-CADNOTAS
              DISPLAY (23 29)'NOTA CADASTRADA P/ O ALUNO'
              STOP ' '
              GO TO 1150-LIN-AQUI.
           DISPLAY (23 30)'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 1100-RESP-CADNOTA.

       1150-LIN-AQUI.
           ADD 2 TO LIN
           IF LIN > 19
              GO TO 0600-NOTAS.
           GO TO 0615-CODALU.

       1200-FINALIZA.
           DISPLAY (23 17)
           "[ENTER] P/ CONTINUAR  [F] P/FINALIZAR    [   ]".

       1300-RESPOSTA.
           ACCEPT (23 60) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (23 17)
            '                                              '
           IF WS-RESPOSTA = SPACES
              GO TO 0200-TELA.

           IF WS-RESPOSTA NOT= "F" AND "f"
              DISPLAY(23 33) 'RESPOSTA INVALIDA'
              STOP ' '
              GO TO 1200-FINALIZA.
           CLOSE CADCURSO
                 CADALUNO
                 CADDISCI
                 CADNOTAS
           CHAIN 'telai01.EXE'.

       1400-VOLTA.
           DISPLAY (23 17)
           "[ENTER] P/ CONTINUAR  [V] P/VOLTAR    [   ]".

       1500-RESPOSTA.
           ACCEPT (23 57) WS-VOLTAR WITH PROMPT AUTO-SKIP
           DISPLAY (23 14)
              '                                                      '
           IF WS-VOLTAR = SPACES
              GO TO 0615-CODALU.

           IF WS-VOLTAR NOT= "V" AND "v"
              DISPLAY(23 33) 'RESPOSTA INVALIDA'
              STOP ' '
              GO TO 1400-VOLTA.
           GO TO 0200-TELA.
