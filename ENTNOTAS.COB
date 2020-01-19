       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENTNOTAS-COB.
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

           SELECT ENTNOTAS ASSIGN TO DISK
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

       FD  ENTNOTAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'NOTAS.DAT'
           RECORD CONTAINS 62 CHARACTERS.

       01  REG-ENTNOTAS.
           05  CHAVE-NOTA.
               10 ANO-NOTA              PIC 9(04).
               10 COD-CURSO-NOTA        PIC 9(02).
               10 COD-ALUNO-NOTA        PIC 9(06).
               10 COD-DISCI-NOTA        PIC 9(04).
           05  BIM-NOTA                 PIC 9.
           05  NOTA                     PIC 99V9.
           05  FALTAS                   PIC 9(02).
           05  NOME-ALUNO-NOTA          PIC X(40).

       WORKING-STORAGE SECTION.
       01  STATUS-CUR          PIC X(02) VALUE SPACES.
       01  STATUS-DIS          PIC X(02) VALUE SPACES.
       01  STATUS-ALU          PIC X(02) VALUE SPACES.
       01  STATUS-NOTA         PIC X(02) VALUE SPACES.
       01  WS-BIM              PIC 9.
       01  WS-NOTA             PIC 99V9.
       01  WS-FALTAS           PIC 9(02).
       01  WS-RESPOSTA         PIC X.
       01  WS-VOLTAR           PIC X.
       01  WS-SAIR             PIC 9.
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
           05  LINE 02   COLUMN 02   VALUE "ENTNOTAS".
           05  LINE 02   COLUMN 34   VALUE "*SISTEMA DE NOTAS*".
           05  LINE 04   COLUMN 34   VALUE "CADASTRO DE NOTAS"
               FOREGROUND-COLOR 15.
           05  LINE 02   COLUMN 73   VALUE "VRS 1.0".
           05  LINE 05   COLUMN 04   VALUE "COD CURSO [  ] [".
           05  LINE 05   COLUMN 79   VALUE "]".
           05  LINE 07   COLUMN 04   VALUE "BIMESTRE [ ]".
           05  LINE 07   COLUMN 30   VALUE "ANO [    ]".
           05  LINE 09   COLUMN 04   VALUE "COD DISCIPLINA [    ] [".
           05  LINE 09   COLUMN 79   VALUE "]".

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
           OPEN I-O ENTNOTAS
           IF STATUS-NOTA = '00'
              GO TO 0200-TELA.
           DISPLAY (12 20)'NOTAS.DAT INEXISTENTE'
           DISPLAY (14 20)'DESEJA GERAR(S OU N)  [ X ]'.

       0150-RESP.
           ACCEPT (14 44) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) '                                           '

           IF WS-RESPOSTA = 'S' OR 's'
              OPEN OUTPUT ENTNOTAS
              CLOSE ENTNOTAS
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
           DISPLAY (22 20) '                                           '
           IF COD-CURSO = 0
              GO TO  1200-FINALIZA.

           READ CADCURSO INVALID KEY
                DISPLAY (22 30) 'CURSO NAO CADASTRADO'
                STOP ' '
                GO TO 0300-ENTRADA-CURSO.

           MOVE COD-CURSO TO COD-CURSO-NOTA
           MOVE COD-CURSO TO COD-CURSO-DIS
           MOVE COD-CURSO TO COD-CURSO-ALU
           DISPLAY (05 22) NOME-CURSO.

       0400-BIMESTRE.
           ACCEPT (07 14) WS-BIM WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0300-ENTRADA-CURSO.
           DISPLAY (22 20) '                                           '
           IF WS-BIM = 0 OR > 5
              DISPLAY (22 32) 'BIMESTRE INVALIDO'
              STOP ' '
              GO TO  0400-BIMESTRE.

       0500-ANO-NOTA.
           ACCEPT (07 35) ANO-DISCI WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0400-BIMESTRE.
           DISPLAY (22 20)
             '                                                     '
           IF ANO-DISCI < 1997 OR > 2004
              DISPLAY (22 31) 'ANO INVALIDO'
              GO TO 0500-ANO-NOTA.

           MOVE ANO-DISCI TO ANO-NOTA.

       0600-ENTRADA-DIS.
           ACCEPT (09 20) COD-DISCIPLINA WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0500-ANO-NOTA.
           DISPLAY (22 20)
             '                                                     '
           IF COD-DISCIPLINA = 0
              DISPLAY (22 33) 'CODIGO INVALIDO'
              STOP ' '
              GO TO  0600-ENTRADA-DIS.

           READ CADDISCI INVALID KEY
                DISPLAY (22 30) 'DISCIPLINA NAO CADASTRADA'
                STOP ' '
                GO TO 0600-ENTRADA-DIS.

           DISPLAY (09 40) NOME-DISCIPLINA
           MOVE COD-DISCIPLINA TO COD-DISCI-NOTA.

       0700-NOTAS.
           MOVE 13 TO LIN
           DISPLAY (13 01) ERASE
           DISPLAY TELA-2.

       0715-CODALU.
           ACCEPT (LIN , 05) COD-ALUNO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY

           IF ESC
              GO TO 0600-ENTRADA-DIS.
           DISPLAY (22 20) '                                           '
           IF COD-ALUNO = 0
              GO TO 1400-VOLTA.

           READ CADALUNO INVALID KEY
                DISPLAY (22 25) 'CODIGO DE ALUNO NAO CADASTRADO'
                GO TO 0715-CODALU.
           DISPLAY (LIN , 16) NOME-ALUNO.
           MOVE NOME-ALUNO TO NOME-ALUNO-NOTA
           MOVE COD-ALUNO TO COD-ALUNO-NOTA

           READ ENTNOTAS INVALID KEY
                GO TO 0730-VER-BIM1.
           GO TO 0745-VER-BIM234.

       0730-VER-BIM1.
           IF WS-BIM NOT= 1
                DISPLAY (22 25) '1งBIM NAO CADASTRADO P/ O ALUNO'
                GO TO 0715-CODALU.
           GO TO 0800-NOTAS-ALU.

       0745-VER-BIM234.

           IF WS-BIM = 1
              DISPLAY (22 26) '1ง BIM JA CADASTRADO'
              GO TO 0715-CODALU.

           IF WS-BIM = 2  AND BIM-NOTA = 4
              DISPLAY (22 26) '2ง BIM JA CADASTRADO '
              STOP ' '
              GO TO 0715-CODALU.
           IF WS-BIM = 2  AND BIM-NOTA = 3
              DISPLAY (22 26) '2ง BIM JA CADASTRADO '
              STOP ' '
              GO TO 0715-CODALU.
           IF WS-BIM = 2  AND BIM-NOTA = 2
              DISPLAY (22 26) '2ง BIM JA CADASTRADO '
              STOP ' '
              GO TO 0715-CODALU.
           IF WS-BIM = 2  AND BIM-NOTA = 1
              GO TO 0800-NOTAS-ALU.

           IF WS-BIM = 3 AND BIM-NOTA = 4
                 DISPLAY (22 26) '3ง BIM JA CADASTRADO '
                 GO TO 0715-CODALU.
           IF WS-BIM = 3 AND BIM-NOTA = 3
                 DISPLAY (22 26) '3ง BIM JA CADASTRADO '
                 GO TO 0715-CODALU.
           IF WS-BIM = 3 AND BIM-NOTA = 1
                 DISPLAY (22 26) '2ง BIM AINDA NAO CADASTRADO '
                 GO TO 0715-CODALU.
           IF WS-BIM = 3 AND BIM-NOTA = 2
                 GO TO 0800-NOTAS-ALU.


           IF WS-BIM = 4 AND BIM-NOTA = 4
                   DISPLAY (22 26) '4ง BIM JA CADASTRADO '
                   GO TO 0715-CODALU.
           IF WS-BIM = 4 AND BIM-NOTA = 2
                   DISPLAY (22 26) '3ง BIM AINDA NAO CADASTRADO '
                   GO TO 0715-CODALU.
           IF WS-BIM = 4 AND BIM-NOTA = 1
                   DISPLAY (22 26) '2ง BIM AINDA NAO CADASTRADO '
                   GO TO 0715-CODALU.
           IF WS-BIM = 4 AND BIM-NOTA = 3
               GO TO 0800-NOTAS-ALU.

           IF WS-BIM = 5 AND BIM-NOTA = 5
                   DISPLAY (22 26) 'NOTA FINAL JA CADASTRADA'
                   GO TO 0715-CODALU.
           IF WS-BIM = 5 AND BIM-NOTA = 3
                   DISPLAY (22 26) '4ง BIM AINDA NAO CADASTRADO '
                   GO TO 0715-CODALU.
           IF WS-BIM = 5 AND BIM-NOTA = 2
                   DISPLAY (22 26) '3ง BIM AINDA NAO CADASTRADO '
                   GO TO 0715-CODALU.
           IF WS-BIM = 5 AND BIM-NOTA = 1
                   DISPLAY (22 26) '2ง BIM AINDA NAO CADASTRADO '
                   GO TO 0715-CODALU.
           IF WS-BIM = 5 AND BIM-NOTA = 4
               GO TO 0800-NOTAS-ALU.

           DISPLAY (22 10) 'ERRRO PASSOU PELOS IFS'
           STOP ' '.

       0800-NOTAS-ALU.
           ACCEPT (LIN , 61) WS-NOTA WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0715-CODALU.
           DISPLAY (22 20) '                                           '
           IF WS-NOTA < 0 OR > 10
              DISPLAY (22 35) 'NOTA INVALIDA'
              STOP ' '
              GO TO 0800-NOTAS-ALU.

       0900-NOTAS-FALTAS.
           IF WS-BIM = 5
              MOVE ZEROS TO WS-FALTAS
              GO TO 1100-RESP-CADNOTA.

           ACCEPT (LIN , 71) WS-FALTAS WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY

           IF ESC
              GO TO 0800-NOTAS-ALU.
           DISPLAY (22 20) '                                           '
           IF WS-FALTAS < 0 OR > 20
              DISPLAY (22 28) 'NUMERO DE FALTAS INVALIDO'
              STOP ' '
              GO TO 0900-NOTAS-FALTAS.

       1100-RESP-CADNOTA.
           DISPLAY (22 14)
              '                                                     '
           DISPLAY (22 14)
              'CONFIRMA CADASTRO DO NOTA P/ O ALUNO(S OU N)  [ X ]'
           ACCEPT (22 62) RESP-NOTA WITH PROMPT AUTO-SKIP
           DISPLAY (22 14)
              '                                                      '

           IF RESP-NOTA = 'N' OR 'n'
              DISPLAY (22 27)'NOTA P/ O ALUNO NAO CADASTRADA'
              STOP ' '
              GO TO 0715-CODALU.

           IF RESP-NOTA = 'S' OR 's'
              READ ENTNOTAS INVALID KEY
                   MOVE WS-BIM TO BIM-NOTA
                   MOVE WS-NOTA TO NOTA
                   MOVE WS-FALTAS TO FALTAS
                   WRITE REG-ENTNOTAS
                   DISPLAY (22 29)'NOTA CADASTRADA P/ O ALUNO'
                   STOP ' '
                   GO TO 1150-LIN-AQUI.
              MOVE WS-BIM TO BIM-NOTA
              MOVE WS-NOTA TO NOTA
              MOVE WS-FALTAS TO FALTAS
              REWRITE REG-ENTNOTAS
              DISPLAY (22 29)'NOTA CADASTRADA P/ O ALUNO'
              STOP ' '
              GO TO 1150-LIN-AQUI.
           DISPLAY (22 30)'RESPOSTA INVALIDA'
           GO TO 1100-RESP-CADNOTA.

       1150-LIN-AQUI.
           ADD 2 TO LIN
           IF LIN > 19
              GO TO 0700-NOTAS.
           GO TO 0715-CODALU.

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
                 CADALUNO
                 CADDISCI
                 ENTNOTAS
           GO TO 1600-ATUALIZA-CADNOTA.

       1400-VOLTA.
           DISPLAY (22 17)
           "[ENTER] P/ CONTINUAR  [V] P/VOLTAR    [   ]".

       1500-RESPOSTA.
           ACCEPT (22 57) WS-VOLTAR WITH PROMPT AUTO-SKIP
           DISPLAY (22 14)
              '                                                      '
           IF WS-VOLTAR = SPACES
              DISPLAY (23 33) '                 '
              GO TO 0715-CODALU.

           IF WS-VOLTAR NOT= "V" AND "v"
              DISPLAY(23 33) 'RESPOSTA INVALIDA'
              GO TO 1400-VOLTA.
           GO TO 0200-TELA.

       1600-ATUALIZA-CADNOTA.
           DISPLAY (22 17)
           "[1] ATUALIZA ARQUIVO DE NOTAS - [2] VOLTA AO MENU".
           DISPLAY (23 17)
           "                OPCAO [ X ]                      ".

           ACCEPT (23 41) WS-SAIR WITH PROMPT AUTO-SKIP
           DISPLAY (23 14)
              '                                                      '
           IF WS-SAIR = SPACES
              GO TO 1600-ATUALIZA-CADNOTA.

           IF WS-SAIR = 1
              CHAIN 'cadnotas.exe'.

           IF WS-SAIR = 2
              CHAIN 'menu.exe'.

           DISPLAY(23 33) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 1600-ATUALIZA-CADNOTA.
