       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADALUNO-COB.
      *     EMPRESA S / A
      * ANALISTA       : FABIO
      * PROGRAMADOR(A) : FABIO
      * FINALIDADE : Efetua CADASTRO de ALUNOS no arq indexado
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

           SELECT CADALUNO ASSIGN TO DISK
                       ORGANIZATION INDEXED
                       ACCESS MODE DYNAMIC
                       RECORD KEY CHAVE-ALU
                       FILE STATUS STATUS-ALU.


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

       WORKING-STORAGE SECTION.
       01  STATUS-CUR          PIC X(02) VALUE SPACES.
       01  STATUS-ALU          PIC X(02) VALUE SPACES.
       01  CEP-WS              PIC 99999/999.
       01  FONE-WS             PIC ZZZZZZZ/ZZZZ.
       01  WS-RESPOSTA         PIC X.
       01  RESP-ALU            PIC X.
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
           05  LINE 02   COLUMN 02   VALUE "CADALUNO".
           05  LINE 02   COLUMN 35   VALUE "SISTEMA DE NOTAS".
           05  LINE 02   COLUMN 73   VALUE "VRS 1.0".
           05  LINE 05   COLUMN 30   VALUE "[ CADASTRO DE ALUNOS ]" 
               FOREGROUND-COLOR 15.
           05  LINE 09   COLUMN 10   VALUE "CODIGO ALUNO [        ]".
           05  LINE 07   COLUMN 10   VALUE "CODIGO CURSO [".
           05  LINE 07   COLUMN 28   VALUE "]  CURSO [ ".
           05  LINE 07   COLUMN 79   VALUE "]".
           05  LINE 11   COLUMN 10   VALUE "NOME DO ALUNO [ ".
           05  LINE 11   COLUMN 79   VALUE "]".
           05  LINE 13   COLUMN 10   VALUE "SEXO [ ".
           05  LINE 13   COLUMN 19   VALUE "]  ENDERECO [".
           05  LINE 13   COLUMN 79   VALUE "]".
           05  LINE 15   COLUMN 10   VALUE "CIDADE [".
           05  LINE 15   COLUMN 37   VALUE "]  CEP [".
           05  LINE 15   COLUMN 56   VALUE "]  FONE [".
           05  LINE 15   COLUMN 79   VALUE "]".


       PROCEDURE DIVISION.

       0050-OPEN-CADCURSO.
           OPEN INPUT CADCURSO
           IF STATUS-CUR = '00'
              GO TO 0100-INICIO.
           DISPLAY (12 20) 'CADCURSO NAO EXISTE'
           STOP RUN.

       0100-INICIO.
           OPEN I-O CADALUNO
           IF STATUS-ALU = '00'
              GO TO 0200-TELA.
           DISPLAY (12 20)'ALUNO.DAT INEXISTENTE'
           DISPLAY (14 20)'DESEJA GERAR(S OU N)  [ X ]'.

       0150-RESP.
           ACCEPT (14 44) WS-RESPOSTA WITH PROMPT AUTO-SKIP
           DISPLAY (17 20) '                                           '

           IF WS-RESPOSTA = 'S' OR 's'
              OPEN OUTPUT CADALUNO
              CLOSE CADALUNO
              GO TO 0100-INICIO.

           IF WS-RESPOSTA = 'N' OR 'n'
              DISPLAY(17 20) 'ALUNO ABORTADO!'
              CLOSE CADCURSO
              CHAIN 'menu.EXE'.

           DISPLAY(17 20) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 0150-RESP.


       0200-TELA.
           DISPLAY TELA-ENTRADA.

       0300-CURSO.
           ACCEPT (07 25) COD-CURSO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0200-TELA.
           DISPLAY (22 20) '                                           '
           IF COD-CURSO = 0
              GO TO  1200-FINALIZA.

           READ CADCURSO INVALID KEY
                DISPLAY (22 30) 'CURSO NAO CADASTRADO'
                STOP ' '
                GO TO 0200-TELA.
           DISPLAY (07 39) NOME-CURSO
           MOVE COD-CURSO TO COD-CURSO-ALU.

       0400-ALUNO.
           ACCEPT (09 25) COD-ALUNO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0300-CURSO.
           DISPLAY (22 20) '                                           '
           IF COD-ALUNO = 0
              DISPLAY (22 33) 'CODIGO INVALIDO'
              STOP ' '
              GO TO  0400-ALUNO.

           READ CADALUNO INVALID KEY
                GO TO 0500-NOME-ALUNO.
           DISPLAY (22 20) 'CODIGO DE ALUNO JA CADASTRADA P/ CURSO'
           GO TO 0400-ALUNO.

       0500-NOME-ALUNO.
           ACCEPT (11 26) NOME-ALUNO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0400-ALUNO.
           DISPLAY (22 20) '                                           '
           IF NOME-ALUNO = SPACES
              DISPLAY (22 31) 'DESCRICAO INVALIDA'
              GO TO 0500-NOME-ALUNO.

       0600-SEXO.
           ACCEPT (13 17) SEXO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0500-NOME-ALUNO.
           DISPLAY (22 20) '                                           '

           IF SEXO = 'F' OR 'f' OR 'M' OR 'm'
              GO TO 0700-ENDERECO.

           DISPLAY (22 33) 'RESPOSTA INVALIDA'
           STOP ' '
           GO TO 0600-SEXO.

       0700-ENDERECO.
           ACCEPT (13 33) ENDERECO WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0600-SEXO.
           DISPLAY (22 20) '                                           '
           IF ENDERECO = SPACES
              DISPLAY (22 31) 'DESCRICAO INVALIDA'
              GO TO 0700-ENDERECO.

       0800-CIDADE.
           ACCEPT (15 20) CIDADE WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0700-ENDERECO.
           DISPLAY (22 20) '                                           '
           IF ENDERECO = SPACES
              DISPLAY (22 31) 'DESCRICAO INVALIDA'
              GO TO 0800-CIDADE.

       0900-CEP.
           ACCEPT (15 46) CEP WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0800-CIDADE.
           DISPLAY (22 20) '                                           '
           IF CEP = 0   
              DISPLAY (22 31) 'DESCRICAO INVALIDA'
              GO TO 0900-CEP.
           MOVE CEP TO CEP-WS
           DISPLAY (15 46) CEP-WS.
                        
       1000-FONE.
           ACCEPT (15 66) FONE WITH PROMPT AUTO-SKIP
           ACCEPT RETORNO FROM ESCAPE KEY
           IF ESC
              GO TO 0900-CEP.
           DISPLAY (22 20) '                                           '
           IF CEP = 0   
              DISPLAY (22 31) 'NUMERO INVALIDO'
              GO TO 1000-FONE.
           MOVE FONE TO FONE-WS
           DISPLAY (15 66) FONE-WS.

       1100-RESP-CADALU.
           DISPLAY (22 20) '                                        '
           DISPLAY (22 20) 'CONFIRMA CADASTRO DO ALUNO(S OU N)  [ X ]'
           ACCEPT (22 58) RESP-ALU WITH PROMPT AUTO-SKIP
           DISPLAY (22 20) '                                         '

           IF RESP-ALU = 'N' OR 'n'
              DISPLAY (22 27)'ALUNO NAO CADASTRADO'
              STOP ' '
              GO TO 0200-TELA.

           IF RESP-ALU = 'S' OR 's'
              READ CADALUNO INVALID KEY
                   WRITE REG-CADALUNO
                   DISPLAY (22 29)'ALUNO CADASTRADO'
                   STOP ' '
                   GO TO 0200-TELA.

           DISPLAY (22 30)'RESPOSTA INVALIDA'
           GO TO 1100-RESP-CADALU.

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
           CHAIN 'menu.EXE'.
