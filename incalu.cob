       IDENTIFICATION DIVISION.
       PROGRAM-ID INCALU-COB.
      *           SISTEMAS DE COMPUTACAO
      *    ANALISTA         :ENZO 19 - JAMILE 26
      *    PROGRAMADOR(A)   :ENZO 19 - JAMILE 26
      *    DATA             :
      *    FINALIDADE       :INCLUSAO DE ALUNOS
      *    VRS              DATA              DESCRICAO
      *    1.5              20/08/2000        IMPLANTACAO

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT alucad ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-aluno
                         FILE STATUS status-arq.

           SELECT curcad ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-curso
                         FILE STATUS statcur-arq.

       DATA DIVISION.

       FILE SECTION.

       FD alucad
           LABEL record STANDARD
           value OF FILE-ID 'alunos.dat'
           record contains 135 characteres.

       01 reg-oarq.
           05 chave-aluno.
              10 cod-aluno         PIC 9(06).
              10 cod-curso         PIC 9(06).
           05 nome                 PIC X(40).
           05 endereco             PIC X(30).
           05 bairro               PIC X(20).
           05 cidade               PIC X(15).
           05 estado               PIC X(02).
           05 data-nasc.
              10 ano-nasc          PIC 9(04).
              10 mes-nasc          PIC 9(02).
              10 dia-nasc          PIC 9(02).
           05 data-nasc-r REDEFINES data-nasc PIC 9(08).
           05 rg.
              10 rg-num            PIC 9(07).
              10 rg-dig            PIC 9(01).
           05 rg-r REDEFINES rg    PIC 9(08).

       FD curcad
           LABEL record STANDARD
           value OF FILE-ID 'curso.dat'
           record contains 52 characteres.

       01 regoarq.               
           05 chave-curso.
              10 codcurso          PIC 9(06).
           05 descricao            PIC X(40).
           05 mensalidade          PIC 9(04)v99.                  

       WORKING-STORAGE SECTION.
       01 ws-resp                  PIC x.
       01 linha-branco             PIC X(40) value spaces.
       01 status-arq               PIC X(02) value spaces.
       01 statcur-arq              PIC X(02) value spaces.
       01 codcur                   PIC 9(06) value 0.
       01 resp                     PIC 9(01) value 1.
       01 ws-retorno               PIC X(02). 
          88 ESC                   VALUE '01'.

       SCREEN SECTION.
       01  tela-cadastra.
           05 blank screen.
           05  LINE 01 COLUMN 01   VALUE "ษอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออป".
           05  LINE 02 COLUMN 01 VALUE "บ".
           05  LINE 02 COLUMN 80 VALUE "บ".
           05  LINE 03 COLUMN 01 VALUE "บ".
           05  LINE 03 COLUMN 80 VALUE "บ".
           05  LINE 04 COLUMN 01   VALUE "ฬอออออออออออออออออออออออออออออ
      -            "อออออออออออออออออออออออออออออออออออออออออออออออออน".
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
           05 line  02   column 02  value 'MENU'.
           05 line  02   column 31  value 'CADASTRO DE ALUNOS'.
           05 line  02   column 72  value 'VRS 1.5'.
           05 line  06   column 07  value 'CODIGO CURSO [000000]'.
           05 line  08   column 07  value 'CODIGO ALUNO [000000]'.
           05 line  10   column 07  value 'NOME ['.
           05 line  10   column 53  value ']'.
           05 line  12   column 07  value 'DATA NASCIMENTO'.
           05 line  12   column 23  value '[00/00/0000]'.
           05 line  12   column 37  value 'RG [        ]'.
           05 line  14   column 07  value 'ENDERECO ['.
           05 line  14   column 47  value ']'.
           05 line  16   column 07  value 'CIDADE ['.
           05 line  16   column 30  value ']'.   
           05 line  16   column 32  value 'ESTADO [  ]'.

       PROCEDURE DIVISION.
       0100-open.
           OPEN I-O alucad
           IF status-arq = '00'
              GO TO 0160-opencurso.
           DISPLAY (12 30) '[ALUNOS.DAT INEXISTENTE]'
           DISPLAY (13 30) 'DESEJA GERAR (S ou N)  [ ]'.

       0150-resp.
           ACCEPT (13 54) ws-resp
           DISPLAY (22 20) linha-branco.
           DISPLAY (12 20) linha-branco.
           DISPLAY (13 20) linha-branco.
           IF ws-resp='S' or 's'
              OPEN OUTPUT alucad
              CLOSE alucad
              GO TO 0100-open.
           IF ws-resp='N' or 'n'
              DISPLAY (22 20) 'INCLUSAO DE ALUNOS ABORTADO...'
              STOP RUN.
           DISPLAY (22 20) 'RESPOSTA INVALIDA...'
           GO TO 0150-resp.
              
       0160-opencurso.
           OPEN INPUT curcad
           IF statcur-arq = '00'
              GO TO 0200-tela.
           DISPLAY (12 30) '[CURSO.DAT INEXISTENTE]'
           CLOSE alucad
           CLOSE curcad
           STOP RUN.

       0200-tela.
           DISPLAY tela-cadastra.

       0210-cod-curso.
           IF RESP = 1
              ACCEPT(06 21) cod-curso with prompt AUTO-SKIP
              MOVE cod-curso TO codcurso
              READ curcad INVALID KEY
                 DISPLAY (22 20) 'REGISTRO NAO EXISTE'
                 GO TO 0210-cod-curso.
              DISPLAY(06 30) descricao
              MOVE cod-curso TO codcur
              DISPLAY (22 20) linha-branco
              IF cod-curso = ZEROS
                 GO TO 1300-finaliza.

       0300-cod-aluno.
           MOVE 2 TO resp
           DISPLAY(06 21) codcur
           ACCEPT(08 21) cod-aluno with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (08 21) '      '
              GO TO 0210-cod-curso.  
           IF cod-aluno = ZEROS
              DISPLAY (22 20) 'CODIGO DO ALUNO INVALIDO'
              GO TO 0300-cod-aluno.
           IF cod-aluno = 999999
              GO TO 1300-finaliza.
           READ alucad INVALID KEY
              DISPLAY (22 20) linha-branco
              GO TO 0400-nome.
           DISPLAY (22 20) linha-branco
           DISPLAY (22 20) 'ALUNO JA CADASTRADO'
           GO TO 0300-cod-aluno.

       0400-nome.
           ACCEPT(10 13) nome with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (10 13) '                                   '
              GO TO  0300-cod-aluno.    

           IF nome = spaces
              DISPLAY (22 20) 'NOME INVALIDO'
              GO TO 0400-nome.

       0500-dia-nasc.
           ACCEPT(12 24) dia-nasc with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (12 24) '  '
              GO TO  0400-nome.  

           IF dia-nasc < 1  or > 31
              DISPLAY (22 20) 'DIA INVALIDO...'
              GO TO 0500-dia-nasc.

       0600-mes-nasc.
           ACCEPT(12 27) mes-nasc  with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (12 27) '  '
              GO TO 0500-dia-nasc.
 
           IF mes-nasc < 1  or > 12
              DISPLAY (22 20) 'MES INVALIDO...'
              GO TO 0600-mes-nasc.

       0700-ano-nasc.
           ACCEPT(12 30) ano-nasc with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (12 30) '    '
              GO TO  0600-mes-nasc.    

           IF mes-nasc=1 or 3 or 5 or 7 or 8 or 10 or 12
              GO TO 0800-rg.
           IF mes-nasc=4 or 6 or 9 or 11
              IF dia-nasc > 30
                 DISPLAY (22 20) 'DIA INVALIDO...'
                 GO TO 0500-dia-nasc
              ELSE
                 GO TO 0800-rg.
           IF dia-nasc > 29
              DISPLAY (22 20) 'DIA INVALIDO...'
              GO TO 0500-dia-nasc.

       0800-rg.
           ACCEPT(12 41) rg with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (12 41) '        '
              GO TO 0700-ano-nasc.  

           IF rg = spaces
              DISPLAY (22 20) 'R.G. INVALIDO...'
              GO TO 0800-rg.

       0900-endereco.
           ACCEPT(14 17) endereco with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (14 17) '                            '
              GO TO 0800-rg.   
 
           IF endereco = spaces
              DISPLAY (22 20) 'ENDERECO INVALIDO...'
              GO TO 0900-endereco.

       1000-cidade.
           ACCEPT(16 15) cidade with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (16 15) '               '
              GO TO 0900-endereco.   

           IF cidade = spaces
              DISPLAY (22 20) 'CIDADE INVALIDA...'
              GO TO 1000-cidade.

       1100-estado.
           ACCEPT(16 40) estado with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (16 40) '  '
              GO TO 1000-cidade.   
           IF estado = spaces
              DISPLAY (22 20) 'ESTADO INVALIDO...'
              GO TO 1100-estado.

       1200-confirma.
           DISPLAY (18 20) 'CONFIRMA OS DADOS ACIMA ? S/N [ ]'.
           ACCEPT (18 51) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           IF ws-resp='S' or 's'
              WRITE reg-oarq
              IF status-arq = '24' or '34'
                 DISPLAY (22 20) 'ARQUIVO CHEIO - ABORTADO...'
                 CLOSE alucad
                 CHAIN 'FACAD.EXE '
              ELSE
                 GO TO 1300-finaliza.
           IF ws-resp='N' or 'n'
              GO TO 1300-finaliza.
           DISPLAY (22 20) 'RESPOSTA INVALIDA'
           GO TO 1200-confirma.

       1300-finaliza.
           DISPLAY (21 20) 'DESEJA CONTINUAR A INCLUSAO ? S/N [ ]'.

       1400-resp.
           ACCEPT (21 55) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           IF ws-resp = 'S' or 's'
              GO TO 0200-tela.
           IF ws-resp = 'N' or 'n'
              CLOSE alucad
              CHAIN 'FACAD.EXE '
           ELSE
              DISPLAY (22 20) 'RESPOSTA INVALIDA'
              GO TO 1400-resp.







