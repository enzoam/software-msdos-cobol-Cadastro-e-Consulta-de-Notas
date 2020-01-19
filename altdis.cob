       IDENTIFICATION DIVISION.
       PROGRAM-ID ALTDIS-COB.
      *           SISTEMAS DE COMPUTACAO
      *    ANALISTA         :ENZO 19 - JAMILE 26
      *    PROGRAMADOR(A)   :ENZO 19 - JAMILE 26
      *    DATA             :
      *    FINALIDADE       :ALTERACAO DE DISCIPLINAS
      *    VRS              DATA              DESCRICAO
      *    1.5              10/08/2000        IMPLANTACAO

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT discad ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-disc
                         FILE STATUS status-arq.

           SELECT curcad ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-curso
                         FILE STATUS statcur-arq.

       DATA DIVISION.

       FILE SECTION.

       FD discad
           LABEL record STANDARD
           value OF FILE-ID 'disc.dat'
           record contains 62 characteres.

       01 reg-oarq.
           05 chave-disc.
              10 cod-disc          PIC 9(06).
              10 cod-curso         PIC 9(06).
              10 ano-dis           PIC 9(04).
           05 desc-disc            PIC X(40).
           05 qtd-aulas            PIC 9(03). 
           05 carga-h              PIC 9(03).
    
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
           88 ESC                  VALUE '01'.
       01 carga                    PIC 9(03) value 45. 


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
           05 line  02   column 31  value 'ALTERACAO DE DISCIPLINAS'.
           05 line  02   column 72  value 'VRS 1.5'.
           05 line  06   column 07  value 'CODIGO CURSO [000000]'.
           05 line  07   column 50  value 'ANO LETIVO [    ]'.
           05 line  08   column 07  value 'CODIGO DISCIPLINA [000000]'.
           05 line  10   column 07  value 'DESCRICAO ['.
           05 line  10   column 58  value ']'.
           05 line  12   column 07  value 'QTDE AULAS [000]'.
           05 line  14   column 07  value 'CARGA HORARIA [   ]'.


       PROCEDURE DIVISION.

       0100-opencurso.
           OPEN INPUT curcad
           IF statcur-arq = '00'
              GO TO 0160-open.
           DISPLAY (12 30) '[CURSO.DAT INEXISTENTE]'
           STOP RUN.

        0160-open.
           OPEN I-O discad
           IF statcur-arq = '00'
              GO TO 0200-tela.
           DISPLAY (12 30) '[DISC.DAT INEXISTENTE]'
           CLOSE curcad
           CLOSE discad
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

       0220-ano-letivo.
           ACCEPT(07 62) ano-dis with update AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (07 62) '   '
              GO TO 0210-cod-curso. 
           IF ano-dis = zeros
              DISPLAY (22 20) 'ANO INVALIDO INVALIDA'
              GO TO 0220-ano-letivo. 

       0300-cod-disc.
           MOVE 2 TO resp
           DISPLAY(06 21) codcur
           ACCEPT(08 26) cod-disc with update AUTO-SKIP
           IF cod-disc = ZEROS
              DISPLAY (22 20) 'CODIGO DA DISCIPLINA INVALIDO'
              GO TO 0300-cod-disc.
           IF cod-disc = 999999
              GO TO 1300-finaliza.
           READ discad INVALID KEY
              DISPLAY (22 20) linha-branco
              DISPLAY (22 20) 'DISCIPLINA NAO CADASTRADA'
              GO TO 0300-cod-disc.
           DISPLAY (22 20) linha-branco
           GO TO 0400-descricao.

       0400-descricao.
           ACCEPT(10 18) desc-disc with update AUTO-SKIP
           DISPLAY (22 20) linha-branco.
        	ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (10 18) '      '
              GO TO 0300-cod-disc. 
           IF desc-disc = spaces
              DISPLAY (22 20) 'NOME INVALIDO'
              GO TO 0400-descricao.

       0500-qtd-aulas.
           ACCEPT(12 19) qtd-aulas with update AUTO-SKIP
           DISPLAY (22 20) linha-branco
           ACCEPT ws-retorno FROM ESCAPE KEY
           IF ESC
              DISPLAY (12 19) '   '
              GO TO 0400-descricao. 
           IF qtd-aulas = zeros
              DISPLAY (22 20) 'QUANTIDADE INVALIDA'
              GO TO 0500-qtd-aulas. 

       0600-carga-h.
           MULTIPLY carga BY qtd-aulas GIVING carga-h
           DIVIDE 60 INTO carga-h GIVING carga-h
           ROUNDED
           DISPLAY (14 22) carga-h.

       1200-confirma.
           DISPLAY (18 20) 'CONFIRMA OS DADOS ACIMA ? S/N [ ]'.
           ACCEPT (18 51) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           IF ws-resp='S' or 's'
              REWRITE reg-oarq
              IF status-arq = '24' or '34'
                 DISPLAY (22 20) 'ARQUIVO CHEIO - ABORTADO...'
                 CLOSE discad
                 CHAIN 'FACAD.EXE '
              ELSE
                 GO TO 1300-finaliza.
           IF ws-resp='N' or 'n'
              GO TO 1300-finaliza.
           DISPLAY (22 20) 'RESPOSTA INVALIDA'
           GO TO 1200-confirma.

       1300-finaliza.
           DISPLAY (21 20) 'DESEJA CONTINUAR AS ALTERACOES ? S/N [ ]'.

       1400-resp.
           ACCEPT (21 58) ws-resp with prompt AUTO-SKIP
           DISPLAY (22 20) linha-branco
           IF ws-resp = 'S' or 's'
              GO TO 0200-tela.
           IF ws-resp = 'N' or 'n'
              CLOSE discad
              CHAIN 'FACAD.EXE '
           ELSE
              DISPLAY (22 20) 'RESPOSTA INVALIDA'
              GO TO 1400-resp.







