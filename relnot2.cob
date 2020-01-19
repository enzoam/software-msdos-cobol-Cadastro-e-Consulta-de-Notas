       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELNOT-COB.
      * ANALISTA       : ENZO 19 - JAMILE 26
      * PROGRAMADOR(A) : ENZO 19 - JAMILE 26
      *    FINALIDADE: EMISSAO DE RELATORIO DE NOTAS
      *                SEM UTILIZACAO DO SORT
      * DATA :
      * VRS         DATA           DESCRICAO
      * 1.5         23/11/2000     IMPLATACAO

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CADDIS ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-dis
                         FILE STATUS statdis-arq.

           SELECT CADCUR ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-cur
                         FILE STATUS statcur-arq.

           SELECT CADNOT ASSIGN TO DISK
                         ORGANIZATION INDEXED
                         ACCESS MODE DYNAMIC
                         RECORD KEY chave-cad
                         FILE STATUS scadnot-arq.

           SELECT SORTCAD ASSIGN TO DISK.
           SELECT RELAT ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  CADDIS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'disc.dat'
           RECORD CONTAINS 62 CHARACTERS.

       01  REG-CADDIS.
           05  chave-dis.
               10  cod-dis             PIC 9(06).
               10  dis-cod-cur         PIC 9(06).
               10  ano-dis             PIC 9(04).
           05  desc-dis                PIC X(40).
           05  qtd-aulas-dis           PIC 9(03).
           05  carga-h                 PIC 9(03).

       FD  CADCUR
           LABEL RECORD STANDARD
           VALUE OF FILE-ID 'curso.dat'
           RECORD CONTAINS 52 CHARACTERS.

       01  REG-CADCUR.
           05 chave-cur.
              10 cod-curso         PIC 9(06).
           05 desc-cur             PIC X(40).
           05 mensalidade          PIC 9(04)v99.

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

       FD RELAT
           LABEL RECORD OMITTED.
       01  REG-RELAT                     PIC X(88).

       WORKING-STORAGE SECTION.
       01  statcur-arq             PIC X(02) VALUE SPACE.
       01  statdis-arq             PIC X(02) VALUE SPACE.
       01  scadnot-arq             PIC X(02) VALUE SPACE.
       01  ws-cont-lin             PIC 9(02) VALUE ZEROS.
       01  ws-cont-pg              PIC 9(05) VALUE ZEROS.
       01  ws-resp                 PIC 9.
       01  ws-freq                 PIC 9(03).
       01  ws-ftotal               PIC 9(03) VALUE ZEROS.
       01  ws-ptotal               PIC 9(04) VALUE ZEROS.
       01  ws-status               PIC X(12).
       01  ws-nota-fin             PIC 9(02)V9 VALUE ZEROS.
       01  ws-alu-ant              PIC 9(06) VALUE ZEROS.
       01  ws-flag                 PIC 9 VALUE ZEROS.
       01  ws-dis-cod              PIC 9(06) VALUE ZEROS.
       01  ws-alu-cod              PIC 9(06) VALUE ZEROS.
       01  ws-cont-alu             PIC 9(06) VALUE ZEROS.

       01  CABE1.
           05 filler               PIC X(33) VALUE 'CURSO'.
           05 filler               PIC X(37) VALUE 'SISTEMA - FACAD'.
           05 filler               PIC X(04) VALUE 'PG. '.
           05 pag-cabe1            PIC ZZ.ZZ9.

       01  CABE2.
           05 filler               PIC X(24) VALUE SPACES.
           05 filler               PIC X(65) VALUE 'NOTA E FREQUENCIA'.

       01  CABE3.
           05 filler               PIC X(02) VALUE SPACES.
           05 filler               PIC X(36) VALUE
           'COD      DISCIPLINA     NOTA1 NOTA2'.
           05 filler               PIC X(40) VALUE
           'NOTA3 NOTA4 NOTF FINAL FREQ  SITUACAO'.

       01  DETAIL1.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail-cur-cod       PIC 9(06).
           05 filler               PIC X(06) VALUE SPACES.
           05 detail-cur-desc      PIC X(40).
           05 filler               PIC X(30) VALUE SPACES.

       01  DETAIL2.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail-alu-cod       PIC 9(06).
           05 filler               PIC X(02) VALUE SPACES.
           05 detail-alu-nome      PIC X(40).
           05 filler               PIC X(35) VALUE SPACES.

       01  DETAIL3.
           05 detail-dis-cod       PIC 9(06).
           05 filler               PIC X(05) VALUE SPACES.
           05 detail-dis-desc      PIC X(15).
           05 filler               PIC X(01) VALUE SPACES.
           05 detail3-nota1        PIC 99,9.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail3-nota2        PIC 99,9.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail3-nota3        PIC 99,9.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail3-nota4        PIC 99,9.
           05 filler               PIC X(01) VALUE SPACES.
           05 detail3-nota5        PIC 99,9.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail3-notaF        PIC 99,9.
           05 filler               PIC X(02) VALUE SPACES.
           05 detail3-freq         PIC 99,9.
           05 filler               PIC X(01) VALUE "%".
           05 filler               PIC X(01) VALUE SPACES.
           05 detail3-status       PIC X(12) VALUE SPACES.

       SCREEN SECTION.
       01  TELA-MAIN.
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
           05  LINE 19 COLUMN 11   VALUE " อออออออออออออออออออออออออออออ
      -            "ออออออออออออออออออออออออออออ ".
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

       0010-abrir-curso.
           OPEN INPUT CADCUR
           IF statcur-arq = '30'
              DISPLAY (18 30) 'CURSO.DAT INEXISTENTE'
              STOP ' '
              CHAIN 'mnotas.exe'.

       0100-abrir-dis.
           OPEN INPUT CADDIS
           IF statdis-arq = '30'
              DISPLAY (18 30) 'DISC.DAT INEXISTENTE'
              CLOSE CADCUR
              STOP ' '
              CHAIN 'mnotas.exe'.

       0200-abrir-cadnot.
           OPEN INPUT CADNOT
           IF scadnot-arq = '30'
              DISPLAY (18 30) 'NOTAFIM.DAT INEXISTENTE'
              CLOSE CADCUR
              CLOSE CADDIS
              STOP ' '
              CHAIN 'mnotas.exe'.

       0300-chama-tela.
           DISPLAY TELA-MAIN.
           DISPLAY (05 32) 'ALUNOS CADASTRADOS'
           DISPLAY (10 20) '[1] - T E L A'
           DISPLAY (11 20) '[2] - I M P R E S S O R A'
           DISPLAY (12 20) '[3] - S A I R'

           DISPLAY (14 20) 'OPCAO  [.]'.

       0200-resp.
           ACCEPT (14 28) ws-resp WITH PROMPT AUTO-SKIP
           IF ws-resp = 1
              GO TO 0700-print-tela.
           IF ws-resp = 2
              OPEN OUTPUT RELAT
              GO TO 0300-print-impressora.
           IF ws-resp = 3
              CLOSE CADCUR
              CLOSE CADDIS
              CLOSE CADNOT
              CLOSE RELAT
              CHAIN 'mnotas.exe'.
           DISPLAY (17 33) 'RESPOSTA INVALIDA'
           GO TO 0200-resp.
            
       0300-print-impressora.
            DISPLAY (01 01) ERASE
            DISPLAY (12 20) 'DIGITE O ANO PARA OBTER O RELATORIO'
            DISPLAY (13 20) 'ANO [....]'
            ACCEPT (13 25) ano-notaf
            DISPLAY (15 20) 'DIGITE O CODIGO DO CURSO'
            DISPLAY (16 20) 'CURSO [......]'
            ACCEPT (16 27) notaf-cod-cur
            MOVE notaf-cod-cur TO cod-curso
            READ CADCUR INVALID KEY
                 DISPLAY (24 25) 'CURSO NAO CADASTRADO'
                 GO TO 0300-print-impressora.
            DISPLAY (20 20) 'PROCESSANDO......'.

       0310-seleciona-alunos.
            MOVE ws-alu-cod TO notaf-cod-alu
            DISPLAY (21 44) ws-alu-cod
            IF ws-alu-cod = 0
               MOVE 1 TO ws-alu-cod
               GO TO 0310-seleciona-alunos.
            IF ws-alu-cod > 9
               GO TO 0600-finaliza.

        0320-seleciona-disciplinas.
            MOVE ws-dis-cod TO notaf-cod-dis
            DISPLAY (20 44) ws-dis-cod
            IF ws-dis-cod = 0
               MOVE 1 TO ws-dis-cod
               GO TO 0320-seleciona-disciplinas.
            IF ws-dis-cod > 9
               ADD 1 TO ws-alu-cod
               MOVE ZEROS TO ws-dis-cod
               GO TO 0310-seleciona-alunos.

        0330-testa-cadnot.
            READ CADNOT INVALID KEY
               GO TO 0340-repara-cadnot.
            IF scadnot-arq = '10'
               GO TO 0600-finaliza.
            GO TO 0500-relat.

        0340-repara-cadnot.
            MOVE notaf-cod-cur TO dis-cod-cur
            MOVE notaf-cod-dis TO cod-dis
            MOVE ano-notaf TO ano-dis
            READ CADDIS INVALID KEY
               ADD 1 TO ws-dis-cod
               GO TO 0320-seleciona-disciplinas.
            ADD 1 TO ws-dis-cod
            GO TO 0320-seleciona-disciplinas.

       0500-relat.
           IF ws-flag = 0
              MOVE ws-alu-cod TO ws-alu-ant
              MOVE 9 TO ws-flag
              ADD 1 TO ws-cont-pg
              MOVE ws-cont-pg TO pag-cabe1
              WRITE REG-RELAT FROM CABE1 AFTER PAGE
              WRITE REG-RELAT FROM CABE2 AFTER 4
              GO TO 0540-seleciona-curso.

       0520-move-aluno.
           IF notaf-cod-alu NOT = ws-alu-ant
                 MOVE ws-alu-cod TO ws-alu-ant
                 MOVE notaf-cod-cur TO detail-cur-cod
                 ADD 1 TO ws-cont-pg
                 MOVE ws-cont-pg TO pag-cabe1
                 WRITE REG-RELAT FROM CABE1 AFTER PAGE
                 WRITE REG-RELAT FROM CABE2 AFTER 4
                 MOVE notaf-cod-cur TO detail-cur-cod
                 GO TO 0540-seleciona-curso
           GO TO 0550-move-notas.

       0540-seleciona-curso.
           MOVE notaf-cod-cur TO detail-cur-cod
           MOVE desc-cur TO detail-cur-desc
           WRITE REG-RELAT FROM DETAIL1 AFTER 3
           MOVE ws-alu-cod TO detail-alu-cod
           MOVE notaf-nome-alu TO detail-alu-nome.
           WRITE REG-RELAT FROM DETAIL2 AFTER 1
           WRITE REG-RELAT FROM CABE3 AFTER 2.

       0550-move-notas.    
           MOVE notaf-cod-cur TO dis-cod-cur
           MOVE notaf-cod-dis TO cod-dis
           MOVE ano-notaf TO ano-dis
           READ CADDIS INVALID KEY
                GO TO 0340-repara-cadnot.
           MOVE ws-dis-cod TO detail-dis-cod
           MOVE desc-dis TO detail-dis-desc
           ADD notaf(1) TO ws-nota-fin
           ADD notaf(2) TO ws-nota-fin
           ADD notaf(3) TO ws-nota-fin
           ADD notaf(4) TO ws-nota-fin
           ADD faltaf(1) TO ws-ftotal
           ADD faltaf(2) TO ws-ftotal
           ADD faltaf(3) TO ws-ftotal
           ADD faltaf(4) TO ws-ftotal
           MOVE notaf(1) TO detail3-nota1
           MOVE notaf(2) TO detail3-nota2
           MOVE notaf(3) TO detail3-nota3
           MOVE notaf(4) TO detail3-nota4
           MOVE notaf(5) TO detail3-nota5
           DIVIDE 4 INTO ws-nota-fin
           MOVE ws-nota-fin TO detail3-notaF
                SUBTRACT ws-ftotal
                FROM qtd-aulas-dis GIVING ws-ptotal
                MULTIPLY 100 BY ws-ptotal GIVING ws-ptotal
                DIVIDE qtd-aulas-dis INTO ws-ptotal
                MOVE ws-ptotal TO detail3-freq
                MOVE 10 TO notaf(1)
                SUBTRACT ws-nota-fin FROM notaf(1)
                IF ws-nota-fin > 7 AND detail3-freq > 75
                  MOVE "APROVADO" TO detail3-status.
                IF ws-nota-fin > 7 AND detail3-freq < 75
                  MOVE "REPROVADO" TO detail3-status.
                IF ws-nota-fin < 7 AND detail3-freq < 75
                  MOVE "REPROVADO" TO detail3-status.
                IF ws-nota-fin < 7 AND detail3-freq > 75
                  AND notaf(5) < notaf(1)
                  MOVE "REPROVADO" TO detail3-status.
                IF ws-nota-fin < 7 AND detail3-freq > 75
                  AND notaf(5) > notaf(1) OR ws-nota-fin < 7
                  AND detail3-freq > 75 AND notaf(5) = notaf(1)
                  MOVE "APROVADO" TO detail3-status.
                WRITE REG-RELAT FROM DETAIL3 AFTER 1
                MOVE ZEROS TO ws-nota-fin
                MOVE ZEROS TO ws-ftotal
                MOVE ZEROS TO ws-ptotal
                ADD 1 TO ws-dis-cod 
                GO TO 0310-seleciona-alunos.

       0600-finaliza.
           CLOSE CADCUR
           CLOSE CADDIS
           CLOSE CADNOT
           CLOSE RELAT
           STOP ' '
           CHAIN 'mnotas.exe'.

       0700-print-tela.
            DISPLAY (01 01) ERASE
            DISPLAY (12 20) 'DIGITE O ANO PARA OBTER O RELATORIO'
            DISPLAY (13 20) 'ANO [....]'
            ACCEPT (13 25) ano-notaf
            DISPLAY (15 20) 'DIGITE O CODIGO DO CURSO'
            DISPLAY (16 20) 'CURSO [......]'
            ACCEPT (16 27) notaf-cod-cur
            MOVE notaf-cod-cur TO cod-curso
            READ CADCUR INVALID KEY
                 DISPLAY (24 25) 'CURSO NAO CADASTRADO'
                 GO TO 0700-print-tela.
            DISPLAY (20 20) 'PROCESSANDO......'.

       0710-seleciona-alunos.
            MOVE ws-alu-cod TO notaf-cod-alu
            DISPLAY (21 44) ws-alu-cod
            IF ws-alu-cod = 0
               MOVE 1 TO ws-alu-cod
               GO TO 0710-seleciona-alunos.
            IF ws-alu-cod > 9
               GO TO 0600-finaliza.

        0720-seleciona-disciplinas.
            MOVE ws-dis-cod TO notaf-cod-dis
            DISPLAY (20 44) ws-dis-cod
            IF ws-dis-cod = 0
               MOVE 1 TO ws-dis-cod
               GO TO 0720-seleciona-disciplinas.
            IF ws-dis-cod > 9
               ADD 1 TO ws-alu-cod
               MOVE ZEROS TO ws-dis-cod
               GO TO 0710-seleciona-alunos.

        0730-testa-cadnot.
            READ CADNOT INVALID KEY
               GO TO 0740-repara-cadnot.
            IF scadnot-arq = '10'
               GO TO 0890-finaliza.
            GO TO 0800-relat.

        0740-repara-cadnot.
            MOVE notaf-cod-cur TO dis-cod-cur
            MOVE notaf-cod-dis TO cod-dis
            MOVE ano-notaf TO ano-dis
            READ CADDIS INVALID KEY
               ADD 1 TO ws-dis-cod
               GO TO 0720-seleciona-disciplinas.
            ADD 1 TO ws-dis-cod
            GO TO 0720-seleciona-disciplinas.

       0800-relat.
           IF ws-flag = 0
              DISPLAY(01 01) ERASE
              MOVE ws-alu-cod TO ws-alu-ant
              MOVE 9 TO LIN
              MOVE 9 TO ws-flag
              ADD 1 TO ws-cont-pg
              MOVE ws-cont-pg TO pag-cabe1
              DISPLAY (01 01) CABE1
              DISPLAY (03 01) CABE2
              MOVE notaf-cod-cur TO cod-curso
              GO TO 0820-seleciona-curso.

       0810-move-alu.
           IF ws-alu-cod NOT= ws-alu-ant
              MOVE 9 TO LIN
              MOVE ws-alu-cod TO ws-alu-ant
              MOVE notaf-cod-cur TO cod-curso
              ADD 1 TO ws-cont-pg
              MOVE ws-cont-pg TO pag-cabe1
              DISPLAY (01 01) CABE1
              DISPLAY (03 01) CABE2
              GO TO 0820-seleciona-curso.
           GO TO 0830-move-notas.

       0820-seleciona-curso.
           MOVE notaf-cod-cur TO detail-cur-cod
           MOVE desc-cur TO detail-cur-desc
           DISPLAY(06 01) DETAIL1 
           MOVE ws-alu-cod TO detail-alu-cod
           MOVE notaf-nome-alu TO detail-alu-nome
           DISPLAY (07 01) DETAIL2
           DISPLAY (09 01) CABE3.

       0830-move-notas.
           MOVE notaf-cod-cur TO dis-cod-cur
           MOVE notaf-cod-dis TO cod-dis
           MOVE ano-notaf TO ano-dis
           READ CADDIS INVALID KEY
                GO TO 0740-repara-cadnot.
           MOVE ws-dis-cod TO detail-dis-cod
           MOVE desc-dis TO detail-dis-desc
           ADD notaf(1) TO ws-nota-fin
           ADD notaf(2) TO ws-nota-fin
           ADD notaf(3) TO ws-nota-fin
           ADD notaf(4) TO ws-nota-fin
           ADD faltaf(1) TO ws-ftotal
           ADD faltaf(2) TO ws-ftotal
           ADD faltaf(3) TO ws-ftotal
           ADD faltaf(4) TO ws-ftotal
           MOVE notaf(1) TO detail3-nota1
           MOVE notaf(2) TO detail3-nota2 
           MOVE notaf(3) TO detail3-nota3 
           MOVE notaf(4) TO detail3-nota4 
           MOVE notaf(5) TO detail3-nota5 
           DIVIDE 4 INTO ws-nota-fin
           MOVE ws-nota-fin TO detail3-notaF
           SUBTRACT ws-ftotal FROM qtd-aulas-dis GIVING ws-ptotal
           MULTIPLY 100 BY ws-ptotal GIVING ws-ptotal
           DIVIDE qtd-aulas-dis INTO ws-ptotal
           MOVE ws-ptotal TO detail3-notaF
           MOVE 10 TO notaf(1)
           SUBTRACT ws-nota-fin FROM notaf(1)
           IF ws-nota-fin > 7 AND detail3-freq > 75
              MOVE "APROVADO" TO detail3-status.
           IF ws-nota-fin > 7 AND detail3-freq < 75
              MOVE "REPROVADO" TO detail3-status.
           IF ws-nota-fin < 7 AND detail3-freq < 75
              MOVE "REPROVADO" TO detail3-status.
           IF ws-nota-fin < 7 AND detail3-freq > 75
              AND notaf(5) < notaf(1)
                  MOVE "REPROVADO" TO detail3-status.
           IF ws-nota-fin < 7 AND detail3-freq > 75
              AND notaf(5) > notaf(1) OR ws-nota-fin < 7
                  AND detail3-freq > 7 AND notaf(5) = notaf(1)
                      MOVE "APROVADO" TO detail3-status.
           ADD 1 TO LIN     
           DISPLAY (LIN , 01) DETAIL3
           MOVE ZEROS TO ws-nota-fin
           MOVE ZEROS TO ws-ftotal
           MOVE ZEROS TO ws-ptotal
           ADD 1 TO ws-dis-cod 
           GO TO 0710-seleciona-alunos.

       0890-finaliza.
           CLOSE CADCUR
           CLOSE CADDIS
           CLOSE CADNOT
           CLOSE RELAT
           CHAIN 'mnotas.exe'.
