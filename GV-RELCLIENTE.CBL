000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. GV-CADCLIENTE.
000300*
000400* Sistema de Gestão de Vendas
      *
      *     Função: Relatório de Clientes
      *     Autor: Mario Cesar Bernat
      *     Criação: Jun/2019
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE ASSIGN TO DISK WID-ARQ-CLIENTE
             organization       is indexed
             access mode        is dynamic
             record key         is CLI-CHAVE-CLIENTE
             lock mode          is manual
             file status        is WS-RESULTADO-ACESSO.
           SELECT ARQ-SORT ASSIGN TO DISK.
           SELECT RELATO   ASSIGN TO DISK.
      *
       DATA DIVISION.
       FILE SECTION.
       COPY GV-CADCLI.CPY.
      *
       SD  ARQ-SORT
       01  REG-SORT.
           03 CHAVE-SORT.
              05  SOR-CODIGO        PIC  9(007).
              05  SOR-RAZAO         PIC  X(040).
           03 RCHAVE-SORT REDEFINES CHAVE-SORT.
              05  RSOR-RAZAO        PIC  X(040).
              05  RSOR-CODIGO       PIC  9(007).
           03 SOR-CD-CLIENTE        PIC 9(007).
           03 SOR-NR-CNPJ-CLIENTE   PIC 9(015).
           03 SOR-NR-LATITUDE       PIC 9(003)V9(008).
           03 SOR-NR-LONGITUDE      PIC 9(003)V9(008).
           03 SOR-CD-VENDEDOR       PIC 9(007).
      *
       FD  RELATO
           VALUE OF FILE-ID IS WS01-ARQUIVO
           LABEL RECORD IS  OMITTED.
       01  LINHA                       PIC X(116).
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-IDENTIFICACAO-ARQUIVOS.
           03 WID-ARQ-CLIENTE        PIC X(60) VALUE "GV-CLIENTE.DAT".
           03 WS01-ARQUIVO           PIC X(60) VALUE "IMPRESSAO.IMP".
      *
       01  WS-RESULTADO-ACESSO.
           03 WS-ESTADO-ARQUIVO      PIC X(02) VALUE "00".
              88 WS-OPERACAO-BEM-SUCEDIDA      VALUE "00".
              88 WS-ERRO-ACESSO                VALUE "39".
      *
       01  WS-CAMPOS-TRABALHO
           03 WS-OPCAO-ORDENA        PIC 9(01) VALUE ZEROS.
              88 WS-ORD-ASCENDENTE             VALUE 1.
              88 WS-ORD-DESCENDENTE            VALUE 2.
           03 WS-OPCAO-CLASS         PIC 9(01) VALUE ZEROS.
              88 WS-CLA-CODIGO                 VALUE 1.
              88 WS-CLA-RAZAO-SOCIAL           VALUE 2.
           03 WS-OPCAO-FILTRO        PIC 9(01) VALUE ZEROS.
              88 WS-FIL-TODOS                  VALUE 0.
              88 WS-FIL-COD-CLIENTE            VALUE 1.
              88 WS-FIL-RAZAO-SOCIAL           VALUE 2.
              88 WS-FIL-COD-VENDEDOR           VALUE 3.
           03 WS-CD-CODIGO-FILTRO    PIC 9(07).
           03 WS-DS-RAZAO-FILTRO     PIC X(40).   
           03 WS-NR-LIN              PIC 9(03) VALUE 999. 
           03 WS-NR-PAG              PIC 9(03) VALUE 0. 

           03 WS-MENSAGEM            PIC X(68).
           03 WS-RESPOSTA            PIC X(01).
              88 WS-SIM                        VALUE "S" "s".
              88 WS-NAO                        VALUE "N" "n".
           03 WS-FINAL-ARQUIVO       PIC X(01) VALUE SPACE.
              88 WS-FIM-ARQUIVO                VALUE "F".
           03 WS-FINAL-SORT          PIC X(01) VALUE SPACES.  
              88 WS-FIM-SORT                   VALUE "F".
           03 WS-ERRO                PIC X(60).
      *
      * DADOS DO CLIENTE
           03 WS-CD-CLIENTE              PIC 9(007).
           03 WS-NR-CNPJ-CLIENTE         PIC 9(015).
           03 WS-DS-RAZAO-SOCIAL         PIC X(040).
           03 WS-NR-LATITUDE             PIC SZZ9,99999999.
           03 WS-NR-LONGITUDE            PIC SZZ9,99999999.
      *
      * LAYOUT DO RELATORIOO DE CLIENTES 
       01  CAB-01.
           02 FILLER            PIC X(40) VALUE SPACES.
           02 FILLER            PIC X(58) VALUE "     RELATORIO DE CLIENTES  ".
           02 FILLER            PIC X(05) VALUE "Pag: ".
           02 WL-NR-PAGINA      PIC 9(03) VALUE ZEROS.
      *
       01  CAB-02.
           03 FILLER    PIC X(08) VALUE SPACES.
           03 FILLER    PIC X(07) VALUE " CODIGO".
           03 FILLER    PIC X(01) VALUE SPACES.
           03 FILLER    PIC X(40) VALUE "RAZÃO SOCIAL".
           03 FILLER    PIC X(01) VALUE SPACES.
           03 FILLER    PIC X(14) VALUE "          CNPJ".
           03 FILLER    PIC X(01) VALUE SPACES.
           03 FILLER    PIC X(13) VALUE "      LATITUDE".
           03 FILLER    PIC X(01) VALUE SPACES.
           03 FILLER    PIC X(13) VALUE "     LONGITUDE".
           03 FILLER    PIC X(01) VALUE SPACES.
           03 FILLER    PIC X(08) VALUE "VENDEDOR".
      *
       01  CAB-03.
           03 FILLER PIC X(116) VALUE IS ALL '='.
      *
       01  LIN-01.
           03 FILLER       PIC  X(08) VALUE SPACES.
           03 WL-CODIGO    PIC  ZZZZZZ9.
           03 FILLER       PIC  X(001) VALUE SPACES.
           03 WL-RAZAO     PIC  X(040).
           03 FILLER       PIC  X(001) VALUE SPACES.
           03 WL-CNPJ      PIC  ZZZZZZZZZZZZZ9.
           03 FILLER       PIC  X(001) VALUE SPACES.
           03 WL-LATITUDE  PIC SZZ9,99999999.
           03 FILLER       PIC  X(001) VALUE SPACES.
           03 WL-LONGITUDE PIC SZZ9,99999999.
           03 FILLER       PIC  X(002) VALUE SPACES.
           03 WL-VENDEDOR  PIC  ZZZZZZ9.
           03 FILLER       PIC  X(008) VALUE SPACES.
      *
       SCREEN SECTION.
      *
       01  SST-LIMPA-TELA.
           03 LINE 22 COL 10 BLANK LINE.
           03 LINE 20 COL 01 BLANK LINE.
           03 LINE 19 COL 01 BLANK LINE.
           03 LINE 18 COL 01 BLANK LINE.
           03 LINE 17 COL 01 BLANK LINE.
           03 LINE 16 COL 01 BLANK LINE.
           03 LINE 15 COL 01 BLANK LINE.
           03 LINE 14 COL 01 BLANK LINE.
           03 LINE 13 COL 01 BLANK LINE.
           03 LINE 12 COL 01 BLANK LINE.
           03 LINE 11 COL 01 BLANK LINE.
           03 LINE 10 COL 01 BLANK LINE.
           03 LINE 09 COL 01 BLANK LINE.
           03 LINE 08 COL 01 BLANK LINE.
           03 LINE 07 COL 01 BLANK LINE.
           03 LINE 06 COL 01 BLANK LINE.
           03 LINE 05 COL 01 BLANK LINE.
           03 LINE 04 COL 01 BLANK LINE.
           03 LINE 02 COL 30 VALUE "                              ".
      *
       01  SST-OPCOES-CLIENTE.
           03 LINE 02 COL 30 VALUE "Relatorio de Clientes".           
           03 LINE 05 COL 08 VALUE "    Tipo de Ordenação: ".
           03 OPCAO1 LINE 05 COL 31 PIC 9 USING WS-OPCAO-ORDENA AUTO.
           03 LINE 05 COL 33 VALUE "(1-Ascendente, 2-Decrescente)".
           03 LINE 07 COL 08 VALUE "Tipo de Classificação: ".
           03 OPCAO2 LINE 07 COL 31 PIC 9 USING WS-OPCAO-CLASS AUTO.
           03 LINE 07 COL 33 VALUE "(1-Código, 2-Razão Social)".

           03 LINE 09 COL 08 VALUE "       Tipo de Filtro: ".
           03 OPCAO3 LINE 09 COL 31 PIC 9 USING WS-OPCAO-FILTRO AUTO.
           03 LINE 09 COL 33 VALUE  "(0-Todos, 1-Cód.Cliente, 2-Razão So
      -                             "cial, 3-Cód.Vendedor)".
      *
       01  SST-FILTRO-CODIGO.
           03 LINE 10 COL 10 VALUE "         Códido: ".
           03 LINE 10 COL 26 USING WS-CD-CODIGO-FILTRO.
      *
       01  SST-FILTRO-RAZAO.
           03 LINE 10 COL 10 VALUE "   Razão Social: ".
           03 LINE 10 COL 26 USING WS-DS-RAZAO-FILTRO.
      *
       01  SST-MENSAGEM.
           03 LINE 22 COL 11 USING WS-MENSAGEM.
      *
       COPY GV-LINKAGE. 
      *
       PROCEDURE DIVISION USING LINKAGE-DADOS.
      *
       0-MODULO-CONTROLE SECTION.
      *-----------------
       0.
           PERFORM 1-MODULO-INICIAL.
           PERFORM 2-MODULO-PRINCIPAL
               UNTIL WS-FIM-PROGRAMA.
           PERFORM 3-MODULO-FINAL.
       0-SAIDA.
           EXIT PROGRAM.
      *
       1-MODULO-INICIAL SECTION.
      *----------------
       1.
           OPEN I-O ARQ-CLIENTE.
           IF WS-ERRO-ACESSO 
              MOVE "F" TO WS-FINAL-ARQUIVO
              MOVE "Arquivo de Clientes em Uso." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM.
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE "F" TO WS-FIM-ARQUIVO
              MOVE "Arquivo de Clientes com Erro." TO WS-MENSAGEM
              PERFORM 99-MENSAGEM.
           OPEN OUTPUT RELATO.
       1-SAIDA.
           EXIT.
      *
       2-MODULO-PRINCIPAL SECTION.
      *------------------
       2.
           ACCEPT SST-OPCOES-CLIENTE.
           IF WS-FIL-CLIENTE
           OR WS-FIL-VENDEDOR
              ACCEPT SST-FILTRO-CODIGO.
           IF WS-FIL-RAZAO
              ACCEPT SST-FILTRO-RAZAO.
      *
           IF  (WS-ORD-ASCENDENTE 
           OR   WS-ORD-DESCENDENTE)
           AND (WS-CLA-CODIGO 
           OR   WS-CLA-RAZAO-SOCIAL)
           AND (WS-FIL-COD-CLIENTE 
           OR   WS-FIL-RAZAO-SOCIAL
           OR   WS-FIL-COD-VENDEDOR)
                PERFORM 21-TRATA-ENTRADA.
       2-SAIDA.
           EXIT.
      *
       21-TRATA-ENTRADA SECTION. 
      *----------------
       21.
           MOVE ZEROS TO CLI-CD-CLIENTE
                         WS-ESTADO-ARQUIVO.
           START ARQ-CLIENTE KEY NOT LESS CLI-CHAVE-CLIENTE.
           PERFORM 99-LER-CLIENTE-NEXT
           IF WS-OPERACAO-BEM-SUCEDIDA
              IF WS-ORD-ASCENDENTE 
                 SORT SORTWK1 ON ASCENDING KEY CHAVE-SORT
                      INPUT  PROCEDURE IS 211-CARREGA-SORT 
                                          UNTIL WS-FIM-ARQUIVO
                      OUTPUT PROCEDURE IS 212-GERA-RELATORIO
                                          UNTIL WS-FIM-SORT
              ELSE
              IF WS-ORD-DESCENDENTE
                  SORT SORTWK1 ON DESCENDING KEY CHAVE-SORT
                      INPUT  PROCEDURE IS 211-CARREGA-SORT 
                                          UNTIL WS-FIM-ARQUIVO
                      OUTPUT PROCEDURE IS 212-GERA-RELATORIO
                                          UNTIL WS-FIM-SORT.
       21-SAIDA.
           EXIT.
      *
       211-CARREGA-SORT SECTION
       211.
           IF (WS-FIL-TODOS)
           OR (WS-FIL-COD-CLIENTE 
           AND CLI-CD-CLIENTE  EQUAL WS-CD-CODIGO-FILTRO)
           OR (WS-FIL-COD-VENDEDOR 
           AND CLI-CD-VENDEDOR EQUAL WS-CD-CODIGO-FILTRO)
           OR (WS-FIL-RAZAO-SOCIAL
           AND CLI-DS-RAZAO-SOCIAL EQUAL WS-DS-RAZAO-FILTRO)   
              MOVE CLI-NR-CNPJ-CLIENTE TO SOR-NR-CNPJ-CLIENTE
              MOVE CLI-NR-LATITUDE     TO SOR-NR-LATITUDE
              MOVE CLI-NR-LONGITUDE    TO SOR-NR-LONGITUDE
              MOVE CLI-CD-CLIENTE      TO SOR-CD-CLIENTE
              MOVE CLI-CD-VENDEDOR     TO SOR-CD-VENDEDOR
              IF WS-CLA-CODIGO
                 MOVE CLI-CD-CLIENTE      TO SOR-CODIGO
                 MOVE CLI-DS-RAZAO-SOCIAL TO SOR-RAZAO
                 RELEASE REG-SORT
              ELSE 
                 MOVE CLI-DS-RAZAO-SOCIAL TO RSOR-RAZAO
                 MOVE ZEROS               TO RSOR-CODIGO
                 RELEASE REG-SORT.
           PERFORM 99-LER-CLIENTE-NEXT.
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE "F" TO WS-FIM-ARQUIVO.
       211-CARREGA-SORT-SAIDA
           EXIT.
      *
       212-GERA-RELATORIO SECTION.
       212.
           PERFORM 99-LEITURA-SORT.
           IF NOT WS-FIM-SORT
              IF WS-CLA-CODIGO
                 MOVE SOR-RAZAO   TO WL-RAZAO
              ELSE
                 MOVE RSOR-RAZAO  TO WL-RAZAO
              END-IF
              MOVE SOR-NR-CNPJ-CLIENTE TO WL-NR-CNPJ
              MOVE SOR-NR-LATITUDE     TO WL-LATITUDE
              MOVE SOR-NR-LONGITUDE    TO WL-LONGITUDE
              MOVE SOR-CD-CLIENTE      TO WL-CODIGO
              MOVE SOR-CD-VENDEDOR     TO WL-VENDEDOR
              PERFORM  2121-IMPRIME-LINHA.
       212-SAIDA.
           EXIT.
      *
       2121-IMPRIME-LINHA SECTION.
       2121.
           ADD 1 TO WS-NR-LIN.
           IF WS-NR-LIN > 66
              ADD 1 TO WS-NR-PAG
              IF WS-NR-PAG > 1
                 MOVE SPACES TO LINHA  
                 WRITE LINHA FROM CAB-03 BEFORE ADVANCING PAGE
              END-IF         
              MOVE WS-NR-PAG TO WL-NR-PAGINA
              WRITE LINHA FROM CAB-01 BEFORE ADVANCING 1
              WRITE LINHA FROM CAB-02 BEFORE ADVANCING 1
              WRITE LINHA FROM CAB-03 BEFORE ADVANCING 1
              MOVE SPACES TO LINHA  
              WRITE LINHA FROM CAB-03 BEFORE ADVANCING 1
              MOVE 5 WS-NR-LIN.
           WRITE LINHA FROM LIN-01 BEFORE 1.        
       2121-SAIDA.
           EXIT.
      *
       3-MODULO-FINAL SECTION.
      *--------------
       3.
           CLOSE ARQ-CLIENTE.
           DISPLAY SST-LIMPA-TELA.
           CLOSE RELATO.
       3-SAIDA.
           EXIT.
      *
       99-ROTINAS-DIVERSAS SECTION.
      *-------------------
      *
       99-LER-CLIENTE.
           READ ARQ-CLIENTE.
      *
       99-LER-CLIENTE-NEXT.
           READ ARQ-CLIENTE NEXT.
      * 
       99-LEITURA-SORT.
           RETURN ARQ-SORT
               AT END
                  MOVE "S" TO WS-FINAL-SORT.
      *
       99-MENSAGEM.
           DISPLAY SST-MENSAGEM.
           ACCEPT  SST-MENSAGEM.
      *
