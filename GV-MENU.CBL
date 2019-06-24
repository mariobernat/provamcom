000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. VENDAS00.
000300*
000400* Sistema de Gestão de Vendas
      *
      *     Função: Menu Principal
      *     Autor: Mario Cesar Bernat
      *     Criação: Jun/2019
      *
      *     Alterações:
      *   
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CAMPOS-TRABALHO
           03 WS-OPCAO      PIC 9(04) VALUE ZEROS.
              88 WS-CADASTRO-CLIENTE        VALUE 101.
              88 WS-CADASTRO-VENDEDOR       VALUE 102.
              88 WS-RELATORIO-CLIENTE       VALUE 201.
              88 WS-RELATORIO-VENDEDOR      VALUE 202.
              88 WS-EXECUTAR-DISTRIBUICAO   VALUE 301.
              88 WS-FIM-PROGRAMA            VALUE 0 9999.
              88 WS-OPCAO-VALIDA            VALUE 101 102 201 202 301. 
      *
       01  WS-LINKAGE-DADOS. 
           03 WS-PROGRAMA    PIC X(60).
      *
       SCREEN SECTION.
      *
       01  SST-CABECALHO.
           03 BLANK SCREEN.
           03 LINE 01 COL 01 VALUE "AMBEV - Sistema de Gestão de C".
           03 LINE 01 COL 31 VALUE "lientes e Vendedores".
           03 LINE 03 COL 01 VALUE "==============================".
           03 LINE 03 COL 31 VALUE "==============================".
           03 LINE 03 COL 61 VALUE "====================".
           03 LINE 21 COL 01 VALUE "==============================".
           03 LINE 21 COL 31 VALUE "==============================".
           03 LINE 21 COL 61 VALUE "====================".
           03 LINE 22 COL 01 VALUE "Mensagem: ".
      *
       01  SST-MENU.
           03 LINE 02 COL 30 VALUE "Menu Principal".           
           03 LINE 05 COL 10 VALUE "   Cadastros".
           03 LINE 06 COL 10 VALUE "01 - Cliente".
           03 LINE 07 COL 10 VALUE "02 - Vendedor".
           03 LINE 09 COL 10 VALUE "   Relatórios".
           03 LINE 10 COL 10 VALUE "03 - Clientes".
           03 LINE 11 COL 10 VALUE "04 - Vendedores".
           03 LINE 13 COL 10 VALUE "   Executar".
           03 LINE 14 COL 10 VALUE "05 - Distribuição de Clientes".
           03 LINE 16 COL 10 VALUE "Informe a Opção: ".
           03 OPCAO LINE 15 COL 34 PIC 9 USING WS-OPCAO AUTO 
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
       PROCEDURE DIVISION.
      *
       0-MODULO-CONTROLE SECTION.
      *-----------------
       0.
           PERFORM 1-MODULO-INICIAL.
           PERFORM 2-MODULO-PRINCIPAL
               UNTIL WS-FIM-PROGRAMA.
           PERFORM 3-MODULO-FINAL.
       0-SAIDA.
           STOP RUN.
      *
       1-MODULO-INICIAL SECTION.
      *----------------
       1.
           DISPLAY SST-CABECALHO.
       1-SAIDA.
           EXIT.
      *
       2-MODULO-PRINCIPAL SECTION.
      *------------------
       2.
           ACCEPT SST-MENU.
           IF  WS-CADASTRO-CLIENTE
               MOVE "GV-CADCLIENTE"     TO WS-PROGRAMA.
           IF  WS-CADASTRO-VENDEDOR
               MOVE "GV-CADVENDEDOR"    TO WS-PROGRAMA.
           IF  WS-RELATORIO-CLIENTE
               MOVE "GV-RELCLIENTE"     TO WS-PROGRAMA.
           IF  WS-RELATORIO-VENDEDOR
               MOVE "GV-RELVENDEDOR"    TO WS-PROGRAMA.
           IF  WS-EXECUTAR-DISTRIBUICAO
               MOVE "GV-DISTRIBUICAO"   TO WS-PROGRAMA.
      *
           IF  WS-OPCAO-VALIDA
               DISPLAY SST-LIMPA-TELA.
               CALL WS-PROGRAMA USING WS-LINKAGE-DADOS. 
       2-SAIDA.
           EXIT.
      *
       3-MODULO-FINAL SECTION.
      *--------------
       3.
           DISPLAY SST-LIMPA-TELA.
       3-SAIDA.
           EXIT.
      *
