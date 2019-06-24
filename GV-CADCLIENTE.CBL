000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. GV-CADCLIENTE.
000300*
000400* Sistema de Gestão de Vendas
      *
      *     Função: Cadastro de Clientes
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
           SELECT ARQ-IMPORTACAO ASSIGN TO DISK
             organization       is line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       COPY GV-CADCLI.CPY.
       COPY GV-IMPCLI.CPY.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-IDENTIFICACAO-ARQUIVOS.
           03 WID-ARQ-CLIENTE        PIC X(15) VALUE "GV-CLIENTE.DAT".
           03 WID-ARQ-IMPORTACAO     PIC X(20) VALUE SPACES.
      *
       01  WS-RESULTADO-ACESSO.
           03 WS-ESTADO-ARQUIVO      PIC X(02) VALUE "00".
              88 WS-OPERACAO-BEM-SUCEDIDA      VALUE "00".
              88 WS-ERRO-ACESSO                VALUE "39".
      *
       01  WS-CAMPOS-TRABALHO
           03 WS-OPCAO               PIC 9(01) VALUE ZEROS.
              88 WS-INCLUSAO                   VALUE 1.
              88 WS-ALTERACAO                  VALUE 2.
              88 WS-EXCLUSAO                   VALUE 3.
              88 WS-IMPORTACAO                 VALUE 4.
              88 WS-FIM-PROGRAMA               VALUE 0 9.
              88 WS-OPCAO-VALIDA               VALUE 1 2 3 4. 
           03 WS-MENSAGEM            PIC X(68).
           03 WS-RESPOSTA            PIC X(01).
              88 WS-SIM                        VALUE "S" "s".
              88 WS-NAO                        VALUE "N" "n".
           03 WS-ACAO                PIC X(01).
              88 WS-ACAO-INCLUSAO              VALUE "I".
              88 WS-ACAO-ALTERACAO             VALUE "A".
              88 WS-FIM-ACAO                   VALUE "F".
           03 WS-FINAL-ARQUIVO       PIC X(01) VALUE SPACE.
              88 WS-FIM-ARQUIVO                VALUE "F".
           03 WS-ERRO                PIC X(60).
      *
      * DADOS DO CLIENTE
           03 WS-CD-CLIENTE              PIC 9(007).
           03 WS-NR-CNPJ-CLIENTE         PIC 9(015).
           03 WS-DS-RAZAO-SOCIAL         PIC X(040).
           03 WS-NR-LATITUDE             PIC ZZ9,99999999.
           03 WS-NR-LONGITUDE            PIC ZZ9,99999999.
      *
      * CALCULO DO DIGITO CNPJ/CPF
        COPY GV-VALIDACPF1.CPY.
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
       01  SST-LIMPA-REGISTRO.
           03 LINE 07 COL 27 VALUE "                    ".
           03 LINE 08 COL 27 VALUE
                           "                                        ".
           03 LINE 09 COL 27 VALUE "                    ".
           03 LINE 10 COL 27 VALUE "                    ".
           03 LINE 11 COL 27 VALUE "                    ".
      *
       01  SST-CADASTRO-CLIENTE.
           03 LINE 02 COL 30 VALUE "Cadastro de Clientes".           
           03 LINE 05 COL 08 VALUE "Informe a Opção: ".
           03 OPCAO LINE 05 COL 26 PIC 9 USING WS-OPCAO AUTO
           03 LINE 05 COL 28 VALUE 
              "(1-Inclusão, 2-Alteração, 3-Exclusão, 4-Importação)".
      *
       01  SST-CD-CLIENTE.
           03 LINE 07 COL 07 VALUE " Códido do Cliente: ".
           03 LINE 07 COL 26 USING WS-CD-CLIENTE.
      *
       01  SST-DADOS-CLIENTE.
           03 LINE 08 COL 07 VALUE "              CNPJ: ".
           03 LINE 08 COL 26 USING WS-NR-CNPJ-CLIENTE.
           03 LINE 09 COL 07 VALUE "      Razão Social: ".
           03 LINE 09 COL 26 USING WS-DS-RAZAO-SOCIAL.
           03 LINE 10 COL 07 VALUE "          Latitude: ".
           03 LINE 10 COL 26 USING WS-NR-LATITUDE.
           03 LINE 11 COL 07 VALUE "         Longitude: ".
           03 LINE 11 COL 26 USING WS-NR-LONGITUDE.
      *
       01  SST-IMPORTA-CLIENTE.
           03 LINE 14 COL 07 VALUE "Informe o Arquivo a ser Importado:".
           03 LINE 14 COL 42 USING WID-ARQUIVO-ENTRADA. 
      *
       01  SST-MENSAGEM.
           03 LINE 22 COL 11 USING WS-MENSAGE.
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
              
       1-SAIDA.
           EXIT.
      *
       2-MODULO-PRINCIPAL SECTION.
      *------------------
       2.
           ACCEPT SST-CADASTRO-CLIENTE.
           IF  WS-INCLUSAO
               PERFORM 21-INCLUI-CLIENTE
                          UNTIL WS-FIM-ACAO. 
           IF  WS-ALTERACAO
               PERFORM 22-ALTERA-CLIENTE
                          UNTIL WS-FIM-ACAO. 
           IF  WS-EXCLUSAO
               PERFORM 23-EXCLUI-CLIENTE
                          UNTIL WS-FIM-ACAO. 
           IF  WS-IMPORTACAO
               PERFORM 24-IMPORTA-CLIENTE
                          UNTIL WS-FIM-ACAO. 
       2-SAIDA.
           EXIT.
      *
       21-INCLUI-CLIENTE SECTION. 
      *-----------------
       21.
           ACCEPT SST-CD-CLIENTE.
           IF WS-CD-CLIENTE EQUAL ZEROS
              MOVE "F" TO TO WS-OPCAO
           ELSE
              PERFORM 211-TRATAR-CLIENTE.          
       21-SAIDA.
           EXIT.
      *
       211-TRATAR-CLIENTE SECTION.
       211.
           ACCEPT SST-DADOS-CLIENTE.
           MOVE "Confirma Inclusão (S/N) ?" TO WS-MENSAGEM. 
           PERFORM 99-MENSAGEM.
           IF WS-SIM
      *
      *       Valida o codigo do cliente ----
              MOVE WS-CD-CLIENTE TO CLI-NR-CNPJ-CLIENTE
              PERFORM 99-LER-CLIENTE
              IF WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Codigo do cliente já cadastrado!" TO WS-ERRO
              ELSE
      *
      *          Valida se o CNPJ já foi existe ----            
                 MOVE WS-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
                                            WS-NR-CNPJ-CLIENTE
                                            WS-CNPJ
                 MOVE ZEROS TO CLI-CD-CLIENTE
                               WS-ESTADO-ARQUIVO
                 START ARQ-CLIENTE KEY NOT LESS CLI-CHAVE-CLIENTE
                 PERFORM 99-LER-CLIENTE-NEXT
                       UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA
                       OR  (WS-NR-CNPJ-CLIENTE EQUAL CLI-NR-CNPJ-CLIENTE
                       AND (WS-CD-CLIENTE NOT EQUAL CLI-CD-CLIENTE)
                 IF  WS-OPERACAO-BEM-SUCEDIDA
                 AND WS-NR-CNPJ-CLIENTE EQUAL CLI-NR-CNPJ-CLIENTE
                    MOVE "CNPJ já Cadastrado" TO WS-ERRO
                 ELSE
      *
      *             Valida o CNPJ do Cliente ----            
                    PERFORM 99-VALIDA-CNPJ
                    IF WS-NR-CNPJ-CLIENTE NOT EQUAL WS-CNPJ 
                       MOVE "CNPJ Invalido!" TO WS-ERRO.
      *
       211-MOVE-DADOS.
           IF WS-SIM
              IF WS-ERRO NOT EQUAL SPACES
                 MOVE WS-ERRO TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE WS-CD-CLIENTE      TO CLI-CD-CLIENTE
                 MOVE WS-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
                 MOVE WS-DS-RAZAO-SOCIAL TO CLI-DS-RAZAO-SOCIAL
                 MOVE WS-NR-LATITUDE     TO CLI-NR-LATITUDE
                 MOVE WS-NR-LONGITUDE    TO CLI-NR-LONGITUDE
                 MOVE ZEROS TO CLI-CD-VENDEDOR
                               CLI-QT-DISTANCIA
                 PERFORM 99-GRAVA-CLIENTE.
       211-SAIDA.
           EXIT.
      *
       22-ALTERA-CLIENTE SECTION.
      *-----------------
       22.
           ACCEPT SST-CD-CLIENTE.
           MOVE WS-CD-CLIENTE   TO CLI-CD-CLIENTE.
           IF WS-CD-CLIENTE EQUAL ZEROS
              MOVE "F" TO TO WS-OPCAO
           ELSE 
              PERFORM 99-LER-CLIENTE
              IF NOT WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Código do Cliente não Encontrado." TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE CLI-NR-CNPJ-CLIENTE TO WS-NR-CNPJ-CLIENTE
                 MOVE CLI-DS-RAZAO-SOCIAL TO WS-DS-RAZAO-SOCIAL
                 MOVE CLI-NR-LATITUDE     TO WS-NR-LATITUDE
                 MOVE CLI-NR-LONGITUDE    TO WS-NR-LONGITUDE
                 PERFORM 221-TRATA-CLIENTE.
       221-SAIDA.
           EXIT.
      *
       221-TRATA-CLIENTE SECTION.
       221.
           ACCEPT SST-DADOS-CLIENTE.
           MOVE "Confirma Alteração (S/N) ?" TO WS-MENSAGEM. 
           PERFORM 99-MENSAGEM.
           IF WS-SIM
      *
      *       Valida se o CNPJ já foi existe ----            
              MOVE WS-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
                                         WS-CNPJ
              MOVE ZEROS TO CLI-CD-CLIENTE
                            WS-ESTADO-ARQUIVO
              START ARQ-CLIENTE KEY NOT LESS CLI-CHAVE-CLIENTE
              PERFORM 99-LER-CLIENTE-NEXT
                      UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA
                       OR  (WS-NR-CNPJ-CLIENTE EQUAL CLI-NR-CNPJ-CLIENTE
                       AND (WS-CD-CLIENTE NOT EQUAL CLI-CD-CLIENTE)
              IF  WS-OPERACAO-BEM-SUCEDIDA
              AND WS-NR-CNPJ-CLIENTE EQUAL CLI-NR-CNPJ-CLIENTE
                 MOVE "CNPJ já Cadastrado" TO WS-ERRO
              ELSE
      *
      *          Valida o CNPJ do Cliente ----            
                 PERFORM 99-VALIDA-CNPJ
                 IF WS-NR-CNPJ-CLIENTE NOT EQUAL WS-CNPJ 
                    MOVE "CNPJ Invalido!" TO WS-ERRO.
      *
       221-MOVE-DADOS.
           IF WS-SIM
              IF WS-ERRO NOT EQUAL SPACES
                 MOVE WS-ERRO TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE WS-CD-CLIENTE      TO CLI-CD-CLIENTE
                 MOVE WS-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
                 MOVE WS-DS-RAZAO-SOCIAL TO CLI-DS-RAZAO-SOCIAL
                 MOVE WS-NR-LATITUDE     TO CLI-NR-LATITUDE
                 MOVE WS-NR-LONGITUDE    TO CLI-NR-LONGITUDE
                 PERFORM 99-REGRAVA-CLIENTE
                 IF NOT WS-OPERACAO-BEM-SUCEDIDA
                    MOVE "Erro ao Excluir o Cliente." TO WS-MENSAGEM
                    PERFORM 99-MENSAGEM
                 ELSE 
                    DISPLAY SST-LIMPA-REGISTRO.
       221-SAIDA.
           EXIT.
      *
       23-EXCLUI-CLIENTE SECTION.
      *-----------------
       23.
           ACCEPT SST-CD-CLIENTE.
           MOVE WS-CD-CLIENTE TO CLI-CD-CLIENTE
           IF WS-CD-CLIENTE EQUAL ZEROS
              MOVE "F" TO TO WS-OPCAO.
      *
       23-TRATA-CLIENTE.
           IF WS-CD-CLIENTE > ZEROS
              PERFORM 99-LER-CLIENTE
              IF NOT WS-OPERACAO-BEM-SUCEDIDA
                 MOVE "Código do Cliente não Encontrado." TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
              ELSE
                 MOVE CLI-NR-CNPJ-CLIENTE TO WS-NR-CNPJ-CLIENTE
                 MOVE CLI-DS-RAZAO-SOCIAL TO WS-DS-RAZAO-SOCIAL
                 MOVE CLI-NR-LATITUDE     TO WS-NR-LATITUDE
                 MOVE CLI-NR-LONGITUDE    TO WS-NR-LONGITUDE
                 DISPLAY SST-DADOS-CLIENTE
                 MOVE "Confirma os Exclusão (S/N) ?" TO WS-MENSAGEM
                 PERFORM 99-MENSAGEM
                 IF WS-SIM
                    PERFORM 99-DELETA-CLIENTE
                    IF NOT WS-OPERACAO-BEM-SUCEDIDA
                       MOVE "Erro ao Excluir o Cliente." TO WS-MENSAGEM
                       PERFORM 99-MENSAGEM
                    ELSE 
                       DISPLAY SST-LIMPA-REGISTRO.
       23-SAIDA.
           EXIT.
      *
       24-IMPORTA-CLIENTE SECTION.
      *------------------
       24.
           DISPLAY SST-IMPORTA-CLIENTE.
           IF WID-ARQUIVO-ENTRADA EQUAL SPACE
              MOVE "F" TO WS-OPCAO
           ELSE
              MOVE "Confirma os dados (S/N) ?" TO WS-MENSAGEM
              PERFORM 99-MENSAGEM
              IF WS-SIM
                 PERFORM 241-TRATA-IMPORTACAO.
       24-SAIDA.
           EXIT.
      *
       241-TRATA-IMPORTACAO SECTION.
       241.
           OPEN INPUT ARQUIVO-ENTRADA
           IF WS-OPERACAO-BEM-SUCEDIDA
              MOVE SPACE TO WS-FIM-ARQUIVO
              PERFORM 99-LER-ARQUIVO-ENTRADA
              PERFORM 2411-TRATA-REGISTROS
                      UNTIL WS-FIM-ARQUIVO
              CLOSE ARQ-IMPORTACAO
           ELSE
              MOVE "Arquivo não encontrado ou com erro!" 
                   TO WS-MENSAGEM 
              PERFORM 99-MENSAGEM.
       241-SAIDA.
           EXIT.
      *
       2411-TRATA-REGISTROS SECTION.
       2411. 
           MOVE SPACES TO WS-ERRO 
                          WS-ACAO.
           MOVE ARQ-CD-CLIENTE      TO CLI-CD-CLIENTE.
           START ARQ-CLIENTE KEY EQUAL CLI-CHAVE-CLIENTE.
           PERFORM 99-LER-CLIENTE-NEXT.
           IF WS-OPERACAO-BEM-SUCEDIDA
              IF ARQ-CD-CLIENTE EQUAL CLI-CD-CLIENTE
                 MOVE "A" TO WS-ACAO
              ELSE
                 MOVE "I" TO WS-ACAO
           ELSE
              MOVE "I" TO WS-ACAO.
      *
           MOVE ARQ-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
           MOVE ZEROS TO CLI-CD-CLIENTE
                         WS-ESTADO-ARQUIVO.           
           START ARQ-CLIENTE KEY NOT LESS CLI-CHAVE-CLIENTE.
           PERFORM 99-LER-CLIENTE-NEXT
                   UNTIL NOT WS-OPERACAO-BEM-SUCEDIDA
                      OR (ARQ-NR-CNPJ-CLIENTE EQUAL CLI-NR-CNPJ-CLIENTE
                     AND (ARQ-CD-CLIENTE NOT EQUAL CLI-CD-CLIENTE).
      *
           IF ARQ-NR-CNPJ-CLIENTE EQUAL CLI-NR-CNPJ-CLIENTE
           AND WS-ACAO = "I"
              MOVE "CNPJ já Cadastrado" TO WS-ERRO.
      *
           MOVE ARQ-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
                                       WS-CNPJ.
           PERFORM 99-VALIDA-CNPJ.
           IF ARQ-NR-CNPJ-CLIENTE NOT EQUAL WS-CNPJ 
              MOVE "CNPJ Invalido" TO WS-ERRO.
      *
       241-MOVE-DADOS.
           IF WS-ERRO EQUAL SPACES
              MOVE ARQ-CD-CLIENTE      TO CLI-CD-CLIENTE
              MOVE ARQ-NR-CNPJ-CLIENTE TO CLI-NR-CNPJ-CLIENTE
              MOVE ARQ-DS-RAZAO-SOCIAL TO CLI-DS-RAZAO-SOCIAL
              MOVE ARQ-NR-LATITUDE     TO CLI-NR-LATITUDE
              MOVE ARQ-NR-LONGITUDE    TO CLI-NR-LONGITUDE
              IF WS-ACAO-INCLUSAO
                 MOVE ZEROS TO CLI-CD-VENDEDOR
                               CLI-QT-DISTANCIA
                 PERFORM 99-GRAVA-CLIENTE
              ELSE 
                 PERFORM 99-REGRAVA-CLIENTE.
      *
       241-LER-PROXIMO.
           PERFORM 99-LER-ARQUIVO-ENTRADA.
      * 
       241-SAIDA.
           EXIT.
      *
       3-MODULO-FINAL SECTION.
      *--------------
       3.
           CLOSE ARQ-CLIENTE.
           DISPLAY SST-LIMPA-TELA.
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
       99-GRAVA-CLIENTE.
           WRITE CLI-REGISTRO-CLIENTE.
      *
       99-REGRAVA-CLIENTE.
           REWRITE CLI-REGISTRO-CLIENTE.
      *
       99-LER-IMPORTACAO.
           READ ARQ-REGISTRO-CLIENTE 
               AT END MOVE "F" TO WS-FIM-ARQUIVO.
      *
       99-FECHA-ARQUIVO-ENTRADA.
           CLOSE ARQ-IMPORTACAO.
      *
       COPY GV-VALIDACPF2.CPY.
      *
       99-MENSAGEM.
           DISPLAY SST-MENSAGEM.
           ACCEPT  SST-MENSAGEM.
      *
