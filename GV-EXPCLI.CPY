      * ARQUIVO DE EXPORTAÇÃO DE CLIENTES EM CSV
       FD  ARQ-EXPORTACAO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-STATUS IS WID-ARQ-EXPORTACAO.
      *
       01  ARQ-REGISTRO-DISTRIBUICAO.
           03 ARQ-CD-CLIENTE         PIC  9(007).
           03 ARQ-DELIMITADOR-1      PIC  X(001).
           03 ARQ-DS-RAZAO-SOCIAL    PIC  X(040).
           03 ARQ-DELIMITADOR-2      PIC  X(001).
           03 ARQ-CD-VENDEDOR        PIC  9(007).
           03 ARQ-DELIMITADOR-3      PIC  X(001).
           03 ARQ-NM-VENDEDOR        PIC  X(040).
           03 ARQ-DELIMITADOR-4      PIC  X(001).
           03 ARQ-QT-DISTRIBUICAO    PIC  9(015).
      *
