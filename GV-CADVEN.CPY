      * CADASTRO DE VENDEDORES
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-STATUS IS WID-ARQ-VENDEDOR.
      *
       01  FOR-REGISTRO-VENDEDOR.
           03 FOR-CHAVE-VENDEDOR.
              05 FOR-CD-VENDEDOR     PIC  9(007).
           03 FOR-NR-CPF             PIC  9(011).
           03 FOR-NM-VENDEDOR        PIC  X(040).
           03 FOR-NR-LATITUDE        PIC S9(003)V9(008).
           03 FOR-NR-LONGITUDE       PIC S9(003)V9(008).
      * 
