      *
      * VALIDACAO CPF/CNPJ
       01  WS-VALIDACAO-CPF-CNPJ.
           03 WS-AGRUPA                  PIC 9(06).
           03 WS-RESULTADO               PIC 9(06).
           03 WS-RESTO                   PIC 9(05).
           03 WS-CNPJ                    PIC 9(15).
           03 WSR-CNPJ REDEFINES WS-CNPJ.
              05 WS-DIG01                PIC 9(01).
              05 WS-DIG02                PIC 9(01).
              05 WS-DIG03                PIC 9(01).
              05 WS-DIG04                PIC 9(01).
              05 WS-DIG05                PIC 9(01).
              05 WS-DIG06                PIC 9(01).
              05 WS-DIG07                PIC 9(01).
              05 WS-DIG08                PIC 9(01).
              05 WS-DIG09                PIC 9(01).
              05 WS-DIG10                PIC 9(01).
              05 WS-DIG11                PIC 9(01).
              05 WS-DIG12                PIC 9(01).
              05 WS-DIG13                PIC 9(01).
              05 WS-DIG14                PIC 9(01).
              05 WS-DIG15                PIC 9(01).
           03 WSR-CPF REDEFINES WS-CNPJ.
              05 WS-CPF                  PIC 9(11).
              05 FILLER                  PIC X(04).
      *
