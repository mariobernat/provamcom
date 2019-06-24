      * CALCULO DO DIGITO VERIFICADOR
       99-VALIDA-CNPJ.
           COMPUTE WS-AGRUPA = ((WS-DIG01 * 6) +
                                (WS-DIG02 * 7) +
                                (WS-DIG03 * 8) +
                                (WS-DIG04 * 9) +
                                (WS-DIG05 * 2) +
                                (WS-DIG06 * 3) +
                                (WS-DIG07 * 4) +
                                (WS-DIG08 * 5) +
                                (WS-DIG09 * 6) +
                                (WS-DIG10 * 7) +
                                (WS-DIG11 * 8) +
                                (WS-DIG12 * 9)).
           DIVIDE WS-AGRUPA BY 11 GIVING WS-RESULTADO.
           COMPUTE WS-RESTO = WS-AGRUPA -
                             (WS-RESULTADO * 11).
           IF WS-RESTO = 10
              MOVE 0 TO WS-RESTO.
           MOVE WS-RESTO TO WS-DIG13. 
      *
           COMPUTE WS-AGRUPA = ((WS-DIG01 * 5) +
                                (WS-DIG02 * 6) +
                                (WS-DIG03 * 7) +
                                (WS-DIG04 * 8) +
                                (WS-DIG05 * 9) +
                                (WS-DIG06 * 2) +
                                (WS-DIG07 * 3) +
                                (WS-DIG08 * 4) +
                                (WS-DIG09 * 5) +
                                (WS-DIG10 * 6) +
                                (WS-DIG11 * 7) +
                                (WS-DIG12 * 8) +
                                (WS-DIG13 * 9)).
           DIVIDE WS-AGRUPA BY 11 GIVING WS-RESULTADO.
           COMPUTE WS-RESTO = WS-AGRUPA -
                             (WS-RESULTADO * 11).
           IF WS-RESTO = 10
              MOVE 0 TO WS-RESTO.
           MOVE WS-RESTO TO WS-DIG14. 
      *
       99-VALIDA-CPF.
           COMPUTE WS-AGRUPA = ((WS-DIG01 * 1) +
                                (WS-DIG02 * 2) +
                                (WS-DIG03 * 3) +
                                (WS-DIG04 * 4) +
                                (WS-DIG05 * 5) +
                                (WS-DIG06 * 6) +
                                (WS-DIG07 * 7) +
                                (WS-DIG08 * 8) +
                                (WS-DIG09 * 9)).
           DIVIDE WS-AGRUPA BY 11 GIVING WS-RESULTADO.
           COMPUTE WS-RESTO = WS-AGRUPA -
                             (WS-RESULTADO * 11).
           IF WS-RESTO = 10
              MOVE 0 TO WS-RESTO.
           MOVE WS-RESTO TO WS-DIG10. 
      *
           COMPUTE WS-AGRUPA = ((WS-DIG01 * 0) +
                                (WS-DIG02 * 1) +
                                (WS-DIG03 * 2) +
                                (WS-DIG04 * 3) +
                                (WS-DIG05 * 4) +
                                (WS-DIG06 * 5) +
                                (WS-DIG07 * 6) +
                                (WS-DIG08 * 7) +
                                (WS-DIG09 * 8) +
                                (WS-DIG10 * 9)).
           DIVIDE WS-AGRUPA BY 11 GIVING WS-RESULTADO.
           COMPUTE WS-RESTO = WS-AGRUPA -
                             (WS-RESULTADO * 11).
           IF WS-RESTO = 10
              MOVE 0 TO WS-RESTO.
           MOVE WS-RESTO TO WS-DIG11. 
      *