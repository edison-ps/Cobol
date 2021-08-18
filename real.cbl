      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGREAL       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Impressao de O.C.T. :                                      *
      *                                                             *
      *  Data da ultima alteracao:    29/06/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgreal.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 impress                      pic x(12) value "PRN".
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGREAL".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(10) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
      *
       01 cab-1-8.
          02 filler                    pic x(01) value x"1b".
          02 filler                    pic x(01) value "0".
      *
       01 cab-1-6.
          02 filler                    pic x(01) value x"1b".
          02 filler                    pic x(01) value "2".
      *
       01 campos.
          02 sele-inicio               pic 9(10) value 0.
          02 sele-fim                  pic 9(10) value 0.
      *
       01 cab-01.
          02 filler                    pic x(02) value spaces.
          02 cab-doc-1                 pic 9(10) value 0.
          02 filler                    pic x(03) value spaces.
          02 cab-ag-1                  pic 9(04) value 0.
          02 filler                    pic x(03) value spaces.
          02 cab-dv-1                  pic 9(01) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-conta                 pic x(11) value spaces.
          02 filler                    pic x(22) value spaces.
          02 cab-abav-1                pic x(34) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(02) value spaces.
          02 cab-abav-2                pic x(34) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(02) value spaces.
          02 cab-nag-1                 pic x(34) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(58) value spaces.
          02 cab-nag-2                 pic x(34) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(58) value spaces.
          02 cab-doc-2                 pic 9(10) value 0.
      *
       01 cab-06.
          02 filler                    pic x(05) value spaces.
          02 filler                    pic x(15) value "INSCRICAO".
      *
       copy workgen.lib.
      * 
       screen section.
      *
       01 tela-01.
          02 line 12 column 27 foreground-color 07 background-color 04
             highlight value "Impressao de O.C.T.".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Inicio........:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Fim...........:".
      *
       01 tela-02.
          02 line 17 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 05 foreground-color 01 background-color 02
             value "(I)mprimir  (P)osicionar".
      *
       01 tela-08.
          02 line 17 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 17 column 12 foreground-color 07 background-color 02
             highlight value "C".
          02 line 17 column 13 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 17 column 26 foreground-color 07 background-color 02
             highlight value "F".
          02 line 17 column 27 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 17 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 17 column 12 foreground-color 07 background-color 02
             highlight value "I".
          02 line 17 column 13 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 17 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 17 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 17 column 05 foreground-color 04 background-color 04
             pic x(41) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division.
      *
       lab-00.
           call "C_Cls".
           move 03 to box-col.
           move 10 to box-lin.
           move 45 to box-col-f.
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01.
      *
       lab-01.
           display tela-limpa-cad.
           perform sec-selecao.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           call "C_Cls".
      *
       lab-fim.
           stop run.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-posicionar.
           move 9999999999 to cab-doc-1 cab-doc-2.
           move 9999 to cab-ag-1.
           move 9 to cab-dv-1.
           move "9.99999  9"to cab-conta.
           move all "X" to cab-abav-1 cab-abav-2.
           move all "X" to cab-nag-1 cab-nag-2.
           write reg-imp from cab-1-8.
           write reg-imp from cab-01 after 4 lines.
           write reg-imp from cab-02 after 3 lines.
           write reg-imp from cab-03 after 3 lines.
           write reg-imp from cab-04 after 1 lines.
           write reg-imp from cab-06 after 06 lines.
           write reg-imp from cab-05 after 04 lines.
           write reg-imp from spaces after 08 lines.
           write reg-imp from cab-1-6.
      *
       rot-open-imp.
           move 0 to erro.
           move zeros to imp-status
           if imp-stat = "F"
              open output arqimp
              if imp-status not = "00"
                 move " Erro de impressao - Tecle <Enter>" to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to imp-stat
              end-if
           end-if.
      *
       rot-close-imp.
           if imp-stat = "A"
              close arqimp 
              unlock arqimp
              move "F" to imp-stat
           end-if.
      *
       rot-interrompe.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105
              display tela-08
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-09
             end-if
           end-if.
      *
       copy rotgen.lib.
      *
      ************************
      *                      *
      *       T e l a s      *
      *                      *
      ************************
      *
       telas section.
      *
       accept-resposta-cad.
           move spaces to resposta.
           accept resposta at 1540 with auto foreground-color 04
                                             background-color 04.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
      *  Sequencia para dar Accept
      *
       acc-inicio.
           accept sele-inicio at 1422 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-fim.
           accept sele-fim at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-inicio.
           display sele-inicio at 1422 with foreground-color 15 
                   background-color 04.
      *
       dsp-fim.
           display sele-fim at 1522 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-inicio.
           display limpa at 1422 with foreground-color 15 
                   background-color 04.
      *
       lmp-fim.
           display limpa at 1522 with foreground-color 15 
                   background-color 04.
      *
       sec-selecao section.
      *
       lab-sele-01.
           move 0 to sele-inicio.
           perform lmp-inicio.
           perform acc-inicio.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-inicio = 0
              go to lab-sele-01
           end-if.
      *
       lab-sele-02.
           move 0 to sele-fim.
           perform lmp-fim.
           perform acc-fim.
           if escape-key = 1
              perform lmp-fim
              go to lab-sele-01
           end-if.
           if sele-fim < sele-inicio
              go to lab-sele-02
           end-if.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-sele-03.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-02
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-inicio thru lmp-fim
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-03
              end-if
           end-if.
           perform sec-impressao.
           display tela-limpa-cad.
           perform lmp-inicio thru lmp-fim.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-inicio thru lmp-fim.
           exit.
      *
       sec-impressao section.
       lab-imp-00.
           perform rot-open-imp.
           if erro not = 0
              go to lab-sele-fim
           end-if.
      *
       lab-imp-01.
           display tela-02.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-imp-fim
           end-if.
           if resposta = "E"
              display tela-limpa-cad
              go to lab-imp-fim
           else
              if resposta = "P"
                 perform rot-posicionar
                 go to lab-imp-01
              else
                 if resposta not = "I"
                    go to lab-imp-01
                 end-if
              end-if
           end-if.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           display tela-09.
      *
       lab-imp-02.
           perform rot-interrompe.
           if resposta = "F" or sele-inicio > sele-fim
              go to lab-imp-fim
           end-if.
           move sele-inicio to cab-doc-1 cab-doc-2.
           move 0006 to cab-ag-1.
           move 0 to cab-dv-1.
           move "0.701189 2"to cab-conta.
           move "ABAV XXII CONGRESSO BRAS AG VIAG" to cab-abav-1  
                                                      cab-abav-2.
           move "CPJ GRACA ARANHA RJAN" to cab-nag-1 cab-nag-2.
           write reg-imp from cab-1-8.
           write reg-imp from cab-01 after 4 lines.
           write reg-imp from cab-02 after 3 lines.
           write reg-imp from cab-03 after 3 lines.
           write reg-imp from cab-04 after 1 lines.
           write reg-imp from cab-06 after 06 lines.
           write reg-imp from cab-05 after 04 lines.
           write reg-imp from spaces after 10 lines.
           write reg-imp from cab-1-6 before 0 lines.
           add 1 to sele-inicio.
           go to lab-imp-02.
      *
       lab-imp-fim.
           perform rot-close-imp.
           exit.
