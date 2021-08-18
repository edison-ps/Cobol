      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGIMP01      *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao de Cadastro de Impressoras:                     *
      *                                                             *
      *  Data da ultima alteracao:    24/04/93     v1.00            *
      *                               24/10/93     v1.01            *
      *                               26/06/95     v1.02            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgimp01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
            select arqimp assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is imp-chave
                   file status is imp-status.
      *
       data division.
       file section.
           
       copy fdimp.lib.
      
       working-storage section.
      *
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 nome-arq-imp.
          02 imp-dir                   pic x(03) value "IMP".
          02 filler                    pic x(01) value "\".
          02 imp-nome                  pic x(07) value "ARQIMPA".
          02 filler                    pic x(01) value ".".
          02 imp-ext                   pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGIMP01".
          02 cb-versao                 pic x(06) value "v1.02 ".
      *
       01 campos.
          02 impress                   pic x(10) value spaces.
          02 descricao                 pic x(20) value spaces.
          02 spool                     pic x(01) value spaces.
          02 device                    pic x(12) value spaces.
      *
       01 spl-dev.
          02 spl                       pic x(03) value "SPL".
          02 filler                    pic x(01) value "\".
          02 dev                       pic x(08) value spaces.
      *
       01 linha                        pic 9(04) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 0.
       01 limpa                        pic x(59) value spaces.
       01 mens-erro                    pic x(59) value spaces.
       01 limpa-aux                    pic x(40) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 tamanho                      pic 9(04) comp-5 value 59.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
      *
       copy workgen.lib.
      *
       linkage section.
      *
       01 param-menu.
          02 param-usr                 pic x(10).
          02 param-senha               pic x(10).
          02 param-prioridade          pic 9(01).
          02 param-data                pic 9(05).
          02 param-impress             pic x(12).
      *
       01 campo-rotina.
          02 rotina-col                pic 9(02).
          02 rotina-lin                pic 9(02).
          02 rotina-borda              pic x(01).
          02 rotina-fundo              pic x(01).
          02 rotina-sombra             pic x(01).
          02 rotina-impress            pic x(10).
        
      *
       screen section.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu campo-rotina.
      *
       lab-00.
           move 0 to box-col.
           move rotina-lin to box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f. 
           move rotina-borda to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move rotina-fundo to box-fundo.
           move rotina-sombra to box-sombra.
           perform rot-save-buffer.
           display tela-cabec.
           move rotina-col to box-col.
           add rotina-col to 60 giving box-col-f
           add rotina-lin to 08 giving box-lin-f
           perform rot-box.
           perform display-tela-01.
      *
       lab-01.
           perform display-tela-limpa-cad
           perform sec-inclusao.
      *
       lab-fim.
           move 0 to box-col.
           move 80 to box-col-f.
           move 25 to box-lin-f. 
           perform rot-rest-buffer.
           exit program.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotina section.
      *
       rot-le-imp.
           move 0 to erro.
           read arqimp invalid key move 1 to erro.
           if imp-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-imp.
      *
       rot-move-imp.
           move impress to imp-impress rotina-impress.
           move descricao to imp-descricao.
           move spool to imp-spool.
           move device to imp-device.
           move param-usr to imp-usr.
           move param-data to imp-data.
      *
       rot-move-campos.
           move imp-impress to impress rotina-impress.
           move imp-descricao to descricao.
           move imp-spool to spool.
           move imp-device to device.
           move imp-usr to cab-usuario.
           move imp-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-impress thru dsp-device.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-le-imp-lock.
           move 0 to erro.
           read arqimp next.
           if imp-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-imp-lock
           end-if
           read arqimp with lock.
      *
       rot-erro-imp.
           move " Erro de leitura - ARQIMPA.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqimp key is equal imp-chave invalid key
                 move 1 to erro
                 perform rot-erro-imp
           end-start.
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
       display-tela-01.
           add rotina-lin to 03 giving linha
           add rotina-col to 02 giving coluna.
           display "Impressora.:" at line linha column coluna with
                    highlight foreground-color 06 background-color 01.
           add rotina-lin to 04 giving linha.
           display "Descricao..:" at line linha column coluna with
                   highlight foreground-color 06 background-color 01.
           add rotina-lin to 05 giving linha.
           display "Spool (S/N):" at line linha column coluna with
                   highlight foreground-color 06 background-color 01.
           add rotina-lin to 06 giving linha.
           display "Device.....:" at line linha column coluna with
                   highlight foreground-color 06 background-color 01.
      *
       display-tela-02.
           add rotina-lin to 8 giving linha.
           add rotina-col to 2 giving coluna.
           display limpa at line linha column coluna with highlight
                            foreground-color 02 background-color 03.
           add 1 to coluna.
           display "F1" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 2 to coluna.
           display "-Help" at line linha column coluna with
                              foreground-color 05 background-color 03.
           add 8 to coluna.
           display "F2" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 2 to coluna.
           display "-Consulta" at line linha column coluna with
                               foreground-color 05 background-color 03.
      *
       display-tela-04.
           add rotina-lin to 8 giving linha.
           add rotina-col to 2 giving coluna.
           display limpa at line linha column coluna with highlight
                            foreground-color 02 background-color 03.
           add 1 to coluna.
           display "F2" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 2 to coluna.
           display "-Alt" at line linha column coluna with
                             foreground-color 05 background-color 03.
           add 6 to coluna.
           display "F3" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 2 to coluna.
           display "-Exc" at line linha column coluna with 
                             foreground-color 05 background-color 03.
           add 6 to coluna.
           display "Home" at line linha column coluna with highlight
                             foreground-color 02 background-color 03.
           add 4 to coluna.
           display "-Inic" at line linha column coluna with
                              foreground-color 05 background-color 03.
           add 7 to coluna.
           display "End" at line linha column coluna with highlight
                            foreground-color 02 background-color 03.
           add 3 to coluna.
           display "-Fim" at line linha column coluna with
                             foreground-color 05 background-color 03.
           add 6 to coluna.
           display "PgDown" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 6 to coluna.
           display "-Prox" at line linha column coluna with
                             foreground-color 05 background-color 03.
           add 8 to coluna.
           display "PgUp" at line linha column coluna with highlight
                             foreground-color 02 background-color 03.
           add 4 to coluna.
           display "-Ant" at line linha column coluna with
                              foreground-color 05 background-color 03.
      *
       display-tela-05.
           add rotina-lin to 02 giving linha
           add rotina-col to 53 giving coluna.
           display "Inclusao" at line linha column coluna with highlight
                   foreground-color 06 background-color 01.

      *
       display-tela-06.
           add rotina-lin to 02 giving linha
           add rotina-col to 53 giving coluna.
           display "Consulta" at line linha column coluna with highlight
                   foreground-color 06 background-color 01.
      * 
       display-tela-limpa-cad.
           add rotina-lin to 7 giving linha.
           add rotina-col to 1 giving coluna.
           call "C_Writexy" using by value coluna
                                  by value linha
                                  by value tamanho
                                  by value box-cor-f
                                  by value box-cor-p
                                  by reference limpa.
      *
       display-tela-mensagem-cad.
           add rotina-lin to 7 giving linha.
           add rotina-col to 1 giving coluna.
           move 15 to box-cor-p.
           call "C_Writexy" using by value coluna
                                  by value linha
                                  by value tamanho
                                  by value box-cor-f
                                  by value box-cor-p
                                  by reference mensagem.
      *
       display-tela-erro-cad.
           add rotina-lin to 8 giving linha.
           add rotina-col to 2 giving coluna.
           display mens-erro at line linha column coluna with beep 
                               reverse-video.
      *
      *  Sequencia para dar accept
      *
       acc-impress.
           add rotina-lin to 3 giving linha.
           add rotina-col to 15 giving coluna.
           accept impress at line linha column coluna with auto update
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-descricao.
           add rotina-lin to 4 giving linha.
           add rotina-col to 15 giving coluna.
           accept descricao at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-spool.
           add rotina-lin to 5 giving linha.
           add rotina-col to 15 giving coluna.
           accept spool at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           add rotina-lin to 6 giving linha.
           add rotina-col to 15 giving coluna.
           accept device at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-impress.
           add rotina-lin to 3 giving linha.
           add rotina-col to 15 giving coluna.
           display impress at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       dsp-descricao.
           add rotina-lin to 4 giving linha.
           add rotina-col to 15 giving coluna.
           display descricao at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       dsp-spool.
           add rotina-lin to 5 giving linha.
           add rotina-col to 15 giving coluna.
           display spool at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       dsp-device.
           add rotina-lin to 6 giving linha.
           add rotina-col to 15 giving coluna.
           display device at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
      *  Sequencia para fazer limpeza
      *
       lmp-impress.
           add rotina-lin to 3 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       lmp-descricao.
           add rotina-lin to 4 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       lmp-spool.
           add rotina-lin to 5 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       lmp-device.
           add rotina-lin to 6 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       accept-resposta-cad.
           add rotina-lin to 6 giving linha.
           add rotina-col to 59 giving coluna.
           move spaces to resposta.
           accept resposta at line linha column coluna with auto
                              foreground-color 01 background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       sec-inclusao section.
       lab-inc-00.
           perform display-tela-limpa-cad.
           if imp-stat = "F"
              open i-o arqimp
              if imp-status not = "00"
                 move 
                 " Erro de abertura no ARQIMPA.DAT - Tecle <Enter>" to 
                  mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
              end-if
              move "A" to imp-stat
           end-if.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
           perform display-tela-05.
           perform display-tela-02.
       lab-inc-01.
           move spaces to impress.
           perform lmp-impress.
           perform acc-impress.
           if escape-key = 1
              perform lmp-impress
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform sec-consulta
              perform display-tela-05
              perform display-tela-02
              go to lab-inc-01
           end-if.
           move impress to txt.
           perform rot-texto.
           move txt to impress.
           if impress = spaces
              go to lab-inc-01
           end-if.
           perform dsp-impress.
           move impress to imp-impress.
           perform rot-le-imp.
           if erro = 0
              move " Impressora ja cadastrado - Tecle <Enter>" to 
              mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              perform  display-tela-02
              go to lab-inc-01
           end-if.
       lab-inc-02.
           perform display-tela-limpa-cad.
           move spaces to descricao.
           perform lmp-descricao.
           perform acc-descricao.
           if escape-key = 1
              perform lmp-descricao
              perform display-tela-02
              go to lab-inc-01
           end-if.
           if descricao = spaces
              go to lab-inc-02
           end-if.
       lab-inc-03.
           move spaces to spool.
           perform lmp-spool.
           perform acc-spool.
           if escape-key = 1
              perform lmp-spool
              go to lab-inc-02
           end-if.
           move spool to txt.
           perform rot-texto.
           move txt to spool.
           if spool not = "S" and "N"
              go to lab-inc-03
           end-if.
           perform dsp-spool.
       lab-inc-04.
           move spaces to device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              perform lmp-device
              go to lab-inc-03
           end-if.
           move device to txt.
           perform rot-texto.
           move txt to device.
           if device = spaces and spool = "S"
              move param-usr to txt
              perform rot-texto
              move txt to device
           end-if.
           if spool = "S"
              move device to dev
              move spl-dev to device
           end-if.
           perform dsp-device.
           move "Cadastrar (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-inc-05.
           perform accept-resposta-cad.
           if escape-key = 1
              perform display-tela-limpa-cad
              go to lab-inc-04
           end-if.
           if resposta = "N"
              perform lmp-impress thru lmp-device
              perform display-tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-05
              end-if
           end-if.
           perform rot-move-imp.
           write reg-imp invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQIMPA.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           move "Registro gravado - Tecle <Enter>" to mensagem.
           perform display-tela-mensagem-cad.
           perform rot-keypress.
           perform lmp-impress thru lmp-device.
           perform display-tela-02
           go to lab-inc-01.
      *
       lab-inc-fim.
           move zeros to campo-kbd.
           if imp-stat = "A"
              close arqimp 
              move "F" to imp-stat
           end-if.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           perform display-tela-limpa-cad.
           perform display-tela-06.
      *
       lab-cns-01.
           move spaces to impress.
           perform lmp-impress.
           perform acc-impress.
           if escape-key = 1
              perform lmp-impress
              go to lab-cns-fim
           end-if.
           perform display-tela-04.
           move low-values to imp-chave.
           move impress to imp-impress.
      *
       lab-cns-01-a.
           start arqimp key is not less imp-chave.
           go to lab-cns-04.
      *
       lab-cns-02.
           move 0 to erro.
           read arqimp previous at end move 1 to erro.
           if imp-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to lab-cns-02
           end-if.
           if erro not = 0 or imp-impress = impress 
              perform lmp-impress thru lmp-device
              move "Inicio do arquivo - Tecle <Enter>" to mensagem
              display tela-mensagem
              perform rot-keypress
              display tela-limpa
              start arqimp key is not less imp-chave
              move 1 to erro
              go to lab-cns-06
           end-if.
           if imp-chave = high-values
              go to lab-cns-02
           end-if.
           go to lab-cns-05.
      *
       lab-cns-03.
           start arqimp key is less imp-chave.
      *
       lab-cns-04.
           move 0 to erro.
           read arqimp next at end move 1 to erro.
           if imp-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to lab-cns-04
           end-if.
           if erro not = 0
              move " Erro de leitura - ARQTABLA.DAT - Tecle <Enter>" to
              mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-cns-fim
           end-if.
           if imp-chave = high-values 
              perform lmp-impress thru lmp-device
              move "Fim do arquivo - Tecle <Enter>" to mensagem
              display tela-mensagem
              perform rot-keypress
              display tela-limpa
              start arqimp key is not less imp-chave
              move spaces to impress
              move 1 to erro
              go to lab-cns-06
           end-if.
      *
       lab-cns-05.
           perform rot-display.
      *
       lab-cns-06.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-01-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-01-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-04
                    when kbd-aux = 73
                         go to lab-cns-02
                    when kbd-aux = 71
                         move low-values to imp-chave
                         go to lab-cns-01-a
                    when kbd-aux = 79
                         move high-values to imp-chave
                         go to lab-cns-03
           end-evaluate.
           if erro = 0
              move impress to rotina-impress
           end-if.
           if kbd-aux not = 1
              go to lab-cns-06
           end-if.
      *
       lab-cns-fim.
           perform lmp-impress thru lmp-device.
           display tela-limpa.
           exit.
      *
       sec-exclusao section.
       lab-exc-00-0.
           perform display-tela-limpa-cad.
           if param-prioridade < 7
              move " Usuario sem prioridade para esta funcao - Tecle <En
      -       "ter>" to mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              go to lab-exc-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-exc-fim
           end-if.
       lab-exc-00.
           perform rot-le-imp-lock.
           perform rot-display.
           move "Excluir (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-exc-01.
           perform accept-resposta-cad.
           if escape-key = 1
              perform display-tela-limpa-cad
              go to lab-exc-fim
           end-if.
           if resposta = "N"
              go to lab-exc-fim
           else
              if resposta not = "S"
                 go to lab-exc-01
              end-if
           end-if.
           delete arqimp invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQIMP.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-fim
           end-delete.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           perform display-tela-mensagem-cad.
           perform rot-keypress.
           perform display-tela-limpa-cad.
      *
       lab-exc-fim.
           unlock arqimp record.
           perform display-tela-04.
           exit.
      *
       sec-alteracao section.
      *
       lab-alt-00-0.
           perform display-tela-limpa-cad.
           if param-prioridade < 5
              move " Usuario sem prioridade para esta funcao - Tecle <En
      -       "ter>" to mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-alt-fim
           end-if.
       lab-alt-00.
           perform rot-le-imp-lock.
           perform rot-display.
       lab-alt-01.
           perform acc-descricao.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           if descricao = spaces 
              go to lab-alt-01
           end-if.
       lab-alt-02.
           perform acc-spool.
           if escape-key = 1
              go to lab-alt-01
           end-if.
           move spool to txt.
           perform rot-texto.
           move txt to spool.
           if spool not = "S" and "N"
              go to lab-alt-02
           end-if.
           perform dsp-spool.
       lab-alt-03.
           if imp-spool = "S"
              move device to spl-dev
              move dev to device
              perform dsp-device
           end-if.
           perform acc-device.
           if escape-key = 1
              if imp-spool = "S"
                 move spl-dev to device
                 perform dsp-device
              end-if
              go to lab-alt-02
           end-if.
           move device to txt.
           perform rot-texto.
           move txt to device.
           if device = spaces and spool = "S"
              move param-usr to txt
              perform rot-texto
              move txt to device
           end-if.
           if spool = "S"
              move device to dev
              move spl-dev to device
           end-if.
           perform dsp-device.
           move "Alterar (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-alt-04.
           perform accept-resposta-cad.
           if escape-key = 1
              move spool to imp-spool
              perform display-tela-limpa-cad
              go to lab-alt-03
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-04
              end-if
           end-if.           
           perform rot-move-imp.
           rewrite reg-imp invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQIMPA.DAT - Tecle <Ente
      -            "r>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           perform display-tela-mensagem-cad.
           perform rot-keypress.
           perform display-tela-limpa-cad.
      *
       lab-alt-fim.
           unlock arqimp.
           perform display-tela-04.
           exit.
