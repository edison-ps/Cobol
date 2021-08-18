      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGTAB01      *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao de Generica de Tabelas:                         *
      *                                                             *
      *  Data da ultima alteracao:    21/04/93     v1.00            *
      *                               24/04/93     v1.01            *
      *                               26/06/95     v1.02            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgtab01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
            select arqtabl assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is tabl-chave
                   alternate record key is tabl-chave-1 with duplicates
                   file status is tabl-status.
      
       data division.
       file section.
           
       copy fdtabl.lib.
      
       working-storage section.
      *
       01 tabl-status                  pic x(02) value "00".
       01 tabl-stat                    pic x(01) value "F".
      *
       01 nome-arq-tabl.
          02 tabl-dir                  pic x(03) value "TBL".
          02 filler                    pic x(01) value "\".
          02 tabl-nome                 pic x(08) value "ARQTABLA".
          02 filler                    pic x(01) value ".".
          02 tabl-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGTAB01".
          02 cb-versao                 pic x(06) value "v1.02 ".
      *
       01 campos.
          02 codigo                    pic 9(03) value 0.
          02 descricao                 pic x(40) value spaces.
          02 descricao-aux             pic x(40) value spaces.
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
       copy wstab01.lib.
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
          02 rotina-tipo               pic 9(02).
          02 rotina-codigo             pic 9(03).
        
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
           display cb-programa at 2402.
           display cb-versao at 2412.
           move rotina-col to box-col.
           add rotina-col to 60 giving box-col-f
           add rotina-lin to 06 giving box-lin-f
           perform rot-box.
           perform display-tela-01.
       lab-01.
           perform display-tela-limpa-cad.
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
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl.
      *
       rot-move-tabl.
           move rotina-tipo to wtab01-tipo wtab01-tipo-1.
           move codigo to wtab01-codigo rotina-codigo.
           move descricao-aux to wtab01-descricao-1.
           move descricao to wtab01-descricao.
           move param-usr to wtab01-usr.
           move param-data to wtab01-data.
           move reg-wtab01 to reg-tabl.
           
      *
       rot-move-campos.
           move reg-tabl to reg-wtab01
           move wtab01-codigo to codigo rotina-codigo.
           move wtab01-descricao to descricao.
           move wtab01-usr to cab-usuario.
           move wtab01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-open-tabl.
           move 0 to erro.
           if tabl-stat = "F"
              open i-o arqtabl
              if tabl-status not = "00"
                 move 
                 " Erro de abertura no ARQTABLA.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to tabl-stat
              end-if
           end-if.
      *
       rot-close-tabl.
           if tabl-stat = "A"
              close arqtabl 
              move "F" to tabl-stat
           end-if.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqtabl previous at end move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqtabl next at end move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-le-tabl-lock.
           move 0 to erro.
           read arqtabl next.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl-lock
           end-if
           read arqtabl with lock.
      *
       rot-erro-tabl.
           move " Erro de leitura - ARQTABLA.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-inicio-arq.
           perform lmp-codigo thru lmp-descricao.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arq.
           perform lmp-codigo thru lmp-descricao.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqtabl key is equal tabl-chave invalid key
                 move 1 to erro
                 perform rot-erro-tabl
           end-start.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-codigo thru dsp-descricao.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
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
       display-tela-01.
           add rotina-lin to 03 giving linha
           add rotina-col to 02 giving coluna.
           display "Codigo....:" at line linha column coluna with
                                    foreground-color 06
                                    background-color 01 highlight.
           add rotina-lin to 04 giving linha.
           display "Descricao.:" at line linha column coluna with
                                    foreground-color 06
                                    background-color 01 highlight.
      *
       display-tela-02.
           add rotina-lin to 6 giving linha.
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
       display-tela-03.
           add rotina-lin to 6 giving linha.
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
           display "C" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 1 to coluna.
           display "odigo" at line linha column coluna with
                              foreground-color 05 background-color 03.
           add 9 to coluna.
           display "D" at line linha column coluna with highlight
                          foreground-color 02 background-color 03.
           add 1 to coluna.
           display "escricao" at line linha column coluna with
                                foreground-color 05 background-color 03.
      *
       display-tela-04.
           add rotina-lin to 6 giving linha.
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
           add rotina-lin to 5 giving linha.
           add rotina-col to 1 giving coluna.
           call "C_Writexy" using by value coluna
                                  by value linha
                                  by value tamanho
                                  by value box-cor-f
                                  by value box-cor-p
                                  by reference limpa.
      *
       display-tela-mensagem-cad.
           add rotina-lin to 5 giving linha.
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
           add rotina-lin to 6 giving linha.
           add rotina-col to 2 giving coluna.
           display mens-erro at line linha column coluna with beep 
                               reverse-video.
      *
      *  Sequencia para dar accept
      *
       acc-codigo.
           add rotina-lin to 3 giving linha.
           add rotina-col to 14 giving coluna.
           accept codigo at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-descricao.
           add rotina-lin to 4 giving linha.
           add rotina-col to 14 giving coluna.
           accept descricao at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           add rotina-lin to 3 giving linha.
           add rotina-col to 14 giving coluna.
           display codigo at line linha column coluna with
                             foreground-color 15 background-color 01.
      *
       dsp-descricao.
           add rotina-lin to 4 giving linha.
           add rotina-col to 14 giving coluna.
           display descricao at line linha column coluna with
                                foreground-color 15 background-color 01.
      *
      *  Sequencia para fazer limpeza
      *
       lmp-codigo.
           add rotina-lin to 3 giving linha.
           add rotina-col to 14 giving coluna.
           display limpa-aux at line linha column coluna with
                                foreground-color 15 background-color 01.
      *
       lmp-descricao.
           add rotina-lin to 4 giving linha.
           add rotina-col to 14 giving coluna.
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
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mens-erro.
           perform display-tela-erro-cad.
           perform rot-keypress.
      *
       sec-inclusao section.

       lab-inc-00.
           perform display-tela-limpa-cad.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
           perform display-tela-05.
           perform display-tela-02.
       lab-inc-01.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-codigo
              perform sec-consulta
              perform display-tela-05
              perform display-tela-02
              go to lab-inc-01
           end-if.
           if codigo = 0
              go to lab-inc-01
           end-if.
           move rotina-tipo to wtab01-tipo.
           move codigo to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro = 0
              move " Codigo ja cadastrado - Tecle <Enter>" to mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              perform  display-tela-02
              go to lab-inc-01
           end-if.
           perform display-tela-limpa-cad.
       lab-inc-02.
           move spaces to descricao.
           perform lmp-descricao.
           perform acc-descricao.
           if escape-key = 1
              perform lmp-descricao
              perform display-tela-02
              go to lab-inc-01
           end-if.
           move descricao to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-02
           end-if.
           move txt to descricao-aux.
           move "Cadastrar (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-inc-03.
           perform accept-resposta-cad.
           if escape-key = 1
              perform display-tela-limpa-cad
              go to lab-inc-02
           end-if.
           if resposta = "N"
              perform lmp-codigo thru lmp-descricao
              perform display-tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-03
              end-if
           end-if.
           perform rot-move-tabl.
           write reg-tabl invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQTABLA.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           move "Registro gravado - Tecle <Enter>" to mensagem.
           perform display-tela-mensagem-cad.
           perform rot-keypress.
           perform lmp-codigo thru lmp-descricao.
           perform display-tela-02
           go to lab-inc-01.
      *
       lab-inc-fim.
           move zeros to campo-kbd.
           perform rot-close-tabl.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           perform display-tela-06.
      *
       lab-cns-01.
           perform display-tela-03.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 67 or 99
                                 perform display-tela-limpa-cad
                                 perform sec-consulta-codigo
                                 perform display-tela-03
                            when kbd2 = 68 or 100
                                 perform display-tela-limpa-cad
                                 perform sec-consulta-descricao
                                 perform display-tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           exit.
      *
       sec-consulta-codigo section.
      *
       lab-cns-codigo-00.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-cns-codigo-fim
           end-if.
           perform display-tela-04.
           move low-values to wtab01-chave.
           move rotina-tipo to wtab01-tipo
           move codigo to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
      *
       lab-cns-codigo-00-a.
           start arqtabl key is not less tabl-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           move reg-tabl to reg-wtab01.
           if erro not = 0 or wtab01-codigo = codigo 
              perform rot-inicio-arq
              start arqtabl key is not less tabl-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if tabl-chave = high-values or tabl-tipo not = rotina-tipo
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqtabl key is less tabl-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-tabl
              go to lab-cns-codigo-fim
           end-if.
           if tabl-chave = high-values 
              perform lmp-codigo thru lmp-descricao
              move "Fim do arquivo - Tecle <Enter>" to mensagem
              display tela-mensagem
              perform rot-keypress
              display tela-limpa
              start arqtabl key is not less tabl-chave
              move 0 to codigo
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if tabl-tipo not = rotina-tipo
              go to lab-cns-codigo-03
           end-if.
      *
       lab-cns-codigo-04.
           perform rot-display.
      *
       lab-cns-codigo-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to tabl-chave
                         move rotina-tipo to tabl-tipo
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to tabl-chave
                         move rotina-tipo to tabl-tipo
                         go to lab-cns-codigo-02
           end-evaluate.
           if erro = 0
              move codigo to rotina-codigo
           end-if.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-descricao.
           exit.
      *
       sec-consulta-descricao section.
      *
       lab-cns-descricao-00.
           move spaces to descricao.
           perform lmp-descricao.
           perform acc-descricao.
           if escape-key = 1
              perform lmp-descricao
              go to lab-cns-descricao-fim
           end-if.
           perform display-tela-04.
           move descricao to txt.
           perform rot-texto.
           move low-values to tabl-chave-1.
           move rotina-tipo to wtab01-tipo-1.
           move txt to wtab01-descricao-1.
           move wtab01-chave-1 to tabl-chave-1.
      *
       lab-cns-descricao-00-a.
           start arqtabl key is not less tabl-chave-1.
           go to lab-cns-descricao-03.
      *
       lab-cns-descricao-01.
           perform rot-le-anterior.
           move reg-tabl to reg-wtab01.
           if erro not = 0 or wtab01-codigo = codigo
              perform rot-inicio-arq
              start arqtabl key is not less tabl-chave-1
              move 1 to erro
              go to lab-cns-descricao-05
           end-if.
           if tabl-chave-1 = high-values or tabl-tipo not = rotina-tipo
              go to lab-cns-descricao-01
           end-if.
           go to lab-cns-descricao-04.
      *
       lab-cns-descricao-02.
           start arqtabl key is less tabl-chave-1.
      *
       lab-cns-descricao-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-tabl
              go to lab-cns-descricao-fim
           end-if.
           if tabl-chave-1 = high-values
              perform lmp-codigo thru lmp-descricao
              move "Fim do arquivo - Tecle <Enter>" to mensagem
              display tela-mensagem
              perform rot-keypress
              display tela-limpa
              start arqtabl key is not less tabl-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-descricao-05
           end-if.
           if tabl-tipo not = rotina-tipo
              go to lab-cns-descricao-03
           end-if.
      *
       lab-cns-descricao-04.
           perform rot-display.
      *
       lab-cns-descricao-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-descricao-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-descricao-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-descricao-03
                    when kbd-aux = 73
                         go to lab-cns-descricao-01
                    when kbd-aux = 71
                         move low-values to tabl-chave-1
                         move rotina-tipo to tabl-tipo-1
                         go to lab-cns-descricao-00-a
                    when kbd-aux = 79
                         move high-values to tabl-chave-1
                         move rotina-tipo to tabl-tipo-1
                         go to lab-cns-descricao-02
           end-evaluate.
           if erro = 0
              move codigo to rotina-codigo
           end-if.
           if kbd-aux not = 1
              go to lab-cns-descricao-05
           end-if.
      *
       lab-cns-descricao-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-descricao.
           exit.
      *
       sec-exclusao section.
       lab-exc-00-0.
           perform display-tela-limpa-cad.
           if param-prioridade < 7
              perform display-erro-usr
              go to lab-exc-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-exc-fim
           end-if.
       lab-exc-00.
           perform rot-le-tabl-lock.
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
           delete arqtabl invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQTABLA.DAT - Tecle <Enter>
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
           unlock arqtabl record.
           perform display-tela-04.
           exit.
      *
       sec-alteracao section.
      *
       lab-alt-00-0.
           perform display-tela-limpa-cad.
           if param-prioridade < 5
              perform display-erro-usr
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-alt-fim
           end-if.
       lab-alt-00.
           perform rot-le-tabl-lock.
           perform rot-display.
       lab-alt-01.
           perform acc-descricao.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move descricao to  txt.
           perform rot-texto.
           if txt = spaces 
              go to lab-alt-01
           end-if.
           move txt to descricao-aux.
           move "Alterar (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-alt-02.
           perform accept-resposta-cad.
           if escape-key = 1
              perform display-tela-limpa-cad
              go to lab-alt-01
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-02
              end-if
           end-if.           
           perform rot-move-tabl.
           rewrite reg-tabl invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQTABLA.DAT - Tecle <Ent
      -            "er>"
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
           unlock arqtabl.
           perform display-tela-04.
           exit.
