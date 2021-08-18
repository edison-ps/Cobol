      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGUSR01      *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao de Cadastro de Usuarios:                        *
      *                                                             *
      *  Data da ultima alteracao:    24/04/93     v1.00            *
      *                               23/10/93     v1.01            *
      *                               16/06/95     v1.02            *
      *                               12/12/97     v1.03            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgusr01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
            select arqusr assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is usr-chave
                   file status is usr-status.
      *
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
      *    
       copy fdusr.lib.
      *
       copy fdimp.lib.
      *
       working-storage section.
      *
       01 usr-status                  pic x(02) value "00".
       01 usr-stat                    pic x(01) value "F".
      *
       01 nome-arq-usr.
          02 usr-dir                   pic x(03) value "USR".
          02 filler                    pic x(01) value "\".
          02 usr-nome                  pic x(08) value "ARQUSR00".
          02 filler                    pic x(01) value ".".
          02 usr-ext                   pic x(03) value "DAT".
      *
       01 imp-status                  pic x(02) value "00".
       01 imp-stat                    pic x(01) value "F".
      *
       01 nome-arq-imp.
          02 imp-dir                   pic x(03) value "IMP".
          02 filler                    pic x(01) value "\".
          02 imp-nome                  pic x(07) value "ARQIMPA".
          02 filler                    pic x(01) value ".".
          02 imp-ext                   pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGUSR01".
          02 cb-versao                 pic x(06) value "v1.03 ".
      *
       01 campos.
          02 usuario                   pic x(10) value spaces.
          02 senha                     pic x(10) value spaces.
          02 prioridade                pic 9(01) value 0.
          02 impress                   pic x(10) value spaces.
          02 senha-aux                 pic x(10) value spaces.
      *
       01 linha                        pic 9(04) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 0.
       01 limpa                        pic x(59) value spaces.
       01 mens-erro                    pic x(59) value spaces.
       01 limpa-aux                    pic x(40) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 tamanho                      pic 9(04) comp-5 value 59.
      *
       01 campo-rotina-imp.
          02 rotina-col-imp            pic 9(02) value 0.
          02 rotina-lin-imp            pic 9(02) value 0.
          02 rotina-borda-imp          pic x(01) value spaces.
          02 rotina-fundo-imp          pic x(01) value spaces.
          02 rotina-sombra-imp         pic x(01) value spaces.
          02 rotina-impress            pic x(10) value spaces.
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
          02 rotina-usr-usuario        pic x(10).
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
           perform display-tela-limpa-cad.
           perform sec-inclusao.
      *
       lab-fim.
           move 0 to box-col.
           move 80 to box-col-f.
           move 25 to box-lin-f. 
           perform rot-rest-buffer.
           move usr-usuario to rotina-usr-usuario.
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
       rot-le-usr.
           move 0 to erro.
           read arqusr invalid key move 1 to erro.
           if usr-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-imp.
      *
       rot-le-imp.
           move 0 to erro.
           read arqimp invalid key move 1 to erro.
           if imp-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-imp.
      *
       rot-move-usr.
           move usuario to usr-usuario.
           move 1 to cript-opcao.
           move 10 to cript-tam.
           move senha to cript-txt
           perform rot-cript.
           move cript-txt to usr-senha.
           move prioridade to usr-prioridade.
           move impress to usr-impress.
           move param-usr to usr-usr.
           move param-data to usr-data.
      *
       rot-move-campos.
           move usr-usuario to usuario.
           move 2 to cript-opcao.
           move 10 to cript-tam.
           move usr-senha to cript-txt
           perform rot-cript.
           move cript-txt to senha.
           move usr-prioridade to prioridade.
           move usr-impress to impress.
           move usr-usr to cab-usuario.
           move usr-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-pesq-imp.
           perform rot-close-imp.
           move 07 to rotina-col-imp.
           move 09 to rotina-lin-imp.
           move box-borda to rotina-borda-imp.
           move spaces to rotina-fundo-imp.
           move "S" to rotina-sombra-imp.
           call "pgimp01" using param-menu campo-rotina-imp.
           cancel "pgimp01".
           perform rot-open-imp.
      *
       rot-open-imp.
           if imp-stat = "F"
              open i-o arqimp
              if imp-status not = "00"
                 move 
                 " Erro de abertura no ARQIMPA.DAT - Tecle <Enter>" to 
                  mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-fim                 
              end-if
              move "A" to imp-stat
           end-if.
      *
       rot-close-imp.
           if imp-stat = "A"
              close arqimp
              move "F" to imp-stat
           end-if.
      *
       rot-display.
           perform rot-move-campos.
           move all "*" to senha.
           perform dsp-usuario thru dsp-impress.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-le-usr-lock.
           move 0 to erro.
           read arqusr next.
           if usr-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-usr-lock
           end-if
           read arqusr with lock.
      *
       rot-erro-usr.
           move " Erro de leitura - ARQUSR00.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqusr key is equal usr-chave invalid key
                 move 1 to erro
                 perform rot-erro-usr
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
           display "Usuario....:" at line linha column coluna with
                   foreground-color 06 background-color 01 highlight.
           add rotina-lin to 04 giving linha.
           display "Senha......:" at line linha column coluna with
                   foreground-color 06 background-color 01 highlight.
           add rotina-lin to 05 giving linha.
           display "Prioridade.:" at line linha column coluna with
                   foreground-color 06 background-color 01 highlight.
           add rotina-lin to 06 giving linha.
           display "Impressora.:" at line linha column coluna with
                   foreground-color 06 background-color 01 highlight.
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
           add rotina-lin to 8 giving linha.
           add rotina-col to 2 giving coluna.
           display limpa at line linha column coluna with highlight
                            foreground-color 02 background-color 03.
           add 1 to coluna.
           display "F2" at line linha column coluna with highlight
                               foreground-color 02 background-color 03.
           add 2 to coluna.
           display "-Impressoras" at line linha column coluna with
                              foreground-color 05 background-color 03.
      *
       display-tela-06.
           add rotina-lin to 02 giving linha
           add rotina-col to 53 giving coluna.
           display "Inclusao" at line linha column coluna with highlight
                   foreground-color 06 background-color 01.

      *
       display-tela-07.
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
       acc-usuario.
           add rotina-lin to 3 giving linha.
           add rotina-col to 15 giving coluna.
           accept usuario at line linha column coluna with auto update
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-senha.
           add rotina-lin to 4 giving linha.
           add rotina-col to 15 giving coluna.
           accept senha at line linha column coluna with auto update 
                  prompt secure foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-prioridade.
           add rotina-lin to 5 giving linha.
           add rotina-col to 15 giving coluna.
           accept prioridade at line linha column coluna with auto 
                 update prompt foreground-color 15  background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-impress.
           add rotina-lin to 6 giving linha.
           add rotina-col to 15 giving coluna.
           accept impress at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-usuario.
           add rotina-lin to 3 giving linha.
           add rotina-col to 15 giving coluna.
           display usuario at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       dsp-senha.
           add rotina-lin to 4 giving linha.
           add rotina-col to 15 giving coluna.
           display senha at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       dsp-prioridade.
           add rotina-lin to 5 giving linha.
           add rotina-col to 15 giving coluna.
           display prioridade at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       dsp-impress.
           add rotina-lin to 6 giving linha.
           add rotina-col to 15 giving coluna.
           display impress at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
      *  Sequencia para fazer limpeza
      *
       lmp-usuario.
           add rotina-lin to 3 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       lmp-senha.
           add rotina-lin to 4 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       lmp-prioridade.
           add rotina-lin to 5 giving linha.
           add rotina-col to 15 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 01.
      *
       lmp-impress.
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
           if usr-stat = "F"
              open i-o arqusr
              if usr-status not = "00"
                 move 
                 " Erro de abertura no ARQUSR00.DAT - Tecle <Enter>" to 
                  mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
              end-if
              move "A" to usr-stat
           end-if.
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
           perform display-tela-06.
           perform display-tela-02.
       lab-inc-01.
           move spaces to usuario.
           perform lmp-usuario.
           perform acc-usuario.
           if escape-key = 1
              perform lmp-usuario
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform sec-consulta
              perform display-tela-06
              perform display-tela-02
              go to lab-inc-01
           end-if.
           move usuario to txt.
           perform rot-texto.
           move txt to usuario.
           if usuario = spaces
              go to lab-inc-01
           end-if.
           perform dsp-usuario.
           move usuario to usr-usuario.
           perform rot-le-usr.
           if erro = 0
              move " Usuario ja cadastrado - Tecle <Enter>" to 
              mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              perform  display-tela-02
              go to lab-inc-01
           end-if.
       lab-inc-02.
           perform display-tela-limpa-cad.
           move spaces to senha.
           perform lmp-senha.
           perform acc-senha.
           if escape-key = 1
              perform lmp-senha
              perform display-tela-02
              go to lab-inc-01
           end-if.
           if senha = spaces
              go to lab-inc-02
           end-if.
           move senha to senha-aux.
           move spaces to senha.
           perform lmp-senha.
           perform acc-senha.
           if escape-key = 1
              perform lmp-senha
              go to lab-inc-01
           end-if.
           if senha not = senha-aux
              go to lab-inc-02
           end-if.
       lab-inc-03.
           move 0 to prioridade.
           perform lmp-prioridade.
           perform acc-prioridade.
           if escape-key = 1
              perform lmp-prioridade
              go to lab-inc-02
           end-if.
           if prioridade > param-prioridade
              go to lab-inc-03
           end-if.
           move spaces to rotina-impress.
           perform display-tela-05.
       lab-inc-04.
           move rotina-impress to impress.
           perform lmp-impress.
           perform acc-impress.
           if escape-key = 1
              perform lmp-impress
              perform display-tela-limpa-cad
              go to lab-inc-03
           end-if.
           if escape-key = 3
              perform rot-pesq-imp
              go to lab-inc-04
           end-if.
           move impress to txt.
           perform rot-texto.
           move txt to impress.
           if impress = spaces
              go to lab-inc-04
           end-if.
           perform dsp-impress.
           move impress to imp-impress.
           perform rot-le-imp.
           if erro not = 0
              move " Impressora nao cadastrado - Tecle <Enter>" to 
              mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              perform  display-tela-05
              go to lab-inc-04
           end-if.
           move "Cadastrar (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-inc-05.
           perform accept-resposta-cad.
           if escape-key = 1
              move spaces to rotina-impress
              perform display-tela-05
              go to lab-inc-04
           end-if.
           if resposta = "N"
              perform lmp-usuario thru lmp-impress
              perform display-tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-05
              end-if
           end-if.
           perform rot-move-usr.
           write reg-usr invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQUSR00.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           move "Registro gravado - Tecle <Enter>" to mensagem.
           perform display-tela-mensagem-cad.
           perform rot-keypress.
           perform lmp-usuario thru lmp-impress.
           perform display-tela-02.
           go to lab-inc-01.
      *
       lab-inc-fim.
           move zeros to campo-kbd.
           if usr-stat = "A"
              close arqusr 
              move "F" to usr-stat
           end-if.
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
           perform display-tela-07.
      *
       lab-cns-01.
           move spaces to usuario.
           perform lmp-usuario.
           perform acc-usuario.
           if escape-key = 1
              perform lmp-usuario
              go to lab-cns-fim
           end-if.
           move usuario to txt.
           perform rot-texto.
           move txt to usuario.
           perform display-tela-04.
           move low-values to usr-chave.
           move usuario to usr-usuario.
      *
       lab-cns-01-a.
           start arqusr key is not less usr-chave.
           go to lab-cns-04.
      *
       lab-cns-02.
           move 0 to erro.
           read arqusr previous at end move 1 to erro.
           if usr-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to lab-cns-02
           end-if.
           if erro not = 0 or usr-usuario = usuario 
              perform lmp-usuario thru lmp-impress
              move "Inicio do arquivo - Tecle <Enter>" to mensagem
              display tela-mensagem
              perform rot-keypress
              display tela-limpa
              start arqusr key is not less usr-chave
              move 1 to erro
              go to lab-cns-06
           end-if.
           if usr-chave = high-values
              go to lab-cns-02
           end-if.
           go to lab-cns-05.
      *
       lab-cns-03.
           start arqusr key is less usr-chave.
      *
       lab-cns-04.
           move 0 to erro.
           read arqusr next at end move 1 to erro.
           if usr-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to lab-cns-04
           end-if.
           if erro not = 0
              move " Erro de leitura - ARQTUSR00.DAT - Tecle <Enter>" to
              mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-cns-fim
           end-if.
           if usr-chave = high-values 
              perform lmp-usuario thru lmp-impress
              move "Fim do arquivo - Tecle <Enter>" to mensagem
              display tela-mensagem
              perform rot-keypress
              display tela-limpa
              start arqusr key is not less usr-chave
              move spaces to usuario
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
                         move low-values to usr-chave
                         go to lab-cns-01-a
                    when kbd-aux = 79
                         move high-values to usr-chave
                         go to lab-cns-03
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-06
           end-if.
      *
       lab-cns-fim.
           perform lmp-usuario thru lmp-impress.
           display tela-limpa.
           exit.
      *
       sec-exclusao section.
       lab-exc-00-0.
           perform display-tela-limpa-cad.
           if param-prioridade < 8 or param-prioridade < usr-prioridade
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
           perform rot-le-usr-lock.
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
           delete arqusr invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQUSR.DAT - Tecle <Enter>
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
           unlock arqusr record.
           perform display-tela-04.
           exit.
      *
       sec-alteracao section.
      *
       lab-alt-00-0.
           perform display-tela-limpa-cad.
           if param-prioridade < 8 and param-usr not = usuario or
              param-prioridade < usr-prioridade
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
           perform rot-le-usr-lock.
           perform rot-display.
       lab-alt-01.
           if param-usr not = usuario
              perform rot-move-campos
              go to lab-alt-02
           end-if.
           move spaces to senha.
           perform acc-senha.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           if senha = spaces
              go to lab-alt-01
           end-if.
           move senha to senha-aux.
           move spaces to senha.
           perform acc-senha.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           if senha not = senha-aux
              go to lab-alt-01
           end-if.
       lab-alt-02.
           move impress to rotina-impress.
           if param-prioridade < 8
              perform display-tela-05
              go to lab-alt-03
           end-if.
           perform acc-prioridade.
           if escape-key = 1
              if param-usr not = usuario
                 go to lab-alt-fim
              end-if
              go to lab-alt-01
           end-if.
           if prioridade > param-prioridade
              go to lab-alt-02
           end-if.
           perform display-tela-05.
       lab-alt-03.
           move rotina-impress to impress.
           perform acc-impress.
           if escape-key = 1
              perform display-tela-limpa-cad
              if param-prioridade > 7
                 go to lab-alt-02
              end-if
              go to lab-alt-01
           end-if.
           if escape-key = 3
              perform rot-pesq-imp
              go to lab-alt-03
           end-if.
           move impress to txt.
           perform rot-texto.
           move txt to impress.
           if impress = spaces
              go to lab-alt-03
           end-if.
           perform dsp-impress.
           move impress to imp-impress.
           perform rot-le-imp.
           if erro not = 0
              move " Impressora nao cadastrado - Tecle <Enter>" to 
              mens-erro
              perform display-tela-erro-cad
              perform rot-keypress
              perform  display-tela-05
              go to lab-alt-03
           end-if.
           move "Alterar (S) (N) ?" to mensagem.
           perform display-tela-mensagem-cad.
       lab-alt-04.
           perform accept-resposta-cad.
           if escape-key = 1
              perform display-tela-05
              go to lab-alt-03
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-04
              end-if
           end-if.           
           perform rot-move-usr.
           rewrite reg-usr invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQUSR00.DAT - Tecle <Ente
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
           if usuario = param-usr
              move imp-device to param-impress
           end-if.
      *
       lab-alt-fim.
           unlock arqusr record.
           perform display-tela-04.
           exit.
