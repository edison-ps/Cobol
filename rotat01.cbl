      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  ROTAT01      *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Consulta do arquivo de cadastro :                          * 
      *                                                             *
      *  Data da ultima alteracao:    08/12/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. rotat01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqat01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is at01-chave
                  alternate record key is at01-chave-1 with duplicates
                  alternate record key is at01-chave-2 with duplicates
                  file status is at01-status.
      *
       data division.
       file section.
      *     
       copy fdat01.lib.
      *     
       working-storage section.
      *
       01 at01-status                  pic x(02) value "00".
       01 at01-stat                    pic x(01) value "F".
      *
       01 nome-arq-at01.
          02 at01-dir                  pic x(03) value "AT2".
          02 filler                    pic x(01) value "\".
          02 at01-nome                 pic x(08) value "ARQAT01A".
          02 filler                    pic x(01) value ".".
          02 at01-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "ROTAT01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 campos.
          02 codigo                    pic x(06) value spaces.
          02 empresa                   pic x(40) value spaces.
          02 diretor                   pic x(40) value spaces.
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
       copy wstab03.lib.
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
          02 rotina-codigo             pic x(06).
        
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
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move rotina-fundo to box-fundo.
           move rotina-sombra to box-sombra.
           perform rot-save-buffer.
           move rotina-col to box-col.
           display cb-programa at 2402.
           display cb-versao at 2412.
           add rotina-col to 60 giving box-col-f
           add rotina-lin to 07 giving box-lin-f
           perform rot-box.
           perform display-tela-01.
      *
       lab-01.
           perform display-tela-limpa-cad.
           perform sec-consulta.
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
       rot-display.
           move at01-codigo to codigo.
           move at01-empresa-a  to empresa.
           move at01-diretor-a to diretor.
           move at01-usuario to cab-usuario.
           move at01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           perform dsp-codigo thru dsp-diretor.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqat01 previous at end move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqat01 next at end move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-at01.
           move 0 to erro.
           if at01-stat = "F"
              open i-o arqat01
              if at01-status not = "00"
                 move 
                 " Erro de abertura no ARQAT01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 move zeros to reg-at01
                 move high-values to at01-controle
                 write reg-at01-1
               else
                  move "A" to at01-stat
               end-if
           end-if.
      *
       rot-close-at01.
           if at01-stat = "A"
              close arqat01
              move "F" to at01-stat
           end-if.
      *
       rot-erro-leitura-at01.
           move " Erro de leitura - ARQAT01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-diretor.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-diretor.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
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
           add rotina-col to 03 giving coluna.
           display "Codigo........:" at line linha column coluna with
                                    foreground-color 06
                                    background-color 04.
           add rotina-lin to 04 giving linha.
           display "Empresa.......:" at line linha column coluna with
                                    foreground-color 06
                                    background-color 04.
           add rotina-lin to 05 giving linha.
           display "Diretor.......:" at line linha column coluna with
                                    foreground-color 06
                                    background-color 04.
      *
       display-tela-03.
           add rotina-lin to 7 giving linha.
           add rotina-col to 2 giving coluna.
           display limpa at line linha column coluna with highlight
                            foreground-color 01 background-color 02.
           add 1 to coluna.
           display "F1" at line linha column coluna with highlight
                            foreground-color 07 background-color 02.
           add 2 to coluna.
           display "-Help" at line linha column coluna with
                              foreground-color 01 background-color 02.
           add 8 to coluna.
           display "E" at line linha column coluna with highlight
                               foreground-color 07 background-color 02.
           add 1 to coluna.
           display "mpresa" at line linha column coluna with
                              foreground-color 01 background-color 02.
           add 10 to coluna.
           display "D" at line linha column coluna with highlight
                          foreground-color 07 background-color 02.
           add 1 to coluna.
           display "iretor" at line linha column coluna with
                                foreground-color 01 background-color 02.
      *
       display-tela-04.
           add rotina-lin to 7 giving linha.
           add rotina-col to 2 giving coluna.
           display limpa at line linha column coluna with highlight
                            foreground-color 01 background-color 02.
           add 4 to coluna.
           display "Home" at line linha column coluna with highlight
                             foreground-color 07 background-color 02.
           add 4 to coluna.
           display "-Inic" at line linha column coluna with
                              foreground-color 01 background-color 02.
           add 9 to coluna.
           display "End" at line linha column coluna with highlight
                            foreground-color 07 background-color 02.
           add 3 to coluna.
           display "-Fim" at line linha column coluna with
                             foreground-color 01 background-color 02.
           add 9 to coluna.
           display "PgDown" at line linha column coluna with highlight
                               foreground-color 07 background-color 02.
           add 6 to coluna.
           display "-Prox" at line linha column coluna with
                             foreground-color 01 background-color 02.
           add 9 to coluna.
           display "PgUp" at line linha column coluna with highlight
                             foreground-color 07 background-color 02.
           add 4 to coluna.
           display "-Ant" at line linha column coluna with
                              foreground-color 01 background-color 02.
      *
       display-tela-06.
           add rotina-lin to 02 giving linha
           add rotina-col to 53 giving coluna.
           display "Consulta" at line linha column coluna with underline
                   foreground-color 15 background-color 04.
      * 
       display-tela-limpa-cad.
           add rotina-lin to 6 giving linha.
           add rotina-col to 1 giving coluna.
           call "C_Writexy" using by value coluna
                                  by value linha
                                  by value tamanho
                                  by value box-cor-f
                                  by value box-cor-p
                                  by reference limpa.
      *
       display-tela-mensagem-cad.
           add rotina-lin to 6 giving linha.
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
           add rotina-lin to 7 giving linha.
           add rotina-col to 2 giving coluna.
           display mens-erro at line linha column coluna with beep 
                               reverse-video.
      *
      *  Sequencia para dar accept
      *
       acc-codigo.
           add rotina-lin to 3 giving linha.
           add rotina-col to 19 giving coluna.
           accept codigo at line linha column coluna with auto update 
                  prompt foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-empresa.
           add rotina-lin to 4 giving linha.
           add rotina-col to 19 giving coluna.
           accept empresa at line linha column coluna with auto 
                  update prompt foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-diretor.
           add rotina-lin to 5 giving linha.
           add rotina-col to 19 giving coluna.
           accept diretor at line linha column coluna with auto 
                  update prompt foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           add rotina-lin to 3 giving linha.
           add rotina-col to 19 giving coluna.
           display codigo at line linha column coluna with
                   foreground-color 15 background-color 04.
      *
       dsp-empresa.
           add rotina-lin to 4 giving linha.
           add rotina-col to 19 giving coluna.
           display empresa at line linha column coluna with
                   foreground-color 15 background-color 04.
      *
       dsp-diretor.
           add rotina-lin to 5 giving linha.
           add rotina-col to 19 giving coluna.
           display diretor at line linha column coluna with
                   foreground-color 15 background-color 04.
      *
      *  Sequencia para fazer limpeza
      *
       lmp-codigo.
           add rotina-lin to 3 giving linha.
           add rotina-col to 19 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 04.
      *
       lmp-empresa.
           add rotina-lin to 4 giving linha.
           add rotina-col to 19 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 04.
      *
       lmp-diretor.
           add rotina-lin to 5 giving linha.
           add rotina-col to 19 giving coluna.
           display limpa-aux at line linha column coluna with
                   foreground-color 15 background-color 04.
      *
       accept-resposta-cad.
           add rotina-lin to 6 giving linha.
           add rotina-col to 59 giving coluna.
           move spaces to resposta.
           accept resposta at line linha column coluna with auto
                              foreground-color 04 background-color 04.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       sec-consulta section.
      *
       lab-cns-00.
           perform rot-open-at01.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           perform display-tela-limpa-cad.
           perform display-tela-06.
      *
       lab-cns-01.
           perform display-tela-03.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 69 or 101
                                 perform display-tela-limpa-cad
                                 perform sec-consulta-empresa
                                 perform display-tela-03
                            when kbd2 = 68 or 100
                                 perform display-tela-limpa-cad
                                 perform sec-consulta-diretor
                                 perform display-tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           perform rot-close-at01.
           move codigo to rotina-codigo.
           exit.
      *
       sec-consulta-empresa section.
      *
       lab-cns-empresa-00.
           move spaces to empresa.
           perform lmp-empresa.
           perform acc-empresa.
           if escape-key = 1
              perform lmp-empresa
              go to lab-cns-empresa-fim
           end-if.
           perform display-tela-04.
           move empresa to txt.
           perform rot-texto.
           move low-values to at01-chave-1.
           move txt to at01-empresa in at01-chave-1.
      *
       lab-cns-empresa-00-a.
           start arqat01 key is not less at01-chave-1.
           go to lab-cns-empresa-03.
      *
       lab-cns-empresa-01.
           perform rot-le-anterior.
           if erro not = 0 or at01-codigo = codigo 
              perform rot-inic-arquivo
              start arqat01 key is not less at01-chave-1
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
           if at01-chave = high-values
              go to lab-cns-empresa-01
           end-if.
           go to lab-cns-empresa-04.
      *
       lab-cns-empresa-02.
           start arqat01 key is less at01-chave-1.
      *
       lab-cns-empresa-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-at01
              go to lab-cns-empresa-fim
           end-if.
           if at01-chave = high-values
              perform rot-fim-arquivo
              start arqat01 key is not less at01-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
      *
       lab-cns-empresa-04.
           perform rot-display.
      *
       lab-cns-empresa-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 81 
                         go to lab-cns-empresa-03
                    when kbd-aux = 73
                         go to lab-cns-empresa-01
                    when kbd-aux = 71
                         move low-values to at01-chave-1
                         go to lab-cns-empresa-00-a
                    when kbd-aux = 79
                         move high-values to at01-chave-1
                         go to lab-cns-empresa-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-empresa-05
           end-if.
           perform lmp-codigo thru lmp-diretor.
           perform display-tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-empresa-00.
      *
       lab-cns-empresa-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-diretor.
           display tela-limpa.
           exit.
      *
       sec-consulta-diretor section.
      *
       lab-cns-diretor-00.
           move spaces to diretor.
           perform lmp-diretor.
           perform acc-diretor.
           if escape-key = 1
              perform lmp-diretor
              go to lab-cns-diretor-fim
           end-if.
           perform display-tela-04.
           move diretor to txt.
           perform rot-texto.
           move low-values to at01-chave-2.
           move txt to at01-diretor in at01-chave-2.
      *
       lab-cns-diretor-00-a.
           start arqat01 key is not less at01-chave-2.
           go to lab-cns-diretor-03.
      *
       lab-cns-diretor-01.
           perform rot-le-anterior.
           if erro not = 0 or at01-codigo = codigo 
              perform rot-inic-arquivo
              start arqat01 key is not less at01-chave-2
              move 1 to erro
              go to lab-cns-diretor-05
           end-if.
           if at01-chave = high-values
              go to lab-cns-diretor-01
           end-if.
           go to lab-cns-diretor-04.
      *
       lab-cns-diretor-02.
           start arqat01 key is less at01-chave-2.
      *
       lab-cns-diretor-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-at01
              go to lab-cns-diretor-fim
           end-if.
           if at01-chave = high-values
              perform rot-fim-arquivo
              start arqat01 key is not less at01-chave-2
              move 0 to codigo
              move 1 to erro
              go to lab-cns-diretor-05
           end-if.
      *
       lab-cns-diretor-04.
           perform rot-display.
      *
       lab-cns-diretor-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 81 
                         go to lab-cns-diretor-03
                    when kbd-aux = 73
                         go to lab-cns-diretor-01
                    when kbd-aux = 71
                         move low-values to at01-chave-2
                         go to lab-cns-diretor-00-a
                    when kbd-aux = 79
                         move high-values to at01-chave-2
                         go to lab-cns-diretor-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-diretor-05
           end-if.
           perform lmp-codigo thru lmp-diretor.
           perform display-tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-diretor-00.
      *
       lab-cns-diretor-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-diretor.
           display tela-limpa.
           exit.
      *